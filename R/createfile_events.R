#### To do ####
# Create On treatment variable

# library(dplyr)
# library(tidyr)
# library(haven)
# library(purrr)
# library(lubridate)

#' Function to create event level megaplots dataset from ADaM datasets
#'
#' @param mp_data
#' @param path_data
#' @param id
#' @param data_filter
#' @param param
#' @param prefix
#' @param event_start
#' @param event_end
#'
#' @return
#' @export
#'
#' @examples
createFile.events <- function(mp_data,
                              path_data,
                              id = "USUBJID",
                              data_filter = NULL,
                              param = list(
                                c("AEBODSYS","AEDECOD"),
                                c("AEBODSYS","AELLT"),
                                c("AEDECOD","AESEV"),
                                c("AEDECOD","AESER")
                              ),
                              prefix = NULL,
                              event_start = c("ASTDT","AESTDT","ADY"),
                              event_end = c("AENDT","AEENDT"),
                              calc_time_to_first = FALSE,
                              calc_days_with = FALSE,
                              left_censor = NULL
){
  # Check if mp_data is a list
  if (!is.list(mp_data)) {
    stop("Error: mp_data must be a list.")
  }

  # Check if mp_data has sl and events entry
  if (length((missing_names <- setdiff(c("sl", "events"), names(mp_data)))) > 0) {
    stop(paste("Error: The following required names are missing in mp_data:", paste(missing_names, collapse = ", ")))
  }

  adsl <- mp_data$sl %>% dplyr::select(subjectid,start_time,end_time,ref_date)
  events <- mp_data$events

  # Read data ----
  if (is.data.frame(path_data)) {
    data <- path_data
  } else if (!is.null(path_data)) {
    tryCatch({
      if (grepl("\\.sas7bdat$", path_data, ignore.case = TRUE)) {
        data <- haven::read_sas(path_data)
      } else if (grepl("\\.csv$", path_data, ignore.case = TRUE)) {
        data <- read.csv(path_data)
      } else if (grepl("\\.RData$", path_data, ignore.case = TRUE)) {
        load(path_data)
        # Assuming the data frame is named 'data' in the RData file
        data <- get(ls()[1])  # Get the first object in the environment
      } else {
        stop("Unsupported file format. Please provide a SAS, CSV, or RData file.")
      }
    }, error = function(e) {
      stop("Error reading the dataset: ", e$message)
    })
  } else {
    stop("Please provide a valid dataset or file path.")
  }

  data <- data %>%
    #Filter data
    {if(!is.null(data_filter)) dplyr::filter(., !!!rlang::parse_exprs(data_filter)) else .} %>%
    #rename and relocate id-variable.
    dplyr::mutate(subjectid = as.numeric(gsub("[^0-9.]", "", as.character(!!sym(id))))) %>%
    dplyr::filter(subjectid %in% adsl$subjectid) %>%
    dplyr::left_join(adsl %>% select(subjectid,start_time,end_time,ref_date), by="subjectid") %>% # Join with ADSL for TRTSDT
    dplyr::relocate(subjectid,ref_date)

  # Check if param is a list
  if (!is.list(param)) {
    stop("Error: param must be a list.")
  }
  # Loop through each entry in the param list
  for (entry in param) {
    # Check if both strings in the entry are column names in data
    if (toupper(entry[1]) %in% toupper(colnames(data)) &&
        toupper(entry[2]) %in% toupper(colnames(data))) {
      # If both are found, print the event group and event
      message(sprintf("Event group: %s, Event: %s", entry[1], entry[2]))
    } else {
      message("None of the input parameters in param are present as column names in the data.")
    }
  }

  event_start <- event_start[toupper(event_start) %in% toupper(colnames(data))][1]
  # Provide the appropriate message
  if (!(is.null(event_start) | is.na(event_start))) {
    message(sprintf("Event start date/time: %s", event_start))
  } else {
    message("None of the input parameters in event_start are present as column names in the data.")
  }

  event_end <- event_end[toupper(event_end) %in% toupper(colnames(data))][1]
  # Provide the appropriate message
  if (!(is.null(event_end) | is.na(event_end))) {
    message(sprintf("Event end date/time: %s", event_end))
  } else {
    message("None of the input parameters in event_end are present as column names in the data.")
  }

  events_tmp <- NULL
  for(i in 1:length(param)){
    entry <- param[[i]]
    pre <- prefix[[i]]
    data_tmp <- data %>%
      dplyr::mutate(event_group = paste0(pre[1], !!!syms(entry)[1]),
                    event = paste0(pre[2], !!!syms(entry)[2])) %>%
      dplyr::filter(!is.na(event) & event != "") %>%
      dplyr::select(subjectid, event_group, event, !!sym(event_start), !!sym(event_end), ref_date, start_time, end_time) %>%
      dplyr::filter(!is.na(!!sym(event_start))) %>%
      arrange(subjectid, event_group, event, !!sym(event_start), !!sym(event_end)) %>%
      group_by(subjectid, event_group, event) %>%
      dplyr::mutate(event_start_time=as.integer(!!sym(event_start)-ref_date+1)) %>%# Calculate start time of adverse events
      dplyr::mutate(event_end_time = case_when(
          !is.na(!!sym(event_end)) ~ as.integer(!!sym(event_end)-ref_date+1),  # Keep existing End_day if not missing
          TRUE ~ as.integer(!!sym(event_start)-ref_date+1) # Set event_end_time to start_time, if missing
        )
      ) %>%
      ungroup() %>%
      dplyr::mutate(event_start_time =
                      ifelse(is.null(left_censor), event_start_time,
                             case_when(
                               event_start_time < start_time + left_censor ~ start_time + left_censor,
                               TRUE ~ event_start_time
                             )
                      )
      ) %>%
      dplyr::select(-c(!!sym(event_start), !!sym(event_end), ref_date)) %>% # Drop original date columns
      relocate(subjectid, event_group, event, event_start_time, event_end_time) %>% # Rearrange columns
      distinct()

    events_tmp <- rbind(events_tmp, data_tmp)
  }

  time_to_first <- NULL
  if(calc_time_to_first==TRUE){
    print("calcuating time to first event")
    time_to_first <- events_tmp %>%
      arrange(subjectid,event_group,event,event_start_time) %>%
      group_by(subjectid,event_group,event) %>%
      dplyr::mutate(first = min(as.numeric(event_start_time))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(event_start_time, event_end_time, start_time, end_time)) %>%
      distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group),
                    event = gsub("[[:punct:][:space:]]+", "_", event)) %>%
      pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group","event"),
        names_prefix = "ttf_",
        names_sep = "_",
        values_from = "first"
      )
    time_to_first_group <- events_tmp %>%
      dplyr::select(-event) %>%
      arrange(subjectid,event_group,event_start_time) %>%
      group_by(subjectid,event_group) %>%
      dplyr::mutate(first = min(as.numeric(event_start_time))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(event_start_time, event_end_time, start_time, end_time)) %>%
      distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group)) %>%
      pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group"),
        names_prefix = "ttf_",
        names_sep = "_",
        values_from = "first"
      )
    mp_data$sl <- mp_data$sl %>%
      left_join(time_to_first_group, by="subjectid") %>%
      left_join(time_to_first, by="subjectid")
  }

  days_with <- NULL
  if(calc_days_with==T){
    print("calcuating days with event")
    days_with <- events_tmp %>%
      arrange(subjectid,event_group,event,event_start_time,event_end_time) %>%
      dplyr::mutate(days = as.numeric(event_end_time) - as.numeric(event_start_time) + 1) %>%
      group_by(subjectid,event_group,event) %>%
      dplyr::mutate(days_with = sum(days)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(event_start_time, event_end_time, start_time, end_time, days)) %>%
      distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group),
                    event = gsub("[[:punct:][:space:]]+", "_", event)) %>%
      pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group","event"),
        names_prefix = "dw_",
        names_sep = "_",
        values_from = "days_with",
        values_fill = 0
      )
    days_with_group <- events_tmp %>%
      dplyr::select(-event) %>%
      arrange(subjectid,event_group,event_start_time, event_end_time) %>%
      dplyr::mutate(days = as.numeric(event_end_time) - as.numeric(event_start_time) + 1) %>%
      group_by(subjectid,event_group) %>%
      dplyr::mutate(days_with = sum(days)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(event_start_time, event_end_time, start_time, end_time, days)) %>%
      distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group)) %>%
      pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group"),
        names_prefix = "dw_",
        names_sep = "_",
        values_from = "days_with",
        values_fill = 0
      )
    mp_data$sl <- mp_data$sl %>%
      left_join(days_with_group, by="subjectid") %>%
      left_join(days_with, by="subjectid")
  }

  mp_data$events <- rbind(mp_data$events, events_tmp)

  return(mp_data)
}
