#' Function to create event level megaplots dataset from ADaM datasets
#'
#' This function processes ADaM datasets to create a comprehensive dataset for event-level analysis with megaplots,
#' including the ability to calculate time to first event or days with event, left censor data and manage additional variables.
#'
#' @param mp_data A list containing the subject-level and event data. It must have entries "sl" and "events" (may be NULL).
#' @param path_data A data frame or a file path to the dataset (SAS, CSV, or RData) to be read. File should be ADaM conform.
#' @param id A string representing the subject identifier column name. Default is "USUBJID".
#' @param data_filter A string for filtering the dataset using dplyr syntax. Default is NULL.
#' @param param A list of character vectors specifying the event group and event names for analysis.
#' @param prefix A list of character vectors for prefixes to be added to event group and event names.
#' @param event_start A character vector of possible event start date/time column names.
#' @param event_end A character vector of possible event end date/time column names.
#' @param calc_time_to_first A logical indicating whether to calculate the time to the first event. Default is FALSE.
#' @param calc_days_with A logical indicating whether to calculate the days with the event. Default is FALSE.
#' @param left_censor A numeric value specifying the left censoring time. Default NULL means no left cesnoring will be performed.
#' @param keep_vars A character vector of additional event level variables to keep in the final dataset.
#'
#' @return A modified list containing updated "sl" and "events" entries with the processed event-level data.
#' @export
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
                              left_censor = NULL,
                              keep_vars = NULL
){
  #set global variable . to NULL to
  #avoid note: no visible binding for global variable '.' when
  # performing package check (via devtools)
  . <- NULL

  # Check if mp_data is a list
  if (!is.list(mp_data)) {
    stop("Error: mp_data must be a list.")
  }

  # Check if mp_data has sl and events entry
  if (length((missing_names <- setdiff(c("sl", "events"), names(mp_data)))) > 0) {
    stop(paste("Error: The following required names are missing in mp_data:", paste(missing_names, collapse = ", ")))
  }

  adsl <- mp_data$sl %>% dplyr::select(.data$subjectid,.data$start_time,.data$end_time,.data$ref_date)
  events <- mp_data$events

  # Read data ----
  if (is.data.frame(path_data)) {
    data <- path_data
  } else if (!is.null(path_data)) {
    tryCatch(data <- read_dataset(path_data), error = function(e) {
      stop("Error reading the dataset: ", e$message)
    })
  } else {
    stop("Please provide a valid dataset or file path.")
  }

  data <- data %>%
    #Filter data
    {if(!is.null(data_filter)) dplyr::filter(., !!!rlang::parse_exprs(data_filter)) else .} %>%
    #rename and relocate id-variable.
    dplyr::mutate(subjectid = as.numeric(gsub("[^0-9.]", "", as.character(!!rlang::sym(id))))) %>%
    dplyr::filter(.data$subjectid %in% adsl$subjectid) %>%
    dplyr::left_join(adsl %>% dplyr::select(.data$subjectid,.data$start_time,.data$end_time,.data$ref_date), by="subjectid") %>% # Join with ADSL for TRTSDT
    dplyr::relocate(.data$subjectid,.data$ref_date)

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

  keep_vars <- keep_vars[toupper(keep_vars) %in% toupper(colnames(data))]
  # Provide the appropriate message
  if (!(is.null(keep_vars) | any(is.na(keep_vars)))) {
    message(sprintf("Keep additional variables: %s", paste0(keep_vars, collapse = ", ")))
    # Add missing columns as NA
    for (var in setdiff(keep_vars, colnames(events))) {
      events[[var]] <- NA
      if (!is.null(mp_data$events)) {
        mp_data$events[[var]] <- NA
      }
    }
  }

  events_tmp <- NULL
  for(i in 1:length(param)){
    entry <- param[[i]]
    pre <- prefix[[i]]
    data_tmp <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(entry[1])), !!rlang::sym(entry[1]) != "") %>%
      dplyr::filter(!is.na(!!rlang::sym(entry[2])), !!rlang::sym(entry[2]) != "") %>%
      dplyr::mutate(event_group = paste0(pre[1], !!!rlang::syms(entry)[1]),
                    event = paste0(pre[2], !!!rlang::syms(entry)[2])) %>%
      dplyr::select(.data$subjectid, .data$event_group, .data$event, !!rlang::sym(event_start), !!rlang::sym(event_end), .data$ref_date, .data$start_time, .data$end_time, !!!rlang::syms(keep_vars)) %>%
      dplyr::filter(!is.na(!!rlang::sym(event_start))) %>%
      dplyr::arrange(.data$subjectid, .data$event_group, .data$event, !!rlang::sym(event_start), !!rlang::sym(event_end)) %>%
      dplyr::group_by(.data$subjectid, .data$event_group, .data$event) %>%
      dplyr::mutate(event_start_time=as.integer(!!rlang::sym(event_start)-.data$ref_date+1)) %>%# Calculate start time of adverse events
      dplyr::mutate(event_end_time = dplyr::case_when(
        !is.na(!!rlang::sym(event_end)) ~ as.integer(!!rlang::sym(event_end)-.data$ref_date+1),  # Keep existing End_day if not missing
        TRUE ~ as.integer(!!rlang::sym(event_start)-.data$ref_date+1) # Set event_end_time to start_time, if missing
      )
      ) %>%
      dplyr::ungroup() %>%
      # left censor data if provided
      dplyr::mutate(event_start_time =
                      if (is.null(left_censor)) {
                        .data$event_start_time
                      } else {
                        dplyr::case_when(
                          .data$event_start_time < .data$start_time + left_censor ~ .data$start_time + left_censor - 1,
                          TRUE ~ .data$event_start_time
                        )
                      }) %>%
      dplyr::select(-c(!!rlang::sym(event_start), !!rlang::sym(event_end), .data$ref_date)) %>% # Drop original date columns
      dplyr::relocate(.data$subjectid, .data$event_group, .data$event, .data$event_start_time, .data$event_end_time) %>% # Rearrange columns
      dplyr::distinct()

    events_tmp <- rbind(events_tmp, data_tmp)
  }

  time_to_first <- NULL
  if(calc_time_to_first==TRUE){
    print("calcuating time to first event")
    events_time_to_first <- calc_time_to_first(data = events_tmp)

    mp_data$sl <- mp_data$sl %>%
      dplyr::left_join(events_time_to_first, by="subjectid")

  }

  days_with <- NULL
  if(calc_days_with==TRUE){
    print("calcuating days with event")
    data_days_with <- calc_days_with(data = events_tmp)

    mp_data$sl <- mp_data$sl %>%
      dplyr::left_join(data_days_with, by="subjectid")

  }

  mp_data$events <- rbind(mp_data$events, events_tmp)

  return(mp_data)
}
