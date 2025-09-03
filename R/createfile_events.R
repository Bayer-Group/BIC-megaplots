#### To do ####
# Create On treatment variable
# Add prefix option for event_group and event and make values more readable


# Function to create event level megaplots dataset from ADaM datasets
library(dplyr)
library(tidyr)
library(haven)
library(purrr)
library(lubridate)

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
                          event_end = c("AENDT","AEENDT")
){
  # Check if mp_data is a list
  if (!is.list(mp_data)) {
    stop("Error: mp_data must be a list.")
  }

  # Check if mp_data has sl and events entry
  if (length(setdiff(c("sl", "events"), names(mp_data))) > 0) {
    stop(paste("Error: The following required names are missing in mp_data:",
               paste(setdiff(c("sl", "events"), names(mp_data)), collapse = ", ")))
  }

  adsl <- mp_data$sl %>% dplyr::select(subjectid,start_time,end_time,ref_date)

  # Read adsl data ----
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

  # Check if param is a list
  if (!is.list(param)) {
    stop("Error: param must be a list.")
  }

  # Check if prefix is NULL or a list
  if (!is.null(prefix) && !is.list(prefix)) {
    stop("Error: prefix must be a list or NULL.")
  }

  # Filter data and process
  data <- data %>%
    {if(!is.null(data_filter)) dplyr::filter(., !!!rlang::parse_exprs(data_filter)) else .} %>%
    dplyr::mutate(subjectid = as.numeric(gsub("[^0-9.]", "", as.character(!!sym(id))))) %>%
    dplyr::filter(subjectid %in% adsl$subjectid) %>%
    dplyr::left_join(adsl %>% select(subjectid,start_time,end_time,ref_date), by="subjectid") %>% # Join with ADSL for TRTSDT
    dplyr::relocate(subjectid,ref_date)

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

  events <- NULL
  for(entry in param){
    data_tmp <- data %>%
      mutate(event_group = paste(entry[1], ": ", !!!syms(entry)[1], " (by ", entry[2], ")"),
             event = paste(!!!syms(entry)[2], " (", entry[2], ")")) %>%
      dplyr::select(subjectid, event_group, event, !!sym(event_start), !!sym(event_end), ref_date, start_time, end_time) %>%
      dplyr::filter(!is.na(!!sym(event_start))) %>%
      arrange(subjectid, event_group, event, !!sym(event_start), !!sym(event_end)) %>%
      group_by(subjectid, event_group, event) %>%
      mutate(
        event_start_time=as.integer(!!sym(event_start)-ref_date+1), # Calculate start time of adverse events
        event_end_time = case_when(
          !is.na(!!sym(event_end)) ~ as.integer(!!sym(event_end)-ref_date+1),  # Keep existing End_day if not missing
          TRUE ~ as.integer(!!sym(event_start)-ref_date+1) # Set event_end_time to start_time, if missing
        )
      ) %>%
      dplyr::select(-c(!!sym(event_start), !!sym(event_end), ref_date)) %>% # Drop original date columns
      relocate(subjectid, event_group, event, event_start_time, event_end_time) %>% # Rearrange columns
      distinct()

    events <- bind_rows(events, data_tmp)
  }

  mp_data$events <- bind_rows(mp_data$events, events)
  return(mp_data)
}

