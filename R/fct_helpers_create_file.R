#' Helper function to calculate time to first event and time to first event group
#'
#' This function calculates the time to the first occurrence of specific events
#' and event groups for each subject in the provided dataset. It can compute the
#' time for individual events as well as for event groups, based on the specified parameters.
#'
#' @param data A data frame containing event data with columns for subject ID,
#' event group, event, event start time, and event end time.
#' @param calc_event_group Logical indicating whether to calculate time to the first
#' event group. Default is TRUE.
#' @param calc_event Logical indicating whether to calculate time to the first
#' individual event. Default is TRUE.
#' @param subjectid Column name for subject ID in the data frame.
#' @param event_group Column name for event group in the data frame.
#' @param event Column name for event in the data frame.
#' @param event_start_time Column name for event start time in the data frame.
#' @param event_end_time Column name for event end time in the data frame.
#'
#' @return A data frame with the time to the first occurrence of each subject's
#' events and event groups.
#' @export
calc_time_to_first <- function(data,
                          calc_event_group = TRUE,
                          calc_event = TRUE,
                          subjectid = subjectid,
                          event_group = event_group,
                          event = event,
                          event_start_time = event_start_time,
                          event_end_time = event_end_time){

  if(calc_event_group == FALSE & calc_event == FALSE){
    stop("Error: One of calc_event_group and calc_event must be TRUE.")
  }
  if(calc_event == TRUE){
    data_time_to_first <- data %>%
      dplyr::arrange(subjectid,event_group,event,event_start_time) %>%
      dplyr::group_by(subjectid,event_group,event) %>%
      dplyr::summarize(first = min(as.numeric(event_start_time), na.rm = TRUE), .groups="drop") %>%
      dplyr::distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group),
                    event = gsub("[[:punct:][:space:]]+", "_", event)) %>%
      tidyr::pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group","event"),
        names_prefix = "ttf_",
        names_sep = "_",
        values_from = "first"
      )
  }

  if(calc_event_group == TRUE){
    data_time_to_first_group <- data %>%
      dplyr::select(-event) %>%
      dplyr::arrange(subjectid,event_group,event_start_time) %>%
      dplyr::group_by(subjectid,event_group) %>%
      dplyr::summarize(first = min(as.numeric(event_start_time), na.rm = TRUE), .groups="drop") %>%
      dplyr::distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group)) %>%
      tidyr::pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group"),
        names_prefix = "ttf_",
        names_sep = "_",
        values_from = "first"
      )
    if(calc_event == TRUE){
      data_time_to_first <- data_time_to_first %>%
        dplyr::left_join(data_time_to_first_group, by="subjectid")
    } else {
      data_time_to_first <- data_time_to_first_group
    }
  }
  return(data_time_to_first)
}

#' Helper function to calculate days with event and days with event group
#'
#' This function calculates the number of days with specific events
#' for each subject in the provided dataset. It can compute days for individual
#' events as well as for event groups, based on the specified parameters.
#'
#' @param data A data frame containing event data with columns for subject ID,
#' event group, event, event start time, and event end time.
#' @param calc_event_group Logical indicating whether to calculate days for event groups. Default is TRUE.
#' @param calc_event Logical indicating whether to calculate days for individual events. Default is TRUE.
#' @param subjectid Column name for subject ID in the data frame.
#' @param event_group Column name for event group in the data frame.
#' @param event Column name for event in the data frame.
#' @param event_start_time Column name for event start time in the data frame.
#' @param event_end_time Column name for event end time in the data frame.
#'
#' @return A data frame with the number of days associated with each subject's
#' events and event groups.
#' @export
calc_days_with <- function(data,
                      calc_event_group = TRUE,
                      calc_event = TRUE,
                      subjectid = subjectid,
                      event_group = event_group,
                      event = event,
                      event_start_time = event_start_time,
                      event_end_time = event_end_time){

  if(calc_event_group == FALSE & calc_event == FALSE){
    stop("Error: One of calc_event_group and calc_event must be TRUE.")
  }
  if(calc_event == TRUE){
    data_days_with <- data %>%
      dplyr::arrange(subjectid,event_group,event,event_start_time,event_end_time) %>%
      dplyr::mutate(days = as.numeric(event_end_time) - as.numeric(event_start_time) + 1) %>%
      dplyr::group_by(subjectid,event_group,event) %>%
      dplyr::summarize(days_with = sum(.data$days, na.rm=TRUE),.groups="drop") %>%
      dplyr::distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group),
                    event = gsub("[[:punct:][:space:]]+", "_", event)) %>%
      tidyr::pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group","event"),
        names_prefix = "dw_",
        names_sep = "_",
        values_from = "days_with",
        values_fill = 0
      )
  }
  if(calc_event_group == TRUE){
    data_days_with_group <- data %>%
      dplyr::select(-event) %>%
      dplyr::arrange(subjectid,event_group,event_start_time, event_end_time) %>%
      dplyr::mutate(days = as.numeric(event_end_time) - as.numeric(event_start_time) + 1) %>%
      dplyr::group_by(subjectid,event_group) %>%
      dplyr::summarize(days_with = sum(.data$days, na.rm=TRUE),.groups="drop") %>%
      dplyr::distinct() %>%
      dplyr::mutate(event_group = gsub("[[:punct:][:space:]]+", "_", event_group)) %>%
      tidyr::pivot_wider(
        id_cols="subjectid",
        names_from = c("event_group"),
        names_prefix = "dw_",
        names_sep = "_",
        values_from = "days_with",
        values_fill = 0
      )
    if(calc_event == TRUE){
      data_days_with <- data_days_with %>%
        dplyr::left_join(data_days_with_group, by="subjectid")
    } else {
      data_days_with <- data_days_with_group
    }
  }
  return(data_days_with)
}

#' Helper function to read datasets
#'
#' @param path Path to a file (.sas7bdat, .csv, .rdata). If .rdata-format the file should best contain only one dataset.
#'
#' @export
read_dataset <- function(path) {
  # Check the file extension to determine how to read the file
  if (grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    return(haven::read_sas(path)) # Read SAS file
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(readr::read_csv(path)) # Read CSV file
  } else if (grepl("\\.RData$", path, ignore.case = TRUE)) {
    name_data <- load(path) # Load RData file
    return(get(ls()[ls() == name_data]))
  } else {
    stop("Unsupported file format. Please provide a SAS, CSV, or RData file.") # Error for unsupported format
  }
}
