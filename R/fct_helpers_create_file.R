#' Helper function to calculate time to first event and time to first event group
#'
#' @param data
#' @param calc_event_group
#' @param calc_event
#' @param subjectid
#' @param event_group
#' @param event
#' @param event_start_time
#' @param event_end_time
#'
#' @returns
#' @export
#'
#' @examples
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
#      dplyr::ungroup() %>%
#      dplyr::select(-c(event_start_time, event_end_time, start_time, end_time)) %>%
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
#      dplyr::ungroup() %>%
#      dplyr::select(-c(event_start_time, event_end_time, start_time, end_time)) %>%
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
#' @param data
#' @param calc_event_group
#' @param calc_event
#' @param subjectid
#' @param event_group
#' @param event
#' @param event_start_time
#' @param event_end_time
#'
#' @returns
#' @export
#'
#' @examples
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
      dplyr::summarize(days_with = sum(days, na.rm=TRUE),.groups="drop") %>%
      # dplyr::ungroup() %>%
      # dplyr::select(-c(event_start_time, event_end_time, start_time, end_time, days)) %>%
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
      dplyr::summarize(days_with = sum(days, na.rm=TRUE),.groups="drop") %>%
      # dplyr::ungroup() %>%
      # dplyr::select(-c(event_start_time, event_end_time, start_time, end_time, days)) %>%
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

# Helper function to read datasets
#' Title
#'
#' @param path Path to a file (.sas7bdat, .csv, .rdata). If .rdata-format the file should best contain only one dataset.
#'
#' @return
#' @export
#'
#' @examples
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
