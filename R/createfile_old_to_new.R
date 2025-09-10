# Function to create megaplots file for rebuild (v??? and later)
# from prepared datasets for old version (v??? and earlier)
#
#
# Parameters:
# - path_data: Path to the subject-level dataset
# - path_data_b: Path to the events dataset
# - subjectid: Name of the subject ID column (as a string)
# - event_time: Name of the event time column (as a string)
#
# Returns: A data frame ready for the new megaplots version

# Helper function to read datasets
#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_dataset <- function(path) {
  if (grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    return(haven::read_sas(path))
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(readr::read_csv(path))
  } else if (grepl("\\.RData$", path, ignore.case = TRUE)) {
    load(path)
    return(get(ls()[1]))  # Assuming the data frame is named 'data'
  } else {
    stop("Unsupported file format. Please provide a SAS, CSV, or RData file.")
  }
}

#' Function to create megaplots file for rebuild (v??? and later) from prepared datasets for old version (v??? and earlier)
#'
#' @param path_data Path to the subject-level dataset
#' @param path_data_b Path to the events dataset
#' @param subjectid Name of the subject ID column (as a string)
#' @param event_time Name of the event time column (as a string)
#'
#' @return
#' @export
#'
#' @examples
createfile.old_to_new <- function(path_data,
                                  path_data_b=NULL,
                                  subjectid="subjectid",
                                  event_time){

  # Validate inputs
  if (missing(event_time)) stop("Please provide the event_time parameter.")

  # Read (A) data ----
  if (is.data.frame(path_data)) {
    data_A <- path_data
  } else if (!is.null(path_data)) {
    tryCatch(read_dataset(path_data), error = function(e) {
      stop("Error reading the dataset: ", e$message)
    })
  } else {
    stop("Please provide a valid subject level dataset or file path.")
  }

  # Read (B) data ----
  if(!is.null(path_data_b)){
    if (is.data.frame(path_data_b)) {
      data_B <- path_data_b
    } else if (!is.null(path_data_b)) {
      tryCatch(read_dataset(path_data_b), error = function(e) {
        stop("Error reading the dataset: ", e$message)
      })
    } else {
      stop("Please provide a valid events dataset or file path.")
    }
  }

  #Process events data
  data_B_long <- data_B %>%
    dplyr::mutate(across(-c(subjectid, !!sym(event_time)), ~ na_if(., ""))) %>%
    tidyr::pivot_longer(
      cols=-c(subjectid, !!sym(event_time)),
      names_to = "event_group",
      values_to = "event"
    ) %>%
    arrange(subjectid,event_group,!!sym(event_time),event) %>%
    group_by(subjectid,event_group,event) %>%
    mutate(group = cumsum(c(1, diff(!!sym(event_time)) > 1))) %>%
    group_by(subjectid,event_group,event,group) %>%
    dplyr::mutate(event_start=min(!!sym(event_time)),
                  event_end=max(!!sym(event_time))) %>%
    ungroup() %>%
    select(-c(group,!!sym(event_time))) %>%
    distinct() %>%
    dplyr::filter(!is.na(event))


  return(data_A %>%
           right_join(data_B_long, by="subjectid") %>%
           relocate(subjectid,start_time,end_time,event_group,event,event_start,event_end))
}
