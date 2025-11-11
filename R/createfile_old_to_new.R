# Function to create megaplots file for rebuild (v??? and later)
# from prepared datasets for old version (v??? and earlier)
#

#' This function transforms subject-level and event datasets from an older format
#' to a new megaplots file format, suitable for rebuilding analyses in newer versions.
#'
#' @param path_data Path to the subject-level dataset. This can be a data frame or a file path.
#' @param path_data_b Path to the events dataset. This can be a data frame or a file path. Default NULL assumes event level data is included in "path_data".
#' @param subjectid Name of the subject ID column (as a string). Default is "subjectid".
#' @param event_time Name of the event time column (as a string). This parameter is required.
#'
#' @return A data frame ready for the new megaplots version, containing merged and processed data.
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
    tryCatch(data_A <- read_dataset(path_data), error = function(e) {
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
      tryCatch(data_B <- read_dataset(path_data_b), error = function(e) {
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
