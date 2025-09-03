# Function to create megaplots file for rebuild (v??? and later)
# from prepared datasets for old version (v??? and earlier)

library(tidyverse)
library(haven)

createfile.old_to_new <- function(path_data,
                                  path_data_b=NULL,
                                  subjectid=subjectid,
                                  event_time){


  # Read (A) data ----
  if (is.data.frame(path_data)) {
    data_A <- path_data
  } else if (!is.null(path_data)) {
    tryCatch({
      if (grepl("\\.sas7bdat$", path_data, ignore.case = TRUE)) {
        data_A <- haven::read_sas(path_data)
      } else if (grepl("\\.csv$", path_data, ignore.case = TRUE)) {
        data_A <- read.csv(path_data)
      } else if (grepl("\\.RData$", path_data, ignore.case = TRUE)) {
        load(path_data)
        # Assuming the data frame is named 'data' in the RData file
        data_A <- get(ls()[1])  # Get the first object in the environment
      } else {
        stop("Unsupported file format. Please provide a SAS, CSV, or RData file.")
      }
    }, error = function(e) {
      stop("Error reading the dataset: ", e$message)
    })
  } else {
    stop("Please provide a valid dataset or file path.")
  }

  # Read (B) data ----
  if(!is.null(path_data_b)){
    if (is.data.frame(path_data_b)) {
      data_B <- path_data_b
    } else if (!is.null(path_data_b)) {
      tryCatch({
        if (grepl("\\.sas7bdat$", path_data_b, ignore.case = TRUE)) {
          data_B <- haven::read_sas(path_data_b)
        } else if (grepl("\\.csv$", path_data_b, ignore.case = TRUE)) {
          data_B <- read.csv(path_data_b)
        } else if (grepl("\\.RData$", path_data_b, ignore.case = TRUE)) {
          load(path_data_b)
          # Assuming the data frame is named 'data' in the RData file
          data_B <- get(ls()[1])  # Get the first object in the environment
        } else {
          stop("Unsupported file format. Please provide a SAS, CSV, or RData file.")
        }
      }, error = function(e) {
        stop("Error reading the dataset: ", e$message)
      })
    } else {
      stop("Please provide a valid dataset or file path.")
    }
  }

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
