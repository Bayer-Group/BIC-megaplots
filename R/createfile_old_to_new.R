# Function to create megaplots file for rebuild (v??? and later)
# from prepared datasets for old version (v??? and earlier)

library(tidyverse)
library(haven)

createfile.old_to_new <- function(path_data,
                                  path_data_b=NULL){


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

  return(list(sl=data_A,
              events=data_B))

}


test <- createfile.old_to_new(path_data = "~/MegaPlots/data/mp_a_nirvana.csv",
                              path_data_b = "~/MegaPlots/data/mp_b_nirvana.csv")
test_A <- test$sl
test_B <- test$events %>%
  dplyr::select(subjectid, treat, ev_time, ice_fl, psg_night_nv, ISIB0999_AVAL, ISIB0999_CHG) %>%
  dplyr::mutate(across(c(psg_night_nv, ISIB0999_AVAL, ISIB0999_CHG), ~ na_if(., ""))) %>%
  dplyr::filter(!is.na(psg_night_nv) & !is.na(ISIB0999_AVAL) & !is.na(ISIB0999_CHG))
  pivot_longer


