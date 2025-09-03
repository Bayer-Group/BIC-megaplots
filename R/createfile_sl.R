#### To do ####
# Insert checks, warnings and messages
# enable csv and R-dataset as input
# enable function call with input data from environment
# insert detailed comments
# describe use of createFile functions

# Function to create subject level megaplots dataset from ADSL (ADaM) dataset
library(dplyr)
library(haven)
library(purrr)
library(lubridate)

createFile.sl <- function(path_adsl,
                         id="USUBJID",
                         data_filter=NULL,
                         display_start_date=c("REFSTDT","RFSTDT","RFSTDTC","RFICDT","RANDDT","TRTSTDT"),
                         display_end_date=c("REFENDT","RFENDTC","RFENDT", "LVDT", "WDICDT"),
                         relative_day_1=c("TRTSTDT","TRTSDT"),
                         trt=TRUE,
                         trtstdt=c("TRTSTDT","TRTSDT"),
                         trtendt=c("TRTENDT","TRTEDT")){
options(scipen=999)
# Input
# path_adsl    Path to the adsl-dataset
# data_filter  Subset dataset accoring to the filter conditions.
#              Conditions should be wrapped in '' and concatenated by c()
# id           unique subject identifier.
# display_start_date
# display_end_date
# trtstdt
# trtendt
#
# Output
# dataframe with id-variable and categorized (previously continuous) variables

  # Read adsl data ----
  if (is.data.frame(path_adsl)) {
    adsl <- path_adsl
  } else if (!is.null(path_adsl)) {
    tryCatch({
      if (grepl("\\.sas7bdat$", path_adsl, ignore.case = TRUE)) {
        adsl <- haven::read_sas(path_adsl)
      } else if (grepl("\\.csv$", path_adsl, ignore.case = TRUE)) {
        adsl <- read.csv(path_adsl)
      } else if (grepl("\\.RData$", path_adsl, ignore.case = TRUE)) {
        load(path_adsl)
        # Assuming the data frame is named 'adsl' in the RData file
        adsl <- get(ls()[1])  # Get the first object in the environment
      } else {
        stop("Unsupported file format. Please provide a SAS, CSV, or RData file.")
      }
    }, error = function(e) {
      stop("Error reading the dataset: ", e$message)
    })
  } else {
    stop("Please provide a valid dataset or file path.")
  }

  # Parameter validation
  if (!id %in% colnames(adsl)) {
    stop(sprintf("The specified id '%s' is not a column in the dataset.", id))
  }

  adsl <- adsl %>%
    #Filter data
    {if(!is.null(data_filter)) dplyr::filter(., !!!rlang::parse_exprs(data_filter)) else .} %>%
    #rename and relocate id-variable.
    dplyr::mutate(subjectid = as.numeric(gsub("[^0-9.]", "", as.character(!!sym(id))))) %>%
    dplyr::relocate(subjectid)

    display_start_date <- display_start_date[toupper(display_start_date) %in% toupper(colnames(adsl))][1]
    # Provide the appropriate message
    if (!(is.null(display_start_date) | is.na(display_start_date))) {
      message(sprintf("Reference start date: %s", display_start_date))
    } else {
      message("None of the input parameters in display_start_date are present as column names in the data.")
    }

    display_end_date <- display_end_date[toupper(display_end_date) %in% toupper(colnames(adsl))]
    # Provide the appropriate message
    if (!(length(display_end_date)==0)) {
      message(sprintf("Reference end date: Maximum of %s", paste(display_end_date, collapse = ", ")))
    } else {
      message("None of the input parameters in display_end_date are present as column names in the data.")
    }

    relative_day_1 <- relative_day_1[toupper(relative_day_1) %in% toupper(colnames(adsl))][1]
    # Provide the appropriate message
    if (!(is.null(relative_day_1) | is.na(relative_day_1))) {
      message(sprintf("Relative day 1: %s", relative_day_1))
    } else {
      message("None of the input parameters in relative_day_1 are present as column names in the data.")
    }

    if(trt==T){
      trtstdt <- trtstdt[toupper(trtstdt) %in% toupper(colnames(adsl))][1]
      # Provide the appropriate message
      if (!(is.null(trtstdt) | is.na(trtstdt))) {
        message(sprintf("Treatment start date: %s", trtstdt))
      } else {
        message("None of the input parameters in trtstdt are present as column names in the data.")
      }

      trtendt <- trtendt[toupper(trtendt) %in% toupper(colnames(adsl))][1]
      # Provide the appropriate message
      if (!(is.null(trtendt) | is.na(trtendt))) {
        message(sprintf("Treatment end date: %s", trtendt))
      } else {
        message("None of the input parameters in trtendt are present as column names in the data.")
      }
    }

  adsl <- adsl %>%
    dplyr::mutate(ref_date = !!sym(relative_day_1)) %>%
    dplyr::mutate(
      across(all_of(c(display_start_date,display_end_date,relative_day_1,{if(trt){c(trtstdt, trtendt)}})),~as.Date(.x)),
      start_time = as.integer(!!sym(display_start_date) - !!sym(relative_day_1) + 1), # Calculate start time
      end_time = as.integer(pmax(!!!syms(display_end_date), na.rm = TRUE) - !!sym(relative_day_1) + 1) # Calculate end time
    #  #{if(trt==T) treatment_duration = as.integer(!!sym(trtendt) - !!sym(trtstdt) + 1)} # Calculate treatment duration
     ) %>%
    { if (trt) {
        dplyr::mutate(., treatment_duration = as.integer(!!sym(trtendt) - !!sym(trtstdt) + 1)) %>%
        dplyr::relocate(., "subjectid", "start_time", "end_time", "ref_date", "treatment_duration")  # Rearrange columns
      } else {
        dplyr::relocate(., "subjectid", "start_time", "end_time", "ref_date")  # Rearrange columns
      }
    }

 return(list(sl=adsl,events=NULL))
}
