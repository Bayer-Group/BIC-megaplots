#' Function to create a subject-level megaplots dataset from an ADSL (ADaM) dataset
#'
#' This function reads an ADSL dataset from various file formats (SAS, CSV, RData) and processes it
#' to create a subject-level dataset suitable for generating megaplots. It allows for filtering of
#' the dataset based on specified conditions and computes relevant date variables.
#'
#' @param path_adsl Path to the adsl-dataset (can be a dataframe or file path).
#' @param id Unique subject identifier (default: "USUBJID").
#' @param data_filter Subset dataset according to the filter conditions (default: NULL). Using dplyr::filter() syntax. Conditions should be wrapped in '' and concatenated by c(). Example: data_filter='SAFFL == "Y"'
#' @param display_start_date Columns to be used as start dates (date of the first contact to the participant). If a vector with multiple column names is given, the first one present in the provided dataset is used. Per default, the following column names are checked: "REFSTDT","RFSTDT","RFSTDTC","RFICDT","RANDDT","TRTSTDT"
#' @param display_end_date Columns to be used as end dates (date of the last contact to the participant. If a vector with multiple column names is given, the maximum of all present in the provided dataset is used. Per default, the following column names are checked: "REFENDT","RFENDTC","RFENDT", "LVDT", "WDICDT"
#' @param relative_day_1 Columns to be used as the reference for relative day 1.
#' @param trt Logical indicating whether to include treatment dates (default: TRUE).
#' @param trtstdt Columns to be used as treatment start dates.
#' @param trtendt Columns to be used as treatment end dates.
#'
#' @return A list containing the processed subject level dataset and NULL for events.
#' @export
createFile.sl <- function(path_adsl,
                          id="USUBJID",
                          data_filter=NULL,
                          display_start_date=c("REFSTDT","RFSTDT","RFSTDTC","RFICDT","RANDDT","TRTSTDT"),
                          display_end_date=c("REFENDT","RFENDTC","RFENDT", "LVDT", "WDICDT"),
                          relative_day_1=c("TRTSTDT","TRTSDT"),
                          trtstdt=NULL,
                          trtendt=NULL,
                          trt=NULL){
  options(scipen=999) # Set options to avoid scientific notation in numbers
  #set global variable . to NULL to
  #avoid note: no visible binding for global variable '.' when
  # performing package check (via devtools)
  . <- NULL

  # Read adsl data ----
  if (is.data.frame(path_adsl)) {
    adsl <- path_adsl # If input is a dataframe, assign it directly
  } else if (!is.null(path_adsl)) {
    tryCatch(adsl <- read_dataset(path_adsl), error = function(e) {
      stop("Error reading the dataset: ", e$message) # Catch and display any errors during reading
    })
  } else {
    stop("Please provide a valid dataset or file path.") # Error if no valid input is provided
  }

  # Parameter validation for the unique subject identifier
  if (!id %in% colnames(adsl)) {
    stop(sprintf("The specified id '%s' is not a column in the dataset.", id))  # Error if id not found in dataset
  }

  # Process the dataset
  adsl <- adsl %>%
    #Filter data
    {if(!is.null(data_filter)) dplyr::filter(., !!!rlang::parse_exprs(data_filter)) else .} %>%
    # Create a numeric subject ID and relocate it to the front
    dplyr::mutate(subjectid = as.numeric(base::gsub("[^0-9.]", "", as.character(!!rlang::sym(id))))) %>%
    dplyr::relocate(.data$subjectid)

  # Determine the appropriate reference start date
  display_start_date <- display_start_date[toupper(display_start_date) %in% toupper(colnames(adsl))][1]
  if (!(is.null(display_start_date) | is.na(display_start_date))) {
    message(sprintf("Reference start date: %s", display_start_date)) # Message for reference start date
  } else {
    message("None of the input parameters in display_start_date are present as column names in the data.")
  }

  # Determine the appropriate reference end dates
  display_end_date <- display_end_date[toupper(display_end_date) %in% toupper(colnames(adsl))]
  if (!(length(display_end_date)==0)) {
    message(sprintf("Reference end date: Maximum of %s", paste(display_end_date, collapse = ", "))) # Message for reference end date
  } else {
    message("None of the input parameters in display_end_date are present as column names in the data.")
  }

  # Determine the appropriate relative day 1
  relative_day_1 <- relative_day_1[toupper(relative_day_1) %in% toupper(colnames(adsl))][1]
  if (!(is.null(relative_day_1) | is.na(relative_day_1))) {
    message(sprintf("Relative day 1: %s", relative_day_1)) # Message for relative day 1
  } else {
    message("None of the input parameters in relative_day_1 are present as column names in the data.")
  }

  # If treatment dates are to be included
  if(is.null(trtstdt) | is.null(trtendt)){
    message("Treatment start date (trtstdt) and/or treatment end date (trtendt) are not provided. Treatment duration is not calculated")
  }


  # Mutate the dataset to create new date-related columns
  adsl <- adsl %>%
    dplyr::mutate(ref_date = !!rlang::sym(relative_day_1)) %>%
    dplyr::mutate(
      dplyr::across(all_of(c(.data$display_start_date,.data$display_end_date,.data$relative_day_1,{if(!is.null(.data$trtstdt) & !is.null(.data$trtendt)){c(.data$trtstdt, .data$trtendt)}})),~as.Date(.x)), # Convert specified columns to Date type
      start_time = as.integer(!!rlang::sym(display_start_date) - !!rlang::sym(relative_day_1) + 1), # Calculate start time relative to day 1
      end_time = as.integer(pmax(!!!rlang::syms(display_end_date), na.rm = TRUE) - !!rlang::sym(relative_day_1) + 1) # Calculate end time
    ) %>%
    { if (!is.null(trtstdt) & !is.null(trtendt)) {
      dplyr::mutate(., treatment_duration = as.integer(!!rlang::sym(trtendt) - !!rlang::sym(trtstdt) + 1)) %>%
      dplyr::relocate(., "subjectid", "start_time", "end_time", "ref_date", "treatment_duration")  # Rearrange columns if treatment is included
    } else {
      dplyr::relocate(., "subjectid", "start_time", "end_time", "ref_date")  # Rearrange columns without treatment
    }
    }

  return(list(sl=adsl,events=NULL)) # Return the processed subject level dataset and NULL for events
}
