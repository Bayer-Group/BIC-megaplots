#' Add subject-level data to megaplots dataset builder
#'
#' This function reads an ADSL dataset from various file formats (SAS, CSV, RData) and processes it
#' to create a subject-level dataset suitable for generating megaplots. It allows for filtering of
#' the dataset based on specified conditions and computes relevant date variables.
#'
#' @details
#' **Subject identifier (`id`):** Must match a column name in `path_adsl` **exactly** (case-sensitive).
#' Subject keys are coerced to numeric by keeping only digits and periods (`0123456789.`); distinct labels
#' that map to the same numeric id can **collide**. Avoid by using unique digit patterns or pre-mapping keys.
#'
#' @param mp An object of class "mp_data_builder", generated init_mp_object() function.
#' @param path_adsl Path to the adsl-dataset (can be a dataframe or file path).
#' @param id Column name of the unique subject identifier (default `"USUBJID"`). Must match `path_adsl`
#'   column names exactly.
#' @param data_filter Optional; passed to [rlang::parse_exprs()] for [dplyr::filter()]. Example:
#'   `data_filter = "SAFFL == \"Y\""`.
#' @param display_start_date Columns to be used as start dates (date of the first contact to the participant). If a vector with multiple column names is given, the first one present in the provided dataset is used. Per default, the following column names are checked: "REFSTDT","RFSTDT","RFSTDTC","RFICDT","RANDDT","TRTSTDT"
#' @param display_end_date Columns to be used as end dates (date of the last contact to the participant. If a vector with multiple column names is given, the maximum of all present in the provided dataset is used. Per default, the following column names are checked: "REFENDT","RFENDTC","RFENDT", "LVDT", "WDICDT"
#' @param relative_day_1 Columns to be used as the reference for relative day 1. If a vector with multiple column names is given, the first one present in the provided dataset is used. Per default, the following column names are checked: "TRTSTDT", "TRTSDT"
#' @param trtstdt Columns to be used as treatment start dates.
#' @param trtendt Columns to be used as treatment end dates.
#'
#' @return A list containing the processed subject level dataset and NULL for events.
#' @export
add.sl_data <- function(
  mp,
  path_adsl,
  id = "USUBJID",
  data_filter = NULL,
  display_start_date = c(
    "REFSTDT",
    "RFSTDT",
    "RFSTDTC",
    "RFICDT",
    "RANDDT",
    "TRTSTDT"
  ),
  display_end_date = c("REFENDT", "RFENDTC", "RFENDT", "LVDT", "WDICDT"),
  relative_day_1 = c("TRTSTDT", "TRTSDT"),
  trtstdt = NULL,
  trtendt = NULL
) {
  if (!inherits(mp, "mp_data_builder")) {
    stop("`mp` must be an object created by init_mp_object().", call. = FALSE)
  }

  options(scipen = 999) # Set options to avoid scientific notation in numbers
  #set global variable . to NULL to avoid note:
  # no visible binding for global variable '.' when performing package check (via devtools)
  . <- NULL

  # Read adsl data ----
  if (is.data.frame(path_adsl)) {
    adsl <- path_adsl # If input is a dataframe, assign it directly
  } else if (!is.null(path_adsl)) {
    adsl <- tryCatch(
      read_dataset(path_adsl),
      error = function(e) stop("Error reading the dataset: ", e$message) # Catch and display any errors during reading
    )
  } else {
    stop("Please provide a valid dataset or file path.") # Error if no valid input is provided
  }

  # Parameter validation for the unique subject identifier
  if (!id %in% colnames(adsl)) {
    stop(sprintf("The specified id '%s' is not a column in the dataset.", id)) # Error if id not found in dataset
  }

  # Process the dataset
  adsl <- adsl %>%
    #Filter data
    {
      if (!is.null(data_filter)) {
        dplyr::filter(., !!!rlang::parse_exprs(data_filter))
      } else {
        .
      }
    } %>%
    # Create a numeric subject ID and relocate it to the front
    dplyr::mutate(
      subjectid = as.numeric(base::gsub(
        "[^0-9.]",
        "",
        as.character(!!rlang::sym(id))
      ))
    ) %>%
    dplyr::relocate(.data$subjectid)

  # Determine the appropriate reference start date
  display_start_date <- display_start_date[
    toupper(display_start_date) %in% toupper(colnames(adsl))
  ][1]
  if (is.null(display_start_date) || is.na(display_start_date)) {
    stop(
      "None of the input parameters in display_start_date are present as column names in the data."
    )
  }
  message(sprintf("Reference start date: %s", display_start_date)) # Message for reference start date

  # Determine the appropriate reference end dates
  display_end_date <- display_end_date[
    toupper(display_end_date) %in% toupper(colnames(adsl))
  ]
  if (!length(display_end_date)) {
    stop(
      "None of the input parameters in display_end_date are present as column names in the data."
    )
  }
  message(sprintf(
    "Reference end date: Maximum of %s",
    paste(display_end_date, collapse = ", ")
  )) # Message for reference end date

  # Determine the appropriate relative day 1
  relative_day_1 <- relative_day_1[
    toupper(relative_day_1) %in% toupper(colnames(adsl))
  ][1]
  if (is.null(relative_day_1) || is.na(relative_day_1)) {
    stop(
      "None of the input parameters in relative_day_1 are present as column names in the data."
    )
  }
  message(sprintf("Relative day 1: %s", relative_day_1)) # Message for relative day 1

  # If treatment dates are to be included
  if (is.null(trtstdt) || is.null(trtendt)) {
    message(
      "Treatment start date (trtstdt) and/or treatment end date (trtendt) are not provided.",
      "Treatment duration is not calculated"
    )
  }

  date_cols <- c(
    display_start_date,
    display_end_date,
    relative_day_1,
    if (!is.null(trtstdt) && !is.null(trtendt)) c(trtstdt, trtendt)
  )

  # Mutate the dataset to create new date-related columns
  adsl <- adsl %>%
    dplyr::mutate(ref_date = !!rlang::sym(relative_day_1)) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(date_cols), ~ as.Date(.x)), # Convert specified date columns to Date type
      start_time = as.integer(
        !!rlang::sym(display_start_date) - !!rlang::sym(relative_day_1) + 1L
      ), # Calculate start time relative to day 1
      end_time = as.integer(
        pmax(!!!rlang::syms(display_end_date), na.rm = TRUE) -
          !!rlang::sym(relative_day_1) + 1L
      ) # Calculate end time
    )

  if (!is.null(trtstdt) && !is.null(trtendt)) {
    adsl <- adsl %>%
      dplyr::mutate(
        treatment_duration = as.integer(
          !!rlang::sym(trtendt) - !!rlang::sym(trtstdt) + 1L
        )
      ) %>%
      dplyr::relocate(
        .,
        "subjectid",
        "start_time",
        "end_time",
        "ref_date",
        "treatment_duration"
      ) # Rearrange columns if treatment is included
  } else {
    adsl <- adsl %>%
      dplyr::relocate("subjectid", "start_time", "end_time", "ref_date") # Rearrange columns without treatment
  }

  mp$sl <- adsl
  return(mp)
}
