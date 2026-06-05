#' Add subject-level data to megaplots dataset builder
#'
#' Optional start of the megaplots data builder when subject-level ADSL data is available.
#' This function reads an ADSL dataset from various file formats (SAS, CSV, RData) and processes it
#' to create a subject-level dataset suitable for generating megaplots. It allows for filtering of
#' the dataset based on specified conditions and computes relevant date variables.
#'
#' @details
#' **Subject identifier (`id`):** Must match a column name in `sl_data` **exactly** (case-sensitive).
#' Subject keys are coerced to numeric by keeping only digits and periods (`0123456789.`); distinct labels
#' that map to the same numeric id can **collide**. Avoid by using unique digit patterns or pre-mapping keys.
#'
#' @param mp_builder A builder object from a previous pipeline step, or `NULL` (default) on the first call.
#' @param sl_data Path to the adsl-dataset (can be a dataframe or file path).
#' @param id Column name of the unique subject identifier (default `"USUBJID"`). Must match `sl_data`
#'   column names exactly.
#' @param data_filter Optional; passed to [rlang::parse_exprs()] for [dplyr::filter()]. Example:
#'   `data_filter = "SAFFL == \"Y\""`.
#' @param display_start_date Columns to be used as start dates (date of the first contact to the participant). If a vector with multiple column names is given, the first one present in the provided dataset with at least one non-missing value is used. Per default, the following column names are checked: "REFSTDT","RFSTDT","RFSTDTC","RFICDT","RANDDT","TRTSTDT"
#' @param display_end_date Columns to be used as end dates (date of the last contact to the participant. If a vector with multiple column names is given, the maximum of all present and non-missing columns in the provided dataset is used. Per default, the following column names are checked: "REFENDT","RFENDTC","RFENDT", "LVDT", "WDICDT"
#' @param relative_day_1 Columns to be used as the reference for relative day 1. If a vector with multiple column names is given, the first one present in the provided dataset with at least one non-missing value is used. Per default, the following column names are checked: "TRTSTDT", "TRTSDT"
#' @param trtstdt Columns to be used as treatment start dates.
#' @param trtendt Columns to be used as treatment end dates.
#'
#' @return A list containing the processed subject level dataset and NULL for events.
#' @examples
#' \dontrun{
#' library(Megaplots)
#' library(dplyr)
#' library(safetyData)
#'
#' data(adam_adsl, adam_adae, adam_adlbc, package = "safetyData")
#'
#' mp_data <- add_sl_data(adam_adsl) %>%
#'   add_events(
#'     adam_adae,
#'     event_group = "AEBODSYS",
#'     event = "AEDECOD",
#'     prefix_group = "SOC: ",
#'     prefix_event = "PT: ",
#'     calc_time_to_first = TRUE,
#'     calc_days_with = TRUE
#'   ) %>%
#'   add_events(
#'     adam_adae,
#'     event_group = "CQ01NAM",
#'     event = "AETERM",
#'     prefix_group = "CQ: "
#'   ) %>%
#'   add_events(
#'     adam_adlbc,
#'     event_group = "PARAM",
#'     event = "LBNRIND",
#'     event_start = "ADT",
#'     event_end = "ADT",
#'     prefix_group = "Lab: "
#'   ) %>%
#'   finalize_mp_object(
#'     event_group_label_case = "title",
#'     event_label_case = "title"
#'   )
#' }
#' @export
add_sl_data <- function(
  mp_builder = NULL,
  sl_data = NULL,
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
  if (
    is.null(sl_data) &&
      !is.null(mp_builder) &&
      !inherits(mp_builder, "mp_data_builder")
  ) {
    sl_data <- mp_builder
    mp_builder <- NULL
  }
  if (is.null(mp_builder)) {
    mp_builder <- init_mp_object()
  }
  if (is.null(sl_data)) {
    stop("Please provide a valid dataset or file path.", call. = FALSE)
  }
  if (!inherits(mp_builder, "mp_data_builder")) {
    stop(
      "`mp_builder` must be a megaplots data builder (`mp_data_builder`).",
      call. = FALSE
    )
  }

  options(scipen = 999) # Set options to avoid scientific notation in numbers
  #set global variable . to NULL to avoid note:
  # no visible binding for global variable '.' when performing package check (via devtools)
  . <- NULL

  # Read adsl data ----
  if (is.data.frame(sl_data)) {
    adsl <- sl_data # If input is a dataframe, assign it directly
  } else if (!is.null(sl_data)) {
    adsl <- tryCatch(
      read_dataset(sl_data),
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
  display_start_date <- resolve_first_match(
    display_start_date,
    colnames(adsl),
    data = adsl,
    label = "display_start_date"
  )
  message(sprintf("Reference start date: %s", display_start_date)) # Message for reference start date

  # Determine the appropriate reference end dates
  display_end_date <- resolve_all_useful_matches(
    display_end_date,
    colnames(adsl),
    data = adsl,
    label = "display_end_date"
  )
  message(sprintf(
    "Reference end date: Maximum of %s",
    paste(display_end_date, collapse = ", ")
  )) # Message for reference end date

  # Determine the appropriate relative day 1
  relative_day_1 <- resolve_first_match(
    relative_day_1,
    colnames(adsl),
    data = adsl,
    label = "relative_day_1"
  )
  message(sprintf("Relative day 1: %s", relative_day_1)) # Message for relative day 1

  # If treatment dates are to be included
  if (is.null(trtstdt) || is.null(trtendt)) {
    message(
      "Treatment start date (trtstdt) and/or treatment end date (trtendt) are not provided.",
      "Treatment duration is not calculated"
    )
  }

  # Collect all date columns to be processed
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
        # Use the latest available end date when several candidates exist
        pmax(!!!rlang::syms(display_end_date), na.rm = TRUE) -
          !!rlang::sym(relative_day_1) + 1L
      ) # Calculate end time
    )

  # If treatment start and end dates are provided, calculate treatment duration
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

  mp_builder$sl <- adsl
  return(mp_builder)
}
