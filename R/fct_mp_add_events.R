#' Function to create event level megaplots dataset from ADaM datasets
#'
#' This function processes ADaM datasets to create a comprehensive dataset for event-level analysis with megaplots,
#' including the ability to calculate time to first event or days with event, left censor data and manage additional variables.
#' Processes a single pair of columns (e.g. "AEBODSYS" and "AEDECOD"). New rows are appended to "mp$events".
#' Call multiple times to stack several pairs or domains, then call "finalize_mp_data()"" to create upload
#' data for the megaplots app.
#'
#' @param mp An object from init_mp_object() after add.sl_data().
#' @param path_data A data frame or a file path to the dataset (SAS, CSV, or RData) to be read. File should be ADaM conform.
#' @param event_group Name of the column to use as Megaplots `event_group`.
#' @param event Name of the column to use as Megaplots `event`.
#' @param id A string representing the subject identifier column name in `path_data`. Default is "USUBJID".
#' @param data_filter A string for filtering the dataset using dplyr syntax. Default is NULL.
#' @param prefix_group Prefix applied to values from `event_group`.
#' @param prefix_event Prefix applied to values from `event`.
#' @param event_start A character vector of possible event start date/time column names.
#' @param event_end A character vector of possible event end date/time column names.
#' @param calc_time_to_first A logical indicating whether to calculate the time to the first event. Default is FALSE.
#' @param calc_days_with A logical indicating whether to calculate the days with the event. Default is FALSE.
#' @param left_censor A numeric value specifying the left censoring time. Default NULL means no left cesnoring will be performed.
#' @param keep_vars A character vector of additional event level variables to keep in the final dataset.
#'
#' @return A modified list containing updated "sl" and "events" entries with the processed event-level data.
#' @export
add.events <- function(
  mp,
  path_data,
  event_group,
  event,
  id = "USUBJID",
  data_filter = NULL,
  prefix_group = "",
  prefix_event = "",
  event_start = c("ASTDT", "AESTDT", "ADY"),
  event_end = c("AENDT", "AEENDT"),
  calc_time_to_first = FALSE,
  calc_days_with = FALSE,
  left_censor = NULL,
  keep_vars = NULL
) {
  if (!inherits(mp, "mp_data_builder")) {
    stop("`mp` must be an object created by init_mp_object().", call. = FALSE)
  }
  if (is.null(mp$sl)) {
    stop(
      "Subject-level data is missing; call add.sl_data() first.",
      call. = FALSE
    )
  }

  #set global variable . to NULL to
  #avoid note: no visible binding for global variable '.' when
  # performing package check (via devtools)
  . <- NULL

  adsl <- mp$sl %>%
    dplyr::select(
      .data$subjectid,
      .data$start_time,
      .data$end_time,
      .data$ref_date
    )

  # Read data ----
  if (is.data.frame(path_data)) {
    data <- path_data
  } else if (!is.null(path_data)) {
    data <- tryCatch(
      read_dataset(path_data),
      error = function(e) stop("Error reading the dataset: ", e$message)
    )
  } else {
    stop("Please provide a valid dataset or file path.")
  }

  # Check if the subject identifier is present
  if (!id %in% colnames(data)) {
    stop(sprintf("The specified id '%s' is not a column in the dataset.", id))
  }

  eg_col <- resolve_colname(event_group, colnames(data))
  ev_col <- resolve_colname(event, colnames(data))
  message("Event group column: ", eg_col, ", event column: ", ev_col)

  data <- data %>%
    #Filter data
    {
      if (!is.null(data_filter)) {
        dplyr::filter(., !!!rlang::parse_exprs(data_filter))
      } else {
        .
      }
    } %>%
    #rename and relocate id-variable.
    dplyr::mutate(
      subjectid = as.numeric(gsub(
        "[^0-9.]",
        "",
        as.character(!!rlang::sym(id))
      ))
    ) %>%
    dplyr::filter(.data$subjectid %in% adsl$subjectid) %>%
    dplyr::left_join(adsl, by = "subjectid") %>% # Join with ADSL for TRTSDT
    dplyr::relocate(.data$subjectid, .data$ref_date)

  event_start <- resolve_first_match(event_start, colnames(data))
  message("Event start date/time: ", event_start)

  event_end <- resolve_first_match(event_end, colnames(data))
  message("Event end date/time: ", event_end)

  if (!is.null(keep_vars)) {
    keep_vars <- keep_vars[toupper(keep_vars) %in% toupper(colnames(data))]
    if (length(keep_vars)) {
      message(sprintf(
        "Keep additional variables: %s",
        paste0(keep_vars, collapse = ", ")
      ))
    } else {
      keep_vars <- character(0)
    }
  } else {
    keep_vars <- character(0)
  }

  #   # Add missing columns as NA
  #   for (var in setdiff(keep_vars, colnames(events))) {
  #     events[[var]] <- NA
  #     if (!is.null(mp_data$events)) {
  #       mp_data$events[[var]] <- NA
  #     }
  #   }
  # }

  data_tmp <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(eg_col)), !!rlang::sym(eg_col) != "") %>%
    dplyr::filter(!is.na(!!rlang::sym(ev_col)), !!rlang::sym(ev_col) != "") %>%
    dplyr::mutate(
      event_group = paste0(prefix_group, !!rlang::sym(eg_col)),
      event = paste0(prefix_event, !!rlang::sym(ev_col))
    ) %>%
    dplyr::select(
      .data$subjectid,
      .data$event_group,
      .data$event,
      !!rlang::sym(event_start),
      !!rlang::sym(event_end),
      .data$ref_date,
      .data$start_time,
      .data$end_time,
      tidyselect::any_of(keep_vars)
    ) %>%
    dplyr::filter(!is.na(!!rlang::sym(event_start))) %>%
    dplyr::arrange(
      .data$subjectid,
      .data$event_group,
      .data$event,
      !!rlang::sym(event_start),
      !!rlang::sym(event_end)
    ) %>%
    dplyr::group_by(.data$subjectid, .data$event_group, .data$event) %>%
    dplyr::mutate(
      event_start_time = as.integer(
        !!rlang::sym(event_start) - .data$ref_date + 1L
      )
    ) %>% # Calculate start time of adverse events
    dplyr::mutate(
      event_end_time = dplyr::case_when(
        !is.na(!!rlang::sym(event_end)) ~ as.integer(
          !!rlang::sym(event_end) - .data$ref_date + 1L
        ), # Keep existing End_day if not missing
        TRUE ~ as.integer(!!rlang::sym(event_start) - .data$ref_date + 1L) # Set event_end_time to start_time, if missing
      )
    ) %>%
    dplyr::ungroup() %>%
    # left censor data if provided
    dplyr::mutate(
      event_start_time = if (is.null(left_censor)) {
        .data$event_start_time
      } else {
        dplyr::case_when(
          .data$event_start_time <
            .data$start_time + left_censor ~ .data$start_time +
            left_censor -
            1L,
          TRUE ~ .data$event_start_time
        )
      }
    ) %>%
    dplyr::select(
      -c(!!rlang::sym(event_start), !!rlang::sym(event_end), .data$ref_date)
    ) %>% # Drop original date columns
    dplyr::relocate(
      .data$subjectid,
      .data$event_group,
      .data$event,
      .data$event_start_time,
      .data$event_end_time
    ) %>% # Rearrange columns
    dplyr::distinct()

  time_to_first <- NULL
  if (calc_time_to_first) {
    print("calcuating time to first event")
    time_to_first <- calc_time_to_first(data = data_tmp)

    mp$sl <- mp$sl %>%
      dplyr::left_join(time_to_first, by = "subjectid")
  }

  days_with <- NULL
  if (calc_days_with) {
    print("calcuating days with event")
    days_with <- calc_days_with(data = data_tmp)

    mp$sl <- mp$sl %>%
      dplyr::left_join(days_with, by = "subjectid")
  }

  mp$events <- rbind(mp$events, data_tmp)

  return(mp)
}
