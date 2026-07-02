#' Function to create event level megaplots dataset from ADaM datasets
#'
#' Start for the builder when [add_sl_data()] is not used; otherwise call after subject-level
#' data has been added. See [add_sl_data()] for a full multi-domain pipeline example.
#' This function processes ADaM datasets to create a comprehensive dataset for event-level analysis with megaplots,
#' including the ability to calculate time to first event or days with event, left censor data and manage additional variables.
#' Processes a single pair of columns (e.g. "AEBODSYS" and "AEDECOD"). New rows are appended to "mp_builder$events".
#' Call multiple times to stack several pairs or domains, then call "finalize_mp_object()" to create upload
#' data for the megaplots app.
#'
#' @details
#' **Subject identifier (`id`):** `id` must match a column name in `events_data` exactly (case-sensitive).
#' Subject keys are coerced to numeric by keeping only digits and periods (`0123456789.`); distinct labels
#' that map to the same numeric id can collide. Avoid by using unique digit patterns or pre-mapping keys.
#'
#' **`calc_time_to_first` / `calc_days_with` across multiple `add_events()` calls:** Each run with these
#' flags joins `ttf_*` / `dw_*` columns onto `mp_builder$sl`. Another call can duplicate or rename columns (e.g.
#' `...x` / `...y`) if derived names overlap. Prefer one pair of calls per analysis, or rely on distinct
#' event labels from `prefix_group` / `prefix_event` / domain-specific columns so pivot names do not clash.
#'
#' **Building `mp_builder$sl` from `events_data`:** When `mp_builder$sl` is NULL (i.e. [add_sl_data()] was not used),
#' you must pass `sl_ref_date`: either a column name in `events_data` with the reference date per subject,
#' or a single numeric value when `event_start` / `event_end` are already on the same relative-day scale
#' (e.g. `sl_ref_date = 1`). Only `subjectid` and `ref_date` are added to `mp_builder$sl` (no `start_time` / `end_time`).
#' One row per `subjectid` is kept (first after filtering). Later `add_events()` calls keep only subjects already
#' in `mp_builder$sl`; use [add_sl_data()] first when you need the full population from ADSL or subject-level timeline
#' columns.
#'
#' @param mp_builder A builder object from a previous pipeline step, or `NULL` (default) on the first call.
#'   If `mp_builder$sl` is NULL, `sl_ref_date` is required.
#' @param events_data A data frame or a file path to the dataset (SAS, CSV, or RData) to be read. File should be ADaM conform.
#' @param event_group Character vector of one or more column names used to build Megaplots `event_group`.
#'   When multiple names are provided, values are pasted together.
#' @param event Character vector of one or more column names used to build Megaplots `event`.
#'   When multiple names are provided, values are pasted together.
#' @param id Column name of the subject identifier in `events_data` (default `"USUBJID"`). Must match the data
#'   frame column name exactly.
#' @param data_filter Optional character vector passed to [rlang::parse_exprs()] and used inside
#' @param prefix_group Prefix applied to values from `event_group`.
#' @param prefix_event Prefix applied to values from `event`.
#' @param event_start A character vector of possible event start date/time column names.
#' @param event_end A character vector of possible event end date/time column names.
#' @param calc_time_to_first A logical indicating whether to calculate the time to the first event. Default is FALSE.
#' @param calc_days_with A logical indicating whether to calculate the days with the event. Default is FALSE.
#' @param left_censor A numeric value specifying the left censoring time. Default NULL means no left censoring will be performed.
#' @param keep_vars A character vector of additional event level variables to keep in the final dataset.
#' @param sl_ref_date Required when `mp_builder$sl` is NULL. Character: column name in `events_data` whose values
#'   are the reference value (typically `Date`) for converting event dates to study days. Numeric
#'   length 1: constant reference on the same scale as `event_start` / `event_end` when those columns
#'   are already relative days (not dates). Ignored when `mp_builder$sl` is already set (e.g. after [add_sl_data()]).
#'
#' @return A modified list containing updated "sl" and "events" entries with the processed event-level data.
#' @examples
#' \dontrun{
#' library(Megaplots)
#' library(dplyr)
#' library(safetyData)
#'
#' data(adam_adae, package = "safetyData")
#'
#' # Events only: first call builds minimal subject-level data from sl_ref_date
#' mp_data <- add_events(
#'   adam_adae,
#'   event_group = "AEBODSYS",
#'   event = "AEDECOD",
#'   sl_ref_date = "TRTSDT",
#'   prefix_group = "SOC: ",
#'   prefix_event = "PT: "
#' ) |>
#'   finalize_mp_object()
#' }
#' @export
add_events <- function(
  mp_builder = NULL,
  events_data = NULL,
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
  keep_vars = NULL,
  sl_ref_date = NULL
) {
  if (
    is.null(events_data) &&
      !is.null(mp_builder) &&
      !inherits(mp_builder, "mp_data_builder")
  ) {
    events_data <- mp_builder
    mp_builder <- NULL
  }
  if (is.null(events_data)) {
    stop("Please provide a valid dataset or file path.", call. = FALSE)
  }
  if (is.null(mp_builder)) {
    mp_builder <- init_mp_object()
  }
  if (!inherits(mp_builder, "mp_data_builder")) {
    stop(
      "
      `mp_builder` must be a megaplots data builder (`mp_data_builder`).",
      call. = FALSE
    )
  }

  #set global variable . to NULL to
  #avoid note: no visible binding for global variable '.' when
  # performing package check (via devtools)
  . <- NULL

  # Read data ----
  if (is.data.frame(events_data)) {
    data <- events_data
  } else if (!is.null(events_data)) {
    data <- tryCatch(
      read_dataset(events_data),
      error = function(e) stop("Error reading the dataset: ", e$message)
    )
  } else {
    stop("Please provide a valid dataset or file path.")
  }

  # Check if the subject identifier is present
  if (!id %in% colnames(data)) {
    stop(sprintf("The specified id '%s' is not a column in the dataset.", id))
  }

  # Apply data filter if provided
  if (!is.null(data_filter)) {
    data <- dplyr::filter(data, !!!rlang::parse_exprs(data_filter))
  }

  # Build subject-level dataset (mp$sl) if not already set ----
  had_sl_before <- !is.null(mp_builder$sl)
  if (!had_sl_before) {
    if (is.null(sl_ref_date)) {
      stop(
        "When subject-level data has not been added (`mp_builder$sl` is NULL), supply `sl_ref_date` ",
        "(column name or numeric constant); or call add_sl_data() first.",
        call. = FALSE
      )
    }
    if (length(sl_ref_date) != 1L) {
      stop("`sl_ref_date` must have length 1.", call. = FALSE)
    }
    sl_tmp <- data |>
      dplyr::mutate(
        # Create a numeric subject ID by removing non-numeric characters from the specified ID column
        subjectid = as.numeric(base::gsub(
          "[^0-9.]",
          "",
          as.character(!!rlang::sym(id))
        ))
      ) |>
      # Keep one baseline row per subject when creating minimal mp_builder$sl on the fly
      dplyr::distinct(.data$subjectid, .keep_all = TRUE) |>
      dplyr::relocate(.data$subjectid)

    # Build mp_builder$sl with subjectid and ref_date from sl_ref_date column or constant
    if (is.character(sl_ref_date)) {
      if (is.na(sl_ref_date)) {
        stop("`sl_ref_date` must not be NA.", call. = FALSE)
      }
      sl_col <- resolve_colname(sl_ref_date, colnames(sl_tmp))
      mp_builder$sl <- sl_tmp |>
        dplyr::select(.data$subjectid, dplyr::all_of(sl_col)) |>
        dplyr::rename(ref_date = !!rlang::sym(sl_col))
      mp_builder$sl$ref_date <- as_date_column(mp_builder$sl$ref_date, sl_col)
    } else if (is.numeric(sl_ref_date)) {
      if (anyNA(sl_ref_date)) {
        stop(
          "`sl_ref_date` must be a single non-missing numeric value.",
          call. = FALSE
        )
      }
      mp_builder$sl <- sl_tmp |>
        dplyr::select(.data$subjectid) |>
        dplyr::mutate(ref_date = as.numeric(sl_ref_date))
    } else {
      stop(
        "`sl_ref_date` must be a column name (character) or a numeric constant.",
        call. = FALSE
      )
    }
  }

  sl_join_cols <- c("subjectid", "ref_date")
  # If start_time and end_time are present in mp_builder$sl, include them in the join to preserve these columns for left censoring and potential later use; if not present, they will simply be ignored in the downstream processing.
  if ("start_time" %in% names(mp_builder$sl)) {
    sl_join_cols <- c(sl_join_cols, "start_time")
  }
  if ("end_time" %in% names(mp_builder$sl)) {
    sl_join_cols <- c(sl_join_cols, "end_time")
  }
  adsl <- mp_builder$sl |> dplyr::select(tidyselect::all_of(sl_join_cols))

  # Validate event_group and event column specifications ----
  if (
    !is.character(event_group) || !length(event_group) || anyNA(event_group)
  ) {
    stop("`event_group` must be a non-missing character vector.", call. = FALSE)
  }
  if (!is.character(event) || !length(event) || anyNA(event)) {
    stop("`event` must be a non-missing character vector.", call. = FALSE)
  }

  eg_cols <- vapply(
    event_group,
    resolve_colname,
    FUN.VALUE = character(1),
    colnames_df = colnames(data),
    USE.NAMES = FALSE
  )
  ev_cols <- vapply(
    event,
    resolve_colname,
    FUN.VALUE = character(1),
    colnames_df = colnames(data),
    USE.NAMES = FALSE
  )

  if (length(eg_cols) > 1L) {
    message(
      "Multiple `event_group` columns provided (",
      paste(eg_cols, collapse = ", "),
      "); values are combined with paste(..., sep = '; ')."
    )
  }
  if (length(ev_cols) > 1L) {
    message(
      "Multiple `event` columns provided (",
      paste(ev_cols, collapse = ", "),
      "); values are combined with paste(..., sep = '; ')."
    )
  }
  if (length(eg_cols) > 2L) {
    message(
      "More than 2 `event_group` columns were provided. It is recommended to restrict to a maximum of 2.",
      call. = FALSE
    )
  }
  if (length(ev_cols) > 2L) {
    message(
      "More than 2 `event` columns were provided. It is recommended to restrict to a maximum of 2.",
      call. = FALSE
    )
  }

  message(
    "Event group column(s): ",
    paste(eg_cols, collapse = ", "),
    "; event column(s): ",
    paste(ev_cols, collapse = ", ")
  )

  data <- data |>
    #rename and relocate id-variable.
    dplyr::mutate(
      subjectid = as.numeric(gsub(
        "[^0-9.]",
        "",
        as.character(!!rlang::sym(id))
      ))
    )

  if (had_sl_before) {
    event_ids <- unique(data$subjectid)
    sl_ids <- unique(adsl$subjectid)
    event_ids <- event_ids[!is.na(event_ids)]
    sl_ids <- sl_ids[!is.na(sl_ids)]
    overlap <- event_ids[event_ids %in% sl_ids]

    if (!length(overlap)) {
      stop(
        "No subjects match subject-level data after transforming `id` to numeric `subjectid`. ",
        "Check that both datasets use compatible id variables.",
        call. = FALSE
      )
    }

    n_event_subjects <- length(event_ids)
    n_matched <- length(overlap)
    if (n_matched < n_event_subjects) {
      message(sprintf(
        "Subject ID match: %d of %d subjects found in subject-level data; events for the rest are dropped.",
        n_matched,
        n_event_subjects
      ))
    }
  }

  data <- data |>
    # Remove unnecessary columns before joining with subject-level data
    dplyr::select(
      -dplyr::any_of(setdiff(
        c("ref_date", "start_time", "end_time"),
        unique(c(eg_cols, ev_cols))
      ))
    ) |>
    dplyr::filter(.data$subjectid %in% adsl$subjectid) |>
    dplyr::left_join(adsl, by = "subjectid") |> # Join to add ref_date and potential start_time / end_time for left censoring
    dplyr::relocate(.data$subjectid, .data$ref_date)

  event_start <- resolve_first_match(event_start, colnames(data), data = data)
  message("Event start date/time: ", event_start)

  event_end <- resolve_first_match(event_end, colnames(data), data = data)
  message("Event end date/time: ", event_end)

  data[[event_start]] <- as_date_column(data[[event_start]], event_start)
  data[[event_end]] <- as_date_column(data[[event_end]], event_end)

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

  # Stage source date columns to internal names to avoid naming conflicts 
  # when input already uses Megaplots output column names.
  staged_result <- stage_mp_source_cols(
    data,
    unique(c(eg_cols, ev_cols, event_start, event_end))
  )
  data <- staged_result$data
  staged_map <- staged_result$staged
  eg_cols_use <- map_mp_staged_names(eg_cols, staged_map)
  ev_cols_use <- map_mp_staged_names(ev_cols, staged_map)
  src_event_start <- map_mp_staged_names(event_start, staged_map)
  src_event_end <- map_mp_staged_names(event_end, staged_map)

  # Filter to rows with non-missing and non-empty values in all event_group and event columns, then create event_group and event by pasting together specified columns with optional prefixes.
  data_tmp <- data |>
    dplyr::filter(dplyr::if_all(
      dplyr::all_of(eg_cols_use),
      ~ !is.na(.) & . != ""
    )) |>
    dplyr::filter(dplyr::if_all(
      dplyr::all_of(ev_cols_use),
      ~ !is.na(.) & . != ""
    )) |>
    dplyr::mutate(
      event_group = paste0(
        prefix_group,
        do.call(
          base::paste,
          c(dplyr::across(dplyr::all_of(eg_cols_use)), sep = "; ")
        )
      ),
      event = paste0(
        prefix_event,
        do.call(
          base::paste,
          c(dplyr::across(dplyr::all_of(ev_cols_use)), sep = "; ")
        )
      )
    ) |>
    dplyr::select(
      .data$subjectid,
      .data$event_group,
      .data$event,
      !!rlang::sym(src_event_start),
      !!rlang::sym(src_event_end),
      .data$ref_date,
      tidyselect::any_of(c("start_time", "end_time")),
      tidyselect::any_of(keep_vars)
    ) |>
    dplyr::filter(!is.na(!!rlang::sym(src_event_start))) |>
    dplyr::arrange(
      .data$subjectid,
      .data$event_group,
      .data$event,
      !!rlang::sym(src_event_start),
      !!rlang::sym(src_event_end)
    )

  # Validate that event_start/event_end and ref_date are on the same scale (dates or numeric); mixed types are not supported.
  ev_is_date <- is_calendar_date(data_tmp[[src_event_start]])
  ref_is_date <- is_calendar_date(data_tmp$ref_date)
  if (ev_is_date != ref_is_date) {
    stop(
      "`event_start` / `event_end` and `ref_date` must be all dates or all numeric; mixed types are not supported.",
      call. = FALSE
    )
  }

  # Check if start_time is available in mp_builder$sl for potential left censoring
  has_sl_start <- "start_time" %in% names(data_tmp)

  # Calculate event start and end times relative to ref_date,
  # then apply left censoring to event_time if requested and start_time is available in mp_builder$sl.
  # Original event_start, event_end, and ref_date columns are dropped after calculations.
  data_tmp <- data_tmp |>
    dplyr::group_by(.data$subjectid, .data$event_group, .data$event) |>
    dplyr::mutate(
      event_time = if (ev_is_date) {
        as.integer(
          !!rlang::sym(src_event_start) - .data$ref_date + 1L
        )
      } else {
        as.integer(!!rlang::sym(src_event_start)) -
          as.integer(.data$ref_date) +
          1L
      },
      event_time_end = if (ev_is_date) {
        dplyr::case_when(
          !is.na(!!rlang::sym(src_event_end)) ~ as.integer(
            !!rlang::sym(src_event_end) - .data$ref_date + 1L
          ),
          # Open events are treated as one-day events at start date
          TRUE ~ as.integer(
            !!rlang::sym(src_event_start) - .data$ref_date + 1L
          )
        )
      } else {
        dplyr::case_when(
          !is.na(!!rlang::sym(src_event_end)) ~ as.integer(
            !!rlang::sym(src_event_end)
          ) -
            as.integer(.data$ref_date) +
            1L,
          # Open events are treated as one-day events at start date
          TRUE ~ as.integer(!!rlang::sym(src_event_start)) -
            as.integer(.data$ref_date) +
            1L
        )
      }
    ) |>
    dplyr::ungroup() |>
    # left censor data if provided and start_time is available in sl;
    dplyr::mutate(
      event_time = if (is.null(left_censor)) {
        .data$event_time
      } else if (!has_sl_start) {
        .data$event_time
      } else {
        dplyr::case_when(
          .data$event_time < .data$start_time + left_censor ~ .data$start_time +
            left_censor -
            1L,
          TRUE ~ .data$event_time
        )
      }
    ) |>
    dplyr::select(
      -dplyr::any_of(unique(c(src_event_start, src_event_end))), 
      -.data$ref_date) |> # Drop original date columns
    dplyr::relocate(
      .data$subjectid,
      .data$event_group,
      .data$event,
      .data$event_time,
      .data$event_time_end
    ) |> # Rearrange columns
    dplyr::distinct()

  time_to_first <- NULL
  if (calc_time_to_first) {
    message("calculating time to first event")
    time_to_first <- calc_time_to_first(data = data_tmp)

    mp_builder$sl <- mp_builder$sl |>
      dplyr::left_join(time_to_first, by = "subjectid")
  }

  days_with <- NULL
  if (calc_days_with) {
    message("calculating days with event")
    days_with <- calc_days_with(data = data_tmp)

    mp_builder$sl <- mp_builder$sl |>
      dplyr::left_join(days_with, by = "subjectid")
  }

  # Bind new events to mp_builder$events ----
  mp_builder$events <- dplyr::bind_rows(mp_builder$events, data_tmp)

  return(mp_builder)
}
