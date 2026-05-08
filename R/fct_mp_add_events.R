#' Function to create event level megaplots dataset from ADaM datasets
#'
#' This function processes ADaM datasets to create a comprehensive dataset for event-level analysis with megaplots,
#' including the ability to calculate time to first event or days with event, left censor data and manage additional variables.
#' Processes a single pair of columns (e.g. "AEBODSYS" and "AEDECOD"). New rows are appended to "mp$events".
#' Call multiple times to stack several pairs or domains, then call "finalize_mp_object()" to create upload
#' data for the megaplots app.
#'
#' @details
#' **Subject identifier (`id`):** `id` must match a column name in `path_data` exactly (case-sensitive).
#' Subject keys are coerced to numeric by keeping only digits and periods (`0123456789.`); distinct labels
#' that map to the same numeric id can collide. Avoid by using unique digit patterns or pre-mapping keys.
#'
#' **`calc_time_to_first` / `calc_days_with` across multiple `add.events()` calls:** Each run with these
#' flags joins `ttf_*` / `dw_*` columns onto `mp$sl`. Another call can duplicate or rename columns (e.g.
#' `...x` / `...y`) if derived names overlap. Prefer one pair of calls per analysis, or rely on distinct
#' event labels from `prefix_group` / `prefix_event` / domain-specific columns so pivot names do not clash.
#'
#' **Building `mp$sl` from `path_data`:** When `mp$sl` is NULL (i.e. [add.sl_data()] was not used),
#' you must pass `sl_ref_date`: either a column name in `path_data` with the reference date per subject,
#' or a single numeric value when `event_start` / `event_end` are already on the same relative-day scale
#' (e.g. `sl_ref_date = 1`). Only `subjectid` and `ref_date` are added to `mp$sl` (no `start_time` / `end_time`).
#' One row per `subjectid` is kept (first after filtering). Later `add.events()` calls keep only subjects already
#' in `mp$sl`; use [add.sl_data()] first when you need the full population from ADSL or subject-level timeline
#' columns.
#'
#' @param mp An object from [init_mp_object()]. If `mp$sl` is NULL, `sl_ref_date` is required.
#' @param path_data A data frame or a file path to the dataset (SAS, CSV, or RData) to be read. File should be ADaM conform.
#' @param event_group Character vector of one or more column names used to build Megaplots `event_group`.
#'   When multiple names are provided, values are pasted together.
#' @param event Character vector of one or more column names used to build Megaplots `event`.
#'   When multiple names are provided, values are pasted together.
#' @param id Column name of the subject identifier in `path_data` (default `"USUBJID"`). Must match the data
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
#' @param sl_ref_date Required when `mp$sl` is NULL. Character: column name in `path_data` whose values
#'   are the reference value (typically `Date`) for converting event dates to study days. Numeric
#'   length 1: constant reference on the same scale as `event_start` / `event_end` when those columns
#'   are already relative days (not dates). Ignored when `mp$sl` is already set (e.g. after [add.sl_data()]).
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
  keep_vars = NULL,
  sl_ref_date = NULL
) {
  if (!inherits(mp, "mp_data_builder")) {
    stop("`mp` must be an object created by init_mp_object().", call. = FALSE)
  }

  #set global variable . to NULL to
  #avoid note: no visible binding for global variable '.' when
  # performing package check (via devtools)
  . <- NULL

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

  if (!is.null(data_filter)) {
    data <- dplyr::filter(data, !!!rlang::parse_exprs(data_filter))
  }

  if (is.null(mp$sl)) {
    if (is.null(sl_ref_date)) {
      stop(
        "When subject-level data has not been added (`mp$sl` is NULL), supply `sl_ref_date` ",
        "(column name or numeric constant); or call add.sl_data() first.",
        call. = FALSE
      )
    }
    if (length(sl_ref_date) != 1L) {
      stop("`sl_ref_date` must have length 1.", call. = FALSE)
    }
    sl_tmp <- data %>%
      dplyr::mutate(
        subjectid = as.numeric(base::gsub(
          "[^0-9.]",
          "",
          as.character(!!rlang::sym(id))
        ))
      ) %>%
      dplyr::distinct(.data$subjectid, .keep_all = TRUE) %>%
      dplyr::relocate(.data$subjectid)

    if (is.character(sl_ref_date)) {
      if (is.na(sl_ref_date)) {
        stop("`sl_ref_date` must not be NA.", call. = FALSE)
      }
      sl_col <- resolve_colname(sl_ref_date, colnames(sl_tmp))
      mp$sl <- sl_tmp %>%
        dplyr::select(.data$subjectid, dplyr::all_of(sl_col)) %>%
        dplyr::rename(ref_date = !!rlang::sym(sl_col))
    } else if (is.numeric(sl_ref_date)) {
      if (anyNA(sl_ref_date)) {
        stop(
          "`sl_ref_date` must be a single non-missing numeric value.",
          call. = FALSE
        )
      }
      mp$sl <- sl_tmp %>%
        dplyr::select(.data$subjectid) %>%
        dplyr::mutate(ref_date = as.numeric(sl_ref_date))
    } else {
      stop(
        "`sl_ref_date` must be a column name (character) or a numeric constant.",
        call. = FALSE
      )
    }
  }

  sl_join_cols <- c("subjectid", "ref_date")
  if ("start_time" %in% names(mp$sl)) {
    sl_join_cols <- c(sl_join_cols, "start_time")
  }
  if ("end_time" %in% names(mp$sl)) {
    sl_join_cols <- c(sl_join_cols, "end_time")
  }
  adsl <- mp$sl %>% dplyr::select(tidyselect::all_of(sl_join_cols))

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
    warning(
      "More than 2 `event_group` columns were provided. It is recommended to restrict to a maximum of 2.",
      call. = FALSE
    )
  }
  if (length(ev_cols) > 2L) {
    warning(
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

  data <- data %>%
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

  data_tmp <- data %>%
    dplyr::filter(dplyr::if_all(
      dplyr::all_of(eg_cols),
      ~ !is.na(.) & . != ""
    )) %>%
    dplyr::filter(dplyr::if_all(
      dplyr::all_of(ev_cols),
      ~ !is.na(.) & . != ""
    )) %>%
    dplyr::mutate(
      event_group = paste0(
        prefix_group,
        do.call(
          base::paste,
          c(dplyr::across(dplyr::all_of(eg_cols)), sep = "; ")
        )
      ),
      event = paste0(
        prefix_event,
        do.call(
          base::paste,
          c(dplyr::across(dplyr::all_of(ev_cols)), sep = "; ")
        )
      )
    ) %>%
    dplyr::select(
      .data$subjectid,
      .data$event_group,
      .data$event,
      !!rlang::sym(event_start),
      !!rlang::sym(event_end),
      .data$ref_date,
      tidyselect::any_of(c("start_time", "end_time")),
      tidyselect::any_of(keep_vars)
    ) %>%
    dplyr::filter(!is.na(!!rlang::sym(event_start))) %>%
    dplyr::arrange(
      .data$subjectid,
      .data$event_group,
      .data$event,
      !!rlang::sym(event_start),
      !!rlang::sym(event_end)
    )

  is_cal <- function(x) {
    inherits(x, "Date") || inherits(x, "POSIXct")
  }
  ev_is_date <- is_cal(data_tmp[[event_start]])
  ref_is_date <- is_cal(data_tmp$ref_date)
  if (ev_is_date != ref_is_date) {
    stop(
      "`event_start` / `event_end` and `ref_date` must be all dates or all numeric; mixed types are not supported.",
      call. = FALSE
    )
  }

  has_sl_start <- "start_time" %in% names(data_tmp)

  data_tmp <- data_tmp %>%
    dplyr::group_by(.data$subjectid, .data$event_group, .data$event) %>%
    dplyr::mutate(
      event_start_time = if (ev_is_date) {
        as.integer(
          !!rlang::sym(event_start) - .data$ref_date + 1L
        )
      } else {
        as.integer(!!rlang::sym(event_start)) -
          as.integer(.data$ref_date) +
          1L
      },
      event_end_time = if (ev_is_date) {
        dplyr::case_when(
          !is.na(!!rlang::sym(event_end)) ~ as.integer(
            !!rlang::sym(event_end) - .data$ref_date + 1L
          ),
          TRUE ~ as.integer(
            !!rlang::sym(event_start) - .data$ref_date + 1L
          )
        )
      } else {
        dplyr::case_when(
          !is.na(!!rlang::sym(event_end)) ~ as.integer(
            !!rlang::sym(event_end)
          ) -
            as.integer(.data$ref_date) +
            1L,
          TRUE ~ as.integer(!!rlang::sym(event_start)) -
            as.integer(.data$ref_date) +
            1L
        )
      }
    ) %>%
    dplyr::ungroup() %>%
    # left censor data if provided ans start_time is available in sl;
    dplyr::mutate(
      event_start_time = if (is.null(left_censor)) {
        .data$event_start_time
      } else if (!has_sl_start) {
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
    message("calculating time to first event")
    time_to_first <- calc_time_to_first(data = data_tmp)

    mp$sl <- mp$sl %>%
      dplyr::left_join(time_to_first, by = "subjectid")
  }

  days_with <- NULL
  if (calc_days_with) {
    message("calculating days with event")
    days_with <- calc_days_with(data = data_tmp)

    mp$sl <- mp$sl %>%
      dplyr::left_join(days_with, by = "subjectid")
  }

  mp$events <- dplyr::bind_rows(mp$events, data_tmp)

  return(mp)
}
