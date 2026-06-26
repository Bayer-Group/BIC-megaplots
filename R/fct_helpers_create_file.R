#' Helper function to calculate time to first event and time to first event group
#'
#' This function calculates the time to the first occurrence of specific events
#' and event groups for each subject in the provided dataset.
#' Expects columns subjectid, event_group, event, event_time,
#' event_time_end.
#'
#' @param data A data frame containing event data with columns for subject ID,
#' event group, event, event start time, and event end time.
#' @param calc_event_group Logical indicating whether to calculate time to the first
#' event group. Default is TRUE.
#' @param calc_event Logical indicating whether to calculate time to the first
#' individual event. Default is TRUE.
#'
#' @return A data frame with the time to the first occurrence of each subject's
#' events and event groups.
#' @export
calc_time_to_first <- function(
  data,
  calc_event_group = TRUE,
  calc_event = TRUE
) {
  if (!calc_event_group && !calc_event) {
    stop("Error: One of calc_event_group and calc_event must be TRUE.")
  }

  # Calculate time to first event if requested
  if (calc_event) {
    data_time_to_first <- data |>
      dplyr::arrange(
        .data$subjectid,
        .data$event_group,
        .data$event,
        .data$event_time
      ) |>
      dplyr::group_by(.data$subjectid, .data$event_group, .data$event) |>
      # Use ifelse to handle cases where all event_time values are NA for a group
      dplyr::summarize(
        first = ifelse(
          all(is.na(.data$event_time)),
          NA,
          min(.data$event_time, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        # Replace punctuation and spaces with underscores for valid column names
        event_group = gsub("[[:punct:][:space:]]+", "_", .data$event_group),
        event = gsub("[[:punct:][:space:]]+", "_", .data$event)
      ) |>
      tidyr::pivot_wider(
        id_cols = "subjectid",
        names_from = c("event_group", "event"),
        names_prefix = "ttf_",
        names_sep = "_",
        values_from = "first"
      )
  }

  # Calculate time to first event group if requested
  if (calc_event_group) {
    data_time_to_first_group <- data |>
      dplyr::select(-"event") |>
      dplyr::arrange(
        .data$subjectid,
        .data$event_group,
        .data$event_time
      ) |>
      dplyr::group_by(.data$subjectid, .data$event_group) |>
      dplyr::summarize(
        first = ifelse(
          all(is.na(.data$event_time)),
          NA,
          min(.data$event_time, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        # Replace punctuation and spaces with underscores for valid column names
        event_group = gsub("[[:punct:][:space:]]+", "_", .data$event_group)
      ) |>
      tidyr::pivot_wider(
        id_cols = "subjectid",
        names_from = "event_group",
        names_prefix = "ttf_",
        names_sep = "_",
        values_from = "first"
      )
    if (calc_event) {
      data_time_to_first <- data_time_to_first |>
        dplyr::left_join(data_time_to_first_group, by = "subjectid")
    } else {
      data_time_to_first <- data_time_to_first_group
    }
  }
  return(data_time_to_first)
}

#' Helper function to calculate days with event and days with event group
#'
#' This function calculates the number of days with specific events
#' for each subject in the provided dataset.
#' Expects columns subjectid, event_group, event, event_time,
#' event_time_end.
#'
#' @param data A data frame containing event data with columns for subject ID,
#' event group, event, event start time, and event end time.
#' @param calc_event_group Logical indicating whether to calculate days for event groups. Default is TRUE.
#' @param calc_event Logical indicating whether to calculate days for individual events. Default is TRUE.
#'
#' @return A data frame with the number of days associated with each subject's
#' events and event groups.
#' @export
calc_days_with <- function(
  data,
  calc_event_group = TRUE,
  calc_event = TRUE
  # subjectid = subjectid,
  # event_group = event_group,
  # event = event,
  # event_time = event_time,
  # event_time_end = event_time_end
) {
  if (!calc_event_group && !calc_event) {
    stop("Error: One of calc_event_group and calc_event must be TRUE.")
  }
  if (calc_event) {
    data_days_with <- data |>
      dplyr::arrange(
        .data$subjectid,
        .data$event_group,
        .data$event,
        .data$event_time,
        .data$event_time_end
      ) |>
      dplyr::mutate(
        # Expand each interval to a day sequence to count unique covered days
        days = purrr::map2(
          .data$event_time,
          .data$event_time_end,
          ~ if (!is.na(.x) && is.finite(.x)) {
            if (!is.na(.y) && is.finite(.y)) {
              seq(from = .x, to = .y)
            } else {
              .x # Count as 1 day if end time is NA
            }
          } else {
            NULL
          }
        )
      ) |>
      dplyr::group_by(.data$subjectid, .data$event_group, .data$event) |>
      dplyr::summarize(
        # Count unique days across all events for each subject
        days_with = dplyr::n_distinct(unlist(.data$days)),
        .groups = "drop"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        # Replace punctuation and spaces with underscores for valid column names
        event_group = gsub("[[:punct:][:space:]]+", "_", .data$event_group),
        event = gsub("[[:punct:][:space:]]+", "_", .data$event)
      ) |>
      tidyr::pivot_wider(
        id_cols = "subjectid",
        names_from = c("event_group", "event"),
        names_prefix = "dw_",
        names_sep = "_",
        values_from = "days_with",
        values_fill = 0
      )
  }
  if (calc_event_group) {
    data_days_with_group <- data |>
      dplyr::select(-.data$event) |>
      dplyr::arrange(
        .data$subjectid,
        .data$event_group,
        .data$event_time,
        .data$event_time_end
      ) |>
      dplyr::mutate(
        # Expand each interval to a day sequence to count unique covered days
        days = purrr::map2(
          .data$event_time,
          .data$event_time_end,
          ~ if (!is.na(.x) && is.finite(.x)) {
            if (!is.na(.y) && is.finite(.y)) {
              seq(from = .x, to = .y)
            } else {
              .x # Count as 1 day if end time is NA
            }
          } else {
            NULL
          }
        )
      ) |>
      dplyr::group_by(.data$subjectid, .data$event_group) |>
      dplyr::summarize(
        # Count unique days across all events in the group for each subject
        days_with = dplyr::n_distinct(unlist(.data$days)),
        .groups = "drop"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        # Replace punctuation and spaces with underscores for valid column names
        event_group = gsub("[[:punct:][:space:]]+", "_", .data$event_group)
      ) |>
      tidyr::pivot_wider(
        id_cols = "subjectid",
        names_from = c("event_group"),
        names_prefix = "dw_",
        names_sep = "_",
        values_from = "days_with",
        values_fill = 0
      )
    if (calc_event) {
      data_days_with <- data_days_with |>
        dplyr::left_join(data_days_with_group, by = "subjectid")
    } else {
      data_days_with <- data_days_with_group
    }
  }
  return(data_days_with)
}

#' Helper function to read datasets
#'
#' @param path Path to a file (.sas7bdat, .csv, .rdata). For `.RData`, the
#'   first object listed by [load()] is returned (a warning is emitted if more
#'   than one object exists).
#'
#' @export
read_dataset <- function(path) {
  # Check the file extension to determine how to read the file
  if (grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    return(haven::read_sas(path)) # Read SAS file
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(readr::read_csv(path, show_col_types = FALSE)) # Read CSV file
  } else if (grepl("\\.rdata$", path, ignore.case = TRUE)) {
    env <- new.env(parent = emptyenv())
    objs <- load(path, envir = env)
    if (length(objs) > 1L) {
      message(
        "RData file contains multiple objects; returning the first: ",
        objs[[1]]
      )
    }
    return(get(objs[[1]], envir = env))
  } else {
    stop("Unsupported file format. Please provide a SAS, CSV, or RData file.") # Error for unsupported format
  }
}

#' @noRd
resolve_colname <- function(label, colnames_df) {
  idx <- which(toupper(colnames_df) == toupper(label))
  if (!length(idx)) {
    stop("Column '", label, "' not found in dataset.", call. = FALSE)
  }
  colnames_df[idx[[1]]]
}

#' @noRd
column_has_values <- function(x) {
  if (is.character(x) || is.factor(x)) {
    any(!is.na(x) & nzchar(trimws(as.character(x))))
  } else {
    any(!is.na(x))
  }
}

#' @noRd
resolve_first_match <- function(
  candidates,
  colnames_df,
  data = NULL,
  label = NULL
) {
  present <- candidates[toupper(candidates) %in% toupper(colnames_df)]
  if (!length(present) || is.na(present[[1]])) {
    if (!is.null(label)) {
      stop(
        "None of these column names exist in the data for ",
        label,
        ": ",
        paste(candidates, collapse = ", ")
      )
    } else {
      stop(
        "None of these column names exist in the data: ",
        paste(candidates, collapse = ", ")
      )
    }
  }
  if (is.null(data)) {
    return(present[[1]])
  }
  useful <- present[vapply(
    present,
    function(candidate) {
      column_has_values(data[[resolve_colname(candidate, colnames_df)]])
    },
    logical(1)
  )]
  if (!length(useful) || is.na(useful[[1]])) {
    stop(
      "Column(s) ",
      paste(present, collapse = ", "),
      " exist but contain no non-missing values. Checked: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  resolve_colname(useful[[1]], colnames_df)
}

#' @noRd
resolve_all_useful_matches <- function(
  candidates,
  colnames_df,
  data = NULL,
  label = NULL
) {
  present <- candidates[toupper(candidates) %in% toupper(colnames_df)]
  if (!length(present)) {
    if (!is.null(label)) {
      stop(
        "None of the input parameters in ",
        label,
        " are present as column names in the data.",
        call. = FALSE
      )
    }
    stop(
      "None of these column names exist in the data: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(data)) {
    return(vapply(
      present,
      resolve_colname,
      FUN.VALUE = character(1),
      colnames_df = colnames_df
    ))
  }
  useful <- present[vapply(
    present,
    function(candidate) {
      column_has_values(data[[resolve_colname(candidate, colnames_df)]])
    },
    logical(1)
  )]
  if (!length(useful)) {
    stop(
      "Column(s) ",
      paste(present, collapse = ", "),
      " exist but contain no non-missing values. Checked: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  vapply(
    useful,
    resolve_colname,
    FUN.VALUE = character(1),
    colnames_df = colnames_df
  )
}

#' @noRd
is_calendar_date <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXct")
}


#' @noRd
as_date_column <- function(x, col_name) {
  if (inherits(x, "Date") || is.numeric(x)) {
    return(x)
  }

  chr <- trimws(as.character(x))
  chr[chr == ""] <- NA
  n_nonmiss <- sum(!is.na(chr))
  if (n_nonmiss == 0L) {
    return(as.Date(rep(NA, length(x))))
  }

  parsed <- tryCatch(
    as.Date(chr),
    error = function(e) NULL
  )

  if (is.null(parsed) || sum(!is.na(parsed[!is.na(chr)])) == 0L) {
    stop(
      sprintf("Column '%s': could not convert values to Date. ", col_name),
      "Please convert date columns to Date format before calling this function.",
      call. = FALSE
    )
  }

  parsed
}
