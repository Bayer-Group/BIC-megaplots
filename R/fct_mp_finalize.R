#' Finalize Megaplots data for the app
#'
#' Completes the build started with [add_events()] (and optionally [add_sl_data()]
#' if you attach subject-level data before events).
#'
#' @param mp_builder A populated `mp_data_builder`.
#' @param event_label_case How to normalize `event` and `event_group` text:
#'   `none` (default, unchanged), `lower`, `upper`, or `title` (first
#'   letter of each word capitalized).
#' @param event_group_label_case How to normalize the `event_group` column;
#'   same choices as `event_label_case`.
#'
#' @return A data frame ready for use in the Megaplots app
#' @export
#' @importFrom dplyr left_join
finalize_mp_object <- function(
  mp_builder,
  event_label_case = c("none", "lower", "upper", "title"),
  event_group_label_case = c("none", "lower", "upper", "title")
) {
  if (!inherits(mp_builder, "mp_data_builder")) {
    stop(
      "`mp_builder` must be a megaplots data builder (`mp_data_builder`).",
      call. = FALSE
    )
  }
  if (is.null(mp_builder$sl)) {
    stop(
      "Subject-level data is missing; call add_events() (and/or add_sl_data()) first.",
      call. = FALSE
    )
  }

  out <- mp_builder$sl %>%
    dplyr::left_join(
      mp_builder$events,
      # Join by all common columns (e.g., subjectid) to combine subject-level and event data
      by = intersect(colnames(mp_builder$sl), colnames(mp_builder$events))
    )

  event_label_case <- match.arg(event_label_case)
  if (event_label_case != "none") {
    # Normalize the event labels according to the specified case transformation
    x <- as.character(out[["event"]])
    out[["event"]] <- switch(
      event_label_case,
      lower = tolower(x),
      upper = toupper(x),
      title = stringr::str_to_title(tolower(x)),
      x
    )
  }
  event_group_label_case <- match.arg(event_group_label_case)
  if (event_group_label_case != "none") {
    # Normalize the event group labels according to the specified case transformation
    x <- as.character(out[["event_group"]])
    out[["event_group"]] <- switch(
      event_group_label_case,
      lower = tolower(x),
      upper = toupper(x),
      title = stringr::str_to_title(tolower(x)),
      x
    )
  }

  arrange_cols <- intersect(
    c(
      "subjectid",
      "start_time",
      "end_time",
      "event_time",
      "event_time_end",
      "event_group",
      "event"
    ),
    names(out)
  )
  if (length(arrange_cols)) {
    out <- out %>% dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols)))
  }

  core_cols <- c(
    "subjectid",
    "start_time",
    "end_time",
    "event_time",
    "event_time_end",
    "event_group",
    "event"
  )
  out <- out %>% dplyr::relocate(dplyr::any_of(core_cols), .before = 1)

  return(out)
}
