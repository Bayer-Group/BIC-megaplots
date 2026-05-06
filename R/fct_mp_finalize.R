#' Finalize Megaplots data for the app
#'
#' Completes the build started with [init_mp_object()] and [add.events()] (and
#' optionally [add.sl_data()] if you attach subject-level data before events).
#'
#' @param mp A populated `mp_data_builder`.
#'
#' @return A data frame ready for use in the Megaplots app
#' @export
#' @importFrom dplyr left_join
finalize_mp_object <- function(mp) {
  if (!inherits(mp, "mp_data_builder")) {
    stop("`mp` must be an object created by init_mp_object().", call. = FALSE)
  }
  if (is.null(mp$sl)) {
    stop(
      "Subject-level data is missing; call add.events() (and/or add.sl_data()) first.",
      call. = FALSE
    )
  }

  mp <- mp$sl %>%
    dplyr::left_join(
      mp$events,
      by = intersect(colnames(mp$sl), colnames(mp$events))
    )

  arrange_cols <- intersect(
    c(
      "subjectid",
      "start_time",
      "end_time",
      "event_start_time",
      "event_end_time",
      "event_group",
      "event"
    ),
    names(mp)
  )
  if (length(arrange_cols)) {
    mp <- mp %>% dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols)))
  }

  return(mp)
}
