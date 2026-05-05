#' Finalize Megaplots data for the app
#'
#' Completes the build started with [init_mp_object()], [add.sl_data()], and
#' [add.events()].
#'
#' @param mp A populated `mp_data_builder`.
#'
#' @return A `list` with elements `sl` and `events`. If no event rows were
#'   added, `events` is `NULL`.
#' @export
#' @importFrom dplyr left_join
finalize_mp_object <- function(mp) {
  if (!inherits(mp, "mp_data_builder")) {
    stop("`mp` must be an object created by init_mp_object().", call. = FALSE)
  }
  if (is.null(mp$sl)) {
    stop(
      "Subject-level data is missing; call add.sl_data() first.",
      call. = FALSE
    )
  }

  mp <- mp$sl %>%
    dplyr::left_join(
      mp$events,
      by = intersect(colnames(mp$sl), colnames(mp$events))
    ) %>%
    dplyr::arrange(
      .data$subjectid,
      .data$start_time,
      .data$end_time,
      .data$event_start_time,
      .data$event_end_time,
      .data$event_group,
      .data$event
    )

  return(mp)
}
