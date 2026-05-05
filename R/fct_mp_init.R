#' @noRd
empty_mp_events <- function() {
  data.frame(
    subjectid = integer(),
    event_group = character(),
    event = character(),
    event_start_time = integer(),
    event_end_time = integer(),
    stringsAsFactors = FALSE
  )
}

#' Initialize an empty Megaplots data builder
#'
#' Returned object is meant to be used with [add.sl_data()], [add.events()],
#' and [finalize_mp_object()]. "events" starts as an empty table with the core
#' Megaplots event columns so rows can be stacked with [add.events()].
#'
#' @return An object of class `mp_data_builder`.
#' @export
init_mp_object <- function() {
  structure(
    list(
      sl = NULL,
      events = empty_mp_events()
    ),
    class = "mp_data_builder"
  )
}
