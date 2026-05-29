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
#' @keywords internal
#' @return An object of class `mp_data_builder`.
init_mp_object <- function() {
  structure(
    list(
      sl = NULL,
      events = empty_mp_events()
    ),
    class = "mp_data_builder"
  )
}
