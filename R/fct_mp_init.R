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
#' Returned object is meant to be used with [add.events()] and [finalize_mp_object()];
#' call [add.sl_data()] first when you want subject-level rows from ADSL.
#' "events" starts as an empty table with the core Megaplots event columns so rows
#' can be stacked with [add.events()].
#'
#' @return An object of class `mp_data_builder`.
#' @examples
#' \dontrun{
#' library(Megaplots)
#' library(dplyr)
#' library(safetyData)
#'
#' data(adam_adsl, adam_adae, adam_adlbc, package = "safetyData")
#'
#' mp_data <- init_mp_object() %>%
#'   add.sl_data(adam_adsl) %>%
#'   add.events(
#'     adam_adae,
#'     event_group = "AEBODSYS",
#'     event = "AEDECOD",
#'     prefix_group = "SOC: ",
#'     prefix_event = "PT: ",
#'     calc_time_to_first = TRUE,
#'     calc_days_with = TRUE
#'   ) %>%
#'   add.events(
#'     adam_adae,
#'     event_group = "CQ01NAM",
#'     event = "AETERM",
#'     prefix_group = "CQ: ",
#'   ) %>%
#'   add.events(
#'     adam_adlbc,
#'     event_group = "PARAM",
#'     event = "LBNRIND",
#'     event_start = "ADT",
#'     event_end = "ADT",
#'     prefix_group = "Lab: "
#'   ) %>%
#'   finalize_mp_object(event_group_label_case = "title",
#'                      event_label_case = "title")
#' }
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
