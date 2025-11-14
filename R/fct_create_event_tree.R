#' create_event_tree
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_event_tree <- function(
    reduced_event_data
  ) {


  unique_event_groups <- reduced_event_data %>%
    dplyr::pull(.data$event_group) %>%
    unique()

  event_list <- vector(mode = 'list', length(unique_event_groups))

  for(i in 1:length(unique_event_groups)) {
    events <- reduced_event_data %>%
      dplyr::filter(.data$event_group == unique_event_groups[i]) %>%
      dplyr::pull(.data$event)

    tmp_list <- vector(mode ='list', length = length(events))

    names(tmp_list) <- events

    for (j in 1:length(events)) {
      tmp_list[[j]] <-structure(j, sticon = "")
    }
    event_list[[i]] <-  structure(tmp_list, stopened = FALSE, sticon="")
  }
  names(event_list) <- unique_event_groups

  event_list <- structure(event_list)
  event_list <- list( "Select all event(s)" = structure(event_list, sticon = ""))
  attr(event_list[[1]], "stopened") <- TRUE

 return(event_list)
}
