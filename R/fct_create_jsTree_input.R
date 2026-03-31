#' create_jsTree_input
#'
#' @description A function to create a jsTree input structure from data.
#'
#' @param data A data frame containing the necessary columns.
#' @return A list representing the jsTree structure.
#' @noRd
create_jsTree_input<- function(data) {

  # Check if the input data frame is empty
  if (nrow(data) == 0) {
    return(list())  # Return an empty list if there's no data
  }

  # Group the data by 'megaplots_selected_event_group'
  grouped_data <- data %>%
    dplyr::group_by(megaplots_selected_event_group) %>%
    dplyr::summarise(events = list(unique(megaplots_selected_event)), .groups = 'drop')

  # Create jsTree structure
  jsTree_output <- purrr::map(grouped_data$megaplots_selected_event_group, function(group_name) {
    children_events <- grouped_data$events[[which(grouped_data$megaplots_selected_event_group == group_name)]]

    list(
      text = unique(group_name),
      type = "root",
      children = purrr::map(children_events, ~ list(text = as.character(.), type = "child"))
    )
  })

  return(jsTree_output)
}
