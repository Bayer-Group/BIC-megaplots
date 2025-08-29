#' create_color_container
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_color_container <- function(
    tree,
    color_vector = color_data$all
) {

  # receive names of selected shinyTree input
  selected_tree <- shinyTree::get_selected(tree, format = "names")

  if (length(selected_tree) > 0) {
    #
    selected_data <- do.call(
      "rbind",
      # perform apply function for every list entrie of shinyTree list
      # to transform list entries into desired form of event/event group
      lapply(selected_tree, function(selected_tree_row) {
        # use attributes to decide if a selected row is an event or event_group
        # if length of attributes 'ancestry'
        if(length(attributes(selected_tree_row)$ancestry) == 2){
          data.frame("event_group" = attributes(selected_tree_row)$ancestry[2], "event" = selected_tree_row[1])
        } else {
          data.frame("event_group" = selected_tree_row[1], "event" = NA)
        }
      })
    )

    # Every shinyTree list element gets an id which can be received via attributes(x)$id.
    # This id will be merged to the data and used for sorting
    sort_id <- as.numeric(unlist(lapply(shinyTree::get_selected(tree, format="classid"), function(x){attributes(x)$id})))
    selected_data <- cbind(selected_data, sort_id) %>%
      dplyr::arrange(sort_id)

    # join color vector to the data and create variables "names_for_color_list" & "type_for_color" which will
    # be used to colorize the div container and make sure that also event_groups are displayed
    selected_data <- selected_data %>%
      dplyr::left_join(
        color_vector %>%
          dplyr::select(event_group, event, event_color),
        by = dplyr::join_by(event_group,event)
      ) %>%
      dplyr::mutate(
        names_for_color_list = ifelse(is.na(event), event_group, event),
        type_for_color = ifelse(is.na(event), "event_group", "event")
      )
  }
  return(selected_data)
}
