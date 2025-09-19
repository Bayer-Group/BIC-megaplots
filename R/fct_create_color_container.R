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
  # # receive names of selected shinyTree input
  # selected_tree <- shinyTree::get_selected(tree, format = "names")
  #
  if (nrow(tree) > 0) {
    selected_data <- tree

    selected_data <- selected_data %>%
      dplyr::group_by(event_group) %>%
      dplyr::mutate(group_index = dplyr::cur_group_id(), index = dplyr::row_number(event_group) ) %>%
      dplyr::mutate(index = dplyr::case_when(
        is.na(event) ~ 0,
        !is.na(event) ~ index,
        )
      )
    selected_data <- selected_data %>%
      dplyr::arrange(group_index,index) %>%
      dplyr::select(event_group,event) %>%
      as.data.frame()
    # Re-arrange within groups to have
    # selected_data <- selected_data %>% dplyr::group_by(event_group) %>% dplyr::arrange(type_for_color, .by_group = TRUE) %>% dplyr::ungroup()

    # Every shinyTree list element gets an id which can be received via attributes(x)$id.
    # This id will be merged to the data and used for sorting


    # sort_id <- as.numeric(unlist(lapply(shinyTree::get_selected(tree, format="classid"), function(x){attributes(x)$id})))
    # selected_data <- cbind(selected_data, sort_id) %>%
    #   dplyr::arrange(sort_id)
    # selected_data <- rbind(
    #   selected_data %>% dplyr::filter(event_group == "Select all event(s)"),
    #   selected_data %>%dplyr::filter(event_group != "Select all event(s)")
    # )

    # join color vector to the data and create variables "names_for_color_list" & "type_for_color" which will
    # be used to colorize the div container and make sure that also event_groups are displayed
    selected_data <- selected_data %>%
      dplyr::left_join(
        color_vector %>%
          dplyr::select(event_group, event, event_color, gradient_event_color_1, gradient_event_color_2, gradient_event_color_3),
        by = dplyr::join_by(event_group,event)
      ) %>%
      dplyr::mutate(
        names_for_color_list = ifelse(is.na(event), event_group, event),
        type_for_color = ifelse(is.na(event), "event_group", "event")
      )


  }
  return(selected_data)
}
