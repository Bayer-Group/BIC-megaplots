#' create_color_container
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_color_container <- function(
    tree,
    color_vector
) {
  # # receive names of selected shinyTree input
  # selected_tree <- shinyTree::get_selected(tree, format = "names")
  #
  if (nrow(tree) > 0) {
    selected_data <- tree

    # factor event_group to ensure correct group_index
    selected_data$megaplots_selected_event_group <- factor(selected_data$megaplots_selected_event_group,levels = unique(selected_data$megaplots_selected_event_group))

    selected_data <- selected_data %>%
      dplyr::group_by(.data$megaplots_selected_event_group) %>%
      dplyr::mutate(group_index = dplyr::cur_group_id(), index = dplyr::row_number(.data$megaplots_selected_event_group) ) %>%
      dplyr::mutate(index = dplyr::case_when(
        is.na(.data$megaplots_selected_event) ~ 0,
        !is.na(.data$megaplots_selected_event) ~ index,
        )
      )
    selected_data <- selected_data %>%
      dplyr::arrange(.data$group_index, .data$index) %>%
      dplyr::select(tidyselect::all_of(c("megaplots_selected_event_group","megaplots_selected_event"))) %>%
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
          dplyr::select(tidyselect::all_of(c("megaplots_selected_event_group", "megaplots_selected_event", "event_color", "gradient_event_color_1", "gradient_event_color_2", "gradient_event_color_3"))),
        by = dplyr::join_by("megaplots_selected_event_group","megaplots_selected_event")
      ) %>%
      dplyr::mutate(
        names_for_color_list = ifelse(is.na(.data$megaplots_selected_event), .data$megaplots_selected_event_group, .data$megaplots_selected_event),
        type_for_color = ifelse(is.na(.data$megaplots_selected_event), "megaplots_selected_event_group", "megaplots_selected_event")
      )

    # in case new event groups are created via jsTreeR, colorize these in grey
    selected_data <- selected_data %>% dplyr::mutate(
      event_color = dplyr::case_when(
        is.na(event_color) ~ "#424242",
        !is.na(event_color) ~ event_color

      )
    )

  }
  return(selected_data)
}
