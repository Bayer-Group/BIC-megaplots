#' filter_megaplot_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
filter_megaplot_data <- function(
    tree,
    megaplot_prepared_data,
    color_data
  ) {

  selected_tree <- shinyTree::get_selected(tree, format = "names")

  if(length(selected_tree) > 0) {
    # perform apply function for every list entry of shinyTree list
    # to transform list entries into desired form of event/event group
    selected_data <- do.call(
      "rbind",
      lapply(selected_tree, function(selected_tree_row) {
        # use attributes to decide if a selected row is an event or event_group
        # if length of attributes 'ancestry'
        if(length(attributes(selected_tree_row)$ancestry) == 2) {
          data.frame("event_group" = attributes(selected_tree_row)$ancestry[2], "event" = selected_tree_row[1])
        }
      })
    )

    prepared_data <- megaplot_prepared_data

    filtered_data <- prepared_data %>%
      dplyr::right_join(selected_data, by = c("event", "event_group"))

    filtered_data <- filtered_data %>%
      dplyr::distinct()

    filtered_data <- filtered_data %>%
      dplyr::select(-event_color,-jittered, -event_id) %>%
      dplyr::left_join(
        color_data %>%
          dplyr::select(event,event_group,event_color,jittered,event_id),
        by=c("event","event_group")
      )

    filtered_data_w_jitter <- filtered_data  %>%
      dplyr::ungroup() %>%
      dplyr::select(event, event_group, max_event_id, event_group_id, event_id, jittered) %>%
      dplyr::distinct() %>%
      dplyr::arrange(event_group_id, event_id) %>%
      dplyr::mutate(seq_nr = 1)

    if (nrow(filtered_data_w_jitter) > 1) {
      for(i in 2:nrow(filtered_data_w_jitter)){
        if (filtered_data_w_jitter$jittered[i]) {
          filtered_data_w_jitter$seq_nr[i] <- filtered_data_w_jitter$seq_nr[i-1]+1
        } else if (!filtered_data_w_jitter$jittered[i] & (filtered_data_w_jitter$event_group_id[i] == filtered_data_w_jitter$event_group_id[i-1])){
          filtered_data_w_jitter$seq_nr[i] <- filtered_data_w_jitter$seq_nr[i-1]
        } else if (!filtered_data_w_jitter$jittered[i] & (filtered_data_w_jitter$event_group_id[i] != filtered_data_w_jitter$event_group_id[i-1]))
          filtered_data_w_jitter$seq_nr[i] <- filtered_data_w_jitter$seq_nr[i-1]+1
      }
    } else {
      filtered_data_w_jitter$seq_nr <- 1
    }

    filtered_data_w_jitter <- filtered_data_w_jitter %>%
      dplyr::mutate(jitter_event_time = seq(-0.20,0.20,length = length(unique(filtered_data_w_jitter$seq_nr)))[seq_nr]) %>%
      dplyr::select(event, event_group, jitter_event_time)

    filtered_data <- filtered_data  %>%
      dplyr::ungroup() %>%
      dplyr::left_join(filtered_data_w_jitter, by = c("event","event_group")) %>%
      dplyr::mutate(subjectid_n_jittered = subjectid_n + jitter_event_time)

    filtered_data <- filtered_data %>% dplyr::left_join(
      filtered_data %>%
        dplyr::select(event,event_group) %>%
        dplyr::distinct() %>%
        dplyr::group_by(event) %>%
        dplyr::mutate(number_event_groups = dplyr::n()) %>%
        dplyr::mutate(
          unique_event = dplyr::case_when(
            number_event_groups == 1 ~ event,
            number_event_groups > 1 ~ paste0(event," (",event_group,")")
          )
        ) %>% dplyr::select(event,event_group, unique_event),
      by = c("event_group","event")
    )
    filtered_data
  } else {
    filtered_data <- NULL
  }
  return(filtered_data)
}
