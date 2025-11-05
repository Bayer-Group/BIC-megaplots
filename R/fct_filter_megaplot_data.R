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

  # selected_tree <- shinyTree::get_selected(tree, format = "names")
  #
   if(nrow(tree) > 0) {

    selected_data <- tree

    prepared_data <- megaplot_prepared_data

    filtered_data <- prepared_data %>%
      dplyr::right_join(selected_data, by = c("megaplots_selected_event", "megaplots_selected_event_group"))

    filtered_data <- filtered_data %>%
      dplyr::distinct()

    filtered_data <- filtered_data %>%
      dplyr::select(-tidyselect::all_of(c("event_color","jittered", "event_id"))) %>%
      dplyr::left_join(
        color_data %>%
          dplyr::select(tidyselect::all_of(c("megaplots_selected_event","megaplots_selected_event_group","event_color","jittered","event_id"))),
        by = c("megaplots_selected_event","megaplots_selected_event_group")
      )

    filtered_data_w_jitter <- filtered_data  %>%
      dplyr::ungroup() %>%
      dplyr::select(tidyselect::all_of(c("megaplots_selected_event", "megaplots_selected_event_group", "max_event_id", "event_group_id", "event_id", "jittered"))) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$event_group_id, .data$event_id) %>%
      dplyr::mutate(seq_nr = 1)

    filtered_data_w_jitter <- filtered_data_w_jitter %>%
      dplyr::filter(!is.na(.data$megaplots_selected_event))

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
      dplyr::mutate(jitter_event_time = seq(-0.20,0.20,length = length(unique(filtered_data_w_jitter$seq_nr)))[.data$seq_nr]) %>%
      dplyr::select(tidyselect::all_of(c("megaplots_selected_event", "megaplots_selected_event_group", "jitter_event_time")))

    filtered_data <- filtered_data  %>%
      dplyr::ungroup() %>%
      dplyr::left_join(filtered_data_w_jitter, by = c("megaplots_selected_event","megaplots_selected_event_group")) %>%
      dplyr::mutate(subjectid_n_jittered = .data$subjectid_n + .data$jitter_event_time)

    filtered_data <- filtered_data %>% dplyr::left_join(
      filtered_data %>%
        dplyr::select(tidyselect::all_of(c("megaplots_selected_event","megaplots_selected_event_group"))) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$megaplots_selected_event) %>%
        dplyr::mutate(number_event_groups = dplyr::n()) %>%
        dplyr::mutate(
          unique_event = dplyr::case_when(
            .data$number_event_groups == 1 ~ .data$megaplots_selected_event,
            .data$number_event_groups > 1 ~ paste0(.data$megaplots_selected_event," (",.data$megaplots_selected_event_group,")")
          )
        ) %>% dplyr::select(tidyselect::all_of(c("megaplots_selected_event","megaplots_selected_event_group", "unique_event"))),
      by = c("megaplots_selected_event_group","megaplots_selected_event")
    )
    filtered_data
  } else {
    filtered_data <- NULL
  }
  return(filtered_data)
}
