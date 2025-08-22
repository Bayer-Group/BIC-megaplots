prepare_megaplot_data <- function(
    megaplot_data = shiny::req(uploaded_data$val),
    grouping_vars = input$select.grouping,
    sorting_var = input$select_sorting,
    event_colors = NULL
) {
  megaplot_data_splitted_by_event_group <- split(megaplot_data, megaplot_data$event_group)

  # create a event_group id ("event_group_id") and a event id ("event_id") for every event (group)
  # create a variable "max_event_id" to get the number of events in a specific group (used for color_func)
  for(i in 1:length(megaplot_data_splitted_by_event_group)) {
    megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
      dplyr::group_by(event) %>%
      dplyr::mutate(
        event_id = dplyr::cur_group_id(),
        event_group_id = i
      )
    megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
      dplyr::mutate(
        max_event_id = max(megaplot_data_splitted_by_event_group[[i]]$event_id)
      )
  }

  #bind all list entries back to one data.frame
  megaplot_data_w_event_ids <- do.call("rbind",megaplot_data_splitted_by_event_group)

  #add variables event_color and jitter_event_time to event column
  megaplot_data_w_jitter <- megaplot_data_w_event_ids  %>%
    dplyr::select(event, event_group, max_event_id, event_group_id, event_id) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(jitter_event_time = seq(-0.1,0.1,length = nrow(.))[dplyr::row_number()])

  megaplot_data_w_color_and_jitter<- megaplot_data_w_jitter %>%
    dplyr::left_join(event_colors, by = c("event"))

  megaplot_data_arranged <- dplyr::arrange(megaplot_data, !!!rlang::syms(grouping_vars), !!rlang::sym(sorting_var))

  megaplot_data_arranged_w_subject_index  <- megaplot_data_arranged %>%
    dplyr::left_join(data.frame(subjectid = unique(megaplot_data_arranged$subjectid), subject_index = seq_along(unique(megaplot_data_arranged$subjectid))), by = "subjectid")

  megaplot_data_arranged_w_group_index <- megaplot_data_arranged_w_subject_index  %>%
    dplyr::group_by(!!!rlang::syms(grouping_vars)) %>%
    dplyr::mutate(group_index = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  megaplot_data_with_group_distance <- megaplot_data_arranged_w_group_index   %>%
    dplyr::mutate(subjectid_n = subject_index + (group_index-1)*10) %>%
    dplyr::left_join(megaplot_data_w_color_and_jitter, by = c("event"))%>%
    dplyr::mutate(subjectid_n_jittered = subjectid_n + jitter_event_time)

  ### add checks if all variables are created

  return(megaplot_data_with_group_distance)
}
