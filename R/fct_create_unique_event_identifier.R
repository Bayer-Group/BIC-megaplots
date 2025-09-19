#' create_unique_event_identifier
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_unique_event_identifier <- function(
    megaplot_data_raw
) {

  #create a data frame with unique combinations of event_group and event
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::select(event_group, event) %>%
    dplyr::distinct()

  #Split unique combinations by event group
  megaplot_data_splitted_by_event_group <- split(megaplot_data_raw, megaplot_data_raw$event_group)

  # Create and save an identifier variable ("event_id" & "event_group_id") for every event and event group and the number of events within
  # a group ("max_event_id"). These variables will be used for the color function to create a unique color for every event
  for(i in 1:length(megaplot_data_splitted_by_event_group)) {
    # create "event_id" & "event_group_id"
    megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
      dplyr::group_by(event) %>%
      dplyr::mutate(
        event_id = dplyr::cur_group_id(),
        event_group_id = i
      )

    #create "max_event_id"
    megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
      dplyr::mutate(
        max_event_id = max(megaplot_data_splitted_by_event_group[[i]]$event_id)
      )
    megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]]%>%
      dplyr::mutate(event_n = suppressWarnings(as.numeric(event))) %>%
      dplyr::mutate(n_flag = dplyr::case_when(event == event_n ~TRUE))

    # Add row for event_group with event_id = 0. This will only be used to colorize all
    # events within a group with specific color (shades).
    megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
      dplyr::ungroup() %>%
      dplyr::add_row(
        event_group = unique(megaplot_data_splitted_by_event_group[[i]]$event_group),
        event = NA,
        event_id = 0,
        event_group_id = unique(megaplot_data_splitted_by_event_group[[i]]$event_group_id),
        max_event_id = unique(megaplot_data_splitted_by_event_group[[i]]$max_event_id),
      )
  }

  #bind all list entries rowwise to one data.frame
  megaplot_data_w_event_ids <- do.call("rbind",megaplot_data_splitted_by_event_group)

  number_event_groups <- length(unique(megaplot_data_w_event_ids$event_group))
  #add variables event_color


  megaplot_event_data_w_color <- megaplot_data_w_event_ids  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      event_color = color_func(event_id, event_group_id, max_event_id, number_event_groups = number_event_groups)
    )

  megaplot_event_data_w_color <- megaplot_event_data_w_color %>%
    dplyr::mutate(
      gradient_event_color_2 = dplyr::case_when(
        event_id == 0 ~event_color,
        event_id != 0 ~ NA
      ),
      gradient_event_color_1 = dplyr::case_when(
          event_id == 0 ~ colorRampPalette(c("white",event_color))(100)[50],
          event_id != 0 ~ NA
        ),
      gradient_event_color_3 = dplyr::case_when(
        event_id == 0 ~ colorRampPalette(c(event_color,"black"))(100)[50],
        event_id != 0 ~ NA
      )
    )

  megaplot_event_data_w_color <- megaplot_event_data_w_color %>% dplyr::mutate(
    jittered = TRUE
  )



  # add created color vector to reactive object color_data$all

  return(megaplot_event_data_w_color)
}
