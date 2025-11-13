#' Draws a line chart with daily counts of events
#'
#' @param megaplot_prepared_data data frame with subject information used for calculation of min and max day
#' @param megaplot_filtered_data data frame with event information used to create event count lines
#' @param select_grouping character vector with grouping variables
#' @param event_summary_cutoff numeric value used as cutoff for hover labels displayed
#'
#' @return
#' @export
#'
#' @examples

draw_event_summary <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select_grouping = NULL,
    event_summary_cutoff = 1
) {

  megaplot_filtered_data <- megaplot_filtered_data %>%
    dplyr::select(-tidyselect::all_of(c("subjectid_n_jittered", "jitter_event_time"))) %>%
    dplyr::filter(!is.na(megaplots_selected_subjectid))


  if (!is.null(select_grouping)) {
    number_group_levels <-  max(megaplot_filtered_data$group_index, na.rm = TRUE)
  } else {
    number_group_levels <- 1
  }

  #initialize list for figures used in subplots (when multiple groups are selected)
  figure_list <- list()
  max_y_range <- c()
  x_min <- min(min(megaplot_prepared_data$start_time,na.rm = TRUE), min(megaplot_prepared_data$megaplots_selected_event_time,na.rm=TRUE))
  x_max <- max(max(megaplot_prepared_data$end_time,na.rm = TRUE), max(megaplot_prepared_data$megaplots_selected_event_time_end,na.rm=TRUE))

  if (!is.null(select_grouping)) {
    label_df <- megaplot_prepared_data %>%
      dplyr::group_by(.data$group_index) %>%
      dplyr::mutate(
        text_position_y = max(.data$subjectid_n, na.rm = TRUE) + 2,
        text_position_x = min(.data$start_time, na.rm = TRUE)
      ) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(tidyselect::all_of(c("subjectid_n","group_index","text_position_y","text_position_x")))

  megaplot_prepared_data_w_group_text <- megaplot_prepared_data  %>%
    dplyr::left_join(label_df, by = c("group_index", "subjectid_n")) %>%
    dplyr::group_by(.data$text_position_y) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::filter(!is.na(.data$text_position_y)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      text_snippet_1 = paste(select_grouping, collapse = " "),
      text_snippet_2 = paste(!!!rlang::syms(select_grouping))
    ) %>%
    dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), unlist(strsplit(.data$text_snippet_2, " ")), sep = ": ", collapse = " & ")) %>%
    dplyr::mutate(event_color = "black")
  }
  for(k in 1:number_group_levels) {

    df <- megaplot_filtered_data %>%
      dplyr::arrange(.data$megaplots_selected_event_group, .data$unique_event) %>%
      dplyr::filter(!is.na(.data$megaplots_selected_event)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        day = list(seq(.data$megaplots_selected_event_time, .data$megaplots_selected_event_time_end, by = 1))
      ) %>%
      tidyr::unnest_longer(col = .data$day) %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(select_grouping,"group_index","megaplots_selected_event_group","megaplots_selected_event","unique_event","event_color","day")))) %>%
      dplyr::summarise(value = dplyr::n()) %>%
      tidyr::complete(
        day = seq(min(.data$day)-1,max(.data$day)+1, 1),
        fill = list(value = 0)
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(select_grouping,"group_index","megaplots_selected_event_group","megaplots_selected_event","unique_event","event_color","day")))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        tooltip = "x+text",
        tooltip_text = ifelse(.data$value < event_summary_cutoff, NA, paste0(.data$megaplots_selected_event,": ", .data$value))
      ) %>% dplyr::filter(.data$group_index == k)

    #update max count used for y axis range
    max_y_range <- max(max_y_range, max(df$value, na.rm = TRUE))

    #initial plotly figure
    fig <- plotly::plot_ly(data = df, x = ~ day)

    # add lines to plotly figure
    fig2 <- fig %>%
      plotly::add_lines(
        y = ~ value,
        color = ~I(event_color),
        line = list(shape = "linear", width = 3),
        name = ~ unique_event,
        text = ~ tooltip_text,
        hoverinfo = ~ tooltip,
        legendgroup = ~ megaplots_selected_event_group,
        legendgrouptitle = list(text = ~ megaplots_selected_event_group)
      )

    trace_info <- get_trace_info(fig2)
    fig2 <- apply_trace_info(trace_info,fig2)
    #update figure layout
    fig3 <- fig2 %>%
      plotly::layout(
        plot_bgcolor = "#404A4E",
        paper_bgcolor ='#404A4E',
        xaxis = list(
          color='#FFFFFF',
          title = "Study Day",
          zeroline = FALSE,
          spikemode = 'across+marker',
          spikethickness = 1,
          spikedash = "dash",
          spikecolor = "#FFFFFF"
        ),
        yaxis = list(
          color='#FFFFFF',
          showgrid = FALSE,
          title ="Event count",
          zeroline = FALSE,
          autotick = FALSE
        ),

        font = list(family = "Agency FB", color = "#FFFFFF"),
        barmode = "overlay"
      ) %>%
      plotly::layout(
        hovermode = "x",
        hoverdistance = 1
      )

    figure_list[[k]] <- fig3
  }

  for(k in 1:number_group_levels) {

    if (!is.null(select_grouping)) {
    figure_list[[k]] <- figure_list[[k]] %>%
      plotly::layout(annotations =list(list(x = mean(c(x_min, x_max)), y = max_y_range, showarrow = FALSE, xacnhor = 'center', yanchor = "top",text = megaplot_prepared_data_w_group_text$text_snippet_total[[k]])))
    }

    figure_list[[k]] <- figure_list[[k]] %>%
      plotly::layout(
        yaxis = list(
          range = c(0, max_y_range)
        ),
        xaxis = list(
          range = c(x_min, x_max)
        )
      )
  }

  g <- plotly::subplot(
    rev(figure_list),
    shareY = TRUE,
    shareX =TRUE,
    nrows = number_group_levels
  )

  g

}
