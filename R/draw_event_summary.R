#' Draws a line chart with daily counts of events
#'
#' @param megaplot_prepared_data data.frame with subject information used for calculation of min and max day
#' @param megaplot_filtered_data data.frame with event information used to create event count lines
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
    dplyr::select(-subjectid_n_jittered, -jitter_event_time)


  if (!is.null(select_grouping)) {
    number_group_levels <-  max(megaplot_filtered_data$group_index, na.rm = TRUE)
  } else {
    number_group_levels <- 1
  }

  #initialize list for figures used in subplots (when multiple groups are selected)
  figure_list <- list()
  max_y_range <- c()
  x_min <- min(min(megaplot_prepared_data$start_time,na.rm = TRUE), min(megaplot_prepared_data$event_time,na.rm=TRUE))
  x_max <- max(max(megaplot_prepared_data$end_time,na.rm = TRUE), max(megaplot_prepared_data$event_time_end,na.rm=TRUE))


  for(k in 1:number_group_levels) {

    df <- megaplot_filtered_data %>%
      dplyr::arrange(event_group, unique_event) %>%
      dplyr::filter(!is.na(event)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        day = list(seq(event_time, event_time_end, by = 1))
      ) %>%
      tidyr::unnest_longer(col = day) %>%
      dplyr::group_by(dplyr::across(all_of(c(select_grouping,"group_index","event_group","event","unique_event","event_color","day")))) %>%
      dplyr::summarise(value = dplyr::n()) %>%
      tidyr::complete(
        day = seq(min(day)-1,max(day)+1, 1),
        fill = list(value = 0)
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(select_grouping,"group_index","event_group","event","unique_event","event_color","day")))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        tooltip = "x+text",
        tooltip_text = ifelse(value < event_summary_cutoff, NA, paste0(event,": ", value))
      ) %>% dplyr::filter(group_index == k)

    #update max count used for y axis range
    max_y_range <- max(max_y_range,max(df$value, na.rm = TRUE))

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
        legendgroup = ~event_group,
        legendgrouptitle = list(text = ~event_group)
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
