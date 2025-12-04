#' Draws a line chart with daily counts of events
#'
#' @param megaplot_prepared_data data frame with subject information used for
#' #calculation of min and max day
#' @param megaplot_filtered_data data frame with event information used to
#' create event count lines
#' @param select_grouping character vector with grouping variables
#' @param event_summary_cutoff numeric value used as cutoff for hover labels
#'  displayed
#' @param event_summary_selection character with type of event summary display
#' ("event_per_day"/"cumulative_event"/"event_by_subject_cumulative")
#' (default: "event_per_day")
#' @param switch_legend_grouping logical value if events should be grouped in
#'  plotly legend (default: TRUE)
#' @param hovermode character for plotly hovermode either "x" or "x unified"
#'  (default "x")
#'
#' @export
draw_event_summary <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select_grouping = NULL,
    event_summary_cutoff = 1,
    event_summary_selection,
    switch_legend_grouping = TRUE,
    hovermode = "x"
) {

  #get number of groups from variable group_index
  number_group_levels <-  max(megaplot_filtered_data$group_index, na.rm = TRUE)
  #initialize list for figures used in subplots (when multiple groups are selected)
  figure_list <- list()
  #create empty vector for maximum y_range (used for calculate maximum y-value over all subplots)
  max_y_range <- c()
  # calculate range for x-axis (min/max of all start/end times and event start/end times)
  x_min <- min(min(megaplot_prepared_data$megaplots_selected_start_time, na.rm = TRUE), min(megaplot_prepared_data$megaplots_selected_event_time, na.rm = TRUE))
  x_max <- max(max(megaplot_prepared_data$megaplots_selected_end_time, na.rm = TRUE), max(megaplot_prepared_data$megaplots_selected_event_time_end, na.rm = TRUE))

  #  one of the two cumulative displays are selected within app
  if (event_summary_selection == "event_by_subject_cumulative" | event_summary_selection == "cumulative_event") {

    #create title for every group in variable "text_snippet_total"
    if (!is.null(select_grouping)) {
      megaplot_prepared_data_w_group_text <- megaplot_filtered_data %>%
        dplyr::select(!!!rlang::syms(select_grouping), .data$group_index) %>%
        dplyr::distinct() %>%
        dplyr::mutate(text_snippet_1 = paste(select_grouping, collapse = " ")) %>%
        dplyr::mutate(text_snippet_2 = paste(!!!rlang::syms(select_grouping), sep = ", "))  %>%
        dplyr::rowwise() %>%
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))), sep = ": ", collapse = " & "))
    }

    # create cumulative total counts (for every group)
    for (k in 1:number_group_levels) {
      if (event_summary_selection == "event_by_subject_cumulative") {
        #recurring events are not taken into account
        megaplot_filtered_data_by_group <- megaplot_filtered_data %>%
          dplyr::filter(.data$group_index == k) %>%
          dplyr::group_by(.data$megaplots_selected_subjectid,.data$unique_event) %>%
          dplyr::arrange(.data$megaplots_selected_subjectid, .data$unique_event, .data$megaplots_selected_event_time) %>%
          dplyr::filter(dplyr::row_number() == 1)  %>%
          dplyr::ungroup()
      } else {
        megaplot_filtered_data_by_group <- megaplot_filtered_data %>%
          dplyr::filter(.data$group_index == k)
      }

      #calculate cumulative sum via function cumsum()
      megaplot_data_with_cumulative_sum <- megaplot_filtered_data_by_group %>%
        dplyr::group_by(.data$unique_event) %>%
        dplyr::arrange(.data$unique_event,.data$megaplots_selected_event_time) %>%
        dplyr::mutate(event_count = 1) %>%
        dplyr::mutate(cumulative_sum = cumsum(event_count))

      # create grid for every event and time point of study to receive hover information on every study day
      expand_grid <- expand.grid(
        unique_event = megaplot_filtered_data %>%
          dplyr::select(unique_event) %>%
          dplyr::distinct() %>%
          dplyr::pull(unique_event),
        megaplots_selected_event_time = x_min:x_max
      )  %>% dplyr::right_join(
        megaplot_filtered_data %>%
          dplyr::select(unique_event, megaplots_selected_event_group, event_group_id, event_id) %>%
          dplyr::distinct(),
        by = "unique_event"
      )

      #merge expand_grid and megaplot_data_with_cumulative_sum
      megaplot_data_with_cumulative_sum_for_every_day <- expand_grid %>%
        dplyr::left_join(
          megaplot_data_with_cumulative_sum,
          by = c("unique_event","megaplots_selected_event_time","megaplots_selected_event_group", "event_group_id","event_id")
        ) %>%
        dplyr::arrange(unique_event, megaplots_selected_event_time) %>%
        dplyr::group_by(unique_event) %>%
        tidyr::fill(cumulative_sum, event_color) %>%
        dplyr::select(cumulative_sum, megaplots_selected_event_time, event_color,unique_event, megaplots_selected_event_group,event_group_id,event_id) %>%
        dplyr::filter(!is.na(cumulative_sum))

      # add cumulative sum 0 for every event at minimum day "x_min"
      # -> this will preserve the correct legend and also ensures that a line between zero and one is drawn
      megaplot_data_with_initial_cumulative_sum_for_every_day <- megaplot_filtered_data %>%
        dplyr::select(event_color, unique_event, megaplots_selected_event_group, event_group_id, event_id) %>%
        dplyr::filter(!is.na(megaplots_selected_event_group)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          cumulative_sum = 0,
          megaplots_selected_event_time = x_min
        ) %>%
        dplyr::arrange(megaplots_selected_event_group)

      megaplot_data_with_cumulative_sums <- rbind(
        megaplot_data_with_initial_cumulative_sum_for_every_day,
        megaplot_data_with_cumulative_sum_for_every_day
      )

      #update maximum of y range to ensure same y-axis over all groups
      max_y_range <- max(max_y_range, max(megaplot_data_with_cumulative_sum_for_every_day$cumulative_sum, na.rm = TRUE))

      #re-arrangement for plotly legend (factorize variable unique_event)
      megaplot_data_with_cumulative_sums <- megaplot_data_with_cumulative_sums %>%
        dplyr::arrange(.data$event_group_id, .data$event_id)
      megaplot_data_with_cumulative_sums$unique_event <- factor(megaplot_data_with_cumulative_sums$unique_event , levels = unique(megaplot_data_with_cumulative_sums$unique_event))

      #initial scatter plot
      fig <- plotly::plot_ly(
        data = megaplot_data_with_cumulative_sums,
        y = ~ cumulative_sum,
        x = ~ megaplots_selected_event_time
      )

      #add lines to initial figure
        if (switch_legend_grouping) {
          fig2 <- fig  %>%
            plotly::add_lines(
              color = ~I(event_color),
              line = list(shape = "hv", width = 3),
              name = ~ unique_event,
              showlegend = FALSE,
              legendgroup = ~ megaplots_selected_event_group,
              legendgrouptitle = list(text = ~ megaplots_selected_event_group)
            )
        } else {
          fig2 <- fig  %>%
            plotly::add_lines(
              color = ~I(event_color),
              line = list(shape = "hv", width = 3),
              name = ~ unique_event,
              showlegend = FALSE,
              legendgroup = ~ unique_event
            )
        }

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
            showgrid = TRUE,
            title = "Cumulative event count per day",
            zeroline = FALSE,
            autotick = TRUE
          ),
          font = list(family = "Agency FB", color = "#FFFFFF")#,
          # barmode = "stack"
        )
      figure_list[[k]] <- fig3
    }

    for (k in 1:number_group_levels) {
      if (!is.null(select_grouping)) {
        megaplot_prepared_data_w_group_text_sorted <- megaplot_prepared_data_w_group_text %>%
          dplyr::arrange(.data$group_index)
        figure_list[[k]] <- figure_list[[k]] %>%
          plotly::layout(annotations =list(list(x = mean(c(x_min, x_max)), y = max_y_range, showarrow = FALSE, xacnhor = 'center', yanchor = "top", text = megaplot_prepared_data_w_group_text_sorted$text_snippet_total[[k]])))
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

    #show legend for first figure to avoid duplicated legend entries
    figure_list[[1]] <- plotly::style(figure_list[[1]], showlegend = TRUE)

    #combine subplots to one plotly object
    g <- plotly::subplot(
      rev(figure_list),
      shareY = TRUE,
      shareX = TRUE,
      nrows = number_group_levels
    )

    g <- g %>%
      plotly::layout(
        hovermode = hovermode,
        hoverdistance = 1
      )

  } else {

    if (!is.null(select_grouping)) {
      megaplot_prepared_data_w_group_text <- megaplot_filtered_data %>%
        dplyr::select(!!!rlang::syms(select_grouping), .data$group_index) %>%
        dplyr::distinct() %>%
        dplyr::mutate(text_snippet_1 = paste(select_grouping, collapse = " ")) %>%
        dplyr::mutate(text_snippet_2 = paste(!!!rlang::syms(select_grouping), sep = ", "))  %>%
        dplyr::rowwise() %>%
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))), sep = ": ", collapse = " & "))
    }

    df <- megaplot_filtered_data %>%
      dplyr::arrange(.data$megaplots_selected_event_group, .data$unique_event) %>%
      dplyr::filter(!is.na(.data$megaplots_selected_event)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        day = list(seq(.data$megaplots_selected_event_time, .data$megaplots_selected_event_time_end, by = 1))
      ) %>%
      tidyr::unnest_longer(col = .data$day) %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(select_grouping,"group_index","event_group_id","event_id","megaplots_selected_event_group","megaplots_selected_event","unique_event","event_color","day")))) %>%
      dplyr::summarise(value = dplyr::n()) %>%
      tidyr::complete(
        day = seq(x_min,x_max, 1),
        fill = list(value = 0)
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(select_grouping,"group_index","megaplots_selected_event_group","megaplots_selected_event","unique_event","event_color","day")))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        tooltip = "x+text",
        tooltip_text = ifelse(.data$value < event_summary_cutoff, NA, paste0(.data$megaplots_selected_event,": ", .data$value))
      )

    for(k in 1:number_group_levels) {

      df_group <- df %>% dplyr::filter(.data$group_index == k)

      #update max count used for y axis range
      max_y_range <- max(max_y_range, max(df_group$value, na.rm = TRUE))

      df_group <- df_group %>%
        dplyr::arrange(.data$event_group_id, .data$event_id)
      df_group$unique_event <- factor(df_group$unique_event , levels = unique(df_group$unique_event))

      #initial plotly figure
      fig <- plotly::plot_ly(data = df_group, x = ~ day)

      # add lines to plotly figur
      if (hovermode == "x") {
        if (switch_legend_grouping) {
          fig2 <- fig %>%
            plotly::add_lines(
              y = ~ value,
              color = ~I(event_color),
              line = list(shape = "hv", width = 3),
              name = ~ unique_event,
              text = ~ tooltip_text,
              showlegend = FALSE,
              hoverinfo = ~ tooltip,
              legendgroup = ~ megaplots_selected_event_group,
              legendgrouptitle = list(text = ~ megaplots_selected_event_group)
            )
        } else {
          fig2 <- fig %>%
            plotly::add_lines(
              y = ~ value,
              color = ~I(event_color),
              line = list(shape = "hv", width = 3),
              name = ~ unique_event,
              showlegend = FALSE,
              hoverinfo = ~ tooltip,
              text = ~ tooltip_text,
              legendgroup = ~ unique_event
            )
        }
      } else {
        if (switch_legend_grouping) {
          fig2 <- fig %>%
            plotly::add_lines(
              y = ~ value,
              color = ~I(event_color),
              line = list(shape = "hv", width = 3),
              name = ~ unique_event,
              showlegend = FALSE,
              legendgroup = ~ megaplots_selected_event_group,
              legendgrouptitle = list(text = ~ megaplots_selected_event_group)
            )
        } else {
          fig2 <- fig %>%
            plotly::add_lines(
              y = ~ value,
              color = ~I(event_color),
              line = list(shape = "hv", width = 3),
              name = ~ unique_event,
              showlegend = FALSE,
              legendgroup = ~ unique_event
            )
        }
      }

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
            showgrid = TRUE,
            title ="Event count per day",
            zeroline = FALSE,
            autotick = TRUE
          ),
          font = list(family = "Agency FB", color = "#FFFFFF")
        )

      figure_list[[k]] <- fig3
    }

    for (k in 1:number_group_levels) {
      if (!is.null(select_grouping)) {
        megaplot_prepared_data_w_group_text_sorted <- megaplot_prepared_data_w_group_text %>%
          dplyr::arrange(.data$group_index)
        figure_list[[k]] <- figure_list[[k]] %>%
          plotly::layout(annotations =list(list(x = mean(c(x_min, x_max)), y = max_y_range, showarrow = FALSE, xacnhor = 'center', yanchor = "top", text = megaplot_prepared_data_w_group_text_sorted$text_snippet_total[[k]])))
      }

      figure_list[[k]] <- figure_list[[k]] %>%
        plotly::layout(
          yaxis = list(
            range = c(0, max_y_range), matches = TRUE
          ),
          xaxis = list(
            range = c(x_min, x_max)
          )
        )
    }

    figure_list[[1]] <- plotly::style(figure_list[[1]], showlegend = TRUE)

    g <- plotly::subplot(
      rev(figure_list),
      shareY = TRUE,
      shareX =TRUE,
      nrows = number_group_levels
    )

    g <- g %>%
      plotly::layout(
        hovermode = hovermode,
        hoverdistance = 1
      )
  }
}
