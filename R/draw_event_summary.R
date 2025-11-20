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
    event_summary_cutoff = 1,
    event_summary_selection,
    switch_legend_grouping = TRUE
) {

  megaplot_filtered_data <- megaplot_filtered_data %>%
    dplyr::select(-tidyselect::all_of(c("subjectid_n_jittered", "jitter_event_time"))) %>%
    dplyr::filter(!is.na(megaplots_selected_subjectid))


  if (!is.null(select_grouping)) {
    number_group_levels <-  max(megaplot_filtered_data$group_index, na.rm = TRUE)
  } else {
    number_group_levels <- 1
  }

  figure_list <- list()
  max_y_range <- c()
  x_min <- min(min(megaplot_prepared_data$start_time,na.rm = TRUE), min(megaplot_prepared_data$megaplots_selected_event_time,na.rm=TRUE))
  x_max <- max(max(megaplot_prepared_data$end_time,na.rm = TRUE), max(megaplot_prepared_data$megaplots_selected_event_time_end,na.rm=TRUE))
  #initialize list for figures used in subplots (when multiple groups are selected)
  if (event_summary_selection == "event_by_subject_cumulative" | event_summary_selection == "cumulative_event") {

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
          text_snippet_2 = paste(!!!rlang::syms(select_grouping), sep = ", ")
        ) %>%
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))), sep = ": ", collapse = " & ")) %>%
        dplyr::mutate(event_color = "black")

    }

    for (k in 1:number_group_levels) {
      if (event_summary_selection == "event_by_subject_cumulative") {
        df <- megaplot_filtered_data %>%
          dplyr::filter(.data$group_index == k) %>%
          dplyr::group_by(.data$megaplots_selected_subjectid,.data$unique_event) %>%
          dplyr::arrange(.data$megaplots_selected_subjectid,.data$unique_event,.data$megaplots_selected_event_time) %>%
          dplyr::filter(dplyr::row_number() == 1)  %>%
          dplyr::ungroup()
      } else {
        df <- megaplot_filtered_data %>%
          dplyr::filter(.data$group_index == k)
      }
      df <- df %>%
        dplyr::group_by(.data$unique_event) %>%
        dplyr::arrange(.data$unique_event,.data$megaplots_selected_event_time) %>%
        dplyr::mutate(event_count = 1) %>%
        dplyr::mutate(cs = cumsum(event_count))

      expand_grid <- expand.grid(unique_event = megaplot_filtered_data %>% dplyr::select(unique_event) %>% dplyr::distinct() %>% dplyr::pull(unique_event), event_time = x_min:x_max)

      df2 <- expand_grid %>% dplyr::left_join(
        df, by = c("unique_event","event_time")) %>%
        dplyr::arrange(unique_event, event_time) %>%
        dplyr::group_by(unique_event) %>%
        tidyr::fill(cs, event_color) %>%
        dplyr::select(cs, event_time, event_color,unique_event, megaplots_selected_event_group) %>%
        dplyr::filter(!is.na(cs))

      max_y_range <- max(max_y_range, max(df2$cs, na.rm = TRUE))

      #initial plotly figure
      fig <- plotly::plot_ly(
        data = df2,
        y = ~ cs,
        x = ~ event_time
      )

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
            legendgroup = ~ unique_event,
            legendgrouptitle =  list(text = " ")
          )
      }

      # trace_info <- get_trace_info(fig2)
      # fig2 <- apply_trace_info(trace_info,fig2)

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
            title ="Event count",
            zeroline = FALSE,
            autotick = TRUE
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

    # max_y_range

    for (k in 1:number_group_levels) {
      if (!is.null(select_grouping)) {

        megaplot_prepared_data_w_group_text_sorted <- megaplot_prepared_data_w_group_text %>%
          dplyr::arrange(group_index)
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

    for (k in 1:number_group_levels) {
      if (!is.null(select_grouping)) {
        megaplot_prepared_data_w_group_text_sorted <- megaplot_prepared_data_w_group_text %>%
          dplyr::arrange(group_index)
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

    for (i in seq_along(figure_list)) {
      if (i == 1) {
        initial_legend_trace_info <- get_trace_info(figure_list[[1]])
        figure_list[[i]] <- plotly::style(figure_list[[i]],showlegend = TRUE)
      } else {
        legend_trace_info <- get_trace_info(figure_list[[i]])
        missing_traces <-  which(!(legend_trace_info$name %in% initial_legend_trace_info$name))
        if(length(missing_traces) != 0) {
          figure_list[[i]] <- plotly::style(figure_list[[i]],showlegend = TRUE, traces = missing_traces)
          initial_legend_trace_info  <- rbind(initial_legend_trace_info,legend_trace_info[missing_traces,])
        } else {
          figure_list[[i]] <- plotly::style(figure_list[[i]],showlegend = FALSE)
        }
      }
    }

    g <- plotly::subplot(
      rev(figure_list),
      shareY = TRUE,
      shareX =TRUE,
      nrows = number_group_levels
    )
  } else {

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
          text_snippet_2 = paste(!!!rlang::syms(select_grouping), sep = ", ")
        ) %>%
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))), sep = ": ", collapse = " & ")) %>%
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
          day = seq(min(day)-1,max(day)+1, 1),
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
            text = ~ tooltip_text,
            legendgroup = ~ unique_event,
            legendgrouptitle =  list(text = " ")
          )
      }

      # trace_info <- get_trace_info(fig2)
      # fig2 <- apply_trace_info(trace_info,fig2)

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
            title ="Event count",
            zeroline = FALSE,
            autotick = TRUE
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

    for (k in 1:number_group_levels) {
      if (!is.null(select_grouping)) {
        megaplot_prepared_data_w_group_text_sorted <- megaplot_prepared_data_w_group_text %>%
          dplyr::arrange(group_index)
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

    for (i in seq_along(figure_list)) {
      if (i == 1) {
        initial_legend_trace_info <- get_trace_info(figure_list[[1]])
        figure_list[[i]] <- plotly::style(figure_list[[i]],showlegend = TRUE)
      } else {
        legend_trace_info <- get_trace_info(figure_list[[i]])
        missing_traces <-  which(!(legend_trace_info$name %in% initial_legend_trace_info$name))
        if(length(missing_traces) != 0) {
          figure_list[[i]] <- plotly::style(figure_list[[i]],showlegend = TRUE, traces = missing_traces)
          initial_legend_trace_info  <- rbind(initial_legend_trace_info,legend_trace_info[missing_traces,])
        } else {
          figure_list[[i]] <- plotly::style(figure_list[[i]],showlegend = FALSE)
        }
      }
    }

    g <- plotly::subplot(
      rev(figure_list),
      shareY = TRUE,
      shareX =TRUE,
      nrows = number_group_levels
    )

    g

  }
}
