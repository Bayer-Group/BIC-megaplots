#' Draws a 'Megaplot' graph
#'
#' @param megaplot_prepared_data data.frame with subject information used for the subject lines
#' @param megaplot_filtered_data data.frame with selected events for the event lines
#' @param select_grouping character vector with grouping variable names
#' @param line_width numeric value for event line width
#' @param line_width_subjects numeric value for subject line width
#' @param event_tooltips logical value if event tooltips should be turned on/off
#' @param switch_legend_grouping logical value if events should be grouped in
#' @param sort_event_groups character vector with drag and drop order of event groups
#' @param sequencing_object seriation object created with TraMineR package
#' @param sequencing_switch logical value of sequencing should be turned on/off
#' @param reference_line_1_value numeric value with start value for first reference line/rect (x-axis)
#' @param reference_line_2_value numeric value with start value for second reference line/rect (x-axis)
#' @param reference_line_3_value numeric value with start value for third reference line/rect (x-axis)
#' @param reference_line_1_value2 numeric value with end value for first reference line/rect (x-axis)
#' @param reference_line_2_value2 numeric value with end value for second reference line/rect (x-axis)
#' @param reference_line_3_value2 numeric value with end value for third reference line/rect (x-axis)
#' @param reference_line_1 logical value if first reference line/rect should be drawn
#' @param reference_line_2 logical value if second reference line/rect should be drawn
#' @param reference_line_3 logical value if third reference line/rect should be drawn
#' @param reference_line_1_color character with hex color code for first reference line/rect
#' @param reference_line_2_color character with hex color code for second reference line/rect
#' @param reference_line_3_color character with hex color code for third reference line/rect
#' @param theme character with app theme ["dark"/"light"]
#' @param line_color_dark character with hex color for subject line (darkmode)
#' @param line_color_light character with hex color for subject line (lightmode)
#' @param circular_vision logical value if a circular vision should be turned on/off
#'  plotly legend (default: TRUE)
#'
#' @export
draw_mega_plot <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select_grouping = NULL,
    line_width = 3,
    line_width_subjects = 3,
    event_tooltips = TRUE,
    switch_legend_grouping = TRUE,
    sort_event_groups = NULL,
    sequencing_object = NULL,
    sequencing_switch = FALSE,
    reference_line_1,
    reference_line_2,
    reference_line_3,
    reference_line_1_value,
    reference_line_2_value,
    reference_line_3_value,
    reference_line_1_value2,
    reference_line_2_value2,
    reference_line_3_value2,
    reference_line_1_color,
    reference_line_2_color,
    reference_line_3_color,
    theme,
    line_color_dark,
    line_color_light,
    circular_vision
  ) {


  if (nrow(megaplot_prepared_data) == 0) {
    stop("megaplot_prepared_data is empty.")
  }

  #check if sequencing switch is on and if a sequencing object is available
  if (sequencing_switch) {
    if (!is.null(sequencing_object)) {
      #Save Sequencing with corresponding identifier
      sequencing_ranks_data <- data.frame(
        megaplots_selected_subjectid = as.numeric(names(seriation::get_rank(sequencing_object))),
        SEQUENCING = as.vector(seriation::get_rank(sequencing_object))
      )

      #Add identifier which might not be available in the sequencing object
      subjectids_without_sequencing_rank <- unique(megaplot_prepared_data$megaplots_selected_subjectid)[!unique(megaplot_prepared_data$megaplots_selected_subjectid) %in% sequencing_ranks_data$megaplots_selected_subjectid]

      #Merge missing identifier if applicable
      if (length(subjectids_without_sequencing_rank) > 0 ) {
        sequencing_unranked_data <- data.frame(
          megaplots_selected_subjectid = subjectids_without_sequencing_rank,
          SEQUENCING = (nrow(sequencing_ranks_data) + 1):(nrow(sequencing_ranks_data)+length(subjectids_without_sequencing_rank))
        )

        #add rows of missing identifier
        if (nrow(sequencing_unranked_data) > 0) {
          sequencing_ranks_data <- rbind(sequencing_ranks_data, sequencing_unranked_data)
        }
      }

      # join/merge Sequencing order to prepared data
      megaplot_prepared_data_w_ranks <- megaplot_prepared_data |>
        dplyr::ungroup() |>
        dplyr::left_join(
          sequencing_ranks_data,
          by = "megaplots_selected_subjectid"
        )

      # get number of subjects
      n_subjects <- megaplot_prepared_data_w_ranks$megaplots_selected_subjectid |>
        unique() |>
        length()

      sequencing_grouped_data <- megaplot_prepared_data_w_ranks |>
        dplyr::mutate(n_subjects = n_subjects) |>
        dplyr::select(.data$megaplots_selected_subjectid, .data$SEQUENCING, .data$group_index, n_subjects) |>
        dplyr::mutate(SEQUENCING_grouped = .data$SEQUENCING + (.data$group_index - 1) * n_subjects)  |>
        dplyr::distinct() |>
        dplyr::mutate(SEQUENCING_grouped_ranked = rank(.data$SEQUENCING_grouped)) |>
        dplyr::select(.data$megaplots_selected_subjectid, .data$SEQUENCING_grouped_ranked) |>
        dplyr::ungroup()

      megaplot_prepared_data <- megaplot_prepared_data |>
        dplyr::left_join(
          sequencing_grouped_data,
          by = "megaplots_selected_subjectid"
        ) |> dplyr::mutate(subjectid_n = .data$SEQUENCING_grouped_ranked + (.data$group_index - 1) * 10)

      megaplot_filtered_data <- megaplot_filtered_data |>
        dplyr::left_join(
          sequencing_grouped_data,
          by = "megaplots_selected_subjectid"
        ) |>
        dplyr::mutate(
          subjectid_n = .data$SEQUENCING_grouped_ranked + (.data$group_index - 1) * 10,
          subjectid_n_jittered = (.data$SEQUENCING_grouped_ranked + (.data$group_index - 1) * 10) + .data$jitter_event_time
        )
    }
  }

  min_start_day <- min(megaplot_prepared_data$megaplots_selected_event_time, na.rm = TRUE)
  max_end_day <- max(megaplot_prepared_data$megaplots_selected_event_time_end, na.rm = TRUE)

  if (!is.null(megaplot_filtered_data)) {
    megaplot_filtered_data <- megaplot_filtered_data |>
      dplyr::mutate(
        text_events = paste0(" Subject identifier: ", .data$megaplots_selected_subjectid, "\n Event: ", .data$megaplots_selected_event, " (",.data$megaplots_selected_event_group,") \n", " Start time: ", .data$megaplots_selected_event_time, "\n End time: ", .data$megaplots_selected_event_time_end)
      )

    #re-arrangement for plotly legend
    megaplot_filtered_data$event_group <- factor(megaplot_filtered_data$event_group, levels = sort_event_groups)

    megaplot_filtered_data <- megaplot_filtered_data |>
      dplyr::arrange(.data$event_group, .data$event_id)


    megaplot_filtered_data$unique_event <- factor(megaplot_filtered_data$unique_event , levels = unique(megaplot_filtered_data$unique_event))
  }

  megaplot_prepared_data  <- megaplot_prepared_data |>
    dplyr::select(
      tidyselect::all_of(
        c("megaplots_selected_subjectid", "subjectid_n",
          "megaplots_selected_start_time", "megaplots_selected_end_time",
          "group_index", select_grouping
        )
      )
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      text_lines = paste0("Subject identifier: ", .data$megaplots_selected_subjectid)
    )

  if (circular_vision) {

   min_start_day_to_one <- ifelse(min_start_day <= 0, abs(min_start_day - 1), 0)

    number_subjects <- length(megaplot_prepared_data$subjectid_n)
    time <- megaplot_prepared_data$subjectid_n/(number_subjects)
    megaplot_prepared_data <- megaplot_prepared_data |>
      dplyr::mutate(
        megaplots_selected_start_time = megaplot_prepared_data$megaplots_selected_start_time*cos(2*pi*time), #x
        megaplots_selected_end_time = megaplot_prepared_data$megaplots_selected_end_time*cos(2*pi*time), #xend
        subjectid_n = megaplot_prepared_data$megaplots_selected_start_time*sin(2*pi*time), #y
        subjectid_n_end = megaplot_prepared_data$megaplots_selected_end_time*sin(2*pi*time),#yend
      )
    time2 <- megaplot_filtered_data$subjectid_n/(number_subjects)
    megaplot_filtered_data <- megaplot_filtered_data |>
      dplyr::mutate(
        megaplots_selected_event_time = (megaplot_filtered_data$megaplots_selected_event_time-0.45)*cos(2*pi*time2), # x
        megaplots_selected_event_time_end = (megaplot_filtered_data$megaplots_selected_event_time_end+0.45)*cos(2*pi*time2), #xend
        subjectid_n_jittered = c((megaplot_filtered_data$megaplots_selected_event_time -0.45)*sin(2*pi*time2)), #y
        subjectid_n_jittered_end = c((megaplot_filtered_data$megaplots_selected_event_time_end+0.45)*sin(2*pi*time2)) #yend
      )
  } else {

    megaplot_prepared_data <- megaplot_prepared_data |>
      dplyr::mutate(subjectid_n_end = .data$subjectid_n)
    if(!is.null(megaplot_filtered_data)) {
    megaplot_filtered_data <- megaplot_filtered_data |>
      dplyr::mutate(
        subjectid_n_jittered_end = .data$subjectid_n_jittered,
        megaplots_selected_event_time = .data$megaplots_selected_event_time -0.45,
        megaplots_selected_event_time_end = .data$megaplots_selected_event_time_end + 0.45
      )
    }
  }
  # initialize plotly object
  p_1 <- megaplot_prepared_data |>
    plotly::plot_ly(                            #create empty plot_ly object
      source = "plotSource",
      #color = ~I(event_color),
      type ="scatter",
      mode = "lines+markers"
    )

  #add subjects line when start_time and end_time is available
  if (!(all(is.na(megaplot_prepared_data$megaplots_selected_start_time)) | all(is.na(megaplot_prepared_data$megaplots_selected_end_time)))) {
    p_1 <- p_1 |>
    plotly::add_segments(                       # create subject lines via add_segments
      y = ~subjectid_n,
      yend ~ subjectid_n_end,
      x  = ~ megaplots_selected_start_time,
      hoverinfo = "text",
      text = ~ text_lines,
      xend = ~ megaplots_selected_end_time,
      line = list(color = ifelse(theme =="dark", line_color_dark,line_color_light), width = line_width_subjects),
      showlegend = FALSE
    )
  }

  #draw reference lines/rectangles (if applicable)
  if (reference_line_1) {
    if (!reference_line_2) {
      reference_line_2_value <- "NA"
      reference_line_2_value2 <- "NA"
    }
    if (!reference_line_3) {
      reference_line_3_value <- "NA"
      reference_line_3_value2 <- "NA"
    }
    p_1 <- p_1 |> plotly::layout(
      shapes = list(
        #use vrect function from utils_helpers.R
        vrect(reference_line_1_value, reference_line_1_value2, reference_line_1_color),
        vrect(reference_line_2_value, reference_line_2_value2, reference_line_2_color),
        vrect(reference_line_3_value, reference_line_3_value2, reference_line_3_color)
      )
    )
  }

  if (!is.null(megaplot_filtered_data)) {
    if (event_tooltips) {
      # if (switch_legend_grouping) {

        p_2 <- p_1 |>
          plotly::add_segments(
            data = plotly::highlight_key(megaplot_filtered_data |> dplyr::filter(is.na(.data$n_flag)), ~ megaplots_selected_event),
            legendgroup = ~ megaplots_selected_event_group,
            name = ~ unique_event,
            x = ~ megaplots_selected_event_time,
            xend = ~ megaplots_selected_event_time_end,
            y = ~ subjectid_n_jittered,
            yend = ~ subjectid_n_jittered_end,
            color = ~ I(event_color),
            line = list(color = ~ event_color, width = line_width),
            showlegend = TRUE,
            hoverinfo = "text",
            text = ~ text_events,
            hoverlabel = list(orientation = "h")
          ) |>
          plotly::highlight(~ megaplots_selected_event, on = "plotly_click", off = "plotly_doubleclick")


      if (!switch_legend_grouping) {
        p_2 <-  p_2 |>
          plotly::layout(legend = list(traceorder = "grouped", groupclick = "toggleitem"))
      }
      #else {


        # p_2 <- p_1 |>
        #   plotly::add_segments(
        #     data = plotly::highlight_key(megaplot_filtered_data |> dplyr::filter(is.na(.data$n_flag)), ~ megaplots_selected_event),
        #     name = ~ unique_event,
        #     x = ~ megaplots_selected_event_time,
        #     xend =~ megaplots_selected_event_time_end,
        #     y = ~subjectid_n_jittered,
        #     yend = ~subjectid_n_jittered,
        #     color = ~I(event_color),
        #     line = list(color = ~ event_color, width = line_width),
        #     showlegend = TRUE,
        #     hoverinfo = "text",
        #     text = ~ text_events,
        #     hoverlabel = list(orientation = "h")
        #   ) |>
        #   plotly::highlight(~ megaplots_selected_event, on = "plotly_click", off="plotly_doubleclick")
      # }
    }# else {
    #   p_2 <- p_1 |>
    #     plotly::add_segments(
    #       data = plotly::highlight_key(megaplot_filtered_data, ~ megaplots_selected_event),
    #       legendgroup = ~ megaplots_selected_event_group,
    #       name = ~ unique_event,
    #       x = ~ megaplots_selected_event_time - 0.45,
    #       xend = ~ megaplots_selected_event_time_end + 0.45,
    #       y = ~ subjectid_n_jittered,
    #       yend = ~subjectid_n_jittered,
    #       color = ~I(event_color),
    #       line = list(color = ~ event_color, width = line_width),
    #       showlegend = TRUE,
    #        hoverinfo = "none",
    #       # text = ~ text_events,
    #       hoverlabel = list(orientation = "h")
    #     ) |>
    #     plotly::highlight(~ megaplots_selected_event, on = "plotly_click", off="plotly_doubleclick")
    # }
    trace_info <- get_trace_info(p_2)
    p_2 <- apply_trace_info(trace_info, p_2)

    if (!is.null(select_grouping)) {
      label_df <- megaplot_prepared_data |>
        #dplyr::select(subject_index, subjectid_n, group_index, sex, treatment) |>
        dplyr::group_by(.data$group_index) |>
        dplyr::mutate(
          text_position_y = max(.data$subjectid_n, na.rm = TRUE) + 2,
          text_position_x = min_start_day
        ) |>
        dplyr::filter(dplyr::row_number() == 1) |>
        dplyr::ungroup() |>
        dplyr::select(tidyselect::all_of(c("subjectid_n","group_index","text_position_y","text_position_x")))

      megaplot_prepared_data_w_group_text <- megaplot_prepared_data  |>
        dplyr::left_join(label_df, by = c("group_index", "subjectid_n")) |>
        dplyr::group_by(.data$text_position_y) |>
        dplyr::filter(dplyr::row_number() == 1) |>
        dplyr::filter(!is.na(.data$text_position_y)) |>
        dplyr::rowwise()

        megaplot_prepared_data_w_group_text <- megaplot_prepared_data_w_group_text |> dplyr::mutate(
          text_snippet_1 = paste(select_grouping, collapse = " "),
          text_snippet_2 = paste(!!!rlang::syms(select_grouping), sep = ", ")
        )

      megaplot_prepared_data_w_group_text <- megaplot_prepared_data_w_group_text |>
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))), sep = ": ", collapse = " & ")) |>
        dplyr::mutate(event_color = "black")

      p_2 <- p_2 |>
        plotly::add_trace(
          data = megaplot_prepared_data_w_group_text,
          type = "scatter",
          mode = "text",
          text = ~text_snippet_total,
          textfont = list(color = ifelse(theme =="dark","#fff","#000"),size = 20), # font size grouping
          textposition = "middle right",
          x = ~text_position_x,
          y = ~text_position_y,
          showlegend = FALSE
        )

    }
  } else {
    p_2 <- p_1
  }
  #Plotly configuration of modebar
  p_3 <- p_2 |> plotly::config(
    scrollZoom = TRUE,                  #Enable Scroll Zoom
    displayModeBar = TRUE,              #Forcing the modebar always to be visible
    displaylogo = FALSE,                #Hiding the plotly logo on the modebar
    modeBarButtonsToRemove =            #Remove not needed buttons from modebar
      c("toImage","select2d","lasso2d","hoverCompareCartesian","hoverClosestCartesian","autoScale2d")
  )

  p_4 <- p_3 |>
    plotly::layout(
      plot_bgcolor = ifelse(theme =="dark","#1D1F21","#fff"),
      paper_bgcolor = ifelse(theme =="dark","#1D1F21","#fff"),
      xaxis = list(
        color=ifelse(theme =="dark","#fff","#000"),
        title = "Day",
        zeroline = FALSE
      ),
      legend = list(
        orientation = 'v'
      ),
      yaxis = list(
        color='#FFFFFF',
        showgrid = FALSE,
        title ="Identifier",
        categoryarray = ~ megaplots_selected_subjectid,
        zeroline = FALSE,
        autotick = FALSE,
        showticklabels = FALSE#,
        ## for circle vision only
        # scaleratio = 1,
        # scaleanchor = "x"
      ),
      font = list(
        family = "Agency FB",
        color = ifelse(theme =="dark","#fff","#000")
      ),
      barmode = "overlay"#,
      #hovermode = "x unified"
    ) |>
    plotly::config(
      toImageButtonOptions = list(format = "svg", filename = "Megaplot")
    ) |>
    plotly::config(
      doubleClick = FALSE
    )

  return(p_4)
}
