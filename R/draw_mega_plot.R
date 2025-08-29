#' Title
#'
#' @param megaplot_prepared_data
#' @param megaplot_filtered_data
#' @param select_grouping
#' @param line_width
#'
#' @return
#' @export
#'
#' @examples
draw_mega_plot <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select_grouping = NULL,
    line_width = 3
  ) {
  megaplot_prepared_data <- megaplot_prepared_data %>% dplyr::mutate(
    text_lines = paste0("Subject identifier: ", subjectid)
  )


  if(!is.null(megaplot_filtered_data)) {
    megaplot_filtered_data <- megaplot_filtered_data %>% dplyr::mutate(
      text_events = paste0(" Subject identifier: ", subjectid, "\n Event: ", event, " (",event_group,") \n", " Start time: ",event_time, "\n End time: ", event_time_end)
    )
  }

  p_1 <- megaplot_prepared_data %>%
    plotly::plot_ly(                            #create empty plot_ly object
      source = "plotSource",
      color = ~I(event_color),
      type ="scatter",
      mode = "lines+markers"
    ) %>%
    plotly::add_segments(
      y = ~subjectid_n,
      yend ~subjectid_n,
      x  = ~start_time,
      hoverinfo = "text",
      text = ~ text_lines,
      xend = ~ end_time,
      line = list(color = "#2c3336", width = 1),
      showlegend = FALSE
    )
  if(!is.null(megaplot_filtered_data)) {
  p_2 <- p_1 %>%
    plotly::add_segments(
      data = plotly::highlight_key(megaplot_filtered_data, ~event),
      legendgroup = ~ event_group,
      name = ~ unique_event,
      x = ~event_time - 0.45,
      xend =~event_time_end + 0.45,
      y = ~subjectid_n_jittered,
      yend = ~subjectid_n_jittered,
      line = list(color = ~ event_color, width = line_width),
      showlegend = TRUE,
      hoverinfo = "text",
      text = ~ text_events,
      hoverlabel = list(orientation = "h")
    ) %>%
    plotly::highlight(~ event, on = "plotly_click", off="plotly_doubleclick")

  trace_info <- get_trace_info(p_2)
  p_2 <- apply_trace_info(trace_info,p_2)

  if (!is.null(select_grouping)) {
    label_df <- megaplot_prepared_data %>%
      #dplyr::select(subject_index, subjectid_n, group_index, sex, treatment) %>%
      dplyr::group_by(group_index) %>%
      dplyr::mutate(text_position_y = max(subjectid_n) + 2) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(subjectid_n,group_index,text_position_y)

    megaplot_prepared_data_w_group_text <- megaplot_prepared_data  %>%
      dplyr::left_join(label_df, by = c("group_index", "subjectid_n")) %>%
      dplyr::group_by(text_position_y) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::filter(!is.na(text_position_y)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        text_snippet_1 = paste(select_grouping, collapse = " "),
        text_snippet_2 = paste(!!!rlang::syms(select_grouping))
      ) %>%
      dplyr::mutate(text_snippet_total = paste(unlist(strsplit(text_snippet_1," ")), unlist(strsplit(text_snippet_2, " ")), sep = ": ", collapse = " & ")) %>%
      dplyr::mutate(event_color = "black")

    p_2 <- p_2 %>% plotly::add_trace(
      data = megaplot_prepared_data_w_group_text,
      type = "scatter",
      mode = "text",
      text = ~text_snippet_total,
      textfont = list(color = "white"),
      textposition = "middle right",
      x = 1,
      y = ~text_position_y,
      showlegend = FALSE
    )

  }
  } else {
    p_2 <- p_1
  }
  #Plotly configuration of modebar
  p_3 <- p_2 %>% plotly::config(
    scrollZoom = TRUE,                  #Enable Scroll Zoom
    displayModeBar = TRUE,              #Forcing the modebar always to be visible
    displaylogo = FALSE,                #Hiding the plotly logo on the modebar
    modeBarButtonsToRemove =            #Remove not needed buttons from modebar
      c("zoomIn2d","zoomOut2d","select2d","lasso2d","hoverCompareCartesian")
  )

  p_4 <- p_3 %>%
    plotly::layout(
      plot_bgcolor = "#404A4E",
      paper_bgcolor ='#404A4E',
      xaxis = list(
        color='#FFFFFF',
        title = "Study Day",
        zeroline = FALSE
      ),
      yaxis = list(
        color='#FFFFFF',
        showgrid = FALSE,
        title ="Subject identifier",
        categoryarray = ~subjectid,
        zeroline = FALSE,
        autotick = FALSE,
        showticklabels = FALSE
      ),
      font = list(family = "Agency FB", color = "#FFFFFF"),
      barmode = "overlay"#,
      #hovermode = "x unified"
    )

  return(p_4)
}
