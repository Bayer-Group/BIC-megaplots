#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  uploaded_data <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$file, {
    shiny::req(input$file)
    megaplot_data <- base::get(load(
      file = input$file$datapath
    ))
    uploaded_data$val <- megaplot_data
    updateSelectizeInput(
      session,
      inputId = "select.grouping",
      choices = colnames(megaplot_data)[sapply(megaplot_data,class) %in% c("factor","character")],
      selected = NULL
    )

    shinyWidgets::updatePickerInput(
      inputId = 'select.events',
      choices = unique(megaplot_data$event),
      selected = NULL,
    )
  })

  output$mega_plots <- plotly::renderPlotly({
    megaplot_color <- c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
      "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
      "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
      "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
      "#fabed4", "#4263d8"
    )


    megaplot_data_total <- shiny::req(uploaded_data$val)
    # megaplot_data_total <- megaplot_data_total %>%
    #   dplyr::filter(event %in% input$select.events)
    megaplot_data_total <<- megaplot_data_total
    select.events <<- input$select.events

    # megaplot_data_totaö <- test_data
    #### ####
    test <- split(megaplot_data_total, megaplot_data_total$event_group)
    for(i in 1:length(test)) {
      test[[i]] <- test[[i]] %>% dplyr::group_by(event) %>% dplyr::mutate(
        event_id = dplyr::cur_group_id(),
        event_group_id = i
      )
      test[[i]] <- test[[i]] %>% dplyr::mutate(
        max_event_id = max(test[[i]]$event_id)
      )
    }

    color_func <- function(x,y,z) {
      colorRampPalette(
        c(colorRampPalette(c("white",megaplot_color[y]))(100)[85],
          colorRampPalette(c(megaplot_color[y],"black"))(100)[15]
        )) (z)[x]
    }

    test2 <- do.call("rbind",test)
    test3 <- test2 %>% dplyr::select(event,event_group,max_event_id,event_group_id, event_id) %>% distinct() %>% ungroup() %>%
      dplyr::mutate( jitter_event_time = seq(-0.1,0.1,length = nrow(.))[dplyr::row_number()]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
      event_color = color_func(event_id,event_group_id,max_event_id)
      ) %>%
      dplyr::select(
        event,event_color,jitter_event_time
      )

    megaplot_jitter_and_color <- test3
    # megaplot_jitter_and_color <- megaplot_data_total %>%
    #   dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))%>%
    #   dplyr::select(event,event_group) %>%
    #   distinct() %>%
    #   ungroup() %>%
    #   mutate(
    #     jitter_event_time = seq(-0.1,0.1,length = nrow(.))[dplyr::row_number()],
    #     event_color = megaplot_color[dplyr::row_number()]
    #   )
    #### ####

    grouping_vars <- input$select.grouping

    tmp <- megaplot_data_total
    tmp <- arrange(tmp, !!!rlang::syms(grouping_vars))
    levels(tmp$subjectid) <- unique(arrange(tmp, !!!rlang::syms(grouping_vars))$subjectid)
    tmp <- tmp %>% group_by(!!!rlang::syms(grouping_vars),subjectid)%>% dplyr::mutate(subject_index = dplyr::cur_group_id())%>% ungroup()
    tmp <- tmp %>% group_by(!!!rlang::syms(grouping_vars))%>% dplyr::mutate(group_index = dplyr::cur_group_id())%>% ungroup()
    tmp <- tmp  %>%
      dplyr::mutate(subjectid_n = subject_index + (group_index-1)*10) %>%
      dplyr::left_join(megaplot_jitter_and_color, by = c("event"))%>%
      dplyr::mutate(subjectid_n_jittered = subjectid_n + jitter_event_time)

    tmp_filtered <- tmp %>%
      dplyr::filter(event %in% input$select.events) %>%
      dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))

    p_1 <- tmp %>%
      plotly::plot_ly(                            #create empty plot_ly object
        source = "plotSource",
        color = ~I(event_color),
        type ="scatter",
        mode = "lines+markers"
      )

    #### segments with shapes
    # line <- list(
    #   type = "rect",
    #   fillcolor = "black",
    #   layer = "below",
    #   line = list(color = "black", width = 0.01),
    #   xref = "start_time",
    #   yref = "subjectid_n"
    # )
    #
    # lines <- list()
    # for (i in 1:nrow(tmp)) {
    #   line[["x0"]] <- tmp[i,]$start_time
    #   line[["x1"]] <- tmp[i,]$end_time
    #   line[["y0"]] <- tmp[i,]$subjectid_n - 0.1
    #   line[["y1"]] <- tmp[i,]$subjectid_n + 0.1
    #   # line[["line"]]$color <- "black"
    #   lines <- c(lines, list(line))
    # }


    p_1 <- p_1 %>% plotly::add_segments(
        y = ~subjectid_n,
        yend ~subjectid_n,
        x  = ~start_time,
        xend = ~end_time,
        line = list(color = "#2c3336", width = 4),
        showlegend = FALSE
      )

    #### Tue Aug 12 10:48:06 2025 ------------------------------
    # for (i in seq(1, nrow(tmp_filtered), by = 1)) {
    #   p_1 <- p_1 %>%
    #     add_trace(
    #       data = tmp_filtered[i,],
    #       name = tmp_filtered[i,"event"],
    #       y= as.numeric(tmp_filtered[i,"subjectid_n_jittered"]),
    #       x= as.numeric(tmp_filtered[i,"event_time"]):as.numeric(tmp_filtered[i,"event_time_end"]),
    #       line = list(width=0, color = tmp_filtered[i,"event_color"]),
    #       marker = list(width=0.8, opacity = 0.1),
    #       showlegend = FALSE
    #     )
    # }

    p_2 <- p_1


    p_2 <- p_1  %>%
    #   plotly::add_trace(
    #   data = plotly::highlight_key(tmp_filtered, ~event),
    #   type = 'scatter',
    #   mode="markers",
    #   name  = ~ event,
    #   marker = list( size = 3, symbol = "circle", symbols ="circle"),
    #   x = ~event_time_end:100,
    #   y = ~subjectid_n_jittered,
    #   showlegend = TRUE#,
    #   # legendgroup = ~event_group,
    #   # legendgrouptitle = list(text = ~event_group)
    # ) #%>%
    #   plotly::add_trace(
    #   type = 'scatter',
    #   mode="markers",
    #   name  = ~ event,
    #   marker = list(size = 4, symbol = "triangle-left", symbols = "triangle-left"),
    #   x = ~ event_time_end + 0.45,
    #   y = ~subjectid_n_jittered,
    #   showlegend = TRUE,
       #  legendgroup = ~event_group,
       # legendgrouptitle = list(text = ~event_group)
    # ) %>%
    plotly::add_segments(
      data = plotly::highlight_key(tmp_filtered, ~event),
      name = ~ event,
      x = ~event_time - 0.45,
      xend =~event_time_end + 0.45,
      y = ~subjectid_n_jittered,
      yend = ~subjectid_n_jittered,
      line = list(color = ~ event_color, width = 2),
      legendgroup = ~ event_group,
      showlegend = TRUE,
      legendgroup = ~ event_group,
      legendgrouptitle = list(text = ~ " ")
      # legendgrouptitle = list(text = ~I(event_group))
    ) %>% plotly::highlight(~ event, on = "plotly_click", off="plotly_doubleclick")
    # # p_2

    if(!is.null(grouping_vars)){
      ## groups
      label_df <- tmp %>%
        dplyr::select(subject_index, subjectid_n, group_index, sex, treatment) %>%
        group_by(group_index) %>%
        dplyr::mutate(text_position_y = max(subjectid_n) + 2) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        ungroup() %>%
        dplyr::select(subjectid_n,group_index,text_position_y)

      tmp2 <- tmp %>%
        dplyr::left_join(label_df, by = c("group_index", "subjectid_n")) %>%
        dplyr::group_by(text_position_y) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::filter(!is.na(text_position_y))

      tmp2 <- tmp2 %>% dplyr::rowwise() %>%
        dplyr::mutate(
          test1 = paste(grouping_vars, collapse = " "),
          test2 = paste(!!!rlang::syms(grouping_vars))
        ) %>%
        dplyr::mutate(test3 = paste(unlist(strsplit(test1," ")), unlist(strsplit(test2, " ")), sep = ": ", collapse = " & ")) %>%
        dplyr::mutate(event_color = "black")

      p_2 <- p_2 %>% plotly::add_trace(
        data = tmp2,
        type = "scatter",
        mode = "text",
        text = ~test3,
        textfont = list(color = "white"),
        textposition = "middle right",
        x = 1,
        y = ~text_position_y,
        showlegend = FALSE
      )

    }

    #Plotly configuration of modebar
    p_3 <- p_2 %>% plotly::config(
      scrollZoom = TRUE,                  #Enable Scroll Zoom
      displayModeBar = TRUE,              #Forcing the modebar always to be visible
      displaylogo = FALSE,                #Hiding the plotly logo on the modebar
      modeBarButtonsToRemove =            #Remove not needed buttons from modebar
        c("hoverClosestCartesian","hoverCompareCartesian","zoomIn2d","zoomOut2d","select2d","lasso2d")
    )

    p_4 <- p_3 %>%
      plotly::layout(
        plot_bgcolor = "#404A4E",
        paper_bgcolor ='#404A4E',
        # legend = list(orientation = "h"),
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
        barmode = "overlay"
      )

    # layout(p_4,shapes = lines)

  })


  output$event_summary <- plotly::renderPlotly({
    megaplot_color <- c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
      "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
      "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
      "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
      "#fabed4", "#4263d8"
    )


    megaplot_data_total <- shiny::req(uploaded_data$val)
    # megaplot_data_total <- megaplot_data_total %>%
    #   dplyr::filter(event %in% input$select.events)
    megaplot_data_total <<- megaplot_data_total
    select.events <<- input$select.events

    test <- split(megaplot_data_total, megaplot_data_total$event_group)
    for(i in 1:length(test)) {
      test[[i]] <- test[[i]] %>% dplyr::group_by(event) %>% dplyr::mutate(
        event_id = dplyr::cur_group_id(),
        event_group_id = i
      )
      test[[i]] <- test[[i]] %>% dplyr::mutate(
        max_event_id = max(test[[i]]$event_id)
      )
    }

    color_func <- function(x,y,z) {
      colorRampPalette(
        c(colorRampPalette(c("white",megaplot_color[y]))(100)[85],
          colorRampPalette(c(megaplot_color[y],"black"))(100)[15]
        )) (z)[x]
    }

    test2 <- do.call("rbind",test)
    test3 <- test2 %>% dplyr::select(event,event_group,max_event_id,event_group_id, event_id) %>% distinct() %>% ungroup() %>%
      dplyr::mutate( jitter_event_time = seq(-0.1,0.1,length = nrow(.))[dplyr::row_number()]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        event_color = color_func(event_id,event_group_id,max_event_id)
      ) %>%
      dplyr::select(
        event,event_color,jitter_event_time
      )

    megaplot_jitter_and_color <- test3
    # megaplot_jitter_and_color <- megaplot_data_total %>%
    #   dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))%>%
    #   dplyr::select(event,event_group) %>%
    #   distinct() %>%
    #   ungroup() %>%
    #   mutate(
    #     jitter_event_time = seq(-0.1,0.1,length = nrow(.))[dplyr::row_number()],
    #     event_color = megaplot_color[dplyr::row_number()]
    #   )
    #### ####

    grouping_vars <- input$select.grouping

    tmp <- megaplot_data_total
    tmp <- arrange(tmp, !!!rlang::syms(grouping_vars))
    levels(tmp$subjectid) <- unique(arrange(tmp, !!!rlang::syms(grouping_vars))$subjectid)
    tmp <- tmp %>% group_by(!!!rlang::syms(grouping_vars),subjectid)%>% dplyr::mutate(subject_index = dplyr::cur_group_id())%>% ungroup()
    tmp <- tmp %>% group_by(!!!rlang::syms(grouping_vars))%>% dplyr::mutate(group_index = dplyr::cur_group_id())%>% ungroup()
    tmp <- tmp  %>%
      dplyr::mutate(subjectid_n = subject_index + (group_index-1)*10) %>%
      dplyr::left_join(megaplot_jitter_and_color, by = c("event"))%>%
      dplyr::mutate(subjectid_n_jittered = subjectid_n + jitter_event_time)

    tmp_filtered <- tmp %>%
      dplyr::filter(event %in% input$select.events) %>%
      dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))

    tmp_summarised <- tmp_filtered %>% dplyr::group_by(event) %>% dplyr::summarise(number_events = n()) %>% left_join(tmp_filtered%>% dplyr::select(event, event_color) %>% distinct(), by ="event")


    p_1 <- plotly::plot_ly(data = tmp_summarised,name = ~event,  x = ~event, y = ~number_events, type = 'bar', color = ~I(event_color))

    p_1 %>%
      plotly::layout(
        plot_bgcolor = "#404A4E",
        paper_bgcolor ='#404A4E',
        yaxis = list(
          color='#FFFFFF',
          showgrid = TRUE,
          title ="Number events",
          zeroline = FALSE,
          showticklabels = TRUE
        ),
        font = list(family = "Agency FB", color = "#FFFFFF"),
        barmode = "overlay"
      )
  })
}
