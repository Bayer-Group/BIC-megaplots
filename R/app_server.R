#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Initialize reactive value for uploaded data
  uploaded_data <- shiny::reactiveValues(val = NULL)

  # Update widgets and reactive data object when fileinput is observed
  shiny::observeEvent(input$file, {
    shiny::req(input$file) #requires input$file

    #add tests for data set
    megaplot_data <- base::get(load(
      file = input$file$datapath
    ))

    uploaded_data$val <- megaplot_data  #update reactive value 'uploaded_data'

    # update choices of grouping variable based on uploaded data
    # includes all factor and character variables
    updateSelectizeInput(
      session,
      inputId = "select.grouping",
      choices = colnames(megaplot_data)[sapply(megaplot_data,class) %in% c("factor","character")],
      selected = NULL
    )

    # update choices of events based on uploaded data
    #
    shinyWidgets::updatePickerInput(
      inputId = 'select.events',
      choices = unique(megaplot_data$event),
      selected = NULL,
    )
  })

  #update kaplan meier event selection based on selected events
  shiny::observeEvent(input$select.events, {
    shiny::req(input$select.events)
    shinyWidgets::updatePickerInput(
      inputId = 'select_event_kaplan_meier',
      choices = unique(input$select.events),
      selected = NULL,
    )
  })


  # color vector with different color and shades
  # replace this color vector when upload page is finished
  megaplot_color <- c(
    "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
    "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
    "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
    "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
    "#fabed4", "#4263d8"
  )



  megaplot_prepared_data <- shiny::reactive({

    megaplot_data_raw <- shiny::req(uploaded_data$val)

    prepare_megaplot_data(
      megaplot_data = shiny::req(uploaded_data$val),
      grouping_vars = input$select.grouping,
      sorting_var = input$select.sorting
    )
  })

  megaplot_filtered_data <- shiny::reactive({

    shiny::req(input$select.events)

    prepared_data <- megaplot_prepared_data()

    filtered_data <- prepared_data  %>%
      dplyr::filter(event %in% input$select.events) %>%
      dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))

    filtered_data
  })


  output$mega_plots <- plotly::renderPlotly({

    megaplot_prepared_data <- megaplot_prepared_data()
    megaplot_filtered_data <- megaplot_filtered_data()

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
        xend = ~end_time,
        line = list(color = "#2c3336", width = 1),
        showlegend = FALSE
      )

    p_2 <- p_1  %>%
      plotly::add_segments(
        data = plotly::highlight_key(megaplot_filtered_data, ~event),
        name = ~ event,
        x = ~event_time - 0.45,
        xend =~event_time_end + 0.45,
        y = ~subjectid_n_jittered,
        yend = ~subjectid_n_jittered,
        line = list(color = ~ event_color, width = 3),
        legendgroup = ~ event_group,
        showlegend = TRUE,
        legendgroup = ~ event_group,
        legendgrouptitle = list(text = ~ " ")
      ) %>%
      plotly::highlight(~ event, on = "plotly_click", off="plotly_doubleclick")

    if (!is.null(input$select.grouping)) {
      label_df <- megaplot_prepared_data %>%
        dplyr::select(subject_index, subjectid_n, group_index, sex, treatment) %>%
        group_by(group_index) %>%
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
          text_snippet_1 = paste(input$select.grouping, collapse = " "),
          text_snippet_2 = paste(!!!rlang::syms(input$select.grouping))
        ) %>%
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(text_snippet_2," ")), unlist(strsplit(text_snippet_2, " ")), sep = ": ", collapse = " & ")) %>%
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


    megaplot_data_total <<- shiny::req(uploaded_data$val)
    # megaplot_data_total <- megaplot_data_total %>%
    #   dplyr::filter(event %in% input$select.events)

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

    tmp_new <<- tmp
    tmp_fil_new <<- tmp_filtered

    tmp_test_list <- split(tmp_filtered,tmp_filtered$event)

    time_vector <-  min(c(tmp_filtered$start_time,tmp_filtered$event_time)) : max(c(tmp_filtered$end_time,tmp_filtered$event_time_end))

    df <- data.frame(day = time_vector)
    for(i in 1:length(tmp_test_list)){

      numbers <- rep(0,length(time_vector))

      for(j in 1:nrow(tmp_test_list[[i]])) {
        tmp_test_list[[i]][j,]$event_time : tmp_test_list[[i]][j,]$event_time_end
        numbers[which(time_vector %in% tmp_test_list[[i]][j,]$event_time : tmp_test_list[[i]][j,]$event_time_end)] <- numbers[which(time_vector %in% tmp_test_list[[i]][j,]$event_time : tmp_test_list[[i]][j,]$event_time_end)]+1
      }
      name <- (paste0(names(tmp_test_list)[i]))
      df <- cbind(df,  name = numbers)
      names(df)[which(names(df) == "name")] <- name

    }

    fig <- plot_ly(data = df, x = ~day)

    for(i in 2:ncol(df)) {
      fig <- fig %>%
        plotly::add_lines(y = df[,i], color = I(tmp%>%dplyr::filter(event == names(df)[i]) %>% dplyr::pull(event_color)%>% unique()),line = list(shape = "linear"), name = names(df)[i])
    }
    fig <- fig %>%
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
          title ="Event frequency",
          zeroline = FALSE,
          autotick = FALSE,
          showticklabels = FALSE
        ),
        font = list(family = "Agency FB", color = "#FFFFFF"),
        barmode = "overlay"
      )
  })

  output$kaplan_meier <- plotly::renderPlotly({
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

    tmp_filtered
    level <- input$select_event_kaplan_meier
    tmp_tte <- tmp_filtered %>%
      dplyr::select(
        subjectid,subjectid_n,
        event_time,
        event #event
      ) %>%
      dplyr::filter(event == level) %>%
      dplyr::group_by(subjectid) %>%
      dplyr::slice_head(n = 1)%>%
      dplyr::mutate(!!paste0("time_to_first") := event_time)%>%
      dplyr::select(subjectid, !!paste0("time_to_first"))

    tmp_tte2 <- tmp_tte %>% dplyr::right_join(tmp%>% dplyr::select(subjectid,subjectid_n,end_time,sex,treatment) %>% distinct(), by ="subjectid")

    tmp_tte3 <- tmp_tte2 %>%
      dplyr::mutate(
        time = dplyr::case_when(is.na(time_to_first) ~ end_time,
                                !is.na(time_to_first) ~ time_to_first),
        status = dplyr::case_when(is.na(time_to_first) ~ 1,
                                  !is.na(time_to_first) ~ 2)
      )

    event_color <- tmp %>% dplyr::filter(event == level) %>% dplyr::pull(event_color) %>% as.character %>% unique()

    fit  <-survfit(Surv(time,status) ~treatment, data = tmp_tte3)

    # g <- ggplot2::autoplot(fit)
    g <- ggsurv(fit, CI = FALSE,cens.col = c("#FFFFFF50"),surv.col = c(event_color,event_color)) + theme_dark() + theme(axis.title = element_text(
      color= "white"))
    ggplotly(g) %>% plotly::layout(
      plot_bgcolor = "#404A4E",
      paper_bgcolor ='#404A4E',
      legend = list(
        bgcolor = "#404A4E",
        bordercolor = "black",
        font = list(color="white")
      ),
      font = list(family = "Agency FB", color = "#FFFFFF"),
      yaxis = list(color = "white")
    )
  })
}
