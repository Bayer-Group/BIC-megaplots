#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  #set global variable . to NULL to
  #avoid note: no visible binding for global variable '.' when
  # performing package check (via devtools)
  . <- NULL

  waiter::waiter_hide()

  #### MODULE CALLS ####
  # Call the module, passing the parent session so the module can reach
  # outside its namespace to update select_grouping and call nav_select()
  upload_data <- mod_data_upload_server(
    id = "data_upload",
    parent_session = session,
    theme = shiny::reactive(input$theme_toggle)
  )
  uploaded_data_renamed <- upload_data$uploaded_data_renamed
  uploaded_data_w_ids   <- upload_data$uploaded_data_w_ids

  # Call color selection module
  color_selection <- mod_color_selection_server(
    id                    = "color_selection",
    uploaded_data_renamed = uploaded_data_renamed,
    uploaded_data_w_ids   = uploaded_data_w_ids,
    theme                 = shiny::reactive(input$theme_toggle),
    js_col_num            = shiny::reactive(input$jsColNum),
    parent_session        = session
  )

  color_data   <- color_selection$color_data
  checked_data <- color_selection$checked_data

  #### update sorting ####
  shiny::observeEvent(uploaded_data_renamed(), {
    choices <- names(which(unlist(lapply(uploaded_data_renamed() %>%
                                           dplyr::relocate(tidyr::starts_with("megaplots_")), is.numeric))))
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select_sorting",
      choices = choices,
      selected = ifelse("megaplots_selected_end_time" %in% choices,"megaplots_selected_end_time" ,"megaplots_selected_subjectid"),
      choicesOpt = list(style =  rep_len(
        "font-size: 60%; line-height: 1.6;", length(choices)
      )
      )
    )
  })

  #### reactive object megaplot_prepared_data ####
  megaplot_prepared_data <- shiny::eventReactive(
    c(uploaded_data_w_ids(),
      filtered_data_reactive$val,
      #input$select_grouping,
      input$arrange_groups,
      input$select_sorting), {

    prepare_megaplot_data(
      #megaplot_data_raw = uploaded_data$val,
      megaplot_data_raw = shiny::req(filtered_data_reactive$val),
      uploaded_data_w_ids = uploaded_data_w_ids(),
      select_sorting = input$select_sorting,
      select_grouping = input$select_grouping,
      arrange_groups = input$arrange_groups
    )
  })

  #### START FILTER PART ####

  #### reactive object megaplot_filtered_data ####
  # updates when: shinyTree selection changes
  #               jittering option changes
  #               color option changes
  megaplot_filtered_data <- shiny::reactive({

    filtered_data <- filter_megaplot_data(
      tree = shiny::req(checked_data$val),
      megaplot_prepared_data = shiny::req(megaplot_prepared_data()),
      color_data = shiny::req(color_data$all)
    )

    if (!is.null(filtered_data)) {
      #remove nas from required variables
      filtered_data <- filtered_data %>%
        dplyr::filter(!is.na(.data$megaplots_selected_subjectid)) %>%
        dplyr::filter(!is.na(.data$megaplots_selected_event))
    }
    #return
    filtered_data
  })

  shiny::observeEvent(uploaded_data_renamed(), {
    shinyWidgets::updatePickerInput(
      inputId ="select_filter_variables",
      choices = colnames(uploaded_data_renamed()),
      selected = NULL
    )
  })

  variables_for_filter <- shiny::reactive({input$select_filter_variables})

  output$filter_enabled <- shiny::reactive({
    !is.null(input$select_filter_variables)
  })

  outputOptions(output, "filter_enabled", suspendWhenHidden = FALSE)

  res_filter <- datamods::filter_data_server(
    id = "filtering",
    data = uploaded_data_renamed,
    vars = variables_for_filter,
    defaults = defaults,
    drop_ids = FALSE,
    widget_num = "slider",
    widget_char = "picker",
    label_na = "NA",
    value_na = TRUE
  )

  savedFilterValues <- reactiveVal()

  shiny::observeEvent(input$select_filter_variables, {
    savedFilterValues <<- res_filter$values()
  },ignoreInit = TRUE)

  defaults <- shiny::reactive({
    #input$load_filter_values
    input$select_filter_variables
    savedFilterValues
  })

  filtered_data_reactive <- reactiveValues(val = NULL)

  shiny::observeEvent(res_filter$filtered(), {
    if (!is.null(res_filter$filtered())) {
      if (nrow(res_filter$filtered()) != 0) {
        filtered_data_reactive$val <- res_filter$filtered()
      }
    }
  })

  shiny::observeEvent(filtered_data_reactive$val,{
    shinyWidgets::updateProgressBar(
      session = session,
      id = "pbar",
      value = length(unique(filtered_data_reactive$val$megaplots_selected_subjectid)),
      total = length(unique(uploaded_data_renamed()$megaplots_selected_subjectid))
    )
  })

  #### END FILTER PART ####


  #### START ARRANGE GROUPS PART ####
  output$arrange_groups <- shiny::renderUI({
    shinyjqui::orderInput(
      inputId = "arrange_groups",
      label = "Arrange Groups",
      items = NULL,
      width = 300
    )
  })
  shiny::observeEvent(c(uploaded_data_renamed(), input$select_grouping), {
    if(!is.null(input$select_grouping)) {
      grouping_label_data <- uploaded_data_renamed() %>%
        dplyr::select(!!!rlang::syms(input$select_grouping)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          text_snippet_1 = paste(input$select_grouping, collapse = " "),
          text_snippet_2 = paste(!!!rlang::syms(input$select_grouping), sep = ", ")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(text_snippet_total = paste(unlist(strsplit(.data$text_snippet_1," ")), gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))), sep = ": ", collapse = " & "))

      shinyjqui::updateOrderInput(
        session,
        inputId = "arrange_groups",
        items = grouping_label_data$text_snippet_total
      )
    } else {
      output$arrange_groups <- shiny::renderUI({
        shinyjqui::orderInput(
          inputId = "arrange_groups",
          label = "Arrange Groups",
          items = NULL,
          width = 300
        )
      })
    }
  })
  #### END ARRANGE GROUPS PART ####

  #### START REFERENCE LINES PART ####
  reference_lines_reactive <- shiny::reactiveValues(
    reference_line_1 = FALSE,
    reference_line_2 = FALSE,
    reference_line_3 = FALSE,
    reference_line_1_value = NULL,
    reference_line_2_value = NULL,
    reference_line_3_value = NULL,
    reference_line_1_value2 = NULL,
    reference_line_2_value2 = NULL,
    reference_line_3_value2 = NULL,
    reference_line_1_color = NULL,
    reference_line_2_color = NULL,
    reference_line_3_color = NULL
  )

  shiny::observeEvent(input$reference_line_1, {
    if (!input$reference_line_1) {
      reference_lines_reactive$reference_line_1 <- FALSE
    }
  })

  shiny::observeEvent(input$update_reference_lines, {
    reference_lines_reactive$reference_line_1 <- shiny::isolate(input$reference_line_1)
    reference_lines_reactive$reference_line_2 <- shiny::isolate(input$reference_line_2)
    reference_lines_reactive$reference_line_3 <- shiny::isolate(input$reference_line_3)
    reference_lines_reactive$reference_line_1_value <- shiny::isolate(input$reference_line_1_value)
    reference_lines_reactive$reference_line_2_value <- shiny::isolate(input$reference_line_2_value)
    reference_lines_reactive$reference_line_3_value <- shiny::isolate(input$reference_line_3_value)
    reference_lines_reactive$reference_line_1_value2 <- shiny::isolate(input$reference_line_1_value2)
    reference_lines_reactive$reference_line_2_value2 <- shiny::isolate(input$reference_line_2_value2)
    reference_lines_reactive$reference_line_3_value2 <- shiny::isolate(input$reference_line_3_value2)
    reference_lines_reactive$reference_line_1_color <- shiny::isolate(input$reference_line_1_color)
    reference_lines_reactive$reference_line_2_color <- shiny::isolate(input$reference_line_2_color)
    reference_lines_reactive$reference_line_3_color <- shiny::isolate(input$reference_line_3_color)
  })
  #### END REFERENCE LINES PART ####

  #### START MEGA PLOTS PART ####
  output$mega_plots <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())

    tmp <- draw_mega_plot(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = shiny::isolate(input$select_grouping),
      line_width = input$line_width,
      line_width_subjects = input$line_width_subjects,
      switch_legend_grouping = input$switch_legend_grouping,
      sort_event_groups = input$sort_event_groups,
      sequencing_object = shiny::isolate(sequencing_object$sequencing_object()),
      sequencing_switch = sequencing_object$sequencing_switch(),
      reference_line_1 = reference_lines_reactive$reference_line_1,
      reference_line_2 = reference_lines_reactive$reference_line_2,
      reference_line_3 = reference_lines_reactive$reference_line_3,
      reference_line_1_value = reference_lines_reactive$reference_line_1_value,
      reference_line_2_value = reference_lines_reactive$reference_line_2_value,
      reference_line_3_value = reference_lines_reactive$reference_line_3_value,
      reference_line_1_value2 = reference_lines_reactive$reference_line_1_value2,
      reference_line_2_value2 = reference_lines_reactive$reference_line_2_value2,
      reference_line_3_value2 = reference_lines_reactive$reference_line_3_value2,
      reference_line_1_color = reference_lines_reactive$reference_line_1_color,
      reference_line_2_color = reference_lines_reactive$reference_line_2_color,
      reference_line_3_color = reference_lines_reactive$reference_line_3_color,
      theme = input$theme_toggle
      # circular_vision = input$circular_vision
    )
    session_store$val <- tmp
    tmp
  })

  #### END MEGA PLOTS PART ####

  #### START SAVE HTML PART ####
  #create reactive variable for saving html output
  session_store <- shiny::reactiveValues(val = NULL)

  output$download_plotly_widget <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      htmlwidgets::saveWidget(plotly::as_widget(session_store$val), file, selfcontained = TRUE)
    }
  )
  #### END SAVE HTML PART ####

  #### START EVENT SUMMARY PART ####
  output$event_summary <- plotly::renderPlotly({
    shiny::req(megaplot_prepared_data())
    shiny::req(megaplot_filtered_data())
    draw_event_summary(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = input$select_grouping,
      event_summary_cutoff = input$event_summary_cutoff,
      event_summary_selection = input$event_summary_selection,
      switch_legend_grouping = input$switch_legend_grouping,
      hovermode = input$event_summary_hovermode,
      reference_line_1 = reference_lines_reactive$reference_line_1,
      reference_line_2 = reference_lines_reactive$reference_line_2,
      reference_line_3 = reference_lines_reactive$reference_line_3,
      reference_line_1_value = reference_lines_reactive$reference_line_1_value,
      reference_line_2_value = reference_lines_reactive$reference_line_2_value,
      reference_line_3_value = reference_lines_reactive$reference_line_3_value,
      reference_line_1_value2 = reference_lines_reactive$reference_line_1_value2,
      reference_line_2_value2 = reference_lines_reactive$reference_line_2_value2,
      reference_line_3_value2 = reference_lines_reactive$reference_line_3_value2,
      reference_line_1_color = reference_lines_reactive$reference_line_1_color,
      reference_line_2_color = reference_lines_reactive$reference_line_2_color,
      reference_line_3_color = reference_lines_reactive$reference_line_3_color,
      theme = input$theme_toggle

    )
  })
  #### END EVENT SUMMARY PART ####

  #### START SEQUENCING PART ####
  shiny::observe({
    sequencing_object$sequencing_object()
  })

  sequencing_object <- callModule(sequencing_server, "sequencing_module", megaplot_filtered_data = shiny::reactive({megaplot_filtered_data()}))
  #### END SEQUENCING PART ####
}
