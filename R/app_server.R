#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  waiter::waiter_hide()

  #### 1. Data Upload Module ####
  upload_data <- mod_data_upload_server(
    id = "data_upload",
    parent_session = session,
    theme = shiny::reactive(input$theme_toggle)
  )
  uploaded_data_renamed <- upload_data$uploaded_data_renamed
  uploaded_data_w_ids   <- upload_data$uploaded_data_w_ids

  #### 2. Color selection module ####
  color_selection <- mod_color_selection_server(
    id = "color_selection",
    uploaded_data_renamed = uploaded_data_renamed,
    uploaded_data_w_ids = uploaded_data_w_ids,
    theme = shiny::reactive(input$theme_toggle),
    js_col_num = shiny::reactive(input$jsColNum),
    parent_session = session
  )

  color_data   <- color_selection$color_data
  checked_data <- color_selection$checked_data

  #### 3. Filter Module ####
  filter_result <- mod_filter_server(
    id = "filter",
    uploaded_data_renamed = uploaded_data_renamed,
    parent_session = session
  )

  filtered_data_reactive <- shiny::reactiveValues(val = NULL)
  # Keep filtered_data_reactive in sync with the module output
  shiny::observe({
    filtered_data_reactive$val <- filter_result$filtered_data()
  })

  #### 4. Reference Lines Module ####
  ref <- mod_reference_lines_server("reference_lines")

  #### 5. Plot Appearance Module ####
  plot_appearance <- mod_plot_appearance_server("plot_appearance")

  #### 6. Sorting & Grouping Module ####
  sorting_grouping <- mod_sorting_grouping_server(
    id = "sorting_grouping",
    uploaded_data_renamed = uploaded_data_renamed
  )

  #### 7. reactive object megaplot_prepared_data ####
  megaplot_prepared_data <- shiny::eventReactive(
    c(uploaded_data_w_ids(),
      filter_result$filtered_data(),
      sorting_grouping$arrange_groups(),
      sorting_grouping$select_sorting()), {

      prepare_megaplot_data(
        megaplot_data_raw = shiny::req(filter_result$filtered_data()),
        uploaded_data_w_ids = uploaded_data_w_ids(),
        select_sorting = sorting_grouping$select_sorting(),
        select_grouping = sorting_grouping$select_grouping(),
        arrange_groups = sorting_grouping$arrange_groups()
      )
  })


  #### 8. reactive object megaplot_filtered_data ####
  megaplot_filtered_data <- shiny::reactive({
    filtered_data <- filter_megaplot_data(
      tree = shiny::req(checked_data$val),
      megaplot_prepared_data = shiny::req(megaplot_prepared_data()),
      color_data = shiny::req(color_data$all)
    )
    if (!is.null(filtered_data)) {
      #remove NAs from required variables
      filtered_data <- filtered_data |>
        dplyr::filter(!is.na(.data$megaplots_selected_subjectid)) |>
        dplyr::filter(!is.na(.data$megaplots_selected_event))
    }
    #return
    filtered_data
  })


  #### 9. Sequencing Module####
  sequencing_object <- callModule(
    sequencing_server,
    "sequencing_module",
    megaplot_filtered_data = shiny::reactive({megaplot_filtered_data()})
  )

  shiny::observe({sequencing_object$sequencing_object()})

  #### 10. Megaplot Module ####
  megaplot_result <- mod_megaplot_server(
    id = "megaplot",
    megaplot_prepared_data = megaplot_prepared_data,
    megaplot_filtered_data = megaplot_filtered_data,
    reference_lines = ref$reference_lines,
    theme = shiny::reactive(input$theme_toggle),
    select_grouping = sorting_grouping$select_grouping,
    appearance = shiny::reactive(
      list(
        line_width = plot_appearance$line_width(),
        line_width_subjects = plot_appearance$line_width_subjects(),
        switch_legend_grouping = plot_appearance$switch_legend_grouping(),
        sort_event_groups = plot_appearance$sort_event_groups(),

        event_summary_cutoff = plot_appearance$event_summary_cutoff(),
        event_summary_hovermode = plot_appearance$event_summary_hovermode(),
        sequencing_object = sequencing_object$sequencing_object(),
        sequencing_switch = sequencing_object$sequencing_switch(),
        line_color_subjects_dark = plot_appearance$line_color_subjects_dark(),
        line_color_subjects_light = plot_appearance$line_color_subjects_light(),
        circular_vision = sequencing_object$circular_vision()
      )
    )
  )

  ####11. download handler ####
  output$download_plotly_widget <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      htmlwidgets::saveWidget(plotly::as_widget(megaplot_result$plot_object()), file, selfcontained = TRUE)
    }
  )
}
