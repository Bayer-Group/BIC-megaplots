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
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select_strata_var",
      choices = colnames(megaplot_data)[sapply(megaplot_data,class) %in% c("factor","character")],
      selected = NULL
    )
  })

  #update kaplan meier event selection based on selected events
  shiny::observeEvent(input$tree, {
    shiny::req(input$tree)


    shinyWidgets::updatePickerInput(
      inputId = 'select_event_kaplan_meier',
      choices = unlist(shinyTree::get_selected(input$tree, format=c("classid"))),
      selected = NULL,
    )
  })


  # observe({
  #   #colnames(mp_B) <- gsub("[[:punct:][:space:]]+", "_", colnames(mp_B))
  #   print( gsub("[[:punct:][:space:]]+", "_", names(as.data.frame(shinyTree::get_selected(input$tree, format="slices")))))
  # })

  # color vector with different color and shades
  # replace this color vector when upload page is finished
  megaplot_color <- c(
    "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
    "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
    "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
    "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
    "#fabed4", "#4263d8"
  )


  ##### megaplot_prepared_data ####
  megaplot_prepared_data <- shiny::reactive({

    megaplot_data_raw <- shiny::req(uploaded_data$val)

    prepare_megaplot_data(
      megaplot_data = shiny::req(uploaded_data$val),
      grouping_vars = input$select.grouping,
      sorting_var = input$select.sorting
    )
  })

  #### megaplot_filtered_data ####
  megaplot_filtered_data <- shiny::reactive({

    shiny::req(input$tree)


    selected.events <- unlist(shinyTree::get_selected(input$tree, format=c("classid")))

    prepared_data <- megaplot_prepared_data()

    filtered_data <- prepared_data  %>%
      dplyr::filter(event %in% selected.events) %>%
      dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))

    filtered_data <- filtered_data %>%
      dplyr::distinct()

    filtered_data
  })


  #### Data Upload / Event selection ####
  output$tree <- shinyTree::renderTree({

    megaplot_data_raw <- shiny::req(uploaded_data$val)

    reduced_event_data <- megaplot_data_raw %>%
      dplyr::filter(!is.na(event)) %>%
      dplyr::select(event_group,event) %>%
      dplyr::distinct()

    unique_event_groups <- reduced_event_data %>%
      dplyr::pull(event_group) %>%
      unique()


    event_list <- vector(mode = 'list', length(unique_event_groups))

    for(i in 1:length(unique_event_groups)) {
      events <- reduced_event_data %>%
        dplyr::filter(event_group == unique_event_groups[i]) %>%
        dplyr::pull(event)

      tmp_list <- vector(mode ='list', length = length(events))

      names(tmp_list) <- events

      for (j in 1:length(events)) {
        tmp_list[[j]] <-structure(j)
      }
      event_list[[i]] <-  structure(tmp_list, stopened = FALSE)
    }
    names(event_list) <- unique_event_groups

    event_list <- structure(event_list)
    event_list <- list( "Select all event (groups)" = event_list)
    attr(event_list[[1]], "stopened") <- TRUE

    event_list
  })

  #### mega_plots ####
  output$mega_plots <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())
    #shiny::req(megaplot_filtered_data())
    draw_mega_plot(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select.grouping = input$select.grouping
    )
  })

  #### event_summary ####
  output$event_summary <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())
    shiny::req(megaplot_filtered_data())
    draw_event_summary(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select.grouping = input$select.grouping
    )

  })

  #### kaplan meier ####
  output$kaplan_meier <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())
    shiny::req(megaplot_filtered_data())

    draw_kaplan_meier(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select.grouping = input$select.grouping,
      select_event_kaplan_meier = input$select_event_kaplan_meier,
      select_strata_var = input$select_strata_var
    )
  })
}
