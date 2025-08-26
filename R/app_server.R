#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  #### Upload page ####
  # shinyjs code to hide/show panels in the beginning
  shinyjs::hideElement(id= "selected_events_panel")
  shinyjs::hideElement(id= "selected_events_color_container_panel")
  shinyjs::hideElement(id= "colour_picker_panel")

  shiny::observeEvent(uploaded_data$val, {
    if(is.null(uploaded_data$val)) {
      shinyjs::hideElement(id = "selected_events_panel")
    } else {
      shinyjs::showElement(id = "selected_events_panel")
    }
  })

  #Changing the maximum height of the upload panels depending on screen size
  shiny::observe({
    runjs(sprintf(paste0("
            document.getElementById('selected_events_panel').style.maxHeight = '",input$dimension-400,"px';")))
      runjs(sprintf(paste0("
            document.getElementById('selected-cols-row').style.maxHeight = '",input$dimension-400,"px';")))
  })

  # renderTree for event/event_group selection
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
        tmp_list[[j]] <-structure(j, sticon = "")
      }
      event_list[[i]] <-  structure(tmp_list, stopened = FALSE, sticon="")
    }
    names(event_list) <- unique_event_groups

    event_list <- structure(event_list)
    event_list <- list( "Select all event(s)" = structure(event_list, sticon = ""))
    attr(event_list[[1]], "stopened") <- TRUE

    event_list
  })

  shiny::observeEvent(input$tree, {
    shinyTree::get_selected(input$tree,format="names")
    if(is.null(input$tree)) {
      shinyjs::hideElement(id = "selected_events_color_container_panel")
      shinyjs::hideElement(id = "colour_picker_panel")
    } else {
      if (length(shinyTree::get_selected(input$tree,format="names")) > 0) {
        shinyjs::showElement(id = "selected_events_color_container_panel")
        shinyjs::showElement(id = "colour_picker_panel")
      } else {
        shinyjs::hideElement(id = "selected_events_color_container_panel")
        shinyjs::hideElement(id = "colour_picker_panel")
      }
    }
  })

  shiny::observe({
    colourpicker::updateColourInput(
      session,
      inputId="picked_colour",
      label = ifelse(is.na(color_data$selected[input$jsColNum,"event"]),color_data$selected[input$jsColNum,"event_group"],color_data$selected[input$jsColNum,"event"]),
      value = color_data$selected[input$jsColNum,"event_color"]
    )
  })


  shiny::observeEvent(input$picked_colour, {
    #require the selected number of colored div container list
    shiny::req(input$jsColNum)

    if (!is.null(color_data$all)) {

      if(!is.na(color_data$selected[input$jsColNum, c("event")])) {
        color_data$all[
          color_data$all$event_group == color_data$selected[input$jsColNum,c("event_group")] & color_data$all$event == color_data$selected[input$jsColNum,c("event")] & !is.na(color_data$all$event == color_data$selected[input$jsColNum,c("event")]),]$event_color <- input$picked_colour
      }
      if (is.na(color_data$selected[input$jsColNum,c("event")])) {

        color_func2 <- function(x,y,z, col = input$picked_colour) {
          if (x != 0 & z != 1) {
            return_colors <- colorRampPalette(
              c(colorRampPalette(c("white",col))(100)[50],
                col,
                colorRampPalette(c(col,"black"))(100)[50]
              )
            ) (z)[x]
          }
          if(x == 0 | z == 1) {
            return_colors <- col
          }
          return(return_colors)
        }

        new_event_group_color <- color_data$all[color_data$all$event_group == color_data$selected[input$jsColNum,c("event_group")],] %>%
          dplyr::mutate(
            event_color = color_func2(event_id, event_group_id, max_event_id)
          ) %>% dplyr::pull(event_color)

        color_data$all[color_data$all$event_group == color_data$selected[input$jsColNum,c("event_group")],]$event_color <- new_event_group_color
      }
    }
  })

  #initialize reactive value color_data with entries "all" and "selected"
  # all includes all events from uploaded data set and selected only values
  # selected in shinyTree input

  color_data <- reactiveValues(
    all = NULL,
    selected = NULL
  )

  # The purpose of this observer is to create/initialize a color vector for every event/event_group
  # available in the data. By default event_groups receive an own color and events within the groups
  # get color shades of group color. Color for group and single events can be changed within app.
  shiny::observe({
    #require reactive object with uploaded data
    megaplot_data_raw <- shiny::req(uploaded_data$val)

    #create a data frame with unique combinations of event_group and event
    megaplot_data_raw <- megaplot_data_raw %>%
      dplyr::select(event_group, event) %>%
      dplyr::distinct()

    #Split unique combinations by event group
    megaplot_data_splitted_by_event_group <- split(megaplot_data_raw, megaplot_data_raw$event_group)

    # Create and save an identifier variable ("event_id" & "event_group_id") for every event and event group and the number of events within
    # a group ("max_event_id"). These variables will be used for the color function to create a unique color for every event
    for(i in 1:length(megaplot_data_splitted_by_event_group)) {
      # create "event_id" & "event_group_id"
      megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
        dplyr::group_by(event) %>%
        dplyr::mutate(
        event_id = dplyr::cur_group_id(),
        event_group_id = i
      )
      #create "max_event_id"
      megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
        dplyr::mutate(
          max_event_id = max(megaplot_data_splitted_by_event_group[[i]]$event_id)
        )

      # Add row for event_group with event_id = 0. This will only be used to colorize all
      # events within a group with specific color (shades).
      megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
        dplyr::ungroup() %>%
        dplyr::add_row(
          event_group = unique(megaplot_data_splitted_by_event_group[[i]]$event_group),
          event = NA,
          event_id = 0,
          event_group_id = unique(megaplot_data_splitted_by_event_group[[i]]$event_group_id),
          max_event_id = unique(megaplot_data_splitted_by_event_group[[i]]$max_event_id),
        )
    }

    #bind all list entries rowwise to one data.frame
    megaplot_data_w_event_ids <- do.call("rbind",megaplot_data_splitted_by_event_group)

    #add variables event_color
    megaplot_event_data_w_color <- megaplot_data_w_event_ids  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        event_color = color_func(event_id, event_group_id, max_event_id)
      )

    # add created color vector to reactive object color_data$all
    color_data$all <- megaplot_event_data_w_color
  })



  output$selected_events_color_container <- renderUI({
    shiny::req(input$tree)

    # receive names of selected shinyTree input
    selected_tree <- shinyTree::get_selected(input$tree, format = "names")

    if (length(selected_tree) > 0) {
      #
      selected_data <- do.call(
        "rbind",
        # perform apply function for every list entrie of shinyTree list
        # to transform list entries into desired form of event/event group
        lapply(selected_tree, function(selected_tree_row) {
           # use attributes to decide if a selected row is an event or event_group
           # if length of attributes 'ancestry'
           if(length(attributes(selected_tree_row)$ancestry) == 2){
             data.frame("event_group" = attributes(selected_tree_row)$ancestry[2], "event" = selected_tree_row[1])
           } else {
             data.frame("event_group" = selected_tree_row[1], "event" = NA)
           }
          })
        )

      # Every shinyTree list element gets an id which can be received via attributes(x)$id.
      # This id will be merged to the data and used for sorting
      sort_id <- as.numeric(unlist(lapply(shinyTree::get_selected(input$tree, format="classid"), function(x){attributes(x)$id})))
      selected_data <- cbind(selected_data, sort_id) %>%
        dplyr::arrange(sort_id)

      # join color vector to the data and create variables "names_for_color_list" & "type_for_color" which will
      # be used to colorize the div container and make sure that also event_groups are displayed
      selected_data <- selected_data %>%
        dplyr::left_join(
          color_data$all %>%
            dplyr::select(event_group, event, event_color), by = dplyr::join_by(event_group,event)
        ) %>%
        dplyr::mutate(
          names_for_color_list = ifelse(is.na(event), event_group, event),
          type_for_color = ifelse(is.na(event), "event_group", "event")
        )

      color_data$selected <- selected_data

      #apply through all events & event groups and create a div container for the color settings
      lapply(seq_along(selected_data$names_for_color_list), function(column_number) {
        div(
          class = "col col-transparent-box selected",
          div(
            class = "selected-col-inner",
            style = paste0(
              "background:", selected_data[column_number,]$event_color,";",
              "padding:", ifelse(selected_data[column_number,]$type == "event", "0px 0px 0px 0px","0px 0px 0px 50px" ),";",
              "color: ",font_color(selected_data[column_number,]$event_color),";"
            ),
            `data-colnum` = column_number,
            selected_data[column_number,]$names_for_color_list
          )
        )
      })
    }
  })
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
    shiny::updateSelectizeInput(
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


  #### Data Preparation & Filter ####
  ##### megaplot_prepared_data ####
  megaplot_prepared_data <- shiny::reactive({

    megaplot_data_raw <- shiny::req(uploaded_data$val)
    all_colors <- color_data$all

    prepare_megaplot_data(
      megaplot_data = shiny::req(uploaded_data$val),
      event_colors = all_colors,
      grouping_vars = input$select.grouping,
      sorting_var = input$select_sorting
    )
  })

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


  #### Mega Plot ####
  output$mega_plots <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())
    #shiny::req(megaplot_filtered_data())
    draw_mega_plot(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select.grouping = input$select.grouping,
      line_width = input$line_width
    )
  })

  #### Event summary ####
  output$event_summary <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())
    shiny::req(megaplot_filtered_data())
    draw_event_summary(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = input$select.grouping,
      event_summary_cutoff = input$event_summary_cutoff
    )

  })

  #### Kaplan Meier Plots ####
  #update kaplan meier event selection based on selected events
  shiny::observeEvent(input$tree, {
    shiny::req(input$tree)

    shinyWidgets::updatePickerInput(
      inputId = 'select_event_kaplan_meier',
      choices = unlist(shinyTree::get_selected(input$tree, format=c("classid"))),
      selected = NULL,
    )
  })

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

  text_scale <- reactive({
    switch(input$font_size,
           "small" = 1.0,
           "standard" = 1.2,
           "large" = 1.4)
  })


  currentScale <- 1

  shiny::observeEvent(input$font_size, {
    scale <- text_scale()
    the_theme <- bslib::bs_current_theme()
    the_theme <- bslib::bs_theme_update(the_theme, font_scale = scale / currentScale)
    session$setCurrentTheme(the_theme)
    currentScale <<- scale
  })
}
