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

  #### Observer to update selected navbar based on button click ####
  shiny::observeEvent(input$upload_2_next_button, {
    bslib::nav_select("MEGAPLOTS", "Megaplots")
  })

  shiny::observeEvent(input$upload_1_next_button, {
    bslib::nav_select("Upload", "Event & color selection")
  })

  shiny::observeEvent(input$upload_2_back_button, {
    bslib::nav_select("Upload", "File & variable selection")
  })

  #### Downloadbuton appearance condition
  #### Output: Funnel Availability ####
  color_changed_reactive <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(input$update_color_palette, input$update_color_palette_2), {
    color_changed_reactive$val <- TRUE
  },ignoreInit = TRUE)

  output$color_changed <- shiny::reactive({
    color_changed_reactive$val
  })
  outputOptions(output, "color_changed", suspendWhenHidden = FALSE)

  output$save_colors <- shiny::downloadHandler(
    filename = function() {
      paste("Megaplot_color_file", gsub(":", "-", Sys.Date()), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(
         color_data$all %>%
           dplyr::select(megaplots_selected_event, megaplots_selected_event_group, event_color, gradient_event_color_1, gradient_event_color_2, gradient_event_color_3, jittered)
        , file
      )
    }
  )

  shiny::observeEvent(input$upload_saved_color_file, {

    dimension_color_data <- dim(color_data$all)
    uploaded_colors <- readRDS(input$upload_saved_color_file$datapath)

    #add checks if uploaded data matches the existing color_data
    if (dim(uploaded_colors)[1] == dim(color_data$all)[1]) {
      if (all(sort(unique(uploaded_colors$megaplots_selected_event)) == sort(unique(color_data$all$megaplots_selected_event)))) {
        uploaded_colors <- uploaded_colors  %>%
          dplyr::mutate(
            gradient_event_color_1 = dplyr::case_when(is.na(megaplots_selected_event) ~ gradient_event_color_1, !is.na(megaplots_selected_event) ~ NA),
            gradient_event_color_2 = dplyr::case_when(is.na(megaplots_selected_event) ~ gradient_event_color_2, !is.na(megaplots_selected_event) ~ NA),
            gradient_event_color_3 = dplyr::case_when(is.na(megaplots_selected_event) ~ gradient_event_color_3, !is.na(megaplots_selected_event) ~ NA)
          )
        color_data_new <- color_data$all %>% dplyr::select(megaplots_selected_event, megaplots_selected_event_group, event_id, event_group_id, max_event_id, event_n, n_flag) %>%
          dplyr::left_join(
            uploaded_colors %>% dplyr::select(megaplots_selected_event, megaplots_selected_event_group, event_color, gradient_event_color_1, gradient_event_color_2, gradient_event_color_3, jittered),
            by = c("megaplots_selected_event", "megaplots_selected_event_group")
          )
        #check new dimension matches
        if (all(dimension_color_data == dim(color_data_new))) {
          color_data$all <- color_data_new
        }
      }
    }
  })

  #### Javascript Code to hide/show single Widgets ####
  # shinyjs code to hide/show panels in the beginning
  shinyjs::hideElement(id = "selected_events_panel")
  shinyjs::hideElement(id = "selected_events_color_container_panel")
  shinyjs::hideElement(id = "colour_picker_panel_1")
  shinyjs::hideElement(id = "colour_picker_panel_2")
  shinyjs::hideElement(id = "colour_picker_panel_3")
  shinyjs::hideElement(id = "colour_picker_panel_event")
  shinyjs::hideElement(id = "update_color_palette_2")
  shinyjs::hideElement(id = "color_method")
  shinyjs::hideElement(id = "jitter_events")
  shinyjs::hideElement(id = "update_color_palette")
  shinyjs::hideElement(id = "colour_picker_panel_unique")
  shinyjs::hideElement(id = "colour_picker_panel")

  shinyjs::hideElement(id = "select_subjectid")
  shinyjs::hideElement(id = "select_start_time")
  shinyjs::hideElement(id = "select_end_time")
  shinyjs::hideElement(id = "select_event_time")
  shinyjs::hideElement(id = "select_event_time_end")
  shinyjs::hideElement(id = "select_event")
  shinyjs::hideElement(id = "select_event_group")
  shinyjs::hideElement(id = "upload_1_next_button")

  shiny::observeEvent(uploaded_data$val, {
    if (is.null(uploaded_data$val)) {
      shinyjs::hideElement(id = "select_subjectid")
      shinyjs::hideElement(id = "select_start_time")
      shinyjs::hideElement(id = "select_end_time")
      shinyjs::hideElement(id = "select_event_time")
      shinyjs::hideElement(id = "select_event_time_end")
      shinyjs::hideElement(id = "select_event")
      shinyjs::hideElement(id = "select_event_group")
      shinyjs::hideElement(id = "upload_1_next_button")
    } else {
      shinyjs::showElement(id = "select_subjectid")
      shinyjs::showElement(id = "select_start_time")
      shinyjs::showElement(id = "select_end_time")
      shinyjs::showElement(id = "select_event_time")
      shinyjs::showElement(id = "select_event_time_end")
      shinyjs::showElement(id = "select_event")
      shinyjs::showElement(id = "select_event_group")

      shiny::updateSelectInput(
        session,
        "select_subjectid",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "subjectid" %in% colnames(uploaded_data$val),
          "subjectid",
          NULL
        )
      )
      shiny::updateSelectInput(
        session,
        "select_start_time",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "start_time" %in% colnames(uploaded_data$val),
          "start_time",
          NULL
        )
      )
      shiny::updateSelectInput(
        session,
        "select_end_time",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "end_time" %in% colnames(uploaded_data$val),
          "end_time",
          NULL
        )
      )
      shiny::updateSelectInput(
        session,
        "select_event_time",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "event_time" %in% colnames(uploaded_data$val),
          "event_time",
          NULL
        )
      )
      shiny::updateSelectInput(
        session,
        "select_event_time_end",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "event_time_end" %in% colnames(uploaded_data$val),
          "event_time_end",
          NULL
        )
      )
      shiny::updateSelectInput(
        session,
        "select_event",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "event" %in% colnames(uploaded_data$val),
          "event",
          NULL
        )
      )
      shiny::updateSelectInput(
        session,
        "select_event_group",
        choices = colnames(uploaded_data$val),
        selected = ifelse(
          "event_group" %in% colnames(uploaded_data$val),
          "event_group",
          NULL
        )
      )
    }
  })

   # variable_check <- shiny::reactiveValues(val = FALSE)
  #### Variable check after data upload ####
  shiny::observeEvent(
    c(uploaded_data$val, input$select_subjectid,
      input$select_start_time, input$select_end_time,
      input$select_event, input$select_event_group,
      input$select_event_time, input$select_event_time_end), {

    variable_check <- check_megaplot_data_variables(
      megaplot_data = uploaded_data$val,
      subjectid = input$select_subjectid,
      start_time = input$select_start_time,
      end_time = input$select_end_time,
      event = input$select_event,
      event_group = input$select_event_group,
      event_time = input$select_event_time,
      event_time_end = input$select_event_time_end
    )
    # variable_check$val <- variable_check
      # when check is successful display next button
      if (variable_check) {
        shinyjs::showElement(id = "upload_1_next_button")
        # variable_check$val <- variable_check
      } else {
        shinyjs::hideElement(id = "upload_1_next_button")
        # variable_check$val <- variable_check
      }
  })

  shiny::observeEvent(uploaded_data$val, {
    if (is.null(uploaded_data$val)) {
      shinyjs::hideElement(id = "selected_events_panel")
    } else {
      shinyjs::showElement(id = "selected_events_panel")
    }
  })

  shiny::observeEvent(checked_data$val, {
    if (nrow(checked_data$val) == 0) {
      shinyjs::hideElement(id = "selected_events_color_container_panel")
    } else {
      shinyjs::showElement(id = "selected_events_color_container_panel")
    }
  })

  #create reactive variable for selected color container row
  js_column <- shiny::reactiveValues(number = NULL)

  shiny::observeEvent(input$jsColNum, {
    js_column$number <- input$jsColNum
  })

  shiny::observeEvent(checked_data$val, {
    js_column$number <- input$jsColNum
  })

  shiny::observeEvent(js_column$number, {
    if (!is.null(js_column$number)) {

    } else  {
      shinyjs::hideElement(id = "colour_picker_panel_1")
      shinyjs::hideElement(id = "colour_picker_panel_2")
      shinyjs::hideElement(id = "colour_picker_panel_3")
      shinyjs::hideElement(id = "update_color_palette")
      shinyjs::hideElement(id = "color_method")
      shinyjs::hideElement(id = "jitter_events")
      shinyjs::hideElement(id = "colour_picker_panel_event")
      shinyjs::hideElement(id = "update_color_palette_2")
      shinyjs::hideElement(id = "colour_picker_panel_unique")
      shinyjs::hideElement(id = "colour_picker_panel")
    }
  }, ignoreNULL = FALSE)

  shiny::observe({
    input$jsColNum
    shiny::req(js_column$number)
    shiny::req(color_data$selected)
    if (is.na(color_data$selected[js_column$number, "megaplots_selected_event"])) {
      # shinyjs::showElement(id = "colour_picker_panel")
      shinyjs::showElement(id = "colour_picker_panel")
      shinyjs::showElement(id = "colour_picker_panel_event")
      if (input$color_method == "gradient") {
        shinyjs::hideElement(id = "colour_picker_panel_event")
        shinyjs::hideElement(id = "update_color_palette_2")
        shinyjs::showElement(id = "colour_picker_panel_1")
        shinyjs::showElement(id = "colour_picker_panel_2")
        shinyjs::showElement(id = "colour_picker_panel_3")
        shinyjs::hideElement(id = "colour_picker_panel_unique")
      } else  if (input$color_method == "unique") {
        shinyjs::hideElement(id = "colour_picker_panel_event")
        shinyjs::hideElement(id = "update_color_palette_2")
        shinyjs::hideElement(id = "colour_picker_panel_1")
        shinyjs::hideElement(id = "colour_picker_panel_2")
        shinyjs::hideElement(id = "colour_picker_panel_3")
        shinyjs::showElement(id = "colour_picker_panel_unique")
      } else  if (input$color_method == "palette") {
        shinyjs::hideElement(id = "colour_picker_panel_event")
        shinyjs::hideElement(id = "update_color_palette_2")
        shinyjs::hideElement(id = "colour_picker_panel_1")
        shinyjs::hideElement(id = "colour_picker_panel_2")
        shinyjs::hideElement(id = "colour_picker_panel_3")
        shinyjs::hideElement(id = "colour_picker_panel_unique")
      }
      shinyjs::showElement(id = "jitter_events")
      shinyjs::showElement(id = "colour_palette")
      shinyjs::showElement(id = "update_color_palette")
      shinyjs::showElement(id = "color_method")
    } else {
      shinyjs::showElement(id = "colour_picker_panel")
      # shinyjs::hideElement(id = "colour_picker_panel")
      shinyjs::showElement(id = "colour_picker_panel_event")
      shinyjs::showElement(id = "update_color_palette_2")
      shinyjs::hideElement(id = "colour_picker_panel_1")
      shinyjs::hideElement(id = "colour_picker_panel_2")
      shinyjs::hideElement(id = "colour_picker_panel_3")
      shinyjs::hideElement(id = "colour_picker_panel_unique")
      shinyjs::hideElement(id = "jitter_events")
      shinyjs::hideElement(id = "colour_palette")
      shinyjs::hideElement(id = "update_color_palette")
      shinyjs::hideElement(id = "color_method")
    }
  })

  #Changing the maximum height of the upload panels depending on screen size
  shiny::observe({
    shinyjs::runjs(
      sprintf(
        paste0(
          "document.getElementById('selected_events_panel').style.maxHeight =
          '", input$dimension - 300, "px';"
        )
      )
    )
    shinyjs::runjs(
      sprintf(
        paste0("document.getElementById('selected-cols-row').style.maxHeight =
               '", input$dimension - 300, "px';"
        )
      )
    )
  })

  #### jsTreeR output ####
  output$tree2 <- jsTreeR::renderJstree({
    #javascript code to avoid that child nodes moved into another child nodes
    check_callback <- jsTreeR::JS(
      "function(operation, node, parent, position, more) {",
      "  if(operation === 'move_node') {",
      "    if(parent.id === '#' || parent.type === 'child') {",
      "      return false;",
      "    }",
      "  }",
      "  return true;",
      "}"
    )

    dnd <- list(
      is_draggable = jsTreeR::JS(
        "function(node) {",
        "  if(node[0].type !== 'child') {",
        "    return false;",
        "  }",
        "  return true;",
        "}"
      )
    )

    #icons
    types <- list(
      root = list(icon = "fa-regular fa-arrow_pointer"),
      child = list(icon = "fa-regular fa-arrow_pointer")
    )

    jsTreeR::jstree(
      create_jsTree_input(data = unique_event_group_data()),
      #use create_jsTree_input function to create desired list input
      dragAndDrop = TRUE,
      search = TRUE,
      dnd = dnd,
      checkCallback = check_callback,
      types = types,
      contextMenu = list(create =TRUE, delete = TRUE),
      checkboxes = TRUE
    )
  })


  #### Reactive value color_data ####
  # initialize reactive value color_data with entries "all" and "selected"
  # all includes all events from uploaded data set and in selected filtered by
  # selected events in shinyTree input
  color_data <- shiny::reactiveValues(
    all = NULL,
    selected = NULL
  )

  shiny::observe({
    color_data$all
  })

  #### update sorting ####
  shiny::observeEvent(uploaded_data$val, {
    choices <- names(which(unlist(lapply(uploaded_data$val, is.numeric))))
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select_sorting",
      choices = choices,
      choicesOpt = list(style =  rep_len(
        "font-size: 60%; line-height: 1.6;", length(choices)
      )
      )
    )
  })


  #observer to update ColourInput based on container click
  shiny::observeEvent(js_column$number, {

    output$colorization_selection <- shiny::renderText(
      ifelse(
        is.na(color_data$selected[js_column$number, "event"]),
        color_data$selected[js_column$number, "event_group"],
        color_data$selected[js_column$number, "event"]
      )
    )
    colourpicker::updateColourInput(
      session,
      inputId = "colour_picker_panel_event",
      label = "",
      value = color_data$selected[js_column$number, "event_color"]
    )
    colourpicker::updateColourInput(
      session,
      inputId = "colour_picker_panel_1",
      label = "",
      value = color_data$selected[js_column$number, "gradient_event_color_1"]
    )
    colourpicker::updateColourInput(
      session,
      inputId = "colour_picker_panel_2",
      label = "",
      value = color_data$selected[js_column$number, "gradient_event_color_2"]
    )
    colourpicker::updateColourInput(
      session,
      inputId = "colour_picker_panel_3",
      label = "",
      value = color_data$selected[js_column$number, "gradient_event_color_3"]
    )
    colourpicker::updateColourInput(
      session,
      inputId = "colour_picker_panel_unique",
      label = "",
      value = color_data$selected[js_column$number, "gradient_event_color_2"]
    )

    val <- any(color_data$all[
      color_data$all$megaplots_selected_event_group ==
        color_data$selected[js_column$number, ]$megaplots_selected_event_group,
    ]$jittered
    )
    shiny::updateCheckboxInput(
      session,
      inputId = "jitter_events",
      value = val
    )

  })

  # The purpose of this observer is to create/initialize a logical vector
  # for every event/event_group available in the data. By default events within
  # event_groups are jittered on the y-axis.
  # Jittering events within an event group can be turned off within app.

  shiny::observeEvent(input$jitter_events, {
    #require the selected number of colored div container list
    shiny::req(js_column$number)
    if (!is.null(color_data$all)) {
      if (!is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")] &
            color_data$all$megaplots_selected_event ==
              color_data$selected[js_column$number, c("megaplots_selected_event")] &
            !is.na(color_data$all$megaplots_selected_event ==
                     color_data$selected[js_column$number, c("megaplots_selected_event")]),
        ]$jittered <- input$jitter_events
      }
      if (is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")],
        ]$jittered <- input$jitter_events
      }
    }
  })

  shiny::observeEvent(input$update_color_palette_2, {
    shiny::req(js_column$number)
    if (!is.null(color_data$all)) {
      if (!is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")] &
            color_data$all$megaplots_selected_event ==
              color_data$selected[js_column$number, c("megaplots_selected_event")]
          & !is.na(color_data$all$megaplots_selected_event ==
                     color_data$selected[js_column$number, c("megaplots_selected_event")]),
        ]$event_color <- input$colour_picker_panel_event
      }
    }
  })

  # The purpose of this observer is to create/initialize a color vector for
  # every event/event_group available in the data.
  # By default event_groups receive an own color and events within the groups
  # get color shades of group color.
  # Color for group and single events can be changed within app.

  shiny::observeEvent(c(input$update_color_palette), {
    #require the selected number of colored div container list
    shiny::req(js_column$number)
    if (!is.null(color_data$all)) {
      if (is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {

        #create new color for entire event group
        if (input$color_method == "gradient") {
          f_col_z <- grDevices::colorRamp(c(input$colour_picker_panel_1,
                                            input$colour_picker_panel_2,
                                            input$colour_picker_panel_3))

          cds_tmp <- color_data$selected[color_data$selected$megaplots_selected_event_group ==
            color_data$selected[js_column$number,
                                c("megaplots_selected_event_group")],
            c("megaplots_selected_event_group", "megaplots_selected_event")] %>%
            dplyr::filter(!is.na(.data$megaplots_selected_event)) %>%
          dplyr::mutate(new_event_id = dplyr::row_number(.data$megaplots_selected_event_group))

          new_event_group_color <- color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[
                js_column$number, c("megaplots_selected_event_group")], ] %>%
          dplyr::filter(.data$event_id >= 1)  %>%
          dplyr::left_join(cds_tmp, by = c("megaplots_selected_event", "megaplots_selected_event_group")) %>%
          dplyr::mutate(event_id = .data$new_event_id)

        new_group_id <- c(new_event_group_color$new_event_id, 0)


        new_event_group_color <- new_event_group_color %>%
          dplyr::select(-tidyselect::all_of(c("new_event_id"))) %>%
          dplyr::mutate(
            event_color =  grDevices::rgb(
              f_col_z(seq(0, 1, length = nrow(.))),
              maxColorValue = 255
            )[.data$event_id]
          ) %>%
          dplyr::pull(.data$event_color)
        new_event_group_color <- c(new_event_group_color, "#404A4E")

        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")],
        ]$event_color <- new_event_group_color
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")],
        ]$event_id <- new_group_id
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")],
        ]$gradient_event_color_1 <- input$colour_picker_panel_1
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$gradient_event_color_2 <- input$colour_picker_panel_2
        color_data$all[
          color_data$all$megaplots_selected_event_group ==
            color_data$selected[js_column$number, c("megaplots_selected_event_group")],
        ]$gradient_event_color_3 <- input$colour_picker_panel_3
      } else if (input$color_method == "unique") {
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$event_color <-  input$colour_picker_panel_unique
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$gradient_event_color_1 <- input$colour_picker_panel_unique
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
            ]$gradient_event_color_2 <- input$colour_picker_panel_unique
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$gradient_event_color_3 <- input$colour_picker_panel_unique
        } else if (input$color_method == "palette") {
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$event_color <- c(
            "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
            "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
            "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
            "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
            "#fabed4", "#4263d8"
          )[seq_along(color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$event_color)]
        }
      }
    }
  })

  shiny::observeEvent(uploaded_data_w_ids(), {
    uploaded_data_w_ids()
    #update data set color_data$all
    color_data$all <- uploaded_data_w_ids()
  })

  checked_data <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(
     input[["tree2"]],
     input[["tree2_checked_tree"]]
   ), {
    checked_tree <- input[["tree2_checked_tree"]]
   selected_data <- data.frame(megaplots_selected_event_group = NULL, megaplots_selected_event = NULL)
    if (!is.null(checked_tree)) {
      if (length(checked_tree) > 0) {
      for (i in seq_along(checked_tree)) {
        for (j in seq_along(checked_tree[[i]]$children)) {
          if (j == 1) {
            selected_data <- rbind(
              selected_data,
              data.frame(megaplots_selected_event_group = checked_tree[[i]]$text, megaplots_selected_event = NA)
            )
          }
          selected_data <- rbind(
            selected_data,
            data.frame(
              megaplots_selected_event_group = checked_tree[[i]]$text,
              megaplots_selected_event = checked_tree[[i]]$children[[j]]$text
            )
          )
        }
      }
      }
    }
    checked_data$val <- selected_data
  })

  shiny::observeEvent(c(
    uploaded_data$val, input$select_subjectid,
     input$select_start_time, input$select_end_time,
     input$select_event, input$select_event_group,
     input$select_event_time, input$select_event_time_end
  ), {
    checked_data$val <- NULL
  })


  # checked_data <- shiny::eventReactive(c(
  #   #tbd: create a reactivevalue out of checked data to make sure,
  #   # this object is set to NULL when input$select_event is changed
  #   # otherwise this leads to an error :argument of length zero
  #    input[["tree2"]],
  #    input[["tree2_checked_tree"]]
  #  ), {
  #   checked_tree <- input[["tree2_checked_tree"]]
  #
  #  selected_data <- data.frame(megaplots_selected_event_group = NULL, megaplots_selected_event = NULL)
  #
  #   if (!is.null(checked_tree)) {
  #     if (length(checked_tree) > 0) {
  #     for (i in seq_along(checked_tree)) {
  #       for (j in seq_along(checked_tree[[i]]$children)) {
  #         if (j == 1) {
  #           selected_data <- rbind(
  #             selected_data,
  #             data.frame(megaplots_selected_event_group = checked_tree[[i]]$text, megaplots_selected_event = NA)
  #           )
  #         }
  #         selected_data <- rbind(
  #           selected_data,
  #           data.frame(
  #             megaplots_selected_event_group = checked_tree[[i]]$text,
  #             megaplots_selected_event = checked_tree[[i]]$children[[j]]$text
  #           )
  #         )
  #       }
  #     }
  #     }
  #   }
  #   selected_data
  # })

  #### Color container output ####
  output$selected_events_color_container <- renderUI({
    if(!is.null(checked_data$val)) {
     if (nrow(checked_data$val) > 0)  {

        selected_data <- create_color_container(
          tree = checked_data$val,
          color_vector = color_data$all
        )

        color_data$selected <- selected_data
        #apply through all events & event groups and create
        # a div container for the color settings
        lapply(seq_along(selected_data$names_for_color_list),
               function(column_number) {
          div(
            class = "col col-transparent-box selected",
            div(
              class = "selected-col-inner",
              style = paste0(
                "background:", ifelse(
                  selected_data[column_number, ]$type == "megaplots_selected_event",
                  selected_data[column_number, ]$event_color,  "#404A4E"),
                ";",
                "padding:", ifelse(
                  selected_data[column_number, ]$type == "megaplots_selected_event",
                  "0px 0px 0px 0px",
                  "0px 0px 0px 50px"),
                ";",
                "color: ",
               # "white",
                font_color(selected_data[column_number, ]$event_color),
                ";"
              ),
              `data-colnum` = column_number,
              selected_data[column_number, ]$names_for_color_list
            )
          )
        })
     } else {
        NULL
     }
    }
  })



  output$colour_palette <- renderPlot({

    if (!is.null(color_data$selected) & !is.null(js_column$number)) {
      if (input$color_method == "gradient") {

      number_events <- nrow(
        color_data$selected[
          color_data$selected$megaplots_selected_event_group ==
            color_data$selected[js_column$number, "megaplots_selected_event_group"] &
            !is.na(color_data$selected$megaplots_selected_event),
          ])

      f_col_z <- grDevices::colorRamp(
        c(input$colour_picker_panel_1,
          input$colour_picker_panel_2,
          input$colour_picker_panel_3)
      )

      par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
      graphics::image(
        1:number_events,
        1,
        as.matrix(1:number_events),
        col = grDevices::rgb(f_col_z(seq(0, 1, length = number_events)),
                             maxColorValue = 255),
        xlab = "",
        ylab = "",
        xaxt = "n",
        yaxt = "n",
        bty = "n"
      )
      graphics::abline(h = 0.602, lwd = 3, col = "#000000")
      graphics::abline(h = 1.398, lwd = 3, col = "#000000")
      } else if (input$color_method == "unique") {
        number_events <- nrow(
          color_data$selected[color_data$selected$megaplots_selected_event_group ==
                                color_data$selected[js_column$number,
                                                    "megaplots_selected_event_group"] &
                                !is.na(color_data$selected$megaplots_selected_event), ])
        par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        graphics::image(
          1:number_events,
          1,
          as.matrix(1:number_events),
          col = input$colour_picker_panel_unique,
          xlab = "",
          ylab = "",
          xaxt = "n",
          yaxt = "n",
          bty = "n"
        )
        graphics::abline(h = 0.602, lwd = 3, col = "#000000")
        graphics::abline(h = 1.398, lwd = 3, col = "#000000")
      } else if (input$color_method == "palette") {
        megaplot_color <- c(
          "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
          "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
          "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
          "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
          "#fabed4", "#4263d8"
        )
        number_events <- nrow(
          color_data$selected[
            color_data$selected$megaplots_selected_event_group ==
              color_data$selected[js_column$number, "megaplots_selected_event_group"] &
              !is.na(color_data$selected$megaplots_selected_event), ]
        )
        par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        graphics::image(
          1:number_events,
          1,
          as.matrix(1:number_events),
          col = megaplot_color[1:number_events],
          xlab = "",
          ylab = "",
          xaxt = "n",
          yaxt = "n",
          bty = "n"
        )
        graphics::abline(h = 0.602, lwd = 3, col = "#000000")
        graphics::abline(h = 1.398, lwd = 3, col = "#000000")
      }
    } else {

      par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
      graphics::image(
        1:10,
        1,
        as.matrix(1:10),
        col = "#404A4E",
        xlab = "",
        ylab = "",
        xaxt = "n",
        yaxt = "n",
        bty = "n"
      )
    }
  })

  #### Data upload & data set preparation ####

  # Initialize reactive value for uploaded data
  uploaded_data <- shiny::reactiveValues(val = NULL)

  # Update widgets and reactive data object when fileinput is observed
  shiny::observeEvent(c(input$file, sequencing_order_data$val), {
    shiny::req(input$file) #requires input$file

    #load data
    megaplot_data <- base::get(
      load(
        file = input$file$datapath
      )
    )

    if (!is.null(sequencing_order_data$val)) {
      megaplot_data <- megaplot_data %>%
        dplyr::left_join(
          sequencing_order_data$val,
          by ="subjectid"
        )
    }

    uploaded_data$val <- megaplot_data  #update reactive value 'uploaded_data'

    # update choices of grouping variable based on uploaded data
    # includes all factor and character variables
    shiny::updateSelectizeInput(
      session,
      inputId = "select_grouping",
      choices = colnames(megaplot_data)[
        sapply(megaplot_data, class) %in% c("factor", "character")],
      selected = NULL
    )

    shinyWidgets::updatePickerInput(
      session,
      inputId = "select_strata_var",
      choices = colnames(megaplot_data)[
        sapply(megaplot_data, class) %in% c("factor", "character")],
      selected = NULL
    )
  })


  # rename variables due to variable selection
  uploaded_data_renamed <- shiny::reactive({
    shiny::req(uploaded_data$val)
    shiny::req(input$select_subjectid)

    rename_require_variables(
      shiny::req(uploaded_data$val),
      selected_subjectid = shiny::req(input$select_subjectid),
      selected_start_time = shiny::req(input$select_start_time),
      selected_end_time = shiny::req(input$select_end_time),
      selected_event = shiny::req(input$select_event),
      selected_event_group = shiny::req(input$select_event_group),
      selected_event_time = shiny::req(input$select_event_time),
      selected_event_time_end = shiny::req(input$select_event_time_end)
    )
  })


  uploaded_data_filtered <- shiny::reactive({
    shiny::req(uploaded_data_renamed())
  })

  # add event & event group identifier to data set and number
  # events within group
  uploaded_data_w_ids <- shiny::reactive({
    create_unique_event_identifier(
      #megaplot_data_raw = shiny::req(uploaded_data$val)
      megaplot_data_raw = uploaded_data_renamed()
    )
  })

  #create a data frame which includes unique rows with
  # event /event group information
  unique_event_group_data <- shiny::eventReactive(uploaded_data_renamed(), {
    #megaplot_data_raw <- shiny::req(uploaded_data$val)
    megaplot_data_raw <- uploaded_data_renamed()
    reduced_event_data <- megaplot_data_raw %>%
      dplyr::filter(!is.na(.data$megaplots_selected_event)) %>%
      dplyr::select(tidyselect::all_of(c("megaplots_selected_event_group", "megaplots_selected_event"))) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$megaplots_selected_event_group, .data$megaplots_selected_event)

    reduced_event_data
  })

  #### reactive object megaplot_prepared_data ####
  megaplot_prepared_data <- shiny::eventReactive(
    c(uploaded_data_w_ids(),
      uploaded_data_renamed(),
      input$select_grouping,
      input$select_sorting), {

    prepare_megaplot_data(
      #megaplot_data_raw = uploaded_data$val,
      megaplot_data_raw = uploaded_data_renamed(),
      uploaded_data_w_ids = uploaded_data_w_ids(),
      select_sorting = input$select_sorting,
      select_grouping = input$select_grouping
    )
  })

  #### reactive object megaplot_filtered_data ####
  # updates when: shinyTree selection changes
  #               jittering option changes
  #               color option changes
  megaplot_filtered_data <- shiny::reactive({

    filter_megaplot_data(
      tree = shiny::req(checked_data$val),
      megaplot_prepared_data = shiny::req(megaplot_prepared_data()),
      color_data = shiny::req(color_data$all)
    )
  })


  #### Mega Plot ####
  #create reactive variable for saving html output
  session_store <- shiny::reactiveValues(val = NULL)

  output$mega_plots <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())
    tmp <- draw_mega_plot(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = input$select_grouping,
      line_width = input$line_width,
      line_width_subjects = input$line_width_subjects
    )
    session_store$val <- tmp
    tmp
  })

  output$download_plotly_widget <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      htmlwidgets::saveWidget(plotly::as_widget(session_store$val), file, selfcontained = TRUE)
    }
  )

  #### Event summary ####
  output$event_summary <- plotly::renderPlotly({
    shiny::req(megaplot_prepared_data())
    shiny::req(megaplot_filtered_data())
    draw_event_summary(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = input$select_grouping,
      event_summary_cutoff = input$event_summary_cutoff
    )
  })

  #### Kaplan Meier Plots ####
  #update kaplan meier event selection based on selected events
  shiny::observeEvent(megaplot_filtered_data(), {
    shiny::req(megaplot_filtered_data())
    megaplot_filtered_data()$unique_event
    shinyWidgets::updatePickerInput(
      inputId = "select_event_kaplan_meier",
      choices = unique(megaplot_filtered_data()$unique_event),
      selected = NULL
    )
  })

  output$kaplan_meier <- plotly::renderPlotly({
    shiny::req(megaplot_prepared_data())
    shiny::req(megaplot_filtered_data())

    draw_kaplan_meier(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = input$select_grouping,
      select_event_kaplan_meier = input$select_event_kaplan_meier,
      select_strata_var = input$select_grouping
    )
  })

  #sequencing
  #sequencing/ai module (server part)
  sequencing_order_data <- shiny::reactiveValues(val = NULL)

  artificial_intelligence <- shiny::callModule(
    artificial_intelligence_server,
    "ai",
    shiny::reactive({megaplot_filtered_data()})
  )

  shiny::observeEvent(artificial_intelligence$seq.button(),{
    if (!is.null(artificial_intelligence$varSeq())) {
      sequencing_output <- megaplots_sequencing_functions(
        final_data = megaplot_filtered_data(),
        variable = artificial_intelligence$varSeq(),
        seriation_parameter = artificial_intelligence$input_seriation(),
        seriation_method = artificial_intelligence$methSer(),
        group = input$select_grouping,
        multiple_distmeasures = artificial_intelligence$multiple_distmeasures()
      )
      sequencing_order_data$val <- sequencing_output
    }
    #data_w_ai_information_reacVal$df <- data_w_ai_information
    # megaplot_filtered_data(sequencing_output)
  })

}
