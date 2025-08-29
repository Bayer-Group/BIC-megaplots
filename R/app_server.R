#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {


  #### Javascript Code ####
  #### Upload page ####
  # shinyjs code to hide/show panels in the beginning
  shinyjs::hideElement(id= "selected_events_panel")
  shinyjs::hideElement(id= "selected_events_color_container_panel")
  shinyjs::hideElement(id= "colour_picker_panel")
  shinyjs::hideElement(id = "jitter_events")

  shiny::observeEvent(uploaded_data$val, {
    if(is.null(uploaded_data$val)) {
      shinyjs::hideElement(id = "selected_events_panel")
    } else {
      shinyjs::showElement(id = "selected_events_panel")
    }
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
    shiny::req(input$jsColNum)
    shiny::req(color_data$selected)
    if(is.na(color_data$selected[input$jsColNum,"event"])) {
      shinyjs::showElement(id = "jitter_events")
    } else {
      shinyjs::hideElement(id = "jitter_events")
    }
  })

  #Changing the maximum height of the upload panels depending on screen size
  shiny::observe({
    shinyjs::runjs(sprintf(paste0("
            document.getElementById('selected_events_panel').style.maxHeight = '",input$dimension-300,"px';")))
    shinyjs::runjs(sprintf(paste0("
            document.getElementById('selected-cols-row').style.maxHeight = '",input$dimension-300,"px';")))
  })


  # renderTree for event/event_group selection
  output$tree <- shinyTree::renderTree({
    create_event_tree(reduced_event_data = shiny::req(unique_event_group_data()))
  })

  shiny::observe({
    colourpicker::updateColourInput(
      session,
      inputId="picked_colour",
      label = ifelse(is.na(color_data$selected[input$jsColNum,"event"]),color_data$selected[input$jsColNum,"event_group"],color_data$selected[input$jsColNum,"event"]),
      value = color_data$selected[input$jsColNum,"event_color"]
    )
  })

  shiny::observeEvent(input$jitter_events, {
    #require the selected number of colored div container list
    shiny::req(input$jsColNum)
    if (!is.null(color_data$all)) {
      if(!is.na(color_data$selected[input$jsColNum, c("event")])) {
        color_data$all[
          color_data$all$event_group == color_data$selected[input$jsColNum,c("event_group")] & color_data$all$event == color_data$selected[input$jsColNum,c("event")] & !is.na(color_data$all$event == color_data$selected[input$jsColNum,c("event")]),]$jittered <- input$jitter_events
      }
      if (is.na(color_data$selected[input$jsColNum,c("event")])) {
        color_data$all[color_data$all$event_group == color_data$selected[input$jsColNum,c("event_group")],]$jittered <- input$jitter_events
      }
    }
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

  uploaded_data_w_ids <- shiny::reactive({
    create_unique_event_identifier(megaplot_data_raw = shiny::req(uploaded_data$val))
  })

  observe({color_data$all})

  output$selected_events_color_container <- renderUI({
    shiny::req(input$tree)
    if(!is.null(input$tree)) {
      if(length(shinyTree::get_selected(input$tree, format = "names"))>0)  {
      selected_data <- create_color_container(
        tree = input$tree,
        color_vector = color_data$all
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
      inputId = "select_grouping",
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


  unique_event_group_data <- shiny::eventReactive(uploaded_data$val, {
    megaplot_data_raw <- shiny::req(uploaded_data$val)

    reduced_event_data <- megaplot_data_raw %>%
      dplyr::filter(!is.na(event)) %>%
      dplyr::select(event_group, event) %>%
      dplyr::distinct() %>%
      dplyr::arrange(event_group,event)

    reduced_event_data
  })

  observeEvent(uploaded_data_w_ids(),{
    # megaplot_prepared_data()
    uploaded_data_w_ids()
    color_data$all <- uploaded_data_w_ids()
     # megaplot_filtered_data()
  })

  #### Data Preparation & Filter ####
  ### megaplot_prepared_data ####
  megaplot_prepared_data <- shiny::eventReactive(c(uploaded_data_w_ids(),uploaded_data$val, input$select_grouping, input$select_sorting),{
    megaplot_data_raw <- uploaded_data$val
    uploaded_data_w_ids <- uploaded_data_w_ids()

    # all_colors <- color_data$all
    megaplot_data_arranged <- dplyr::arrange(megaplot_data_raw, !!!rlang::syms(input$select_grouping), !!rlang::sym(input$select_sorting))

    megaplot_data_raw <- megaplot_data_raw %>%
      dplyr::left_join(
        data.frame(
          subjectid = unique(megaplot_data_arranged$subjectid),
          subject_index = seq_along(unique(megaplot_data_arranged$subjectid))),
          by = "subjectid"
        ) %>%
      dplyr::mutate(subjectid_n = subject_index)

    megaplot_data_raw <- megaplot_data_raw %>%
      dplyr::left_join(
        uploaded_data_w_ids,
        by = c("event_group","event")
      )


    megaplot_data_raw <- megaplot_data_raw %>%
      dplyr::group_by(!!!rlang::syms(input$select_grouping)) %>%
      dplyr::mutate(group_index = dplyr::cur_group_id())

    megaplot_data_raw
  })


  megaplot_filtered_data <- shiny::reactive({
    # selected.events <- unlist(shinyTree::get_selected(input$tree, format=c("classid")))
    selected_tree <- shinyTree::get_selected(input$tree, format = "names")
    if(length(selected_tree) > 0) {
    selected_data <- do.call(
      "rbind",
      # perform apply function for every list entrie of shinyTree list
      # to transform list entries into desired form of event/event group
      lapply(selected_tree, function(selected_tree_row) {
        # use attributes to decide if a selected row is an event or event_group
        # if length of attributes 'ancestry'
        if(length(attributes(selected_tree_row)$ancestry) == 2){
          data.frame("event_group" = attributes(selected_tree_row)$ancestry[2], "event" = selected_tree_row[1])
        }
      })
    )

    prepared_data <- megaplot_prepared_data()
    # filtered_data <- prepared_data  %>%
    #   dplyr::filter(event %in% selected.events) %>%
    #   dplyr::filter(event != "NA" & event_group != "NA" & !is.na(event_group) & !is.na(event))
    filtered_data <- prepared_data %>%
      dplyr::right_join(selected_data, by = c("event","event_group"))

    filtered_data <- filtered_data %>%
      dplyr::distinct()

    filtered_data <- filtered_data %>%
      dplyr::select(-event_color,-jittered) %>%
      dplyr::left_join(
        color_data$all %>%
          dplyr::select(event,event_group,event_color,jittered),
        by=c("event","event_group")
      )

    filtered_data_w_jitter <- filtered_data  %>% dplyr::ungroup() %>%
      dplyr::select(event, event_group, max_event_id, event_group_id, event_id,jittered) %>%
      dplyr::distinct() %>%
      dplyr::arrange(event_group_id, event_id) %>%
      dplyr::mutate(seq_nr = 1)

     if(nrow(filtered_data_w_jitter) > 1) {
      for(i in 2:nrow(filtered_data_w_jitter)){
         if (filtered_data_w_jitter$jittered[i]) {
           filtered_data_w_jitter$seq_nr[i] <- filtered_data_w_jitter$seq_nr[i-1]+1
         } else if (!filtered_data_w_jitter$jittered[i] & (filtered_data_w_jitter$event_group_id[i] == filtered_data_w_jitter$event_group_id[i-1])){
           filtered_data_w_jitter$seq_nr[i] <- filtered_data_w_jitter$seq_nr[i-1]
         } else if (!filtered_data_w_jitter$jittered[i] & (filtered_data_w_jitter$event_group_id[i] != filtered_data_w_jitter$event_group_id[i-1]))
           filtered_data_w_jitter$seq_nr[i] <- filtered_data_w_jitter$seq_nr[i-1]+1
       }
     } else {
       filtered_data_w_jitter$seq_nr <- 1
     }

    filtered_data_w_jitter <- filtered_data_w_jitter %>%
      dplyr::mutate(jitter_event_time = seq(-0.20,0.20,length = length(unique(filtered_data_w_jitter$seq_nr)))[seq_nr]) %>%
      dplyr::select(event, event_group, jitter_event_time)

    filtered_data <- filtered_data  %>% dplyr::ungroup() %>%
      dplyr::left_join(filtered_data_w_jitter, by = c("event","event_group")) %>%
      dplyr::mutate(subjectid_n_jittered = subjectid_n + jitter_event_time)

    filtered_data <- filtered_data %>% dplyr::left_join(
      filtered_data %>%
        dplyr::select(event,event_group) %>%
        distinct() %>%
        group_by(event) %>%
        dplyr::mutate(number_event_groups = n()) %>%
        dplyr::mutate(
          unique_event = case_when(
            number_event_groups == 1 ~ event,
            number_event_groups > 1 ~ paste0(event," (",event_group,")")
          )
        ) %>% dplyr::select(event,event_group, unique_event),
      by = c("event_group","event")
    )
    filtered_data
    } else {
      filtered_data <- NULL
    }
  })


  #### Mega Plot ####
  output$mega_plots <- plotly::renderPlotly({

    shiny::req(megaplot_prepared_data())

    draw_mega_plot(
      megaplot_prepared_data = megaplot_prepared_data(),
      megaplot_filtered_data = megaplot_filtered_data(),
      select_grouping = input$select_grouping,
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
      inputId = 'select_event_kaplan_meier',
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
      select_strata_var = input$select_strata_var
    )
  })

}
