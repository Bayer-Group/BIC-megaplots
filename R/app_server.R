#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  shinyjs::hideElement(id= "selectedEventsPanel")
  shinyjs::hideElement(id= "selected_events_color_container_panel")
  shinyjs::hideElement(id= "colourPickerPanel")

  observeEvent(uploaded_data$val, {
    if(is.null(uploaded_data$val)) {
      shinyjs::hideElement(id = "selectedEventsPanel")
    } else {
      shinyjs::showElement(id = "selectedEventsPanel")
    }
  })

  observeEvent(input$tree, {
    shinyTree::get_selected(input$tree,format="names")
    if(is.null(input$tree)) {
      shinyjs::hideElement(id = "selected_events_color_container_panel")
      shinyjs::hideElement(id = "colourPickerPanel")
    } else {
      if (length(shinyTree::get_selected(input$tree,format="names"))>0) {
        shinyjs::showElement(id = "selected_events_color_container_panel")
        shinyjs::showElement(id = "colourPickerPanel")
      } else {
        shinyjs::hideElement(id = "selected_events_color_container_panel")
        shinyjs::hideElement(id = "colourPickerPanel")
      }
    }
  })

  shiny::observe({
    colourpicker::updateColourInput(
      session,
      inputId="placeholder",
      label = ifelse(is.na(color_data$selected[input$jsColNum,"event"]),color_data$selected[input$jsColNum,"event_group"],color_data$selected[input$jsColNum,"event"]),
      value = color_data$selected[input$jsColNum,"event_color"]
    )
  })


  shiny::observeEvent(input$placeholder, {
    shiny::req(input$jsColNum)
    if(!is.null(color_data$all)){

    if(!is.na(color_data$selected[input$jsColNum,c("event")])){
      color_data$all[color_data$all$event_group == color_data$selected[input$jsColNum,c("event_group")] & color_data$all$event == color_data$selected[input$jsColNum,c("event")] & !is.na(color_data$all$event == color_data$selected[input$jsColNum,c("event")]),]$event_color <- input$placeholder
    }
    if (is.na(color_data$selected[input$jsColNum,c("event")])) {

      color_func2 <- function(x,y,z, col = input$placeholder) {
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

  color_data <- reactiveValues(
    all = NULL,
    selected = NULL
  )

    shiny::observe({
      megaplot_data_raw <- shiny::req(uploaded_data$val)

      megaplot_data_raw <- megaplot_data_raw %>% dplyr::select(event_group,event)%>% dplyr::distinct()

      megaplot_data_splitted_by_event_group <- split(megaplot_data_raw, megaplot_data_raw$event_group)

      for(i in 1:length(megaplot_data_splitted_by_event_group)) {
        megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
          dplyr::group_by(event) %>%
          dplyr::mutate(
            event_id = dplyr::cur_group_id(),
            event_group_id = i
          )
        megaplot_data_splitted_by_event_group[[i]] <- megaplot_data_splitted_by_event_group[[i]] %>%
          dplyr::mutate(
            max_event_id = max(megaplot_data_splitted_by_event_group[[i]]$event_id)
          )
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
      color_func <- function(x,y,z) {
        if (x != 0 & z != 1) {
          return_colors <- colorRampPalette(
            c(colorRampPalette(c("white",megaplot_color[y]))(100)[50],
              megaplot_color[y],
              colorRampPalette(c(megaplot_color[y],"black"))(100)[50]
            )
          ) (z)[x]
        }
        if(x == 0 | z == 1) {
         return_colors <- megaplot_color[y]
        }
        return(return_colors)
      }

      #bind all list entries back to one data.frame
      megaplot_data_w_event_ids <- do.call("rbind",megaplot_data_splitted_by_event_group)

      #add variables event_color and jitter_event_time to event column
      megaplot_event_data_w_color <- megaplot_data_w_event_ids  %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          event_color = color_func(event_id, event_group_id, max_event_id)
        )

      color_data$all <- megaplot_event_data_w_color
    })



    output$selected_events_color_container <- renderUI({
      shiny::req(input$tree)

      selected_tree <- shinyTree::get_selected(input$tree,format="names")
      if (length(selected_tree) > 0) {
        selected_data <- do.call("rbind",lapply(selected_tree,
         function(test){
           if(length(attributes(test)$ancestry) == 2){
             data.frame("event_group" = attributes(test)$ancestry[2],"event" = test[1])
           } else {
             data.frame("event_group" = test[1],"event" = NA)
           }
         })) %>% dplyr::mutate(
           selected = 1
         )


        sort_id <- as.numeric(unlist(lapply(shinyTree::get_selected(input$tree, format="classid"), function(x){attributes(x)$id})))
        selected_data <- cbind(selected_data,sort_id) %>% dplyr::arrange(sort_id)

        selected_data <- selected_data %>%
          dplyr::left_join(color_data$all %>%
                             dplyr::select(event_group, event, event_color),by = dplyr::join_by(event_group,event))%>%
          dplyr::mutate(names_for_color_list = ifelse(is.na(event), event_group, event), type_for_color = ifelse(is.na(event), "event_group", "event"))

        color_data$selected <- selected_data


      lapply(seq_along(selected_data$names_for_color_list), function(colNum) {
        div_class <- "col col-transparent-box selected"
        div(
          class = div_class,
          div(
            style = paste0(
                        "background:",
                        selected_data[colNum,]$event_color,";",
                        "padding:",
                        ifelse(selected_data[colNum,]$type == "event", "0px 0px 0px 0px","0px 0px 0px 50px" ),
                        ";",
                        "color: ",font_color(selected_data[colNum,]$event_color),";"
                      ),
            class = "selected-col-inner",
            `data-colnum` = colNum,
            selected_data[colNum,]$names_for_color_list
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
    all_colors <- color_data$all

    prepare_megaplot_data(
      megaplot_data = shiny::req(uploaded_data$val),
      event_colors = all_colors,
      grouping_vars = input$select.grouping,
      sorting_var = input$select_sorting
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
        tmp_list[[j]] <-structure(j, sticon="")
      }
      event_list[[i]] <-  structure(tmp_list, stopened = FALSE, sticon="")
    }
    names(event_list) <- unique_event_groups

    event_list <- structure(event_list)
    event_list <- list( "Select all event(s)" = structure(event_list, sticon = ""))
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
