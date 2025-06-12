#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#'
#' @importFrom dplyr arrange select group_by count filter n_distinct mutate ungroup row_number all_of
#' @importFrom plyr ddply summarize .
#' @importFrom DT renderDT datatable renderDataTable formatRound DTOutput
#' @importFrom graphics axis grconvertX grconvertY legend par points rect strheight strwidth text mtext
#' @importFrom reshape2 melt dcast
#' @importFrom rlang syms :=
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyjs runjs click disable enable
#' @import shinyWidgets
#' @importFrom stats aggregate cutree
#' @importFrom stringr str_wrap
#' @importFrom utils tail read.csv
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @noRd
#' @keywords internal

app_server <- function(input, output, session) {

  # initialize values with NULL
  A <- B <- megaplots_demo_data <- EVENT <- Group_ID <- LEVEL <- megaplots_selected_event_time <- subject <- megaplots_selected_subjectid <- NULL
  megaplots_selected_start_time <- megaplots_selected_end_time <- 'GROUP BY' <- NULL

  ns <- session$ns

  # set maximu‚m data upload size to 750MB
  options(shiny.maxRequestSize = 750 * 1024^2)

  # #### DATA UPLOAD ####
  # # disable/enable import button if data are missing/available
  shiny::observe({
    shinyjs::disable(id = 'data_upload-import.button', selector = NULL)
    df <- shiny::req(uploaded_files$preprocess_data()$megaplot_data)
    #if uploaded data are available and preprocessed enable import button
    if (!is.null(df)) { shinyjs::enable(id = 'data_upload-import.button', selector = NULL) }
  })

  #### PROCESSING DATA ####
  # 0. 'preprocessed_data' = standardized object in which the uploaded data are
  # preprocessed to get desired structure for data used in megaplots (within module data_upload_server)
  # can be called via 'uploaded_files$preprocess_data()'

  #server part of module data upload - save options as uploaded_files
  uploaded_files <- shiny::callModule(
    data_upload_server,
    "data_upload"
  )

  #update tab to megaplot tab if import button is clicked
  observeEvent(uploaded_files$import.button(), {
    newtab <- switch(input$sidebarmenu, "dashboard")
    shinydashboard::updateTabItems(session, "sidebarmenu", newtab)
  })


  # 1. calculate aggregated stats for sequencing and clustering after clicking import button
  summary_statistics_data <-shiny::eventReactive(uploaded_files$import.button(), {
    # function to summarise_megaplot_data and save results as list object with
    # entries 'total' and 'detail'
    summarise_megaplot_data(data = uploaded_files$preprocess_data())
  })


  # 2. 'data_w_event_and_group_information' = standardized object in which the original data is stored
  #           together with information on the grouping and event variables
  #           (will not be reactively modified in the app and can be used to
  #           create some of the UI input fields)

  data_w_event_and_group_information <- shiny::eventReactive(
    c(summary_statistics_data(),uploaded_files$select.ev.lev1(),uploaded_files$select.ev.lev2(),uploaded_files$select.ev.lev3(),uploaded_files$select.ev.lev4()),{

    # uploaded_files$select.ev.lev1() uploaded_data$select.ev.lev1
    shiny::req(uploaded_files$preprocess_data())
   # shiny::req(c(uploaded_files$select.ev.lev1(),uploaded_files$select.ev.lev2(),uploaded_files$select.ev.lev3(),uploaded_files$select.ev.lev4()))

    #add grouping and event information to preprocessed data
    add_event_and_group_information(
      data = uploaded_files$preprocess_data(),
      summary_stats = summary_statistics_data(),
      event1 = uploaded_files$select.ev.lev1(),
      event2 = uploaded_files$select.ev.lev2(),
      event3 = uploaded_files$select.ev.lev3(),
      event4 = uploaded_files$select.ev.lev4(),
      updated_event1 = update_select.ev1(),
      updated_event2 = update_select.ev2(),
      updated_event3 = update_select.ev3(),
      updated_event4 = update_select.ev4(),
      data_selection = uploaded_files$selectdata()
    )
  })


  #re-calculate length of legend if data are updated and save length to reactive object max_legend_char()
  shiny::observeEvent(data_w_event_and_group_information(), {
    event_levels <- sapply(data_w_event_and_group_information()$event.lev, function(x) paste(x, collapse = ""))
    number_levels <- unlist(lapply(data_w_event_and_group_information()$event.lev, length))
    x <- unlist(lapply(number_levels, function(x) paste(rep("_", x), collapse = "")))
    event_levels <- paste(event_levels, x)
    max_length_event_levels <- event_levels[which.max(nchar(event_levels))]
    cex.leg <- 1.2
    x <- strwidth(max_length_event_levels, cex = cex.leg * 1.2, units = "inches") *96
    max_legend_char(x)
  })

  # update sequencing pickerinput
  shiny::observeEvent(data_w_event_and_group_information(), {
    choices <- data_w_event_and_group_information()$event
    selected <- data_w_event_and_group_information()$event[1]

    shinyWidgets::updatePickerInput(
      session,
      inputId = "varSeq",
      choices = choices,
      selected = selected
    )
  })


  ## AI FUNCTIONALITY ##

  # 3. 'data_w_ai_information' = copy from 'data_w_event_and_group_information'
  # that will be reactively modified based on the AI selections
  # from the user
  data_w_ai_information <- shiny::eventReactive(c(data_w_event_and_group_information(),input$seq.button), { # aiButton$seq), {
    data_w_ai_information <- shiny::req(data_w_event_and_group_information())
    if (!is.null(collectSeq$varSeq)) {
      # Call the sequencing function with all the corresponding parameters:
      data_w_ai_information <- sequencing_var_app(
        data_w_ai_information,
        collectSeq$varSeq,
        input_seriation(),
        collectSeq$methSer,
        group = main_settings$select.grouping(),
        multiple_distmeasures = input$multiple_distmeasures
      )
    }
    data_w_ai_information
  })


  # 4. 'data_grouped_and_sorted' = copy from 'data_w_ai_information' that will be reactively modified
  #           in the sorting/grouping part based on user input (preparation for the plotting)
  #
  data_grouped_and_sorted <- shiny::reactive({
    displayed_subjects$selection_button()
    displayed_subjects$subset.button()

    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   if (upload_indicator$dat == 1) {
    #     shinyjs::click("apply.color")
    #     upload_indicator$dat <- 2
    #   }
    # }

    add_sorting_information(
      data_frame = data_w_ai_information(),
      select_subsetting = main_settings$select.subsetting(),
      randet = displayed_subjects$selection_button(),
      seed =displayed_subjects$seedset(),
      nshow =displayed_subjects$random(),
      start =displayed_subjects$startsubj(),
      random = displayed_subjects$random(),
      specific_ids = input$specific_ids,
      event_levels = main_settings$event.levels(),
      select_sorting = main_settings$select.sorting(),
      select_grouping = main_settings$select.grouping(),
      select_events = main_settings$select.events()
    )
  })

  #initialize reactive value upload_indicator$dat
  # upload_indicator <- shiny::reactiveValues(dat = 1)


  # create reactive plotting object
  # 5. 'data_w_plot_info' = add plot information to data set (zoom/range and colors)

  data_w_plot_info <- shiny::reactive({
    # session$clientData[["image1"]]
    #requirements
    shiny::req(data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())

    # create plotting data based on user selections
    data_w_plot_info <- data_grouped_and_sorted()
    data_w_event_and_group_information <- data_w_event_and_group_information()
    # constrain to selected x-range
    data_w_plot_info$A$megaplots_selected_start_time <- pmax(data_w_plot_info$A$megaplots_selected_start_time, settings$range()[1])
    data_w_plot_info$A$megaplots_selected_end_time <-
      pmin(pmax(data_w_plot_info$A$megaplots_selected_end_time, settings$range()[1]), settings$range()[2])
    data_w_plot_info$B <-
      subset(data_w_plot_info$B, megaplots_selected_event_time >= settings$range()[1] & megaplots_selected_event_time <= settings$range()[2])

    # set plotting colors
    col.ev <- list()
    type.ev <- list()

    if (!is.na(data_w_event_and_group_information$event[1])) {
      col.ev[[1]] <- color_pal1$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[1]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      col.ev[[2]] <- color_pal2$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[2]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      col.ev[[3]] <- color_pal3$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[3]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      col.ev[[4]] <- color_pal4$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[4]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[1])) {
      type.ev[[1]] <- color_pal1$val[length(color_pal1$val)]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      type.ev[[2]] <- color_pal2$val[length(color_pal2$val)]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      type.ev[[3]] <- color_pal3$val[length(color_pal3$val)]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      type.ev[[4]] <- color_pal4$val[length(color_pal4$val)]
    }

    if (length(col.ev)>0) {
      for (i in 1:length(data_w_event_and_group_information$event)) {
        names(col.ev[[i]]) <- data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[i]]]
      }
      names(col.ev) <- data_w_event_and_group_information$event
      names(type.ev) <- data_w_event_and_group_information$event

      data_w_plot_info$col.ev <- col.ev
      data_w_plot_info$type.ev <- type.ev
    }
    data_w_plot_info
  })

  # global plot settings
  plot_par_settings <- shiny::reactive({
    shiny::req(data_w_plot_info(), data_w_event_and_group_information(), data_grouped_and_sorted())
    mar2 <- 5
    # create group labels
    grLab <- NULL
    mar4 <- 1
    if ((data_w_plot_info()$group[1] != 'NULL')) {
      mar4 <- 12
      tmp <-strsplit(levels(droplevels(data_w_plot_info()$A$Group_ID_char)), split = '::')
      grLab <- data.frame(
        'LABEL' = sapply(tmp,FUN = function(x) {paste(paste0(data_w_plot_info()$group,'=',stringr::str_wrap(rev(x), width = 25, exdent = 5)), collapse = '\n')}),
        'POS' = plyr::ddply(data_w_plot_info()$A,plyr::.(Group_ID), plyr::summarize,'POS' = mean(subject))$POS,
        stringsAsFactors = FALSE
      )
    }
    list('mar' = c(0, mar2, 0, mar4), 'grLab' = grLab)
  })

  #### PLOTTING ####
  # Plot Megaplots
  output$image1 <- shiny::renderPlot({
    #reactivity
    shiny::req(settings$height_slider())
    #session$clientData[["image1"]]
    session$clientData$output_image1_width
    # main_settings$event.levels()
    # main_settings$select.grouping()
    #requirements
    shiny::req(data_w_plot_info(), data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())
    draw_megaplot(
      megaplot_data = data_w_plot_info(),
      select_color = select.col(),
      par_settings = plot_par_settings(),
      background_stripes = settings$background_stripes(),
      background_stripes_length = settings$background.stripes.length(),
      event_levels = main_settings$event.levels(),
      xlim = c(settings$range()[1], settings$range()[2]),
      ylim = range(data_w_plot_info()$A$subject) + c(-1.5, 1.5),
      lines_instead_symbols = settings$lines_instead_symbols(),
      lines_options = settings$lines_options(),
      line_width = settings$thick(),
      y_axis_label = settings$y_axis_label(),
      reference_line_1 = settings$reference_line_1(),
      reference_line_1_value = settings$reference_line_1_value(),
      reference_line_2 = settings$reference_line_2(),
      reference_line_2_value = settings$reference_line_2_value(),
      reference_line_3 = settings$reference_line_3(),
      reference_line_3_value = settings$reference_line_3_value(),
      select_events = main_settings$select.events(),
      color_subject_line_by_first_event = settings$inc.ev.subj()
    )
  },
  # function for UI auto height resizing
  height = function() {
    # calculate height based on selected subject number
    (max(c(1, data_grouped_and_sorted())$A$'subject') * 12) * settings$height_slider()
  })


  #### Plot Legend ####
  output$image1Legend <- shiny::renderPlot({
    #requirements
    shiny::req(max_legend_char())
    shiny::req(data_w_plot_info(), data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())

    draw_megaplot_legend(
      megaplot_data = data_w_plot_info(),
      select_color = select.col()
    )
  })


  #### Plot x-axis ####
  output$image1Axis <- shiny::renderPlot({
    # reactivity
    settings$x_axis_label()

    #requirements
    shiny::req(settings$range())

    draw_megaplot_x_axis(
      range = settings$range(),
      select_color = select.col(),
      megaplot_axis_width = session$clientData$output_image1Axis_width,
      megaplot_width = shiny::req(session$clientData$output_image1_width),
      plot_par_settings = plot_par_settings(),
      axis_ticks = settings$det.xaxt(),
      x_axis_label = settings$x_axis_label(),
      reference_line_1 = settings$reference_line_1(),
      reference_line_1_value = settings$reference_line_1_value(),
      reference_line_2 = settings$reference_line_2(),
      reference_line_2_value = settings$reference_line_2_value(),
      reference_line_3 = settings$reference_line_3(),
      reference_line_3_value = settings$reference_line_3_value()
    )
  })


  # box with main plot
  # create reactive megaplot image with dynamic height adjustment as UI output
  output$megaplot <- shiny::renderUI({
      shiny::plotOutput(
        outputId = 'image1',
        dblclick = shiny::clickOpts(id = "dblclick_scatter"),
        brush = shiny::brushOpts(
          id = "image1_brush",
          fill = "#ffffff",
          stroke = "#036",
          opacity = 0.25,
          delay = 300
        ),
        hover = shiny::hoverOpts(
          "image1_hover",
          delay = 300,
          delayType = "debounce"
        ),
        height = 'auto'
      )
  })

  #### Hover/Brush Functionality for Zoom Panel
  brush_coord <- shiny::reactiveValues(
    x = NULL,
    y = NULL
  )

  shiny::observeEvent(input$image1_brush, {
    brush_coord$x <- c(input$image1_brush$xmin, input$image1_brush$xmax)
    brush_coord$y <-
      c(input$image1_brush$ymin, input$image1_brush$ymax)
  })

  shiny::observeEvent(input$dblclick_scatter, {
    brush_coord$x <- NULL
    brush_coord$y <- NULL
  })


  # Create a logical value output "check_slider_used", used in the conditional panel in the user interface.
  # If the Zoom slider is used, two buttons on the top right side of the app appear, which can be used to
  # move forward the x-axis by the range of the zoom slider
  # (e.g. zoom slider is set from 0 to 30 and the user click on the 'next/forward' button, the slider
  # updates to 30 to 60).

  output$check_slider_used <- shiny::reactive({
    shiny::req(data_w_event_and_group_information(), settings$range())
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    rangemin1 <- settings$range()[1]
    rangemax1 <- settings$range()[2]

    if (min1 != rangemin1 | max1 != rangemax1) {
      tmp <- TRUE
    } else {
      tmp <- FALSE
    }
    tmp
  })

  shiny::outputOptions(output, "check_slider_used", suspendWhenHidden = FALSE)

  output$next_buttons <- shiny::renderUI({
    shiny::absolutePanel(
      id = "next_buttons",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #222d32'>"
      )),
      top = 300,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = 60,
      height = "auto",
      shiny::fluidRow(
        shinyWidgets::circleButton(
          inputId = "btn1",
          icon = icon("step-backward"),
          status = "default",
          size = "xs"
        ),
        shinyWidgets::circleButton(
          inputId = "btn2",
          icon = icon("step-forward"),
          status = "default",
          size = "xs"
        )
      ),
      style = "z-index: 10;"
    )
  })

  #### Summary Panel ####
  output$summarypanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "summarypanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: #222d32'>"
        )
      ),
      HTML(
        '
        <button style =
        "background: #0091DF;
        color:#ffffff",
        data-toggle="collapse" data-target="#demo2" style="color:white;">
        <i class="fa-solid fa-search-plus"></i> Open/Close Summary Panel</button>'
      ),
      top = 120,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      shiny::tags$div(
        id = 'demo2',
        class = "collapse",
          HTML(
            summary_statistics_text$val
          )
      ),
      style = "z-index: 10;"
    )
  })

  #### Hover Panel ####
  # Hover plot output$hover
  output$hover <- shiny::renderPlot({
    shiny::req(data_w_plot_info())
    color_bg <- select.col()
    data_w_plot_info <- data_w_plot_info()

    if (!is.null(brush_coord$x) & !is.null(brush_coord$y)) {

      draw_megaplot(
        megaplot_data = data_w_plot_info(),
        select_color = select.col(),
        par_settings = plot_par_settings(),
        background_stripes = settings$background_stripes(),
        background_stripes_length = settings$background.stripes.length(),
        event_levels = main_settings$event.levels(),
        xlim = brush_coord$x,
        ylim = brush_coord$y + c(-1.5, 1.5),
        lines_instead_symbols = settings$lines_instead_symbols(),
        lines_options = settings$lines_options(),
        line_width = settings$thick(),
        y_axis_label = settings$y_axis_label(),
        reference_line_1 = settings$reference_line_1(),
        reference_line_1_value = settings$reference_line_1_value(),
        reference_line_2 = settings$reference_line_2(),
        reference_line_2_value = settings$reference_line_2_value(),
        reference_line_3 = settings$reference_line_3(),
        reference_line_3_value = settings$reference_line_3_value(),
        select_events = main_settings$select.events(),
        color_subject_line_by_first_event = settings$inc.ev.subj()
      )

    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = select.col()['plot.bg'],
        xpd = TRUE
      )
      text(0.5, 0.6, "Please use mouse brush on plot", col = select.col()['plot.id'])
      text(0.5, 0.5, "to get an enlarged version of an area!", col = select.col()['plot.id'])
      text(0.5,
           0.4,
           "(Brushing is clicking and dragging a selection box. ",
           col = select.col()['plot.id'])
      text(0.5,
           0.3,
           "Remove selection box with doubleclick.)",
           col = select.col()['plot.id'])
    }
  }, width = 400)


  #hover plot absolute panel
  output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #222d32'>"
      )),
      HTML('<button style ="background: #0091DF;color:#ffffff",
        data-toggle="collapse" data-target="#demo" style="color:white;">
        <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'
      ),
      top = 80,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      shiny::tags$div(
        id = 'demo',
        class = "collapse",
        shiny::plotOutput('hover')
      ),
      style = "z-index: 10;"
    )
  })

  max_legend_char <- shiny::reactiveVal({270})

  # re-create panels after reset panel click to display them when hidden
  shiny::observeEvent(settings$reset_draggable_panel_positions(), {
   output$hoverpanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = "hoverpanel",
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: #222d32'>"
        )),
        HTML(
          '
          <button style =
          "background: #0091DF;
          color:#ffffff",
          data-toggle="collapse" data-target="#demo" style="color:white;">
          <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'
        ),
        top = 80,
        left = "auto",
        right = 100,
        bottom = "auto",
        width = 400,
        height = "auto",
        shiny::tags$div(
          id = 'demo',
          class = "collapse",
          shiny::fluidRow(shiny::column(2,
                                        shiny::plotOutput('hover')))
        ),
        style = "z-index: 10;"
      )
    })

    output$summarypanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = "summarypanel",
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
          HTML(paste0(
            "<div style='background-color: #222d32'>"
          )
        ),
        HTML('<button style = "background: #0091DF; color:#ffffff",
          data-toggle="collapse" data-target="#demo2" style="color:white;">
          <i class="fa-solid fa-search-plus"></i> Open/Close Summary Panel</button>'
        ),
        top = 120,
        left = "auto",
        right = 100,
        bottom = "auto",
        width = 400,
        height = "auto",
        shiny::tags$div(
          id = 'demo2',
          class = "collapse",
            HTML(
              summary_statistics_text$val
            )
        ),
        style = "z-index: 10;"
      )
    })

    output$hover_legend <- shiny::renderUI({
      # leg.height <- paste0(40*length(data_w_plot_info()$event),'px')
      # if (input.fontsize$fontsize) {
        leg.height <- paste0(60*length(data_w_plot_info()$event),'px')
      # }
      shiny::absolutePanel(
        id = "hover_legend",
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: #404A4E'>"
        )),
        top = 333,
        left = "auto",
        right = 75,
        bottom = "auto",
        width = max_legend_char(),
        height = "auto",
        shiny::fluidRow(
          shiny::plotOutput('image1Legend', height = leg.height, width = max_legend_char())
        ),
        style = "z-index: 10;"
      )
    })
  })


  output$hover_legend <- shiny::renderUI({
    # leg.height <- paste0(40*length(data_w_plot_info()$event),'px')
    # if (input.fontsize$fontsize) {
      leg.height <- paste0(60*length(data_w_plot_info()$event),'px')
    # }
    shiny::absolutePanel(
      id = "hover_legend",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #404A4E'>"
      )),
      top = 333,
      left = "auto",
      right = 75,
      bottom = "auto",
      width = max_legend_char(),
      height = "auto",
      shiny::fluidRow(
        shiny::plotOutput('image1Legend', height = leg.height, width = max_legend_char())
      ),
      style = "z-index: 10;"
    )
  })

  # box with axis
  output$axisbox <- shiny::renderUI({
    shiny::plotOutput(
      outputId = 'image1Axis',
      height = '40px'
    )
  })

  output$select.raw <- shiny::renderUI({
    choices <- names(req(uploaded_files$preprocess_data()$megaplot_data))[c(1, 2)]
    shinyWidgets::pickerInput(
      inputId = 'select.raw',
      label = 'Select data set based on main options:',
      choices = choices,
      width = 'fit',
      multiple = FALSE,
      selected = choices[1],
      options = list(`style` = 'background: btn-primary')
    )
  })



  #### REACTIVES & OBSERVERS ####

  loaded_sel_pal1 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal1, uploaded_files$import.button()), {
    loaded_sel_pal1$dat <- !is.null(uploaded_files$select.ev1())
  })

  output$load_sel_pal1 <- shiny::reactive(loaded_sel_pal1$dat)

  shiny::outputOptions(output, "load_sel_pal1", suspendWhenHidden = FALSE)

  loaded_sel_pal2 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal2, uploaded_files$import.button()), {
    loaded_sel_pal2$dat <- !is.null(uploaded_files$select.ev2())
  })

  output$load_sel_pal2 <- shiny::reactive(loaded_sel_pal2$dat)

  shiny::outputOptions(output, "load_sel_pal2", suspendWhenHidden = FALSE)

  loaded_sel_pal3 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal3, uploaded_files$import.button()), {
    loaded_sel_pal3$dat <- !is.null(uploaded_files$select.ev3())
  })

  output$load_sel_pal3 <- shiny::reactive(loaded_sel_pal3$dat)
  shiny::outputOptions(output, "load_sel_pal3", suspendWhenHidden = FALSE)

  loaded_sel_pal4 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal4, uploaded_files$import.button()), {
    loaded_sel_pal4$dat <- !is.null(uploaded_files$select.ev4())
  })

  output$load_sel_pal4 <- shiny::reactive(loaded_sel_pal4$dat)
  shiny::outputOptions(output, "load_sel_pal4", suspendWhenHidden = FALSE)



  # connect to submit button (in UI)
  update_select.ev1 <- shiny::eventReactive(c(uploaded_files$import.button(), uploaded_files$preprocess_data()$megaplot_data), {
      uploaded_files$select.ev1()
  })
  update_select.ev2 <- shiny::eventReactive(c(uploaded_files$import.button(), uploaded_files$preprocess_data()$megaplot_data), {
      uploaded_files$select.ev2()
  })
  update_select.ev3 <- shiny::eventReactive(c(uploaded_files$import.button(), uploaded_files$preprocess_data()$megaplot_data), {
      uploaded_files$select.ev3()
  })
  update_select.ev4 <- shiny::eventReactive(c(uploaded_files$import.button(), uploaded_files$preprocess_data()$megaplot_data), {
      uploaded_files$select.ev4()
  })


  # set color theme
  coltheme <- shiny::reactiveValues(col_sel = 'grey (app version)')

  shiny::observeEvent(input$select.col, {
    coltheme$col_sel <- input$select.col
  })

  select.col <- shiny::reactive({
    if (coltheme$col_sel == 'grey (app version)') {
      col_sel <- c(
        'plot.bg' = '#404A4E',
        'plot.lines' = '#000000',
        'plot.wp' = '#404A4E',
        'plot.id' = 'white',
        'axleg.bg' = '#222d32',
        'cont.bg' = '#222d32'
      )
    } else if (coltheme$col_sel == 'white (print version)') {
      col_sel <- c(
        'plot.bg' = '#dce4e8',
        'plot.lines' = '#404A4E',
        'plot.wp' = '#dce4e8',
        'plot.id' = 'black',
        'axleg.bg' = '#dce4e8',
        'cont.bg' = '#dce4e8'
      )
    } else {
      col_sel <- "red"
    }
    col_sel
  })



  # shiny::observeEvent(input$distmeasure, {
  #   shiny::updateTabsetPanel(inputId = "Change_input_for_seq_metod", selected = input$distmeasure)
  # })


  collectSeq <- shiny::reactiveValues(
    varSeq = NULL,
    methSer = NULL
  )

  shiny::observeEvent(input$seq.button, {
    collectSeq$varSeq <- input$varSeq
    collectSeq$methSer <- input$methSer
  })

  # create raw data table as UI output (based on data_grouped_and_sorted() user selection)
  rawData <- shiny::reactive({
    tmp <- shiny::req(data_w_ai_information())
    tmp[[req(input$select.raw)]]
  })

  output$rawtable = DT::renderDT({
    rd <- shiny::req(rawData())
    rd <-
      rd[, setdiff(colnames(rd), c('Group_ID', 'Group_ID_char', 'subject'))]
    rd <- DT::datatable(rd, rownames = FALSE)
    rd
  })

  shiny::observeEvent(input$btn1, {
    start_val1 <- settings$range()[1]
    start_val2 <- settings$range()[2]
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    diff_val <- start_val2 - start_val1

    if (start_val1 > min1) {
      if ((start_val1 - diff_val) < min1) {
        shiny::updateSliderInput(session,
                                 inputId = "range",
                                 value = c(min1, min1 + diff_val))
      } else {
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = c(start_val1, start_val2) - diff_val
        )
      }
    }
  })

  shiny::observeEvent(input$btn2, {
    start_val1 <- settings$range()[1]
    start_val2 <- settings$range()[2]
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    diff_val <- start_val2 - start_val1

    if (start_val2 < max1) {
      if ((start_val2 + diff_val) > max1) {
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = c(max1 - diff_val, max1 + diff_val)
        )
      } else {
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = c(start_val1, start_val2) + diff_val
        )
      }
    }
  })

  # SUMMARY STATS
  output$sumtable <- DT::renderDataTable({
    shiny::req(data_grouped_and_sorted())
    mel.sum <- summary_statistics$val

    if (!is.null(mel.sum)) {
      if (nrow(mel.sum) > 0) {
        mel.sum <- DT::datatable(mel.sum, rownames = FALSE, filter = 'top')
        mel.sum <- DT::formatRound(mel.sum, columns = 'MEAN COUNT PER SUBJECT', digits = 1)
        mel.sum
      }
    }
  })

 # observer for creating a summary table
  shiny::observeEvent(c(data_grouped_and_sorted(),uploaded_files$import.button()), {


    ds_ <- data_grouped_and_sorted()$B[, c('megaplots_selected_subjectid', 'megaplots_selected_event_time', data_grouped_and_sorted()$event.total, "Group_ID_char")] %>%
      dplyr::mutate_if(is.factor, as.character)
    mel <- reshape2::melt(
      ds_,
      id.vars = c('megaplots_selected_subjectid', 'megaplots_selected_event_time', 'Group_ID_char'),
      variable.name = 'EVENT',
      value.name = 'LEVEL'
    )

    #calculate number of rows/subjects in each group
    nr_subjects <-
      table(unique(mel[c("Group_ID_char", "megaplots_selected_subjectid")])$Group_ID_char)
    #calculate number of events in each group and for each event level
    if (nrow(mel) > 0 & !all(is.na(mel$LEVEL))) {
      mel.sum <- stats::aggregate(
        megaplots_selected_event_time ~ EVENT + LEVEL + Group_ID_char,
         data = mel,
         FUN = length
      )
      mel.sum$Number_events_per_group <- mel.sum$megaplots_selected_event_time
      #calculate event average per subject in each group and event level
      #(depending on number of subjects)
      if (length(nr_subjects) == 1) {
        mel.sum$megaplots_selected_event_time <- apply(mel.sum, 1, function(x) {
            as.numeric(x[4]) / nr_subjects
        })
      }
      if (length(nr_subjects) > 1) {
        mel.sum$megaplots_selected_event_time <-
          apply(mel.sum, 1, function(x) {
            as.numeric(x[4]) / nr_subjects[x[3]]
        })
      }
      #define a complete "grid" of variable combinations for merging and
      #replace the missing values with zero(s)
      complete.grid <- unique(merge(mel.sum[,c("EVENT","LEVEL")], unique(mel.sum$Group_ID_char)))
      colnames(complete.grid)[colnames(complete.grid) == 'y'] <-
        'Group_ID_char'
      mel.sum <- mel.sum %>%
        dplyr::right_join(complete.grid, by =c("EVENT","LEVEL","Group_ID_char")) %>%
        replace(is.na(.), 0)

      mel.sum <- mel.sum %>% dplyr::full_join(
        data.frame("Group_ID_char" = names(nr_subjects),"NUMBER" = as.numeric(nr_subjects)),
      by = "Group_ID_char")

      #renaming for datatable in summary tab
      colnames(mel.sum)[colnames(mel.sum) == 'megaplots_selected_event_time'] <-
        'MEAN COUNT PER SUBJECT'
      colnames(mel.sum)[colnames(mel.sum) == 'Number_events_per_group'] <-
        'NUMBER EVENTS BY GROUP'
      colnames(mel.sum)[colnames(mel.sum) == 'Group_ID_char'] <-
        'GROUP BY'
      roundID <- which(colnames(mel.sum) == 'MEAN COUNT PER SUBJECT')
      summary_statistics$val <- mel.sum
    } else {
      summary_statistics$val <- NULL
    }
  })

  #reactive value for the summary statistic data table (default NULL)
  summary_statistics <- shiny::reactiveValues(val = NULL)
  summary_statistics_text <- shiny::reactiveValues(val = NULL)

  #observer to create the summary statistics text for "Summary Panel"
  shiny::observe({

    shiny::req(summary_statistics$val)
    tmp <- summary_statistics$val
    if (!is.null(tmp) & !is.null(main_settings$select.events()) & !is.null(main_settings$event.levels())) {

      #filter data and colors for selected events & selected levels
      tmp <- tmp[tmp$EVENT %in% main_settings$select.events(),]

      data_w_plot_info_col <- data_w_plot_info()$col.ev[names(data_w_plot_info()$col.ev) %in% main_settings$select.events()]

      summary_color <- unlist(data_w_plot_info_col)[names(unlist(data_w_plot_info_col)) %in% gsub(" = ", ".",main_settings$event.levels())]

      #filter for selected levels
      tmp$filter <- paste0(tmp$EVENT," = ", tmp$LEVEL)
      tmp <- tmp[tmp$filter %in% main_settings$event.levels(),]

      tmp <- tmp %>%
        dplyr::arrange(`GROUP BY`,match(filter, main_settings$event.levels()))
      if (!is.null(tmp)) {
        text <- c()

        if (all(tmp$`GROUP BY` == "")) {
          group_index <- ""
        } else {
          group_index <- rev(levels(data_w_plot_info()$B$Group_ID_char))
        }
        #generate event summary text for each group and each selected
        #event and event level
        for(group in group_index) {
          text <- c(
            text,
            paste0(
              "<u>",
              ifelse(group != "", group, "Overall"),
              " (N = ", unique(tmp[tmp$`GROUP BY` == group,]$NUMBER) ,")",
              "</u>",
              paste("<p> <mark style = 'color: white; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black; background:",summary_color,";'>",
                tmp[tmp$`GROUP BY` == group,]$LEVEL,
                "</mark>  Events = ",
                tmp[tmp$`GROUP BY` == group,]$`NUMBER EVENTS BY GROUP`,
                " (Avg. by row: ",
                round(tmp[tmp$`GROUP BY` == group,]$`MEAN COUNT PER SUBJECT`, 2),
                ")",
                "</p>",
                collapse = ""
              )
            )
          )
        }
        summary_statistics_text$val <- text
      }

    } else {
      summary_statistics_text$val <- NULL
    }
  })

  #### COLOR MODULES ####
  # color module 1
  color_pal1 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(uploaded_files$import.button()), {
    shiny::req(uploaded_files$import.button())
    if (!is.null(uploaded_files$select.ev.lev1())) {
      if (uploaded_files$selectdata()== "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)
          custom_colour <- mod_colour_palette_server(
            "color_palette1",
            event = shiny::reactive({
              uploaded_files$select.ev1()
            }),
            level = shiny::reactive({
              uploaded_files$select.ev.lev1()
            }),
            colors = shiny::reactive({
              saved_file$color.pal1
            })
          )

          shiny::observe({
            color_pal1$val <- custom_colour$colors()
          })
        }
       } else if (uploaded_files$selectdata()== "Use demo data") {
      custom_colour <- mod_colour_palette_server(
          "color_palette1",
          event = shiny::reactive({
            uploaded_files$select.ev1()
          }),
          level = shiny::reactive({
            uploaded_files$select.ev.lev1()
          }),
          colors = shiny::reactive({c("seagreen1", "#ffff99", "#ff7f00", "#5CACEE", "#FDBF6F", "#1F78B4", "#6A3D9A", "#FF7F00") })
        )

        shiny::observe({
          color_pal1$val <- custom_colour$colors()
        })
      } else {
        custom_colour <- mod_colour_palette_server(
          "color_palette1",
          event = shiny::reactive({
            uploaded_files$select.ev1()
          }),
          level = shiny::reactive({
            uploaded_files$select.ev.lev1()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 1"]]$col
          })
        )
        shiny::observe({
          color_pal1$val <- custom_colour$colors()
        })
      }
    }
  })

  # color module 2
  color_pal2 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(uploaded_files$import.button()), {
    if (!is.null(uploaded_files$select.ev.lev2())) {
      if (uploaded_files$selectdata()== "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)
          custom_colour2 <- mod_colour_palette_server(
            "color_palette2",
            event = shiny::reactive({
              uploaded_files$select.ev2()
            }),
            level = shiny::reactive({
              uploaded_files$select.ev.lev2()
            }),
            colors = shiny::reactive({
              saved_file$color.pal2
            })
          )
          observe({
            color_pal2$val <- custom_colour2$colors()
          })
        }
      } else {
        custom_colour2 <- mod_colour_palette_server(
          "color_palette2",
          event = shiny::reactive({
            uploaded_files$select.ev2()
          }),
          level = shiny::reactive({
            uploaded_files$select.ev.lev2()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 2"]]$col
          })
        )
        observe({
          color_pal2$val <- custom_colour2$colors()
        })
      }

    }
  })

  # color module 3
  color_pal3 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(uploaded_files$import.button()), {
    if (!is.null(uploaded_files$select.ev.lev3())) {
      if (uploaded_files$selectdata()== "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)

          custom_colour3 <- mod_colour_palette_server(
            "color_palette3",
            event = shiny::reactive({
              uploaded_files$select.ev3()
            }),
            level = shiny::reactive({
              uploaded_files$select.ev.lev3()
            }),
            colors = shiny::reactive({
              saved_file$color.pal3
            })
          )
          observe({
            color_pal3$val <- custom_colour3$colors()
          })
        }
      } else {
        custom_colour3 <- mod_colour_palette_server(
          "color_palette3",
          event = shiny::reactive({
            uploaded_files$select.ev3()
          }),
          level = shiny::reactive({
            uploaded_files$select.ev.lev3()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 3"]]$col
          })
        )
        observe({
          color_pal3$val <- custom_colour3$colors()
        })
      }

    }
  })


  # color module 4
  color_pal4 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(uploaded_files$import.button()), {
    if (!is.null(uploaded_files$select.ev.lev4())) {
      if (uploaded_files$selectdata()== "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)

          custom_colour4 <- mod_colour_palette_server(
            "color_palette4",
            event = shiny::reactive({
              uploaded_files$select.ev4()
            }),
            level = shiny::reactive({
              uploaded_files$select.ev.lev4()
            }),
            colors = shiny::reactive({
              saved_file$color.pal4
            })
          )
          observe({
            color_pal4$val <- custom_colour4$colors()
          })
        }
      } else {
        custom_colour4 <- mod_colour_palette_server(
          "color_palette4",
          event = shiny::reactive({
            uploaded_files$select.ev4()
          }),
          level = shiny::reactive({
            uploaded_files$select.ev.lev4()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 4"]]$col
          })
        )
        observe({
          color_pal4$val <- custom_colour4$colors()
        })
      }

    } else {
      color_pal4$val <- NULL
    }
  })

  observe({
    collectSeq$varSeq <- NULL
    collectSeq$methSer <- NULL
  })
  #### Datatable ####
  output$indivtable <- DT::renderDataTable({
    shiny::req(data_grouped_and_sorted())
    ds_ <- data_grouped_and_sorted()$B[, c('megaplots_selected_subjectid', 'megaplots_selected_event_time', data_grouped_and_sorted()$event.total)] %>%
      dplyr::mutate_if(is.factor, as.character)
    B.long <-
      na.exclude(
        reshape2::melt(
        ds_,
        id.vars = c('megaplots_selected_subjectid', 'megaplots_selected_event_time'),
        variable.name = 'EVENT',
        value.name = 'LEVEL'
        )
      )

    # calculate the counts as COUNT
    if (nrow(B.long) > 0) {
      B.sum <-
        stats::aggregate(megaplots_selected_event_time ~ megaplots_selected_subjectid + EVENT + LEVEL,
                         data = B.long,
                         FUN = length)
      colnames(B.sum)[colnames(B.sum) == 'megaplots_selected_event_time'] <- 'COUNT'
      B.sum <- dplyr::arrange(B.sum, megaplots_selected_subjectid, EVENT, LEVEL)

      B.sum <- DT::datatable(B.sum, rownames = FALSE)

      B.sum
    }
  })


  #### SAVE(D) SETTINGS ####

  #   shiny::observeEvent(uploaded_files$import.button(), {
  #   if (uploaded_files$selectdata()== "Upload saved data") {
  #     if (upload_indicator$dat == 2) {
  #       #simulate click on apply button to change color selection
  #       shinyjs::click("apply.color")
  #     }
  #   }
  # })

    #### Update all widgets when data are uploaded ####
  # shiny::observeEvent(uploaded_files$preprocess_data()$megaplot_data, {
    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   saved_file <- readRDS(input$setting_file$datapath)
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = "select.ev1",
    #     selected = saved_file$select.ev1
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = "select.ev2",
    #     selected = saved_file$select.ev2
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = "select.ev3",
    #     selected = saved_file$select.ev3
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = "select.ev4",
    #     selected = saved_file$select.ev4
    #   )
    #   ## update order
    #   shinyjqui::updateOrderInput(
    #     session,
    #     inputId = "select.ev.lev1",
    #     items = saved_file$select.ev.lev1
    #   )
    #   shinyjqui::updateOrderInput(
    #     session,
    #     inputId = "select.ev.lev2",
    #     items = saved_file$select.ev.lev2
    #   )
    #   shinyjqui::updateOrderInput(
    #     session,
    #     inputId = "select.ev.lev3",
    #     items = saved_file$select.ev.lev3
    #   )
    #   shinyjqui::updateOrderInput(
    #     session,
    #     inputId = "select.ev.lev4",
    #     items = saved_file$select.ev.lev4
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = 'select.pal1',
    #     selected = uploaded_files$preprocess_data()$megaplot_data$saved$select.pal1,
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = 'select.pal2',
    #     selected = uploaded_files$preprocess_data()$megaplot_data$saved$select.pal2,
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = 'select.pal3',
    #     selected = uploaded_files$preprocess_data()$megaplot_data$saved$select.pal3,
    #   )
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = 'select.pal4',
    #     selected = uploaded_files$preprocess_data()$megaplot_data$saved$select.pal4,
    #   )
    #   shiny::updateTextInput(
    #     session,
    #     inputId = "y_axis_label",
    #     label = "y axis label",
    #     value = uploaded_files$preprocess_data()$megaplot_data$saved$y_label
    #   )
    #   shiny::updateTextInput(
    #     session,
    #     inputId = "x_axis_label",
    #     label = "x axis label",
    #     value = uploaded_files$preprocess_data()$megaplot_data$saved$x_label
    #   )
    # }
  # })


  # output$save_setting2 <- shiny::downloadHandler(
  #   filename = function() {
  #     paste("Megaplot_Session", gsub(":", "-", Sys.time()), ".rds", sep = "")
  #   },
  #   content = function(file) {
  #     saveRDS(settings(), file)
  #   }
  # )
  #
  # settings <- shiny::reactive({
  #   shiny::req(uploaded_files$preprocess_data()$megaplot_data)
  #   param <- list(
  #     A = uploaded_files$preprocess_data()$megaplot_data$A,
  #     B = uploaded_files$preprocess_data()$megaplot_data$B,
  #     A_data_w_plot_info = data_w_plot_info()$A,
  #     B_data_w_plot_info = data_w_plot_info()$B,
  #     name = uploaded_files$preprocess_data()$name,
  #     select.ev1 = uploaded_files$select.ev1(),
  #     select.ev2 = uploaded_files$select.ev2(),
  #     select.ev3 = uploaded_files$select.ev3(),
  #     select.ev4 = uploaded_files$select.ev4(),
  #     select.ev.lev1 = uploaded_files$select.ev.lev1(),
  #     select.ev.lev2 = uploaded_files$select.ev.lev2(),
  #     select.ev.lev3 = uploaded_files$select.ev.lev3(),
  #     select.ev.lev4 = uploaded_files$select.ev.lev4(),
  #     # megaplot
  #     select.events = main_settings$select.events(),
  #     select.grouping = main_settings$select.grouping(),
  #     select.sorting = main_settings$select.sorting(),
  #     reference_line_1 = settings$reference_line_1(),
  #     reference_line_1_value = settings$reference_line_1_value(),
  #     reference_line_2 = settings$reference_line_2(),
  #     reference_line_2_value = settings$reference_line_2_value(),
  #     reference_line_3 = settings$reference_line_3(),
  #     reference_line_3_value = settings$reference_line_3_value(),
  #     event.levels = main_settings$event.levels(),
  #     select.subsetting = main_settings$select.subsetting(),
  #     zoom.range = settings$range(),
  #     # displayed subjects
  #     random = displayed_subjects$random(),
  #     selection_button = displayed_subjects$selection_button(),
  #     startsubj = displayed_subjects$startsubj(),
  #     seedset = displayed_subjects$seedset(),
  #     specific_ids = input$specific_ids,
  #     # settings
  #     select.device = input$select.device,
  #     select.col = input$select.col,
  #     thick.line = input$thick,
  #     inc.ev.subj = settings$inc.ev.subj(),
  #     lines_instead_symbols = settings$lines_instead_symbols()
  #     det.xaxt = input$det.xaxt,
  #     incr.font = input$incr.font,
  #     # color options...
  #     color.pal1 = color_pal1$val,
  #     color.pal2 = color_pal2$val,
  #     color.pal3 = color_pal3$val,
  #     color.pal4 = color_pal4$val,
  #     choiceGroup = main_settings$select.grouping(),
  #     choiceSort = choiceSort(),
  #     A_da = data_w_ai_information()$A,
  #     group_da = data_w_ai_information()$group,
  #     sequencing = data_w_ai_information()$A$'SEQUENCING',
  #     group = data_w_ai_information()$group,
  #     group.lev = data_w_ai_information()$group.lev,
  #     # seriation
  #     varSeq = input$varSeq,
  #     methSer = input$methSer,
  #     group_seriation = main_settings$select.grouping(),
  #     multiple_distmeasures = input$multiple_distmeasures,
  #     parameters_seriation = input_seriation(),
  #     #plot labels
  #     y_label = settings$y_axis_label(),
  #     x_label = settings$x_axis_label()
  #   )
  #   param
  # })



  #### Megaplots Module ####
  main_settings <- shiny::callModule(
    main_option_server,
    "megaplots",
    shiny::reactive({data_w_event_and_group_information()}),
    shiny::reactive({data_w_ai_information()}),
    shiny::reactive({uploaded_files$selectdata()}),
    shiny::reactive({input$seq.button})
  )

  settings <- shiny::callModule(
    settings_server,
    "settings",
    shiny::reactive({data_w_event_and_group_information()})
  )


  displayed_subjects <- shiny::callModule(
    displayed_subjects_server,
    "displayed_subjects",
    preprocess_data = shiny::reactive({uploaded_files$preprocess_data()})
  )

  #### Seriation Module ####
  shiny::callModule(mod_data_specification_server, "data_spec")

  # information-pdf for Seriation
  shiny::observeEvent(input$link_to_pdf_view, {
    js <- 'window.open("www/Megaplots_Seriation_Manual.pdf", "_blank", "height=700,width=1120");'
    shinyjs::runjs(js)
  })

  # call seriation module
  input_seriation <- seriation_server(
    "parametersModule",
    reactive(input$varSeq),
    reactive(input$multiple_distmeasures),
    reactive(uploaded_files$preprocess_data()$megaplot_data$saved$parameters_seriation),
    reactive(uploaded_files$selectdata())
  )

}
