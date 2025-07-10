#' Megaplot Main Option Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

mega_plot_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shiny::uiOutput(ns('hover_legend')),
    shiny::uiOutput(ns('megaplot')),
    p("", style = "margin-bottom: 40px;"),
    fixedPanel(
      shiny::uiOutput(ns('axisbox')),
      bottom = 1,
      width = "100%",
      height = 42
    ),
    shiny::uiOutput(ns('hoverpanel')),
    shiny::uiOutput(ns('summarypanel'))#,
    # shiny::conditionalPanel(condition = "output.check_slider_used == true",
    #   shiny::uiOutput(ns('next_buttons')),
    #   ns = NS(id)
    # )
  )
}


#' Megaplot Main Option Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param data_w_plot_info list with data and megaplot information from server
#' @param data_w_event_and_group_information list with grouped data information from server
#' @param data_grouped_and_sorted list with sorted data information from server
#' @param data_w_ai_information list with data including sequencing information from server
#' @param selectdata character of data upload method ("Use demo data"/"Data upload")
#' @param seq.button reactive actionButton value for update on sequencing
#' @param main_settings list with main options above megaplot in app
#' @param settings list with main options above megaplot in app
#' @param select.col color theme information
#' @param summary_statistics reactive with summary statistics data for summary panel
#'
#' @return List with 'Main option'-inputs
#'
#' @noRd
#' @keywords internal

mega_plot_server <- function(
    input,
    output,
    session,
    data_w_plot_info,
    data_w_event_and_group_information,
    data_grouped_and_sorted,
    data_w_ai_information,
    selectdata,
    seq.button,
    select.col,
    main_settings,
    settings,
    summary_statistics
    ){

  ns <- session$ns

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

  plot_par_settings_zoom <- shiny::reactive({
    shiny::req(data_w_plot_info(), data_w_event_and_group_information(), data_grouped_and_sorted())
    mar2 <- 5
    grLab <- NULL
    mar4 <- 1
    list('mar' = c(0, mar2, 0, mar4), 'grLab' = grLab)
  })

  #### PLOTTING ####
  # Plot Megaplots
  output$image1 <- shiny::renderPlot({
    # reactivity
    shiny::req(image1_plot_height())
    #shiny::req(settings()$height_slider())
    session$clientData[["output_mega_plot-image1_width"]]
    # main_settings()$event.levels()
    # main_settings()$select.grouping()
    main_settings()$event.levels()
    #requirements
    shiny::req(data_w_plot_info(), data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())
    draw_megaplot(
      megaplot_data = data_w_plot_info(),
      select_color = select.col(),
      par_settings = plot_par_settings(),
      background_stripes = settings()$background_stripes,
      background_stripes_length = settings()$background.stripes.length,
      event_levels = main_settings()$event.levels(),
      xlim = c(settings()$range[1], settings()$range[2]),
      ylim = range(data_w_plot_info()$A$subject) + c(-1.5, 1.5),
      lines_instead_symbols = settings()$lines_instead_symbols,
      lines_options = settings()$lines_options,
      line_width = settings()$thick,
      y_axis_label = settings()$y_axis_label,
      reference_line_1 = settings()$reference_line_1,
      reference_line_1_value = settings()$reference_line_1_value,
      reference_line_2 = settings()$reference_line_2,
      reference_line_2_value = settings()$reference_line_2_value,
      reference_line_3 = settings()$reference_line_3,
      reference_line_3_value = settings()$reference_line_3_value,
      select_events = main_settings()$select.events(),
      color_subject_line_by_first_event = settings()$inc.ev.subj
    )
  },
  # function for UI auto height resizing
  height = function(){image1_plot_height()})

  image1_plot_height <- shiny::reactive({
    (max(c(1, data_grouped_and_sorted())$A$'subject') * 12) * settings()$height_slider
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
    settings()$x_axis_label

    session$clientData[["output_mega_plot-image1_width"]]
    #requirements
    shiny::req(settings()$range)
    draw_megaplot_x_axis(
      range = settings()$range,
      select_color = select.col(),
      megaplot_axis_width = session$clientData[["output_mega_plot-image1Axis_width"]],
      megaplot_width = shiny::req(session$clientData[["output_mega_plot-image1_width"]]),
      plot_par_settings = plot_par_settings(),
      axis_ticks = settings()$det.xaxt,
      x_axis_label = settings()$x_axis_label,
      reference_line_1 = settings()$reference_line_1,
      reference_line_1_value = settings()$reference_line_1_value,
      reference_line_2 = settings()$reference_line_2,
      reference_line_2_value = settings()$reference_line_2_value,
      reference_line_3 = settings()$reference_line_3,
      reference_line_3_value = settings()$reference_line_3_value
    )
  })


  # box with main plot
  # create reactive megaplot image with dynamic height adjustment as UI output
  output$megaplot <- shiny::renderUI({
      shiny::plotOutput(
        outputId = ns('image1'),
        dblclick = shiny::clickOpts(id = ns("dblclick_scatter")),
        brush = shiny::brushOpts(
          id = ns("image1_brush"),
          fill = "#ffffff",
          stroke = "#036",
          opacity = 0.25,
          delay = 300
        ),
        hover = shiny::hoverOpts(
          ns("image1_hover"),
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



  # output$next_buttons <- shiny::renderUI({
  #   shiny::absolutePanel(
  #     id = ns("next_buttons"),
  #     class = "modal-content",
  #     fixed = TRUE,
  #     draggable = TRUE,
  #     HTML(paste0(
  #       "<div style='background-color: ",select.col()['plot.bg'],"'>"
  #     )),
  #     top = 300,
  #     left = "auto",
  #     right = 50,
  #     bottom = "auto",
  #     width = 60,
  #     height = "auto",
  #     shiny::fluidRow(
  #       shinyWidgets::circleButton(
  #         inputId = ns("btn1"),
  #         icon = icon("step-backward"),
  #         status = "default",
  #         size = "xs"
  #       ),
  #       shinyWidgets::circleButton(
  #         inputId = ns("btn2"),
  #         icon = icon("step-forward"),
  #         status = "default",
  #         size = "xs"
  #       )
  #     ),
  #     style = "z-index: 10;"
  #   )
  # })

  #### Summary Panel ####
  output$summarypanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = ns("summarypanel"),
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: ",select.col()['plot.bg'],"'>"
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
            summary_statistics()$summary_statistics_text()
          )
      ),
      style = "z-index: 10;"
    )
  })

  #### Hover Panel ####
  # Hover plot output$hover
  output$hover <- shiny::renderPlot({
    # shiny::req(data_w_plot_info())
    color_bg <- select.col()
    data_w_plot_info <- data_w_plot_info()
    if (!is.null(brush_coord$x) & !is.null(brush_coord$y)) {
      draw_megaplot(
        megaplot_data = data_w_plot_info(),
        select_color = select.col(),
        par_settings = plot_par_settings_zoom(),
        background_stripes = settings()$background_stripes,
        background_stripes_length = settings()$background.stripes.length,
        event_levels = main_settings()$event.levels(),
        xlim = brush_coord$x,
        ylim = brush_coord$y + c(-1.5, 1.5),
        lines_instead_symbols = settings()$lines_instead_symbols,
        lines_options = settings()$lines_options,
        line_width = settings()$thick,
        y_axis_label = settings()$y_axis_label,
        reference_line_1 = settings()$reference_line_1,
        reference_line_1_value = settings()$reference_line_1_value,
        reference_line_2 = settings()$reference_line_2,
        reference_line_2_value = settings()$reference_line_2_value,
        reference_line_3 = settings()$reference_line_3,
        reference_line_3_value = settings()$reference_line_3_value,
        select_events = main_settings()$select.events(),
        color_subject_line_by_first_event = settings()$inc.ev.subj
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
      id = ns("hoverpanel"),
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: ",select.col()['plot.bg'],"'>"
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
        shiny::plotOutput(ns('hover'))
      ),
      style = "z-index: 10;"
    )
  })

  max_legend_char <- shiny::reactiveVal({270})

  # re-create panels after reset panel click to display them when hidden
  shiny::observeEvent(settings()$reset_draggable_panel_positions, {
   output$hoverpanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = ns("hoverpanel"),
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: ",select.col()['plot.bg'],"'>"
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
                                        shiny::plotOutput(ns('hover'))))
        ),
        style = "z-index: 10;"
      )
    })

    output$summarypanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = ns("summarypanel"),
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
          HTML(paste0(
            "<div style='background-color: ",select.col()['plot.bg'],"'>"
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
              summary_statistics()$summary_statistics_text()
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
        id = ns("hover_legend"),
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: ",select.col()['plot.bg'],"'>"
        )),
        top = 333,
        left = "auto",
        right = 75,
        bottom = "auto",
        width = max_legend_char(),
        height = "auto",
        shiny::fluidRow(
          shiny::plotOutput(ns('image1Legend'), height = leg.height, width = max_legend_char())
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
      id = ns("hover_legend"),
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: ",select.col()['plot.bg'],"'>"
      )),
      top = 333,
      left = "auto",
      right = 75,
      bottom = "auto",
      width = max_legend_char(),
      height = "auto",
      shiny::fluidRow(
        shiny::plotOutput(ns('image1Legend'), height = leg.height, width = max_legend_char())
      ),
      style = "z-index: 10;"
    )
  })

  # box with axis
  output$axisbox <- shiny::renderUI({
    shiny::plotOutput(
      outputId = ns('image1Axis'),
      height = '40px'
    )
  })
}
