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

  #change font colors of dataTables
  output$includeCSS <- shiny::renderUI({
    col <- ifelse(coltheme$col_sel == 'grey (app version)',"#ffffff","#000000")
    shiny::tags$style(
      shiny::HTML(
        paste0(
          ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
          color: ",col," !important;
          }"
        )
      )
    )
  })

  #### MODULE CALLS ####

  # Data upload module (server part)
  uploaded_files <- shiny::callModule(
    data_upload_server,
    "data_upload",
    setting_file = shiny::reactive({main_settings$setting_file()})
  )

  # Main settings module (server part) (box at top of the megaplots)
  main_settings <- shiny::callModule(
    main_option_server,
    "megaplots",
    shiny::reactive({data_w_event_and_group_information()}),
    shiny::reactive({data_w_ai_information()}),
    shiny::reactive({uploaded_files$selectdata()}),
    shiny::reactive({artificial_intelligence$seq.button()}),
    shiny::reactive({displayed_subjects$displayed_subjects_settings()}),
    settings = shiny::reactive({settings$settings()}),
    color_options = shiny::reactive({color_options$color_info()}),
    var = shiny::reactive({artificial_intelligence$varSeq()}),
    par = shiny::reactive({artificial_intelligence$input_seriation()}),
    sermethod = shiny::reactive({artificial_intelligence$methSer()})
  )

  #  Megaplot module (server part)
  shiny::callModule(
    mega_plot_server,
    "mega_plot",
    data_w_plot_info = shiny::reactive({data_w_plot_info()}),
    data_w_event_and_group_information = shiny::reactive({data_w_event_and_group_information()}),
    data_grouped_and_sorted = shiny::reactive({data_grouped_and_sorted()}),
    data_w_ai_information = shiny::reactive({data_w_ai_information()}),
    selectdata = shiny::reactive({uploaded_files$selectdata()}),
    seq.button = shiny::reactive({artificial_intelligence$seq.button()}),
    select.col= shiny::reactive({select.col()}),
    main_settings = shiny::reactive({main_settings}),
    settings = shiny::reactive({settings$settings()}),
    summary_statistics = shiny::reactive({summary_statistics})
  )

  # displayed subjects settings module (server part)
  displayed_subjects <- shiny::callModule(
    displayed_subjects_server,
    "displayed_subjects",
    preprocess_data = shiny::reactive({uploaded_files$preprocess_data()}),
    setting_file = shiny::reactive({main_settings$setting_file()})
  )

  # Settings module (server part) (sidebar)
  settings <- shiny::callModule(
    settings_server,
    "settings",
    shiny::reactive({data_w_event_and_group_information()}),
    setting_file = shiny::reactive({main_settings$setting_file()})
  )

  # summary statistics module (server part)
  summary_statistics <- shiny::callModule(
    summary_statistics_server,
    "summary_statistics",
    shiny::reactive({data_w_plot_info()}),
    shiny::reactive({data_grouped_and_sorted()}),
    shiny::reactive({uploaded_files$import.button()}),
    shiny::reactive({main_settings$select.events()}),
    shiny::reactive({main_settings$event.levels()}),
    select_color = shiny::reactive({select.col()})
  )

  # raw data module (server part)
  raw_data <- shiny::callModule(
    raw_data_server,
    "raw_data",
    preprocess_data = shiny::reactive({uploaded_files$preprocess_data()}),
    data_w_ai_information = shiny::reactive({data_w_ai_information()}),
    select_color = shiny::reactive({select.col()})
  )

  # color options module (server part)
  color_options <- shiny::callModule(
    color_options_server,
    "color_options",
    shiny::reactive({uploaded_files$import.button()}),
    shiny::reactive({uploaded_files$select.ev1()}),
    shiny::reactive({uploaded_files$select.ev2()}),
    shiny::reactive({uploaded_files$select.ev3()}),
    shiny::reactive({uploaded_files$select.ev4()}),
    shiny::reactive({uploaded_files$select.ev.lev1()}),
    shiny::reactive({uploaded_files$select.ev.lev2()}),
    shiny::reactive({uploaded_files$select.ev.lev3()}),
    shiny::reactive({uploaded_files$select.ev.lev4()}),
    setting_file = shiny::reactive({main_settings$setting_file()})
  )

  #sequencing/ai module (server part)
  artificial_intelligence <- shiny::callModule(
    artificial_intelligence_server,
    "ai",
    shiny::reactive({uploaded_files$preprocess_data()}),
    shiny::reactive({uploaded_files$selectdata()}),
    shiny::reactive({data_w_event_and_group_information()}),
    setting_file = shiny::reactive({main_settings$setting_file()})
  )

  # data specification module (server part)
  shiny::callModule(
    mod_data_specification_server,
    "data_spec",
    select_color = shiny::reactive({select.col()})
  )



  #### DATA UPLOAD ####
  # disable/enable import button if data are missing/available
  shiny::observe({
    shinyjs::disable(id = 'data_upload-import.button', selector = NULL)
    df <- shiny::req(uploaded_files$preprocess_data()$megaplot_data)
    #if uploaded data are available and preprocessed enable import button
    if (!is.null(df)) { shinyjs::enable(id = 'data_upload-import.button', selector = NULL) }
  })

  #update tab to megaplot tab if import button is clicked
  observeEvent(uploaded_files$import.button(), {
    newtab <- switch(input$sidebarmenu, "dashboard")
    shinydashboard::updateTabItems(session, "sidebarmenu", newtab)
  })

  #### PROCESSING DATA ####
  # 1. calculate aggregated stats for sequencing and clustering after clicking import button

  summary_statistics_data <- shiny::eventReactive(c(uploaded_files$import.button(), uploaded_files$impswitch()), {
    # function to summarise_megaplot_data and save results as list object with
    # entries 'total' and 'detail'
    shiny::req(uploaded_files$preprocess_data())
    if(!is.null(uploaded_files$preprocess_data()$megaplot_data$A)){
      summarise_megaplot_data(data = uploaded_files$preprocess_data())
    } else {
      NULL
    }
  })

  # 2. 'data_w_event_and_group_information' = standardized object in which the original data is stored
  #           together with information on the grouping and event variables
  #           (will not be reactively modified in the app and can be used to
  #           create some of the UI input fields)

  data_w_event_and_group_information <- shiny::eventReactive(
    c(summary_statistics_data(),uploaded_files$select.ev.lev1(),uploaded_files$select.ev.lev2(),uploaded_files$select.ev.lev3(),uploaded_files$select.ev.lev4()),{

    shiny::req(uploaded_files$preprocess_data())

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


  ## AI FUNCTIONALITY ##
  # 3. 'data_w_ai_information' = copy from 'data_w_event_and_group_information'
  # that will be reactively modified based on the AI selections
  # from the user

  data_w_ai_information_reacVal <- reactiveValues(df = NULL)

  shiny::observeEvent(data_w_event_and_group_information(), {
    shiny::req(uploaded_files$preprocess_data())
    data_w_ai_information <- shiny::req(data_w_event_and_group_information())
    data_w_ai_information_reacVal$df <- data_w_ai_information
  })

  shiny::observeEvent(artificial_intelligence$seq.button(),{
    shiny::req(uploaded_files$preprocess_data())
    data_w_ai_information <- shiny::req(data_w_event_and_group_information())

    if (!is.null(artificial_intelligence$varSeq())) {
      # Call the sequencing function with all the corresponding parameters:
      data_w_ai_information <- sequencing_var_app(
        da = data_w_ai_information,
        var = artificial_intelligence$varSeq(),
        par = artificial_intelligence$input_seriation(),
        sermethod = artificial_intelligence$methSer(),
        group = main_settings$select.grouping(),
        multiple_distmeasures = artificial_intelligence$multiple_distmeasures()
      )
    }
    data_w_ai_information_reacVal$df <- data_w_ai_information
  })

  shiny::observeEvent(main_settings$setting_file(),{
    shiny::req(uploaded_files$preprocess_data())
    data_w_ai_information <- shiny::req(data_w_event_and_group_information())
    shiny::req(main_settings$setting_file())
    if (!is.null(main_settings$setting_file())) {
      saved_file <- main_settings$setting_file()
      if (is.list(saved_file)) {
        if(!is.null(saved_file$var) & !is.null(saved_file$sermethod)){
          #Call the sequencing function with all the corresponding parameters:
          data_w_ai_information <- sequencing_var_app(
            da = data_w_ai_information,
            var = saved_file$var,
            par = saved_file$par,
            sermethod = saved_file$sermethod,
            group = main_settings$select.grouping(),
            multiple_distmeasures = input$multiple_distmeasures
          )
        }
      }
    }
      data_w_ai_information_reacVal$df <- data_w_ai_information
  })


  data_w_ai_information <- shiny::eventReactive(data_w_ai_information_reacVal$df, {
    shiny::req(uploaded_files$preprocess_data())
    data_w_ai_information <- data_w_ai_information_reacVal$df
    data_w_ai_information
  })


  # 4. 'data_grouped_and_sorted' = copy from 'data_w_ai_information' that will be reactively modified
  #           in the sorting/grouping part based on user input (preparation for the plotting)
  #
  data_grouped_and_sorted <- shiny::reactive({
    shiny::req(uploaded_files$preprocess_data())

    displayed_subjects$selection_button()
    displayed_subjects$subset.button()

    add_sorting_information(
      data_frame = data_w_ai_information(),
      select_subsetting = main_settings$select.subsetting(),
      randet = displayed_subjects$selection_button(),
      seed =displayed_subjects$seedset(),
      nshow =displayed_subjects$random(),
      start =displayed_subjects$startsubj(),
      specific_ids = displayed_subjects$specific_ids(),
      event_levels = main_settings$event.levels(),
      select_sorting = main_settings$select.sorting(),
      select_grouping = main_settings$select.grouping(),
      select_events = main_settings$select.events()
    )
  })


  # create reactive plotting object
  # 5. 'data_w_plot_info' = add plot information to data set (zoom/range and colors)

  data_w_plot_info <- shiny::reactive({
    # session$clientData[["image1"]]
    #requirements
    shiny::req(data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())
    shiny::req(uploaded_files$preprocess_data())
    # create plotting data based on user selections
    data_w_plot_info <- data_grouped_and_sorted()
    data_w_event_and_group_information <- data_w_event_and_group_information()
    # constrain to selected x-range
    data_w_plot_info$A$megaplots_selected_start_time <- pmax(data_w_plot_info$A$megaplots_selected_start_time, settings$settings()$range[1])
    data_w_plot_info$A$megaplots_selected_end_time <-
      pmin(pmax(data_w_plot_info$A$megaplots_selected_end_time, settings$settings()$range[1]), settings$settings()$range[2])
    data_w_plot_info$B <-
      subset(data_w_plot_info$B, megaplots_selected_event_time >= settings$settings()$range[1] & megaplots_selected_event_time <= settings$settings()$range[2])

    # set plotting colors
    col.ev <- list()
    type.ev <- list()

    if (!is.na(data_w_event_and_group_information$event[1])) {
      col.ev[[1]] <- color_options$color_info()$color_pal1[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[1]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      col.ev[[2]] <- color_options$color_info()$color_pal2[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[2]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      col.ev[[3]] <- color_options$color_info()$color_pal3[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[3]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      col.ev[[4]] <- color_options$color_info()$color_pal4[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[4]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[1])) {
      type.ev[[1]] <- color_options$color_info()$color_pal1[length(color_options$color_info()$color_pal1)]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      type.ev[[2]] <- color_options$color_info()$color_pal2[length(color_options$color_info()$color_pal2)]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      type.ev[[3]] <- color_options$color_info()$color_pal3[length(color_options$color_info()$color_pal3)]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      type.ev[[4]] <- color_options$color_info()$color_pal4[length(color_options$color_info()$color_pal4)]
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


  #### REACTIVES & OBSERVERS ####
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

  shiny::observeEvent(color_options$color_info()$select.col, {
    coltheme$col_sel <- color_options$color_info()$select.col
  })

  #update theme depending on select in color menuitem (grey or white verison)
  output$theme <- renderUI({
      if (color_options$color_info()$select.col == 'grey (app version)') {
        fresh::use_theme(
          fresh::create_theme(
          fresh::adminlte_color(
            light_blue = "#0091DF",
            black = "#222d32"#
          ),
          fresh::adminlte_sidebar(
            width = "275px",
            dark_bg = "#222d32",#Background color (dark mode).
            dark_hover_bg = "#0091DF",#Background hover color (dark mode).
            dark_color = "#dce4e8",#Text color (dark mode).
            dark_hover_color ="#dce4e8",#Text hover color (dark mode).
          ),
          fresh::adminlte_global(
            content_bg = "#404A4E", # Background color of the body.
            box_bg = "#222d32",#Default background color for boxes.
            info_box_bg = "#222d32"#Default background color for info boxes.
          )
        ))
      } else {
         fresh::use_theme(
          fresh::create_theme(
          fresh::adminlte_color(
            light_blue = "#0091DF",
            black = "#9c9a9a"#
          ),
          fresh::adminlte_sidebar(
            width = "275px",
            dark_bg = "#9c9a9a",#Background color (dark mode).
            dark_hover_bg = "#0091DF",#Background hover color (dark mode).
            dark_color = "#222d32",#Text color (dark mode).
            dark_hover_color ="#222d32",#Text hover color (dark mode).
            light_color = "#222d32",
            light_hover_color ="#222d32"
          ),
          fresh::adminlte_global(
            content_bg = "#ffffff", # Background color of the body.
            box_bg = "#9c9a9a",#Default background color for boxes.
            info_box_bg = "#313438"#Default background color for info boxes.
          )
        ))
     }
  })

  select.col <- shiny::reactive({
    if (coltheme$col_sel == 'grey (app version)') {
      col_sel <- c(
        'plot.bg' = '#404A4E',
        'plot.bg2' = '#40444a',
        'plot.lines' = '#000000',
        'plot.wp' = '#404A4E',
        'plot.id' = 'white',
        'axleg.bg' = '#222d32',
        'cont.bg' = '#222d32'
      )
    } else if (coltheme$col_sel == 'white (print version)') {
      col_sel <- c(
        'plot.bg' = '#ffffff',
        'plot.bg2' = '#f2f2f2',
        'plot.lines' = '#000000',
        'plot.wp' = '#ffffff',
        'plot.id' = 'black',
        'axleg.bg' = '#ffffff',
        'cont.bg' = '#ffffff'
      )
    } else {
      col_sel <- "red"
    }
    col_sel
  })
}
