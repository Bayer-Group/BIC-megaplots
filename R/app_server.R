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


  ## AI FUNCTIONALITY ##

  # 3. 'data_w_ai_information' = copy from 'data_w_event_and_group_information'
  # that will be reactively modified based on the AI selections
  # from the user

  data_w_ai_information <- shiny::eventReactive(c(data_w_event_and_group_information(),artificial_intelligence$seq.button()), { # aiButton$seq), {
    data_w_ai_information <- shiny::req(data_w_event_and_group_information())

    if (!is.null(artificial_intelligence$varSeq())) {
      # Call the sequencing function with all the corresponding parameters:
      data_w_ai_information <- sequencing_var_app(
        da = data_w_ai_information,
        var = artificial_intelligence$varSeq(),
        par = artificial_intelligence$input_seriation(),
        sermethod = artificial_intelligence$methSer(),
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
      col.ev[[1]] <- color_options$color_pal1()[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[1]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      col.ev[[2]] <- color_options$color_pal2()[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[2]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      col.ev[[3]] <- color_options$color_pal3()[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[3]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      col.ev[[4]] <- color_options$color_pal4()[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[4]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[1])) {
      type.ev[[1]] <- color_options$color_pal1()[length(color_options$color_pal1())]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      type.ev[[2]] <- color_options$color_pal2()[length(color_options$color_pal2())]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      type.ev[[3]] <- color_options$color_pal3()[length(color_options$color_pal3())]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      type.ev[[4]] <- color_options$color_pal4()[length(color_options$color_pal4())]
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




  #### Megaplots Module ####
  main_settings <- shiny::callModule(
    main_option_server,
    "megaplots",
    shiny::reactive({data_w_event_and_group_information()}),
    shiny::reactive({data_w_ai_information()}),
    shiny::reactive({uploaded_files$selectdata()}),
    shiny::reactive({artificial_intelligence$seq.button()})
  )

  settings <- shiny::callModule(
    settings_server,
    "settings",
    shiny::reactive({data_w_event_and_group_information()})
  )

  summary_statistics <- shiny::callModule(
    summary_statistics_server,
    "summary_statistics",
    shiny::reactive({data_w_plot_info()}),
    shiny::reactive({data_grouped_and_sorted()}),
    shiny::reactive({uploaded_files$import.button()}),
    shiny::reactive({main_settings$select.events()}),
    shiny::reactive({main_settings$event.levels()})
  )

  raw_data <- shiny::callModule(
    raw_data_server,
    "raw_data",
    preprocess_data = shiny::reactive({uploaded_files$preprocess_data()}),
    data_w_ai_information = shiny::reactive({data_w_ai_information()})
  )

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
    shiny::reactive({uploaded_files$selectdata()})
  )

  artificial_intelligence <- shiny::callModule(
    artificial_intelligence_server,
    "ai",
    shiny::reactive({uploaded_files$preprocess_data()}),
    shiny::reactive({uploaded_files$selectdata()}),
    shiny::reactive({data_w_event_and_group_information()})
  )

  displayed_subjects <- shiny::callModule(
    displayed_subjects_server,
    "displayed_subjects",
    preprocess_data = shiny::reactive({uploaded_files$preprocess_data()})
  )

  #### Data Spec Module ####
  shiny::callModule(mod_data_specification_server, "data_spec")

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
    settings = shiny::reactive({settings}),
    summary_statistics = shiny::reactive({summary_statistics})
  )

}
