#' Megaplot Main Option Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

main_option_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shinydashboard::box(
      width = NULL,
      title = HTML('<p style ="color:white;"> Main options </p>'),
      solidHeader = TRUE,
      collapsible = TRUE,
      column(12,
        column(3,
          shiny::selectizeInput(
            inputId = ns('select.events'),
            label = HTML('<p style ="color:white;"> Select events</p>'),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list('plugins' = list('remove_button', 'drag_drop'))
          )
        ),
        column(3,
          shiny::selectizeInput(
            inputId = ns('select.grouping'),
            label = HTML('<p style ="color:white;"> Select grouping </p>'),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list('plugins' = list('remove_button', 'drag_drop'))
          )
        ),
        column(3,
          shinyWidgets::pickerInput(
            inputId = ns('select.sorting'),
            label = HTML('<p style ="color:white;"> Sort (descending) </p>'),
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              `style` = 'background: btn-primary',
              `header` = 'Select item'
            )
          )
        )
      ),
      shiny::column(12,
        column(3,
          shinyWidgets::pickerInput(
            inputId = ns('event.levels'),
            label = HTML('<p style ="color:white;"> Select event levels</p>'),
            choices = NULL,
            multiple = TRUE,
            selected = NULL,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = 'count > 0',
              `count-selected-text` = '{0} selected (of {1})',
              `live-search` = TRUE,
              `style` = 'background: btn-primary',
              `header` = 'Select multiple items',
              `none-selected-text` = 'Please select'
            )
          )
        ),
        column(3,
          shinyWidgets::pickerInput(
            inputId = ns('select.subsetting'),
            label = HTML('<p style ="color:white;"> Select subsets</p>'),
            choices = NULL,
            multiple = TRUE,
            selected = NULL,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = 'count > 0',
              `count-selected-text` = '{0} selected (of {1})',
              `live-search` = TRUE,
              `style` = 'background: btn-primary',
              `header` = '(For every factor at least one value must be selected)',
              `none-selected-text` = 'All dropped!'
            )
          )
        ),
        column(3,
        HTML('<p style ="color:white;"> Save settings as .rds file:</p>'),
        shinyWidgets::downloadBttn(
          outputId = ns("save_settings"),
          label = "Save Session Settings",
          style = 'gradient',
          color = 'primary',
          size = 'sm',
          no_outline = FALSE,
          block = FALSE
        )
        ),
        column(3,
          column(4,
          shinyWidgets::materialSwitch(
            inputId = ns('save_settings_switch'),
            label = HTML('<p style ="color:white;"> Use saved app settings </p>'),
            status = 'primary'
          )
        ),
        shiny::conditionalPanel(condition = "input.save_settings_switch == true",
            column(8,
            shiny::fileInput(
              inputId = ns('setting_file'),
              label = HTML(
                '<p style ="color:white;"> Upload app settings file (.rds) </p>'
              ),
              multiple = FALSE,
              accept = '.rds'
            ),
            shinyWidgets::actionBttn(
              inputId = ns('test'),
              label = "Use saved settings",
              style = 'gradient',
              color = 'primary',
              size = 'sm',
              no_outline = FALSE,
              block = TRUE
            ),
            shiny::helpText(
              'Note: When updating the events order, please press twice to ensure that the settings have been loaded.'
            )
            ),
            ns = NS(id)
        )
        )
      )
    )
    # ,
    # shiny::tags$head(
    #   tags$style(
    #     HTML(
    #       '.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'
    #     )
    #   )
    # ),
  )
}


#' Megaplot Main Option Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param data_w_event_and_group_information list with grouped data information from server
#' @param data_w_ai_information list with data including sequencing information from server
#' @param selectdata character of data upload method ("Use demo data"/"Data upload")
#' @param seq.button reactive actionButton value for update on sequencing
#' @param displayed_subjects_settings list with options from displayed subjects side bar menu
#' @param settings list with settings from menuitem settings
#' @param color_options list with color options from menuitem color option
#' @param var character with variables selected for sequencing (one or multiple variables)
#' @param par parameter list for sequencing methods
#' @param sermethod character with sequencing method name
#'
#' @return List with 'Main option'-inputs
#'
#' @noRd
#' @keywords internal

main_option_server <- function(
    input,
    output,
    session,
    data_w_event_and_group_information,
    data_w_ai_information,
    selectdata,
    seq.button,
    displayed_subjects_settings,
    settings,
    upload_settings,
    color_options,
    var,
    par,
    sermethod,
    update_saved_settings
  ){

  ns <- session$ns

  #### MAIN OPTIONS ####
  choices_events <- shiny::reactiveValues(val = NULL)
  ####... select.events ####
  shiny::observeEvent(c(data_w_event_and_group_information()), {

    shiny::req(data_w_event_and_group_information())

    choices <- shiny::isolate(data_w_event_and_group_information()$event)
    if (is.null(isolate(data_w_event_and_group_information()$event))) {
      selected <- choices
    } else {
      selected <- shiny::isolate(data_w_event_and_group_information()$event)
    }
    choices_events$val <- choices
    shiny::updateSelectizeInput(
      session,
      inputId = "select.events",
      choices = choices,
      selected = selected
    )
  })

  ####... select.grouping ####
  choiceGroup <- shiny::eventReactive(c(data_w_event_and_group_information()), {
      data_w_event_and_group_information()$group
  })

  shiny::observeEvent(choiceGroup(), {
    choices <- shiny::req(choiceGroup())
    selected <- NULL

    shiny::updateSelectizeInput(
      session,
      inputId = "select.grouping",
      selected = selected,
      choices = choices
    )
  })

  ####... select.sorting ####
  choiceSort <- shiny::eventReactive(c(data_w_ai_information(), seq.button()), {
    data_w_ai_information()$nume_A[!is.na(data_w_ai_information()$nume_A)]
  })


  shiny::observeEvent(update_saved_settings(), {
    choices <- choiceSort()

    # selected <- 'megaplots_selected_subjectid'
    # if (any(choices == 'SEQUENCING')) selected <- utils::tail(choices, 1)
    if (!is.null(setting_file())) {
      # saved_file <- readRDS(input$setting_file$datapath)
      if (is.list(setting_file())) {
        selected <- setting_file()$select.sorting
      }
    } else {
      if (any(choices == 'SEQUENCING')) {
        selected <- utils::tail(choices, 1)
      } else {
        selected <- "megaplots_selected_subjectid"
      }
    }
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.sorting",
      choices = choices,
      selected = selected
    )
  })


  shiny::observeEvent(choiceSort(), {
    choices <- choiceSort()

    # selected <- 'megaplots_selected_subjectid'
    # if (any(choices == 'SEQUENCING')) selected <- utils::tail(choices, 1)
    # if (!is.null(setting_file())) {
    #   # saved_file <- readRDS(input$setting_file$datapath)
    #   if (is.list(setting_file())) {
    #     selected <- setting_file()$select.sorting
    #   }
    # } else {
      if (any(choices == 'SEQUENCING')) {
        selected <- utils::tail(choices, 1)
      } else {
        selected <- "megaplots_selected_subjectid"
      }
    # }
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.sorting",
      choices = choices,
      selected = selected
    )
  })

  ####... event.levels ####

  choices_event_levels <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(data_w_event_and_group_information(), {

    # choices <- unlist(lapply(data_w_event_and_group_information()$ai.initselect, function(x){unlist(strsplit(x, " = ")[[1]][2])}))
    choices <- unlist(data_w_event_and_group_information()$event.lev,
                      use.names = FALSE)
    # lev.ev <- list()
    # lev.ev.n <- list()
    # for (i in 1:length(test_w_event$event)) {
    #   lev.ev[[i]] <- levels(test_w_event$B[, test_w_event$event[i]])
    #   lev.ev.n[[i]] <- nlevels(test_w_event$B[, test_w_event$event[i]])
    # }
    #

    tmp <- data_w_event_and_group_information()$event.lev.n
    choices.lab <- rep(data_w_event_and_group_information()$event,
                       tmp)
    choices.sym <- rep('glyphicon-cloud',
                       length(choices.lab))
    choices.col <- paste('color:',
                         unlist(data_w_event_and_group_information()$col.ev[data_w_event_and_group_information()$event],
                                use.names = FALSE))
    choices <- paste0(choices.lab, ' = ', choices)

    choices_event_levels$val <- choices
    selected <- choices

    shinyWidgets::updatePickerInput(
      session,
      inputId = "event.levels",
      choices = choices,
      selected = selected,
      choicesOpt = list(
        `icon` = choices.sym,
        `style` = choices.col
      )
    )
  })


  choices_subsetting <- shiny::reactiveValues(val = NULL)
  ####... select.subsetting ####
    shiny::observeEvent(data_w_ai_information(), {
    shiny::req(data_w_ai_information())
    choices <- unlist(data_w_ai_information()$group.lev, use.names = FALSE)
    choices.lab <- rep(data_w_ai_information()$group, sapply(data_w_ai_information()$group.lev, FUN = length))
    choices <- paste0(choices.lab, ' = ', choices)

    choices_subsetting$val <- choices
    selected <- choices

    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.subsetting",
      choices = choices,
      selected = selected
    )
  })

  main_option_settings <- shiny::reactive({
    param <- list(
       select.events = input$select.events,
       choices.events = choices_events$val,
       select.grouping = input$select.grouping,
       select.sorting = input$select.sorting,
       choices.sorting = choiceSort(),
       select.event.levels = input$event.levels,
       choices.event.levels = choices_event_levels$val,
       select.subsetting = input$select.subsetting,
       choices.subsetting = choices_subsetting$val
    )
    param
  })

  setting_file <- shiny::reactive({
    if (!is.null(input$setting_file)) {
      saved_file <- readRDS(input$setting_file$datapath)
      if (is.list(saved_file)) {
        saved_file
      }
    }
  })


  shiny::observeEvent(update_saved_settings(), {

        #update main options
        shiny::updateSelectizeInput(
          session,
          inputId = "select.events",
          choices = setting_file()$choices_events,
          selected = setting_file()$select.events
        )
        shiny::updateSelectizeInput(
          session,
          inputId = "select.grouping",
          selected = setting_file()$select.grouping,
        )
        shinyWidgets::updatePickerInput(
          session,
          inputId = "event.levels",
          choices = setting_file()$choices.event.levels,
          selected = setting_file()$select.event.levels

        )
        shinyWidgets::updatePickerInput(
          session,
          inputId = "select.subsetting",
          choices = setting_file()$choices.subsetting,
          selected = setting_file()$select.subsetting
        )

        # choices <- setting_file()[[paste0("select.ev.lev",2)]]
        #   print("TRIGGER UPDATE SAVED")
        #   shinyjqui::updateOrderInput(
        #     session,
        #     inputId = "data_upload-select_ev.2-select.ev.lev",
        #     items = choices
        #   )
        # shinyWidgets::updatePickerInput(
        #   session,
        #   inputId = "select.sorting",
        #   selected = setting_file()$select.sorting#,
        #   #choices = setting_file()$select.sorting.choices
        # )

        # shinyWidgets::updatePickerInput(
        #   session,
        #   inputId = "select.sorting",
        #   selected = setting_file()$select.sorting,
        #   choices = setting_file()$select.sorting.choices
        # )
    #   }
    # }
  })

  output$save_settings <- shiny::downloadHandler(
    filename = function() {
      paste("Megaplot_Session", gsub(":", "-", Sys.time()), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(
        c(main_option_settings(),upload_settings(),displayed_subjects_settings(),settings(),color_options(),list(par = par()),list(var = var()),list(sermethod = sermethod()))
        , file
      )
    }
  )
  # #### reactiveValue inputIMP ####
  # inputIMP <- shiny::reactiveValues(select.events = NULL, select.grouping = NULL,name = '')
  #
  # shiny::observeEvent(input$select.events, ignoreNULL = FALSE, {
  #   inputIMP$select.events <- input$select.events
  # })
  #
  # # #### reactiveValue selected.event.levels ####
  # selected.event.levels <- shiny::reactiveValues(val = NULL)
  # shiny::observeEvent(input$event.levels, {
  #   selected.event.levels$val <- input$event.levels
  # })
  #
  # shiny::observeEvent(input$select.grouping, ignoreNULL = FALSE, {
  #   inputIMP$select.grouping <- input$select.grouping
  # })

return(list(
  select.events = shiny::reactive({input$select.events}),
  select.grouping = shiny::reactive({input$select.grouping}),
  select.sorting = shiny::reactive({input$select.sorting}),
  event.levels = shiny::reactive({input$event.levels}),
  select.subsetting = shiny::reactive({input$select.subsetting}),
  setting_file = shiny::reactive({setting_file()}),
  use_saved_settings_button = shiny::reactive({input$test})
))
}
