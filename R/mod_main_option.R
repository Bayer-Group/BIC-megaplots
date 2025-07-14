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
            )#,
            # shiny::helpText(
            #   'Please upload the data set of the last session which
            #   was saved via the "Save Session Settings"-button
            #   in the MegaPlot-tab.'
            # )
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
    color_options,
    var,
    par,
    sermethod
  ){

  ns <- session$ns

  #### MAIN OPTIONS ####

  ####... select.events ####
  shiny::observeEvent(c(data_w_event_and_group_information()), {

    shiny::req(data_w_event_and_group_information())

    choices <- shiny::isolate(data_w_event_and_group_information()$event)
    if (is.null(isolate(data_w_event_and_group_information()$event))) {
      selected <- choices
    } else {
      selected <- shiny::isolate(data_w_event_and_group_information()$event)
    }

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

#
  shiny::observeEvent(choiceSort(), {
    choices <- choiceSort()

    # selected <- 'megaplots_selected_subjectid'
    # if (any(choices == 'SEQUENCING')) selected <- utils::tail(choices, 1)

    if (!is.null(input$setting_file)) {
      saved_file <- readRDS(input$setting_file$datapath)
      if (is.list(saved_file)) {
        selected <- saved_file$select.sorting
      }} else {
        if (any(choices == 'SEQUENCING')) {
          selected <- utils::tail(choices, 1)
        } else {
          selected <- "megaplots_selected_subject"
        }
      }
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.sorting",
      choices = choices,
      selected = selected
    )
  })

  ####... event.levels ####

  shiny::observeEvent(data_w_event_and_group_information(), {

    choices <- unlist(data_w_event_and_group_information()$event.lev,
                      use.names = FALSE)
    tmp <- data_w_event_and_group_information()$event.lev.n
    choices.lab <- rep(data_w_event_and_group_information()$event,
                       tmp)
    choices.sym <- rep('glyphicon-cloud',
                       length(choices.lab))
    choices.col <- paste('color:',
                         unlist(data_w_event_and_group_information()$col.ev[data_w_event_and_group_information()$event],
                                use.names = FALSE))
    choices <- paste0(choices.lab, ' = ', choices)

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


  ####... select.subsetting ####
    shiny::observeEvent(data_w_ai_information(), {
    shiny::req(data_w_ai_information())

    choices <- unlist(data_w_ai_information()$group.lev, use.names = FALSE)
    choices.lab <- rep(data_w_ai_information()$group, sapply(data_w_ai_information()$group.lev, FUN = length))
    choices <- paste0(choices.lab, ' = ', choices)

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
       select.grouping = input$select.grouping,
       select.sorting = input$select.sorting,
       select.sorting.choices = choiceSort(),
       select.event.levels = input$event.levels,
       select.subsetting = input$select.subsetting
    )
    param
  })


  shiny::observeEvent(input$setting_file, {
    if (!is.null(input$setting_file)) {
      saved_file <- readRDS(input$setting_file$datapath)
      if (is.list(saved_file)) {
        #update main options
        shiny::updateSelectizeInput(
          session,
          inputId = "select.events",
          selected = saved_file$select.events
        )
        shiny::updateSelectizeInput(
          session,
          inputId = "select.grouping",
          selected = saved_file$select.grouping,
        )
        shinyWidgets::updatePickerInput(
          session,
          inputId = "event.levels",
          selected = saved_file$select.event.levels
        )
        shinyWidgets::updatePickerInput(
          session,
          inputId = "select.subsetting",
          selected = saved_file$select.subsetting
        )
        # shinyWidgets::updatePickerInput(
        #   session,
        #   inputId = "select.sorting",
        #   selected = saved_file$select.sorting#,
        #   #choices = saved_file$select.sorting.choices
        # )

        # shinyWidgets::updatePickerInput(
        #   session,
        #   inputId = "select.sorting",
        #   selected = saved_file$select.sorting,
        #   choices = saved_file$select.sorting.choices
        # )
      }
    }
  })

  output$save_settings <- shiny::downloadHandler(
    filename = function() {
      paste("Megaplot_Session", gsub(":", "-", Sys.time()), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(
        c(main_option_settings(),displayed_subjects_settings(),settings(),color_options(),list(par = par()),list(var = var()),list(sermethod = sermethod()))
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
  setting_file = shiny::reactive({input$setting_file})
))
}
