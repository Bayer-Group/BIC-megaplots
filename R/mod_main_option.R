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
      shiny::column(
        3,
        shiny::selectizeInput(
          inputId = ns('select.events'),
          label = HTML('<p style ="color:white;"> Select events</p>'),
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list('plugins' = list('remove_button', 'drag_drop'))
        ),
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
      shiny::column(3,
        shiny::selectizeInput(
          inputId = ns('select.grouping'),
          label = HTML('<p style ="color:white;"> Select grouping </p>'),
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list('plugins' = list('remove_button', 'drag_drop'))
        ),
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
      shiny::column(3,
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
        ),
        "Save settings as .rds file:",
        shinyWidgets::downloadBttn(
          outputId = ns("save_setting2"),
          label = "Save Session Settings",
          style = 'gradient',
          color = 'primary',
          size = 'sm',
          no_outline = FALSE,
          block = FALSE
        )
      ),
    )
    # ,
    # shiny::tags$head(
    #   tags$style(
    #     HTML(
    #       '.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'
    #     )
    #   )
    # ),
    # shiny::uiOutput(ns('hover_legend')),
    # shiny::uiOutput(ns('megaplot')),
    # fixedPanel(
    #   shiny::uiOutput(ns('axisbox')),
    #   bottom = 1,
    #   width = "100%",
    #   height = 42
    #
    # ),
    # shiny::uiOutput(ns('hoverpanel')),
    # shiny::uiOutput(ns('summarypanel')),
    # shiny::conditionalPanel(condition = "output.check_slider_used == true",
    #   shiny::uiOutput('next_buttons')
    # )
  )
}


#' Megaplot Main Option Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param data_w_event_and_group_information
#'
#' @return List with 'Main option'-inputs
#'
#' @noRd
#' @keywords internal

main_option_server <- function(input, output, session, data_w_event_and_group_information,data_w_ai_information, selectdata,seq.button){

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

    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   selected <- uploaded_files$preprocess_data()$megaplot_data$saved$select.events
    # }
    shiny::updateSelectizeInput(
      session,
      inputId = "select.events",
      choices = choices,
      selected = selected
    )
  })

  ####... select.grouping ####
  choiceGroup <- shiny::eventReactive(c(data_w_event_and_group_information()), {
    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   uploaded_files$preprocess_data()$megaplot_data$saved$select.grouping
    # } else {
      data_w_event_and_group_information()$group
    # }
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

  # shiny::observeEvent(input$select.grouping, ignoreNULL = FALSE, {
  #   inputIMP$select.grouping <- input$select.grouping
  # })

  ####... select.sorting ####
  choiceSort <- shiny::eventReactive(c(data_w_ai_information(), seq.button()), {
    data_w_ai_information()$nume_A[!is.na(data_w_ai_information()$nume_A)]
  })


  shiny::observeEvent(choiceSort(), {
    choices <- choiceSort()

    selected <- 'megaplots_selected_subjectid'
    if (any(choices == 'SEQUENCING'))
      selected <- utils::tail(choices, 1)

    # if (uploaded_files$selectdata()== 'Upload saved data') {
    #   selected <- uploaded_files$preprocess_data()$megaplot_data$saved$select.sorting
    # }
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.sorting",
      choices = choices,
      selected = selected
    )
  })

  ####... event.levels ####

  shiny::observeEvent(data_w_event_and_group_information(), {
   # inputIMP$name <- data_w_event_and_group_information()$megaplot_data$name

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

    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   selected <- uploaded_files$preprocess_data()$megaplot_data$saved$event.levels
    # }

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

    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   choices <-
    #     unlist(uploaded_files$preprocess_data()$megaplot_data$saved$group.lev, use.names = FALSE)
    #   choices.lab <-
    #     rep(
    #       uploaded_files$preprocess_data()$megaplot_data$saved$group,
    #       sapply(uploaded_files$preprocess_data()$megaplot_data$saved$group.lev, FUN = length)
    #     )
    #   choices <- paste0(choices.lab, ' = ', choices)
    #
    #   selected <- uploaded_files$preprocess_data()$megaplot_data$saved$select.subsetting
    # } else {
      selected <- choices
    # }

    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.subsetting",
      choices = choices,
      selected = selected
    )
  })


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
  select.subsetting = shiny::reactive({input$select.subsetting})
))
}
