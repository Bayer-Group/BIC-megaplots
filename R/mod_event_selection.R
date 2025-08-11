#' Event Selection Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal

event_selection_ui <- function(id,event_number) {
  ns <- NS(id)
  shiny::column(3,
    #shiny::uiOutput(ns('select.ev')),
    shinyWidgets::pickerInput(
      inputId = ns('select.ev'),
      label = paste0("Select event (", event_number,")"),
      choices = NULL,#choices,
      selected = NULL,# choices[event_number],
      multiple = FALSE,
      options = list(
        `live-search` = TRUE,
        `style` = 'background: btn-primary',
        `header` = 'Select item'
      )
    ),
    shiny::checkboxInput(
      inputId = ns("select_ev_lev_button"),
      label = "Sort events",
      value = FALSE
    ),
    #shiny::uiOutput(ns('select_ev_lev_button')),
    shiny::conditionalPanel(condition = "input.select_ev_lev_button == true",
        shinyjqui::orderInput(
          inputId = ns("select.ev.lev"),
          label = paste0("Select order of event (", event_number,")"),
          items = NULL,
          width = "75px"
        ),
      #shiny::uiOutput(ns('select.ev.lev')),
      ns = NS(id)
    )
  )
}

#' Event Selection Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param event_number integer value between 1-4 for event number
#' @param preprocessed_data reactive object with preprocessed data
#' @param event.info reactive object with vector of character variables names
#' @param selectdata character of data upload method ("Use demo data"/"Data upload")
#'
#' @noRd
#' @keywords internal
event_selection_server <- function(input, output, session, event_number, preprocessed_data, event.info, selectdata, setting_file, use_saved_settings_button) {

  ns <- session$ns

  shiny::observeEvent(event_update$val, {
    shinyjqui::updateOrderInput(
      session,
      inputId = "select.ev.lev",
      items = setting_file()[[paste0("select.ev.lev",event_number)]]
    )
  })

  observe({
    if (length(req(event.info())) >= event_number) {
      choices <- shiny::req(event.info())
      shinyWidgets::updatePickerInput(
        inputId = 'select.ev',
        choices = choices,
        selected = choices[event_number],
      )
    }
  })

  observeEvent(c(input$select.ev,preprocessed_data()),{
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev]])
    choices <- sort(choices[!is.na(choices)])
    shinyjqui::updateOrderInput(
      session,
      inputId = "select.ev.lev",
      items = choices
    )
  })

  # output$select_ev_lev_button <- shiny::renderUI({
  #    if (length(req(event.info())) >= event_number) {
  #     shiny::req(input$select.ev)
  #     shiny::checkboxInput(
  #       inputId = ns("select_ev_lev_button"),
  #       label = "Sort events",
  #       value = FALSE
  #     )
  #    }
  # })

  event_update <- shiny::reactiveValues(val = 0)

  shiny::observeEvent(use_saved_settings_button(), {
    if (!is.null(setting_file())) {
      if (is.list(setting_file())) {
        shinyWidgets::updatePickerInput(
          session,
          inputId = 'select.ev',
          selected = setting_file()[[paste0("select.ev",event_number)]]
        )
        event_update$val <- event_update$val + 1
      }
    }

  })

  shiny::observeEvent(use_saved_settings_button(), {
    if (!is.null(setting_file())) {
      if (is.list(setting_file())) {
        choices <- setting_file()[[paste0("select.ev.lev",event_number)]]
        shinyjqui::updateOrderInput(
          session,
          inputId = "select.ev.lev",
          items = choices
        )
      }
    }
  })
 return(
    list(
      select.ev = shiny::reactive({input$select.ev}),
      select.ev.lev = shiny::reactive({input$select.ev.lev}),
      select_ev_lev_button= shiny::reactive({input$select_ev_lev_button}),
      event_update = shiny::reactive({event_update$val})
    )
  )
}
