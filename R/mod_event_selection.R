#' Event Selection Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal

event_selection_ui <- function(id) {
  ns <- NS(id)
  shiny::column(3,
    shiny::uiOutput(ns('select.ev')),
    shiny::uiOutput(ns('select_ev_lev_button')),
    shiny::conditionalPanel(condition = "input.select_ev_lev_button == true",
      shiny::uiOutput(ns('select.ev.lev')),
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
event_selection_server <- function(input, output, session, event_number, preprocessed_data, event.info, selectdata,setting_file) {

  ns <- session$ns

  #   shiny::observeEvent(setting_file(), {
  #   if (!is.null(setting_file())) {
  #     saved_file <- readRDS(setting_file()$datapath)
  #     if (is.list(saved_file)) {
  #
  #
  #     }
  #   }
  # })


  output$select.ev <- shiny::renderUI({
    if (length(req(event.info())) >= event_number) {
      choices <- shiny::req(event.info())
      uiElement <- shinyWidgets::pickerInput(
        inputId = ns('select.ev'),
        label = paste0("Select event (", event_number,")"),
        choices = choices,
        selected = choices[event_number],
        multiple = FALSE,
        options = list(
          `live-search` = TRUE,
          `style` = 'background: btn-primary',
          `header` = 'Select item'
        )
      )
    }
  })

  output$select.ev.lev <- shiny::renderUI({
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev]])
    choices <- sort(choices[!is.na(choices)])

    if (!is.null(setting_file())) {
      saved_file <- readRDS(setting_file()$datapath)
      if (is.list(saved_file)) {
        choices <- saved_file$select.event.levels[saved_file$select.event.levels %in% paste(input$select.ev,"=",choices)]
        choices <- unlist(strsplit(paste(input$select.ev,"=",choices)," = "))[!unlist(strsplit(paste(input$select.ev,"=",choices)," = ")) %in% input$select.ev]
      }
    }

    shinyjqui::orderInput(
      inputId = ns("select.ev.lev"),
      label = paste0("Select order of event (", event_number,")"),
      items = choices,
      width = "75px"
    )
  })

  shiny::outputOptions(output, "select.ev.lev", suspendWhenHidden = FALSE)

  output$select_ev_lev_button <- shiny::renderUI({
     if (length(req(event.info())) >= event_number) {
      shiny::req(input$select.ev)
      shiny::checkboxInput(
        inputId = ns("select_ev_lev_button"),
        label = "Sort events",
        value = FALSE
      )
     }
  })

 return(
    list(
      select.ev = shiny::reactive({input$select.ev}),
      select.ev.lev = shiny::reactive({input$select.ev.lev}),
      select_ev_lev_button= shiny::reactive({input$select_ev_lev_button})
    )
  )
}
