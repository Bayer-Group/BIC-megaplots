#' Event selection ui function
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
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

#' Seriation Server Function
#'
#' @param id Shiny Session id
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#' @keywords internal
event_selection_server <- function(input, output, session, event_number, preprocessed_data, event.info, selectdata) {

  ns <- session$ns

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
    if (selectdata()== "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev
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
