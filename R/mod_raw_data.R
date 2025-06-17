#' Raw Data Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

raw_data_ui <- function(id) {

  ns <- NS(id)

shiny::tagList(
    shiny::br(),
    shiny::uiOutput(ns('select.raw')),
    shiny::br(),
    DT::DTOutput(ns('rawtable'))
  )
}

#' Raw Data Module  - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal
#'

raw_data_server <- function(input, output, session, preprocess_data, data_w_ai_information) {

  ns <- session$ns

   output$select.raw <- shiny::renderUI({
    choices <- names(req(preprocess_data()$megaplot_data))[c(1, 2)]
    shinyWidgets::pickerInput(
      inputId = ns('select.raw'),
      label = 'Select data set based on main options:',
      choices = choices,
      width = 'fit',
      multiple = FALSE,
      selected = choices[1],
      options = list(`style` = 'background: btn-primary')
    )
  })

  # create raw data table as UI output (based on data_grouped_and_sorted() user selection)
  rawData <- shiny::reactive({
    tmp <- shiny::req(data_w_ai_information())
    tmp[[req(input$select.raw)]]
  })

  output$rawtable = DT::renderDT({
    rd <- shiny::req(rawData())
    rd <- rd[, setdiff(colnames(rd), c('Group_ID', 'Group_ID_char', 'subject'))]
    rd <- DT::datatable(rd, rownames = FALSE)
    rd
  })
}
