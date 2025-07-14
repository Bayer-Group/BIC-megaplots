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
    # shinydashboard::box(
    #   width = NULL,
    #   solidHeader = TRUE,
    #   collapsible = FALSE,
      shiny::uiOutput(ns('select.raw')),
      shiny::br(),
      DT::DTOutput(ns('rawtable'))
    )
  # )
}

#' Raw Data Module  - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param preprocess_data data list with uploaded data from module mod_data_upload
#' @param data_w_ai_information list with data including sequencing information from server
#' @param select_color character vector with color definition ("plot.bg","plot.lines","plot.wp","plot.id","axleg.bg","cont.bg")
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal
#'

raw_data_server <- function(
    input,
    output,
    session,
    preprocess_data,
    data_w_ai_information,
    select_color
  ) {

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
    rd_length <- ncol(rd)
    rd <- DT::datatable(rd,
          options = list(
            initComplete = DT::JS(
            "function(settings, json) {",
            paste0(
              "$(this.api().table().header()).css({'background-color': '",
                   select_color()['plot.bg2'],
                   "', 'color': '",
                   select_color()['plot.id'],
                   "'});"
              ),"}"
            ),
            dom = 'Brtip',
            class = 'cell-border stripe'
          ), rownames = FALSE)


        col.tabFont <- select_color()['plot.id']
        rd <- DT::formatStyle(
          table = rd,
          columns = 1:(rd_length + 1),
          target = "cell",
          color = col.tabFont,
          backgroundColor = select_color()['plot.bg'],
          border = paste0('.5px solid ', select_color()['plot.bg'])
        )

    rd
  })
}
