#' Artifical Intelligence Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

artificial_intelligence_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shinyWidgets::pickerInput(
        inputId = ns('methSer'),
        label = 'Seriation method',
        choices = sort(
          c(
            'GW_average',
            'GW_complete',
            'GW_single',
            'GW_ward',
            'HC_single',
            'HC_average',
            'HC_complete',
            'HC_ward',
            'OLO_average',
            'OLO_complete',
            'OLO_single',
            'OLO_ward',
            'VAT',
            'TSP',
            'ARSA'
          )
        ),
        selected = 'GW_average',
        multiple = FALSE,
        options = list(
          `live-search` = TRUE,
          `header` = 'Select item'
        )
      ),
      shinyWidgets::pickerInput(
        inputId = ns('varSeq'),
        label = 'Sequencing Variables',
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'All dropped!'
        )
      ),
      conditionalPanel(condition = "input.varSeq.length > 1",
        shiny::checkboxInput(
          inputId = ns('multiple_distmeasures'),
          label = 'Use different distance measures for variables',
          value = FALSE
        ),
        ns = NS(id)
      ),
      seriation_ui(ns("parametersModule")),
      shinyWidgets::actionBttn(
        inputId = ns('seq.button'),
        label = 'Apply sequencing',
        style = 'gradient',
        color = 'primary',
        size = 'xs',
        no_outline = FALSE
      ),
      shiny::br(),
      shiny::actionLink(
        "link_to_pdf_view",
        "Information",
        icon = icon('circle-info', lib = 'font-awesome')
      ),
      shiny::uiOutput(ns("pdfview")),
      shiny::span(shiny::HTML(gsub('\n','<br/>',stringr::str_wrap(
        '(will appear in the sorting menu and replace the current sorting selection)',
        width = 30)
      ))),
      shiny::br()
    )
}

#' Artifical Intelligence Module  - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param preprocess_data list with uploaded data from module mod_data_upload
#' @param selectdata character of data upload method ("Use demo data"/"Data upload")
#' @param data_w_event_and_group_information list with grouped data information from server
#' @param setting_file list with saved settings information
#' @param
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal
#'

artificial_intelligence_server <- function(
    input,
    output,
    session,
    preprocess_data,
    selectdata,
    data_w_event_and_group_information,
    setting_file
  ) {


  ns <- session$ns

    # information-pdf for Seriation
  shiny::observeEvent(input$link_to_pdf_view, {
    js <- 'window.open("www/Megaplots_Seriation_Manual.pdf", "_blank", "height=700,width=1120");'
    shinyjs::runjs(js)
  })

  # call seriation module
  input_seriation <- seriation_server(
    "parametersModule",
    shiny::reactive({input$varSeq}),
    shiny::reactive({input$multiple_distmeasures}),
    shiny::reactive({selectdata()}),
    shiny::reactive({input$seq.button}),
    shiny::reactive({setting_file()})
  )

   collectSeq <- shiny::reactiveValues(
    varSeq = NULL,
    methSer = NULL
  )

  shiny::observeEvent(input$seq.button, {
    collectSeq$varSeq <- input$varSeq
    collectSeq$methSer <- input$methSer
  })


  # update sequencing pickerinput
  shiny::observeEvent(data_w_event_and_group_information(), {
    choices <- data_w_event_and_group_information()$event
    selected <- data_w_event_and_group_information()$event[1]

    shinyWidgets::updatePickerInput(
      session,
      inputId = "varSeq",
      choices = choices,
      selected = selected
    )
  })


    shiny::observeEvent(setting_file(), {
      if (!is.null(setting_file())) {
        saved_file <- readRDS(setting_file()$datapath)
        if (is.list(saved_file)) {

            shinyWidgets::updatePickerInput(
              session,
              inputId = "varSeq",
              selected = saved_file$var
            )

            shinyWidgets::updatePickerInput(
              session,
              inputId ='methSer',
              label = 'Seriation method',
              selected = saved_file$sermethod
            )
        }
      }
    })

  return(list(
    varSeq = shiny::reactive({collectSeq$varSeq}),
    methSer = shiny::reactive({collectSeq$methSer}),
    input_seriation = shiny::reactive({input_seriation$input_seriation()}),
    seq.button = shiny::reactive({input$seq.button}),
    multiple_distmeasures = shiny::reactive({input$multiple_distmeasures})
  ))
}
