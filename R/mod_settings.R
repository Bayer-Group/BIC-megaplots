#' Settings Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

settings_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("range"),
      label = "Zoom",
      min = 0,
      max = 100,
      value = c(0, 100),
      step = 1
    ),

    shiny::sliderInput(
      inputId = ns("height_slider"),
      label = "Change height ratio",
      min = 0.1,
      max = 2,
      value = 1
    ),
    shiny::sliderInput(
      inputId = ns("thick"),
      label = "Thickness of subject lines",
      min = 0.05,
      max = 0.4,
      value = 0.1,
      step = 0.05
    ),
    shiny::checkboxInput(
      inputId = ns('inc.ev.subj'),
      label = 'Color subject line by first selected event (for \'continuous\' event)',
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId = ns("lines_instead_symbols"),
      label = "Use lines for all events",
      value = FALSE
    ),
    shiny::conditionalPanel(condition = "input.lines_instead_symbols == true",
      shiny::radioButtons(
        inputId = ns("lines_options"),
        label = "Select line(s) appearance",
        choices = c("Adjacent", "Overlaying")
      ),
        HTML(
         "<p> Note: Some events might not be visible</p>"
        ),
        HTML(
         "<p> for option 'Overlaying'!</p>"
        ),
      ns=NS(id)
    ),
    shiny::checkboxInput(
      inputId = ns('det.xaxt'),
      label = 'Detailed tickmarks on the x-axis',
      value = TRUE
    ),
    # shiny::checkboxInput(
    #   inputId = ns('incr.font'),
    #   label = 'Increase font size',
    #   value = FALSE
    # ),
    shiny::checkboxInput(
      inputId = ns('background_stripes'),
      label = 'Add background stripes',
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.background_stripes == true",
      shiny::numericInput(
        inputId = ns("background.stripes.length"),
        label = "Length of background stripes",
        value = 7,
        min = 1
      ),
      ns=NS(id)
    ),
    shiny::checkboxInput(
      inputId = ns('reference_line_1'),
      label = 'Add reference line',
      value = TRUE
    ),
    shiny::conditionalPanel(
      condition = "input.reference_line_1 == true",
      shiny::numericInput(
        inputId = ns("reference_line_1_value"),
        label = "Reference line (1)",
        value = 0
      ),
      shiny::checkboxInput(
        inputId = ns('reference_line_2'),
        label = 'Add second reference line',
        value = FALSE
      )
      ,ns = NS(id)
    ),
    shiny::conditionalPanel(condition = "input.reference_line_2 == true",
      shiny::numericInput(
         inputId = ns("reference_line_2_value"),
        label = "Reference line (2)",
        value = 0
      ),
      shiny::checkboxInput(
        inputId = ns('reference_line_3'),
        label = 'Add third reference line',
        value = FALSE
      ),
      ns = NS(id)
    ),
    shiny::conditionalPanel(condition = "input.reference_line_3 == true",
      shiny::numericInput(
         inputId = ns("reference_line_3_value"),
        label = "Reference line (3)",
        value = 0
      ),
      ns = NS(id)
    ),
    shiny::textInput(
      inputId = ns("y_axis_label"),
      label = "y axis label",
      value = "Subject identifier",
      placeholder = "Subject identifier"
    ),
    shiny::textInput(
      inputId = ns("x_axis_label"),
      label = "x axis label",
      value = "Time",
      placeholder = "Time"
    ),
    shiny::tags$br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset_draggable_panel_positions"),
      label = "Reset panel positions",
      style = "gradient",
      color = "primary",
      size = 'xs',
      no_outline = FALSE,
      icon = icon("refresh")
    ),
    shiny::tags$br()
  )
}


#' Settings Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal

settings_server <- function(input, output, session, data_w_event_and_group_information, setting_file) {

  ns <- session$ns


   shiny::observeEvent(data_w_event_and_group_information(), {

    shiny::req(data_w_event_and_group_information())
      min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
      max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)

    shiny::updateSliderInput(
      session,
      inputId = "range",
      min = min1,
      max = max1,
      value = c(min1, max1)
    )

    # newtab <- switch(ns(input$sidebarmenu), "dashboard")
    # shinydashboard::updateTabItems(session, "sidebarmenu", newtab)
  })

    settings <- shiny::reactive({
    param <- list(
      range = input$range,
      height_slider = input$height_slider,
      thick = input$thick,
      inc.ev.subj = input$inc.ev.subj,
      lines_instead_symbols = input$lines_instead_symbols,
      lines_options = input$lines_options,
      det.xaxt = input$det.xaxt,
      # incr.font = input$incr.font,
      background_stripes= input$background_stripes,
      background.stripes.length = input$background.stripes.length,
      reset_draggable_panel_positions = input$reset_draggable_panel_positions,
      reference_line_1 = input$reference_line_1,
      reference_line_1_value = input$reference_line_1_value,
      reference_line_2 = input$reference_line_2,
      reference_line_2_value = input$reference_line_2_value,
      reference_line_3 =input$reference_line_3,
      reference_line_3_value = input$reference_line_3_value,
      y_axis_label = input$y_axis_label,
      x_axis_label = input$x_axis_label
    )
    param
  })


  shiny::observeEvent(setting_file(), {
    if (!is.null(setting_file())) {
      saved_file <- readRDS(setting_file()$datapath)
      if (is.list(saved_file)) {
        #update main options
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = saved_file$range
        )
        shiny::updateSliderInput(
          session,
          inputId = "height_slider",
          value = saved_file$height_slider
        )
        shiny::updateSliderInput(
          session,
          inputId = "thick",
          value = saved_file$thick
        )
        updateCheckboxInput(
          session,
          inputId = "inc.ev.subj",
          value = saved_file$inc.ev.subj
        )
        updateCheckboxInput(
          session,
          inputId = "lines_instead_symbols",
          value = saved_file$lines_instead_symbols
        )
        updateCheckboxInput(
          session,
          inputId = "det.axt",
          value = saved_file$det.axt
        )
        updateCheckboxInput(
          session,
          inputId = "background_stripes",
          value = saved_file$background_stripes
        )
        updateCheckboxInput(
          session,
          inputId = "reference_line_1",
          value = saved_file$reference_line_1
        )
        updateCheckboxInput(
          session,
          inputId = "reference_line_2",
          value = saved_file$reference_line_2
        )
        updateCheckboxInput(
          session,
          inputId = "reference_line_3",
          value = saved_file$reference_line_3
        )
        updateRadioButtons(
          session,
          inputId = "lines_options",
          selected = saved_file$lines_options
        )
        updateNumericInput(
          session,
          inputId = "background.stripes.length",
          value = saved_file$background.stripes.length
        )
        updateNumericInput(
          session,
          inputId = "reference_line_1_value",
          value = saved_file$reference_line_1_value
        )
        updateNumericInput(
          session,
          inputId = "reference_line_2_value",
          value = saved_file$reference_line_2_value
        )
        updateNumericInput(
          session,
          inputId = "reference_line_3_value",
          value = saved_file$reference_line_3_value
        )
        updateTextInput(
          session,
          inputId = "y_axis_label",
          value = saved_file$y_axis_label
        )
        updateTextInput(
          session,
          inputId = "x_axis_label",
          value = saved_file$x_axis_label
        )
      }
    }
  })

  return(list(
    # range = shiny::reactive({input$range}),
    # height_slider = shiny::reactive({input$height_slider}),
    # thick = shiny::reactive({input$thick }),
    # inc.ev.subj = shiny::reactive({input$inc.ev.subj}),
    # lines_instead_symbols = shiny::reactive({input$lines_instead_symbols}),
    # lines_options = shiny::reactive({input$lines_options}),
    # det.xaxt = shiny::reactive({input$det.xaxt}),
    # # incr.font = shiny::reactive({input$incr.font}),
    # background_stripes= shiny::reactive({input$background_stripes}),
    # background.stripes.length = shiny::reactive({input$background.stripes.length}),
    # reset_draggable_panel_positions = shiny::reactive({input$reset_draggable_panel_positions}),
    # reference_line_1 = shiny::reactive({input$reference_line_1}),
    # reference_line_1_value = shiny::reactive({input$reference_line_1_value}),
    # reference_line_2 = shiny::reactive({input$reference_line_2}),
    # reference_line_2_value = shiny::reactive({input$reference_line_2_value}),
    # reference_line_3 = shiny::reactive({input$reference_line_3}),
    # reference_line_3_value = shiny::reactive({input$reference_line_3_value}),
    # y_axis_label = shiny::reactive({input$y_axis_label}),
    # x_axis_label = shiny::reactive({input$x_axis_label}),

    settings = shiny::reactive({settings()})
  ))

}
