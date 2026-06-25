#' UI for the reference lines module
#'
#' Renders the reference rectangle controls inside a
#' [bslib::accordion_panel()]. Up to three reference rectangles can be
#' configured, each revealed by checking the previous rectangle's checkbox.
#' Intended to be placed inside the sidebar accordion in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::accordion_panel()] containing the reference rectangle
#'   interface.
#' @keywords internal
mod_reference_lines_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::accordion_panel(
    "Reference rectangles",
    icon = bsicons::bs_icon("align-center"),
    #checkboxInput to add a reference line or rectangle
    shiny::checkboxInput(
      inputId = ns('reference_line_1'),
      label = 'Add reference rectangle',
      value = FALSE
    ),
    #action button to update reference line selection
    shiny::conditionalPanel(
      condition = paste0("input['", ns("reference_line_1"), "'] == true"),
      shinyWidgets::actionBttn(
        inputId = ns("update_reference_lines"),
        label = "Update Reference rectangles",
        color = "success",
        style = "simple",
        icon = icon("refresh")
      ),
      #colourInput to adjust color of first reference line/rectangle
      colourpicker::colourInput(
        inputId = ns("reference_line_1_color"),
        label = "Reference rectangle color",
        value = "#fe333f20",
        allowTransparent = TRUE
      ),
      shiny::numericInput(
        inputId = ns("reference_line_1_value"),
        label = "Reference rectangle x1",
        value = 0
      ),
      shiny::numericInput(
        inputId = ns("reference_line_1_value2"),
        label = "Reference rectangle x2",
        value = 0
      ),
      shiny::checkboxInput(
        inputId = ns('reference_line_2'),
        label = 'Add second reference line',
        value = FALSE
      )
    ),
    shiny::conditionalPanel(
      condition = paste0(
        "input['", ns("reference_line_1"), "'] == true && ",
        "input['", ns("reference_line_2"), "'] == true"
      ),
      #colourInput to adjust color of first reference line/rectangle
      colourpicker::colourInput(
        inputId = ns("reference_line_2_color"),
        label = "Reference rectangle color",
        value = "#fe333f20",
        allowTransparent = TRUE
      ),
      shiny::numericInput(
        inputId = ns("reference_line_2_value"),
        label = "Reference rectangle x1",
        value = 0
      ),
      shiny::numericInput(
        inputId = ns("reference_line_2_value2"),
        label = "Reference rectangle x2",
        value = 0
      ),
      shiny::checkboxInput(
        inputId = ns('reference_line_3'),
        label = 'Add third reference line',
        value = FALSE
      )
    ),
    shiny::conditionalPanel(
      condition = paste0(
        "input['", ns("reference_line_1"), "'] == true && ",
        "input['", ns("reference_line_2"), "'] == true && ",
        "input['", ns("reference_line_3"), "'] == true"
      ),
      colourpicker::colourInput(
        #colourInput to adjust color of first reference line/rectangle
        inputId = ns("reference_line_3_color"),
        label = "Reference rectangle color",
        value = "#fe333f20",
        allowTransparent = TRUE
      ),
      shiny::numericInput(
        inputId = ns("reference_line_3_value"),
        label = "Reference rectangle x1",
        value = 0
      ),
      shiny::numericInput(
        inputId = ns("reference_line_3_value2"),
        label = "Reference rectangle x2",
        value = 0
      ),
    )
  )

}

mod_reference_lines_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    reference_lines_reactive <- shiny::reactiveValues(
      reference_line_1 = FALSE,
      reference_line_2 = FALSE,
      reference_line_3 = FALSE,
      reference_line_1_value = NULL,
      reference_line_2_value = NULL,
      reference_line_3_value = NULL,
      reference_line_1_value2 = NULL,
      reference_line_2_value2 = NULL,
      reference_line_3_value2 = NULL,
      reference_line_1_color = NULL,
      reference_line_2_color = NULL,
      reference_line_3_color = NULL
    )

    shiny::observeEvent(input$reference_line_1, {
      if (!input$reference_line_1) {
        reference_lines_reactive$reference_line_1 <- FALSE
      }
    })

    shiny::observeEvent(input$update_reference_lines, {
      reference_lines_reactive$reference_line_1 <- shiny::isolate(input$reference_line_1)
      reference_lines_reactive$reference_line_2 <- shiny::isolate(input$reference_line_2)
      reference_lines_reactive$reference_line_3 <- shiny::isolate(input$reference_line_3)
      reference_lines_reactive$reference_line_1_value <- shiny::isolate(input$reference_line_1_value)
      reference_lines_reactive$reference_line_2_value <- shiny::isolate(input$reference_line_2_value)
      reference_lines_reactive$reference_line_3_value <- shiny::isolate(input$reference_line_3_value)
      reference_lines_reactive$reference_line_1_value2 <- shiny::isolate(input$reference_line_1_value2)
      reference_lines_reactive$reference_line_2_value2 <- shiny::isolate(input$reference_line_2_value2)
      reference_lines_reactive$reference_line_3_value2 <- shiny::isolate(input$reference_line_3_value2)
      reference_lines_reactive$reference_line_1_color <- shiny::isolate(input$reference_line_1_color)
      reference_lines_reactive$reference_line_2_color <- shiny::isolate(input$reference_line_2_color)
      reference_lines_reactive$reference_line_3_color <- shiny::isolate(input$reference_line_3_color)
    })

    #return
    list(reference_lines = reference_lines_reactive)
  })
}
