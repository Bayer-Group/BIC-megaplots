#' UI for the plot appearance module
#'
#' Renders all plot appearance controls inside a [bslib::accordion_panel()].
#' Intended to be placed inside the sidebar accordion in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::accordion_panel()] containing the plot appearance
#'   interface.
#' @keywords internal
mod_plot_appearance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  bslib::accordion_panel(
    "Line appearance",
    icon = bsicons::bs_icon("border-width"),
    #orderInput to change event group order (influences which event group
    # is plotted first and thus may overlap others events)
    shinyjqui::orderInput(
      inputId = ns("sort_event_groups"),
      label = "Line Plotting Order",
      items = NULL,
      width = 300
    ),
    # colourInput for subject line
    colourpicker::colourInput(
      inputId = ns("line_color_subjects_dark"),
      label = HTML(paste0("Time line color  ",icon = bsicons::bs_icon("moon"))),
      value = "#454545",
      allowTransparent = TRUE
    ),
    colourpicker::colourInput(
      inputId = ns("line_color_subjects_light"),
      label = HTML(paste0("Time line color  ",icon = bsicons::bs_icon("sun"))),
      value = "#bababa",
      allowTransparent = TRUE
    ),
    # sliderInput to adjust subject line width
    shiny::sliderInput(
      inputId = ns("line_width_subjects"),
      label = "Time line width",
      min = 1,
      max = 10,
      value = 1,
      step = 0.5
    ),
    # sliderInput to adjust event line width
    shiny::sliderInput(
      inputId = ns("line_width"),
      label = "Event line width",
      min = 1,
      max = 5,
      value = 3,
      step = 0.5
    )#,
    # # radioButton change hover window style
    # shiny::radioButtons(
    #   inputId = ns("event_summary_hovermode"),
    #   label = "Hover mode (Event Summary)",
    #   choices = c("One label for each event" = "x", "One label for all events" = "x unified"),
    #   inline = TRUE,
    #   selected = "x"
    # ),
    # shiny::numericInput(
    #   inputId = ns("event_summary_cutoff"),
    #   label = "Display hover for counts greater than or equal to:",
    #   value = 1,
    #   min = 1,
    #   max = NA,
    #   step = 1
    # )
    ),
    bslib::accordion_panel(
      "Legend appearance",
      icon = bsicons::bs_icon("card-list"),
    # prettySwitch to turn on/off legend grouping option
      shinyWidgets::prettySwitch(
        inputId = ns("switch_legend_grouping"),
        label = "On/Off Legend Grouping",
        value = TRUE,
        status = "primary"
      )
    )
  )
}


#' Server logic for the plot appearance module
#'
#' Exposes all plot appearance input values as reactives for consumption
#' by the parent server. The `sort_event_groups` orderInput items are
#' managed externally by [mod_color_selection_server()] via
#' [shinyjqui::updateOrderInput()] — this module only owns the widget
#' and exposes its current value.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A named list with six reactive elements:
#'   \describe{
#'     \item{`line_width`}{A reactive returning the event line width.}
#'     \item{`line_width_subjects`}{A reactive returning the subject line
#'       width.}
#'     \item{`switch_legend_grouping`}{A reactive returning the legend
#'       grouping toggle state.}
#'     \item{`sort_event_groups`}{A reactive returning the current event
#'       group order as a character vector.}
#'     \item{`event_summary_hovermode`}{A reactive returning the selected
#'       hover mode string.}
#'     \item{`event_summary_cutoff`}{A reactive returning the hover count
#'       cutoff value.}
#'   }
#' @keywords internal
mod_plot_appearance_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {


    list(
      line_width = shiny::reactive({
        input$line_width %||% 3
      }),
      line_width_subjects = shiny::reactive({
        input$line_width_subjects %||% 1
      }),
      switch_legend_grouping = shiny::reactive({
        # Fallback to TRUE matches the UI default value = TRUE
        input$switch_legend_grouping %||% TRUE
      }),
      sort_event_groups = shiny::reactive({
        input$sort_event_groups %||% character(0)
      }),
      # event_summary_hovermode = shiny::reactive({
      #   input$event_summary_hovermode %||% "x"
      # }),
      # event_summary_cutoff = shiny::reactive({
      #   input$event_summary_cutoff %||% 1L
      # }),
      line_color_subjects_dark = shiny::reactive({
        input$line_color_subjects_dark %||% "#454545"
      }),
      line_color_subjects_light = shiny::reactive({
        input$line_color_subjects_light %||% "#bababa"
      })
    )
  })
}
