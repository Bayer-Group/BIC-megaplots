#' UI for the filter module
#'
#' Renders the filter variable picker and the `datamods` filter panel.
#' Intended to be placed as a [bslib::accordion_panel()] inside the sidebar
#' accordion in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::accordion_panel()] containing the filter interface.
#' @keywords internal
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::accordion_panel(
    "Filter",
    icon = bsicons::bs_icon("filter"),
    shinyWidgets::pickerInput(
      inputId =ns("select_filter_variables"),
      label = "Select filter variable(s)",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = 'count > 0',
        `count-selected-text` = '{0} selected (of {1})',
        `live-search` = TRUE,
        `style` = 'background: btn-primary',
        `header` = 'Please select variable(s) for filter',
        `none-selected-text` = 'All dropped!'
      )
    ),
    shiny::conditionalPanel(condition = paste0("output['", ns("filter_enabled"), "'] == true"),
      datamods::filter_data_ui(ns("filtering"), max_height = "500px")
    )
  )


}




#' Server logic for the filter module
#'
#' Manages filter variable selection, delegates to
#' [datamods::filter_data_server()], persists filter values across variable
#' changes, and returns the filtered data frame as a reactive.
#'
#' @param id Character string. The module namespace identifier.
#' @param uploaded_data_renamed A reactive data frame with standardised
#'   `megaplots_selected_*` column names, as returned by
#'   [mod_data_upload_server()].
#' @param parent_session The parent Shiny session object, used to update
#'   the progress bar which lives outside this module's namespace.
#'
#' @return A named list with one element:
#'   \describe{
#'     \item{`filtered_data`}{A reactive returning the filtered data frame,
#'       or `NULL` if no data has been uploaded yet.}
#'   }
#' @keywords internal
mod_filter_server <- function(id, uploaded_data_renamed, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {

    stopifnot(
      "mod_filter_server expects `uploaded_data_renamed` to be a reactive." = {
        shiny::is.reactive(uploaded_data_renamed)
      }
    )

    shiny::observeEvent(uploaded_data_renamed(), {
      shinyWidgets::updatePickerInput(
        inputId ="select_filter_variables",
        choices = colnames(uploaded_data_renamed()),
        selected = NULL
      )
    })

    output$filter_enabled <- shiny::reactive({
      !is.null(input$select_filter_variables)
    })

    outputOptions(output, "filter_enabled", suspendWhenHidden = FALSE)

    savedFilterValues <- reactiveVal()

    shiny::observeEvent(input$select_filter_variables, {
      savedFilterValues <<- res_filter$values()
    },ignoreInit = TRUE)

    defaults <- shiny::reactive({
      #input$load_filter_values
      input$select_filter_variables
      savedFilterValues
    })


    variables_for_filter <- shiny::reactive({input$select_filter_variables})

    res_filter <- datamods::filter_data_server(
      id = "filtering",
      data = uploaded_data_renamed,
      vars = variables_for_filter,
      defaults = defaults,
      drop_ids = FALSE,
      widget_num = "slider",
      widget_char = "picker",
      label_na = "NA",
      value_na = TRUE
    )

    filtered_data_reactive <- reactiveValues(val = NULL)

    shiny::observeEvent(res_filter$filtered(), {
      if (!is.null(res_filter$filtered())) {
        if (nrow(res_filter$filtered()) != 0) {
          filtered_data_reactive$val <- res_filter$filtered()
        }
      }
    })

    shiny::observeEvent(filtered_data_reactive$val,{
      shinyWidgets::updateProgressBar(
        session = session,
        id = "pbar",
        value = length(unique(filtered_data_reactive$val$megaplots_selected_subjectid)),
        total = length(unique(uploaded_data_renamed()$megaplots_selected_subjectid))
      )
    })

    list(
      filtered_data = shiny::reactive({ filtered_data_reactive$val })
    )
  })
}
