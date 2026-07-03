#' UI for the sorting and grouping module
#'
#' Renders the sorting variable picker, grouping variable selector, and the
#' drag-and-drop group arrangement input inside a [bslib::accordion_panel()].
#' Intended to be placed inside the sidebar accordion in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::accordion_panel()] containing the sorting and grouping
#'   interface.
#' @keywords internal
mod_sorting_grouping_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::accordion_panel(
    "Sorting/Grouping",
    icon = bsicons::bs_icon("sort-down"),
    shinyWidgets::pickerInput(
      inputId = ns('select_sorting'),
      label = "Sorting variable",
      choices = c(
        "megaplots_selected_subjectid",
        "megaplots_selected_start_time",
        "megaplots_selected_end_time"
      ),
      selected = NULL,
      multiple = FALSE,
      options = list(
        `live-search` = TRUE
      ),
      choicesOpt = list(
        style = rep_len("font-size: 60%; line-height: 1.6;", 3)
      )
    ),
    shiny::selectizeInput(
      inputId = ns("select_grouping"),
      label = "Grouping variable",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list('plugins' = list('remove_button', 'drag_drop'))
    ),
    # orderInput widget from shinyjqui package to drag and drop the
    # order of groups (if applicable)
    div(
      id = ns("arrange_groups_container"),
      #style = "display: none",
      # shinyjqui::orderInput(
      #   inputId = ns("arrange_groups"),
      #   label = "Arrange Groups",
      #   items = NULL,
      #   width = 300
      # )
      shiny::uiOutput(ns("arrange_groups"))
    )
  )
}


#' Server logic for the sorting and grouping module
#'
#' Updates the sorting picker based on available numeric columns, renders
#' the drag-and-drop group arrangement input, and computes group labels
#' when a grouping variable is selected. Returns the current sorting,
#' grouping, and arrangement selections as reactives for consumption by
#' the parent server.
#'
#' @param id Character string. The module namespace identifier.
#' @param uploaded_data_renamed A reactive data frame with standardised
#'   `megaplots_selected_*` column names, as returned by
#'   [mod_data_upload_server()].
#'
#' @return A named list with three reactive elements:
#'   \describe{
#'     \item{`select_sorting`}{A reactive returning the currently selected
#'       sorting variable name.}
#'     \item{`select_grouping`}{A reactive returning the currently selected
#'       grouping variable name(s), or `NULL` if none selected.}
#'     \item{`arrange_groups`}{A reactive returning the current group
#'       arrangement order as a character vector.}
#'   }
#' @keywords internal
mod_sorting_grouping_server <- function(id, uploaded_data_renamed) {
  shiny::moduleServer(id, function(input, output, session) {
    stopifnot(
      "mod_sorting_grouping_server expects `uploaded_data_renamed` to be a reactive." = {
        shiny::is.reactive(uploaded_data_renamed)
      }
    )

    ns <- session$ns

    # output$arrange_groups <- shiny::renderUI({
    #   shinyjqui::orderInput(
    #     inputId = ns("arrange_groups"),
    #     label = "Arrange Groups",
    #     items = NULL,
    #     width = 300
    #   )
    # })

    #### Sorting ####
    shiny::observeEvent(uploaded_data_renamed(), {
      numeric_choices <- names(which(unlist(lapply(
        uploaded_data_renamed() |>
          dplyr::relocate(tidyr::starts_with("megaplots_")),
        is.numeric
      ))))
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select_sorting",
        choices = numeric_choices,
        selected = ifelse(
          "megaplots_selected_end_time" %in% numeric_choices,
          "megaplots_selected_end_time",
          "megaplots_selected_subjectid"
        ),
        choicesOpt = list(
          style = rep_len(
            "font-size: 60%; line-height: 1.6;",
            length(numeric_choices)
          )
        )
      )

      shiny::updateSelectizeInput(
        session,
        inputId = "select_grouping",
        choices = colnames(uploaded_data_renamed())[
          sapply(uploaded_data_renamed(), class) %in% c("factor", "character")
        ],
        selected = NULL
      )
    })

    output$arrange_groups <- shiny::renderUI({
      shinyjqui::orderInput(
        inputId = ns("arrange_groups"),
        label = "Arrange Groups",
        items = NULL,
        width = 300
      )
    })

    shiny::observeEvent(
      c(uploaded_data_renamed(), input$select_grouping),
      {
        if (is.null(input$select_grouping)) {
          # Hide the orderInput and clear its items when no grouping selected
          shinyjs::hideElement(id = ns("arrange_groups_container"))
          # shinyjqui::updateOrderInput(
          #   session,
          #   inputId = "arrange_groups",
          #   items   = NULL
          # )
          output$arrange_groups <- shiny::renderUI({
            shinyjqui::orderInput(
              inputId = ns("arrange_groups"),
              label = "Arrange Groups",
              items = NULL,
              width = 300
            )
          })
          return()
        } else {
          # Build a label for each unique combination of
          # grouping variable values
          grouping_label_data <- uploaded_data_renamed() |>
            dplyr::select(!!!rlang::syms(input$select_grouping)) |>
            dplyr::distinct() |>
            dplyr::mutate(
              text_snippet_1 = paste(input$select_grouping, collapse = " "),
              text_snippet_2 = paste(
                !!!rlang::syms(input$select_grouping),
                sep = ", "
              )
            ) |>
            dplyr::rowwise() |>
            dplyr::mutate(
              text_snippet_total = paste(
                unlist(strsplit(.data$text_snippet_1, " ")),
                gsub(" ", "", unlist(strsplit(.data$text_snippet_2, ", "))),
                sep = ": ",
                collapse = " & "
              )
            )

          shinyjqui::updateOrderInput(
            session,
            inputId = "arrange_groups",
            items = grouping_label_data$text_snippet_total
          )
          shinyjs::showElement(id = ns("arrange_groups_container"))
        }
      }
    )

    #return
    list(
      select_sorting = shiny::reactive({
        input$select_sorting
      }),
      select_grouping = shiny::reactive({
        input$select_grouping
      }),
      arrange_groups = shiny::reactive({
        input$arrange_groups
      })
    )
  })
}
