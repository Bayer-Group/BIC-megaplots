#' UI for the megaplot display module
#'
#' Renders the main megaplot and event summary panels inside a
#' [bslib::navset_card_underline()]. Intended to be placed inside the
#' `"Megaplots"` [bslib::nav_panel()] in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::navset_card_underline()] containing the megaplot and
#'   event summary panels.
#' @keywords internal
mod_megaplot_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_underline(
    full_screen = TRUE,
    bslib::nav_panel(
      tags$div(
        HTML(paste0("Megaplots","&emsp;","&emsp;"))
      ),
      id = "Megaplots",
      bslib::as_fill_carrier(
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("mega_plots")),
          color = "white",
          image = "www/megaplot_hexsticker_n.png",
          image.height = "175px",
          caption = "Loading..."
        )
      )
    ),
    bslib::nav_panel(
      title = tags$div(HTML(paste0("Event summary","&emsp;","&emsp;"))
      ),
      tags$head(
        tags$style(
          type = "text/css",
          ".inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                 .inline .form-group { display: table-row;}"
        )
      ),
      tags$div(class = "inline",
               shinyWidgets::pickerInput(
                 inputId = ns("event_summary_selection"),
                 label = "Select summary display :  ",
                 choices = c(list("Number of events per day" = "event_per_day"), list("Number of events per day (cumulative total)" ="cumulative_event"), list("Number of first events per day and subject (cumulative total)" = "event_by_subject_cumulative")),
                 selected = "event_per_day"
               )
      ),
      bslib::as_fill_carrier(
        shinycssloaders::withSpinner(
          ui_element = plotly::plotlyOutput(ns("event_summary")),
          color = "white",
          image = "www/megaplot_hexsticker_n.png",
          image.height = "175px",
          caption = "Loading..."
        )
      )
    )
  )
}


#' Server logic for the megaplot display module
#'
#' Renders the main megaplot and event summary plots, manages the HTML
#' download, and returns the current plot object for use by the parent's
#' download handler.
#'
#' @param id Character string. The module namespace identifier.
#' @param megaplot_prepared_data A reactive returning the prepared megaplot
#'   data frame, as produced by `prepare_megaplot_data()`.
#' @param megaplot_filtered_data A reactive returning the filtered megaplot
#'   data frame, as produced by `filter_megaplot_data()`.
#' @param reference_lines A [shiny::reactiveValues()] object containing the
#'   twelve reference rectangle parameters, as returned by
#'   [mod_reference_lines_server()].
#' @param appearance A reactive returning a named list of plot appearance
#'   inputs. Expected names: `line_width`, `line_width_subjects`,
#'   `switch_legend_grouping`, `sort_event_groups`, `select_grouping`,
#'   `event_summary_cutoff`, `event_summary_hovermode`,
#'   `sequencing_object`, `sequencing_switch`.
#' @param theme A reactive string returning the current theme toggle value
#'   (e.g. `"dark"`). Passed from the parent because `input$theme_toggle`
#'   lives outside this module's namespace.
#'
#' @return A named list with one element:
#'   \describe{
#'     \item{`plot_object`}{A reactive returning the current plotly widget,
#'       for use by the sidebar download handler in the parent server.}
#'   }
#' @keywords internal
mod_megaplot_server <- function(id,
                                megaplot_prepared_data,
                                megaplot_filtered_data,
                                reference_lines,
                                select_grouping,
                                appearance,
                                theme) {
  shiny::moduleServer(id, function(input, output, session) {

    stopifnot(
      "mod_megaplot_server expects `megaplot_prepared_data` to be a reactive." = {
        shiny::is.reactive(megaplot_prepared_data)
      },
      "mod_megaplot_server expects `megaplot_filtered_data` to be a reactive." = {
        shiny::is.reactive(megaplot_filtered_data)
      },
      "mod_megaplot_server expects `reference_lines` to be a reactiveValues." = {
        shiny::is.reactivevalues(reference_lines)
      },
      "mod_megaplot_server expects `appearance` to be a reactive." = {
        shiny::is.reactive(appearance)
      },
      "mod_megaplot_server expects `theme` to be a reactive." = {
        shiny::is.reactive(theme)
      }
    )


    session_store <- shiny::reactiveValues(val = NULL)

    #### START MEGA PLOTS PART ####
    output$mega_plots <- plotly::renderPlotly({

      shiny::req(megaplot_prepared_data())

      app <- appearance()

      tmp <- draw_mega_plot(
        megaplot_prepared_data = megaplot_prepared_data(),
        megaplot_filtered_data = megaplot_filtered_data(),
        select_grouping = shiny::isolate(select_grouping()),
        line_width = app$line_width,
        line_width_subjects = app$line_width_subjects,
        switch_legend_grouping = app$switch_legend_grouping,
        sort_event_groups = app$sort_event_groups,
        sequencing_object = shiny::isolate(app$sequencing_object),
        sequencing_switch = app$sequencing_switch,
        reference_line_1 = reference_lines$reference_line_1,
        reference_line_2 = reference_lines$reference_line_2,
        reference_line_3 = reference_lines$reference_line_3,
        reference_line_1_value = reference_lines$reference_line_1_value,
        reference_line_2_value = reference_lines$reference_line_2_value,
        reference_line_3_value = reference_lines$reference_line_3_value,
        reference_line_1_value2 = reference_lines$reference_line_1_value2,
        reference_line_2_value2 = reference_lines$reference_line_2_value2,
        reference_line_3_value2 = reference_lines$reference_line_3_value2,
        reference_line_1_color = reference_lines$reference_line_1_color,
        reference_line_2_color = reference_lines$reference_line_2_color,
        reference_line_3_color = reference_lines$reference_line_3_color,
        theme = theme(),
        line_color_dark = app$line_color_subjects_dark,
        line_color_light = app$line_color_subjects_light,
        circular_vision = app$circular_vision
      )
      session_store$val <- tmp
      tmp
    })


    #### START EVENT SUMMARY PART ####

    output$event_summary <- plotly::renderPlotly({
      shiny::req(megaplot_prepared_data())
      shiny::req(megaplot_filtered_data())

      draw_event_summary(
        megaplot_prepared_data = megaplot_prepared_data(),
        megaplot_filtered_data = megaplot_filtered_data(),
        select_grouping = shiny::isolate(select_grouping()),
        event_summary_cutoff =appearance()$event_summary_cutoff,
        event_summary_selection = input$event_summary_selection,
        switch_legend_grouping = appearance()$switch_legend_grouping,
        hovermode = appearance()$event_summary_hovermode,
        reference_line_1 = reference_lines$reference_line_1,
        reference_line_2 = reference_lines$reference_line_2,
        reference_line_3 = reference_lines$reference_line_3,
        reference_line_1_value = reference_lines$reference_line_1_value,
        reference_line_2_value = reference_lines$reference_line_2_value,
        reference_line_3_value = reference_lines$reference_line_3_value,
        reference_line_1_value2 = reference_lines$reference_line_1_value2,
        reference_line_2_value2 = reference_lines$reference_line_2_value2,
        reference_line_3_value2 = reference_lines$reference_line_3_value2,
        reference_line_1_color = reference_lines$reference_line_1_color,
        reference_line_2_color = reference_lines$reference_line_2_color,
        reference_line_3_color = reference_lines$reference_line_3_color,
        theme = theme()

      )
    })

    list(
      plot_object = shiny::reactive({session_store$val})
    )
  })
}
