#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "MEGAPLOTS Rebuild",
      theme = bslib::bs_theme(
        primary = "#0091DF",
        bg = "#404A4E",
        fg = "white",
        heading_font = "Agency FB",
        base_font = "Agency FB",
        "input-border-color" = "#dce4e8"
      ),
      bg = "#0091DF",
      inverse = FALSE,
      # Sidebar
      sidebar = bslib::sidebar(
        bg = "#424242",
        title = "Settings",
        bslib::accordion_panel(
          "Sorting/Grouping",
          icon = bsicons::bs_icon("sort-down"),

          shinyWidgets::pickerInput(
            inputId = 'select.sorting',
            label = "Sorting variable",
            choices = c("subjectid","start_time","end_time"),
            selected = NULL,
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              `style` = 'background: btn-primary',
              `header` = 'Select item'
            )
          ),
          shiny::selectizeInput(
            inputId ='select.grouping',
            label = "Grouping variable",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list('plugins' = list('remove_button', 'drag_drop'))
          )
        ),
        bslib::accordion_panel(
          "Sequencing",
          icon = bsicons::bs_icon("sort-up-alt"),
          "TBD"
        ),
        bslib::accordion_panel(
          "Plot appearance",
          icon = bsicons::bs_icon("border-width"),
          "TBD"
        )
      ),
      #Main area
      bslib::nav_panel(
        title = "Data Upload",
        shiny::fluidRow(
          shiny::fileInput(
            inputId = 'file',
            label = "Choose RData file",
            multiple = FALSE,
            accept = '.RData'
          )
        ),
        shiny::fluidRow(
          shinyWidgets::pickerInput(
            inputId = 'select.events',
            label = "Select event(s)",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list('actions-box' = TRUE)
          )
        )
      ),
      bslib::nav_panel(
        title = "Megaplots",
        bslib::navset_card_underline(
          title = "MEGAPLOTS",
          bslib::nav_panel("Megaplots", icon =  bsicons::bs_icon("filter-left"), plotly::plotlyOutput("mega_plots")),
          bslib::nav_panel("Event Summary", plotly::plotlyOutput("event_summary")),
          bslib::nav_panel("Kaplan Meier",
             shiny::fluidRow(
               shinyWidgets::pickerInput(
                 inputId = 'select_event_kaplan_meier',
                 label = "Select event(s)",
                 choices = NULL,
                 selected = NULL,
                 multiple = FALSE,
                 options = list('actions-box' = TRUE)
               )
             ),
             plotly::plotlyOutput("kaplan_meier"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Megaplots"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
