#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @importFrom DT renderDT datatable renderDataTable formatRound DTOutput
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs
#' @import shinyWidgets
#' @importFrom stringr str_wrap
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @noRd
#' @keywords internal

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = img(src = 'www/megaplot_logo_white.png', width = '250px')),
      shinydashboard::dashboardSidebar(
        collapsed = FALSE,
        disable = FALSE,
        width = '260px',
        shinydashboard::sidebarMenu(
          id = 'sidebarmenu',
          shinyjs::useShinyjs(),

          shinydashboard::menuItem(
            text = 'Data import',
            tabName = 'datimport',
            icon = icon('upload'),
            selected = TRUE
          ),
          shinydashboard::menuItem(
            text = 'MegaPlot',
            icon = icon('align-left'),
            tabName = 'dashboard'
          ),
          shinydashboard::menuItem(
            text = 'Displayed subjects...',
            tabName = 'popopt',
            icon = icon('users'),
            startExpanded = FALSE,
            ##### Module call: displayed subjects ####
            displayed_subjects_ui("displayed_subjects"),
            shiny::tags$br()
          ),
          shinydashboard::menuItem(
            text = 'Artificial intelligence...',
            icon = icon('rocket'),
            tabName = 'artint',
            startExpanded = FALSE,
            shinydashboard::menuItem(
              text = 'Sequencing...',
              icon = icon('random', lib = 'font-awesome'),
              tabName = 'sequencing',

              #### Module call: artifical intelligence
              artificial_intelligence_ui("ai")
            )
          ),
          shinydashboard::menuItem(
            text = 'Settings...',
            icon = icon('cog'),
            tabName = 'mpopt',
            startExpanded = FALSE,
            #### Module call: settings ####
            settings_ui("settings")
          ),

          shinydashboard::menuItem(
            text = 'Color Options',
            icon = icon('tint'),
            tabName = 'br3',
            startExpanded = FALSE,
            #### Module call: color options ####
            color_options_ui("color_options")
          ),
          shinydashboard::menuItem(
            text = 'Raw data',
            tabName = 'rawdata',
            icon = icon('table')
          ),
          shinydashboard::menuItem(
            text = 'Summary statistics',
            icon = icon('bar-chart'),
            tabName = 'sumstab'
          ),
          shinydashboard::menuItem(
            text = 'Package Manual',
            icon = icon('info'),
            tabName = 'data_specification'
          )
        )
      ),
      shinydashboard::dashboardBody(
        # individual color_theme defined in file global.R
        fresh::use_theme(dark_theme),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            'dashboard',
            tags$head(tags$style(".sidebar-menu li { margin-bottom: 15px; }")),
            #### Module call: main options ####
            main_option_ui("megaplots"),

            #### Module call: megaplot ####
            mega_plot_ui("mega_plot")
            # shiny::uiOutput('hover_legend'),
            # shiny::uiOutput('megaplot'),
            # fixedPanel(
            #   shiny::uiOutput('axisbox'),
            #   bottom = 1,
            #   width = "100%",
            #   height = 42
            #
            # ),
            # shiny::uiOutput('hoverpanel'),
            # shiny::uiOutput('summarypanel'),
            # shiny::conditionalPanel(condition = "output.check_slider_used == true",
            #   shiny::uiOutput('next_buttons')
            # )
          ),
          shinydashboard::tabItem('datimport',
            #### Module call: data upload ####
            data_upload_ui("data_upload")
          ),
          shinydashboard::tabItem(
            'data_specification',
            #### Module call: data specificiation
            mod_data_specification_ui("data_spec")
          ),
          shinydashboard::tabItem(
            'sumstab',

            #### Module call: summary statistics####

            summary_statistics_ui("summary_statistics")
          ),
          shinydashboard::tabItem(
            'rawdata',
            #### Module call: raw data ####
            raw_data_ui("raw_data")
          )
        ),
        shiny::tags$script(HTML( "$('body').addClass('sidebar-mini');")),
        shiny::tags$style( type = 'text/css', ".selectize-dropdown-content {max-height: 150px;}"),
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom golem add_resource_path bundle_resources
#' @noRd
#' @keywords internal
golem_add_external_resources <- function() {
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  tags$head(
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'megaplot'
    )
  )
}
