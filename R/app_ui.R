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
            text = ' ',
            icon = NULL,
            tabName = 'br1'
          ),
          shinydashboard::menuItem(
            text = ' ',
            icon = NULL,
            tabName = 'br2'
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
              shinyWidgets::pickerInput(
                inputId = 'methSer',
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
                inputId = 'varSeq',
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
              conditionalPanel(
                condition = "input.varSeq.length > 1",
                shiny::checkboxInput(
                  inputId = 'multiple_distmeasures',
                  label = 'Use different distance measures for variables',
                  value = FALSE
                )
              ),
              seriation_ui("parametersModule"),
              shinyWidgets::actionBttn(
                inputId = 'seq.button',
                label = 'Apply sequencing',
                style = 'gradient',
                # color = 'primary',
                size = 'xs',
                no_outline = FALSE
              ),
              shiny::br(),
              shiny::actionLink(
                "link_to_pdf_view",
                "Information",
                icon = icon('circle-info', lib = 'font-awesome')
              ),
              shiny::uiOutput("pdfview"),
              shiny::span(
                shiny::HTML(
                  gsub(
                    '\n',
                    '<br/>',
                    stringr::str_wrap(
                      '(will appear in the sorting menue and replace the current sorting selection)',
                      width = 30
                    )
                  )
                )#,
              #style = 'color:#bdc6ce'
              ),
              shiny::br()
            )
          ),
          shinydashboard::menuItem(
            text = 'Settings...',
            icon = icon('cog'),
            tabName = 'mpopt',
            startExpanded = FALSE,

            settings_ui("settings")

            # shiny::sliderInput(
            #   inputId = "range",
            #   label = "Zoom",
            #   min = 0,
            #   max = 100,
            #   value = c(0, 100),
            #   step = 1
            # ),
            #
            # shiny::sliderInput(
            #   inputId = "height_slider",
            #   label = "Change height ratio",
            #   min = 0.1,
            #   max = 2,
            #   value = 1
            # ),
            # shiny::sliderInput(
            #   inputId = "thick",
            #   label = "Thickness of subject lines",
            #   min = 0.05,
            #   max = 0.4,
            #   value = 0.1,
            #   step = 0.05
            # ),
            # shiny::checkboxInput(
            #   inputId = 'inc.ev.subj',
            #   label = 'Color subject line by first selected event (for \'continuous\' event)',
            #   value = FALSE
            # ),
            # shiny::checkboxInput(
            #   inputId = "lines_instead_symbols",
            #   label = "Use lines for all events",
            #   value = FALSE
            # ),
            # shiny::conditionalPanel(condition = "input.lines_instead_symbols == true",
            #   shiny::radioButtons(
            #     inputId = "lines_options",
            #     label = "Select line(s) appearance",
            #     choices = c("Adjacent", "Overlaying")
            #   ),
            #     HTML(
            #      "<p> Note: Some events might not be visible</p>"
            #     ),
            #     HTML(
            #      "<p> for option 'Overlaying'!</p>"
            #     )
            # ),
            # shiny::checkboxInput(
            #   inputId = 'det.xaxt',
            #   label = 'Detailed tickmarks on the x-axis',
            #   value = TRUE
            # ),
            # shiny::checkboxInput(
            #   inputId = 'incr.font',
            #   label = 'Increase font size',
            #   value = FALSE
            # ),
            # shiny::checkboxInput(
            #   inputId = 'background_stripes',
            #   label = 'Add background stripes',
            #   value = FALSE
            # ),
            # shiny::conditionalPanel(
            #   condition = "input.background_stripes == true",
            #   shiny::numericInput(
            #     inputId = "background.stripes.length",
            #     label = "Length of background stripes",
            #     value = 7,
            #     min = 1
            #   )
            # ),
            # shiny::checkboxInput(
            #   inputId = 'reference_line_1',
            #   label = 'Add reference line',
            #   value = TRUE
            # ),
            # shiny::conditionalPanel(
            #   condition = "input.reference_line_1 == true",
            #   shiny::numericInput(
            #     inputId = "reference_line_1_value",
            #     label = "Reference line (1)",
            #     value = 0
            #   ),
            #   shiny::checkboxInput(
            #     inputId = 'reference_line_2',
            #     label = 'Add second reference line',
            #     value = FALSE
            #   )
            # ),
            # shiny::conditionalPanel(condition = "input.reference_line_2 == true",
            #   shiny::numericInput(
            #      inputId = "reference_line_2_value",
            #     label = "Reference line (2)",
            #     value = 0
            #   ),
            #   shiny::checkboxInput(
            #     inputId = 'reference_line_3',
            #     label = 'Add third reference line',
            #     value = FALSE
            #   )
            # ),
            # shiny::conditionalPanel(condition = "input.reference_line_3 == true",
            #   shiny::numericInput(
            #      inputId = "reference_line_3_value",
            #     label = "Reference line (3)",
            #     value = 0
            #   )
            # ),
            # shiny::textInput(
            #   inputId = "y_axis_label",
            #   label = "y axis label",
            #   value = "Subject identifier",
            #   placeholder = "Subject identifier"
            # ),
            # shiny::textInput(
            #   inputId = "x_axis_label",
            #   label = "x axis label",
            #   value = "Time",
            #   placeholder = "Time"
            # ),
            # shiny::tags$br(),
            # shinyWidgets::actionBttn(
            #   inputId = "reset_draggable_panel_positions",
            #   label = "Reset panel positions",
            #   style = "gradient",
            #   color = "primary",
            #   size = 'xs',
            #   no_outline = FALSE,
            #   icon = icon("refresh")
            # ),
            # shiny::tags$br()
          ),
          shinydashboard::menuItem(
            text = 'Color Options',
            icon = icon('tint'),
            tabName = 'br3',
            startExpanded = FALSE,
            shinyWidgets::pickerInput(
              inputId = 'select.col',
              label = "Select color theme",
              width = 'fit',
              choices = list('grey (app version)', 'white (print version)'),
              multiple = FALSE,
              selected = 'grey (app version)',
              choicesOpt = list(`icon` = rep('glyphicon-blackboard', 2))
            ),
            mod_colour_palette_ui("color_palette1"),
            shiny::conditionalPanel(condition = "output.load_sel_pal1 == true",
              shiny::tags$br(),
              shiny::tags$hr()
            ),
            # shinyWidgets::pickerInput(
            #   inputId = "plot_symbol1",
            #   label = "Choose Symbol (1)",
            #   choices = c(0,1,2,3),
            #   choicesOpt = list(
            #     content = sprintf("<i class='fa-solid fa-square'></i>")
            #   )
            # ),
            mod_colour_palette_ui("color_palette2"),
            shiny::conditionalPanel(condition = "output.load_sel_pal2 == true",
                                    shiny::tags$br(),
                                    shiny::tags$hr()),
            mod_colour_palette_ui("color_palette3"),
            shiny::conditionalPanel(condition = "output.load_sel_pal3 == true",
                                    shiny::tags$br(),
                                    shiny::tags$hr()),
            mod_colour_palette_ui("color_palette4"),
            shiny::conditionalPanel(condition = "output.load_sel_pal4 == true",
                                    shiny::tags$br(),
                                    shiny::tags$hr())
          ),
          shinydashboard::menuItem(
            text = ' ',
            icon = NULL,
            tabName = 'br4'
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
            text = ' ',
            icon = NULL,
            tabName = 'br4'
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
            tags$style("{color: white;}"),

            main_option_ui("megaplots"),

            shiny::tags$head(
              tags$style(
                HTML(
                  '.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'
                )
              )
            ),
            shiny::uiOutput('hover_legend'),
            shiny::uiOutput('megaplot'),
            fixedPanel(
              shiny::uiOutput('axisbox'),
              bottom = 1,
              width = "100%",
              height = 42

            ),
            shiny::uiOutput('hoverpanel'),
            shiny::uiOutput('summarypanel'),
            shiny::conditionalPanel(condition = "output.check_slider_used == true",
              shiny::uiOutput('next_buttons')
            )


            # shinydashboard::box(
            #   width = NULL,
            #   title = HTML('<p style ="color:white;"> Main options </p>'),
            #   solidHeader = TRUE,
            #   collapsible = TRUE,
            #   shiny::column(
            #     3,
            #     shiny::selectizeInput(
            #       inputId = 'select.events',
            #       label = HTML('<p style ="color:white;"> Select events</p>'),
            #       choices = NULL,
            #       selected = NULL,
            #       multiple = TRUE,
            #       options = list('plugins' = list('remove_button', 'drag_drop'))
            #     ),
            #     shinyWidgets::pickerInput(
            #       inputId = 'event.levels',
            #       label = HTML('<p style ="color:white;"> Select event levels</p>'),
            #       choices = NULL,
            #       multiple = TRUE,
            #       selected = NULL,
            #       options = list(
            #         `actions-box` = TRUE,
            #         `selected-text-format` = 'count > 0',
            #         `count-selected-text` = '{0} selected (of {1})',
            #         `live-search` = TRUE,
            #         `style` = 'background: btn-primary',
            #         `header` = 'Select multiple items',
            #         `none-selected-text` = 'Please select'
            #       )
            #     )
            #   ),
            #   shiny::column(3,
            #     shiny::selectizeInput(
            #       inputId = 'select.grouping',
            #       label = HTML('<p style ="color:white;"> Select grouping </p>'),
            #       choices = NULL,
            #       selected = NULL,
            #       multiple = TRUE,
            #       options = list('plugins' = list('remove_button', 'drag_drop'))
            #     ),
            #     shinyWidgets::pickerInput(
            #       inputId = 'select.subsetting',
            #       label = HTML('<p style ="color:white;"> Select subsets</p>'),
            #       choices = NULL,
            #       multiple = TRUE,
            #       selected = NULL,
            #       options = list(
            #         `actions-box` = TRUE,
            #         `selected-text-format` = 'count > 0',
            #         `count-selected-text` = '{0} selected (of {1})',
            #         `live-search` = TRUE,
            #         `style` = 'background: btn-primary',
            #         `header` = '(For every factor at least one value must be selected)',
            #         `none-selected-text` = 'All dropped!'
            #       )
            #     )
            #   ),
            #   shiny::column(
            #     3,
            #     shinyWidgets::pickerInput(
            #       inputId = 'select.sorting',
            #       label = HTML('<p style ="color:white;"> Sort (descending) </p>'),
            #       choices = NULL,
            #       selected = NULL,
            #       multiple = FALSE,
            #       options = list(
            #         `live-search` = TRUE,
            #         `style` = 'background: btn-primary',
            #         `header` = 'Select item'
            #       )
            #     ),
            #     "Save settings as .rds file:",
            #     shinyWidgets::downloadBttn(
            #       outputId = "save_setting2",
            #       label = "Save Session Settings",
            #       style = 'gradient',
            #       # color = 'primary',
            #       size = 'sm',
            #       no_outline = FALSE,
            #       block = FALSE
            #     )
            #   ),
            # ),
            # shiny::tags$head(
            #   tags$style(
            #     HTML(
            #       '.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'
            #     )
            #   )
            # ),
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
          # use data upload module to create tabItem 'datimport'
          shinydashboard::tabItem('datimport',
            data_upload_ui("data_upload")
          ),
          #use data specification module
          shinydashboard::tabItem(
            'data_specification',
            mod_data_specification_ui("data_spec")
          ),
          shinydashboard::tabItem(
            'sumstab',
            shiny::br(),
            DT::DTOutput('sumtable'),
            shiny::br(),
            shiny::br(),
            DT::DTOutput('indivtable')
          ),
          shinydashboard::tabItem(
            'rawdata',
            shiny::br(),
            shiny::uiOutput('select.raw'),
            shiny::br(),
            DT::DTOutput('rawtable')
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
