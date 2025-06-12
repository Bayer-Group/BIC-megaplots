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
      shinydashboard::dashboardHeader(title = img(src = 'www/megaplot_logo_white.png', width = '200px')),
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
            shiny::sliderInput(
              inputId = "random",
              label = paste("Number of displayed subjects"),
              min = 1,
              max = 999,
              value = 1
            ),
            shiny::conditionalPanel(condition = "output.check_subset == false",
              shiny::radioButtons(
                inputId = 'selection_button',
                label = 'Subset selection type',
                choices = c('deterministic', 'random'),
                selected = 'deterministic'
              ),
              tabsetPanel(
                id = "Change_input_for_deterministic_or_random",
                type = "hidden",
                tabPanel(
                  "deterministic",
                  shiny::numericInput(
                    inputId = "startsubj",
                    label = "Selection starts in row ... of dataset",
                    min = 1,
                    max = 1,
                    value = 1,
                    step = 1
                  )
                ),
                tabPanel(
                  "random",
                  shiny::numericInput(
                    inputId = "seedset",
                    label = "Select a seed for the random subject selection",
                    min = 1,
                    max = 10000,
                    value = 2006,
                    step = 1
                  )
                )
              ),
              shinyWidgets::pickerInput(
                inputId = "specific_ids",
                label = "Select specific subjects which should be displayed",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `selected-text-format` = 'count > 0',
                  `count-selected-text` = '{0} selected (of {1})',
                  `live-search` = TRUE,
                  `header` = 'Select multiple items',
                  `max-options` = 150,
                  `max-options-text` = "No more!",
                  `none-selected-text` = 'All dropped!'
                )
              ),
              shinyWidgets::actionBttn(
                inputId = 'subset.button',
                label = "Update subject selection!",
                style = "gradient",
                # color = "primary",
                size = 'xs',
                no_outline = FALSE,
                icon = icon("refresh")
              )
            ),
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
            shiny::sliderInput(
              inputId = "range",
              label = "Zoom",
              min = 0,
              max = 100,
              value = c(0, 100),
              step = 1
            ),
            # shinyWidgets::pickerInput(
            #   inputId = 'select.device',
            #   label = "Select device format",
            #   width = 'fit',
            #   choices = c('16:9', '16:10', '21:9', '4:3', '9:21', '10:16', '9:16'),
            #   multiple = FALSE,
            #   selected = '16:9',
            #   options = list(
            #     `actions-box` = FALSE,
            #     `style` = 'background: btn-primary',
            #     `none-selected-text` = 'Please select'
            #   ),
            #   choicesOpt = list(`icon` = rep('glyphicon-picture', 7))
            # ),
            # HTML(
            #  "<p style='color: grey'> Note: To maximize your browser window:</p>",
            #   "<p style='color: grey'> Open application in browser,</p>",
            #   "<p style='color: grey'> press F11 (Full screen)</p>"
            # ),

            shiny::sliderInput(
              inputId = "height_slider",
              label = "Change height ratio",
              min = 0.1,
              max = 2,
              value = 1
            ),
            # shinyWidgets::pickerInput(
            #   inputId = 'select.col',
            #   label = "Select color theme",
            #   width = 'fit',
            #   choices = list('grey (app version)', 'white (print version)'),
            #   multiple = FALSE,
            #   selected = 'grey (app version)',
            #   choicesOpt = list(`icon` = rep('glyphicon-blackboard', 2))
            # ),
            shiny::sliderInput(
              inputId = "thick",
              label = "Thickness of subject lines",
              min = 0.05,
              max = 0.4,
              value = 0.1,
              step = 0.05
            ),
            shiny::checkboxInput(
              inputId = 'inc.ev.subj',
              label = 'Color subject line by first selected event (for \'continuous\' event)',
              value = FALSE
            ),
            shiny::checkboxInput(
              inputId = "lines_instead_symbols",
              label = "Use lines for all events",
              value = FALSE
            ),
            shiny::conditionalPanel(condition = "input.lines_instead_symbols == true",
              shiny::radioButtons(
                inputId = "lines_options",
                label = "Select line(s) appearance",
                choices = c("Adjacent", "Overlaying")
              ),
                HTML(
                 "<p> Note: Some events might not be visible</p>"
                ),
                HTML(
                 "<p> for option 'Overlaying'!</p>"
                )
            ),
            shiny::checkboxInput(
              inputId = 'det.xaxt',
              label = 'Detailed tickmarks on the x-axis',
              value = TRUE
            ),
            shiny::checkboxInput(
              inputId = 'incr.font',
              label = 'Increase font size',
              value = FALSE
            ),
            shiny::checkboxInput(
              inputId = 'background_stripes',
              label = 'Add background stripes',
              value = FALSE
            ),
            shiny::conditionalPanel(
              condition = "input.background_stripes == true",
              shiny::numericInput(
                inputId = "background.stripes.length",
                label = "Length of background stripes",
                value = 7,
                min = 1
              )
            ),
            shiny::checkboxInput(
              inputId = 'reference_line_1',
              label = 'Add reference line',
              value = TRUE
            ),
            shiny::conditionalPanel(
              condition = "input.reference_line_1 == true",
              shiny::numericInput(
                inputId = "reference_line_1_value",
                label = "Reference line (1)",
                value = 0
              ),
              shiny::checkboxInput(
                inputId = 'reference_line_2',
                label = 'Add second reference line',
                value = FALSE
              )
            ),
            shiny::conditionalPanel(condition = "input.reference_line_2 == true",
              shiny::numericInput(
                 inputId = "reference_line_2_value",
                label = "Reference line (2)",
                value = 0
              ),
              shiny::checkboxInput(
                inputId = 'reference_line_3',
                label = 'Add third reference line',
                value = FALSE
              )
            ),
            shiny::conditionalPanel(condition = "input.reference_line_3 == true",
              shiny::numericInput(
                 inputId = "reference_line_3_value",
                label = "Reference line (3)",
                value = 0
              )
            ),
            shiny::textInput(
              inputId = "y_axis_label",
              label = "y axis label",
              value = "Subject identifier",
              placeholder = "Subject identifier"
            ),
            shiny::textInput(
              inputId = "x_axis_label",
              label = "x axis label",
              value = "Time",
              placeholder = "Time"
            ),
            shiny::tags$br(),
            shinyWidgets::actionBttn(
                inputId = "reset_draggable_panel_positions",
                label = "Reset panel positions",
                 style = "gradient",
                # color = "primary",
                size = 'xs',
                no_outline = FALSE,
                icon = icon("refresh")
              ),
            shiny::tags$br()
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
                                    shiny::tags$hr()),
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
            shinydashboard::box(
              width = NULL,
              title = HTML('<p style ="color:white;"> Main options </p>'),
              solidHeader = TRUE,
              collapsible = TRUE,
              shiny::column(
                3,
                shiny::selectizeInput(
                  inputId = 'select.events',
                  label = HTML('<p style ="color:white;"> Select events</p>'),
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list('plugins' = list('remove_button', 'drag_drop'))
                ),
                shinyWidgets::pickerInput(
                  inputId = 'event.levels',
                  label = HTML('<p style ="color:white;"> Select event levels</p>'),
                  choices = NULL,
                  multiple = TRUE,
                  selected = NULL,
                  options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = 'count > 0',
                    `count-selected-text` = '{0} selected (of {1})',
                    `live-search` = TRUE,
                    `style` = 'background: btn-primary',
                    `header` = 'Select multiple items',
                    `none-selected-text` = 'Please select'
                  )
                )
              ),
              shiny::column(3,
                shiny::selectizeInput(
                  inputId = 'select.grouping',
                  label = HTML('<p style ="color:white;"> Select grouping </p>'),
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list('plugins' = list('remove_button', 'drag_drop'))
                ),
                shinyWidgets::pickerInput(
                  inputId = 'select.subsetting',
                  label = HTML('<p style ="color:white;"> Select subsets</p>'),
                  choices = NULL,
                  multiple = TRUE,
                  selected = NULL,
                  options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = 'count > 0',
                    `count-selected-text` = '{0} selected (of {1})',
                    `live-search` = TRUE,
                    `style` = 'background: btn-primary',
                    `header` = '(For every factor at least one value must be selected)',
                    `none-selected-text` = 'All dropped!'
                  )
                )
              ),
              shiny::column(
                3,
                shinyWidgets::pickerInput(
                  inputId = 'select.sorting',
                  label = HTML('<p style ="color:white;"> Sort (descending) </p>'),
                  choices = NULL,
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    `live-search` = TRUE,
                    `style` = 'background: btn-primary',
                    `header` = 'Select item'
                  )
                ),
                "Save settings as .rds file:",
                shinyWidgets::downloadBttn(
                  outputId = "save_setting2",
                  label = "Save Session Settings",
                  style = 'gradient',
                  # color = 'primary',
                  size = 'sm',
                  no_outline = FALSE,
                  block = FALSE
                )
              ),
              # shiny::column(3,
                # shiny::sliderInput(
                #   inputId = "refdate",
                #   label = "Reference Line",
                #   min = 0,
                #   max = 100,
                #   value = c(0, 0),
                #   step = 1
                # ),
              # )
            ),
            #shiny::uiOutput('container_tag'),
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
          ),

          # data upload tabItem
          data_upload_ui("data_upload"),
          # shinydashboard::tabItem(
          #   'datimport',
          #
          #   shinydashboard::box(
          #     width = NULL,
          #     solidHeader = TRUE,
          #     collapsible = FALSE,
          #     background = 'black',
          #     shiny::fixedRow(
          #
          #     shiny::column(8,
          #       img(src = 'www/megaplot-logo.png', width = '500px'),
          #       HTML("<h5 style = 'color: white;'> depict individual patient journey per day. Pre-specified events on certain days, for example
          #            drug intake, efficacy, or safety events, are indicated by different symbols. The detection of
          #            patterns in the data or the temporal connection of study procedures and outcome events is
          #            supported by artificial intelligence-based functions, such as sorting, grouping, as well as
          #            graphical features like changing the complexity of the display. You can either use demo data or upload your own data to generate a
          #            Megaplot. Please take a look at the Package Manual for more information. </h5>"),
          #       shiny::br(),
          #       shiny::br(),
          #       shinyjs::useShinyjs(),
          #       shiny::fluidRow(
          #         shiny::column(4,
          #           shinyWidgets::prettyRadioButtons(
          #             inputId = 'selectdata',
          #             label = HTML('<p style ="color:white;"> Select data</p>'),
          #             shape = 'round',
          #             animation = 'smooth',
          #             choices =c("Upload data", "Use demo data", "Upload saved data")
          #           )
          #         ),
          #         shiny::conditionalPanel(condition = "input.selectdata == 'Upload data'",
          #           shiny::column(4,
          #             shinyWidgets::prettyRadioButtons(
          #               inputId = 'impswitch',
          #               label = HTML('<p style ="color:white;"> Select file format</p>'),
          #               shape = 'square',
          #               animation = 'smooth',
          #               choices = c(
          #                 '*.RData files (two files)',
          #                 '*.RData file',
          #                 '*.CSV files')
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::uiOutput('impdata')
          #           )
          #         )
          #       ),
          #       shiny::conditionalPanel(condition = "output.fileUploaded_rdata & input.selectdata == 'Upload data'
          #                               & input.impswitch == '*.RData file'",
          #         shiny::fluidRow(
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "A_subjectid_rdata",
          #               label = HTML('<p style ="color:white;"> A: subjectid</p>'),
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "A_start_time_rdata",
          #               label = HTML('<p style ="color:white;"> A: start time</p>'),
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #              shiny::selectInput(
          #               inputId = "A_end_time_rdata",
          #               label = HTML('<p style ="color:white;"> A: end time</p>'),
          #               choices = "",
          #               selected = NULL
          #             )
          #           )
          #         ),
          #         shiny::fluidRow(
          #           shiny::column(4,
          #              shiny::selectInput(
          #               inputId = "B_subjectid_rdata",
          #               label = HTML('<p style ="color:white;"> B: subjectid</p>'),
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "B_event_time_rdata",
          #               label = HTML('<p style ="color:white;"> B: event_time</p>'),
          #               choices = "",
          #               selected = NULL
          #             )
          #           )
          #         )
          #       ),
          #       shiny::conditionalPanel(condition = "output.fileUploaded_csv_A & input.selectdata == 'Upload data'
          #                               & input.impswitch == '*.CSV files'",
          #         shiny::fluidRow(
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "A_subjectid_csv",
          #               label = "A: subjectid",
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "A_start_time_csv",
          #               label = "A: start time",
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #              shiny::selectInput(
          #               inputId = "A_end_time_csv",
          #               label = "A: end time",
          #               choices = "",
          #               selected = NULL
          #             )
          #           )
          #         )
          #       ),
          #       shiny::conditionalPanel(condition = "output.fileUploaded_csv_B & input.selectdata == 'Upload data'
          #                               & input.impswitch == '*.CSV files'",
          #         shiny::fluidRow(
          #           shiny::column(4,
          #              shiny::selectInput(
          #               inputId = "B_subjectid_csv",
          #               label = "B: subjectid",
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "B_event_time_csv",
          #               label = "B: event_time",
          #               choices = "",
          #               selected = NULL
          #             )
          #           )
          #         )
          #       ),
          #       shiny::conditionalPanel(condition = "output.fileUploaded_rdata_A & input.selectdata == 'Upload data'
          #                               & input.impswitch == '*.RData files (two files)'",
          #         shiny::fluidRow(
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "A_subjectid_rdata_files",
          #               label = "A: subjectid",
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "A_start_time_rdata_files",
          #               label = "A: start time",
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #              shiny::selectInput(
          #               inputId = "A_end_time_rdata_files",
          #               label = "A: end time",
          #               choices = "",
          #               selected = NULL
          #             )
          #           )
          #         )
          #       ),
          #       shiny::conditionalPanel(condition = "output.fileUploaded_rdata_B & input.selectdata == 'Upload data'
          #                               & input.impswitch == '*.RData files (two files)'",
          #         shiny::fluidRow(
          #           shiny::column(4,
          #              shiny::selectInput(
          #               inputId = "B_subjectid_rdata_files",
          #               label = "B: subjectid",
          #               choices = "",
          #               selected = NULL
          #             )
          #           ),
          #           shiny::column(4,
          #             shiny::selectInput(
          #               inputId = "B_event_time_rdata_files",
          #               label = "B: event_time",
          #               choices = "",
          #               selected = NULL
          #             )
          #           )
          #         )
          #       ),
          #       shiny::column(4,
          #         shiny::conditionalPanel(condition = "input.selectdata == 'Upload saved data'",
          #           shiny::fileInput(
          #             inputId = 'setting_file',
          #             label = HTML(
          #               '<p> Upload previous Session Settings (.rds) </p>'
          #             ),
          #             multiple = FALSE,
          #             accept = '.rds'
          #           ),
          #           shiny::helpText(
          #             'Please upload the data set of the last session which
          #             was saved via the "Save Session Settings"-button
          #             in the MegaPlot-tab.'
          #           )
          #         )
          #       )
          #     )
          #   ),
          #   shiny::br(),
          #   shiny::fluidRow(
          #     shiny::column(3,
          #       shiny::uiOutput('select.ev1'),
          #       shiny::uiOutput('select_ev_lev1_button'),
          #       shiny::conditionalPanel(condition = "input.select_ev_lev1_button == true",
          #         shiny::uiOutput('select.ev.lev1')
          #       )
          #     ),
          #     shiny::column(3,
          #       shiny::uiOutput('select.ev2'),
          #       shiny::uiOutput('select_ev_lev2_button'),
          #       shiny::conditionalPanel(condition = "input.select_ev_lev2_button == true",
          #         shiny::uiOutput('select.ev.lev2')
          #       )
          #     ),
          #     shiny::column(3,
          #       shiny::uiOutput('select.ev3'),
          #       shiny::uiOutput('select_ev_lev3_button'),
          #       shiny::conditionalPanel(condition = "input.select_ev_lev3_button == true",
          #         shiny::uiOutput('select.ev.lev3')
          #       )
          #     ),
          #     shiny::column(3,
          #       shiny::uiOutput('select.ev4'),
          #       shiny::uiOutput('select_ev_lev4_button'),
          #       shiny::conditionalPanel(condition = "input.select_ev_lev4_button == true",
          #         shiny::uiOutput('select.ev.lev4')
          #       )
          #     )
          #   ),
          #   shiny::fluidRow(
          #     shiny::htmlOutput("err_message"),
          #     tags$head(
          #       tags$style(
          #        "#err_message{color: red;
          #        font-size: 12px;
          #        margin-size: 20px;
          #        }"
          #       )
          #     )
          #   ),
          #   shiny::br(),
          #   shiny::br(),
          #   shiny::fluidRow(
          #     shiny::column(3,
          #       shinyWidgets::actionBttn(
          #         inputId = 'import.button',
          #         label = 'Submit...',
          #         style = 'gradient',
          #         # color = 'primary',
          #         size = 'sm',
          #         no_outline = FALSE,
          #         block = TRUE
          #       ),
          #       shiny::helpText('Please upload a data set first or use the demo data set to submit.')
          #     )
          #   )
          #   )
          # ),
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
        shiny::tags$script(
          HTML(
            "$('body').addClass('sidebar-mini');"
          )
        ),
        # shiny::tags$head(
        #   shiny::tags$style(
        #     HTML(
        #       '.myClass {
        #       font-size: 25px;
        #       line-height: 42px;
        #       text-align: left;
        #       font-family: "Arial Black", Gadget, sans-serif;
        #       padding: 0 5px;
        #       overflow: hidden;
        #       color: white;
        #       }
        #       .skin-blue .main-header .navbar .sidebar-toggle {color:#222d32;}
        #       .skin-blue .main-header .navbar .sidebar-toggle:hover {color:#222d32;}
        #       '
        #     )
        #   )
        # ),
        shiny::tags$script(
          HTML(
            paste0(
              '
              $(document).ready(function() {
              $("header").find("nav").append(\'<span, class="myClass"> <img src="www/megaplot-logo.png" height=30></span>',
              ' \');
              })
              '
            )
          )
        ),
        shiny::tags$style(
          type = 'text/css',
                          ".selectize-dropdown-content {max-height: 150px;}"
        ),
        # shiny::tags$style(type = 'text/css',
        #                   ".selectize-input { background-color: #3c8dbc;}")
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
