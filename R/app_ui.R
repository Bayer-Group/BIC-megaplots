#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # This function must be called in order for all other
    #shinyjs functions to work
    shinyjs::useShinyjs(),
    # Add own JavaScript functions that can be called from R
    # (This function is used to get row number of which
    #event/color container is clicked)
    shinyjs::extendShinyjs(
      text = "shinyjs.init = function() {
        $('#selected-cols-row').on('click', '.col', function(event) {
        var colnum = $(event.target).data('colnum');
        Shiny.onInputChange('jsColNum', [colnum]);
      });

      $('#rclosecolsSection, #allColsSection').on('click', '.rcol', function(event) {
        var col = $(event.target).data('col');
        Shiny.onInputChange('jsCol', [col]);
      });
      };",
      functions = c("init")
    ),
    # Add CSS styles (overwrite color appearance of fileInput button)
    tags$style(
      type = "text/css",
      ".btn-outline-default,
      .btn-default:not(
        .btn-primary,
        .btn-secondary,
        .btn-info,.btn-success,
        .btn-danger,
        .btn-warning,
        .btn-light,
        .btn-dark,
        .btn-link,[class*='btn-outline-']) {
        --bs-btn-color: white;
        --bs-btn-border-color: white;
        --bs-btn-hover-border-color: white;
        --bs-btn-hover-color: white;
      }"
    ),
    # Add CSS styles (overwrite shinyTree hover/highlight appearance and search highlight color)
    ## shinyTree appearances
    tags$style(type = 'text/css', ".jstree-default .jstree-clicked {background-color: #404A4E}"),
    tags$style(type = 'text/css', ".jstree-default .jstree-hovered {background-color: #1d2224}"),
    tags$style(type = 'text/css', ".jstree-default .jstree-search { color: yellow;}"),
    # creates an reactive variable "input$dimension" with screen height as value
    # (used for maximum wellPanel height to maximize size depending on screen size)
    tags$head(
      tags$script('
        var dimension = [0];
        $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
        dimension[0] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });
        '
      )
    ),
    # Use page_navbar from bslib package
    bslib::page_navbar(
      title = "MEGAPLOTS",
      id = "MEGAPLOTS",
      #create color theme for user interface
      theme = bslib::bs_theme(
        primary = "#0091DF",                     #primary color used for inputs
        "navbar-bg" = "#0091DF",                 #navbar background color
        bg = "#404A4E",                          #app background-color
        fg = "white",                            #font-color
        heading_font = "Agency FB",              #font
        base_font = "Agency FB",                 #font
        font_scale = 1.4,                        #font size
        "input-border-color" = "#d2d2d2"
      ),
      # Sidebar
      # Use accordion_panels from bslib
      sidebar = bslib::sidebar(
        width = 300,
        title = div(img(src = "www/megaplot_hexsticker.png", height = "175px")),
        bslib::accordion(open = FALSE,
        bslib::accordion_panel(
          "Sorting/Grouping",
          icon = bsicons::bs_icon("sort-down"),
          # tags$style(".pickerinput-class {font-size: 60%; line-height: 1.6;}"),
          shinyWidgets::pickerInput(
            inputId = 'select_sorting',
            label = "Sorting variable",
            choices = c("subjectid","start_time","end_time"),
            selected = NULL,
            multiple = FALSE,
            options = list(
              # style = "pickerinput-class",
              `live-search` = TRUE
            ),
            choicesOpt = list(style =  rep_len("font-size: 60%; line-height: 1.6;", 3)
            )
          ),
          shiny::selectizeInput(
            inputId ='select_grouping',
            label = "Grouping variable",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list('plugins' = list('remove_button', 'drag_drop'))
          )
        ),
        bslib::accordion_panel(
          "Plot appearance",
          icon = bsicons::bs_icon("border-width"),
          shiny::sliderInput(
            inputId = "line_width_subjects",
            label = "Subject line width",
            min = 1,
            max = 10,
            value = 1,
            step = 0.5
          ),
          shiny::sliderInput(
            inputId = "line_width",
            label = "Event line width",
            min = 1,
            max = 5,
            value = 3,
            step = 0.5
          ),
          shinyWidgets::prettySwitch(
            inputId = "switch_legend_grouping",
            label = "On/Off Legend Grouping",
            value = TRUE,
            status = "primary"
          ),
          shiny::numericInput(
            inputId = "event_summary_cutoff",
            label = "Display hover for counts greater than or equal to:",
            value = 1,
            min = 1,
            max = NA,
            step = 1
          )
        ),
        bslib::accordion_panel(
          "Filter",
          icon = bsicons::bs_icon("filter"),
          # shinyWidgets::actionBttn(
          #   inputId = "save_filter_values",
          #   label = "Save filter values",
          #   style = "bordered",
          #   color = "primary",
          #   icon = shiny::icon("download")
          # ),
          # shinyWidgets::actionBttn(
          #   inputId = "load_filter_values",
          #   label = "Load filter values",
          #   style = "bordered",
          #   color = "primary",
          #   icon = shiny::icon("upload")
          # ),
          shinyWidgets::pickerInput(
            inputId ="select_filter_variables",
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
          shiny::conditionalPanel(condition = "output.filter_enabled == true",
            datamods::filter_data_ui("filtering", max_height = "500px")
          )
          # datamods::filter_data_ui("filtering", max_height = "500px")
        ),
        bslib::accordion_panel(
          "Download",
          icon = bsicons::bs_icon("download"),
          shiny::downloadButton("download_plotly_widget", "Download Mega plot as HTML")
        ),
        bslib::accordion_panel(
          "Sequencing",
          icon = bsicons::bs_icon("plus"),
          artificial_intelligence_ui("ai")
        )
        )
      ),
      #Main area
      bslib::nav_panel(
        id = "Data Upload",
        title = "Data Upload",
        waiter::useGarcon(),
        waiter::useWaiter(),
        waiter::waiterShowOnLoad(
          tags$img(
            src = "www/megaplot_hexsticker.png",
            height = "175px",
            id = "megaplot_hexsticker"
          )
        ),
        bslib::navset_card_underline(
          title = "Upload",
          id = "Upload",
          bslib::nav_panel("File & variable selection", id = "File & variable selection",
           shiny::fluidRow(
             shiny::fileInput(
               inputId = 'file',
               label = "Choose RData file",
               multiple = FALSE,
               accept = '.RData'
             )
            ),
            shiny::fluidRow(
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_subjectid",
                  label = "subjectid",
                  choices = "subjectid",
                  selected = "subjectid"
                )
              ),
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_start_time",
                  label = "start time",
                  choices = "start_time",
                  selected = "start_time"
                )
              ),
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_end_time",
                  label = "end time",
                  choices = "end_time",
                  selected = "end_time"
                )
              ),
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_event_time",
                  label = "event start",
                  choices = "event_time",
                  selected = "event_time"
                )
              ),
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_event_time_end",
                  label = "event end",
                  choices = "event_time_end",
                  selected = "event_time_end"
                )
              ),
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_event",
                  label = "event",
                  choices = "event",
                  selected = "event"
                )
              ),
              shiny::column(2,
                shiny::selectInput(
                  inputId = "select_event_group",
                  label = "event group",
                  choices = "event_group",
                  selected = "event_group"
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(2,
                shinyWidgets::actionBttn(
                  inputId = "upload_1_next_button",
                  label = "Next",
                  style = "material-flat",
                  color = "primary",
                  icon = shiny::icon("angle-right")
                )
              )
            )
          ),
          # filter tab
          # bslib::nav_panel("Filtering", id = "Filtering",
          #     shiny::fluidRow(
          #       shinyWidgets::progressBar(
          #         id = "pbar", value = 100,
          #         total = 100, display_pct = TRUE,
          #         status = "success"
          #       ),
          #       tags$h4("Filter data:"),
          #       shinyWidgets::pickerInput(
          #         inputId ="select_filter_variables",
          #         label = "Select filter variable(s)",
          #         choices = NULL,
          #         selected = NULL,
          #         multiple = TRUE,
          #         options = list(liveSearch = TRUE)
          #       ),
          #       shiny::fluidRow(
          #         shiny::column(1,
          #                       shinyWidgets::actionBttn(
          #                         inputId = "upload_2_back_button",
          #                         label = "Back",
          #                         style = "material-flat",
          #                         color = "primary",
          #                         icon = icon("angle-left")
          #                       )
          #         ),
          #         shiny::column(1,
          #                       shinyWidgets::actionBttn(
          #                         inputId = "upload_2_next_button",
          #                         label = "Next",
          #                         style = "material-flat",
          #                         color = "primary",
          #                         icon = icon("angle-right")
          #                       )
          #         )
          #       )#,
          #       # shiny::conditionalPanel(condition = "output.filter_enabled == true",
          #       #datamods::filter_data_ui("filtering", max_height = "500px")
          #       # )
          #     )
          #
          # ),
          bslib::nav_panel("Event & color selection", id = "Event & color selection",
          shiny::fluidRow(
            shiny::column(4,
              shiny::wellPanel(
                id = "selected_events_panel",
                style = "overflow-y:scroll; max-height: 10000px;", #init high value and then update max height within server.R
                jsTreeR::jstreeOutput(
                  "tree2"
                )
              )
            ),
            shiny::column(4,
              shiny::wellPanel(id = "selected_events_color_container_panel",
                style ="overflow-y:scroll;max-height: 10000px;",
                div(
                  id = "header-section",
                  div(
                    id = "selected-cols-row",
                  uiOutput("selected_events_color_container")
                  )
                )
              )
            ),
            shiny::column(4,
              shiny::wellPanel(id = "colour_picker_panel",
                shiny::fluidRow(
                  shiny::column(12,
                    shiny::radioButtons(
                      inputId = "color_method",
                      label = "Select method to colorize events:",
                      choices = c(
                      "Color gradient (3 colors)" = "gradient",
                      "Unique color for all events within group" = "unique",
                      "Distinct color by selected palette" = "palette"
                      )
                    )
                  ),
                  shiny::column(12,
                    shiny::textOutput(
                      "colorization_selection"
                    )
                  ),
                  shiny::column(4,
                    colourpicker::colourInput(
                      inputId = "colour_picker_panel_1",
                      label = "Click colored event container and use this Picker to update any color",
                      value = "white"
                    )
                  ),
                  shiny::column(4,
                    colourpicker::colourInput(
                      inputId = "colour_picker_panel_2",
                      label = "Color 2",
                      value = "white"
                    )
                  ),
                  shiny::column(4,
                    colourpicker::colourInput(
                      inputId = "colour_picker_panel_3",
                      label = "Color 3",
                      value = "blue"
                    )
                  ),
                  shiny::column(12,
                    shiny::column(4,
                      colourpicker::colourInput(
                        inputId = "colour_picker_panel_event",
                        label = "",
                        value = "white"
                      ),
                      shinyWidgets::actionBttn(
                        inputId = "update_color_palette_2",
                        label = "Update color",
                        color = "success",
                        style = "simple",
                        icon = icon("refresh")
                      ),
                    )
                  ),
                  shiny::column(12,
                    shiny::column(4,
                      colourpicker::colourInput(
                        inputId = "colour_picker_panel_unique",
                        label = "",
                        value = "white"
                      )
                    )
                  ),
                  column(12,
                    shiny::plotOutput("colour_palette", height = "40px")
                  )
                ),
                br(),
                shinyWidgets::actionBttn(
                  inputId = "update_color_palette",
                  label = "Update colors",
                  color = "success",
                  style = "simple",
                  icon = icon("refresh")
                ),
                br(),
                shiny::checkboxInput(
                  inputId = "jitter_events",
                  label = "Jitter events for event group",
                  value = TRUE
                )
              ),
              shiny::conditionalPanel(condition = "output.color_changed == true",
                shinyWidgets::downloadBttn(
                  outputId = "save_colors",
                  label = "Save color file",
                  icon = icon("save"),
                  style = "material-flat",
                  color = "primary"
                )
              ),
              shiny::fileInput(
                inputId = 'upload_saved_color_file',
                label = "Upload saved color file",
                multiple = FALSE,
                accept = '.rds'
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(1,
              shinyWidgets::actionBttn(
                inputId = "upload_3_back_button",
                label = "Back",
                style = "material-flat",
                color = "primary",
                icon = icon("angle-left")
              )
            ),
            shiny::column(1,
              shinyWidgets::actionBttn(
                inputId = "upload_3_next_button",
                label = "Next",
                style = "material-flat",
                color = "primary",
                icon = icon("angle-right")
              )
            )
            )
          )
        )
      ),
      bslib::nav_panel(
        id = "Megaplots",
        title = "Megaplots",
        bslib::navset_card_underline(
          title = "MEGAPLOTS",
          full_screen = TRUE,
          bslib::nav_panel("Megaplots", id = "Megaplots", icon =  bsicons::bs_icon("filter-left"),
           bslib::as_fill_carrier(
             shinycssloaders::withSpinner(
               plotly::plotlyOutput("mega_plots"),
               color = "white",
               image = "www/megaplot_hexsticker.png",
               image.height = "175px",
               caption = "Loading..."
             )
            )
          ),
          bslib::nav_panel("Event Summary",
            # shinyWidgets::prettySwitch(
            #   inputId = "event_summary_switch",
            #   label = "Change view",
            #   value = FALSE,
            #   status = "primary"
            # ),
            shinyWidgets::pickerInput(
              inputId = "event_summary_selection",
              label = "Select summary display",
              choices = c(list("Number of events per day" = "event_per_day"), list("Number of events per day (cumulative total)" ="cumulative_event"), list("Number of first events per day and subject (cumulative total)" = "event_by_subject_cumulative")),
              selected = "event_per_day"
            ),
            bslib::as_fill_carrier(
              shinycssloaders::withSpinner(
                ui_element = plotly::plotlyOutput("event_summary"),
                color = "white",
                image = "www/megaplot_hexsticker.png",
                image.height = "175px",
                caption = "Loading..."
              )
            )
          )#,
          # bslib::nav_panel("Kaplan Meier",
          #    shiny::fluidRow(
          #      shinyWidgets::pickerInput(
          #        inputId = 'select_event_kaplan_meier',
          #        label = "Select event(s)",
          #        choices = NULL,
          #        selected = NULL,
          #        multiple = FALSE,
          #        options = list('actions-box' = TRUE)
          #      )
          #    ),
          #    plotly::plotlyOutput("kaplan_meier")
          # )
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
    favicon(ext='png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Megaplots"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
