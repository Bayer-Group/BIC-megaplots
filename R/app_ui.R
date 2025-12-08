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
      .btn-link,[class*='btn-outline-']
      ) {
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
        font_scale = 1.6,                        #font size
        "input-border-color" = "#d2d2d2"
      ),

      #### Sidebar ####
      # Use accordion_panels from bslib
      sidebar = bslib::sidebar(
        width = 300,
        title = div(img(src = "www/megaplot_hexsticker.png", height = "175px")),
        bslib::accordion(open = FALSE,
          bslib::accordion_panel(
            "Sorting/Grouping",
            icon = bsicons::bs_icon("sort-down"),
            shinyWidgets::pickerInput(
              inputId = 'select_sorting',
              label = "Sorting variable",
              choices = c("megaplots_selected_subjectid","megaplots_selected_start_time","megaplots_selected_end_time"),
              selected = NULL,
              multiple = FALSE,
              options = list(
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
            shiny::radioButtons(
              inputId = "event_summary_hovermode",
              label = "Hover mode (Event Summary)",
              choices = c("One label for each event" = "x", "One label for all events" = "x unified"),
              inline = TRUE,
              selected = "x"
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
          ),
          bslib::accordion_panel(
            "Download",
            icon = bsicons::bs_icon("download"),
            shiny::downloadButton("download_plotly_widget", "Download Mega plot as HTML")
          )
        )
      ),

      #### Main area ####
      bslib::nav_panel(
        id = "Data Upload",
        title = "Data Upload",
        #initialize waiter functions
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
          id = "Upload",
          bslib::nav_panel("File & variable selection", id = "File & variable selection",
           shiny::fluidRow(
             shiny::fileInput(
               inputId = 'file',
               label = "Choose RData file",
               multiple = FALSE,
               accept = '.RData'
             ),
             span(textOutput("file_upload_message"),style = "color:#cc0a21;")
            ),


            shiny::fluidRow(
              shiny::column(2,
                shiny::selectizeInput(
                  inputId = "select_subjectid",
                  label = "subjectid",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
                )
              ),
              # shiny::column(1,
              #   shinyWidgets::actionBttn(
              #     inputId = "delete_select_subjectid",
              #     label = NULL,
              #     icon = icon("trash"),
              #     size = "s",
              #     style = "material-flat",
              #     color = "primary",
              #   )
              # ),
              shiny::column(2,
                  shiny::selectizeInput(
                  inputId = "select_start_time",
                  label = "start time",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
                )
              ),
              shiny::column(2,
                shiny::selectizeInput(
                  inputId = "select_end_time",
                  label = "end time",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
                )
              ),
              shiny::column(2,
                shiny::selectizeInput(
                  inputId = "select_event_time",
                  label = "event start",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
                )
              ),
              shiny::column(2,
                shiny::selectizeInput(
                  inputId = "select_event_time_end",
                  label = "event end",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
                )
              ),
              shiny::column(2,
                shiny::selectizeInput(
                  inputId = "select_event",
                  label = "event",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
                )
              ),
              shiny::column(2,
                shiny::selectizeInput(
                  inputId = "select_event_group",
                  label = "event group",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1
                  )
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
                      shiny::selectInput(
                        inputId = "select_color_palette",
                        label = "Select color palette",
                        choices = c("Set1","Set2","Set3","Pastel1","Pastel2","Paired","Dark2","Accent","Spectral","Rainbow")
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
                        value = "white", allowTransparent = TRUE
                      )
                    ),
                    shiny::column(4,
                      colourpicker::colourInput(
                        inputId = "colour_picker_panel_2",
                        label = "Color 2",
                        value = "white", allowTransparent = TRUE
                      )
                    ),
                    shiny::column(4,
                      colourpicker::colourInput(
                        inputId = "colour_picker_panel_3",
                        label = "Color 3",
                        value = "blue", allowTransparent = TRUE
                      )
                    ),
                    shiny::column(12,
                      shiny::fluidRow(
                        shiny::column(6,
                          colourpicker::colourInput(
                            inputId = "colour_picker_panel_event",
                            label = NULL,
                            value = "white", allowTransparent = TRUE
                          )
                        ),
                        shiny::column(6,
                          shinyWidgets::actionBttn(
                            inputId = "update_color_palette_2",
                            label = "Update color",
                            color = "success",
                            style = "simple",
                            icon = icon("refresh")
                          )
                        )
                      )
                    ),
                    shiny::column(12,
                      shiny::column(4,
                        colourpicker::colourInput(
                          inputId = "colour_picker_panel_unique",
                          label = "",
                          value = "white", allowTransparent = TRUE
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
          full_screen = TRUE,
          bslib::nav_panel("Megaplots",
            id = "Megaplots",
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
            tags$head(
              tags$style(
                type = "text/css",
                ".inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                 .inline .form-group { display: table-row;}"
              )
            ),
            tags$div(class = "inline",
              shinyWidgets::pickerInput(
                inputId = "event_summary_selection",
                label = "Select summary display :  ",
                choices = c(list("Number of events per day" = "event_per_day"), list("Number of events per day (cumulative total)" ="cumulative_event"), list("Number of first events per day and subject (cumulative total)" = "event_by_subject_cumulative")),
                selected = "event_per_day"
              )
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
          )
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
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Megaplots"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
