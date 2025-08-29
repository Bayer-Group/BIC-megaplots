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

    #This function must be called in order for all other shinyjs functions to work
    shinyjs::useShinyjs(),
    #Add own JavaScript functions that can be called from R
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
      type = 'text/css',
      ".btn-outline-default,
      .btn-default:not(.btn-primary,.btn-secondary,.btn-info,.btn-success,.btn-danger,.btn-warning,.btn-light,.btn-dark,.btn-link,[class*='btn-outline-']) {
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
    #creates an reactive variable "input$dimension" with screen height as value
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
      #create color theme for user interface
      theme = bslib::bs_theme(
        primary = "#0091DF",                     #primary color used for inputs
        bg = "#404A4E",                          #background-color
        fg = "white",                            #font-color
        heading_font = "Agency FB",              #font
        base_font = "Agency FB",
        font_scale = 1.4,
        "input-border-color" = "#d2d2d2"
      ),
      bg = "#0091DF",                            #Top navbar background-color
      inverse = FALSE,                           #FALSE for a light text color at navbar
      # Sidebar
      # Use accordion_panels from bslib
      sidebar = bslib::sidebar(

        title =
        div(img(src = "www/megaplot_hexsticker.png", height = "175px")),
        bslib::accordion_panel(
          "Sorting/Grouping",
          icon = bsicons::bs_icon("sort-down"),
          shinyWidgets::pickerInput(
            inputId = 'select_sorting',
            label = "Sorting variable",
            choices = c("subjectid","start_time","end_time"),
            selected = NULL,
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              #`style` = 'background: btn-primary',
              `header` = 'Select item'
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
        # bslib::accordion_panel(
        #   "Sequencing",
        #   icon = bsicons::bs_icon("sort-up-alt"),
        #   "TBD"
        # ),
        bslib::accordion_panel(
          "Plot appearance",
          icon = bsicons::bs_icon("border-width"),
          shiny::sliderInput(
            inputId = "line_width",
            label = "Line width (Mega plot)",
            min = 1,
            max = 5,
            value = 3,
            step = 0.5
          ),
          shiny::numericInput(
            inputId = "event_summary_cutoff",
            label = "Display hover for counts greater than or equal to:",
            value = 1,
            min = 1,
            max = NA,
            step = 1
          )#,
          # shiny::radioButtons(
          #   inputId="font_size",
          #   label="Text Size:",
          #   selected = "standard",
          #   choiceNames = c("Small", "Standard", "Large"),
          #   choiceValues = c("small", "standard", "large")
          # )
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
          shiny::column(4,
            shiny::wellPanel(
              id = "selected_events_panel",
              style = "overflow-y:scroll;max-height: 10000px;",
              shinyTree::shinyTree(
                "tree",
                checkbox = TRUE,
                search = TRUE,
                themeIcons = FALSE
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
            shiny::wellPanel(
              id = "colour_picker_panel",
              colourpicker::colourInput(
                inputId = "picked_colour",
                label = "Click colored event container and use this Picker to update any color",
                value = "white"
              ),
              shiny::checkboxInput(
                inputId = "jitter_events",
                label = "Jitter events for event group",
                value = TRUE
              )
            )
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
               ),
               shinyWidgets::pickerInput(
                 inputId = 'select_strata_var',
                 label = "Select stratification variable(s)",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
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
    favicon(ext='png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Megaplots"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
