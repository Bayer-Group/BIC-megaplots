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
    # This function must be called in order for all other shinyjs functions to work
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

    # Add CSS styles (overwrite shinyTree hover/highlight appearance and search highlight color)
    ## shinyTree appearances
    # tags$style(type = 'text/css', ".jstree-default .jstree-hovered {background-color: #1d2224}"),
    # tags$style(type = 'text/css', ".jstree-default .jstree-search { color: yellow;}"),

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
      title = HTML(paste0("<b> MEGAPLOTS </b>")),
      id = "MEGAPLOTS",
      #create  theme for user interface
      theme = bslib::bs_theme(
        version = 5,
        heading_font = "Agency FB",              #font
        base_font = "Agency FB",                 #font
        font_scale = 1.4,                    #font size
        primary = "#007CBF"
      ),
      #### Sidebar ####
      # Use accordion_panels from bslib
      sidebar = bslib::sidebar(
        width = 350,
        title = tagList(
          #depending on theme display hexsticker in theme colors
          shiny::conditionalPanel(condition = "input.theme_toggle == 'dark'",
                                  hexsticker_logo(src = "www/megaplot_hexsticker_n.png")
          ),
          shiny::conditionalPanel(condition = "input.theme_toggle != 'dark'",
                                  hexsticker_logo(src = "www/megaplot_hexsticker_n2.png")
          ),
        HTML("
          <div style='display: flex; text-align: center;'>
              <span style='display: flex; flex-direction: column; font-size: 1.2rem; line-height: 1.5; text-align: center; '>
                <span style = 'text-decoration: underline;'> The Complete Picture </span>
                <span style = 'font-size: 0.9rem;'> Connect events. Reveal patterns. Generate insight. </span>
              </span>

            </div>
         ")),
        bslib::accordion(open = FALSE,
          # Sorting/Grouping (Sidebar)
          mod_sorting_grouping_ui("sorting_grouping"),
          mod_plot_appearance_ui("plot_appearance"),
          mod_reference_lines_ui("reference_lines"),
          mod_filter_ui("filter"),
          bslib::accordion_panel(
            "Download",
            icon = bsicons::bs_icon("download"),
            shiny::downloadButton("download_plotly_widget", "Download Mega plot as HTML")
          ),
          # module call for sequencing
          sequencing_ui("sequencing_module")
        ),
        HTML(paste0("<p style = 'color: #dedede;'> Version: ", utils::packageVersion("Megaplots")))
      ),
      #### Main area ####
      bslib::nav_panel(
        id = "Data Upload",
        title = "Data Upload",
        #initialize waiter functions
        # waiter::useGarcon(),
        waiter::useWaiter(),
        waiter::waiterShowOnLoad(
          tags$img(
            src = "www/megaplot_hexsticker_n.png",
            height = "175px",
            id = "megaplot_hexsticker"
          ),
          color = "#1D1F21"
        ),
        bslib::navset_card_underline(
          id = "Upload",
          mod_data_upload_ui("data_upload"),
          mod_color_selection_ui("color_selection")
        )
      ),
      bslib::nav_panel(
        id = "Megaplots",
        title = "Megaplots",
        mod_megaplot_ui("megaplot")
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
      bslib::input_dark_mode(id = "theme_toggle", mode = "dark")
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
    ),
    tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "www/megaplots.css"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
