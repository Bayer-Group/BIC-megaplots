#' Server logic for the data upload module
#'
#' Handles file upload, smart column-name pre-selection, variable validation,
#' show/hide behaviour for picker inputs, and data renaming. Returns the
#' prepared data reactives for consumption by the parent server.
#'
#' @param id Character string. The module namespace identifier.
#' @param parent_session The parent Shiny session object, used for
#'   `bslib::nav_select()` navigation calls.
#'
#' @return A named list with two reactive elements:
#'   \describe{
#'     \item{`uploaded_data_renamed`}{A reactive data frame with standardised
#'       `megaplots_selected_*` column names.}
#'     \item{`uploaded_data_w_ids`}{A reactive data frame with event and
#'       event-group identifiers added.}
#'   }
#' @keywords internal
mod_data_upload_server <- function(id, parent_session, theme) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #### Data upload & data set preparation ####

    shinyjs::hideElement(id = "select_subjectid")
    shinyjs::hideElement(id = "select_start_time")
    shinyjs::hideElement(id = "select_end_time")
    shinyjs::hideElement(id = "select_event_time")
    shinyjs::hideElement(id = "select_event_time_end")
    shinyjs::hideElement(id = "select_event")
    shinyjs::hideElement(id = "select_event_group")
    shinyjs::hideElement(id = "upload_1_next_button")

    # Initialize reactive value for uploaded data
    uploaded_data <- shiny::reactiveValues(val = NULL)
    file_upload_message <- shiny::reactiveValues(val = NULL)

    output$file_upload_message <- renderText(file_upload_message$val)

    shiny::observeEvent(c(input$file), {
      shiny::req(input$file) #requires input$file

      # load data
      file_extension <- strsplit(input$file$datapath, "/.")[[
        1
      ]][length(strsplit(input$file$datapath, "/.")[[1]])]
      # add function load_fileinput
      # megaplot_data <- load_fileinput(input$file)
      # currently only .RData files are allowed
      if (file_extension %in% c(".RData", ".rdata", ".Rdata")) {
        megaplot_data <- base::get(
          load(
            file = input$file$datapath
          )
        )
        file_upload_message$val <- NULL
      } else {
        megaplot_data <- NULL
        file_upload_message$val <- "Note: Only .RData files are allowed!"
      }
      uploaded_data$val <- megaplot_data #update reactive value 'uploaded_data'
    })

    shiny::observeEvent(uploaded_data$val, {
      if (is.null(uploaded_data$val)) {
        shinyjs::hideElement(id = "select_subjectid")
        shinyjs::hideElement(id = "select_start_time")
        shinyjs::hideElement(id = "select_end_time")
        shinyjs::hideElement(id = "select_event_time")
        shinyjs::hideElement(id = "select_event_time_end")
        shinyjs::hideElement(id = "select_event")
        shinyjs::hideElement(id = "select_event_group")
        shinyjs::hideElement(id = "upload_1_next_button")
      } else {
        shinyjs::showElement(id = "select_subjectid")
        shinyjs::showElement(id = "select_start_time")
        shinyjs::showElement(id = "select_end_time")
        shinyjs::showElement(id = "select_event_time")
        shinyjs::showElement(id = "select_event_time_end")
        shinyjs::showElement(id = "select_event")
        shinyjs::showElement(id = "select_event_group")
        # shinyjs::showElement(id = ns("upload_3_next_button"))
        # shinyjs::showElement(id = ns("upload_3_back_button"))
        # shinyjs::showElement(id = ns("upload_saved_color_file"))

        colnames_uploaded <- colnames(uploaded_data$val)

        if ("subjectid" %in% colnames_uploaded) {
          init_subjectid <- "subjectid"
        } else if (
          any(startsWith(colnames_uploaded, "subj")) |
            any(startsWith(colnames_uploaded, "SUBJ"))
        ) {
          init_subjectid <- colnames_uploaded[
            startsWith(colnames_uploaded, "subj") |
              startsWith(colnames_uploaded, "SUBJ")
          ][1]
        } else {
          init_subjectid <- NULL
        }
        shinyWidgets::updatePickerInput(
          session,
          "select_subjectid",
          choices = colnames_uploaded,
          selected = init_subjectid
        )

        if ("start_time" %in% colnames_uploaded) {
          init_start_time <- "start_time"
        } else if (
          any(startsWith(colnames_uploaded, "start")) |
            any(startsWith(colnames_uploaded, "START"))
        ) {
          init_start_time <- colnames_uploaded[
            startsWith(colnames_uploaded, "start") |
              startsWith(colnames_uploaded, "START")
          ][1]
        } else {
          init_start_time <- NULL
        }
        shinyWidgets::updatePickerInput(
          session,
          "select_start_time",
          choices = colnames_uploaded,
          selected = init_start_time
        )

        if ("end_time" %in% colnames_uploaded) {
          init_end_time <- "end_time"
        } else if (
          any(startsWith(colnames_uploaded, "end")) |
            any(startsWith(colnames_uploaded, "END"))
        ) {
          init_end_time <- colnames_uploaded[
            startsWith(colnames_uploaded, "end") |
              startsWith(colnames_uploaded, "END")
          ][1]
        } else {
          init_end_time <- NULL
        }
        shinyWidgets::updatePickerInput(
          session,
          "select_end_time",
          choices = colnames_uploaded,
          selected = init_end_time
        )

        if ("event_time" %in% colnames_uploaded) {
          init_event_time <- "event_time"
        } else if (
          any(startsWith(colnames_uploaded, "event_time")) |
            any(startsWith(colnames_uploaded, "EVENT_TIME"))
        ) {
          init_event_time <- colnames_uploaded[
            startsWith(colnames_uploaded, "event_time") |
              startsWith(colnames_uploaded, "EVENT_TIME")
          ][1]
        } else {
          init_event_time <- NULL
        }
        shinyWidgets::updatePickerInput(
          session,
          "select_event_time",
          choices = colnames(uploaded_data$val),
          selected = init_event_time
        )

        if ("event_time_end" %in% colnames_uploaded) {
          init_event_time_end <- "event_time_end"
        } else if (
          any(startsWith(colnames_uploaded, "event_time")) |
            any(startsWith(colnames_uploaded, "EVENT_TIME"))
        ) {
          init_event_time_end <- colnames_uploaded[
            startsWith(colnames_uploaded, "event_time") |
              startsWith(colnames_uploaded, "EVENT_TIME")
          ][1]
        } else {
          init_event_time_end <- NULL
        }
        shinyWidgets::updatePickerInput(
          session,
          "select_event_time_end",
          choices = colnames(uploaded_data$val),
          selected = init_event_time_end
        )

        if ("event" %in% colnames_uploaded) {
          init_event <- "event"
        } else if (
          any(startsWith(colnames_uploaded, "ev")) |
            any(startsWith(colnames_uploaded, "EV"))
        ) {
          init_event <- colnames_uploaded[
            startsWith(colnames_uploaded, "ev") |
              startsWith(colnames_uploaded, "EV")
          ][1]
        } else {
          init_event <- NULL
        }

        shinyWidgets::updatePickerInput(
          session,
          "select_event",
          choices = colnames(uploaded_data$val),
          selected = init_event
        )

        if ("event_group" %in% colnames_uploaded) {
          init_event_group <- "event_group"
        } else if (
          any(startsWith(colnames_uploaded, "event_g")) |
            any(startsWith(colnames_uploaded, "EVENT_G"))
        ) {
          init_event_group <- colnames_uploaded[
            startsWith(colnames_uploaded, "event_g") |
              startsWith(colnames_uploaded, "EVENT_G")
          ][1]
        } else {
          init_event_group <- NULL
        }
        shinyWidgets::updatePickerInput(
          session,
          "select_event_group",
          choices = colnames(uploaded_data$val),
          selected = init_event_group
        )
      }
    })

    #### Variable check after data upload ####
    shiny::observeEvent(
      c(
        uploaded_data$val,
        input$select_subjectid,
        input$select_start_time,
        input$select_end_time,
        input$select_event,
        input$select_event_group,
        input$select_event_time,
        input$select_event_time_end
      ),
      {
        variable_check <- check_megaplot_data_variables(
          check_megaplot_data = uploaded_data$val,
          check_subjectid = input$select_subjectid,
          check_start_time = input$select_start_time,
          check_end_time = input$select_end_time,
          check_event = input$select_event,
          check_event_group = input$select_event_group,
          check_event_time = input$select_event_time,
          check_event_time_end = input$select_event_time_end
        )
        # variable_check$val <- variable_check
        # when check is successful display next button
        if (!is.null(variable_check)) {
          if (variable_check) {
            shinyjs::showElement(id = "upload_1_next_button")
            # variable_check$val <- variable_check
          } else {
            shinyjs::hideElement(id = "upload_1_next_button")
            # variable_check$val <- variable_check
          }
        }
      }
    )

    shiny::observeEvent(uploaded_data$val, {
      if (is.null(uploaded_data$val)) {
        shinyjs::hideElement(id = "selected_events_panel")
      } else {
        shinyjs::showElement(id = "selected_events_panel")
      }
    })

    # shiny::observeEvent(c(
    #   uploaded_data$val, input$select_subjectid,
    #   input$select_start_time, input$select_end_time,
    #   input$select_event, input$select_event_group,
    #   input$select_event_time, input$select_event_time_end
    # ), {
    #   checked_data$val <- NULL
    # })
    # Switch tab (with nav_select) if next button was clicked
    shiny::observeEvent(input$upload_1_next_button, {
      # bslib::nav_select("Upload", "Filtering")
      bslib::nav_select(
        "Upload",
        "Event & color selection 2",
        session = parent_session
      )
    })

    # rename variables due to variable selection
    uploaded_data_renamed <- shiny::reactive({
      # shiny::req(uploaded_data$val)
      # shiny::req(input$select_subjectid)
      rename_require_variables(
        shiny::req(uploaded_data$val),
        selected_subjectid = shiny::req(input$select_subjectid),
        selected_start_time = input$select_start_time,
        selected_end_time = input$select_end_time,
        selected_event = shiny::req(input$select_event),
        selected_event_group = input$select_event_group,
        selected_event_time = input$select_event_time,
        selected_event_time_end = input$select_event_time_end
      )
    })

    # add event & event group identifier to data set and number
    # events within group
    uploaded_data_w_ids <- shiny::reactive({
      create_unique_event_identifier(
        #megaplot_data_raw = shiny::req(uploaded_data$val)
        megaplot_data_raw = shiny::req(uploaded_data_renamed()),
        theme = shiny::isolate(theme())
      )
    })

    #return
    list(
      uploaded_data_renamed = uploaded_data_renamed,
      uploaded_data_w_ids = uploaded_data_w_ids
    )
  })
}


#' UI for the data upload module
#'
#' Renders the file upload input, welcome text, variable selection pickers,
#' and the navigation button for the first upload step. Intended to be placed
#' inside a [bslib::nav_panel()] in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::navset_card_underline()] containing the upload and
#'   variable selection panel.
#' @keywords internal
mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    # create a div with nav panel name and help button
    tags$div(
      HTML(
        paste0(
          "File & variable selection",
          "&emsp;",
          "&emsp;"
        )
      )
    ),
    # create a different named value "File & variable selection 2"
    # used in app_server.R for
    # bslib::nav_select() to switch panels after pressing
    # "NEXT-"/"BACK"-buttons
    value = "File & variable selection 2",
    shiny::wellPanel(
      HTML(
        paste0(
          "<p>
                  WELCOME!
                </p>"
        )
      ),
      HTML(
        paste0(
          "<p>
                  MEGAPLOTS is a huge graphical display showing individual-level data over time.
                  MEGAPLOTS seek to represent longitudinal data while focusing on event visualization!
                </p>"
        )
      ),
      HTML(
        paste0(
          "<p>
                  To get started, please upload your data below, select the desired
                  variables, and click the 'NEXT' button.
                </p>"
        )
      )
    ),
    shiny::wellPanel(
      shiny::column(
        3,
        shiny::fileInput(
          inputId = ns('file'),
          label = "Choose RData file",
          multiple = FALSE,
          accept = '.RData'
        )
      ),
      span(textOutput(ns("file_upload_message")), style = "color:#cc0a21;"),
      tags$style(
        HTML(
          "select[data-max-options=\"1\"] ~ .dropdown-menu .bs-actionsbox .bs-select-all {display: none;}"
        )
      ),
      tags$style(HTML(paste0(
        ".dropdown-toggle::after{
                  content: none;
              }
             "
      ))),
      shiny::fluidRow(
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_subjectid"),
            label = HTML(
              "<p> Identifier <em style = 'color: #f9b8c7;'> *required </em></p>"
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        ),
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_start_time"),
            label = "Timeline Start Day",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        ),
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_end_time"),
            label = "Timeline End Day",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_event"),
            label = HTML(
              "<p> Event <em style = 'color: #f9b8c7;'> *required </em></p>"
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        ),
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_event_group"),
            label = HTML(
              "<p> Event Group <em style = 'color: #f9b8c7;'> *required </em></p>"
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        ),
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_event_time"),
            label = HTML(
              "<p> Event Start Day <em style = 'color: #f9b8c7;'> *required </em></p>"
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        ),
        shiny::column(
          2,
          shinyWidgets::pickerInput(
            inputId = ns("select_event_time_end"),
            label = HTML(
              "<p> Event End Day <em style = 'color: #f9b8c7;'> *required </em></p>"
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              maxOptions = 1,
              actionsBox = TRUE,
              deselectAllText = "Clear",
              dropupAuto = FALSE
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          2,
          shinyWidgets::actionBttn(
            inputId = ns("upload_1_next_button"),
            label = "Next",
            style = "material-flat",
            color = "primary",
            icon = shiny::icon("angle-right")
          )
        )
      )
    ) #,theme = bslib::bs_theme(version = 5)
  )
}
