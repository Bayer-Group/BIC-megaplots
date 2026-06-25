#' UI for the event and colour selection module
#'
#' Renders the jsTree event selector, the colour container panel, and the
#' colour picker controls. Intended to be placed as a [bslib::nav_panel()]
#' inside the `"Upload"` [bslib::navset_card_underline()] in the parent UI.
#'
#' @param id Character string. The module namespace identifier.
#'
#' @return A [bslib::nav_panel()] containing the event and colour selection
#'   interface.
#' @keywords internal
mod_color_selection_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    tags$div(
      HTML(
        paste0(
          "Event & color selection",  "&emsp;","&emsp;"
        )
      )
    ),
    # create a different named value "File & variable selection 2"
    # used in app_server.R for
    # bslib::nav_select() to switch panels after pressing
    # "NEXT-"/"BACK"-buttons
    value = "Event & color selection 2",
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          id = ns("selected_events_panel"),
          style = "overflow-y:scroll; max-height: 10000px;", #init high value and then update max height within server.R
          jsTreeR::jstreeOutput(
            ns("tree2")
          )
        )
      ),
      shiny::column(4,
        shiny::wellPanel(id = ns("selected_events_color_container_panel"),
           style ="overflow-y:scroll;max-height: 10000px;",
           div(
             id = "header-section",
             div(
               id = "selected-cols-row",
               uiOutput(ns("selected_events_color_container"))
             )
           )
        )
      ),
      shiny::column(4,
        shiny::wellPanel(id = ns("colour_picker_panel"),
           shiny::fluidRow(
             shiny::column(12,
               shiny::radioButtons(
                 inputId = ns("color_method"),
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
                 inputId = ns("select_color_palette"),
                 label = "Select color palette",
                 choices = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent", "Spectral", "Rainbow")
               )
             ),
             shiny::column(12,
               shiny::textOutput(
                 ns("colorization_selection")
               )
             ),
             shiny::column(4,
               colourpicker::colourInput(
                 inputId = ns("colour_picker_panel_1"),
                 label = "Click colored event container and use this Picker to update any color",
                 value = "white", allowTransparent = TRUE
               )
             ),
             shiny::column(4,
               colourpicker::colourInput(
                 inputId = ns("colour_picker_panel_2"),
                 label = "Color 2",
                 value = "white", allowTransparent = TRUE
               )
             ),
             shiny::column(4,
               colourpicker::colourInput(
                 inputId = ns("colour_picker_panel_3"),
                 label = "Color 3",
                 value = "blue", allowTransparent = TRUE
               )
             ),
             shiny::column(12,
               shiny::fluidRow(
                 shiny::column(6,
                   colourpicker::colourInput(
                     inputId = ns("colour_picker_panel_event"),
                     label = NULL,
                     value = "white", allowTransparent = TRUE
                   )
                 ),
                 shiny::column(6,
                   shinyWidgets::actionBttn(
                     inputId = ns("update_color_palette_2"),
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
                  inputId = ns("colour_picker_panel_unique"),
                  label = "",
                  value = "white",
                  allowTransparent = TRUE
                )
              )
            ),
            shiny::column(12,
              shiny::plotOutput(ns("colour_palette"), height = "40px")
            )
          ),
          br(),
          shinyWidgets::actionBttn(
            inputId = ns("update_color_palette"),
            label = "Update colors",
            color = "success",
            style = "simple",
            icon = icon("refresh")
          ),
          br(),
          shiny::checkboxInput(
            inputId = ns("jitter_events"),
            label = "Jitter events for event group",
            value = TRUE
          )
        ),
        shiny::conditionalPanel(condition = paste0("output['", ns("color_changed"), "'] == true"),
          shinyWidgets::downloadBttn(
            outputId = ns("save_colors"),
            label = "Save color file",
            icon = icon("save"),
            style = "material-flat",
            color = "primary"
          )
        ),
        shiny::fileInput(
          inputId = ns('upload_saved_color_file'),
          label = "Upload saved color file",
          multiple = FALSE,
          accept = '.rds'
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(1,
        shinyWidgets::actionBttn(
          inputId = ns("upload_3_back_button"),
          label = "Back",
          style = "material-flat",
          color = "primary",
          icon = icon("angle-left")
        )
      ),
      shiny::column(1,
        shinyWidgets::actionBttn(
          inputId = ns("upload_3_next_button"),
          label = "Next",
          style = "material-flat",
          color = "primary",
          icon = icon("angle-right")
        )
      )
    )
  )
}



#' Server logic for the event and colour selection module
#'
#' Manages the jsTree event selector, colour container rendering, all colour
#' picker interactions, jitter toggling, and colour file save/load. Returns
#' `color_data` and `checked_data` reactive values for consumption by the
#' parent server.
#'
#' @param id Character string. The module namespace identifier.
#' @param uploaded_data_renamed A reactive data frame with standardised
#'   `megaplots_selected_*` column names, as returned by
#'   [mod_data_upload_server()].
#' @param uploaded_data_w_ids A reactive data frame with event and event-group
#'   identifiers added, as returned by [mod_data_upload_server()].
#' @param theme A reactive string returning the current theme toggle value
#'   (e.g. `"dark"`). Passed from the parent because `input$theme_toggle`
#'   lives outside this module's namespace.
#' @param js_col_num A reactive returning the column number of the clicked
#'   colour container, sourced from `input$jsColNum` in the parent session.
#'   Passed in because the JavaScript listener is registered at the app level.
#' @param parent_session The parent Shiny session object, used for
#'   [bslib::nav_select()] navigation calls.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`color_data`}{A [shiny::reactiveValues()] object with slots
#'       `all` and `selected`.}
#'     \item{`checked_data`}{A [shiny::reactiveValues()] object with slot
#'       `val` containing the current jsTree selection as a data frame.}
#'   }
#' @keywords internal
mod_color_selection_server <- function(
  id,
  window_height,
  uploaded_data_renamed,
  uploaded_data_w_ids,
  theme,
  js_col_num,
  parent_session) {

    shiny::moduleServer(id, function(input, output, session) {

      stopifnot(
        "mod_color_selection_server expects `uploaded_data_renamed` to be a reactive." = {
          shiny::is.reactive(uploaded_data_renamed)
        },
        "mod_color_selection_server expects `uploaded_data_w_ids` to be a reactive." = {
          shiny::is.reactive(uploaded_data_w_ids)
        },
        "mod_color_selection_server expects `theme` to be a reactive." = {
          shiny::is.reactive(theme)
        },
        "mod_color_selection_server expects `js_col_num` to be a reactive." = {
          shiny::is.reactive(js_col_num)
        }
      )

      ns <- session$ns

      shinyjs::hideElement(id = "selected_events_panel")
      shinyjs::hideElement(id = "selected_events_color_container_panel")
      shinyjs::hideElement(id = "colour_picker_panel_1")
      shinyjs::hideElement(id = "colour_picker_panel_2")
      shinyjs::hideElement(id = "colour_picker_panel_3")
      shinyjs::hideElement(id = "colour_picker_panel_event")
      shinyjs::hideElement(id = "update_color_palette_2")
      shinyjs::hideElement(id ="color_method")
      shinyjs::hideElement(id = "jitter_events")
      shinyjs::hideElement(id = "update_color_palette")
      shinyjs::hideElement(id = "colour_picker_panel_unique")
      shinyjs::hideElement(id = "colour_picker_panel")



    #### Reactive value color_data ####
    # initialize reactive value color_data with entries "all" and "selected"
    # all includes all events from uploaded data set and in selected filtered by
    # selected events in shinyTree input
    color_data <- shiny::reactiveValues(
      all = NULL,
      selected = NULL
    )
    checked_data <- shiny::reactiveValues(val = NULL)

    color_changed <- shiny::reactiveValues(val = FALSE)

    #create reactive variable for selected color container row
    js_column <- shiny::reactiveValues(number = NULL)

    shiny::observeEvent(js_col_num(), {
      js_column$number <- js_col_num()
    })

    shiny::observe({
      js_col_num()
      shiny::req(js_column$number)
      shiny::req(color_data$selected)
      if (is.na(color_data$selected[js_column$number, "megaplots_selected_event"])) {
        # shinyjs::showElement(id = "colour_picker_panel")
        shinyjs::showElement(id = "colour_picker_panel")
        shinyjs::showElement(id = "colour_picker_panel_event")
        if (input$color_method == "gradient") {
          shinyjs::hideElement(id = "colour_picker_panel_event")
          shinyjs::hideElement(id = "update_color_palette_2")
          shinyjs::showElement(id = "colour_picker_panel_1")
          shinyjs::showElement(id = "colour_picker_panel_2")
          shinyjs::showElement(id = "colour_picker_panel_3")
          shinyjs::hideElement(id = "select_color_palette")
          shinyjs::hideElement(id = "colour_picker_panel_unique")
        } else  if (input$color_method == "unique") {
          shinyjs::hideElement(id = "colour_picker_panel_event")
          shinyjs::hideElement(id = "update_color_palette_2")
          shinyjs::hideElement(id = "colour_picker_panel_1")
          shinyjs::hideElement(id = "colour_picker_panel_2")
          shinyjs::hideElement(id = "colour_picker_panel_3")
          shinyjs::showElement(id = "colour_picker_panel_unique")
          shinyjs::hideElement(id = "select_color_palette")
        } else  if (input$color_method == "palette") {
          shinyjs::showElement(id = "select_color_palette")
          shinyjs::hideElement(id = "colour_picker_panel_event")
          shinyjs::hideElement(id = "update_color_palette_2")
          shinyjs::hideElement(id = "colour_picker_panel_1")
          shinyjs::hideElement(id = "colour_picker_panel_2")
          shinyjs::hideElement(id = "colour_picker_panel_3")
          shinyjs::hideElement(id = "colour_picker_panel_unique")
        }
        shinyjs::showElement(id = "jitter_events")
        shinyjs::showElement(id = "colour_palette")
        #shinyjs::showElement(id = "select_color_palette")
        shinyjs::showElement(id = "update_color_palette")
        shinyjs::showElement(id = "color_method")
      } else {
        shinyjs::showElement(id = "colour_picker_panel")
        # shinyjs::hideElement(id = "colour_picker_panel")
        shinyjs::showElement(id = "colour_picker_panel_event")
        shinyjs::showElement(id = "update_color_palette_2")
        shinyjs::hideElement(id = "colour_picker_panel_1")
        shinyjs::hideElement(id = "colour_picker_panel_2")
        shinyjs::hideElement(id = "colour_picker_panel_3")
        shinyjs::hideElement(id = "colour_picker_panel_unique")
        shinyjs::hideElement(id = "jitter_events")
        shinyjs::hideElement(id = "colour_palette")
        shinyjs::hideElement(id = "select_color_palette")
        shinyjs::hideElement(id = "update_color_palette")
        shinyjs::hideElement(id = "color_method")
      }
    })

    shiny::observeEvent(c(input$update_color_palette, input$update_color_palette_2), {
      color_changed$val <- TRUE
    }, ignoreInit = TRUE)

    output$color_changed <- shiny::reactive({
      color_changed$val
    })

    outputOptions(output, "color_changed", suspendWhenHidden = FALSE)

    #
    shiny::observeEvent(uploaded_data_renamed(), {
      if (is.null(uploaded_data_renamed())) {
        shinyjs::hideElement(id = "selected_events_panel")
      } else {
        shinyjs::showElement(id = "selected_events_panel")
        shinyjs::showElement(id = "upload_saved_color_file")
      }
    })


    shiny::observeEvent(checked_data$val, {
      if (is.null(checked_data$val) || nrow(checked_data$val) == 0) {
        shinyjs::hideElement(id = "colour_picker_panel")
        shinyjs::hideElement(id = "selected_events_color_container_panel")
      } else {
        shinyjs::showElement(id = "selected_events_color_container_panel")
      }
    })

    #Changing the maximum height of the upload panels depending on screen size
    shiny::observe({
      shinyjs::runjs(
        sprintf(
          paste0(
            "document.getElementById('",ns("selected_events_panel"),"').style.maxHeight =
          '", window_height() - 300, "px';"
          )
        )
      )
      shinyjs::runjs(
        sprintf(
          paste0("document.getElementById('selected-cols-row').style.maxHeight =
               '", window_height() - 300, "px';"
          )
        )
      )
    })

    #create a data frame which includes unique rows with
    # event /event group information
    unique_event_group_data <- shiny::eventReactive(uploaded_data_renamed(), {
      #megaplot_data_raw <- shiny::req(uploaded_data$val)
      megaplot_data_raw <- shiny::req(uploaded_data_renamed())
      reduced_event_data <- megaplot_data_raw |>
        dplyr::filter(!is.na(.data$megaplots_selected_event)) |>
        dplyr::select(tidyselect::all_of(c("megaplots_selected_event_group", "megaplots_selected_event"))) |>
        dplyr::distinct() |>
        dplyr::arrange(.data$megaplots_selected_event_group, .data$megaplots_selected_event)

      reduced_event_data
    })

    #### START jsTreeR PART ####
    output$tree2 <- jsTreeR::renderJstree({
      #javascript code to avoid that child nodes moved into another child nodes

      check_callback <- jsTreeR::JS(
        "function(operation, node, parent, position, more) {",
        "  if (operation === 'move_node') {",
        "    var isRootDrop    = parent.id === '#';",
        "    var isChildParent = parent.type === 'child';",
        "    var sameParent    = node.parent === parent.id;",
        # `more.ref` guards against cross-parent drops near parent boundaries
        "    var refParentOk   = !more.ref || more.ref.parent === node.parent;",
        "    if (isRootDrop || isChildParent || !sameParent || !refParentOk) {",
        "      return false;",
        "    }",
        "  }",
        "  return true;",
        "}"
      )

      drag_and_drop_config <- list(
        is_draggable = jsTreeR::JS(
          "function(node) {",
          "  return node[0].type === 'child';",
          "}"
        ),
        always_copy = FALSE,
        use_html5   = FALSE,
        copy        = FALSE
      )

      #icons
      types <- list(
        root = list(icon = "fa-regular fa-arrow_pointer"),
        child = list(icon = "fa-regular fa-arrow_pointer")
      )

      suppressMessages(
        jsTreeR::jstree(
          create_jsTree_input(data = unique_event_group_data()),
          #use create_jsTree_input function to create desired list input
          dragAndDrop = TRUE,
          search = list(show_only_matches = TRUE),
          dnd = drag_and_drop_config,
          checkCallback = check_callback,
          types = types,
          contextMenu = list(create = FALSE, delete = FALSE),
          checkboxes = TRUE
        )
      )
    })

    # shiny::observe({
    #   color_data$all
    # })
    #### END jsTreeR PART ####

    shiny::observeEvent(c(
      input[["tree2"]],
      input[["tree2_checked_tree"]]
    ), {

      checked_tree <- input[["tree2_checked_tree"]]
      selected_data <- data.frame(megaplots_selected_event_group = NULL, megaplots_selected_event = NULL)
      if (!is.null(checked_tree)) {
        if (length(checked_tree) > 0) {
          for (i in seq_along(checked_tree)) {
            for (j in seq_along(checked_tree[[i]]$children)) {
              if (j == 1) {
                selected_data <- rbind(
                  selected_data,
                  data.frame(megaplots_selected_event_group = checked_tree[[i]]$text, megaplots_selected_event = NA)
                )
              }
              selected_data <- rbind(
                selected_data,
                data.frame(
                  megaplots_selected_event_group = checked_tree[[i]]$text,
                  megaplots_selected_event = checked_tree[[i]]$children[[j]]$text
                )
              )
            }
          }
        }
      }

      shinyjqui::updateOrderInput(
        parent_session,
        inputId = "plot_appearance-sort_event_groups",
        label = "Sort event groups",
        items = unique(selected_data$megaplots_selected_event_group)
      )
      checked_data$val <- selected_data
    })


    shiny::observeEvent(c(
      #   uploaded_data$val, input$select_subjectid,
      #   input$select_start_time, input$select_end_time,
      #   input$select_event, input$select_event_group,
      #   input$select_event_time, input$select_event_time_end
      uploaded_data_renamed()
    ), {
      checked_data$val <- NULL
      js_column$number <- NULL
    })

    shiny::observeEvent(uploaded_data_w_ids(), {
      uploaded_data_w_ids()
      #update data set color_data$all
      color_data$all <- uploaded_data_w_ids()
    })


    shiny::observeEvent(theme(), {
      if (!is.null(color_data$all)) {
        if (nrow(color_data$all)>0) {
          header_color <- ifelse(theme() =="dark", "#1D1F21", "#fff")
          color_data$all <- color_data$all |>
            dplyr::mutate(
              event_color = dplyr::case_when(
                is.na(megaplots_selected_event) ~ header_color,
                !is.na(megaplots_selected_event) ~ event_color
              )
            )
        }
      }
    })

    shiny::observeEvent(js_column$number, {
      if (!is.null(js_column$number)) {

      } else  {
        shinyjs::hideElement(id = "colour_picker_panel_1")
        shinyjs::hideElement(id = "colour_picker_panel_2")
        shinyjs::hideElement(id = "colour_picker_panel_3")
        shinyjs::hideElement(id = "update_color_palette")
        shinyjs::hideElement(id = "color_method")
        shinyjs::hideElement(id = "jitter_events")
        shinyjs::hideElement(id = "colour_picker_panel_event")
        shinyjs::hideElement(id = "update_color_palette_2")
        shinyjs::hideElement(id = "colour_picker_panel_unique")
        shinyjs::hideElement(id = "colour_picker_panel")
      }
    }, ignoreNULL = FALSE)


    #observer to update ColourInput based on container click
    shiny::observeEvent(js_column$number, {
      output$colorization_selection <- shiny::renderText(
        ifelse(
          is.na(color_data$selected[js_column$number, "event"]),
          color_data$selected[js_column$number, "event_group"],
          color_data$selected[js_column$number, "event"]
        )
      )
      colourpicker::updateColourInput(
        session,
        inputId = "colour_picker_panel_event",
        label = "",
        value = color_data$selected[js_column$number, "event_color"],
        allowTransparent = TRUE
      )
      colourpicker::updateColourInput(
        session,
        inputId = "colour_picker_panel_1",
        label = "",
        value = color_data$selected[js_column$number, "gradient_event_color_1"],
        allowTransparent = TRUE
      )
      colourpicker::updateColourInput(
        session,
        inputId = "colour_picker_panel_2",
        label = "",
        value = color_data$selected[js_column$number, "gradient_event_color_2"],
        allowTransparent = TRUE
      )
      colourpicker::updateColourInput(
        session,
        inputId = "colour_picker_panel_3",
        label = "",
        value = color_data$selected[js_column$number, "gradient_event_color_3"],
        allowTransparent = TRUE
      )
      colourpicker::updateColourInput(
        session,
        inputId = "colour_picker_panel_unique",
        label = "",
        value = color_data$selected[js_column$number, "gradient_event_color_2"],
        allowTransparent = TRUE
      )

      val <- any(color_data$all[
        color_data$all$megaplots_selected_event_group ==
          color_data$selected[js_column$number, ]$megaplots_selected_event_group,
      ]$jittered
      )
      shiny::updateCheckboxInput(
        session,
        inputId = "jitter_events",
        value = val
      )

    })


    shiny::observeEvent(input$jitter_events, {
      #require the selected number of colored div container list
      shiny::req(js_column$number)

      if (!is.null(color_data$all)) {
        if (!is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")] &
              color_data$all$megaplots_selected_event ==
              color_data$selected[js_column$number, c("megaplots_selected_event")] &
              !is.na(color_data$all$megaplots_selected_event ==
                       color_data$selected[js_column$number, c("megaplots_selected_event")]),
          ]$jittered <- input$jitter_events
        }
        if (is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
          color_data$all[
            color_data$all$megaplots_selected_event_group ==
              color_data$selected[js_column$number, c("megaplots_selected_event_group")],
          ]$jittered <- input$jitter_events
        }
      }
    })


    shiny::observeEvent(input$update_color_palette_2, {
      shiny::req(js_column$number)
      if (!is.null(color_data$all)) {
        if (!is.null(color_data$selected)) {
          if (!is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
            color_data$all[
              color_data$all$megaplots_selected_event_group ==
                color_data$selected[js_column$number, c("megaplots_selected_event_group")] &
                color_data$all$megaplots_selected_event ==
                color_data$selected[js_column$number, c("megaplots_selected_event")]
              & !is.na(color_data$all$megaplots_selected_event ==
                         color_data$selected[js_column$number, c("megaplots_selected_event")]),
            ]$event_color <- input$colour_picker_panel_event
          }
        }
      }
    })



    shiny::observeEvent(c(input$update_color_palette), {
      #require the selected number of colored div container list
      shiny::req(js_column$number)
      if (!is.null(color_data$all)) {
        if (!is.null(color_data$selected)) {
          if (is.na(color_data$selected[js_column$number, c("megaplots_selected_event")])) {
            #create new color for entire event group
            if (input$color_method == "gradient") {
              f_col_z <- scales::colour_ramp(
                c(input$colour_picker_panel_1,
                  input$colour_picker_panel_2,
                  input$colour_picker_panel_3
                )
              )
              cds_tmp <- color_data$selected[color_data$selected$megaplots_selected_event_group ==
                                               color_data$selected[js_column$number,
                                                                   c("megaplots_selected_event_group")],
                                             c("megaplots_selected_event_group", "megaplots_selected_event")] |>
                dplyr::filter(!is.na(.data$megaplots_selected_event)) |>
                dplyr::mutate(new_event_id = dplyr::row_number(.data$megaplots_selected_event_group))

              new_event_group_color <- color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[
                    js_column$number, c("megaplots_selected_event_group")], ] |>
                dplyr::left_join(cds_tmp, by = c("megaplots_selected_event", "megaplots_selected_event_group")) |>
                dplyr::mutate(event_id = .data$new_event_id)

              new_group_id2 <- c(new_event_group_color$new_event_id)

              new_event_group_color2 <- new_event_group_color |>
                dplyr::select(-tidyselect::all_of(c("new_event_id"))) |>
                dplyr::mutate(
                  event_color =
                    f_col_z(seq(0, 1, length = sum(!is.na(new_event_group_color$event_id)))
                    )[.data$event_id]
                ) |>
                dplyr::pull(.data$event_color)

              new_event_group_color3 <- c(new_event_group_color2)

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$event_color <- new_event_group_color3

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$event_id <- new_group_id2

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$gradient_event_color_1 <- input$colour_picker_panel_1

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$gradient_event_color_2 <- input$colour_picker_panel_2

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$gradient_event_color_3 <- input$colour_picker_panel_3

            } else if (input$color_method == "unique") {
              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$event_color <-  input$colour_picker_panel_unique
              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$gradient_event_color_1 <- input$colour_picker_panel_unique
              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$gradient_event_color_2 <- input$colour_picker_panel_unique
              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$gradient_event_color_3 <- input$colour_picker_panel_unique

            } else if (input$color_method == "palette") {

              number_events <- nrow(
                color_data$selected[
                  color_data$selected$megaplots_selected_event_group ==
                    color_data$selected[js_column$number, "megaplots_selected_event_group"] &
                    !is.na(color_data$selected$megaplots_selected_event), ]
              )


              selected_color_palette <- create_palette(n = number_events, name = input$select_color_palette)

              cds_tmp <- color_data$selected[color_data$selected$megaplots_selected_event_group ==
                                               color_data$selected[js_column$number,
                                                                   c("megaplots_selected_event_group")],
                                             c("megaplots_selected_event_group", "megaplots_selected_event")] |>
                dplyr::filter(!is.na(.data$megaplots_selected_event)) |>
                dplyr::mutate(new_event_id = dplyr::row_number(.data$megaplots_selected_event_group))

              new_event_group_color <- color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[
                    js_column$number, c("megaplots_selected_event_group")], ] |>
                # dplyr::filter(.data$event_id >= 1)  |>
                dplyr::left_join(cds_tmp, by = c("megaplots_selected_event", "megaplots_selected_event_group")) |>
                dplyr::mutate(event_id = .data$new_event_id)

              new_group_id <- c(new_event_group_color$new_event_id)#, 0)


              new_event_group_color2 <- new_event_group_color |>
                dplyr::select(-tidyselect::all_of(c("new_event_id"))) |>
                dplyr::mutate(
                  event_color =  selected_color_palette[.data$event_id]
                ) |>
                dplyr::pull(.data$event_color)

              new_event_group_color3 <- c(new_event_group_color2)#, "#404A4E")

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$event_color <- new_event_group_color3

              color_data$all[
                color_data$all$megaplots_selected_event_group ==
                  color_data$selected[js_column$number, c("megaplots_selected_event_group")],
              ]$event_id <- new_group_id

            }
          }
        }
      }
    })

    #### Color container output ####
    output$selected_events_color_container <- renderUI({
      if(!is.null(checked_data$val)) {
        if (nrow(checked_data$val) > 0)  {

          selected_data <- create_color_container(
            tree = checked_data$val,
            color_vector = color_data$all,
            theme = theme()
          )

          color_data$selected <- selected_data
          #apply through all events & event groups and create
          # a div container for the color settings
          lapply(seq_along(selected_data$names_for_color_list),
                 function(column_number) {
                   div(
                     class = "col col-transparent-box selected",
                     div(
                       class = "selected-col-inner",
                       style = paste0(
                         "border: ", ifelse(column_number == js_column$number,"3px solid","0px"),";",
                         "border-color: ",font_color(selected_data[column_number, ]$event_color),";",
                         "background:", ifelse(
                           selected_data[column_number, ]$type == "megaplots_selected_event",
                           selected_data[column_number, ]$event_color,  ifelse(theme() =="dark","#1D1F21","#fff")),
                         ";",
                         "padding:", ifelse(
                           selected_data[column_number, ]$type == "megaplots_selected_event",
                           "2px 2px 2px 2px",
                           "2px 2px 2px 50px"),
                         ";",
                         "color: ",
                         font_color(selected_data[column_number, ]$event_color),
                         ";"

                       ),
                       `data-colnum` = column_number,
                       selected_data[column_number, ]$names_for_color_list
                     )
                   )
                 })
        } else {
          NULL
        }
      }
    })

    #### START COLOUR PALETTE PART ####
    output$colour_palette <- shiny::renderPlot({
      if (!is.null(color_data$selected) & !is.null(js_column$number)) {
        if (input$color_method == "gradient") {

          number_events <- nrow(
            color_data$selected[
              color_data$selected$megaplots_selected_event_group ==
                color_data$selected[js_column$number, "megaplots_selected_event_group"] &
                !is.na(color_data$selected$megaplots_selected_event),
            ])

          f_col_z  <- scales::colour_ramp(
            colors = c(
              input$colour_picker_panel_1,
              input$colour_picker_panel_2,
              input$colour_picker_panel_3
            ),
            alpha = TRUE
          )

          par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
          graphics::image(
            1:number_events,
            1,
            as.matrix(1:number_events),
            col = #grDevices::rgb(
              f_col_z(seq(0, 1, length = number_events)),
            #,maxColorValue = 255
            #),
            xlab = "",
            ylab = "",
            xaxt = "n",
            yaxt = "n",
            bty = "n"
          )
          graphics::abline(h = 0.602, lwd = 3, col = "#000000")
          graphics::abline(h = 1.398, lwd = 3, col = "#000000")
        } else if (input$color_method == "unique") {
          number_events <- nrow(
            color_data$selected[color_data$selected$megaplots_selected_event_group ==
                                  color_data$selected[js_column$number,
                                                      "megaplots_selected_event_group"] &
                                  !is.na(color_data$selected$megaplots_selected_event), ])
          par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
          graphics::image(
            1:number_events,
            1,
            as.matrix(1:number_events),
            col = input$colour_picker_panel_unique,
            xlab = "",
            ylab = "",
            xaxt = "n",
            yaxt = "n",
            bty = "n"
          )
          graphics::abline(h = 0.602, lwd = 3, col = "#000000")
          graphics::abline(h = 1.398, lwd = 3, col = "#000000")
        } else if (input$color_method == "palette") {

          number_events <- nrow(
            color_data$selected[
              color_data$selected$megaplots_selected_event_group ==
                color_data$selected[js_column$number, "megaplots_selected_event_group"] &
                !is.na(color_data$selected$megaplots_selected_event), ]
          )

          selected_color_palette <- create_palette(n = number_events, name = input$select_color_palette)

          par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
          graphics::image(
            1:number_events,
            1,
            as.matrix(1:number_events),
            col = selected_color_palette[1:number_events],
            xlab = "",
            ylab = "",
            xaxt = "n",
            yaxt = "n",
            bty = "n"
          )
          graphics::abline(h = 0.602, lwd = 3, col = "#000000")
          graphics::abline(h = 1.398, lwd = 3, col = "#000000")
        }
      } else {

        par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        graphics::image(
          1:10,
          1,
          as.matrix(1:10),
          col = "#404A4E",
          xlab = "",
          ylab = "",
          xaxt = "n",
          yaxt = "n",
          bty = "n"
        )
      }
    })
    #### END COLOUR PALETTE PART ####

    # downloadHandler for "Save color file"-option
    output$save_colors <- shiny::downloadHandler(
      filename = function() {
        paste("Megaplot_color_file", gsub(":", "-", Sys.Date()), ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(
          color_data$all |>
            dplyr::select(
              .data$megaplots_selected_event,
              .data$megaplots_selected_event_group,
              .data$event_color,
              .data$gradient_event_color_1,
              .data$gradient_event_color_2,
              .data$gradient_event_color_3,
              .data$jittered
            )
          , file
        )
      }
    )

    #observer for uploading a saved color file
    shiny::observeEvent(input$upload_saved_color_file, {

      #get dimension of color file and uploaded saved color file
      dimension_color_data <- dim(color_data$all)
      uploaded_colors <- readRDS(input$upload_saved_color_file$datapath)

      #add checks if uploaded data matches the existing color_data
      if (dim(uploaded_colors)[1] == dim(color_data$all)[1]) {
        if (all(sort(unique(uploaded_colors$megaplots_selected_event)) == sort(unique(color_data$all$megaplots_selected_event)))) {
          uploaded_colors <- uploaded_colors  |>
            dplyr::mutate(
              gradient_event_color_1 = dplyr::case_when(is.na(.data$megaplots_selected_event) ~ gradient_event_color_1, !is.na(.data$megaplots_selected_event) ~ NA),
              gradient_event_color_2 = dplyr::case_when(is.na(.data$megaplots_selected_event) ~ gradient_event_color_2, !is.na(.data$megaplots_selected_event) ~ NA),
              gradient_event_color_3 = dplyr::case_when(is.na(.data$megaplots_selected_event) ~ gradient_event_color_3, !is.na(.data$megaplots_selected_event) ~ NA)
            )
          color_data_new <- color_data$all |>
            dplyr::select(.data$megaplots_selected_event, .data$megaplots_selected_event_group, .data$event_id, .data$event_group_id, .data$max_event_id, .data$event_n, .data$n_flag) |>
            dplyr::left_join(
              uploaded_colors |>
                dplyr::select(.data$megaplots_selected_event, .data$megaplots_selected_event_group, .data$event_color, .data$gradient_event_color_1, .data$gradient_event_color_2, .data$gradient_event_color_3, .data$jittered),
              by = c("megaplots_selected_event", "megaplots_selected_event_group")
            )
          #check new dimension matches
          if (all(dimension_color_data == dim(color_data_new))) {
            color_data$all <- color_data_new
          }
        }
      }
    })
    #### Observer to update selected navbar based on button click ####
    shiny::observeEvent(input$upload_3_next_button, {
      bslib::nav_select("MEGAPLOTS", "Megaplots", session = parent_session)
    })
    shiny::observeEvent(input$upload_3_back_button, {
      #bslib::nav_select("Upload", "Filtering")
      bslib::nav_select("Upload", "File & variable selection 2", session = parent_session)
    })

    # shiny::observeEvent(checked_data$val, {
    #   js_column$number <- NULL
    # })

    #module return
    list(
      color_data = color_data,
      checked_data = checked_data
    )
    })
}
