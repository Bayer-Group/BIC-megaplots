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

    # custom css style of custom "helptext_status" used in fct_help_text_dropdown_button.R
    shiny::tags$style(
      ".btn-helptext_status {background-color: #404A4E; color: #0091DF; border: 0.2px solid; border-color:#6d787d;}"
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
        version = 5,
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
            ),
            # orderInput widget from shinyjqui package to drag and drop the
            # order of groups (if applicable)
            shiny::uiOutput("arrange_groups"),
          ),
          bslib::accordion_panel(
            "Plot appearance",
            icon = bsicons::bs_icon("border-width"),
            shinyjqui::orderInput(
              inputId = "sort_event_groups",
              label = "Change Event Group Order",
              items = NULL,
              width = 300
            ),
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
          bslib::nav_panel(
            # create a div with nav panel name and help button
            tags$div(
              HTML(
                paste0(
                  "File & variable selection", "&emsp;","&emsp;"
                )
              ),
              #custom dropdownbutton with help text
              help_text_dropdown_button(
                id = "file_variable_selection_helptext",
                text = shiny::wellPanel(style = "width: 1000px;",
                  HTML(
                  paste0(
                    "
                    <p> File selection </p>
                    <p>
                    Please click on the “Browse…” button and upload the desired data set.
                    Currently, it is only possible to use ‘.RData’ as data format. If other
                    formats are used, an error message appears.
                    </p>
                      <img src='www/Screenshots/1_Megaplots.png' align='center' width='90%'/>
                    <p>
                      After successful upload, options for the variable selection will appear.
                    </p>
                      <img src='www/Screenshots/4_Megaplots.png' align='center' width='90%'/>
                     <p> Event selection </p>
                    <p>
                      The variable ‘Identifier’ can be a numeric or character variable and
                      will be used to assign individual time sequences. The variables
                      ‘Timeline Start Day’ and ‘Timeline End Day’ define the start and end
                      time of every individual time course. Both variables should be integers,
                      otherwise they will be rounded. ‘Event’ and ‘Event Group’ should contain
                      the names of the events that are displayed. Multiple events can belong
                      to a specific event group. If no event group is selected, event is
                      automatically set as event group. The event names do not necessarily
                      have to be unique. For display purposes, they are renamed if non-unique,
                      by writing the event group in brackets after them. Like the time
                      courses, every event requires a start and end day. The corresponding
                      variables can be selected via ‘Event Start Day’ and ‘Event End Day’. The
                      same rounding procedure is applied as for the timelines.
                    </p>
                    "
                  )
                )
              ),
              right = FALSE
            )
          ),
          # create a different named value "File & variable selection 2"
          # used in app_server.R for
          # bslib::nav_select() to switch panels after pressing
          # "NEXT-"/"BACK"-buttons
          value = "File & variable selection 2",
          shiny::fluidRow(
            shiny::column(3,
              shiny::fileInput(
                inputId = 'file',
                label = "Choose RData file",
                multiple = FALSE,
                accept = '.RData'
              )
            ),
            span(textOutput("file_upload_message"),style = "color:#cc0a21;")
          ),
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
              shiny::column(2,
                shinyWidgets::pickerInput(
                  inputId = "select_subjectid",
                  label = "Identifier",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options =  pickerOptions(
                    maxOptions = 1,
                    actionsBox = TRUE,
                    deselectAllText = "Clear",
                    dropupAuto = FALSE
                  )
                )
              ),
              shiny::column(2,
                shinyWidgets::pickerInput(
                  inputId = "select_start_time",
                  label = "Timeline Start Day",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options =  pickerOptions(
                    maxOptions = 1,
                    actionsBox = TRUE,
                    deselectAllText = "Clear",
                    dropupAuto = FALSE
                  )
                )
              ),
              shiny::column(2,
                shinyWidgets::pickerInput(
                  inputId = "select_end_time",
                  label = "Timeline End Day",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options =  pickerOptions(
                    maxOptions = 1,
                    actionsBox = TRUE,
                    deselectAllText = "Clear",
                    dropupAuto = FALSE
                  )
                )
              )
            ),
           shiny::fluidRow(
             shiny::column(2,
               shinyWidgets::pickerInput(
                 inputId = "select_event",
                 label = "Event",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
                 options =  pickerOptions(
                   maxOptions = 1,
                   actionsBox = TRUE,
                   deselectAllText = "Clear",
                   dropupAuto = FALSE
                 )
               )
             ),
             shiny::column(2,
               shinyWidgets::pickerInput(
                 inputId = "select_event_group",
                 label = "Event Group",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
                 options =  pickerOptions(
                   maxOptions = 1,
                   actionsBox = TRUE,
                   deselectAllText = "Clear",
                   dropupAuto = FALSE
                 )
               )
             ),
              shiny::column(2,
                shinyWidgets::pickerInput(
                  inputId = "select_event_time",
                  label = "Event Start Day",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options =  pickerOptions(
                    maxOptions = 1,
                    actionsBox = TRUE,
                    deselectAllText = "Clear",
                    dropupAuto = FALSE
                  )
                )
              ),
              shiny::column(2,
                shinyWidgets::pickerInput(
                  inputId = "select_event_time_end",
                  label = "Event End Day",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options =  pickerOptions(
                    maxOptions = 1,
                    actionsBox = TRUE,
                    deselectAllText = "Clear",
                    dropupAuto = FALSE
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
            ),theme = bslib::bs_theme(version = 5)
          ),
          bslib::nav_panel(
            tags$div(
              HTML(
                paste0(
                  "Event & color selection",  "&emsp;","&emsp;"
                )
              ),
              help_text_dropdown_button(
                id = "event_color_selection_helptext",
                text = wellPanel(style ="width: 1000px;",
                 HTML(
                   paste0(
                     "
                    <p> Event & Color Selection</p>
                    <p>
                    In this panel, the events that should be displayed can be selected.
                    Events can also be sorted and all event colors can be defined. All
                    settings can be saved so that they can be quickly reloaded in future
                    sessions. First, the events are selected on the left side.
                    </p>
                    <p>  Event / Event Group Tree </p>
                    <p>
                    On the left side appears a list with checkboxes for all event groups.
                    Clicking on a box selects all events of the event group. The small
                    arrows next to the box can be used to expand the list. Then also every
                    event appears in the list.
                    </p>
                    <img src='www/Screenshots/7_Megaplots.png' align='center' width='95%'/>
                    <p>
                    This can be used to select only individual events of a event group. To
                    find individual events or event groups in a long list, the search box
                    above the list can be used. For every selected event a colored box
                    appears right to the event selection list. These can be used to set the
                    colors (see next chapter).
                    </p>
                    <p>
                    Another feature is the ‘drag and drop’ of the events within the list.
                    This can be used to sort events within a event group. This can be
                    particularly helpful for ordinal scaled events. Sorting affects the
                    legend displayed next to the graph but can also be used to create a
                    color palette explained in the next chapter. After sorting, any box of
                    the event selection list must be clicked again to update the color
                    selection. Further known issues with the drag & drop feature are that no
                    events may be selected in order to move variables and events can be
                    dragged out of the event group, which leads to errors.
                    </p>
                    <p>
                    After completing the event selection, it is possible to go directly to
                    the megaplot graphic by clicking the next button, or make various color
                    settings, which are explained in the next section.
                    </p>
                    <p> Event / Event Group Color List </p>
                    <p>
                    In the middle of the ‘Event & color selection’-panel, a colored list
                    with all selected event groups and events appears. It is now possible to
                    mark an event group or a single event by clicking on the corresponding
                    event/event group name. The selected event or event group gets a
                    highlighted border.
                    </p>
                    <img src='www/Screenshots/10_Megaplots.png' align='center' width='95%'/>
                    <p>
                    By clicking one of these events a color selection panel on the right
                    side appears. Depending on whether a single event or a event group is
                    clicked, the options in the color selection panel differ.
                    </p>
                    <p>
                    Note: If you change the event selection on the left side after making a
                    selection for the colors in the middle, it may happen that a event/event
                    group selection does not respond. By clicking any other event in the
                    color list updates the selection and resolve the issue.
                    </p>

                    <p> Color Selection Panel </p>
                    <p>
                    After selecting an event or event group in the color list, options to
                    change the colors appear on the right side. There is also an jitter
                    option to adjust the event position of the event group, which will be
                    explained in the chapter after next.
                    </p>
                    <p> Color Methods </p>
                    <p>
                    Selecting suitable colors is particularly difficult when many different
                    events are to be represented. The user is responsible for selecting
                    suitable colors and take color blindness into account. For the correct
                    color selection, please refer to the various best practice guides. The
                    methods here are only intended as an aid to their implementation.
                    </p>
                    <p>
                    When an event group is selected, there are three different methods to
                    colorize the events: ‘Color gradient (3 colors)’, ‘Unique color for all
                    events withing group’ and ‘Distinct color by selected palette’.
                    </p>
                    <img src='www/Screenshots/11_Megaplots.png' align='center' width='95%'/>
                    <p>
                    When using color gradient, three color inputs appear which will define a
                    color palette displayed below. In this example, the colors yellow, gray
                    and turquoise are used. Depending on how many events are there within an
                    event group, there are that many colors, created by these three colors.
                    By clicking one of these color inputs a color palette appears which can
                    be used to change the colors and their saturation. However, it is also
                    possible to directly access a color hexcode into the color box. Once a
                    satisfactory color palette has been found, click the ‘Update colors’
                    button to apply the changes.
                    </p>
                    <p>
                    The second option ‘Unique color for all events within group’ obviously
                    colors all events of an event group in the same color. So there will be
                    exactly one color input. Here too, the color can be selected using the
                    palette or by entering the hexcode.
                    </p>
                    <p>
                    The third option ‘Distinct color by selected palette’ brings up another
                    drop-down menu ‘Select color palette’ where predefined color palettes
                    can be selected. By selecting one of these palettes a good visulization
                    is not guaranteed and must be ensured by the user.
                    </p>
                    <p>
                    If an single event is selected in the color list the color selection on
                    the right will be the same for ‘Unique color for all events within
                    group’ explained above. So it is possible to first create a color
                    palette for the complete event group and after that change individual
                    colors by selection the event.
                    </p>
                    <img src='www/Screenshots/14_Megaplots.png' align='center' width='95%'/>

                    <p> Offset Events</p>

                    <p>
                    In order to better distinguish events in addition to color, they are
                    also slightly offset in height by default. To prevent this, the check
                    mark can be removed for option 'Offset events for event group'.
                    </p>
                    <img src='www/Screenshots/18_Megaplots.png' align='center' width='50%'/>

                    <p>
                    The offset position will be the same for each time course. An offset
                    position for events within an event group only makes sense if different
                    events at a given day can appear. For unique events within a event
                    group, it is recommended to disable this option.
                    </p>
                    <p> Save & Upload Color Files </p>

                    <p>
                    Once all color settings have been made, they can be saved via the ‘save
                    color file’-button.
                    </p>
                    <img src='www/Screenshots/19_Megaplots.png' align='center' width='20%'/>
                    <p>
                    This saves a data frame as ‘.rds’-file including the selected event and
                    event group, the selected color and if available all three selected
                    gradient color and also the logical value if variables should be offset.
                    </p>
                    <p>
                    To upload the saved color settings in a new session use the ‘Upload
                    saved color file’-file input like the data upload.
                    </p>
                    "
                    )
                  )
                ),
                right = FALSE
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
          bslib::nav_panel(
             tags$div(
              HTML(paste0("Megaplots","&emsp;","&emsp;")),
                help_text_dropdown_button(
                  id = "megaplot_helptext",
                  text = wellPanel(style ="width: 1000px;",
                  HTML(
                    paste0(
                      "
                      <p> Megaplot </p>
                      <p>
                      In this chapter the main graphical display will be explained.
                      The megaplot  is a huge graphical display used to show individual-level data over time.
                      The megaplots uses horizontal lines to represent individual trajectories and events of any identifier over days (in clinical trial context: study units).
                      </p>
                      <img src='www/Screenshots/23_Megaplots.png' align='center' width='95%'/>
                      <p>
                      The legend on the right side can be used to select and deselect event groups to provide an better
                      overview. Selecting a high number of events can lead to overlaying event lines. In this case
                      it is recommended to focus on a few number of event/event groups or zoom-in on the graphic
                      accordingly.
                      </p>
                      <p>
                      For every single event displayed a hover panel is available with information about the
                      identifier, the event name as well as the start and end time. It is also possible to use the mouse hover
                      for the individual timelines, however these are only visible when hovering near the start or end of
                      the line.
                      </p>
                      <img src='www/Screenshots/25_Megaplots.png' align='center' width='95%'/>
                      <p>
                      In addition to hovering, you can also click on event lines. The complete clicked event are then highlighted in comparison to other events. It is possible to click on and highlight as many events as desired.
                      To undo the effect, double-click on the plot window.
                      Note: The zoom setting will also be reset by double-clicking.
                      </p>

                      <img src='www/Screenshots/27_Megaplots.png' align='center' width='95%'/>
                      <p>
                      There are several ways to zoom-in to the graphic. First, it is possible to use
                      mouse-scrolling to zoom-in.
                      To re-scale only one axis, click and drag near the edge of one of the axes.
                      If the drag mode is set to 'pan' (see chapter Plot options) it is possible to click
                      and drag on the plot.
                      Another zoom possibility is to click and drag on the plot when option 'Zoom' is selected.
                      <p>

                      <img src='www/Screenshots/28_Megaplots.png' align='center' width='95%'/>
                      <p>
                      Third way to zoom is to use the 'plus' or 'minus' button on the modebar to zoom-in or zoom-out.
                      </p>
                      <p>
                      Further options for this megaplots graph are described in the chapter 'Sidebar Options', such
                      as sorting, grouping or line thickness.
                      </p>
                      "
                    )
                  )
                ),
                right = FALSE
              ),
            ),
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
          bslib::nav_panel(
            title = tags$div(HTML(paste0("Event summary","&emsp;","&emsp;")),
              help_text_dropdown_button(
               id = "event_summary_helptext",
               text = wellPanel(style ="width: 1000px;",
                  HTML(
                    paste0(
                      "
                    <p> Event Summary</p>
                    <p>
                        TBD
                    </p>
                    "
                    )
                  )
                ),
                right = FALSE
              )
            ),
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
