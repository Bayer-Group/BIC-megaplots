#' Displayed Subjects (Sidebar Option Panel) Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

displayed_subjects_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("random"),
      label = paste("Number of displayed subjects"),
      min = 1,
      max = 999,
      value = 1
    ),
    shiny::conditionalPanel(condition = "output.check_subset == false",
      shiny::radioButtons(
        inputId = ns('selection_button'),
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
            inputId = ns("startsubj"),
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
            inputId = ns("seedset"),
            label = "Select a seed for the random subject selection",
            min = 1,
            max = 10000,
            value = 2006,
            step = 1
          )
        )
      ),
      shinyWidgets::pickerInput(
        inputId = ns("specific_ids"),
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
        inputId = ns('subset.button'),
        label = "Update subject selection!",
        style = "gradient",
        color = "primary",
        size = 'xs',
        no_outline = FALSE,
        icon = icon("refresh")
      ),
      ns = NS(id)
    ))
}


#' Displayed Subjects (Sidebar Option Panel) Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal

displayed_subjects_server <- function(input, output, session, preprocess_data) {

  ns <- session$ns

  ####... random ####
    shiny::observe({

    nmax <- length(unique(preprocess_data()$megaplot_data$A$megaplots_selected_subjectid))
    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   nmax <- preprocess_data()$megaplot_data$saved$random
    # }

    num_sub <- length(unique(preprocess_data()$megaplot_data$A$megaplots_selected_subjectid))
#
#     if (uploaded_files$selectdata()== "Upload saved data") {
#       shiny::updateSliderInput(
#         session,
#         inputId = 'random',
#         label = paste("Number of displayed subjects"),
#         min = 1,
#         value = preprocess_data()$megaplot_data$saved$random,
#         max = num_sub,
#         step = 1
#       )
#     } else {

      shiny::updateSliderInput(
        session,
        inputId = 'random',
        label = paste("Number of displayed subjects"),
        min = 1,
        value = num_sub,
        max = num_sub,
        step = 1
      )
    # }
  })

 subset.flag <- shiny::reactiveValues(val = FALSE)

  output$check_subset <- shiny::reactive({
    subset.flag$val
  })

  shiny::outputOptions(output, "check_subset", suspendWhenHidden = FALSE)

  observe({subset.flag$val})
  shiny::observeEvent(c(input$random), {
    shiny::req(preprocess_data())
    nmax <- length(unique(preprocess_data()$megaplot_data$A$megaplots_selected_subjectid))
    if (!is.null(nmax)) {
      subset.flag$val <-  nmax == input$random
    }
  })

  observeEvent(preprocess_data(),{
    shinyWidgets::updatePickerInput(
      session,
      inputId = "specific_ids",
      selected = NULL,
      choices = preprocess_data()$megaplot_data$A$megaplots_selected_subjectid,
    )
  })

  shiny::observeEvent(c(input$random, preprocess_data()), {
    shiny::req(input$random)

    nmax <- length(unique(preprocess_data()$megaplot_data$A$megaplots_selected_subjectid))
    # if (uploaded_files$selectdata()== "Upload saved data") {
    #   nmax <- uploaded_files$preprocess_data()$saved$random
    # }

    shiny::updateNumericInput(
      session,
      inputId = "startsubj",
      max = nmax - input$random + 1,
      value = #ifelse(
        #uploaded_files$selectdata()== "Upload saved data",
        #preprocess_data()$megaplot_data$saved$startsubj,
        1
      # )
    )
  })

  shiny::observeEvent(input$selection_button, {
    shiny::updateTabsetPanel(
      inputId = "Change_input_for_deterministic_or_random",
      selected = input$selection_button
    )
  })

  return(list(
    random = shiny::reactive({input$random}),
    selection_button = shiny::reactive({input$selection_button}),
    startsubj = shiny::reactive({input$startsubj}),
    seedset = shiny::reactive({input$seedset}),
    specific_ids = shiny::reactive({input$specific_ids}),
    subset.button = shiny::reactive({input$subset.button})
  ))
}
