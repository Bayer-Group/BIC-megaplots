#' Data upload ui function
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#' @keywords internal

data_upload_ui <- function(id) {
  ns <- NS(id)

  shinydashboard::tabItem(
  'datimport',

  shinydashboard::box(
    width = NULL,
    solidHeader = TRUE,
    collapsible = FALSE,
    background = 'black',
    shiny::fixedRow(

    shiny::column(8,
      img(src = 'www/megaplot-logo.png', width = '500px'),
      HTML("<h5 style = 'color: white;'> depict individual patient journey per day. Pre-specified events on certain days, for example
           drug intake, efficacy, or safety events, are indicated by different symbols. The detection of
           patterns in the data or the temporal connection of study procedures and outcome events is
           supported by artificial intelligence-based functions, such as sorting, grouping, as well as
           graphical features like changing the complexity of the display. You can either use demo data or upload your own data to generate a
           Megaplot. Please take a look at the Package Manual for more information. </h5>"),
      shiny::br(),
      shiny::br(),
      shinyjs::useShinyjs(),
      shiny::fluidRow(
        shiny::column(4,
          shinyWidgets::prettyRadioButtons(
            inputId =ns('selectdata'),
            label = HTML('<p style ="color:white;"> Select data</p>'),
            shape = 'round',
            animation = 'smooth',
            choices =c("Upload data", "Use demo data", "Upload saved data")
          )
        ),
        shiny::conditionalPanel(condition = "input.selectdata == 'Upload data'",
          shiny::column(4,
            shinyWidgets::prettyRadioButtons(
              inputId = ns('impswitch'),
              label = HTML('<p style ="color:white;"> Select file format</p>'),
              shape = 'square',
              animation = 'smooth',
              choices = c(
                '*.RData files (two files)',
                '*.RData file',
                '*.CSV files')
            )
          ),
          shiny::column(4,
            shiny::uiOutput(ns('impdata'))
          ),
          ns = NS(id)
        )
      ),
      shiny::conditionalPanel(condition = "output.fileUploaded_rdata & input.selectdata == 'Upload data'
                              & input.impswitch == '*.RData file'",
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("A_subjectid_rdata"),
              label = HTML('<p style ="color:white;"> A: subjectid</p>'),
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("A_start_time_rdata"),
              label = HTML('<p style ="color:white;"> A: start time</p>'),
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
             shiny::selectInput(
              inputId = ns("A_end_time_rdata"),
              label = HTML('<p style ="color:white;"> A: end time</p>'),
              choices = "",
              selected = NULL
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(4,
             shiny::selectInput(
              inputId = ns("B_subjectid_rdata"),
              label = HTML('<p style ="color:white;"> B: subjectid</p>'),
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("B_event_time_rdata"),
              label = HTML('<p style ="color:white;"> B: event_time</p>'),
              choices = "",
              selected = NULL
            )
          )
        ),
          ns = NS(id)
      ),
      shiny::conditionalPanel(condition = "output.fileUploaded_csv_A & input.selectdata == 'Upload data'
                              & input.impswitch == '*.CSV files'",
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("A_subjectid_csv"),
              label = "A: subjectid",
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("A_start_time_csv"),
              label = "A: start time",
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
             shiny::selectInput(
              inputId = ns("A_end_time_csv"),
              label = "A: end time",
              choices = "",
              selected = NULL
            )
          )
        ),
          ns = NS(id)
      ),
      shiny::conditionalPanel(condition = "output.fileUploaded_csv_B & input.selectdata == 'Upload data'
                              & input.impswitch == '*.CSV files'",
        shiny::fluidRow(
          shiny::column(4,
             shiny::selectInput(
              inputId = ns("B_subjectid_csv"),
              label = "B: subjectid",
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("B_event_time_csv"),
              label = "B: event_time",
              choices = "",
              selected = NULL
            )
          )
        ),
          ns = NS(id)
      ),
      shiny::conditionalPanel(condition = "output.fileUploaded_rdata_A & input.selectdata == 'Upload data'
                              & input.impswitch == '*.RData files (two files)'",
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("A_subjectid_rdata_files"),
              label = "A: subjectid",
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("A_start_time_rdata_files"),
              label = "A: start time",
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
             shiny::selectInput(
              inputId = ns("A_end_time_rdata_files"),
              label = "A: end time",
              choices = "",
              selected = NULL
            )
          )
        ),
          ns = NS(id)
      ),
      shiny::conditionalPanel(condition = "output.fileUploaded_rdata_B & input.selectdata == 'Upload data'
                              & input.impswitch == '*.RData files (two files)'",
        shiny::fluidRow(
          shiny::column(4,
             shiny::selectInput(
              inputId = ns("B_subjectid_rdata_files"),
              label = "B: subjectid",
              choices = "",
              selected = NULL
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              inputId = ns("B_event_time_rdata_files"),
              label = "B: event_time",
              choices = "",
              selected = NULL
            )
          )
        ),
          ns = NS(id)
      ),
      shiny::column(4,
        shiny::conditionalPanel(condition = "input.selectdata == 'Upload saved data'",
          shiny::fileInput(
            inputId = ns('setting_file'),
            label = HTML(
              '<p> Upload previous Session Settings (.rds) </p>'
            ),
            multiple = FALSE,
            accept = '.rds'
          ),
          shiny::helpText(
            'Please upload the data set of the last session which
            was saved via the "Save Session Settings"-button
            in the MegaPlot-tab.'
          ),
          ns = NS(id)
        )
      )
    )
  ),
  shiny::br(),
  shiny::fluidRow(
    event_selection_ui(ns("select_ev.1")),
    # shiny::column(3,
    #   shiny::uiOutput(ns('select.ev1')),
    #   shiny::uiOutput(ns('select_ev_lev1_button')),
    #   shiny::conditionalPanel(condition = "input.select_ev_lev1_button == true",
    #     shiny::uiOutput(ns('select.ev.lev1')),
    #       ns = NS(id)
    #   )
    # ),
    shiny::column(3,
      shiny::uiOutput(ns('select.ev2')),
      shiny::uiOutput(ns('select_ev_lev2_button')),
      shiny::conditionalPanel(condition = "input.select_ev_lev2_button == true",
        shiny::uiOutput(ns('select.ev.lev2')),
          ns = NS(id)
      )
    ),
    shiny::column(3,
      shiny::uiOutput(ns('select.ev3')),
      shiny::uiOutput(ns('select_ev_lev3_button')),
      shiny::conditionalPanel(condition = "input.select_ev_lev3_button == true",
        shiny::uiOutput(ns('select.ev.lev3')),
          ns = NS(id)
      )
    ),
    shiny::column(3,
      shiny::uiOutput(ns('select.ev4')),
      shiny::uiOutput(ns('select_ev_lev4_button')),
      shiny::conditionalPanel(condition = "input.select_ev_lev4_button == true",
        shiny::uiOutput(ns('select.ev.lev4')),
          ns = NS(id)
      )
    )
  ),
  shiny::fluidRow(
    shiny::htmlOutput("err_message"),
    tags$head(
      tags$style(
       "#err_message{color: red;
       font-size: 12px;
       margin-size: 20px;
       }"
      )
    )
  ),
  shiny::br(),
  shiny::br(),
  shiny::fluidRow(
    shiny::column(3,
      shinyWidgets::actionBttn(
        inputId = ns('import.button'),
        label = 'Submit...',
        style = 'gradient',
        # color = 'primary',
        size = 'sm',
        no_outline = FALSE,
        block = TRUE
      ),
      shiny::helpText('Please upload a data set first or use the demo data set to submit.')
    )
  )
  )
)
}



#' Seriation Server Function
#'
#' @param id Shiny Session id
#' @param varSeq reactive shiny object with variables used for the seriation
#' @param multiple_distmeasures logical. Should multiple distance measures be used for the different variables?
#'
#' @return Inputs of the selected distance measure and corresponding parameters
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#' @keywords internal
data_upload_server <- function(input, output, session){

  ns <- session$ns

  # update variables selection if Rdata file is uploaded
  shiny::observeEvent(input$file, {
    #requirement
    shiny::req(input$file)
    if (input$impswitch == '*.RData file') {
      updates_variables_selection_file(file = input$file, session = session)
    }
  })

  # update variables selection if csv (A) file is uploaded
  shiny::observeEvent(input$csvA, {
    #requirement
    shiny::req(input$csvA)
    if (!is.null(input$csvA)) {
      updates_variables_selection_csvA(
        file = input$csvA,
        session = session,
        csvA_sep = input$sep,
        csvA_quote = input$quote,
        csvA_dec = input$dec
      )
    }
  })

  #update variables selection if csv (B) file is uploaded
  shiny::observeEvent(input$csvB, {
    #requirement
    shiny::req(input$csvB)
    if (!is.null(input$csvB)) {
      updates_variables_selection_csvB(
        file = input$csvB,
        session = session,
        csvB_sep = input$sep,
        csvB_quote = input$quote,
        csvB_dec = input$dec
      )
    }
  })

  #update variables selection if RData (A) file is uploaded
  shiny::observeEvent(c(input$rdataA), {
    #requirement
    shiny::req(input$rdataA)
    if (!is.null(input$rdataA)) {
      updates_variables_selection_rdataA(
        file = input$rdataA,
        session = session
      )
    }
  })

  #update variables selection if RData (B) file is uploaded
  shiny::observeEvent(c(input$rdataB), {
    #requirement
    shiny::req(input$rdataB)
    if (!is.null(input$rdataB)) {
      updates_variables_selection_rdataB(
        file = input$rdataB,
        session = session
      )
    }
  })

    ## ui elements in the side bar
  output$impdata <- shiny::renderUI({
    req(input$impswitch)
    if (input$impswitch == '*.RData file') {
      shiny::fileInput(
        inputId = ns('file'),
        label = "Choose RData file with a list including data sets A and B",
        multiple = FALSE,
        accept = '.RData'
      )
    } else if (input$impswitch == '*.CSV files') {
      shiny::tagList(
        shiny::fixedRow(
          shiny::column(6,
            shiny::fileInput(
              inputId = ns('csvA'),
              label = "Choose csv file with data set A (subject information)",
              multiple = TRUE,
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv'
              )
            ),
            shiny::fileInput(
              inputId = ns('csvB'),
              label = "Choose csv file with data set B (event information)",
              multiple = TRUE,
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv'
              )
            )
          ),
          shiny::column(6,
            shiny::radioButtons(
              inputId = ns('sep'),
              label = HTML('<p style ="color:white;> Select separator </p>'),
              inline = TRUE,
              choices = c(
                'Comma' = ',',
                'Semicolon' = ';',
                'Tab' = '\t'
              ),
              selected = ','
            ),
            shiny::radioButtons(
              inputId = ns('quote'),
              label = HTML('<p style ="color:white;> Select quote </p>'),
              inline = TRUE,
              choices = c(
                None = '',
                'Double Quote (")' = '"',
                "Single Quote (')" = "'"
              ),
              selected = '"'
            ),
            shiny::radioButtons(
              inputId = ns('dec'),
              label = HTML('<p style ="color:white;> Select decimal character</p>'),
              inline = TRUE,
              choices = c('Point (.)' = '.',
                          'Comma (,)' = ','),
              selected = '.'
            )
          )
        )
      )
    } else if ( input$impswitch == "*.RData files (two files)"){
      shiny::tagList(
        shiny::fixedRow(
          shiny::fileInput(

            inputId = ns('rdataA'),
            label = "Choose RData file with data set A (subject information)",
            multiple = TRUE,
            accept = c('.RData','.rdata','.Rdata')
          ),
          shiny::fileInput(
            inputId = ns('rdataB'),
            label = "Choose RData file with data set B (event information)",
            multiple = TRUE,
            accept = c('.RData','.rdata','.Rdata')
          )
        )
      )
    }
  })

  #create outputs for conditionalpanel
  output$fileUploaded_rdata <- shiny::reactive({
    return(!is.null(input$file$datapath))
  })
  outputOptions(output, 'fileUploaded_rdata', suspendWhenHidden = FALSE)

  output$fileUploaded_csv_A <- shiny::reactive({
    return(!is.null(input$csvA))
  })
  outputOptions(output, 'fileUploaded_csv_A', suspendWhenHidden = FALSE)

  output$fileUploaded_csv_B <- shiny::reactive({
    return(!is.null(input$csvB))
  })
  outputOptions(output, 'fileUploaded_csv_B', suspendWhenHidden = FALSE)

  output$fileUploaded_rdata_A <- shiny::reactive({
    return(!is.null(input$rdataA))
  })
  outputOptions(output, 'fileUploaded_rdata_A', suspendWhenHidden = FALSE)

  output$fileUploaded_rdata_B <- shiny::reactive({
    return(!is.null(input$rdataB))
  })
  outputOptions(output, 'fileUploaded_rdata_B', suspendWhenHidden = FALSE)



  #### 1event_selection_server <- function(input, output, session, event_number, preprocessed_data, event.info, selectdata) {

  event_selection_1 <- shiny::callModule(event_selection_server, "select_ev.1",1,shiny::reactive({preprocessed_data()}),shiny::reactive({event.info()}),shiny::reactive({input$selectdata}))


  # output$select.ev1 <- shiny::renderUI({
  #    if (length(req(event.info())) >= 1) {
  #   choices <- shiny::req(event.info())
  #   uiElement <- shinyWidgets::pickerInput(
  #     inputId = ns('select.ev1'),
  #     label = "Select event (1)",
  #     choices = choices,
  #     selected = choices[1],
  #     multiple = FALSE,
  #     options = list(
  #       `live-search` = TRUE,
  #       `style` = 'background: btn-primary',
  #       `header` = 'Select item'
  #     )
  #   )
  #  }
  # })
  #
  # output$select.ev.lev1 <- shiny::renderUI({
  #   shiny::req(preprocessed_data())
  #   shiny::req(input$select.ev1)
  #   choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev1]])
  #   choices <- sort(choices[!is.na(choices)])
  #   if (input$selectdata== "Upload saved data") {
  #     if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev1)) {
  #       choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev1
  #     }
  #   }
  #   shinyjqui::orderInput(
  #     inputId = ns("select.ev.lev1"),
  #     label = "Select order of event (1)",
  #     items = choices,
  #     width = "75px"
  #   )
  #
  # })
  #
  # shiny::outputOptions(output, "select.ev.lev1", suspendWhenHidden = FALSE)
  #
  # output$select_ev_lev1_button <- shiny::renderUI({
  #    if (length(req(event.info())) >= 1) {
  #     shiny::req(input$select.ev1)
  #     shiny::checkboxInput(
  #       inputId = ns("select_ev_lev1_button"),
  #       label = "Sort events",
  #       value = FALSE
  #     )
  #    }
  # })

  ## 2
  output$select.ev2 <- shiny::renderUI({
    if (length(req(event.info())) >= 2) {
      choices <- shiny::req(event.info())
      uiElement <- shinyWidgets::pickerInput(
        inputId = ns('select.ev2'),
        label = "Select event (2)",
        choices = choices,
        selected = choices[2],
        multiple = FALSE,
        options = list(
          `live-search` = TRUE,
          `style` = 'background: btn-primary',
          `header` = 'Select item'
        )
      )
    }
  })
   output$select.ev.lev2 <- shiny::renderUI({
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev2)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev2]])
    choices <- sort(choices[!is.na(choices)])
    if (input$selectdata== "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev2)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev2
      }
    }
    shinyjqui::orderInput(
      inputId = ns("select.ev.lev2"),
      label = "Select order of event (2)",
      items = choices,
      width = "75px"
    )
  })

  shiny::outputOptions(output, "select.ev.lev2", suspendWhenHidden = FALSE)

  output$select_ev_lev2_button <- shiny::renderUI({
    if (length(req(event.info())) >= 2) {
      shiny::req(input$select.ev2)
      shiny::checkboxInput(
        inputId = ns("select_ev_lev2_button"),
        label = "Sort events",
        value = FALSE
      )
    }
  })
  output$select_ev_lev2_button <- shiny::renderUI({
    if (length(req(event.info())) >= 2) {
      shiny::req(input$select.ev2)
      shiny::checkboxInput(
        inputId = ns("select_ev_lev2_button"),
        label = "Sort events",
        value = FALSE
      )
    }
  })




  output$select.ev.lev3 <- shiny::renderUI({
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev3)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev3]])
    choices <- sort(choices[!is.na(choices)])
    if (input$selectdata== "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev3)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev3
      }
    }
    shinyjqui::orderInput(
      inputId = ns("select.ev.lev3"),
      label = "Select order of event (3)",
      items = choices,
      width = "75px"
    )
  })

  shiny::outputOptions(output, "select.ev.lev3", suspendWhenHidden = FALSE)

  output$select_ev_lev3_button <- shiny::renderUI({
    if (length(req(event.info())) >= 3) {
      shiny::req(input$select.ev3)
      shiny::checkboxInput(
        inputId = ns("select_ev_lev3_button"),
         label = "Sort events",
         value = FALSE
      )
    }
  })

  output$select.ev.lev4 <- shiny::renderUI({
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev4)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev4]])
    choices <- sort(choices[!is.na(choices)])
    if (input$selectdata== "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev4)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev4
      }
    }
    shinyjqui::orderInput(
      inputId = ns("select.ev.lev4"),
      label = "Select order of event (4)",
      items = choices,
      width = "75px"
    )
  })

  shiny::outputOptions(output, "select.ev.lev4", suspendWhenHidden = FALSE)

  output$select_ev_lev4_button <- shiny::renderUI({
    if (length(req(event.info())) >= 4) {
      shiny::req(input$select.ev4)
      shiny::checkboxInput(
        inputId =ns("select_ev_lev4_button"),
       label = "Sort events",
       value = FALSE)
    }
  })


 output$select.ev3 <- shiny::renderUI({
    if (length(req(event.info())) >= 3) {
      choices <- shiny::req(event.info())
      uiElement <- shinyWidgets::pickerInput(
        inputId = ns('select.ev3'),
        label = "Select event (3)",
        choices = choices,
        selected = choices[3],
        multiple = FALSE,
        options = list(
          `live-search` = TRUE,
          `style` = 'background: btn-primary',
          `header` = 'Select item'
        )
      )
    }
  })

  output$select.ev4 <- shiny::renderUI({
    if (length(req(event.info())) >= 4) {
      choices <- shiny::req(event.info())
      uiElement <- shinyWidgets::pickerInput(
        inputId = ns('select.ev4'),
        label = "Select event (4)",
        choices = choices,
        selected = choices[4],
        multiple = FALSE,
        options = list(
          `live-search` = TRUE,
          `style` = 'background: btn-primary',
          `header` = 'Select item'
        )
      )
    }
  })

  # event main selections
  event.info <- shiny::reactive({
    if (!is.null(preprocessed_data()$megaplot_data)) {
      B <- preprocessed_data()$megaplot_data$B
      char_B <- names(which(sapply(B, is.character)))
      char_B
    }
  })

  preprocessed_data <-shiny::reactive({

    #read & preprocess data in desired format
    preprocessed_df <- preprocess_data_frame(
      selectdata = input$selectdata,
      impswitch = input$impswitch,
      file = input$file,
      csvA = input$csvA,
      csvB = input$csvB,
      rdataA = input$rdataA,
      rdataB = input$rdataB,
      A_subjectid_rdata = input$A_subjectid_rdata,
      A_start_time_rdata = input$A_start_time_rdata,
      A_end_time_rdata = input$A_end_time_rdata,
      B_subjectid_rdata = input$B_subjectid_rdata,
      B_event_time_rdata = input$B_event_time_rdata,
      A_subjectid_csv= input$A_subjectid_csv,
      A_start_time_csv = input$A_start_time_csv,
      A_end_time_csv = input$A_end_time_csv,
      B_subjectid_csv = input$B_subjectid_csv,
      B_event_time_csv = input$B_event_time_csv,
      A_subjectid_rdata_files = input$A_subjectid_rdata_files,
      A_start_time_rdata_files = input$A_start_time_rdata_files,
      A_end_time_rdata_files = input$A_end_time_rdata_files,
      B_subjectid_rdata_files = input$B_subjectid_rdata_files,
      B_event_time_rdata_files = input$B_event_time_rdata_files,
      csv_sep = input$sep,
      csv_quote = input$quote,
      csv_dec = input$dec,
      setting_file = input$setting_file
    )

    # remove optional variables with only one value/category
    if (!is.null(preprocessed_df$megaplot_data)) {
      #get removable variables
      remove_variables <- names(which(apply(
        preprocessed_df$megaplot_data$A %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time)),
        2,
        function(x) {length(unique(x)) == 1})))
      #deselect removable variables
      preprocessed_df$megaplot_data$A <- preprocessed_df$megaplot_data$A %>%
        dplyr::select(-dplyr::all_of(remove_variables))


      #transform Missings to character "NA"
      for(i in 1:dim(preprocessed_df$megaplot_data$A)[2]) {
        if (is.numeric(preprocessed_df$megaplot_data$A[,i]) | is.numeric(preprocessed_df$megaplot_data$A[,i])) {
        } else if (
          inherits(preprocessed_df$megaplot_data$A[,i], 'Date')
          ){
          preprocessed_df$megaplot_data$A[,i] <- as.numeric(preprocessed_df$megaplot_data$A[,i])
        } else {
          preprocessed_df$megaplot_data$A[,i] <- as.character(preprocessed_df$megaplot_data$A[,i])
          preprocessed_df$megaplot_data$A[,i][is.na(preprocessed_df$megaplot_data$A[,i])] <- "NA"
          preprocessed_df$megaplot_data$A[,i][preprocessed_df$megaplot_data$A[,i] == ""] <- "NA"
        }
      }
    }
    preprocessed_df
  })

   return(
    list(
      selectdata = shiny::reactive({input$selectdata}),
      impswitch = shiny::reactive({input$impswitch}),
      import.button = shiny::reactive({input$import.button}),
      select.ev1 = shiny::reactive({event_selection_1$select.ev()}),
      select.ev2 = shiny::reactive({input$select.ev2}),
      select.ev3 = shiny::reactive({input$select.ev3}),
      select.ev4 = shiny::reactive({input$select.ev4}),
      select.ev.lev1 = shiny::reactive({event_selection_1$select.ev.lev()}),
      select.ev.lev2 = shiny::reactive({input$select.ev.lev2}),
      select.ev.lev3 = shiny::reactive({input$select.ev.lev3}),
      select.ev.lev4 = shiny::reactive({input$select.ev.lev4}),
      preprocess_data = shiny::reactive({preprocessed_data()})
    )
  )
}
