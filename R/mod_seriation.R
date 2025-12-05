#' Seriation UI Function
#'
#' Function that makes the UI for the seriation, called by the module server function
#'
#' @param var Variable name used for seriation
#' @param name Name used for the drop-down menu
#'
#' @return No return
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinyWidgets
#' @importFrom stringr str_wrap
#'
#' @noRd
#' @keywords internal
make_parameter_ui <- function(id, var, name) {
  ns <- NS(id)
  distmeasure_name <- paste0(var, "_distmeasure")
  tagList(
    shinyWidgets::pickerInput(
      inputId = ns(distmeasure_name),
      label = paste0('Distance Measure ', name),
      choices = c(
        "OM",
        "OMloc",
        "OMslen",
        "OMspell",
        "OMstran",
        "CHI2",
        "EUCLID",
        "LCS",
        "LCP",
        "RLCP",
        "HAM",
        "DHD"
      ),
      selected = 'OM',
      multiple = FALSE,
      options = list(`live-search` = TRUE,
                     `header` = 'Select item'),
    ),
    # shinydashboard::menuItem(
    #   text = p("Parameters", style = "white-space: pre-wrap; display: inline-block;"),
    #   icon = icon('cogs', lib = 'font-awesome'),
    #   tabName = 'parameters',
    bslib::accordion_panel(
      "Parameters",
      conditionalPanel(
        condition = paste0(
          "['OM', 'OMloc', 'OMslen', 'OMspell','OMstran', 'HAM',
                           'TWED'].indexOf(input['", ns(distmeasure_name),"']) !== -1"
        ),
        shinyWidgets::pickerInput(
          inputId = ns(paste0(var, "_", "sm")),
          label = "Substitution Cost",
          choices = sort(c(
            "CONSTANT", "INDELS", "INDELSLOG", "TRATE", "ORDINAL"
          )),
          selected = 'CONSTANT',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        )
      ),


      conditionalPanel(
        condition = paste0("['DHD'].indexOf(input['", ns(distmeasure_name), "']) !== -1"),
        shinyWidgets::pickerInput(
          inputId = ns(paste0(var, "_", "smDHD")),
          label = "Substitution Cost",
          choices = sort(c("INDELS", "INDELSLOG", "TRATE")),
          selected = 'INDELS',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        )
      ),
      conditionalPanel(
        condition = paste0(
          "['OM', 'OMslen', 'OMspell',
                'OMstran'].indexOf(input['",  ns(distmeasure_name), "']) !== -1"
        ),
        shinyWidgets::pickerInput(
          inputId = ns(paste0(var, "_", "indel")),
          label = "Insertion/Deletion Cost",
          choices = sort(c("auto", "numeric value")),
          selected = "auto",
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        ),
        conditionalPanel(
          condition = paste0("input['", ns(paste0(var, "_indel")), "'] == 'numeric value'"),
          numericInput(
            inputId =  ns(paste0(var, "_", "indel_numeric")),
            label = "Insertion/Deletion Cost (double)",
            value = 1,
            min = 0,
            width = 150,
          )
        ),
      ),
      conditionalPanel(
        condition = paste0(
          "['OM', 'OMloc', 'OMslen', 'OMspell',
                'OMstran', 'DHD', 'LCS', 'LCP', 'RLCP'
                ].indexOf(input['", ns(distmeasure_name), "']) !== -1"
        ),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "norm")),
          label = "Normalization",
          choices = sort(
            c("auto", "none", "maxlength", "gmean",
              "maxdist", "YujianBo")
          ),
          selected = 'auto',
          multiple = FALSE,
          # width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
       )
      ),
      conditionalPanel(
        condition = paste0(
          "['CHI2', 'EUCLID'].indexOf(input['", ns(distmeasure_name), "']) !== -1"
        ),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "norm2")),
          label = "Normalization",
          choices = sort(c("auto", "none")),
          selected = 'auto',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        )
      ),
      conditionalPanel(
        condition = paste0(
          "['OMloc','OMspell'].indexOf(input['", ns(distmeasure_name),"']) !== -1"
        ),
        numericInput(
          inputId =  ns(paste0(var, "_", "expcost")),
          label = "Cost of spell length transformation",
          value = 0.5,
          min = 0,
          step = 0.1,
          width = 150
        )
      ),
      conditionalPanel(
        condition = paste0("'OMloc' == input['", ns(distmeasure_name), "']"),
        numericInput(
          inputId =  ns(paste0(var, "_", "context")),
          label = "Local Insertion Cost",
          value = 0,
          min = 0,
          width = 150
        )
      ),
      conditionalPanel(
        condition = paste0("'OMslen' == input['", ns(distmeasure_name), "']"),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "link")),
          label = "Substituion Costs Function",
          choices = sort(c("mean", "gmean")),
          selected = 'mean',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        )
      ),
      conditionalPanel(
        condition = paste0("'OMslen' == input['", ns(distmeasure_name), "']"),
        numericInput(
          inputId =  ns(paste0(var, "_", "h_OMslen")),
          label = "Exponential weight of spell length",
          width = 150,
          value = 0.5,
          min = 0
        )
      ),
      conditionalPanel(
        condition = paste0("'OMspell' == input['", ns(distmeasure_name), "']"),
        numericInput(
          inputId =  ns(paste0(var, "_", "tpow")),
          label = "Exponential weight of spell length",
          value = 1,
          width = 150
        )
      ),
      conditionalPanel(
        condition = paste0("'OMstran' == input['", ns(distmeasure_name), "']"),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "transindel")),
          label = "Transition Indel Cost Method",
          choices = sort(c("constant", "subcost", "prob")),
          selected = 'constant',
          width = 150,
          multiple = FALSE,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        ),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "previous")),
          label = "Account for the transition from the previous state",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'FALSE',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        ),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "add.column")),
          label = "Duplicate the last column",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'TRUE',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        )
      ),
      conditionalPanel(
        condition = paste0("'OMstran' == input['", ns(distmeasure_name), "']"),
        numericInput(
          inputId =  ns(paste0(var, "_", "otto")),
          label = "Origin-Transition Trade-Off Weight",
          value = 0.5,
          min = 0,
          step = 0.1,
          width = 150,
          max = 1
        )
      ),
      conditionalPanel(
        condition = paste0("['CHI2', 'EUCLID'].indexOf(input['", ns(distmeasure_name),
                           "']) !== -1"
        ),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "overlap")),
          label = "Intervals overlapping",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'FALSE',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        ),
        conditionalPanel(
          condition = paste0("'TRUE' == input['",ns(paste0(var, "_", "overlap")), "']"),
          shiny::span(shiny::HTML(
            gsub(
              '\n',
              '<br/>',
              stringr::str_wrap(
                'The interval length must be an even number',
                width = 30
              )
            )
          ), style = 'color:#e6710b'),
          shiny::br()
        ),
        numericInput(
          inputId =  ns(paste0(var, "_", "step")),
          label = "Interval Length",
          value = 1,
          width = 150,
          min = 1,
          step = 1
        )

      ),
      conditionalPanel(
        condition = paste0("'CHI2' == input['", ns(distmeasure_name), "']"),
        shinyWidgets::pickerInput(
          inputId =  ns(paste0(var, "_", "weighted")),
          label = "Distribution of states as weights",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'TRUE',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE,
                         `header` = 'Select item'),
        )
      ),

      shinyWidgets::pickerInput(
        inputId =  ns(paste0(var, "_", 'methMissing')),
        label = 'Missing method',
        choices = c("new state", "last observed state"),
        selected = "new state",
        multiple = FALSE,
        options = list(`live-search` = TRUE,
                       `header` = 'Select item')
      )
    ),
    conditionalPanel(
      condition = paste0(
        "['HAM', 'DHD'].indexOf(input['", ns(distmeasure_name),"']) !== -1"
      ),
      shiny::span(shiny::HTML(
        gsub(
          '\n',
          '<br/>',
          stringr::str_wrap(
            'This distance measure only works for sequences of the same length!',
            width = 30
          )
        )
      ), style = 'color:#e6250b'),
      shiny::br()
    )
  )
}




#' Seriation UI Function
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#' @keywords internal

seriation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("parameters"))
  )
}



#' Seriation Server Function
#'
#' @param id Shiny Session id
#' @param varSeq reactive shiny object with variables used for the seriation
#' @param multiple_distmeasures logical. Should multiple distance measures be used for the different variables?
#' @param select_data character of data upload method ("Use demo data"/"Data upload")
#' @param seq.button reactive actionButton value for update on sequencing
#' @param setting_file list with saved settings information
#'
#' @return Inputs of the selected distance measure and corresponding parameters
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#' @keywords internal

seriation_server <- function(
    id,
    varSeq,
    multiple_distmeasures,
    megaplot_filtered_data,
    seq.button
){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      # make the inputs for the parameters:
      if ((length(varSeq()) > 1) & multiple_distmeasures()) {
        output$parameters <- renderUI({
          lapply(varSeq(), function(x)
            make_parameter_ui(ns(id), var = x, name = x))
        })
      } else {
        output$parameters <- renderUI({
          make_parameter_ui(ns(id), var = varSeq()[[1]], name = "")
        })
      }
    })

    # save the new inputs and return them to the server-function
    input_seriation <- reactive({
      if ((length(varSeq()) > 1) & multiple_distmeasures()) {
        list("distmeasure" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_distmeasure")]])),
             "norm" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_norm")]])),
             "norm2" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_norm2")]])),
             "indel" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_indel")]])),
             "indel_numeric" = as.numeric(unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_indel_numeric")]]))),
             "sm" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_sm")]])),
             "smDHD" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_smDHD")]])),
             "expcost" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_expcost")]])),
             "context" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_context")]])),
             "link" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_link")]])),
             "h_OMslen" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_h_OMslen")]])),
             "tpow" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_distmeasure")]])),
             "transindel" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_transindel")]])),
             "otto" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_otto")]])),
             "previous" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_previous")]])),
             "add.column" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_add.column")]])),
             "step" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_step")]])),
             "overlap" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_overlap")]])),
             "weighted" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_weighted")]])),
             "nu" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_nu")]])),
             "methMissing" = unlist(lapply(varSeq(), function(x) input[[paste0("parametersModule-",x, "_methMissing")]]))
        )
      } else {
        list("distmeasure" = input[[paste0("parametersModule-",varSeq()[[1]], "_distmeasure")]],
             "sm" = input[[paste0("parametersModule-",varSeq()[[1]], "_sm")]],
             "smDHD" = input[[paste0("parametersModule-",varSeq()[[1]], "_smDHD")]],
             "norm" = input[[paste0("parametersModule-",varSeq()[[1]], "_norm")]],
             "norm2" = input[[paste0("parametersModule-",varSeq()[[1]], "_norm2")]],
             "indel" = input[[paste0("parametersModule-",varSeq()[[1]], "_indel")]],
             "indel_numeric" = as.numeric(input[[paste0("parametersModule-",varSeq()[[1]], "_indel_numeric")]]),
             "expcost" = input[[paste0("parametersModule-",varSeq()[[1]], "_expcost")]],
             "context" = input[[paste0("parametersModule-",varSeq()[[1]], "_context")]],
             "link" = input[[paste0("parametersModule-",varSeq()[[1]], "_link")]],
             "h_OMslen" = input[[paste0("parametersModule-",varSeq()[[1]], "_h_OMslen")]],
             "transindel" = input[[paste0("parametersModule-",varSeq()[[1]], "_transindel")]],
             "tpow" = input[[paste0("parametersModule-",varSeq()[[1]], "_tpow")]],
             "otto" = input[[paste0("parametersModule-",varSeq()[[1]], "_otto")]],
             "previous" = input[[paste0("parametersModule-",varSeq()[[1]], "_previous")]],
             "add.column" = input[[paste0("parametersModule-",varSeq()[[1]], "_add.column")]],
             "overlap" = input[[paste0("parametersModule-",varSeq()[[1]], "_overlap")]],
             "step" = input[[paste0("parametersModule-",varSeq()[[1]], "_step")]],
             "weighted" = input[[paste0("parametersModule-",varSeq()[[1]], "_weighted")]],
             "methMissing" = input[[paste0("parametersModule-",varSeq()[[1]], "_methMissing")]]
        )
      }
    })

    return(list(
      input_seriation = shiny::reactive({input_seriation()})
    )
    )
  })
}
