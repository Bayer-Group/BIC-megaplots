#' mod_colour_palette UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom colourpicker colourInput
#' @keywords internal

mod_colour_palette_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(12,
           textOutput(ns("header"))),
    tags$head(
      tags$style(paste0(
        "#", ns('header'), "{color: white;
            font-size: 15px;
            }"
      )),
      tags$style(
        "#inline label{ display: table-cell; text-align: left; vertical-align: middle; width: 80%; padding-left: 5%; font-size: 15px;}
          .form-group { display: table-row}"
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel1'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col1"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[1],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel2'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col2"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[2],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel3'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col3"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[3],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel4'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col4"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[4],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel5'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col5"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[5],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel6'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col6"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[6],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel7'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col7"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[7],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    ),
    tags$div(
      id = "inline",
      shiny::conditionalPanel(
        condition = paste0('output[\'', ns('cond_panel8'), "\'] == true"),
        colourpicker::colourInput(
          inputId = ns("col8"),
          label = "",
          value = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                unlist(colChoice) != "TRUE"])[8],
          palette = "limited",
          allowedCols = as.vector(unlist(colChoice)[unlist(colChoice) != "FALSE" &
                                                      unlist(colChoice) != "TRUE"]),
          showColour = "background"
        )
      )
    )
  )
}


#' mod_colour_palette Server function
#'
#' @param id 
#' @param event 
#' @param level 
#' @param colors 
#' 
#' @importFrom colourpicker updateColourInput
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @noRd
#' @keywords internal
mod_colour_palette_server <- function(id, event, level, colors) {
  moduleServer(id, function(input,
                            output,
                            session,
                            ev = event(),
                            lev = level(),
                            col = colors()) {
    ns <- session$ns
    observeEvent(col, {
      for (i in 1:length(lev)) {
        colourpicker::updateColourInput(
          session = session,
          inputId = paste0("col", i),
          label = lev[i],
          value = col[i]
        )
      }
    })
    
    output$header <- renderText({
      ev
    })
    
    # 1
    cond_panel_1 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel1 <- shiny::reactive({
      cond_panel_1$val
    })
    observeEvent(lev, {
      if (!is.na(lev[1])) {
        cond_panel_1$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel1", suspendWhenHidden = FALSE)
    # 2
    cond_panel_2 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel2 <- shiny::reactive({
      cond_panel_2$val
    })
    observeEvent(lev, {
      if (!is.na(lev[2])) {
        cond_panel_2$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel2", suspendWhenHidden = FALSE)
    # 3
    cond_panel_3 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel3 <- shiny::reactive({
      cond_panel_3$val
    })
    observeEvent(lev, {
      if (!is.na(lev[3])) {
        cond_panel_3$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel3", suspendWhenHidden = FALSE)
    # 4
    cond_panel_4 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel4 <- shiny::reactive({
      cond_panel_4$val
    })
    observeEvent(lev, {
      if (!is.na(lev[4])) {
        cond_panel_4$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel4", suspendWhenHidden = FALSE)
    
    # 5
    cond_panel_5 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel5 <- shiny::reactive({
      cond_panel_5$val
    })
    observeEvent(lev, {
      if (!is.na(lev[5])) {
        cond_panel_5$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel5", suspendWhenHidden = FALSE)
    # 6
    cond_panel_6 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel6 <- shiny::reactive({
      cond_panel_6$val
    })
    observeEvent(lev, {
      if (!is.na(lev[6])) {
        cond_panel_6$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel6", suspendWhenHidden = FALSE)
    # 7
    cond_panel_7 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel7 <- shiny::reactive({
      cond_panel_7$val
    })
    observeEvent(lev, {
      if (!is.na(lev[7])) {
        cond_panel_7$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel7", suspendWhenHidden = FALSE)
    # 8
    cond_panel_8 <- shiny::reactiveValues(val = FALSE)
    output$cond_panel8 <- shiny::reactive({
      cond_panel_8$val
    })
    observeEvent(lev, {
      if (!is.na(lev[8])) {
        cond_panel_8$val <- TRUE
      }
    })
    outputOptions(output, "cond_panel8", suspendWhenHidden = FALSE)
    
    out <-
      shiny::reactiveValues(
        val = c(
          input$col1,
          input$col2,
          input$col3,
          input$col4,
          input$col5,
          input$col6,
          input$col7,
          input$col8
        )
      )
    
    observeEvent(input$col1, {
      out$val[1] <- input$col1
    })
    observeEvent(input$col2, {
      out$val[2] <- input$col2
    })
    observeEvent(input$col3, {
      out$val[3] <- input$col3
    })
    observeEvent(input$col4, {
      out$val[4] <- input$col4
    })
    observeEvent(input$col5, {
      out$val[5] <- input$col5
    })
    observeEvent(input$col6, {
      out$val[6] <- input$col6
    })
    observeEvent(input$col7, {
      out$val[7] <- input$col7
    })
    observeEvent(input$col8, {
      out$val[8] <- input$col8
    })
    
    
    return(list(colors = shiny::reactive({
      out$val
    })))
  })
}