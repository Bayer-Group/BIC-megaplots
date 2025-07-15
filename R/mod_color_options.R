#' Color Options - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

color_options_ui <- function(id) {

  ns <- NS(id)

shiny::tagList(
  shinyWidgets::pickerInput(
    inputId = ns('select.col'),
    label = "Select color theme",
    width = 'fit',
    choices = list('grey (app version)', 'white (print version)'),
    multiple = FALSE,
    selected = 'grey (app version)',
    choicesOpt = list(`icon` = rep('glyphicon-blackboard', 2))
  ),
  tags$style(HTML(".dropdown-item {color: #222d32!important;")),
  #### Module call: mod_colour_palette
  mod_colour_palette_ui(ns("color_palette1")),
    shiny::conditionalPanel(condition = "output.load_sel_pal1 == true",
      shiny::tags$br(),
      shiny::tags$hr(),
      ns = NS(id)
    ),
    # shinyWidgets::pickerInput(
    #   inputId = ns(""plot_symbol1")),
    #   label = "Choose Symbol (1)",
    #   choices = c(0,1,2,3),
    #   choicesOpt = list(
    #     content = sprintf("<i class='fa-solid fa-square'></i>")
    #   )
    # ),
    mod_colour_palette_ui(ns("color_palette2")),
    shiny::conditionalPanel(condition = "output.load_sel_pal2 == true",
      shiny::tags$br(),
      shiny::tags$hr(),
      ns = NS(id)
    ),
    mod_colour_palette_ui(ns("color_palette3")),
    shiny::conditionalPanel(condition = "output.load_sel_pal3 == true",
      shiny::tags$br(),
      shiny::tags$hr(),
      ns = NS(id)
    ),
    mod_colour_palette_ui(ns("color_palette4")),
    shiny::conditionalPanel(condition = "output.load_sel_pal4 == true",
      shiny::tags$br(),
      shiny::tags$hr(),
      ns = NS(id)
    )
  )
}

#' Color Options Module - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param import.button reactive actionButton information
#' @param select.ev1 character value of selected event 1
#' @param select.ev2 character value of selected event 2
#' @param select.ev3 character value of selected event 3
#' @param select.ev4 character value of selected event 4
#' @param select.ev.lev1 character vector of event levels from event 1
#' @param select.ev.lev2 character vector of event levels from event 2
#' @param select.ev.lev3 character vector of event levels from event 3
#' @param select.ev.lev4 character vector of event levels from event 4
#' @param setting_file list with saved settings information
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal


color_options_server <- function(
    input,
    output,
    session,
    import.button,
    select.ev1,
    select.ev2,
    select.ev3,
    select.ev4,
    select.ev.lev1,
    select.ev.lev2,
    select.ev.lev3,
    select.ev.lev4,
    setting_file
  ) {

  ns <- session$ns


  loaded_sel_pal1 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal1, import.button()), {
    loaded_sel_pal1$dat <- !is.null(select.ev1())
  })

  output$load_sel_pal1 <- shiny::reactive(loaded_sel_pal1$dat)

  shiny::outputOptions(output, "load_sel_pal1", suspendWhenHidden = FALSE)

  loaded_sel_pal2 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal2, import.button()), {
    loaded_sel_pal2$dat <- !is.null(select.ev2())
  })

  output$load_sel_pal2 <- shiny::reactive(loaded_sel_pal2$dat)

  shiny::outputOptions(output, "load_sel_pal2", suspendWhenHidden = FALSE)

  loaded_sel_pal3 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal3, import.button()), {
    loaded_sel_pal3$dat <- !is.null(select.ev3())
  })

  output$load_sel_pal3 <- shiny::reactive(loaded_sel_pal3$dat)
  shiny::outputOptions(output, "load_sel_pal3", suspendWhenHidden = FALSE)

  loaded_sel_pal4 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal4, import.button()), {
    loaded_sel_pal4$dat <- !is.null(select.ev4())
  })

  output$load_sel_pal4 <- shiny::reactive(loaded_sel_pal4$dat)
  shiny::outputOptions(output, "load_sel_pal4", suspendWhenHidden = FALSE)

  #### COLOR MODULES ####
  # color module 1
  color_pal1 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(import.button()), {
    shiny::req(import.button())
    if (!is.null(select.ev.lev1())) {
      # if (selectdata()== "Use demo data") {
      # custom_colour <- mod_colour_palette_server(
      #     "color_palette1",
      #     event = shiny::reactive({
      #       select.ev1()
      #     }),
      #     level = shiny::reactive({
      #       select.ev.lev1()
      #     }),
      #     colors = shiny::reactive({c("seagreen1", "#ffff99", "#ff7f00", "#5CACEE", "#FDBF6F", "#1F78B4", "#6A3D9A", "#FF7F00") })
      #   )
      #
      #   shiny::observe({
      #     color_pal1$val <- custom_colour$colors()
      #   })
      # } else {
        custom_colour <- mod_colour_palette_server(
          "color_palette1",
          event = shiny::reactive({
            select.ev1()
          }),
          level = shiny::reactive({
            select.ev.lev1()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 1"]]$col
          })
        )
        shiny::observe({
          color_pal1$val <- custom_colour$colors()
        })
      # }
    }
  })

  # color module 2
  color_pal2 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(import.button()), {
    if (!is.null(select.ev.lev2())) {
        custom_colour2 <- mod_colour_palette_server(
          "color_palette2",
          event = shiny::reactive({
            select.ev2()
          }),
          level = shiny::reactive({
            select.ev.lev2()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 2"]]$col
          })
        )
        observe({
          color_pal2$val <- custom_colour2$colors()
        })
      # }

    }
  })

  # color module 3
  color_pal3 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(import.button()), {
    if (!is.null(select.ev.lev3())) {
        custom_colour3 <- mod_colour_palette_server(
          "color_palette3",
          event = shiny::reactive({
            select.ev3()
          }),
          level = shiny::reactive({
            select.ev.lev3()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 3"]]$col
          })
        )
        observe({
          color_pal3$val <- custom_colour3$colors()
        })
      # }

    }
  })


  # color module 4
  color_pal4 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(import.button()), {
    if (!is.null(select.ev.lev4())) {
        custom_colour4 <- mod_colour_palette_server(
          "color_palette4",
          event = shiny::reactive({
            select.ev4()
          }),
          level = shiny::reactive({
            select.ev.lev4()
          }),
          colors = shiny::reactive({
            colChoice[["color palette 4"]]$col
          })
        )
        observe({
          color_pal4$val <- custom_colour4$colors()
        })
      # }

    } else {
      color_pal4$val <- NULL
    }
  })
#

  shiny::observeEvent(setting_file(), {
    if (!is.null(setting_file())) {
      saved_file <- setting_file()$datapath
      #saved_file <- readRDS(setting_file()$datapath)
      if (is.list(saved_file)) {

        custom_colour <- mod_colour_palette_server(
          "color_palette1",
          event = shiny::reactive({
            select.ev1()
          }),
          level = shiny::reactive({
            select.ev.lev1()
          }),
          colors = shiny::reactive({saved_file$color_pal1})
        )

        shiny::observe({
          color_pal1$val <- custom_colour$colors()
        })

         custom_colour2 <- mod_colour_palette_server(
          "color_palette2",
          event = shiny::reactive({
            select.ev2()
          }),
          level = shiny::reactive({
            select.ev.lev2()
          }),
          colors = shiny::reactive({saved_file$color_pal2})
        )
        observe({
          color_pal2$val <- custom_colour2$colors()
        })

         custom_colour3 <- mod_colour_palette_server(
          "color_palette3",
          event = shiny::reactive({
            select.ev3()
          }),
          level = shiny::reactive({
            select.ev.lev3()
          }),
          colors = shiny::reactive({saved_file$color_pal3})
        )
        observe({
          color_pal3$val <- custom_colour3$colors()
        })

         custom_colour4 <- mod_colour_palette_server(
          "color_palette4",
          event = shiny::reactive({
            select.ev4()
          }),
          level = shiny::reactive({
            select.ev.lev4()
          }),
          colors = shiny::reactive({saved_file$color_pal4})
        )
        observe({
          color_pal4$val <- custom_colour4$colors()
        })
      }
    }
  })

  color_infos <- shiny::reactive({
    param <- list(
      color_pal1 = color_pal1$val,
      color_pal2 = color_pal2$val,
      color_pal3 = color_pal3$val,
      color_pal4 = color_pal4$val,
      select.col = input$select.col
    )
    param
  })

  return(
    list(
      color_infos = shiny::reactive({color_infos()})
    )
  )
}
