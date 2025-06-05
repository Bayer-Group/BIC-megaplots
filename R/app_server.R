#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#'
#' @importFrom dplyr arrange select group_by count filter n_distinct mutate ungroup row_number all_of
#' @importFrom plyr ddply summarize .
#' @importFrom DT renderDT datatable renderDataTable formatRound DTOutput
#' @importFrom graphics axis grconvertX grconvertY legend par points rect strheight strwidth text mtext
#' @importFrom reshape2 melt dcast
#' @importFrom rlang syms :=
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyjs runjs click disable enable
#' @import shinyWidgets
#' @importFrom stats aggregate cutree
#' @importFrom stringr str_wrap
#' @importFrom utils tail read.csv
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @noRd
#' @keywords internal

app_server <- function(input, output, session) {

  #initialize values with NULL
  A <- B <- megaplots_demo_data <- EVENT <- Group_ID <- LEVEL <- megaplots_selected_event_time <- subject <- megaplots_selected_subjectid <- NULL
  megaplots_selected_start_time <- megaplots_selected_end_time <- 'GROUP BY' <- NULL

  ns <- session$ns

  # set maximum data upload size to 750MB
  options(shiny.maxRequestSize = 750 * 1024^2)

  #### server part data upload ####
  # disable/enable import button if data are missing/available
  shiny::observe({
    shinyjs::disable(id = 'import.button', selector = NULL)
    df <- shiny::req(preprocessed_data()$megaplot_data)
    #if uploaded data are available and preprocessed enable import button
    if (!is.null(df)) { shinyjs::enable(id = 'import.button', selector = NULL) }
  })

  # update variables selection if Rdata file is uploaded
  shiny::observeEvent(c(input$file), {
    shiny::req(input$file)
    if (input$impswitch == '*.RData file') {
      updates_variables_selection_file(file = input$file, session = session)
    }
  })

  # update variables selection if csv (A) file is uploaded
  shiny::observeEvent(c(input$csvA), {
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
  shiny::observeEvent(c(input$csvB), {
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
    shiny::req(input$rdataB)
    if (!is.null(input$rdataB)) {
      updates_variables_selection_rdataB(
        file = input$rdataB,
        session = session
      )
    }
  })

  #### Preprocess uploaded data ####
  # 1. 'preprocessed_data' = standardized object in which the uploaded data are
  # preprocessed to get desired structure for data used in megaplots
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
      remove_variables <- names(
        which(
          apply(
            preprocessed_df$megaplot_data$A %>%
              dplyr::select(
                -c(
                  megaplots_selected_subjectid,
                  megaplots_selected_start_time,
                  megaplots_selected_end_time
                )
              ),
            2,
            function(x) {length(unique(x)) == 1}
          )
        )
      )
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

  output$err_message <- shiny::renderText({
    if (!is.null(preprocessed_data()$megaplot_error_message)) {
      str1 <- preprocessed_data()$megaplot_error_message
      paste(str1)
    }
  })

  #### Update all widgets when data are uploaded ####
  shiny::observeEvent(preprocessed_data()$megaplot_data, {
    if (input$selectdata == "Upload saved data") {
      saved_file <- readRDS(input$setting_file$datapath)
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.ev1",
        selected = saved_file$select.ev1
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.ev2",
        selected = saved_file$select.ev2
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.ev3",
        selected = saved_file$select.ev3
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.ev4",
        selected = saved_file$select.ev4
      )
      ## update order
      shinyjqui::updateOrderInput(
        session,
        inputId = "select.ev.lev1",
        items = saved_file$select.ev.lev1
      )
      shinyjqui::updateOrderInput(
        session,
        inputId = "select.ev.lev2",
        items = saved_file$select.ev.lev2
      )
      shinyjqui::updateOrderInput(
        session,
        inputId = "select.ev.lev3",
        items = saved_file$select.ev.lev3
      )
      shinyjqui::updateOrderInput(
        session,
        inputId = "select.ev.lev4",
        items = saved_file$select.ev.lev4
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = 'select.pal1',
        selected = preprocessed_data()$megaplot_data$saved$select.pal1,
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = 'select.pal2',
        selected = preprocessed_data()$megaplot_data$saved$select.pal2,
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = 'select.pal3',
        selected = preprocessed_data()$megaplot_data$saved$select.pal3,
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = 'select.pal4',
        selected = preprocessed_data()$megaplot_data$saved$select.pal4,
      )
      shiny::updateTextInput(
        session,
        inputId = "y_axis_label",
        label = "y axis label",
        value = preprocessed_data()$megaplot_data$saved$y_label
      )
      shiny::updateTextInput(
        session,
        inputId = "x_axis_label",
        label = "x axis label",
        value = preprocessed_data()$megaplot_data$saved$x_label
      )
    }
  })

  # 1b. calculate aggregated stats for sequencing and clustering after clicking import button
  summary_statistics_data <-shiny::eventReactive(input$import.button, {
    # function to summarise_megaplot_data and save results as list object with
    # entries 'total' and 'detail'
    summarise_megaplot_data(data = preprocessed_data())
  })


  # 2. 'data_w_event_and_group_information' = standardized object in which the original data is stored
  #           together with information on the grouping and event variables
  #           (will not be reactively modified in the app and can be used to
  #           create some of the UI input fields)
  data_w_event_and_group_information <- shiny::eventReactive(
    c(summary_statistics_data(),input$select.ev.lev1,input$select.ev.lev2,input$select.ev.lev3,input$select.ev.lev4),
    {

    shiny::req(preprocessed_data())
    shiny::req(c(input$select.ev.lev1,input$select.ev.lev2,input$select.ev.lev3,input$select.ev.lev4))

    #add grouping and event information to preprocessed data
    add_event_and_group_information(
      data = preprocessed_data(),
      summary_stats = summary_statistics_data(),
      event1 = input$select.ev.lev1,
      event2 = input$select.ev.lev2,
      event3 = input$select.ev.lev3,
      event4 = input$select.ev.lev4,
      updated_event1 = update_select.ev1(),
      updated_event2 = update_select.ev2(),
      updated_event3 = update_select.ev3(),
      updated_event4 = update_select.ev4(),
      data_selection = input$selectdata
    )
  })

  ## AI FUNCTIONALITY ##
  #initialize reactivevalue aiButton$seq
  aiButton <- shiny::reactiveValues(seq = 0 )

  shiny::observeEvent(input$seq.button, {
    if (input$seq.button > 0) {
      aiButton$seq <- input$seq.button
    }
  })

  # 3. 'data_w_ai_information' = copy from 'data_w_event_and_group_information'
  # that will be reactively modified based on the AI selections
  # from the user
  data_w_ai_information <- shiny::eventReactive(c(data_w_event_and_group_information(), aiButton$seq), {
    data_w_ai_information <- shiny::req(data_w_event_and_group_information())
    if (!is.null(collectSeq$varSeq)) {
      # Call the sequencing function with all the corresponding parameters:
      data_w_ai_information <- sequencing_var_app(
        data_w_ai_information,
        collectSeq$varSeq,
        input_seriation(),
        collectSeq$methSer,
        group = input$select.grouping,
        multiple_distmeasures = input$multiple_distmeasures
      )
    }
    data_w_ai_information
  })


  # 4. 'data_grouped_and_sorted' = copy from 'data_w_ai_information' that will be reactively modified
  #           in the sorting/grouping part based on user input (preparation for the plotting)
  #

  data_grouped_and_sorted <- shiny::reactive({
    input$selection_button
    input$subset.button

    if (input$selectdata == "Upload saved data") {
      if (upload_indicator$dat == 1) {
        shinyjs::click("apply.color")
        upload_indicator$dat <- 2
      }
    }

    add_sorting_information(
      data_frame = data_w_ai_information(),
      select_subsetting = input$select.subsetting,
      randet = inputB1$randet,
      seed =inputB1$seed,
      nshow =inputB1$nshow,
      start =inputB1$start,
      random = input$random,
      specific_ids = input$specific_ids,
      event_levels = input$event.levels,
      select_sorting = input$select.sorting,
      select_grouping = inputIMP$select.grouping,
      select_events = inputIMP$select.events
    )
  })

  #initialize reactive value upload_indicator$dat
  upload_indicator <- shiny::reactiveValues(dat = 1)

  shiny::observeEvent(input$import.button, {
    if (input$selectdata == "Upload saved data") {
      if (upload_indicator$dat == 2) {
        #simulate click on apply button to change color selection
        shinyjs::click("apply.color")
      }
    }
  })


  ## PLOTTING ##
  # height function for pixel-size based on subject number
  # height.random <- shiny::reactive({
  #   shiny::req(data_grouped_and_sorted(), data_w_event_and_group_information(), data_w_ai_information(), input$height_slider)
  #   (max(c(1, data_grouped_and_sorted()$A$'subject')) * 12) * input$height_slider
  # })

  # height function for megaplot image
  # height.mp <- shiny::eventReactive(c(session$clientData$output_image1_height, select.device()), {
  #   # calculate height based on width and screen-format (default=16:9)
  #   scale.par <- 0.65
  #   if (is.null(select.device())) {
  #    dev.sel <- '16:9'
  #   } else {
  #    dev.sel <- select.device()
  #   }
  #
  #   dsp <- 1 / eval(parse(text = gsub(':', '/', dev.sel)))
  #   img.width <- shiny::isolate(session$clientData$output_image1_width)
  #   if (is.null(img.width))
  #    img.width <- 250
  #   img.width * (dsp) * scale.par
  # })

  # set reactive value as start value for dynamic height adjustment
  # height_chk <- shiny::reactiveValues(hchk = 250)

  # update reactive value for height only if the new adjustment is bigger then 5%
  # shiny::observeEvent(height.mp(), {
  #   if (abs(height.mp() / height_chk$hchk - 1) > 0.05)
  #     height_chk$hchk <- height.mp()
  # })

  # create reactive plotting object
  # 5. 'data_w_plot_info' = add plot information to data set (zoom/range and colors)
  #

  data_w_plot_info <- shiny::reactive({
    session$clientData[["image1"]]
    shiny::req(data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())
    # create plotting data based on user selections

    data_w_plot_info <- data_grouped_and_sorted()
    data_w_event_and_group_information <- data_w_event_and_group_information()
    # constrain to selected x-range
    data_w_plot_info$A$megaplots_selected_start_time <- pmax(data_w_plot_info$A$megaplots_selected_start_time, input$range[1])
    data_w_plot_info$A$megaplots_selected_end_time <-
      pmin(pmax(data_w_plot_info$A$megaplots_selected_end_time, input$range[1]), input$range[2])
    data_w_plot_info$B <-
      subset(data_w_plot_info$B, megaplots_selected_event_time >= input$range[1] & megaplots_selected_event_time <= input$range[2])

    # set plotting colors
    col.ev <- list()
    type.ev <- list()

    if (!is.na(data_w_event_and_group_information$event[1])) {
      col.ev[[1]] <- color_pal1$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[1]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      col.ev[[2]] <- color_pal2$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[2]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      col.ev[[3]] <- color_pal3$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[3]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      col.ev[[4]] <- color_pal4$val[1:length(data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[4]]])]
    }
    if (!is.na(data_w_event_and_group_information$event[1])) {
      type.ev[[1]] <- color_pal1$val[length(color_pal1$val)]
    }
    if (!is.na(data_w_event_and_group_information$event[2])) {
      type.ev[[2]] <- color_pal2$val[length(color_pal2$val)]
    }
    if (!is.na(data_w_event_and_group_information$event[3])) {
      type.ev[[3]] <- color_pal3$val[length(color_pal3$val)]
    }
    if (!is.na(data_w_event_and_group_information$event[4])) {
      type.ev[[4]] <- color_pal4$val[length(color_pal4$val)]
    }
    if (length(col.ev)>0) {
      for (i in 1:length(data_w_event_and_group_information$event)) {
        names(col.ev[[i]]) <- data_w_event_and_group_information$event.lev[[data_w_event_and_group_information$event[i]]]
      }
      names(col.ev) <- data_w_event_and_group_information$event
      names(type.ev) <- data_w_event_and_group_information$event

      data_w_plot_info$col.ev <- col.ev
      data_w_plot_info$type.ev <- type.ev
    }
    data_w_plot_info
  })

  # global plot settings
  plot_par_settings <- shiny::reactive({
    shiny::req(data_w_plot_info(), data_w_event_and_group_information(), data_grouped_and_sorted())
    mar2 <- 5
    # create group labels
    grLab <- NULL
    mar4 <- 1
    if ((data_w_plot_info()$group[1] != 'NULL')) {
      mar4 <- 12
      tmp <-strsplit(levels(droplevels(data_w_plot_info()$A$Group_ID_char)), split = '::')
      grLab <- data.frame(
        'LABEL' = sapply(tmp,FUN = function(x) {paste(paste0(data_w_plot_info()$group,'=',stringr::str_wrap(rev(x), width = 25, exdent = 5)), collapse = '\n')}),
        'POS' = plyr::ddply(data_w_plot_info()$A,plyr::.(Group_ID), plyr::summarize,'POS' = mean(subject))$POS,
        stringsAsFactors = FALSE
      )
    }
    list('mar' = c(0, mar2, 0, mar4), 'grLab' = grLab)
  })

  #### Main plot output image1 ####
  output$image1 <- shiny::renderPlot({

    input$height_slider
    session$clientData[["image1"]]
    choiceGroup()
    shiny::req(data_w_plot_info(), data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())

    draw_megaplot(
      megaplot_data = data_w_plot_info(),
      select_color = select.col(),
      par_settings = plot_par_settings(),
      background_stripes = input$background_stripes,
      background_stripes_length = input$background.stripes.length,
      event_levels = input$event.levels,
      range = input$range,
      lines_instead_symbols = input$lines_instead_symbols,
      lines_options = input$lines_options,
      line_width = subl$thick,
      y_axis_label = input$y_axis_label,
      reference_line_1 = input$reference_line_1,
      reference_line_1_value = input$reference_line_1_value,
      reference_line_2 = input$reference_line_2,
      reference_line_2_value = input$reference_line_2_value,
      reference_line_3 = input$reference_line_3,
      reference_line_3_value = input$reference_line_3_value,
      select_events = inputIMP$select.events,
      color_subject_line_by_first_event = input.incev$inc.ev.subj
    )
  },
  # function for UI auto height resizing
  height = function() {
    # calculate height based on selected subject number
    (max(c(1, shiny::req(data_grouped_and_sorted())$A$'subject')) * 12) * shiny::req(input$height_slider)
  })



  max_legend_char <- shiny::reactiveVal({270})

  event_trigger <- reactive({list(data_w_event_and_group_information(), input.fontsize$fontsize)})

  shiny::observeEvent(event_trigger(), {
    event_levels <- sapply(data_w_event_and_group_information()$event.lev, function(x) paste(x, collapse = ""))
    number_levels <- unlist(lapply(data_w_event_and_group_information()$event.lev, length))
    x <- unlist(lapply(number_levels, function(x) paste(rep("_", x), collapse = "")))
    event_levels <- paste(event_levels, x)
    max_length_event_levels <- event_levels[which.max(nchar(event_levels))]
    if(input.fontsize$fontsize){
      cex.leg <- 1.2
    }
    else{
      cex.leg <- 0.9
    }
    x <- strwidth(max_length_event_levels, cex = cex.leg * 1.2, units = "inches") *96
    max_legend_char(x)
  })

  # legend plot
  output$image1Legend <- shiny::renderPlot({
    shiny::req(max_legend_char())
    shiny::req(data_w_plot_info(), data_grouped_and_sorted(), data_w_ai_information(), data_w_event_and_group_information())


    draw_megaplot_legend(
      megaplot_data = data_w_plot_info(),
      select_color = select.col()#,
      # par_settings = plot_par_settings(),
      # background_stripes = input$background_stripes,
      # background_stripes_length = input$background.stripes.length,
      # event_levels = input$event.levels,
      # range = input$range,
      # lines_instead_symbols = input$lines_instead_symbols,
      # lines_options = input$lines_options,
      # line_width = subl$thick,
      # y_axis_label = input$y_axis_label,
      # reference_line_1 = input$reference_line_1,
      # reference_line_1_value = input$reference_line_1_value,
      # reference_line_2 = input$reference_line_2,
      # reference_line_2_value = input$reference_line_2_value,
      # reference_line_3 = input$reference_line_3,
      # reference_line_3_value = input$reference_line_3_value,
      # select_events = inputIMP$select.events,
      # color_subject_line_by_first_event = input.incev$inc.ev.subj
    )

    # data_w_plot_info <- data_w_plot_info()
    # color_bg <- select.col()
    # par(mar = c(0, 0, 0, 0), bg = color_bg['plot.bg'])
    # plot(
    #   0,
    #   0,
    #   xlim = c(0, 1),
    #   ylim = c(0, 1),
    #   xlab = '',
    #   ylab = '',
    #   type = 'n',
    #   axes = FALSE
    # )
    #
    # # use rectangle as background (the par() setting does not work on some devices)
    # rect(
    #   xleft = grconvertX(0, 'ndc', 'user'),
    #   xright = grconvertX(1, 'ndc', 'user'),
    #   ybottom = grconvertY(0, 'ndc', 'user'),
    #   ytop = grconvertY(1, 'ndc', 'user'),
    #   xpd = NA,
    #   border = NA,
    #   col = color_bg['plot.bg']
    # )
    #
    # add.leg <- TRUE
    #
    # if (add.leg & (data_w_plot_info$event[1] != "NULL")) {
    #   # starting coordinates
    #   legY <-
    #     grconvertY(1 / (2* length(data_w_plot_info$event)), from = 'ndc', to = 'user')
    #   legY_num <-  1/ (4*length(data_w_plot_info$event))
    #   legY_num_o <- 1/ (2*length(data_w_plot_info$event))
    #   legYmax_num <- 1
    #   legYmax <- grconvertY(1, from = 'ndc', to = 'user')
    #   legX <- grconvertX(0, from = 'npc', to = 'user')
    #   # calculate 'cex'
    #   heightMar3 <-
    #     grconvertY(1, from = 'ndc', to = 'user') - grconvertY(0, from = 'ndc', to = 'user')
    #   leg.test <- legend(legX, legY, xpd = NA, pch = 15, legend = 'Why', plot = FALSE)
    #   if(input.fontsize$fontsize){
    #     cex.leg <- 1.2
    #   }
    #   else{
    #     cex.leg <- 0.9
    #   }
    #
    #   for (i in 1:length(data_w_plot_info$event)) {
    #     # set text color ('grey' if not shown in the plot)
    #     tmp <- data_w_event_and_group_information()$event.lev[[data_w_plot_info$event[i]]]
    #     col.legtxt <- rep(color_bg['plot.id'], length(tmp))
    #     names(col.legtxt) <- tmp
    #     col.legtxt[!tmp %in% data_w_plot_info$event.lev[[data_w_plot_info$event[i]]]] <-
    #       c('grey40', '#93a3ae', '#5D6A70', '#404A4E')[3]
    #     font.legtxt <- ifelse(col.legtxt == '#5D6A70', 3, 1)
    #     col.leg <- data_w_plot_info$col.ev[[data_w_plot_info$event[i]]]
    #
    #     # event name
    #     ltitle <- legend(
    #       legX,
    #       grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
    #       xjust = 0,
    #       yjust = 0.5,
    #       xpd = NA,
    #       bty = 'n',
    #       pch = NA,
    #       horiz = TRUE,
    #       col = NA,
    #       legend = "",
    #       text.col = color_bg['plot.id'],
    #       cex = cex.leg,
    #       text.font = 2
    #     )
    #     text(
    #       x = legX,
    #       y = grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
    #       xpd = NA,
    #       cex = cex.leg,
    #       font = 2,
    #       adj = c(0, 0.5),
    #       labels = paste0(data_w_plot_info$event[i], ': '),
    #       col = color_bg['plot.id']
    #     )
    #     legY_num <- legY_num + legY_num_o
    #     # legend
    #     lleft <- legX
    #
    #     for (j in 1:length(col.leg)) {
    #       l <-
    #         legend(
    #           lleft[j],
    #           grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
    #           xjust = 0,
    #           yjust = 0.5,
    #           xpd = NA,
    #           bty = 'n',
    #           pch = data_w_plot_info$sym.ev[i],
    #           horiz = TRUE,
    #           col = col.leg[j],
    #           legend = names(col.leg)[j],
    #           text.col = col.legtxt[j],
    #           pt.cex = 2.5,
    #           cex = cex.leg *1.2,
    #           text.font = font.legtxt[j]
    #         )
    #       lleft[j + 1] <- l$rect$left + l$rect$w
    #     }
    #     # modify y-coordinate for next legend
    #     legY_num <- legY_num + legY_num_o
    #
    #   }
    # }
  })


  # x-axis plot
  output$image1Axis <- shiny::renderPlot({
    # x-axis
    input$x_axis_label
    ax1.min <- shiny::req(input$range[1])
    ax1.max <- shiny::req(input$range[2])
    color_bg <- select.col()

    # calculate outer margin to account for the width of the scrollbar in the main plot
    scroll.px <-
      shiny::isolate(session$clientData$output_image1Axis_width) - shiny::isolate(session$clientData$output_image1_width)
    scroll.pct <-
      scroll.px / shiny::isolate(session$clientData$output_image1Axis_width)
    if (!is.na(scroll.pct)){
      if(scroll.pct <= 1 & scroll.pct >=0){
        par(
          mar = plot_par_settings()$mar,
          bg = color_bg['axleg.bg'],
          omd = c(0, 1 - scroll.pct, 0, 1)
        )
      } else {
        par(
          mar = plot_par_settings()$mar,
          bg = color_bg['axleg.bg'],
          omd = c(0, 1, 0, 1)
        )
      }
    }
    plot(
      0,
      0,
      xlim = c(ax1.min, ax1.max),
      ylim = c(0, 1),
      xlab = '',
      ylab = '',
      type = 'n',
      axes = FALSE
    )
    # use rectangle as background (the par() setting does not work on some devices)
    rect(
      xleft = grconvertX(0, 'ndc', 'user'),
      xright = grconvertX(1, 'ndc', 'user'),
      ybottom = grconvertY(0, 'ndc', 'user'),
      ytop = grconvertY(1, 'ndc', 'user'),
      xpd = NA,
      border = NA,
      col = color_bg['axleg.bg']
    )

    ax1.ticks <- seq(ax1.min, ax1.max, 1)
    if (input.xaxt$det.xaxt == TRUE)
      axis(
        1,
        at = ax1.ticks,
        labels = FALSE,
        tcl = -0.1,
        pos = grconvertY(0.9, from = 'nfc', to = 'user'),
        col = color_bg['plot.id'],
        col.axis = color_bg['plot.id']
      )
    axis(
      1,
      at = ax1.ticks[ax1.ticks %% 10 == 0],
      tcl = -0.4,
      pos = grconvertY(0.9, from = 'nfc', to = 'user'),
      col = color_bg['plot.id'],
      col.axis = color_bg['plot.id']
    )

    rowHeightY <- strheight('A', units = 'user', cex = par('cex'))
    cex.pt <-  0.3 * par('cex') / rowHeightY

    mtext(
      input$x_axis_label,
      side = 2,
      line = 1,
      adj = TRUE,
      las = 1,
      cex = cex.pt,
      col = color_bg['plot.id']
    )
    # points(
    #   x = input$refdate[1],
    #   y = grconvertY(0.9, from = 'nfc', to = 'user'),
    #   cex = cex.pt,
    #   pch = 18,
    #   col = rgb(1, 0, 0, alpha = 0.3)
    # )
    #
    # points(
    #   x = input$refdate[2],
    #   y = grconvertY(0.9, from = 'nfc', to = 'user'),
    #   cex = cex.pt,
    #   pch = 18,
    #   col = rgb(1, 0, 0, alpha = 0.3)
    # )

    if(input$reference_line_1) {
        points(
          x = input$reference_line_1_value,
          y = grconvertY(0.9, from = 'nfc', to = 'user'),
          cex = cex.pt,
          pch = 18,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }
      if(input$reference_line_2) {
        points(
          x = input$reference_line_2_value,
          y = grconvertY(0.9, from = 'nfc', to = 'user'),
          cex = cex.pt,
          pch = 18,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }
      if(input$reference_line_3) {
        points(
          x = input$reference_line_3_value,
          y = grconvertY(0.9, from = 'nfc', to = 'user'),
          cex = cex.pt,
          pch = 18,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }



  })

  # reactive plot output elements
  output$container_tag <- shiny::renderUI({
    shiny::tags$head(shiny::tags$style(HTML(
      paste0(
        '.container-fluid {background-color: ',
        select.col()['cont.bg'],
        ';}'
      )
    )))
  })

  # box with main plot
  # create reactive megaplot image with dynamic height adjustment as UI output
  output$megaplot <- shiny::renderUI({
   #  color_bg <- select.col()
   #  style.panel <- paste0(
   #    'overflow-y:scroll; background-color: ',
   #    color_bg['plot.bg'],
   #    '; margin-bottom: 0%; margin-top: -1%;',
   #    'max-height: ',
   #    height_chk$hchk,
   #    'px;'
   #  )
   #
   # shiny::wellPanel(
      shiny::plotOutput(
        outputId = 'image1',
        dblclick = shiny::clickOpts(id = "dblclick_scatter"),
        brush = shiny::brushOpts(
          id = "image1_brush",
          fill = "#ffffff",
          stroke = "#036",
          opacity = 0.25,
          delay = 300
        ),
        hover = shiny::hoverOpts(
          "image1_hover",
          delay = 300,
          delayType = "debounce"
        ),
        height = 'auto'
      )#,
    #   style = style.panel
    # )
  })

  brush_coord <- shiny::reactiveValues(
    x = NULL,
    y = NULL
  )

  shiny::observeEvent(input$image1_brush, {
    brush_coord$x <- c(input$image1_brush$xmin, input$image1_brush$xmax)
    brush_coord$y <-
      c(input$image1_brush$ymin, input$image1_brush$ymax)
  })

  shiny::observeEvent(input$dblclick_scatter, {
    brush_coord$x <- NULL
    brush_coord$y <- NULL
  })

  output$check_slider_used <- shiny::reactive({
    req(data_w_event_and_group_information(), input$range)
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    rangemin1 <- input$range[1]
    rangemax1 <- input$range[2]

    if (min1 != rangemin1 | max1 != rangemax1) {
      tmp <- TRUE
    } else {
      tmp <- FALSE
    }
    tmp
  })

  shiny::outputOptions(output, "check_slider_used", suspendWhenHidden = FALSE)

  output$next_buttons <- shiny::renderUI({
    shiny::absolutePanel(
      id = "next_buttons",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #222d32'>"
      )),
      top = 300,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = 60,
      height = "auto",
      shiny::fluidRow(
        shinyWidgets::circleButton(
          inputId = "btn1",
          icon = icon("step-backward"),
          status = "default",
          size = "xs"
        ),
        shinyWidgets::circleButton(
          inputId = "btn2",
          icon = icon("step-forward"),
          status = "default",
          size = "xs"
        )
      ),
      style = "z-index: 10;"
    )
  })

  output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #222d32'>"
      )),
      HTML(
        '
        <button style =
        "background: #3c8dbc;
        color:#ffffff",
        data-toggle="collapse" data-target="#demo" style="color:white;">
        <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'
      ),
      top = 80,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      shiny::tags$div(
        id = 'demo',
        class = "collapse",
        shiny::fluidRow(shiny::column(2,
                                      shiny::plotOutput('hover')))
      ),
      style = "z-index: 10;"
    )
  })

  output$summarypanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "summarypanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: #222d32'>"
        )
      ),
      HTML(
        '
        <button style =
        "background: #3c8dbc;
        color:#ffffff",
        data-toggle="collapse" data-target="#demo2" style="color:white;">
        <i class="fa-solid fa-search-plus"></i> Open/Close Summary Panel</button>'
      ),
      top = 120,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      shiny::tags$div(
        id = 'demo2',
        class = "collapse",
          HTML(
            summary_statistics_text$val
          )
      ),
      style = "z-index: 10;"
    )
  })

  output$hover <- shiny::renderPlot({
    shiny::req(data_w_plot_info())
    color_bg <- select.col()
    data_w_plot_info <- data_w_plot_info()

    if (!is.null(brush_coord$x) & !is.null(brush_coord$y)) {
      xlim <- brush_coord$x
      ylim <- brush_coord$y
      opar <- par("mfrow", "mar")
      par("mar" = c(5.1, 5.6, 4.1, 0.6))
      on.exit(par(opar))
      plot(
        NULL,
        xlim = xlim,
        ylim = ylim,
        xlab = '',
        ylab = '',
        axes = FALSE,
        yaxs = 'i'
      )

      # use rectangle as background (the par() setting does not work on some devices)
      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        xpd = NA,
        border = NA,
        col = color_bg['plot.bg']
      )

      # set label and point size
      rowHeightY <- strheight('A', units = 'user', cex = par('cex'))
      rowHeightX <- strwidth('A', units = 'user', cex = par('cex'))
      yxRatio <- rowHeightY / rowHeightX

      # cex.subjLab <- (0.4 - (median(nchar(data_w_event_and_group_information()$A$megaplots_selected_subjectid))*0.02))  / max(rowHeightY,0.5)
      # cex.subjLab <-  1.3 * par('cex') / rowHeightY
      cex.point <-
        0.5 * par('cex') / rowHeightY * min(c(1, 0.95 * yxRatio))

      cex.point <- c(1, 0.92, 0.47, 0.8, 0.9) * cex.point
      # draw lines
      rect(
        xleft = data_w_plot_info$A$megaplots_selected_start_time,
        xright = data_w_plot_info$A$megaplots_selected_end_time,
        ybottom = data_w_plot_info$A$subject - subl$thick,
        ytop = data_w_plot_info$A$subject + subl$thick,
        col = color_bg[2],
        border = NA
      )

      if (!is.null(inputIMP$select.events) &
          !is.null(input$event.levels)) {
        levs <- character(0)
        sevs <- character(0)
        for (i in 1:length(data_w_plot_info$event)) {
          if (data_w_plot_info$type.ev[[data_w_plot_info$event[i]]] == "line")
            levs <- c(levs, data_w_plot_info$event[i])
          else if (data_w_plot_info$type.ev[[data_w_plot_info$event[i]]] == "symbol")
            sevs <- c(sevs, data_w_plot_info$event[i])
        }

        if (sum(data_w_plot_info$type.ev == "line") > 0) {
          tmp.height <-
            seq(-subl$thick, subl$thick, length = (length(levs) + 1))
          for (i in 1:length(levs)) {
            tmp <- na.exclude(data_w_plot_info$B[, c('subject', 'megaplots_selected_event_time', levs[i])])
            tmp.col <- data_w_plot_info$col.ev[[levs[i]]]
            rect(
              xleft = tmp$megaplots_selected_event_time - 0.5,
              xright = tmp$megaplots_selected_event_time + 0.5,
              ybottom = tmp$subject + tmp.height[i],
              ytop = tmp$subject + tmp.height[i + 1],
              border = NA,
              col = tmp.col[as.character(tmp[, levs[i]])]
            )
          }
        }

        if (sum(data_w_plot_info$type.ev == "symbol") > 0) {
          for (i in 1:length(sevs)) {
            tmp <- na.exclude(data_w_plot_info$B[, c('subject', 'megaplots_selected_event_time', sevs[i])])
            tmp.col <- data_w_plot_info$col.ev[[sevs[i]]]
            points(
              tmp$megaplots_selected_event_time,
              tmp$subject,
              pch = data_w_plot_info$sym.ev[i],
              cex = cex.point[i],
              col = tmp.col[as.character(tmp[, sevs[i]])]
            )
          }
        }
      }

      # add points
      if (!is.null(inputIMP$select.events) &
          !is.null(input$event.levels) & !(input$lines_instead_symbols)) {
        for (i in 1:length(data_w_plot_info$event)) {
          tmp <- na.exclude(data_w_plot_info$B[, c('subject', 'megaplots_selected_event_time', data_w_plot_info$event[i])])
          tmp.col <- data_w_plot_info$col.ev[[data_w_plot_info$event[i]]]
          if (i == 1 & input.incev$inc.ev.subj) {
            rect(
              xleft = tmp$megaplots_selected_event_time - 0.5,
              xright = tmp$megaplots_selected_event_time + 0.5,
              ybottom = tmp$subject - subl$thick,
              ytop = tmp$subject + subl$thick,
              border = NA,
              col = tmp.col[as.character(tmp[, data_w_plot_info$event[i]])]
            )
          } else {
            points(
              tmp$megaplots_selected_event_time,
              tmp$subject,
              pch = data_w_plot_info$sym.ev[i],
              cex = cex.point[i],
              col = tmp.col[as.character(tmp[, data_w_plot_info$event[i]])]
            )
          }
        }
      }

      if (!is.null(inputIMP$select.events) &
          !is.null(input$event.levels) & input$lines_instead_symbols) {
        tmp.height <-
          seq(-subl$thick, subl$thick, length = (length(data_w_plot_info$event) + 1))
        for (i in 1:length(data_w_plot_info$event)) {
          tmp <- na.exclude(data_w_plot_info$B[, c('subject', 'megaplots_selected_event_time', data_w_plot_info$event[i])])
          tmp.col <- data_w_plot_info$col.ev[[data_w_plot_info$event[i]]]
          rect(
            xleft = tmp$megaplots_selected_event_time - 0.5,
            xright = tmp$megaplots_selected_event_time + 0.5,
            ybottom = tmp$subject + tmp.height[i],
            ytop = tmp$subject + tmp.height[i + 1],
            border = NA,
            col = tmp.col[as.character(tmp[, data_w_plot_info$event[i]])]
          )
        }
      }

      index <- c(floor(ylim), ceiling(ylim))

      subject.brush <-
        data_w_plot_info$A[data_w_plot_info$A$subject %in% index[1]:index[2], ]$megaplots_selected_subjectid

      A.sub <- data_w_plot_info$A[data_w_plot_info$A$subject %in% index[1]:index[2], ]
      rowHeightY <-
        strheight('A.sub', units = 'user', cex = par('cex'))


      cex.sub <- (0.6 - (median(nchar(data_w_event_and_group_information()$A$megaplots_selected_subjectid))*0.02))  / max(rowHeightY,0.5)

      if (length(subject.brush) > 0) {
        text(
          x = grconvertX(0.001, from = 'npc', to = 'user'),
          y = A.sub$subject,
          xpd = NA,
          adj = c(1, 0.5),
          cex = cex.sub,
          labels = subject.brush,
          col = color_bg[4]
        )
      }

      # reference line

      if(input$reference_line_1) {
        rect(
          xleft = input$reference_line_1_value - 0.25,
          xright = input$reference_line_1_value + 0.25,
          ybottom = grconvertY(0, 'npc', 'user'),
          ytop = grconvertY(1, 'npc', 'user'),
          border = NA,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }
      if(input$reference_line_2) {
        rect(
          xleft = input$reference_line_2_value - 0.25,
          xright = input$reference_line_2_value + 0.25,
          ybottom = grconvertY(0, 'npc', 'user'),
          ytop = grconvertY(1, 'npc', 'user'),
          border = NA,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }
      if(input$reference_line_3) {
        rect(
          xleft = input$reference_line_3_value - 0.25,
          xright = input$reference_line_3_value + 0.25,
          ybottom = grconvertY(0, 'npc', 'user'),
          ytop = grconvertY(1, 'npc', 'user'),
          border = NA,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }

      # rect(
      #   xleft = input$refdate[1] - 0.25,
      #   xright = input$refdate[1] + 0.25,
      #   ybottom = grconvertY(0, 'npc', 'user'),
      #   ytop = grconvertY(1, 'npc', 'user'),
      #   border = NA,
      #   col = rgb(1, 0, 0, alpha = 0.3)
      # )
      #
      # rect(
      #   xleft = input$refdate[2] - 0.25,
      #   xright = input$refdate[2] + 0.25,
      #   ybottom = grconvertY(0, 'npc', 'user'),
      #   ytop = grconvertY(1, 'npc', 'user'),
      #   border = NA,
      #   col = rgb(1, 0, 0, alpha = 0.3)
      # )

      ax1.ticks <- floor(seq(xlim[1], xlim[2], 1))
      if (input.xaxt$det.xaxt == TRUE) {
        axis(
          1,
          at = ax1.ticks,
          labels = TRUE,
          tcl = -0.1,
          pos = grconvertY(0.1, from = 'nfc', to = 'user'),
          col = color_bg['plot.id'],
          col.axis = color_bg['plot.id']
        )
      } else {
        axis(
          1,
          at = ax1.ticks[ax1.ticks %% 10 == 0],
          tcl = -0.4,
          pos = grconvertY(0.1, from = 'nfc', to = 'user'),
          col = color_bg['plot.id'],
          col.axis = color_bg['plot.id']
        )
      }
    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = select.col()['plot.bg'],
        xpd = TRUE
      )
      text(0.5, 0.6, "Please use mouse brush on plot", col = select.col()['plot.id'])
      text(0.5, 0.5, "to get an enlarged version of an area!", col = select.col()['plot.id'])
      text(0.5,
           0.4,
           "(Brushing is clicking and dragging a selection box. ",
           col = select.col()['plot.id'])
      text(0.5,
           0.3,
           "Remove selection box with doubleclick.)",
           col = select.col()['plot.id'])
    }
  }, width = 400)



  shiny::observeEvent(input$reset_draggable_panel_positions, {

 output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #222d32'>"
      )),
      HTML(
        '
        <button style =
        "background: #3c8dbc;
        color:#ffffff",
        data-toggle="collapse" data-target="#demo" style="color:white;">
        <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'
      ),
      top = 80,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      shiny::tags$div(
        id = 'demo',
        class = "collapse",
        shiny::fluidRow(shiny::column(2,
                                      shiny::plotOutput('hover')))
      ),
      style = "z-index: 10;"
    )
  })

  output$summarypanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "summarypanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
        HTML(paste0(
          "<div style='background-color: #222d32'>"
        )
      ),
      HTML(
        '
        <button style =
        "background: #3c8dbc;
        color:#ffffff",
        data-toggle="collapse" data-target="#demo2" style="color:white;">
        <i class="fa-solid fa-search-plus"></i> Open/Close Summary Panel</button>'
      ),
      top = 120,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      shiny::tags$div(
        id = 'demo2',
        class = "collapse",
          HTML(
            summary_statistics_text$val
          )
      ),
      style = "z-index: 10;"
    )
  })

    output$hover_legend <- shiny::renderUI({
    leg.height <- paste0(40*length(data_w_plot_info()$event),'px')
    if (input.fontsize$fontsize) {
      leg.height <- paste0(60*length(data_w_plot_info()$event),'px')
    }

    shiny::absolutePanel(
      id = "hover_legend",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #404A4E'>"
      )),
      top = 333,
      left = "auto",
      right = 75,
      bottom = "auto",
      width = max_legend_char(),
      height = "auto",
      shiny::fluidRow(
        shiny::plotOutput('image1Legend', height = leg.height, width = max_legend_char())
      ),
      style = "z-index: 10;"
    )
  })
  })


  output$hover_legend <- shiny::renderUI({
    leg.height <- paste0(40*length(data_w_plot_info()$event),'px')
    if (input.fontsize$fontsize) {
      leg.height <- paste0(60*length(data_w_plot_info()$event),'px')
    }

    shiny::absolutePanel(
      id = "hover_legend",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0(
        "<div style='background-color: #404A4E'>"
      )),
      top = 333,
      left = "auto",
      right = 75,
      bottom = "auto",
      width = max_legend_char(),
      height = "auto",
      shiny::fluidRow(
        shiny::plotOutput('image1Legend', height = leg.height, width = max_legend_char())
      ),
      style = "z-index: 10;"
    )
  })

  # box with axis
  output$axisbox <- shiny::renderUI({
    # color_bg <- select.col()
    # style.panel_axisbox <-
    #   paste0('background-color: ', color_bg['cont.bg'], '; border-style: none;')
    # shiny::wellPanel(
      shiny::plotOutput(
        outputId = 'image1Axis',
        height = '40px'
      )#,
     #   style = style.panel_axisbox
     # )
  })

  # RAW DATA
  # set UI data set input
  output$select.raw <- shiny::renderUI({
    choices <- names(req(preprocessed_data()$megaplot_data))[c(1, 2)]
    shinyWidgets::pickerInput(
      inputId = 'select.raw',
      label = 'Select data set based on main options:',
      choices = choices,
      width = 'fit',
      multiple = FALSE,
      selected = choices[1],
      options = list(`style` = 'background: btn-primary')
    )
  })

    # CREATE UI ELEMENTS
  inputIMP <- shiny::reactiveValues(
    select.events = NULL,
    select.grouping = NULL,
    name = ''
  )

  shiny::observeEvent(input$select.events, ignoreNULL = FALSE, {
    inputIMP$select.events <- input$select.events
  })

  shiny::observeEvent(c(input$import.button, data_w_event_and_group_information()), {
    shiny::req(data_w_event_and_group_information())
    choices <- shiny::isolate(data_w_event_and_group_information()$event)
    if (is.null(isolate(data_w_event_and_group_information()$event))) {
      selected <- choices
    } else {
      selected <- shiny::isolate(data_w_event_and_group_information()$event)
    }

    if (input$selectdata == "Upload saved data") {
      selected <- preprocessed_data()$megaplot_data$saved$select.events

    }
    shiny::updateSelectizeInput(
      session,
      inputId = "select.events",
      choices = choices,
      selected = selected
    )
  })

  selected.event.levels <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$event.levels, {
    selected.event.levels$val <- input$event.levels
  })

  # grouping
  choiceGroup <- shiny::eventReactive(c(data_w_event_and_group_information()), {
    if (input$selectdata == "Upload saved data") {
      preprocessed_data()$megaplot_data$saved$select.grouping
    } else {
      data_w_ai_information()$group
    }
  })

  shiny::observeEvent(choiceGroup(), {
    choices <- shiny::req(choiceGroup())
    selected <- NULL

    shiny::updateSelectizeInput(
      session,
      inputId = "select.grouping",
      selected = selected,
      choices = choices
    )
  })

  shiny::observeEvent(input$select.grouping, ignoreNULL = FALSE, {
    inputIMP$select.grouping <- input$select.grouping
  })

  # subset selection
  shiny::observeEvent(data_w_ai_information(), {
    shiny::req(data_w_ai_information())

    choices <- unlist(data_w_ai_information()$group.lev, use.names = FALSE)
    choices.lab <-
      rep(data_w_ai_information()$group, sapply(data_w_ai_information()$group.lev, FUN = length))
    choices <- paste0(choices.lab, ' = ', choices)

    if (input$selectdata == "Upload saved data") {
      choices <-
        unlist(preprocessed_data()$megaplot_data$saved$group.lev, use.names = FALSE)
      choices.lab <-
        rep(
          preprocessed_data()$megaplot_data$saved$group,
          sapply(preprocessed_data()$megaplot_data$saved$group.lev, FUN = length)
        )
      choices <- paste0(choices.lab, ' = ', choices)

      selected <- preprocessed_data()$megaplot_data$saved$select.subsetting
    } else {
      selected <- choices
    }

    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.subsetting",
      choices = choices,
      selected = selected
    )
  })

  # sorting
  choiceSort <- shiny::eventReactive(c(data_w_event_and_group_information(), aiButton$seq), {
    data_w_ai_information()$nume_A[!is.na(data_w_ai_information()$nume_A)]
  })

  shiny::observeEvent(choiceSort(), {
    choices <- choiceSort()

    selected <- 'megaplots_selected_subjectid'
    if (any(choices == 'SEQUENCING'))
      selected <- utils::tail(choices, 1)

    if (input$selectdata == 'Upload saved data') {
      selected <- preprocessed_data()$megaplot_data$saved$select.sorting
    }
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.sorting",
      choices = choices,
      selected = selected
    )
  })

  ## ui elements in the side bar
  output$impdata <- shiny::renderUI({

    if (input$impswitch == '*.RData file') {
      shiny::fileInput(
        inputId = 'file',
        label = HTML('<p style="color:#4493da"> Choose RData file with a list including data sets A and B </p>'),
        multiple = FALSE,
        accept = '.RData'
      )
    } else if (input$impswitch == '*.CSV files') {
      shiny::tagList(
        shiny::fixedRow(
          shiny::column(6,
            shiny::fileInput(
              inputId = 'csvA',
              label = 'Choose csv file with data set A (subject information)',
              multiple = TRUE,
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv'
              )
            ),
            shiny::fileInput(
              inputId = 'csvB',
              label = 'Choose csv file with data set B (event information)',
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
              inputId = 'sep',
              label = 'Select separator',
              inline = TRUE,
              choices = c(
                'Comma' = ',',
                'Semicolon' = ';',
                'Tab' = '\t'
              ),
              selected = ','
            ),
            shiny::radioButtons(
              inputId = 'quote',
              label = 'Select quote',
              inline = TRUE,
              choices = c(
                None = '',
                'Double Quote (")' = '"',
                "Single Quote (')" = "'"
              ),
              selected = '"'
            ),
            shiny::radioButtons(
              inputId = 'dec',
              label = 'Select decimal character',
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
            inputId = 'rdataA',
            label = 'Choose RData file with data set A (subject information)',
            multiple = TRUE,
            accept = c('.RData','.rdata','.Rdata')
          ),
          shiny::fileInput(
            inputId = 'rdataB',
            label = 'Choose RData file with data set B (event information)',
            multiple = TRUE,
            accept = c('.RData','.rdata','.Rdata')
          )
        )
      )
    }
  })

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


  # event main selections
  event.info <- shiny::reactive({
    if (!is.null(preprocessed_data()$megaplot_data)) {
      B <- preprocessed_data()$megaplot_data$B
      char_B <- names(which(sapply(B, is.character)))
      char_B
    }
  })

  output$select.ev1 <- shiny::renderUI({
    choices <- shiny::req(event.info())
    uiElement <- shinyWidgets::pickerInput(
      inputId = 'select.ev1',
      label = "Select event (1)",
      choices = choices,
      selected = choices[1],
      multiple = FALSE,
      options = list(
        `live-search` = TRUE,
        `style` = 'background: btn-primary',
        `header` = 'Select item'
      )
    )
  })

  output$select.ev.lev1 <- shiny::renderUI({
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev1)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev1]])
    choices <- sort(choices[!is.na(choices)])
    if (input$selectdata == "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev1)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev1
      }
    }
    shinyjqui::orderInput(
      inputId = "select.ev.lev1",
      label = "Select order of event (1)",
      items = choices,
      width = "75px"
    )

  })

  shiny::outputOptions(output, "select.ev.lev1", suspendWhenHidden = FALSE)

  output$select_ev_lev1_button <- shiny::renderUI({
    if (length(req(event.info())) >= 1) {
      shiny::req(input$select.ev1)
      shiny::checkboxInput(
        inputId = "select_ev_lev1_button",
        label = "Sort events",
        value = FALSE
      )
    }
  })

  output$select.ev.lev2 <- shiny::renderUI({
    shiny::req(preprocessed_data())
    shiny::req(input$select.ev2)
    choices <- unique(preprocessed_data()$megaplot_data$B[[input$select.ev2]])
    choices <- sort(choices[!is.na(choices)])
    if (input$selectdata == "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev2)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev2
      }
    }
    shinyjqui::orderInput(
      inputId = "select.ev.lev2",
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
        inputId = "select_ev_lev2_button",
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
    if (input$selectdata == "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev3)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev3
      }
    }
    shinyjqui::orderInput(
      inputId = "select.ev.lev3",
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
        inputId = "select_ev_lev3_button",
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
    if (input$selectdata == "Upload saved data") {
      if (all(choices %in% preprocessed_data()$megaplot_data$saved$select.ev.lev4)) {
        choices <- preprocessed_data()$megaplot_data$saved$select.ev.lev4
      }
    }
    shinyjqui::orderInput(
      inputId = "select.ev.lev4",
      label = "Select order of event (4)",
      items = choices,
      width = "75px"
    )
  })

  shiny::outputOptions(output, "select.ev.lev4", suspendWhenHidden = FALSE)

  output$select_ev_lev4_button <- shiny::renderUI({
    if (length(req(event.info())) >= 4) {
      shiny::req(input$select.ev4)
      shiny::checkboxInput(inputId = "select_ev_lev4_button",
                           label = "Sort events",
                           value = FALSE)
    }
  })

  output$select.ev2 <- shiny::renderUI({
    if (length(req(event.info())) >= 2) {
      choices <- shiny::req(event.info())
      uiElement <- shinyWidgets::pickerInput(
        inputId = 'select.ev2',
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

  loaded_sel_pal1 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal1, input$import.button), {
    loaded_sel_pal1$dat <- !is.null(input$select.ev1)
  })

  output$load_sel_pal1 <- shiny::reactive(loaded_sel_pal1$dat)

  shiny::outputOptions(output, "load_sel_pal1", suspendWhenHidden = FALSE)

  loaded_sel_pal2 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal2, input$import.button), {
    loaded_sel_pal2$dat <- !is.null(input$select.ev2)
  })

  output$load_sel_pal2 <- shiny::reactive(loaded_sel_pal2$dat)

  shiny::outputOptions(output, "load_sel_pal2", suspendWhenHidden = FALSE)

  loaded_sel_pal3 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal3, input$import.button), {
    loaded_sel_pal3$dat <- !is.null(input$select.ev3)
  })

  output$load_sel_pal3 <- shiny::reactive(loaded_sel_pal3$dat)
  shiny::outputOptions(output, "load_sel_pal3", suspendWhenHidden = FALSE)

  loaded_sel_pal4 <- shiny::reactiveValues(dat = FALSE)

  shiny::observeEvent(c(input$select.pal4, input$import.button), {
    loaded_sel_pal4$dat <- !is.null(input$select.ev4)
  })

  output$load_sel_pal4 <- shiny::reactive(loaded_sel_pal4$dat)
  shiny::outputOptions(output, "load_sel_pal4", suspendWhenHidden = FALSE)

  output$select.ev3 <- shiny::renderUI({
    if (length(req(event.info())) >= 3) {
      choices <- shiny::req(event.info())
      uiElement <- shinyWidgets::pickerInput(
        inputId = 'select.ev3',
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
        inputId = 'select.ev4',
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

  # connect to submit button (in UI)
  update_select.ev1 <-
    shiny::eventReactive(c(input$import.button, preprocessed_data()$megaplot_data), {
      input$select.ev1
    })
  update_select.ev2 <-
    shiny::eventReactive(c(input$import.button, preprocessed_data()$megaplot_data), {
      input$select.ev2
    })
  update_select.ev3 <-
    shiny::eventReactive(c(input$import.button, preprocessed_data()$megaplot_data), {
      input$select.ev3
    })
  update_select.ev4 <-
    shiny::eventReactive(c(input$import.button, preprocessed_data()$megaplot_data), {
      input$select.ev4
  })

  # population options
  dispmax <- 150

  subset.flag <- shiny::reactiveValues(val = FALSE)

  output$check_subset <- shiny::reactive({
    subset.flag$val
  })

  shiny::outputOptions(output, "check_subset", suspendWhenHidden = FALSE)

  shiny::observeEvent(c(input$random, input$selectdata), {
    shiny::req(preprocessed_data())
    nmax <- length(unique(preprocessed_data()$megaplot_data$A$megaplots_selected_subjectid))
    if (!is.null(nmax)) {
      subset.flag$val <-  nmax == input$random
    }
  })

  shiny::observeEvent(c(input$selectdata, input$import.button), {
    nmax <- length(unique(preprocessed_data()$megaplot_data$A$megaplots_selected_subjectid))
    if (input$selectdata == "Upload saved data") {
      nmax <- preprocessed_data()$megaplot_data$saved$random
    }

    num_sub <- length(unique(preprocessed_data()$megaplot_data$A$megaplots_selected_subjectid))

    if (input$selectdata == "Upload saved data") {
      shiny::updateSliderInput(
        session,
        inputId = 'random',
        label = paste("Number of displayed subjects"),
        min = 1,
        value = preprocessed_data()$megaplot_data$saved$random,
        max = num_sub,
        step = 1
      )
    } else {
      shiny::updateSliderInput(
        session,
        inputId = 'random',
        label = paste("Number of displayed subjects"),
        min = 1,
        #value = min(num_sub, 150),
        value = num_sub,
        max = num_sub,
        step = 1
      )
    }
  })

  shiny::observeEvent(c(input$random, input$import.button), {
    shiny::req(input$random)

    nmax <- length(unique(preprocessed_data()$megaplot_data$A$megaplots_selected_subjectid))
    if (input$selectdata == "Upload saved data") {
      nmax <- preprocessed_data()$saved$random
    }

    shiny::updateNumericInput(
      session,
      inputId = "startsubj",
      max = nmax - input$random + 1,
      value = ifelse(
        input$selectdata == "Upload saved data",
        preprocessed_data()$megaplot_data$saved$startsubj,
        1
      )
    )
  })

  # shiny::observeEvent(input$user.device, {
  #   shinyWidgets::updatePickerInput(
  #     session,
  #     inputId = "select.device",
  #     selected = input$user.device
  #   )
  # })

  shiny::observeEvent(input$selection_button, {
    shiny::updateTabsetPanel(
      inputId = "Change_input_for_deterministic_or_random",
      selected = input$selection_button
    )
  })

  inputB1 <- shiny::reactiveValues(
    nshow = 150,
    start = 1,
    randet = 'deterministic',
    seed = 2006
  )

  shiny::observeEvent(c(input$import.button, input$subset.button, input$random),{
    inputB1$nshow <- input$random
    inputB1$start <- input$startsubj
    inputB1$randet <- input$selection_button
    inputB1$seed <- input$seedset
  })

  # setting history tables
  seedhist <- shiny::reactiveValues(
    'curval' = data.frame(
      'Seed' = NA,
      'Subjects' = NA,
      'Start' = NA
    ),
    'histval' = NULL
  )

  shiny::observeEvent(input$import.button, {
    seedhist$curval <- data.frame(
      'Seed' = ifelse(
        shiny::req(input$selection_button) != 'deterministic',
        input$seedset,
        NA
      ),
      'Subjects' = ifelse(
        input$selectdata == "Upload saved data",
        preprocessed_data()$megaplot_data$saved$random,
        min(input$random, dispmax)
      ),
      'Start' = ifelse(
        shiny::req(input$selection_button) == 'deterministic',
        input$startsubj,
        NA
      )
    )
  })

  # device settings
  # thickness of subject lines
  subl <- shiny::reactiveValues(thick = 0.05)

  shiny::observeEvent(input$thick, {
    subl$thick <- input$thick
  })

  # detailed axis ticks
  input.xaxt <- shiny::reactiveValues(det.xaxt = TRUE)

  shiny::observeEvent(input$det.xaxt, {
    input.xaxt$det.xaxt <- input$det.xaxt
  })

  # first event is part of subject line
  input.incev <- shiny::reactiveValues(inc.ev.subj = FALSE)

  shiny::observeEvent(input$inc.ev.subj, {
    input.incev$inc.ev.subj <- input$inc.ev.subj
  })

  # increase font size (30%)
  input.fontsize <- shiny::reactiveValues(fontsize = FALSE)

  output$fontsize <- shiny::renderUI({
    NULL
  })

  shiny::observeEvent(input$incr.font, ignoreInit = TRUE, {
    input.fontsize$fontsize <- input$incr.font
    output$fontsize <- shiny::renderUI({
      if (input$incr.font) {
        tags$head(tags$style(HTML('* {font-size: 18px;}')))
      } else {
        tags$head(tags$style(HTML('* {font-size: 14px;}')))
      }
    })
  })

  # set color theme
  coltheme <- shiny::reactiveValues(col_sel = 'grey (app version)')

  shiny::observeEvent(input$select.col, {
    coltheme$col_sel <- input$select.col
  })

  select.col <- shiny::reactive({
    if (coltheme$col_sel == 'grey (app version)') {
      col_sel <- c(
        'plot.bg' = '#404A4E',
        'plot.lines' = '#000000',
        'plot.wp' = '#404A4E',
        'plot.id' = 'white',
        'axleg.bg' = '#222d32',
        'cont.bg' = '#222d32'
      )
    } else if (coltheme$col_sel == 'white (print version)') {
      col_sel <- c(
        'plot.bg' = '#ffffff',
        'plot.lines' = '#c9c9c9',
        'plot.wp' = '#ffffff',
        'plot.id' = 'black',
        'axleg.bg' = '#ffffff',
        'cont.bg' = '#ffffff'
      )
    } else {
      col_sel <- "red"
    }
    col_sel
  })

  select.device <- shiny::reactive({
    if (is.null(input$select.device)) {
      dev.out <- input$user.device
    } else{
      dev.out <- input$select.device
    }
    dev.out
  })

  # sequencing
  shiny::observeEvent(data_w_event_and_group_information(), {
    choices <- data_w_event_and_group_information()$event
    selected <- data_w_event_and_group_information()$event[1]

    shinyWidgets::updatePickerInput(
      session,
      inputId = "varSeq",
      choices = choices,
      selected = selected
    )
  })

  collectSeq <- shiny::reactiveValues(
    varSeq = NULL,
    methSer = NULL
  )

  # shiny::observeEvent(input$distmeasure, {
  #   shiny::updateTabsetPanel(inputId = "Change_input_for_seq_metod", selected = input$distmeasure)
  # })

  shiny::observeEvent(input$import.button, {
    collectSeq$varSeq <- NULL
    collectSeq$methSer <- NULL

    if (input$selectdata == "Upload saved data") {
      shiny::updateRadioButtons(
        session,
        inputId = "selection_button",
        selected = preprocessed_data()$megaplot_data$saved$selection_button
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.device",
        selected = preprocessed_data()$megaplot_data$saved$select.device
      )
    }

    shinyWidgets::updatePickerInput(
      session,
      inputId = "specific_ids",
      selected = NULL,
      choices = preprocessed_data()$megaplot_data$A$megaplots_selected_subjectid,
    )

    if (input$selectdata == "Upload saved data") {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "specific_ids",
        selected = preprocessed_data()$megaplot_data$saved$specific_ids
      )
    }

    if (input$selectdata == "Upload saved data") {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.col",
        selected = preprocessed_data()$megaplot_data$saved$select.col
      )
    }

    if (input$selectdata == "Upload saved data") {
      shiny::updateNumericInput(
        session,
        inputId = "seedset",
        value = preprocessed_data()$megaplot_data$saved$seedset
      )

      inputB1$nshow <- preprocessed_data()$megaplot_data$saved$random
      inputB1$start <- preprocessed_data()$megaplot_data$saved$startsubj
      inputB1$randet <- preprocessed_data()$megaplot_data$saved$selection_button
      inputB1$seed <- preprocessed_data()$megaplot_data$saved$seedset

      shiny::updateSliderInput(
        session,
        inputId = "thick",
        value = preprocessed_data()$megaplot_data$saved$thick.line
      )

      shiny::updateCheckboxInput(
        session,
        inputId = "inc.ev.subj",
        value = preprocessed_data()$megaplot_data$saved$inc.ev.subj
      )

      shiny::updateCheckboxInput(
        session,
        inputId = "lines_instead_symbols",
        value = preprocessed_data()$megaplot_data$saved$lines_instead_symbols
      )

      shiny::updateCheckboxInput(
        session,
        inputId = "det.xaxt",
        value = preprocessed_data()$megaplot_data$saved$det.xaxt
      )

      shiny::updateCheckboxInput(
        session,
        inputId = "incr.font",
        value = preprocessed_data()$megaplot_data$saved$incr.font
      )
    }

    if (input$selectdata == "Upload saved data") {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "methSer",
        selected = preprocessed_data()$megaplot_data$saved$methSer
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "varSeq",
        selected = preprocessed_data()$megaplot_data$saved$varSeq
      )
      shiny::updateCheckboxInput(
        session,
        inputId = "multiple_distmeasures",
        value = preprocessed_data()$megaplot_data$saved$multiple_distmeasures
      )
    }


    shiny::req(preprocessed_data()$megaplot_data)
    inputIMP$name <- preprocessed_data()$megaplot_data$name

    choices <- unlist(data_w_event_and_group_information()$event.lev,
                      use.names = FALSE)
    tmp <- data_w_event_and_group_information()$event.lev.n
    choices.lab <- rep(data_w_event_and_group_information()$event,
                       tmp)
    choices.sym <- rep('glyphicon-cloud',
                       length(choices.lab))
    choices.col <- paste('color:',
                         unlist(data_w_event_and_group_information()$col.ev[data_w_event_and_group_information()$event],
                                use.names = FALSE))
    choices <- paste0(choices.lab, ' = ', choices)

    selected <- choices

    if (input$selectdata == "Upload saved data") {
      selected <- preprocessed_data()$megaplot_data$saved$event.levels
    }

    shinyWidgets::updatePickerInput(
      session,
      inputId = "event.levels",
      choices = choices,
      selected = selected,
      choicesOpt = list(
        `icon` = choices.sym,
        `style` = choices.col
      )
    )

    shiny::req(data_w_event_and_group_information())
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)

    if (input$selectdata == "Upload saved data") {
      shiny::updateSliderInput(
        session,
        inputId = "range",
        min = min1,
        max = max1,
        value = c(
          preprocessed_data()$megaplot_data$saved$zoom.range[1],
          preprocessed_data()$megaplot_data$saved$zoom.range[2]
        )
      )
    } else {
      shiny::updateSliderInput(
        session,
        inputId = "range",
        min = min1,
        max = max1,
        value = c(min1, max1)
      )
    }

    shiny::req(data_w_event_and_group_information())
    # min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    # max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    if (input$selectdata == "Upload saved data") {
      #1
      if(!is.null(preprocessed_data()$megaplot_data$saved$reference_line_1)) {
        shiny::updateCheckboxInput(
          session,
          inputId = 'reference_line_1',
          label = 'Add reference line',
          value =  preprocessed_data()$megaplot_data$saved$reference_line_1
        )
        shiny::updateNumericInput(
          inputId = "reference_line_1_value",
          label = "Reference line (1)",
          value = preprocessed_data()$megaplot_data$saved$reference_line_1_value
        )
      }
      #2
      if(!is.null(preprocessed_data()$megaplot_data$saved$reference_line_2)) {
        shiny::updateCheckboxInput(
          session,
          inputId = 'reference_line_2',
          label = 'Add reference line',
          value =  preprocessed_data()$megaplot_data$saved$reference_line_2
        )
        shiny::updateNumericInput(
          inputId = "reference_line_2_value",
          label = "Reference line (2)",
          value = preprocessed_data()$megaplot_data$saved$reference_line_2_value
        )
      }
      #3
      if(!is.null(preprocessed_data()$megaplot_data$saved$reference_line_3)) {
        shiny::updateCheckboxInput(
          session,
          inputId = 'reference_line_3',
          label = 'Add reference line',
          value =  preprocessed_data()$megaplot_data$saved$reference_line_3
        )
        shiny::updateNumericInput(
          inputId = "reference_line_3_value",
          label = "Reference line (3)",
          value = preprocessed_data()$megaplot_data$saved$reference_line_3_value
        )
      }

      if(!is.null(preprocessed_data()$megaplot_data$saved$reference_line)) {
        shiny::updateCheckboxInput(
          session,
          inputId = 'reference_line_1',
          label = 'Add reference line',
          value =  TRUE
        )
        shiny::updateNumericInput(
          inputId = "reference_line_1_value",
          label = "Reference line (1)",
          value = preprocessed_data()$megaplot_data$saved$reference_line[1]
        )
        shiny::updateCheckboxInput(
          session,
          inputId = 'reference_line_2',
          label = 'Add reference line',
          value =  TRUE
        )
        shiny::updateNumericInput(
          inputId = "reference_line_2_value",
          label = "Reference line (2)",
          value = preprocessed_data()$megaplot_data$saved$reference_line[2]
        )
      }
      # sel_min1 <- preprocessed_data()$megaplot_data$saved$reference_line[1]
      # sel_max1 <- preprocessed_data()$megaplot_data$saved$reference_line[2]
      # shiny::updateSliderInput(
      #   session,
      #   inputId = "refdate",
      #   min = min1,
      #   max = max1,
      #   value = c(sel_min1, sel_max1)
      # )
    } #else {
    #   shiny::updateSliderInput(
    #     session,
    #     inputId = "refdate",
    #     min = min1,
    #     max = max1
    #   )
    # }

    if (input$selectdata == 'Upload saved data') {
      selected <- preprocessed_data()$megaplot_data$saved$select.grouping
      choices <- preprocessed_data()$megaplot_data$saved$choiceGroup

      shiny::updateSelectizeInput(
        session,
        inputId = "select.grouping",
        selected = selected,
        choices = choices
      )
    }
    inputIMP$select.grouping <- NULL

    if (input$selectdata == 'Upload saved data') {
      selected <- preprocessed_data()$megaplot_data$saved$select.sorting
      choices <- preprocessed_data()$megaplot_data$saved$choiceSort

      shinyWidgets::updatePickerInput(
        session,
        inputId = "select.sorting",
        selected = selected,
        choices = choices
      )

    }
    newtab <- switch(input$sidebarmenu, "dashboard")
    shinydashboard::updateTabItems(session, "sidebarmenu", newtab)
  })



  shiny::observeEvent(input$seq.button, {
    collectSeq$varSeq <- input$varSeq
    collectSeq$methSer <- input$methSer
  })

  # create raw data table as UI output (based on data_grouped_and_sorted() user selection)
  rawData <- shiny::reactive({
    tmp <- shiny::req(data_w_ai_information())
    tmp[[req(input$select.raw)]]
  })

  output$rawtable = DT::renderDT({
    rd <- shiny::req(rawData())
    rd <-
      rd[, setdiff(colnames(rd), c('Group_ID', 'Group_ID_char', 'subject'))]
    rd <- DT::datatable(rd, rownames = FALSE)
    rd
  })

  shiny::observeEvent(input$btn1, {
    start_val1 <- input$range[1]
    start_val2 <- input$range[2]
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    diff_val <- start_val2 - start_val1

    if (start_val1 > min1) {
      if ((start_val1 - diff_val) < min1) {
        shiny::updateSliderInput(session,
                                 inputId = "range",
                                 value = c(min1, min1 + diff_val))
      } else {
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = c(start_val1, start_val2) - diff_val
        )
      }
    }
  })

  shiny::observeEvent(input$btn2, {
    start_val1 <- input$range[1]
    start_val2 <- input$range[2]
    min1 <- min(data_w_event_and_group_information()$A$megaplots_selected_start_time)
    max1 <- max(data_w_event_and_group_information()$A$megaplots_selected_end_time)
    diff_val <- start_val2 - start_val1

    if (start_val2 < max1) {
      if ((start_val2 + diff_val) > max1) {
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = c(max1 - diff_val, max1 + diff_val)
        )
      } else {
        shiny::updateSliderInput(
          session,
          inputId = "range",
          value = c(start_val1, start_val2) + diff_val
        )
      }
    }
  })

  # SUMMARY STATS
  output$sumtable <- DT::renderDataTable({
    shiny::req(data_grouped_and_sorted())
    mel.sum <- summary_statistics$val

    if (!is.null(mel.sum)) {
      if (nrow(mel.sum) > 0) {
        mel.sum <- DT::datatable(mel.sum, rownames = FALSE, filter = 'top')
        mel.sum <- DT::formatRound(mel.sum, columns = 'MEAN COUNT PER SUBJECT', digits = 1)
        mel.sum
      }
    }
  })

 # observer for creating a summary table
  shiny::observeEvent(c(data_grouped_and_sorted(),input$import.button), {


    ds_ <- data_grouped_and_sorted()$B[, c('megaplots_selected_subjectid', 'megaplots_selected_event_time', data_grouped_and_sorted()$event.total, "Group_ID_char")] %>%
      dplyr::mutate_if(is.factor, as.character)
    mel <- reshape2::melt(
      ds_,
      id.vars = c('megaplots_selected_subjectid', 'megaplots_selected_event_time', 'Group_ID_char'),
      variable.name = 'EVENT',
      value.name = 'LEVEL'
    )

    #calculate number of rows/subjects in each group
    nr_subjects <-
      table(unique(mel[c("Group_ID_char", "megaplots_selected_subjectid")])$Group_ID_char)
    #calculate number of events in each group and for each event level
    if (nrow(mel) > 0 & !all(is.na(mel$LEVEL))) {
      mel.sum <- stats::aggregate(
        megaplots_selected_event_time ~ EVENT + LEVEL + Group_ID_char,
         data = mel,
         FUN = length
      )
      mel.sum$Number_events_per_group <- mel.sum$megaplots_selected_event_time
      #calculate event average per subject in each group and event level
      #(depending on number of subjects)
      if (length(nr_subjects) == 1) {
        mel.sum$megaplots_selected_event_time <- apply(mel.sum, 1, function(x) {
            as.numeric(x[4]) / nr_subjects
        })
      }
      if (length(nr_subjects) > 1) {
        mel.sum$megaplots_selected_event_time <-
          apply(mel.sum, 1, function(x) {
            as.numeric(x[4]) / nr_subjects[x[3]]
        })
      }
      #define a complete "grid" of variable combinations for merging and
      #replace the missing values with zero(s)
      complete.grid <- unique(merge(mel.sum[,c("EVENT","LEVEL")], unique(mel.sum$Group_ID_char)))
      colnames(complete.grid)[colnames(complete.grid) == 'y'] <-
        'Group_ID_char'
      mel.sum <- mel.sum %>%
        dplyr::right_join(complete.grid, by =c("EVENT","LEVEL","Group_ID_char")) %>%
        replace(is.na(.), 0)

      mel.sum <- mel.sum %>% dplyr::full_join(
        data.frame("Group_ID_char" = names(nr_subjects),"NUMBER" = as.numeric(nr_subjects)),
      by = "Group_ID_char")

      #renaming for datatable in summary tab
      colnames(mel.sum)[colnames(mel.sum) == 'megaplots_selected_event_time'] <-
        'MEAN COUNT PER SUBJECT'
      colnames(mel.sum)[colnames(mel.sum) == 'Number_events_per_group'] <-
        'NUMBER EVENTS BY GROUP'
      colnames(mel.sum)[colnames(mel.sum) == 'Group_ID_char'] <-
        'GROUP BY'
      roundID <- which(colnames(mel.sum) == 'MEAN COUNT PER SUBJECT')
      summary_statistics$val <- mel.sum
    } else {
      summary_statistics$val <- NULL
    }
  })

  #reactive value for the summary statistic data table (default NULL)
  summary_statistics <- shiny::reactiveValues(val = NULL)
  summary_statistics_text <- shiny::reactiveValues(val = NULL)

  #observer to create the summary statistics text for "Summary Panel"
  shiny::observe({
    shiny::req(summary_statistics$val)
    tmp <- summary_statistics$val
    if (!is.null(tmp) & !is.null(input$select.events) & !is.null(input$event.levels)) {

      #filter data and colors for selected events & selected levels
      tmp <- tmp[tmp$EVENT %in% input$select.events,]

        data_w_plot_info_col <- data_w_plot_info()$col.ev[names(data_w_plot_info()$col.ev) %in% input$select.events]

      summary_color <- unlist(data_w_plot_info_col)[names(unlist(data_w_plot_info_col)) %in% gsub(" = ", ".",input$event.levels)]

      #filter for selected levels
      tmp$filter <- paste0(tmp$EVENT," = ", tmp$LEVEL)
      tmp <- tmp[tmp$filter %in% input$event.levels,]

      tmp <- tmp %>%
        dplyr::arrange(`GROUP BY`,match(filter, input$event.levels))
      if (!is.null(tmp)) {
        text <- c()

        if (all(tmp$`GROUP BY` == "")) {
          group_index <- ""
        } else {
          group_index <- rev(levels(data_w_plot_info()$B$Group_ID_char))
        }
        #generate event summary text for each group and each selected
        #event and event level
        for(group in group_index) {
          text <- c(
            text,
            paste0(
              "<u style = 'color: white;'>",
              ifelse(group != "", group, "Overall"),
              " (N = ", unique(tmp[tmp$`GROUP BY` == group,]$NUMBER) ,")",
              "</u>",
              paste("<p style = 'color: white;'> <mark style = 'color: white; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black; background:",summary_color,";'>",
                tmp[tmp$`GROUP BY` == group,]$LEVEL,
                "</mark>  Events = ",
                tmp[tmp$`GROUP BY` == group,]$`NUMBER EVENTS BY GROUP`,
                " (Avg. by row: ",
                round(tmp[tmp$`GROUP BY` == group,]$`MEAN COUNT PER SUBJECT`, 2),
                ")",
                "</p>",
                collapse = ""
              )
            )
          )
        }
        summary_statistics_text$val <- text
      }

    } else {
      summary_statistics_text$val <- NULL
    }
  })

  # color module 1
  color_pal1 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(input$import.button), {
    shiny::req(input$import.button)
    if (!is.null(input$select.ev.lev1)) {
      if (input$selectdata == "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)
          custom_colour <- mod_colour_palette_server(
            "color_palette1",
            event = shiny::reactive({
              input$select.ev1
            }),
            level = shiny::reactive({
              input$select.ev.lev1
            }),
            colors = shiny::reactive({
              saved_file$color.pal1
            })
          )

          shiny::observe({
            color_pal1$val <- custom_colour$colors()
          })
        }
       } else if (input$selectdata == "Use demo data") {
      custom_colour <- mod_colour_palette_server(
          "color_palette1",
          event = shiny::reactive({
            input$select.ev1
          }),
          level = shiny::reactive({
            input$select.ev.lev1
          }),
          colors = shiny::reactive({c("seagreen1", "#ffff99", "#ff7f00", "#5CACEE", "#FDBF6F", "#1F78B4", "#6A3D9A", "#FF7F00") })
        )

        shiny::observe({
          color_pal1$val <- custom_colour$colors()
        })
      } else {
        custom_colour <- mod_colour_palette_server(
          "color_palette1",
          event = shiny::reactive({
            input$select.ev1
          }),
          level = shiny::reactive({
            input$select.ev.lev1
          }),
          colors = shiny::reactive({
            colChoice[["color palette 1"]]$col
          })
        )
        shiny::observe({
          color_pal1$val <- custom_colour$colors()
        })
      }
    }
  })

  # color module 2
  color_pal2 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(input$import.button), {
    if (!is.null(input$select.ev.lev2)) {
      if (input$selectdata == "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)
          custom_colour2 <- mod_colour_palette_server(
            "color_palette2",
            event = shiny::reactive({
              input$select.ev2
            }),
            level = shiny::reactive({
              input$select.ev.lev2
            }),
            colors = shiny::reactive({
              saved_file$color.pal2
            })
          )
          observe({
            color_pal2$val <- custom_colour2$colors()
          })
        }
      } else {
        custom_colour2 <- mod_colour_palette_server(
          "color_palette2",
          event = shiny::reactive({
            input$select.ev2
          }),
          level = shiny::reactive({
            input$select.ev.lev2
          }),
          colors = shiny::reactive({
            colChoice[["color palette 2"]]$col
          })
        )
        observe({
          color_pal2$val <- custom_colour2$colors()
        })
      }

    }
  })

  # color module 3
  color_pal3 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(input$import.button), {
    if (!is.null(input$select.ev.lev3)) {
      if (input$selectdata == "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)

          custom_colour3 <- mod_colour_palette_server(
            "color_palette3",
            event = shiny::reactive({
              input$select.ev3
            }),
            level = shiny::reactive({
              input$select.ev.lev3
            }),
            colors = shiny::reactive({
              saved_file$color.pal3
            })
          )
          observe({
            color_pal3$val <- custom_colour3$colors()
          })
        }
      } else {
        custom_colour3 <- mod_colour_palette_server(
          "color_palette3",
          event = shiny::reactive({
            input$select.ev3
          }),
          level = shiny::reactive({
            input$select.ev.lev3
          }),
          colors = shiny::reactive({
            colChoice[["color palette 3"]]$col
          })
        )
        observe({
          color_pal3$val <- custom_colour3$colors()
        })
      }

    }
  })


  # color module 4
  color_pal4 <- shiny::reactiveValues(val = NULL)
  shiny::observeEvent(c(input$import.button), {
    if (!is.null(input$select.ev.lev4)) {
      if (input$selectdata == "Upload saved data") {
        if (!is.null(input$setting_file$datapath)) {
          saved_file <- readRDS(input$setting_file$datapath)

          custom_colour4 <- mod_colour_palette_server(
            "color_palette4",
            event = shiny::reactive({
              input$select.ev4
            }),
            level = shiny::reactive({
              input$select.ev.lev4
            }),
            colors = shiny::reactive({
              saved_file$color.pal4
            })
          )
          observe({
            color_pal4$val <- custom_colour4$colors()
          })
        }
      } else {
        custom_colour4 <- mod_colour_palette_server(
          "color_palette4",
          event = shiny::reactive({
            input$select.ev4
          }),
          level = shiny::reactive({
            input$select.ev.lev4
          }),
          colors = shiny::reactive({
            colChoice[["color palette 4"]]$col
          })
        )
        observe({
          color_pal4$val <- custom_colour4$colors()
        })
      }

    } else {
      color_pal4$val <- NULL
    }
  })

  output$indivtable <- DT::renderDataTable({
    shiny::req(data_grouped_and_sorted())
    ds_ <- data_grouped_and_sorted()$B[, c('megaplots_selected_subjectid', 'megaplots_selected_event_time', data_grouped_and_sorted()$event.total)] %>%
      dplyr::mutate_if(is.factor, as.character)
    B.long <-
      na.exclude(
        reshape2::melt(
        ds_,
        id.vars = c('megaplots_selected_subjectid', 'megaplots_selected_event_time'),
        variable.name = 'EVENT',
        value.name = 'LEVEL'
        )
      )

    # calculate the counts as COUNT
    if (nrow(B.long) > 0) {
      B.sum <-
        stats::aggregate(megaplots_selected_event_time ~ megaplots_selected_subjectid + EVENT + LEVEL,
                         data = B.long,
                         FUN = length)
      colnames(B.sum)[colnames(B.sum) == 'megaplots_selected_event_time'] <- 'COUNT'
      B.sum <- dplyr::arrange(B.sum, megaplots_selected_subjectid, EVENT, LEVEL)

      B.sum <- DT::datatable(B.sum, rownames = FALSE)

      B.sum
    }
  })


  output$save_setting2 <- shiny::downloadHandler(
    filename = function() {
      paste("Megaplot_Session", gsub(":", "-", Sys.time()), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(settings(), file)
    }
  )

  settings <- shiny::reactive({
    shiny::req(preprocessed_data()$megaplot_data)
    param <- list(
      A = preprocessed_data()$megaplot_data$A,
      B = preprocessed_data()$megaplot_data$B,
      A_data_w_plot_info = data_w_plot_info()$A,
      B_data_w_plot_info = data_w_plot_info()$B,
      name = preprocessed_data()$name,
      select.ev1 = input$select.ev1,
      select.ev2 = input$select.ev2,
      select.ev3 = input$select.ev3,
      select.ev4 = input$select.ev4,
      select.ev.lev1 = input$select.ev.lev1,
      select.ev.lev2 = input$select.ev.lev2,
      select.ev.lev3 = input$select.ev.lev3,
      select.ev.lev4 = input$select.ev.lev4,
      # megaplot
      select.events = input$select.events,
      select.grouping = input$select.grouping,
      select.sorting = input$select.sorting,
      reference_line_1 = input$reference_line_1,
      reference_line_1_value = input$reference_line_1_value,
      reference_line_2 = input$reference_line_2,
      reference_line_2_value = input$reference_line_2_value,
      reference_line_3 = input$reference_line_3,
      reference_line_3_value = input$reference_line_3_value,
      event.levels = input$event.levels,
      select.subsetting = input$select.subsetting,
      zoom.range = input$range,
      # displayed subjects
      random = input$random,
      selection_button = input$selection_button,
      startsubj = input$startsubj,
      seedset = input$seedset,
      specific_ids = input$specific_ids,
      # settings
      select.device = input$select.device,
      select.col = input$select.col,
      thick.line = input$thick,
      inc.ev.subj = input$inc.ev.subj,
      lines_instead_symbols = input$lines_instead_symbols,
      det.xaxt = input$det.xaxt,
      incr.font = input$incr.font,
      # color options...
      color.pal1 = color_pal1$val,
      color.pal2 = color_pal2$val,
      color.pal3 = color_pal3$val,
      color.pal4 = color_pal4$val,
      choiceGroup = choiceGroup(),
      choiceSort = choiceSort(),
      A_da = data_w_ai_information()$A,
      group_da = data_w_ai_information()$group,
      sequencing = data_w_ai_information()$A$'SEQUENCING',
      group = data_w_ai_information()$group,
      group.lev = data_w_ai_information()$group.lev,
      # seriation
      varSeq = input$varSeq,
      methSer = input$methSer,
      group_seriation = input$select.grouping,
      multiple_distmeasures = input$multiple_distmeasures,
      parameters_seriation = input_seriation(),
      #plot labels
      y_label = input$y_axis_label,
      x_label = input$x_axis_label
    )
    param
  })

  shiny::callModule(mod_data_specification_server, "data_spec")

  # information-pdf for Seriation
  shiny::observeEvent(input$link_to_pdf_view, {
    js <-
      'window.open("www/Megaplots_Seriation_Manual.pdf", "_blank", "height=700,width=1120");'
    shinyjs::runjs(js)
  })

  # call seriation module
  input_seriation <- seriation_server(
    "parametersModule",
    reactive(input$varSeq),
    reactive(input$multiple_distmeasures),
    reactive(preprocessed_data()$megaplot_data$saved$parameters_seriation),
    reactive(input$selectdata)
  )
}
