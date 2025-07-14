#' Summary Statistics Module - User Interface Part
#'
#' @param id Shiny Session id
#'
#' @return No return
#'
#' @noRd
#' @keywords internal
#

summary_statistics_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    # shinydashboard::box(
    #   width = NULL,
    #   solidHeader = TRUE,
    #   collapsible = FALSE,
      DT::DTOutput(ns('sumtable')),
      shiny::br(),
      shiny::br(),
      DT::DTOutput(ns('indivtable'))
    # )
  )
}

#' Summary Statistics Module  - Server Part
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param data_w_plot_info list with data and megaplot information from server
#' @param data_grouped_and_sorted list with sorted data information from server
#' @param import.button reactive actionButton information
#' @param select.events list with selected events
#' @param event.levels list with event.levels
#' @param select_color character vector with color information
#'
#' @return List with preprocessed data and upload panel inputs
#'
#' @noRd
#' @keywords internal
#'

summary_statistics_server <- function(
    input,
    output,
    session,
    data_w_plot_info,
    data_grouped_and_sorted,
    import.button,
    select.events,
    event.levels,
    select_color
  ) {

  ns <- session$ns

  output$sumtable <- DT::renderDataTable({
    shiny::req(data_grouped_and_sorted())

    mel.sum <- summary_statistics$val

    if (!is.null(mel.sum)) {
      if (nrow(mel.sum) > 0) {

        mel.sum <- DT::datatable(
          mel.sum,
          options = list(
            initComplete = DT::JS(
            "function(settings, json) {",
            paste0(
              "$(this.api().table().header()).css({'background-color': '",
                   select_color()['plot.bg2'],
                   "', 'color': '",
                   select_color()['plot.id'],
                   "'});"
              ),"}"
            ),
            dom = 'Brtip',
            class = 'cell-border stripe'
          ),
          rownames = FALSE, filter = 'top'
        )

        col.tabFont <- select_color()['plot.id']
        mel.sum<- DT::formatStyle(
          table = mel.sum,
          columns = 1:(ncol(summary_statistics$val) + 1),
          target = "cell",
          color = col.tabFont,
          backgroundColor = select_color()['plot.bg'],
          border = paste0('.5px solid ', select_color()['plot.bg'])
        )

        mel.sum <- DT::formatRound(mel.sum, columns = 'MEAN COUNT PER SUBJECT', digits = 1)
        mel.sum
      }
    }
  })

   # observer for creating a summary table
  shiny::observeEvent(c(data_grouped_and_sorted(),import.button()), {
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

    shiny::observe({

    shiny::req(summary_statistics$val)
    tmp <- summary_statistics$val
    if (!is.null(tmp) & !is.null(select.events()) & !is.null(event.levels())) {

      #filter data and colors for selected events & selected levels
      tmp <- tmp[tmp$EVENT %in% select.events(),]

      data_w_plot_info_col <- data_w_plot_info()$col.ev[names(data_w_plot_info()$col.ev) %in% select.events()]

      summary_color <- unlist(data_w_plot_info_col)[names(unlist(data_w_plot_info_col)) %in% gsub(" = ", ".",event.levels())]

      #filter for selected levels
      tmp$filter <- paste0(tmp$EVENT," = ", tmp$LEVEL)
      tmp <- tmp[tmp$filter %in% event.levels(),]

      tmp <- tmp %>%
        dplyr::arrange(`GROUP BY`,match(filter, event.levels()))
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
              "<u style = 'color: white; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;'>",
              ifelse(group != "", group, "Overall"),
              " (N = ", unique(tmp[tmp$`GROUP BY` == group,]$NUMBER) ,")",
              "</u>",
              paste("<p style ='color: white; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;'> <mark style = 'color: white; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black; background:",summary_color,";'>",
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

  #### Datatable ####
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
      length_B.sum <- ncol(B.sum)
      colnames(B.sum)[colnames(B.sum) == 'megaplots_selected_event_time'] <- 'COUNT'
      B.sum <- dplyr::arrange(B.sum, megaplots_selected_subjectid, EVENT, LEVEL)

      B.sum <- DT::datatable(B.sum,
          options = list(
            initComplete = DT::JS(
            "function(settings, json) {",
            paste0(
              "$(this.api().table().header()).css({'background-color': '",
                   select_color()['plot.bg2'],
                   "', 'color': '",
                   select_color()['plot.id'],
                   "'});"
              ),"}"
            ),
            dom = 'Brtip',
            class = 'cell-border stripe'
          ), rownames = FALSE)

        col.tabFont <- select_color()['plot.id']
        B.sum <- DT::formatStyle(
          table = B.sum,
          columns = 1:(length_B.sum + 1),
          target = "cell",
          color = col.tabFont,
          backgroundColor = select_color()['plot.bg'],
          border = paste0('.5px solid ', select_color()['plot.bg'])
        )


      B.sum
    }
  })


  return(list(
    summary_statistics = shiny::reactive({summary_statistics}),
    summary_statistics_text = shiny::reactive({summary_statistics_text$val})
  ))
}
