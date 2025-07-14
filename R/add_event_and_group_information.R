#' Add Event Information and Group Information to Megaplot Data
#'
#' @param data list with megaplot data
#' @param summary_stats list with megaplot data summary
#' @param event1 character with first event variable
#' @param event2 character with second event variable
#' @param event3 character with third event variable
#' @param event4 character with fourth event variable
#' @param updated_event1 character with uploaded event 1
#' @param updated_event2 character with uploaded event 2
#' @param updated_event3 character with uploaded event 3
#' @param updated_event4 character with uploaded event 4
#' @param data_selection character of data upload method ("Use demo data"/"Data upload")
#'

add_event_and_group_information <- function(
    data,
    summary_stats,
    event1,
    event2,
    event3,
    event4,
    updated_event1,
    updated_event2,
    updated_event3,
    updated_event4,
    data_selection
    ) {

    A <- data$megaplot_data$A
    B <- data$megaplot_data$B

    if (!is.null(A) & !is.null(B)) {

    # read all character values from data set A as char_A
    char_A <- names(which(sapply(A, is.character)))

    # read all character values from data set B as char_B
    char_B <- names(which(sapply(B, is.character)))
    # read all numeric values from data set B as nume_B
    nume_B <- names(which(sapply(B, is.numeric)))

    # selected events
    char_B.sel <-unique(base::intersect(c(updated_event1, updated_event2, updated_event3, updated_event4),char_B))

    data_group_event_list <- list('A' = A, 'B' = B, 'group' = char_A, 'event' = char_B.sel, 'event.total' = char_B)

    # ensure that group variables and events are factors
    list_order <- list(
      event1, event2, event3, event4
    )

    if(length(data_group_event_list$group) > 0) {
      for (i in 1:length(data_group_event_list$group))
        data_group_event_list$A[, data_group_event_list$group[i]] <- factor(data_group_event_list$A[, data_group_event_list$group[i]])
    }
    if(length(data_group_event_list$event) > 0) {
    for (i in 1:length(data_group_event_list$event))
      data_group_event_list$B[, data_group_event_list$event[i]] <-
      factor(data_group_event_list$B[, data_group_event_list$event[i]], levels = list_order[[i]])
    }
    # save group levels
    lev.gr <- list()
    if(length(data_group_event_list$group) > 0) {
    for (i in 1:length(data_group_event_list$group))
      lev.gr[[i]] <- levels(data_group_event_list$A[, data_group_event_list$group[i]])
    }

    names(lev.gr) <- data_group_event_list$group
    data_group_event_list[['group.lev']] <- lev.gr

    # save event levels
    lev.ev <- list()
    lev.ev.n <- list()
    for (i in 1:length(data_group_event_list$event)) {
      lev.ev[[i]] <- levels(data_group_event_list$B[, data_group_event_list$event[i]])
      lev.ev.n[[i]] <- nlevels(data_group_event_list$B[, data_group_event_list$event[i]])
    }
    names(lev.ev) <- data_group_event_list$event
    names(lev.ev.n) <- data_group_event_list$event
    data_group_event_list[['event.lev']] <- lev.ev
    data_group_event_list[['event.lev.n']] <- lev.ev.n

    for(i in 1:length(data_group_event_list$event.lev)){
      for(j in 1:length(data_group_event_list$event.lev[[i]])){
        data_group_event_list$A <- add_ttfe(
          A = data_group_event_list$A,
          B = data_group_event_list$B,
          event = names(data_group_event_list$event.lev[i]),
          level = data_group_event_list$event.lev[[i]][j]
        )
      }
    }

    # read all numeric values from data set A as nume_A
    nume_A <- names(which(sapply(data_group_event_list$A, is.numeric)))

    # set plotting symbols
    sym.ev <- c(15, 18, 16, 4)[1:length(data_group_event_list$event)]

    ai.tab <- base::merge(summary_stats$'total', summary_stats$'detail')

    ai.tab <- base::merge(ai.tab, data_group_event_list$A[, nume_A])
    rownames(ai.tab) <- ai.tab$'megaplots_selected_subjectid'
    ai.tab <- subset(ai.tab, select = -c(megaplots_selected_subjectid))

    data_group_event_list$'ai.tab' <- ai.tab
    data_group_event_list$'ai.initselect' <- colnames(summary_stats$'detail')[-1]

    # if (data_selection == "Upload saved data") {
    #   data_group_event_list$A$'SEQUENCING' <- data$megaplot_data$saved$sequencing
    # } else {
      data_group_event_list$A$'SEQUENCING' <- data_group_event_list$A$megaplots_selected_subjectid
    # }
    # reactive return
    c(
      data_group_event_list,
      list(
        sym.ev = sym.ev,
        char_A = char_A,
        char_B = char_B,
        nume_A = nume_A,
        nume_B = nume_B
      )
    )
    }
}
