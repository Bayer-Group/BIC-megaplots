#' Summarise Megaplot Data for Summary Panel
#'
#' @param data list of data sets used in megaplots
#'
summarise_megaplot_data <-  function(data) {
  B.long <- na.exclude(
    reshape2::melt(
      data$megaplot_data$B,
      id.vars = c('megaplots_selected_subjectid', 'megaplots_selected_event_time'),
      variable.name = 'EVENT',
      value.name = 'LEVEL'
    )
  )

  # calculate the counts as COUNT
  B.sum1 <- stats::aggregate(megaplots_selected_event_time ~ megaplots_selected_subjectid + EVENT + LEVEL,
                             data = B.long,
                             FUN = length)
  colnames(B.sum1)[colnames(B.sum1) == 'megaplots_selected_event_time'] <- 'COUNT'
  B.sum2 <- stats::aggregate(megaplots_selected_event_time ~ megaplots_selected_subjectid + EVENT,
                             data = B.long,
                             FUN = length)
  colnames(B.sum2)[colnames(B.sum2) == 'megaplots_selected_event_time'] <- 'COUNT'
  # calculate the number of event level
  B.sum1 <- dplyr::arrange(B.sum1,
                           megaplots_selected_subjectid,
                           EVENT,
                           LEVEL)
  B.sum1$'EVENT2' <- paste0('n ', as.character(B.sum1$EVENT), ' = ', B.sum1$LEVEL)
  B.sum1 <- reshape2::dcast(B.sum1,
                            megaplots_selected_subjectid ~ EVENT2,
                            value.var = 'COUNT',
                            fill = 0)
  B.sum2 <- dplyr::arrange(B.sum2, megaplots_selected_subjectid, EVENT)
  B.sum2$'EVENT2' <-
    paste0('n ', as.character(B.sum2$EVENT), ' TOTAL')
  B.sum2 <- reshape2::dcast(B.sum2,
                            megaplots_selected_subjectid ~ EVENT2,
                            value.var = 'COUNT',
                            fill = 0)
  if (any(!data$megaplot_data$A$megaplots_selected_subjectid %in% data$megaplot_data$B$megaplots_selected_subjectid)) {
    add.id <-
      base::setdiff(data$megaplot_data$A$megaplots_selected_subjectid,
                    data$megaplot_data$B$megaplots_selected_subjectid)
    B.sum1[nrow(B.sum1) + (1:length(add.id)), 'megaplots_selected_subjectid'] <-
      add.id
    B.sum1[is.na(B.sum1)] <- 0
    B.sum2[nrow(B.sum2) + (1:length(add.id)), 'megaplots_selected_subjectid'] <-
      add.id
    B.sum2[is.na(B.sum2)] <- 0
  }

  return(list('total' = B.sum2, 'detail' = B.sum1))

}
