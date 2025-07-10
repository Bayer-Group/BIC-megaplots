#' Add Sorting Information to Data in Megaplots
#'
#' @param data_frame list with megaplot data
#' @param select_subsetting Select subsets
#' @param randet character value ('deterministic'/'random')
#' @param seed integer with seed number for random selection
#' @param nshow integer with number of subjects displayed
#' @param start integer with row selection for subsetting
#' @param specific_ids character vector with specific subject identifier which should be displayed
#' @param event_levels character vector with event levels
#' @param select_sorting character with sorting variable selection
#' @param select_grouping character vector with grouping variable(s) selection
#' @param select_events character vector with event variable selection
#'

add_sorting_information <- function(
  data_frame,
  select_subsetting,
  randet,
  seed,
  nshow,
  start,
  specific_ids,
  event_levels,
  select_sorting,
  select_grouping,
  select_events
){

  ds <- data_frame
  # subset selection
  # ...by group levels
  tmp.sub <- select_subsetting

  if (!is.null(tmp.sub)) {
    mt.sub <-
      data.frame(
        'VAR' = sapply(
          strsplit(tmp.sub, ' = '),
          FUN = function(x)
            x[1]
        ),
        'LEV' = sapply(
          strsplit(tmp.sub, ' = '),
          FUN = function(x)
            x[2]
        ),
        stringsAsFactors = FALSE
      )

  } else {
    mt.sub <- NULL
  }

    # delete group levels that were not selected
    if (length(ds$group.lev) > 0) {
      for (i in 1:length(ds$group.lev)) {
        # level vector of group i
        tmp <- ds$group.lev[[ds$group[i]]]
        tmp.sel <- mt.sub[mt.sub$VAR == ds$group[i], 'LEV']
        # store only the selected levels in 'ds'
        ds$group.lev[[ds$group[i]]] <- tmp[tmp %in% tmp.sel]
        # delete groups that were not selected from data
        ds$A <- ds$A[ds$A[, ds$group[i]] %in% ds$group.lev[[ds$group[i]]],]
        ds$A[, ds$group[i]] <- droplevels(ds$A[, ds$group[i]])
      }
    }

    # save total number of subjects
    ds$max_sub2 <- length(ds$A$megaplots_selected_subjectid)

    # ...by number of displayed subjects

    # set seed value if the selection is 'random'
    if (shiny::req(randet) != 'deterministic') {
      if (!is.na(as.integer(seed))) {
        seed <- as.numeric(seed)
        set.seed(seed)
      } else {
        seed <- sample(0:1000, 1)
        set.seed(shiny::req(seed))
      }
    }

    # select a random subset based on the selected number of subjects
    if (shiny::req(randet) != 'deterministic') {
      len_selected_subjects <- length(ds$A$megaplots_selected_subjectid)
      min_selected_and_shown <-
        min(len_selected_subjects, nshow, na.rm = TRUE)
      rand <- sample(ds$A$megaplots_selected_subjectid, min_selected_and_shown)
      if (length(shiny::isolate(specific_ids[1:nshow])) > 0) {
        # check if ids are valid
        valid_specific_ids <-
          shiny::isolate(specific_ids[1:nshow])[shiny::isolate(specific_ids[1:nshow]) %in% ds$A$megaplots_selected_subjectid]
        if (length(valid_specific_ids) > 0) {
          valid_specific_ids_unique <-
            valid_specific_ids[!valid_specific_ids %in% rand]
          if (length(valid_specific_ids_unique) > 0) {
            rand[1:length(valid_specific_ids_unique)] <-
              as.numeric(valid_specific_ids_unique)
          }
        }
      }
      ds$A <- subset(ds$A, (ds$A$megaplots_selected_subjectid %in% rand))
      ds$B <- subset(ds$B, (ds$B$megaplots_selected_subjectid %in% rand))
    }

    # select a deterministic subset based on the selected number of subjects
    if (shiny::req(randet) == 'deterministic') {
      len_selected_subjects <- length(ds$A$megaplots_selected_subjectid)
      min_selected_and_shown <-
        min(len_selected_subjects, nshow, na.rm = TRUE)

      rand <-
        ds$A$megaplots_selected_subjectid[(start:(start + nshow - 1))[1:min_selected_and_shown]]
      if (length(specific_ids[1:nshow]) > 0) {
        valid_specific_ids <-
          specific_ids[1:nshow][specific_ids[1:nshow] %in% ds$A$megaplots_selected_subjectid]
        if (length(valid_specific_ids) > 0) {
          valid_specific_ids_unique <-
            as.numeric(valid_specific_ids[!valid_specific_ids %in% rand])
          if (length(valid_specific_ids_unique) > 0) {
            index <- which(!rand %in% valid_specific_ids)
            rand[(length(index) - length(valid_specific_ids_unique) + 1):length(index)] <-
              as.numeric(valid_specific_ids_unique)
          }
        }
      }
      ds$A <- subset(ds$A, (ds$A$megaplots_selected_subjectid %in% rand))
      ds$B <- subset(ds$B, (ds$B$megaplots_selected_subjectid %in% rand))
    }

    # ...selected grouping variables
    if (is.null(select_grouping)) {
      ds$group <- 'NULL'
    } else {
      ds$group <- select_grouping
    }

    # ...selected events

    ds$event <- select_events
    if (is.null(select_events)) {
      ds$event <- 'NULL'
    }

    # ...selected event levels
    if (!is.null(select_events) &
        !is.null(event_levels) & nrow(ds$A) > 0) {
      # matching table with event variables and selected levels
      tmp.ev <- shiny::req(event_levels)
      mt.ev <-
        data.frame(
          'VAR' = sapply(
            strsplit(tmp.ev, ' = '),
            FUN = function(x)
              x[1]
          ),
          'LEV' = sapply(
            strsplit(tmp.ev, ' = '),
            FUN = function(x)
              x[2]
          ),
          stringsAsFactors = FALSE
        )
      # delete event levels that were not selected

      for (i in 1:length(ds$event)) {
        # level vector of group i
        tmp <- ds$event.lev[[ds$event[i]]]
        tmp.sel <- mt.ev[mt.ev$VAR == ds$event[i], 'LEV']
        if(ds$event[i] %in% names(ds$event.lev)){
        # store only the selected levels in 'ds'
        ds$event.lev[[ds$event[i]]] <- tmp[tmp %in% tmp.sel]
        # replace levels that were not selected with NA
        ds$B[!ds$B[, ds$event[i]] %in% ds$event.lev[[ds$event[i]]], ds$event[i]] <-
          NA
        ds$B[, ds$event[i]] <- droplevels(ds$B[, ds$event[i]])
      }}
    }
    # ...sorting

    # sorting variables
    if (!is.null(select_grouping)) {
      var.sort <- c(ds$group, shiny::req(select_sorting))
    } else {
      var.sort <- shiny::req(select_sorting)
    }
    # sort data set

    if (all(var.sort %in% colnames(ds$A))) {
      ds$A <- dplyr::arrange(ds$A,!!!rlang::syms(var.sort))
    }

    # transform subjectid to factor...
    if (any(is.na(ds$A$megaplots_selected_subjectid))) {
      ds$A[is.na(ds$A$megaplots_selected_subjectid), ]$subjectid <- ""
    }
    if (any(is.na(ds$B$megaplots_selected_subjectid))) {
      ds$B[is.na(ds$B$megaplots_selected_subjectid), ]$subjectid <- ""
    }
    ds$A[, 'megaplots_selected_subjectid'] <-
      factor(ds$A[, 'megaplots_selected_subjectid'], levels = ds$A[, 'megaplots_selected_subjectid'])
    ds$B[, 'megaplots_selected_subjectid'] <-
      factor(ds$B[, 'megaplots_selected_subjectid'], levels = ds$A[, 'megaplots_selected_subjectid'])
    # create numeric id variable based on current order
    ds$A <- transform(ds$A, subject = as.numeric(megaplots_selected_subjectid))

    # create group ID variable
    if (!is.null(select_grouping)) {
      tmp.A <- droplevels(ds$A[, rev(ds$group)])
      ds$A$'Group_ID' <- as.numeric(interaction(tmp.A))
      ds$A$'Group_ID_char' <-
        droplevels(interaction(tmp.A, sep = '::'))
      # transform numeric subject ID and implement gaps between groups
      ds$A$'subject' <- ds$A$subject + (8 * (ds$A$Group_ID - 1))
    } else {
      ds$A$'Group_ID' <- rep(1, dim(ds$A)[1])
      ds$A$'Group_ID_char' <- rep('', dim(ds$A)[1])
      ds$A$'subject' <- ds$A$subject
    }

    # add new ID columns to second data set
    ds$B <- base::merge(ds$A[, c('megaplots_selected_subjectid', 'subject', 'Group_ID', 'Group_ID_char')], ds$B, by = c('megaplots_selected_subjectid'), all.y = TRUE)
    return(ds)
}
