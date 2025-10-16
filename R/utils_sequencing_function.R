#' Sequencing Calculation
#'
#' @description This function calculates pairwise distances between the subjects using a selected distance measure and applies a seriation method to order the subjects accordingly.
#'
#' @param da  reactive shiny list object containing data frames A and B
#' @param var variable or vector of variables which are used for the sequencing
#' @param par list of parameters for the distance measure
#' @param sermethod applied seriation method
#' @param group variable that is used for grouping
#' @param multiple_distmeasures logical, if TRUE different distance measures are used for sequencing-variables
#'
#' @return updated da object
#'
#' @importFrom seriation seriate permute
#' @importFrom stats as.dist
#' @importFrom TraMineR seqdef seqdist
#' @noRd
#' @keywords internal
sequencing_var_app <- function(
  da,
  var,
  par,
  sermethod,
  group,
  multiple_distmeasures
) {

  if (length(var) == 1) {
      par <- lapply(par, function(x) x[1])
      # Get the data into a wide format
      B_wide <- get_B_wide(da, var, par$methMissing, par)[[1]]
      par <- get_B_wide(da, var, par$methMissing, par)[[2]]

      # Get the alphabet for the sequencing object
      alphab <- get_alphab(da, B_wide, var, par$methMissing)

      # No grouping:
      if (is.null(group)) {
        # Define the sequencing object
        seq <-
          suppressMessages(TraMineR::seqdef(B_wide, 2:ncol(B_wide), labels = alphab))

        # Get the parameters for the distance function
        seqargs_all <- get_parameters(seq, par)

        # Calculate pairwise distances:
        dist <-
          suppressMessages(do.call(TraMineR::seqdist, seqargs_all))

        ddist <-
          stats::as.dist(dist) # the following needs it to be a dist object

        # Use the seriation package to compute the order
        sq <- suppressMessages(seriation::seriate(ddist, method = sermethod))
      }
      else{
        sq <- sq_grouping(da, var, par, sermethod, group, B_wide, alphab)
      }
    }

    # More than one variable
    else{
      # if all variables use the same distance measure
      if (!multiple_distmeasures) {
        par <- lapply(par, function(x)
          x[1])
        # no grouping:
        if (is.null(group)) {
          # Empty list where we will save the distance matrices
          dist_list <- list()
          for (vari in var) {
            # Get the data into a wide format
            B_wide <- get_B_wide(da, vari, par$methMissing, par)[[1]]
            par <- get_B_wide(da, vari, par$methMissing, par)[[2]]


            # Get the alphabet for the sequencing object
            alphab <- get_alphab(da, B_wide, vari, par$methMissing)

            # Define the sequencing object
            seq <-
              suppressMessages(TraMineR::seqdef(B_wide, 2:ncol(B_wide), labels = alphab))

            # Get the parameters for the distance function
            seqargs_all <- get_parameters(seq, par)

            # Calculate the pairwise distances for this variable:
            dist_list[[vari]] <-
              suppressMessages(do.call(TraMineR::seqdist, seqargs_all))

          }
          dist <- Reduce(`+`, dist_list)

          ddist <-
            stats::as.dist(dist) # the following needs it to be a dist object


          # Use the seriation package to compute the order
          sq <- suppressMessages(seriation::seriate(ddist, method = sermethod))
        }
        # with grouping:
        else{
          sq <-
            sq_grouping_var(da, var, par, sermethod, group, multiple_distmeasures)
        }
      }

      # different distance measures for the variables
      else{
        # no grouping:
        if (is.null(group)) {
          # Empty list where we will save the distance matrices
          dist_list <- list()
          for (i in 1:length(var)) {
            vari <- var[i]
            pari <- lapply(par, function(x)
              x[i])

            # Get the data into a wide format
            B_wide <-
              get_B_wide(da, vari, pari$methMissing, pari)[[1]]
            pari <- get_B_wide(da, vari, pari$methMissing, pari)[[2]]


            # Get the alphabet for the sequencing object
            alphab <- get_alphab(da, B_wide, vari, pari$methMissing)

            # Define the sequencing object
            seq <-
              suppressMessages(TraMineR::seqdef(B_wide, 2:ncol(B_wide), labels = alphab))

            # Get the parameters for the distance function
            seqargs_all <- get_parameters(seq, pari)

            # Calculate the pairwise distances for this variable:
            dist_list[[vari]] <-
              suppressMessages(do.call(TraMineR::seqdist, seqargs_all))
          }
          dist <- Reduce(`+`, dist_list)

          ddist <-
            stats::as.dist(dist) # the following needs it to be a dist object

          # Use the seriation package to compute the order
          sq <- suppressMessages(seriation::seriate(ddist, method = sermethod))
        }
        # with grouping:
        else{
          sq <-
            sq_grouping_var(da, var, par, sermethod, group, multiple_distmeasures)
        }
      }
    }

    ## Input the sequencing in the da object:
    da$A <- da$A[order(da$A$megaplots_selected_subjectid),]
    da$A <- suppressMessages(seriation::permute(da$A, order = sq, margin = 1))
    da$A$'SEQUENCING' <- 1:nrow(da$A)
    da$A <- da$A[order(da$A$megaplots_selected_subjectid),]
    da$nume_A <- c(da$nume_A, 'SEQUENCING')

    da

  }

#' Sequencing with grouping
#'
#' @description Internal function called by sequencing_var_app
#'
#' @param da  reactive shiny list object containing data frames A and B
#' @param var variable or vector of variables which are used for the sequencing
#' @param par list of parameters for the distance measure
#' @param sermethod applied seriation method
#' @param group variable that is used for grouping
#' @param B_wide data frame of the input data B in a wide format
#' @param alphab alphabet used for calculating the distance measures using seqdist
#'
#' @return Ordered sequence of the subjects
#' @noRd
#'
#' @importFrom dplyr arrange select group_by count filter n_distinct mutate ungroup row_number
#' @importFrom stats as.dist
#' @importFrom TraMineR seqdef seqdist
#'
#' @keywords internal
sq_grouping <- function(
  da,
  var,
  par,
  sermethod,
  group,
  B_wide,
  alphab) {

    method_missing <- par$methMissing
    distmeasure <- par$distmeasure
    id_seq <- data.frame("megaplots_selected_subjectid" = sort(da$A$megaplots_selected_subjectid), "sequencing" = NA)

    # one grouping variable
    if (length(group) == 1) {
      group_levels <- levels(da$A[, group])
      n <- length(group_levels)
    }
    # more than one grouping variable
    else{
      levels <- list()
      for (i in 1:length(group)) {
        levels[[group[i]]] <- levels(da$A[, group[i]])
      }
      group_levels <- expand.grid(levels)
      n <- nrow(group_levels)
    }

    # For each group level
    for (i in 1:n) {
      # Get subset within the group level
      if (length(group) == 1) {
        lev <- group_levels[i]
        ids_index <- da$A[, group] == lev
      }
      else{
        lev <- group_levels[i, ]
        ids_index <- apply(da$A[, group], 1, function(x)
          all(x == lev))
      }
      # no subjects in this group: skip
      if (sum(ids_index) == 0) {
        next
      }

      B_wide_group <-
        B_wide[B_wide$megaplots_selected_subjectid %in% da$A$megaplots_selected_subjectid[ids_index], ]
      id_seq_group <-
        data.frame("megaplots_selected_subjectid" = sort(da$A$megaplots_selected_subjectid[ids_index]),
                   "sequencing" = NA)
      # if only one subject in the group:
      if (sum(ids_index) == 1) {
        # save the order overall in the dataframe
        i <-
          sum(!is.na(id_seq$sequencing)) # number of elements that are already ordered
        id_seq_group$sequencing <- 1 + i
        id_seq$sequencing[id_seq$megaplots_selected_subjectid %in% id_seq_group$megaplots_selected_subjectid] <-
          id_seq_group$sequencing

        next
      }


      alphabg <- alphab[sort(unique(as.vector(as.matrix(B_wide_group[, -1]))) + 1)]

      #Define the sequencing object
      seq_group <-
        suppressMessages(TraMineR::seqdef(B_wide_group, 2:ncol(B_wide_group), labels = alphabg))


      # Check if all subjects have only one state:
      if (all(apply(seq_group, 2, function(x)
        dplyr::n_distinct(x) == 1))) {
        # sequencing doesn't make a difference since all subjects have the same trajectory
        id_seq_group$sequencing <- 1:nrow(id_seq_group)

      }
      else{
        # Get the parameters for the distance function
        seqargs_all_group <- get_parameters(seq_group, par)

        # Calculate pairwise distances:
        dist_group <- suppressMessages(do.call(TraMineR::seqdist, seqargs_all_group))

        ddist_group <- stats::as.dist(dist_group)

        # Use the seriation package to compute the order
        sq_group <-
          suppressMessages(seriation::seriate(ddist_group, method = sermethod))

        # save the order within the group in the dataframe id_seq_group
        id_seq_group <- suppressMessages(seriation::permute(id_seq_group, order = sq_group, margin = 1))
        id_seq_group$sequencing <- 1:nrow(id_seq_group)
        id_seq_group <- id_seq_group[order(id_seq_group$megaplots_selected_subjectid), ]
      }

      # save the order overall in the dataframe
      i <- sum(!is.na(id_seq$sequencing)) # number of elements that are already ordered
      id_seq_group$sequencing <- id_seq_group$sequencing + i
      id_seq$sequencing[id_seq$megaplots_selected_subjectid %in% id_seq_group$megaplots_selected_subjectid] <-
        id_seq_group$sequencing

    }
    sq <- order(id_seq$sequencing)
  }


#' Sequencing with grouping and multiple variables
#'
#' @description Internal function called by sequencing_var_app
#'
#' @param da  reactive shiny list object containing data frames A and B
#' @param var variable or vector of variables which are used for the sequencing
#' @param par list of parameters for the distance measure
#' @param sermethod applied seriation method
#' @param group variable that is used for grouping
#' @param multiple_distmeasures logical, if TRUE different distance measures are used for sequencing-variables
#'
#' @return Ordered sequence of the subjects
#' @noRd
#'
#' @importFrom seriation seriate permute
#' @keywords internal

sq_grouping_var <-
  function(da,
           var,
           par,
           sermethod,
           group,
           multiple_distmeasures) {
    id_seq <-
      data.frame("megaplots_selected_subjectid" = sort(da$A$megaplots_selected_subjectid),
                 "sequencing" = NA)

    # one grouping variable
    if (length(group) == 1) {
      group_levels <- levels(da$A[, group])
      n <- length(group_levels)
    }
    # more than one grouping variable
    else{
      levels <- list()
      for (i in 1:length(group)) {
        levels[[group[i]]] <- levels(da$A[, group[i]])
      }
      group_levels <- expand.grid(levels)
      n <- nrow(group_levels)
    }


    # For each group level
    for (i in 1:n) {
      # Get subset within the group level
      if (length(group) == 1) {
        lev <- group_levels[i]
        ids_index <- da$A[, group] == lev
      }
      else{
        lev <- group_levels[i, ]
        ids_index <- apply(da$A[, group], 1, function(x)
          all(x == lev))
      }

      # no subjects in this group: skip
      if (sum(ids_index) == 0) {
        next
      }

      # Get da in the right format
      da_group <- da
      da_group$A <- da$A[ids_index, ]
      da_group$B <- da$B[da$B$megaplots_selected_subjectid %in% da_group$A$megaplots_selected_subjectid, ]

      id_seq_group <-
        data.frame("megaplots_selected_subjectid" = sort(da$A$megaplots_selected_subjectid[ids_index]),
                   "sequencing" = NA)

      # if only one subject in the group:
      if (sum(ids_index) == 1) {
        # save the order overall in the dataframe
        i <-
          sum(!is.na(id_seq$sequencing)) # number of elements that are already ordered
        id_seq_group$sequencing <- 1 + i
        id_seq$sequencing[id_seq$megaplots_selected_subjectid %in% id_seq_group$megaplots_selected_subjectid] <-
          id_seq_group$sequencing

        next
      }

      dist_list <- list()
      for (vari in var) {
        if (multiple_distmeasures) {
          i <- which(var == vari)
          method_missing <- par$methMissing[i]
          pari <- lapply(par, function(x)
            x[i])
        }
        else{
          method_missing <- par$methMissing
          pari <- par
        }

        # Get the data into a wide format
        B_wide <-
          get_B_wide(da_group, vari, method_missing, pari)[[1]]
        pari <- get_B_wide(da_group, vari, method_missing, pari)[[2]]


        # Get the alphabet for the sequencing object
        alphab <- get_alphab(da_group, B_wide, vari, pari$methMissing)
        alphabg <-
          alphab[sort(unique(as.vector(as.matrix(B_wide[, -1]))) + 1)]

        # Define the sequencing object
        seq <-
          suppressMessages(TraMineR::seqdef(B_wide, 2:ncol(B_wide), labels = alphabg))

        # Get the parameters for the distance function
        seqargs_all <- get_parameters(seq, pari)

        # Check if all subjects have only one state:
        if (all(apply(seq, 2, function(x)
          dplyr::n_distinct(x) == 1))) {
          # sequencing doesn't make a difference since all subjects have the same trajectory
          dist_list[[vari]] <-
            matrix(rep(0, nrow(B_wide) ^ 2),
                   nrow = nrow(B_wide),
                   ncol = nrow(B_wide))

        }
        else{
          # Calculate the pairwise distances for this variable:
          dist_list[[vari]] <-
            suppressMessages(do.call(TraMineR::seqdist, seqargs_all))
        }
      }
      dist <- Reduce(`+`, dist_list)

      ddist_group <- stats::as.dist(dist)


      # Use the seriation package to compute the order
      sq_group <- suppressMessages(seriation::seriate(ddist_group, method = sermethod))

      # save the order within the group in the dataframe id_seq_group
      id_seq_group <-
        suppressMessages(seriation::permute(id_seq_group, order = sq_group, margin = 1))
      id_seq_group$sequencing <- 1:nrow(id_seq_group)
      id_seq_group <- id_seq_group[order(id_seq_group$megaplots_selected_subjectid), ]

      # save the order overall in the dataframe
      i <-
        sum(!is.na(id_seq$sequencing)) # number of elements that are already ordered
      id_seq_group$sequencing <- id_seq_group$sequencing + i
      id_seq$sequencing[id_seq$megaplots_selected_subjectid %in% id_seq_group$megaplots_selected_subjectid] <-
        id_seq_group$sequencing

    }
    sq <- order(id_seq$sequencing)
    return(sq)
  }





#' Getting input-parameters for the seqdist-function
#'
#' @description This internal function returns a named list with all necessary input arguments for the seqdist function.
#'
#' @param seq state sequence object
#' @param par list of parameters for the distance measure
#'
#' @return named list as an input for the seqdist function
#'
#' @importFrom TraMineR alphabet
#'
#' @noRd
#' @keywords internal
get_parameters <- function(seq, par) {
  seqargs_all <- list(
    "seqdata" = seq,
    "method" = par$distmeasure,
    "with.missing" = TRUE
  )
  distmeasure <- par$distmeasure

  if (distmeasure %in% c('OM', 'OMloc', 'OMslen', 'OMspell', 'OMstran', 'HAM', 'TWED')) {
    if (par$sm == "ORDINAL") {
      if (any(seq == "*")) {
        with.missing <- TRUE
      }
      else{
        with.missing <- FALSE
      }
      l <-
        length(TraMineR::alphabet(seq, with.missing = with.missing))
      sub_matrix <- matrix(
        rep(0, l ^ 2),
        nrow = l,
        ncol = l,
        dimnames = list(
          TraMineR::alphabet(seq, with.missing = with.missing),
          TraMineR::alphabet(seq, with.missing = with.missing)
        )
      )
      for (i in 1:l) {
        for (j in 1:l) {
          if (i != j) {
            dimn <-
              c(dimnames(sub_matrix)[[1]][i], dimnames(sub_matrix)[[2]][j])
            if (any(dimn == "*")) {
              sub_matrix[i, j] <- mean(1:(l - 1))
            }
            else{
              sub_matrix[i, j] <- abs(i - j)
            }
          }
        }
      }
      seqargs_all[["sm"]] <- sub_matrix
    }
    else{
      seqargs_all["sm"] <- par$sm
    }
  }
  if (distmeasure == "DHD") {
    if (par$smDHD == "ORDINAL") {
      if (any(seq == "*")) {
        with.missing <- TRUE
      }
      else{
        with.missing <- FALSE
      }
      l <-
        length(TraMineR::alphabet(seq, with.missing = with.missing))
      sub_matrix <- matrix(
        rep(0, l ^ 2),
        nrow = l,
        ncol = l,
        dimnames = list(
          TraMineR::alphabet(seq, with.missing = with.missing),
          TraMineR::alphabet(seq, with.missing = with.missing)
        )
      )
      for (i in 1:l) {
        for (j in 1:l) {
          if (i != j) {
            dimn <-
              c(dimnames(sub_matrix)[[1]][i], dimnames(sub_matrix)[[2]][j])
            if (any(dimn == "*")) {
              sub_matrix[i, j] <- mean(1:(l - 1))
            }
            else{
              sub_matrix[i, j] <- abs(i - j)
            }
          }
        }
      }
      seqargs_all[["sm"]] <- sub_matrix
    }
    else{
      seqargs_all["sm"] <- par$smDHD
    }

  }

  if (distmeasure %in% c(
    'OM',
    'OMloc',
    'OMslen',
    'OMspell',
    'OMstran',
    'HAM',
    'DHD',
    'LCS',
    'LCS',
    'RLCP',
    'TWED'
  )) {
    seqargs_all["norm"] <- par$norm
  }
  if (distmeasure %in% c('CHI2', 'EUCLID')) {
    seqargs_all["norm"] <- par$norm2
  }
  if (distmeasure %in% c('OM', 'OMslen', 'OMspell', 'OMstran')) {
    if (par$indel == "numeric value") {
      seqargs_all["indel"] <- par$indel_numeric
    }
    else{
      seqargs_all["indel"] <- par$indel
    }

  }
  if (distmeasure %in% c('OMloc', 'OMspell')) {
    seqargs_all["expcost"] <- as.numeric(par$expcost)
  }
  if (distmeasure == "OMloc") {
    seqargs_all["context"] <- as.numeric(par$context)
  }
  if (distmeasure == "OMslen") {
    seqargs_all["link"] <- par$link
    seqargs_all["h"] <- par$h_OMslen
  }
  if (distmeasure == "OMstran") {
    seqargs_all["transindel"] <- par$transindel
    seqargs_all["otto"] <- par$otto
    seqargs_all["previous"] <- as.logical(par$previous)
    seqargs_all["add.column"] <- as.logical(par$add.column)

  }
  if (distmeasure %in% c("OMloc", "NMSMST", "SVRspell")) {
    seqargs_all["tpow"] <- par$tpow
  }
  if (distmeasure %in% c("EUCLID", "CHI2")) {
    seqargs_all["step"] <- as.numeric(par$step)
    seqargs_all["overlap"] <- as.logical(par$overlap)
  }
  if (distmeasure %in% "CHI2") {
    seqargs_all["weighted"] <- as.logical(par$weighted)
  }


  return(seqargs_all)

}


#' Alphabet Generation for state sequence object
#'
#'
#' @param da reactive shiny list object containing data frames A and B
#' @param B_wide data frame of the input data B in a wide format
#' @param var variable or vector of variables which are used for the sequencing
#' @param method_missing method how to handle missing values, either "new state" or "last observed state"
#'
#' @return alphabet used for calculating the distance measures using seqdist
#'
#' @noRd
#' @keywords internal
get_alphab <- function(da, B_wide, var, method_missing) {
  if (is.factor(da$B[, var])) {
    alphab <- levels(da$B[, var])
    if (method_missing == "new state" & any(is.na(da$B))) {
      alphab <- c("*", alphab)
    }
  }
  else{
    alphab <-  unique(unlist(B_wide[grep(var, colnames(B_wide))]))
    alphab <- alphab[!is.na(alphab)]
    alphab <- as.character(alphab)
  }

  return(alphab)
}



utils::globalVariables(c("megaplots_selected_subjectid", "megaplots_selected_event_time", "row_num"))

#' Conversion of data set B into a wide format
#'
#' @param da reactive shiny list object containing data frames A and B
#' @param var variable or vector of variables which are used for the sequencing
#' @param method_missing method how to handle missing values, either "new state" or "last observed state"
#' @param par list of parameters for the distance measure
#'
#' @return list with two elements: data set B in a wide format, updated parameters list
#'
#' @importFrom dplyr arrange select group_by count filter n_distinct mutate ungroup row_number
#' @importFrom tidyr pivot_wider
#' @noRd
#' @keywords internal
get_B_wide <- function(da, var, method_missing, par) {
  # Are there non-unqiue subjectid and ev_day combinations?
  # If yes, select only the first subjectid - ev_day combination
  if (any(duplicated(da$B[, c("megaplots_selected_subjectid", "megaplots_selected_event_time")]))) {
    B <- da$B %>%
      dplyr::right_join(da$A %>% dplyr::select(megaplots_selected_subjectid), by = "megaplots_selected_subjectid") %>%
      dplyr::group_by(megaplots_selected_subjectid, megaplots_selected_event_time) %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      dplyr::filter(row_num == 1) %>%
      dplyr::ungroup()
  }
  else{
    B <- da$B
  }

  # If the dataset has missing values/days where nothing happens:
  if (any(is.na(B))) {
    # Get the dataframe B in a wide format with a column for every day
    B_wide <- B %>%
      dplyr::select(megaplots_selected_subjectid, megaplots_selected_event_time, dplyr::all_of(var)) %>% # select only the columns of interest
      tidyr::pivot_wider(names_from = megaplots_selected_event_time, values_from = dplyr::all_of(var)) %>% # pivot the data
      dplyr::rename_with( ~ paste(var, "_", .x, sep = ""),-megaplots_selected_subjectid)

    # Sort the columns:
    numeric_suffixes <-
      as.numeric(sub(".*[^0-9](\\d+)$", "\\1", colnames(B_wide)[-1]))
    # Identify negative suffixes and adjust their sign
    is_negative <- grepl("-", colnames(B_wide)[-1])
    numeric_suffixes[is_negative] <- -numeric_suffixes[is_negative]
    # add the columns for the days where nothing happens
    range_days <- min(numeric_suffixes):max(numeric_suffixes)
    add_col_id <- range_days[!(range_days %in% numeric_suffixes)]
    if (length(add_col_id) != 0) {
      add_col_dat <-
        as.data.frame(matrix(NA, nrow = nrow(B_wide), ncol = length(add_col_id)))
      colnames(add_col_dat) <- paste(var, "_", add_col_id, sep = "")
      B_wide <- cbind(B_wide, add_col_dat)
      numeric_suffixes <- c(numeric_suffixes, add_col_id)
    }

    # Sort the column names based on the numeric suffixes
    sorted_column_names <-
      colnames(B_wide)[-1][order(numeric_suffixes)]
    # Reorder the columns in the dataframe
    B_wide <- B_wide[, c("megaplots_selected_subjectid", sorted_column_names)]

    # Has the dataset subjects, that do not have any event?
    if (!all(da$A$megaplots_selected_subjectid %in% B_wide$megaplots_selected_subjectid)) {
      # add the rows to the B_wide data:
      subj <- da$A$megaplots_selected_subjectid[!(da$A$megaplots_selected_subjectid %in% B_wide$megaplots_selected_subjectid)]
      add_B <- as.data.frame(matrix(NA, nrow = length(subj), ncol = ncol(B_wide)))
      colnames(add_B) <- colnames(B_wide)
      add_B$megaplots_selected_subjectid <- subj
      B_wide <- rbind(B_wide, add_B)
    }

    # Check for OMloc and empty sequences
    if (par$distmeasure == "OMloc" &
        any(apply(B_wide[, -1], 1, function(x)
          all(is.na(x))))) {
      method_missing <- "new state"
      par$methMissing <- "new state"
    }

    # Two different missing methods:
    if (method_missing == "last observed state") {
      # Method 1: if a subject does not have a state at a time x, the last state from
      #           before is taken -> all subjects have always a state (starting from start point)
      for (i in 3:ncol(B_wide)) {
        B_wide[, i] <-
          ifelse(is.na(B_wide[, i]), B_wide[, i - 1, drop = TRUE], B_wide[, i, drop = TRUE])
      }
      B_wide[, 2] <- as.numeric(B_wide[, 2, drop = TRUE])
    }
    else{
      # Method 2: set all NAs to a new state
      B_wide <- B_wide %>%
        dplyr::mutate(dplyr::across(colnames(B_wide)[grep(var, colnames(B_wide))], as.numeric))

      B_wide[is.na(B_wide)] <- 0
    }

  }
  # If the dataset is without missing values/days where nothing happens:
  else{
    # Get the dataframe B in a wide format with a column for every day
    B_wide <- B %>%
      dplyr::select(megaplots_selected_subjectid, megaplots_selected_event_time, dplyr::all_of(var)) %>% # select only the columns of interest
      tidyr::pivot_wider(names_from = megaplots_selected_event_time, values_from = dplyr::all_of(var)) %>% # pivot the data
      dplyr::rename_with( ~ paste(var, "_", .x, sep = ""),-megaplots_selected_subjectid)
  }


  B_wide <- B_wide %>%
    dplyr::mutate_if(is.factor, as.numeric)

  return(list(B_wide, par))
}
