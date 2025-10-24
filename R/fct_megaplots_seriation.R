megaplots_sequencing_functions <- function(
    final_data,
    variable,
    seriation_parameter,
    seriation_method,
    group,
    multiple_distmeasures
) {

  expanded_days_final_data <- final_data %>%
    dplyr::select(subjectid, event_time, event_time_end, event, event_group) %>%
    dplyr::filter(variable == event_group) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(event = factor(event)) %>%
    dplyr::mutate(event_numeric = as.numeric(event)) %>%
    dplyr::filter(!is.na(event)) %>%
    dplyr::mutate(
      day = list(seq(.data$event_time, .data$event_time_end, by = 1))
    ) %>%
    tidyr::unnest_longer(col = day)

  expanded_days_final_data_wide <- expanded_days_final_data %>%
    dplyr::select(-event_time, -event_time_end, -event_group, -event) %>%
    group_by(subjectid,day) %>%
    dplyr::slice(1) %>% # (!!  removes duplicated and looses information)
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = day, values_from = event_numeric)

  # Sort the columns:
  numeric_suffixes <- as.numeric(sub(".*[^0-9](\\d+)$", "\\1", colnames(expanded_days_final_data_wide)[-1]))
  # Identify negative suffixes and adjust their sign
  is_negative <- grepl("-", colnames(expanded_days_final_data_wide)[-1])
  numeric_suffixes[is_negative] <- -numeric_suffixes[is_negative]
  # add the columns for the days where nothing happens
  range_days <- min(numeric_suffixes):max(numeric_suffixes)
  add_col_id <- range_days[!(range_days %in% numeric_suffixes)]

  if (length(add_col_id) != 0) {
    add_col_dat <- as.data.frame(matrix(NA, nrow = nrow(expanded_days_final_data_wide), ncol = length(add_col_id)))
    colnames(add_col_dat) <- paste(variable, "_", add_col_id, sep = "")
    expanded_days_final_data_wide <- cbind(expanded_days_final_data_wide, add_col_dat)
    numeric_suffixes <- c(numeric_suffixes, add_col_id)
  }

  # Sort the column names based on the numeric suffixes
  sorted_column_names <-
    colnames(expanded_days_final_data_wide)[-1][order(numeric_suffixes)]
  # Reorder the columns in the dataframe
  expanded_days_final_data_wide <- expanded_days_final_data_wide[, c("subjectid", sorted_column_names)]

  ##method 2
  expanded_days_final_data_wide[is.na(expanded_days_final_data_wide)] <- 0

  if (any(expanded_days_final_data_wide == 0)){
    alphab <- c('*', levels(expanded_days_final_data$event))
  } else {
    alphab <- c(levels(expanded_days_final_data$event))
  }
  # No grouping:
  if (is.null(group)) {
    # Define the sequencing object
    seq <- suppressMessages(TraMineR::seqdef(expanded_days_final_data_wide, 2:ncol(expanded_days_final_data_wide), labels = alphab))

    # Get the parameters for the distance function
    seqargs_all <- get_parameters(seq, seriation_parameter)

    # Calculate pairwise distances:
    dist <- suppressMessages(do.call(TraMineR::seqdist, seqargs_all))

    ddist <- stats::as.dist(dist) # the following needs it to be a dist object

    # Use the seriation package to compute the order
    sq <- suppressMessages(seriation::seriate(ddist, method = seriation_method))
  }
  else{
    sq <- sq_grouping(final_data, variable, seriation_parameter, seriation_method, group, expanded_days_final_data_wide, alphab)
  }
  ## Input the sequencing in the da object:
  sequencing_order_data <- data.frame(cbind("subjectid" = expanded_days_final_data_wide$subjectid, "SEQUENCING" = unique(seriation::get_order(sq))))

  res<- final_data %>%
    dplyr::left_join(sequencing_order_data, by ="subjectid")

  return(sequencing_order_data)
}
