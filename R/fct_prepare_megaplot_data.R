#' prepare_megaplot_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prepare_megaplot_data <- function(
    megaplot_data_raw,
    uploaded_data_w_ids,
    select_sorting,
    select_grouping
  ) {

  # all_colors <- color_data$all
  megaplot_data_arranged <- dplyr::arrange(megaplot_data_raw, !!!rlang::syms(select_grouping), !!rlang::sym(select_sorting))

  #create numeric subjectid variable for plotting
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::left_join(
      data.frame(
        subjectid = unique(megaplot_data_arranged$subjectid),
        subject_index = seq_along(unique(megaplot_data_arranged$subjectid))),
      by = "subjectid"
    ) #%>%
    #dplyr::mutate(subjectid_n = subject_index)

  #join event identifier
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::left_join(
      uploaded_data_w_ids,
      by = c("event_group","event")
    )

  #create column group_index
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::group_by(!!!rlang::syms(select_grouping)) %>%
    dplyr::mutate(group_index = dplyr::cur_group_id()) #%>% dplyr::ungroup()


  ##
  megaplot_data_raw  <- megaplot_data_raw %>%
    dplyr::mutate(subjectid_n = subject_index + (group_index-1)*10)

  #return value
  return(megaplot_data_raw)
}
