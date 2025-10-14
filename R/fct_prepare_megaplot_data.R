#' Prepare megaplot_data for plotting by adding necessary variables
#'
#' @param megaplot_data_raw data frame with unprepared uploaded mega plot data
#' @param uploaded_data_w_ids data frame with prepared data done by function 'create_unique_event_identifier'
#' @param select_sorting character with sorting variable
#' @param select_grouping  character vector with grouping variable (or NULL if no grouping is selected)
#'
#' @description The purpose of this function is to create variables 'subject_index' & 'group_index' used for
#' arranging subjects and add variables from data set uploaded_data_w_ids for color & jitter information
#'
#' @return data frame megaplot_data_raw with added variables
#'
#' @noRd

prepare_megaplot_data <- function(
    megaplot_data_raw,
    uploaded_data_w_ids,
    select_sorting,
    select_grouping
  ) {

  # create arranged dataset 'megaplot_data_arranged'
  # to create a "subject_index" variable in next step
  megaplot_data_arranged <- dplyr::arrange(
    megaplot_data_raw,
    !!!rlang::syms(select_grouping),
    !!rlang::sym(select_sorting)
  )

  # create and merge variable subject_index to dataset megaplot_data_raw
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::left_join(
      data.frame(
        subjectid = unique(megaplot_data_arranged$subjectid),
        subject_index = seq_along(unique(megaplot_data_arranged$subjectid))
      ),
      by = "subjectid"
    )

  # merge raw data set and prepared dataset
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::left_join(
      uploaded_data_w_ids,
      by = c("event_group","event")
    )

  # create column group_index
  megaplot_data_raw <- megaplot_data_raw %>%
    dplyr::group_by(!!!rlang::syms(select_grouping)) %>%
    dplyr::mutate(group_index = dplyr::cur_group_id()) #%>% dplyr::ungroup()


  # add a custom space of 10 empty lines (empty subjectid_n) to distinguish groups in mega plots
  megaplot_data_raw  <- megaplot_data_raw %>%
    dplyr::mutate(subjectid_n = subject_index + (group_index - 1) * 10)

  # return value
  return(megaplot_data_raw)
}
