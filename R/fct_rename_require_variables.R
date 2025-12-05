rename_require_variables <- function(
    megaplot_data,
    selected_subjectid,
    selected_start_time,
    selected_end_time,
    selected_event,
    selected_event_group,
    selected_event_time,
    selected_event_time_end
) {

  if ( !is.null(selected_subjectid) &
       !is.null(selected_start_time) &
       !is.null(selected_end_time) &
       !is.null(selected_event) &
       !is.null(selected_event_group) &
       !is.null(selected_event_time) &
       !is.null(selected_event_time_end)
  ) {
    megaplot_data <- megaplot_data %>%
      dplyr::mutate(
        megaplots_selected_subjectid = !!rlang::sym(selected_subjectid),
        megaplots_selected_start_time = !!rlang::sym(selected_start_time),
        megaplots_selected_end_time = !!rlang::sym(selected_end_time),
        megaplots_selected_event = !!rlang::sym(selected_event),
        megaplots_selected_event_group = !!rlang::sym(selected_event_group),
        megaplots_selected_event_time = !!rlang::sym(selected_event_time),
        megaplots_selected_event_time_end = !!rlang::sym(selected_event_time_end)
      )

    return(megaplot_data)
  } else {

    return(NULL)
  }
}
