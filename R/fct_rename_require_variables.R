#' Rename required variables based on variable selection within megaplots app
#'
#' @param megaplot_data data.frame with uploaded data set
#' @param selected_subjectid character with name of identifier variable
#' @param selected_start_time character with name of start time variable
#' @param selected_end_time character with name of end time variable
#' @param selected_event character with name of event variable
#' @param selected_event_group character with name of event group variable
#' @param selected_event_time character with name of event start time variable
#' @param selected_event_time_end character with name of event end time variable
#'
#' @return data.frame megaplot_data (function input) with additional variables:
#'         megaplots_selected_subjectid,
#'         megaplots_selected_event, megaplots_selected_event_group,
#'         megaplots_selected_event_time, megaplots_selected_event_time_end,
#'         megaplots_selected_start_time, megaplots_selected_end_time
#'
#' @noRd

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
  #check if megaplot_data is empty
  if (is.null(megaplot_data)) {
    return(NULL)
  }

  #check if variable names are available in data and variable names are not empty
  if (
    !is.null(selected_subjectid) &&
      !is.null(selected_event) &&
      selected_event %in% colnames(megaplot_data) &&
      (
        !is.null(selected_start_time) &
          !is.null(selected_end_time) | #&
          #(selected_start_time %in% colnames(megaplot_data) & selected_end_time %in% colnames(megaplot_data))
          !is.null(selected_event_time) & !is.null(selected_event_time_end) #&
        #(selected_event_time %in% colnames(megaplot_data) & selected_event_time_end %in% colnames(megaplot_data))
      )
  ) {
    # Create/Add variables: megaplots_selected_subjectid, megaplots_selected_event
    megaplot_data <- megaplot_data |>
      dplyr::mutate(
        megaplots_selected_subjectid = !!rlang::sym(selected_subjectid),
        megaplots_selected_event = as.character(!!rlang::sym(selected_event))
      )

    # Create/Add variables: megaplots_selected_start_time, megaplots_selected_end_time
    if (!is.null(selected_start_time) && !is.null(selected_end_time)) {
      if (
        is.numeric(megaplot_data[[selected_start_time]]) &&
          is.numeric(megaplot_data[[selected_end_time]])
      ) {
        megaplot_data <- megaplot_data |>
          dplyr::mutate(
            megaplots_selected_start_time = floor(
              !!rlang::sym(selected_start_time)
            ),
            megaplots_selected_end_time = floor(!!rlang::sym(selected_end_time))
          )
      } else {
        megaplot_data <- megaplot_data |>
          dplyr::mutate(
            megaplots_selected_start_time = NA,
            megaplots_selected_end_time = NA
          )
      }
    } else {
      megaplot_data <- megaplot_data |>
        dplyr::mutate(
          megaplots_selected_start_time = NA,
          megaplots_selected_end_time = NA
        )
    }

    # Create/Add variables: megaplots_selected_event_time, megaplots_selected_event_time_end
    if (!is.null(selected_event_time) && !is.null(selected_event_time_end)) {
      if (
        is.numeric(megaplot_data[[selected_event_time]]) &&
          is.numeric(megaplot_data[[selected_event_time_end]])
      ) {
        megaplot_data <- megaplot_data |>
          dplyr::mutate(
            megaplots_selected_event_time = floor(
              !!rlang::sym(selected_event_time)
            ),
            megaplots_selected_event_time_end = floor(
              !!rlang::sym(selected_event_time_end)
            )
          )
      } else {
        megaplot_data <- megaplot_data |>
          dplyr::mutate(
            megaplots_selected_event_time = NA,
            megaplots_selected_event_time_end = NA
          )
      }
    } else {
      megaplot_data <- megaplot_data |>
        dplyr::mutate(
          megaplots_selected_event_time = NA,
          megaplots_selected_event_time_end = NA
        )
    }

    # Create/Add variable: megaplots_selected_event_group
    if (!is.null(selected_event_group)) {
      megaplot_data <- megaplot_data |>
        dplyr::mutate(
          megaplots_selected_event_group = !!rlang::sym(selected_event_group),
        )
    } else {
      megaplot_data <- megaplot_data |>
        dplyr::mutate(
          megaplots_selected_event_group = as.character(
            !!rlang::sym(selected_event)
          ),
        )
    }
    return(megaplot_data)
  } else {
    return(NULL)
  }
}
