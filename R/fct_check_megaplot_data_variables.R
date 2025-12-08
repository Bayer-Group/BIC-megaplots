#' Check if data are valid for megaplot application
#'
#' @param megaplot_data data frame with data uploaded in megaplots application
#' @param subjectid character with variable name for subject identifier
#' @param start_time character with variable name for subject start time
#' @param end_time character with variable name for subject end time
#' @param event character with variable name for event
#' @param event_group character with variable name for event group
#' @param event_time character with variable name for subject start time
#' @param event_time_end character with variable name for subject end time
#'
#' @description A function to check if megaplot data are valid and includes all
#' required variables
#'
#' @return Logical value (TRUE if all checks are successfull)
#'
#' @noRd

check_megaplot_data_variables <- function(
  check_megaplot_data,
  check_subjectid,
  check_start_time,
  check_end_time,
  check_event,
  check_event_group,
  check_event_time,
  check_event_time_end
) {
  #create initial logical value intit_val and set its value to TRUE.
  #If any check fails, this value will set to FALSE
  init_val <- TRUE

  #check if either start/end_time or event_time/event_time_end  are available
  if (!(!is.null(check_start_time) & !is.null(check_end_time) | !is.null(check_event_time) & !is.null(check_event_time_end))) {
    init_val <- FALSE
  }

  # check that required variables arent missing (subjectid/event)
  if (is.null(check_subjectid)) {
    init_val <- FALSE
  }

  if (is.null(check_event)) {
    init_val <- FALSE
  }

  #check that megaplot_data is a data frame
  if (!is.data.frame(check_megaplot_data)) {
    init_val <- FALSE
  }

  #check that variables have the appropriate format in data set
  if (!is.null(check_start_time)) {
    if (!is.numeric(check_megaplot_data[[check_start_time]])) {
      init_val <- FALSE
    }
  }

  if (!is.null(check_end_time)) {
    if (!is.numeric(check_megaplot_data[[check_end_time]])) {
      init_val <- FALSE
    }
  }

  #check that variables have the appropriate format in data set
  if (!is.null(check_event_time)) {
    if (!is.numeric(check_megaplot_data[[check_event_time]])) {
      init_val <- FALSE
    }
  }

  if (!is.null(check_event_time_end)) {
    if (!is.numeric(check_megaplot_data[[check_event_time_end]])) {
      init_val <- FALSE
    }
  }

  return(init_val)
}
