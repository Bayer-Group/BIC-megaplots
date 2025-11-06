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
  megaplot_data,
  subjectid,
  start_time,
  end_time,
  event,
  event_group,
  event_time,
  event_time_end
) {

  #create initial logical value intit_val and set its value to TRUE.
  #If any check fails, this value will set to FALSE
  init_val <- TRUE

  # check that variables arent missing
  if (is.null(subjectid)) {
    init_val <- FALSE
  }
  if (is.null(start_time)) {
    init_val <- FALSE
  }
  if (is.null(end_time)) {
    init_val <- FALSE
  }
  if (is.null(event)) {
    init_val <- FALSE
  }
  if (is.null(event_group)) {
    init_val <- FALSE
  }
  if (is.null(event_time)) {
    init_val <- FALSE
  }
  if (is.null(event_time_end)) {
    init_val <- FALSE
  }

  #check that megaplot_data is a data frame
  if (!is.data.frame(megaplot_data)) {
    init_val <- FALSE
  }
  #check that variables have the appropriate format in data set
  if (!is.null(start_time)) {
    if (!is.numeric(megaplot_data[[start_time]])) {
      init_val <- FALSE
    }
  }
  if (!is.null(end_time)) {
    if (!is.numeric(megaplot_data[[end_time]])) {
      init_val <- FALSE
    }
  }
  if (!is.null(event)) {
    if (!is.character(megaplot_data[[event]])) {
      init_val <- FALSE
    }
  }
  if (!is.null(event_group)) {
    if (!is.character(megaplot_data[[event_group]])) {
      init_val <- FALSE
    }
  }
  if (!is.null(event_time)) {
    if (!is.numeric(megaplot_data[[event_time]])) {
      init_val <- FALSE
    }
  }
  if (!is.null(event_time_end)) {
    if (!is.numeric(megaplot_data[[event_time_end]])) {
      init_val <- FALSE
    }
  }

  return(init_val)
}
