#' check_megaplot_data_variables
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
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

  init_val <- TRUE

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

  if (!is.data.frame(megaplot_data)) {
    init_val <- FALSE
  }
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
