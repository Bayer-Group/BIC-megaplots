#' Updates SelectInputs for variables selection of data set B (rdata).
#'
#' @param file fileInput object uploaded within app.
#' @param session Internal parameters for {shiny}.
#'
#' @noRd
#' @keywords internal

updates_variables_selection_rdataB <- function(
  file,
  session
) {  
  if (class(file) == "data.frame") {
    if (is.character(file$datapath)) {
      if (nchar(file$datapath) > 0 ) {
        if (utils::tail(strsplit(file$datapath, ".", fixed = TRUE)[[1]], n = 1) %in% c("RData","rdata","Rdata")) {
          B <- get(load(file$datapath))
          B2 <- numeric_to_integer(B)
          
          integers_B <- names(which(unlist(lapply(B2,is.integer))))
          
          if (shiny::isRunning()) {
            shiny::updateSelectInput(
              session,
              inputId = "B_subjectid_rdata_files",
              choices = integers_B,
              selected = integers_B[1]
            )
            shiny::updateSelectInput(
              session,
              inputId = "B_event_time_rdata_files",
              choices = integers_B,
              selected = integers_B[2]
            )
          }
          return(integers_B)
        } else {
          return(NULL)
        }
      } else {
        warning("datapath is of length 0 in updates_variables_selection_rdataB")
      } 
    } else {
      warning("datapath is non character in updates_variables_selection_rdataB")
    } 
  } else {
    warning("parameter 'file' needs to be of class data.frame in function updates_variables_selection_rdataB")
  }
}