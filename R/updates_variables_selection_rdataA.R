#' Updates SelectInputs for variables selection of data set A (rdata).
#'
#' @param file fileInput object uploaded within app.
#' @param session Internal parameters for {shiny}.
#'
#' @noRd
#' @keywords internal

updates_variables_selection_rdataA <- function(
  file,
  session
) {
  if (class(file) == "data.frame") {
    if (is.character(file$datapath)) {
      if (nchar(file$datapath) > 0 ) {
        if (utils::tail(strsplit(file$datapath, ".", fixed = TRUE)[[1]], n = 1) %in% c("RData","rdata","Rdata")) {
          A <- get(load(file$datapath))
          A2 <- numeric_to_integer(A)
          
          integers_A <- names(which(unlist(lapply(A2,is.integer))))
          
          if (shiny::isRunning()) {
            shiny::updateSelectInput(
              session,
              inputId = "A_subjectid_rdata_files",
              choices = integers_A,
              selected = integers_A[1]
            )
            
            shiny::updateSelectInput(
              session,
              inputId = "A_start_time_rdata_files",
              choices = integers_A,
              selected = integers_A[2]
            )
            
            shiny::updateSelectInput(
              session,
              inputId = "A_end_time_rdata_files",
              choices = integers_A,
              selected = integers_A[3]
            )
          }
          return(integers_A)
        } else {
          return(NULL)
        }
      } else {
        warning("datapath is of length 0 in updates_variables_selection_rdataA")
      } 
    } else {
      warning("datapath is non character in updates_variables_selection_rdataA")
    } 
  } else {
    warning("parameter 'file' needs to be of class data.frame in function updates_variables_selection_rdataA")
  }
}