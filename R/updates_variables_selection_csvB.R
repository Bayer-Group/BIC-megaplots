#' Updates SelectInputs for variables selection of data set B (csv).
#'
#' @param file Data.frame with 1 obs. of 4 variables created by fileInput within app.
#' @param session Internal parameters for {shiny}.
#' @param csvB_sep separator character in read.csv function
#' @param csvB_quote quoting character in read.csv function
#' @param csvB_dec character used for decimal points in read.csv function
#'
#' @noRd
#' @keywords internal

updates_variables_selection_csvB <- function(
  file,
  session,
  csvB_sep,
  csvB_quote,
  csvB_dec
) {
  if (class(file) == "data.frame") {
    if (is.character(file$datapath)) {
      if (nchar(file$datapath) > 0 ) {
        if (!utils::tail(strsplit(file$datapath, ".", fixed = TRUE)[[1]], n = 1) != "csv") {
          B <- read.csv(
            file = file$datapath,
            header = TRUE,
            na.strings = c('NA', '.', '', 'N/A'),
            sep = csvB_sep,
            quote = csvB_quote,
            dec = csvB_dec,
            stringsAsFactors = FALSE
          )
          
          B2 <- numeric_to_integer(B)
          
          integers_B <- names(which(unlist(lapply(B2,is.integer))))
          
          if (shiny::isRunning()) {
            shiny::updateSelectInput(
              session,
              inputId = "B_subjectid_csv",
              choices = integers_B,
              selected = integers_B[1]
            )
            
            shiny::updateSelectInput(
              session,
              inputId = "B_event_time_csv",
              choices = integers_B,
              selected = integers_B[2]
            )
          }
          return(integers_B)
        } else {
          return(NULL)
        }
      } else {
        warning("datapath is of length 0 in updates_variables_selection_csvB")
      } 
    } else {
      warning("datapath is non character in updates_variables_selection_csvB")
    } 
  } else {
    warning("parameter 'file' needs to be of class data.frame in function updates_variables_selection_csvB")
  }
}