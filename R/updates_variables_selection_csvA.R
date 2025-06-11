#' Updates SelectInputs for variables selection of data set A (csv).
#'
#' @param file Data.frame with 1 obs. of 4 variables created by fileInput within app.
#' @param session Internal parameters for {shiny}.
#' @param csvA_sep Separator character in read.csv function
#' @param csvA_quote Quoting character in read.csv function
#' @param csvA_dec Character used for decimal points in read.csv function
#'
#' @noRd
#' @keywords internal

updates_variables_selection_csvA <- function(
  file,
  session,
  csvA_sep,
  csvA_quote,
  csvA_dec
) {
  if (inherits(file, "data.frame")) {
    if (is.character(file$datapath)) {
      if (nchar(file$datapath) > 0 ) {
        if (!utils::tail(strsplit(file$datapath, ".", fixed = TRUE)[[1]], n = 1) != "csv") {
          A <- read.csv(
            file = file$datapath,
            header = TRUE,
            na.strings = c('NA', '.', '', 'N/A'),
            sep = csvA_sep,
            quote = csvA_quote,
            dec = csvA_dec,
            stringsAsFactors = FALSE
          )

          A2 <- numeric_to_integer(A)

          integers_A <- names(which(unlist(lapply(A2,is.numeric))))

          if (shiny::isRunning()) {
            shiny::updateSelectInput(
              session,
              inputId = "A_subjectid_csv",
              choices = integers_A,
              selected = integers_A[1]
            )
            shiny::updateSelectInput(
              session,
              inputId = "A_start_time_csv",
              choices = integers_A,
              selected = integers_A[2]
            )
            shiny::updateSelectInput(
              session,
              inputId = "A_end_time_csv",
              choices = integers_A,
              selected = integers_A[3]
            )
          }
          return(integers_A)
        } else {
          return(NULL)
        }
      } else {
        warning("datapath is of length 0 in updates_variables_selection_csvA")
      }
    } else {
      warning("datapath is non character in updates_variables_selection_csvA")
    }
  } else {
    warning("parameter 'file' needs to be of class data.frame in function updates_variables_selection_csvA")
  }
}
