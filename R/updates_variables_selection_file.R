#' Updates SelectInputs for variables selection of data set A and B (RData).
#'
#' @param file fileInput object uploaded within app.
#' @param session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @keywords internal

updates_variables_selection_file <- function(
  file,
  session
) {
  A <- NULL
  B <- NULL
  if (
    utils::tail(strsplit(file$datapath, ".", fixed = TRUE)[[1]], n = 1) %in% c("RData","rdata","Rdata")
  ) {
  load(file$datapath)

    A2 <- numeric_to_integer(A)
    B2 <- numeric_to_integer(B)

    integers_A <- names(which(unlist(lapply(A2,is.numeric))))


    shiny::updateSelectInput(
      session,
      inputId = "A_subjectid_rdata",
      choices = integers_A,
      selected = integers_A[1]
    )
    shiny::updateSelectInput(
      session,
      inputId = "A_start_time_rdata",
      choices = integers_A,
      selected = integers_A[2]
    )
     shiny::updateSelectInput(
      session,
      inputId = "A_end_time_rdata",
      choices = integers_A,
      selected = integers_A[3]
    )
    integers_B <- names(which(unlist(lapply(B2,is.numeric))))
    shiny::updateSelectInput(
      session,
      inputId = "B_subjectid_rdata",
      choices = integers_B,
      selected = integers_B[1]
    )
    shiny::updateSelectInput(
      session,
      inputId = "B_event_time_rdata",
      choices = integers_B,
      selected = integers_B[2]
    )
  }
}
