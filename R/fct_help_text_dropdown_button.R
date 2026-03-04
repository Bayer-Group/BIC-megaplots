#' helper function for helptext in megaplots application
#'
#' @param id character with identifier name
#' @param text character string with help text
#' @param right logical (TRUE/FALSE) if help text should appear on right side


help_text_dropdown_button <- function(id, text, right) {
  shinyWidgets::dropdownButton(
    inputId = id,
    text,
    status = "helptext_status",
    circle = TRUE,
    size = "xs",
    icon = icon("question"),
    right = right,
    inline = TRUE
  )
}
