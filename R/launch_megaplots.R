#' launch_megaplots - Launches the Megaplots application
#'
#' @export
#'
#' @description
#' Starts the Megaplots application in the client's browser.
#'
#' @param ... A series of options to be used inside the app.
#'
#' @keywords megaplots
#'
#' @examples
#' \dontrun{
#' launch_megaplots()
#' }
#'
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom dplyr arrange select group_by count filter n_distinct mutate ungroup row_number
#' @importFrom DT renderDT datatable renderDataTable formatRound DTOutput
#' @importFrom golem add_resource_path bundle_resources with_golem_options
#' @importFrom graphics axis grconvertX grconvertY legend par points rect strheight strwidth text
#' @importFrom grDevices rgb
#' @importFrom plyr ddply . summarise
#' @importFrom RColorBrewer brewer.pal
#' @importFrom reshape2 melt dcast
#' @importFrom rlang syms
#' @importFrom seriation seriate permute
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @importFrom shinyjs runjs click disable useShinyjs
#' @importFrom shinyjqui orderInput updateOrderInput
#' @import shinyWidgets
#' @importFrom stats aggregate cutree as.dist dist hclust na.exclude
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_wider
#' @importFrom TraMineR seqdef seqdist alphabet
#' @importFrom utils tail read.csv globalVariables packageVersion
#' @importFrom magrittr "%>%"
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @return A shiny app

launch_megaplots <- function(host = "0.0.0.0", port = 3838, ...) {
  png(tempfile(), width = 800, height = 600)
  invisible(dev.off())
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(host = host, port = port)
    ),
    golem_opts = list(...)
  )
}
