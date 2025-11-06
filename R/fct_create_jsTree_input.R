#' create_jsTree_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_jsTree_input <- function(data) {

    splitted_data <- split(data, data$event_group)
    rownames(splitted_data) <- NULL
    names(splitted_data) <- NULL
    list_output <- lapply(
      splitted_data,
      function(x) {
        list(
          text = unique(x$event_group),
          type = "root",
          children = list(NULL)
        )
      }
    )
    for(i in 1:length(list_output)) {
      filtered_splitted_data <- splitted_data[[i]]
      rownames(filtered_splitted_data) <- NULL
      list_output[[i]]$children <- apply(filtered_splitted_data, 1, function(y){
        list(text = as.character(y["event"]), type = "child")
      })
    }
  return(list_output)
}
