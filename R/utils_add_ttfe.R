#' Add column 'time to first'-event to data set A
#'
#' @param A  data frame A from megaplots app
#' @param B  data frame B from megaplots app
#' @param event character with event name
#' @param level character with event level name
#'
#' @return updated data frame object
#'
#' @noRd
#' @keywords internal


add_ttfe <- function(
  A,
  B,
  event,
  level){

  if (!identical(level,character(0))) {


  A_new <-  A %>%
    dplyr::left_join(B %>%
      dplyr::select(
        megaplots_selected_subjectid,
        megaplots_selected_event_time,
        !!rlang::sym(event) #event
      ) %>%
      dplyr::filter(!is.na(event), !!rlang::sym(event)  == level) %>%
      dplyr::group_by(megaplots_selected_subjectid) %>%
      dplyr::slice_head(n = 1)%>%
      dplyr::mutate(!!paste0("time_to_first_",gsub(" ", "",event),"_",gsub(" ", "",level)) := megaplots_selected_event_time)%>%
      dplyr::select(megaplots_selected_subjectid, !!paste0("time_to_first_",gsub(" ", "",event),"_",gsub(" ", "",level))),
      by = "megaplots_selected_subjectid"
    )
  } else {
    A_new <- A
  }
  return(A_new)
}
