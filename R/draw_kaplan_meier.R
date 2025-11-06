#' Draws Kaplan Meier plots with time to first event
#'
#' @param megaplot_prepared_data data.frame with subject information used for the subject lines
#' @param megaplot_filtered_data data.frame with selected events for the event lines
#' @param select_grouping character vector with grouping variable names
#' @param select_event_kaplan_meier character with event variable name
#' @param select_strata_var character vector with stratification variables
#'
#' @return
#' @export
#'
#' @examples
draw_kaplan_meier <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select_grouping = NULL,
    select_event_kaplan_meier,
    select_strata_var = NULL
) {

  grouping_vars <- select_grouping

  level <- select_event_kaplan_meier

  time_to_first_event <- megaplot_filtered_data %>%
    dplyr::select(
      tidyselect::all_of(c(
      "subjectid",
      "subjectid_n",
      "event_time",
      "unique_event"
    ))) %>%
    dplyr::filter(.data$unique_event == level) %>%
    dplyr::group_by(.data$subjectid) %>%
    dplyr::arrange(.data$event_time) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(!!paste0("time_to_first") := .data$event_time) %>%
    dplyr::select(tidyselect::all_of(c("subjectid", "time_to_first")))

  megaplot_data_w_time_to_first_event <- megaplot_prepared_data %>%
    dplyr::left_join(
      time_to_first_event,
      by ="subjectid"
    )

  megaplot_data_for_survfit <- megaplot_data_w_time_to_first_event %>%
    dplyr::mutate(
      time = dplyr::case_when(is.na(.data$time_to_first) ~ .data$end_time,
                              !is.na(.data$time_to_first) ~ .data$time_to_first),
      status = dplyr::case_when(is.na(.data$time_to_first) ~ 1,
                                !is.na(.data$time_to_first) ~ 2)
    )

  event_color <- megaplot_filtered_data %>%
    dplyr::filter(.data$unique_event == level) %>%
    dplyr::pull(.data$event_color) %>%
    as.character %>% unique()


  if(is.null(select_strata_var)){
    strata <- "1"
  } else {
    strata <- select_strata_var
  }

  fit  <- survival::survfit(as.formula(paste0("survival::Surv(time,status) ~", paste(strata, collapse  = "+"))), data = megaplot_data_for_survfit)


  g <- ggsurv(
    fit,
    CI = TRUE,
    cens.col = c("#FFFFFF50"),
    surv.col = rep(event_color, ifelse(length(fit$strata)>0,length(fit$strata),1))
  ) + ggplot2::theme_dark() + ggplot2::theme(axis.title = ggplot2::element_text(color= "white"))

  plotly::ggplotly(g) %>% plotly::layout(
    plot_bgcolor = "#404A4E",
    paper_bgcolor ='#404A4E',
    legend = list(
      bgcolor = "#404A4E",
      font = list(color="white")
    ),
    font = list(family = "Agency FB", color = "#FFFFFF"),
    yaxis = list(color = "white")
  )
}
