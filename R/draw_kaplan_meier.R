draw_kaplan_meier <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select.grouping = NULL,
    select_event_kaplan_meier,
    select_strata_var = NULL,
    megaplot_color = c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
      "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
      "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
      "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
      "#fabed4", "#4263d8"
    )
) {
  grouping_vars <- select.grouping

  level <- select_event_kaplan_meier

  time_to_first_event <- megaplot_filtered_data %>%
    dplyr::select(
      subjectid,
      subjectid_n,
      event_time,
      event
    ) %>%
    dplyr::filter(event == level) %>%
    dplyr::group_by(subjectid) %>%
    dplyr::arrange(event_time) %>%
    dplyr::slice_head(n = 1)%>%
    dplyr::mutate(!!paste0("time_to_first") := event_time) %>%
    dplyr::select(subjectid, !!paste0("time_to_first"))

  megaplot_data_w_time_to_first_event <- time_to_first_event %>%
    dplyr::right_join(
      megaplot_filtered_data,
      by ="subjectid")

  megaplot_data_for_survfit <- megaplot_data_w_time_to_first_event %>%
    dplyr::mutate(
      time = dplyr::case_when(is.na(time_to_first) ~ end_time,
                              !is.na(time_to_first) ~ time_to_first),
      status = dplyr::case_when(is.na(time_to_first) ~ 1,
                                !is.na(time_to_first) ~ 2)
    )

  event_color <- megaplot_filtered_data %>% dplyr::filter(event == level) %>% dplyr::pull(event_color) %>% as.character %>% unique()


  if(is.null(select_strata_var)){
    strata <- "1"
  } else {
    strata <- select_strata_var
  }

  fit  <- survival::survfit(as.formula(paste0("survival::Surv(time,status) ~", paste(strata, collapse  = "+"))), data = megaplot_data_for_survfit)


  g <- ggsurv(
    fit,
    CI = FALSE,
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
