draw_event_summary <- function(
    megaplot_prepared_data = megaplot_prepared_data(),
    megaplot_filtered_data = megaplot_filtered_data(),
    select.grouping = NULL,
    megaplot_color = c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
      "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
      "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
      "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
      "#fabed4", "#4263d8"
    )
) {
  # megaplot_prepared_data <- megaplot_prepared_data()
  # megaplot_filtered_data <- megaplot_filtered_data()

  if(!is.null(select.grouping)) {
    grouping_vars <- select.grouping

    number_group_levels <-  max(megaplot_filtered_data$group_index)

    # megaplot_filtered_data <- megaplot_filtered_data %>%
    #   dplyr::right_join(group_ids, by = grouping_vars)
  } else {
    number_group_levels <- 1
    # megaplot_filtered_data <- megaplot_filtered_data %>%
    #   dplyr::mutate(group_id = 1)
  }

  figure_list <- list()
  max_y_range <- c()
  for(k in 1:number_group_levels) {

    megaplot_filtered_data_group <- megaplot_filtered_data %>%
      dplyr::filter(group_index == k) %>%
      dplyr::arrange(event_group)


    megaplot_data_splitted_by_event <- split(megaplot_filtered_data_group, megaplot_filtered_data_group$event)

    time_vector <-  min(c(megaplot_filtered_data$start_time, megaplot_filtered_data$event_time)) : max(c(megaplot_filtered_data$end_time, megaplot_filtered_data$event_time_end))

    df <- data.frame(day = time_vector)
    for(i in 1:length(megaplot_data_splitted_by_event)) {
      numbers <- rep(0,length(time_vector))
      for(j in 1:nrow(megaplot_data_splitted_by_event[[i]])) {
        megaplot_data_splitted_by_event[[i]][j,]$event_time : megaplot_data_splitted_by_event[[i]][j,]$event_time_end
        numbers[which(time_vector %in% megaplot_data_splitted_by_event[[i]][j,]$event_time : megaplot_data_splitted_by_event[[i]][j,]$event_time_end)] <- numbers[which(time_vector %in% megaplot_data_splitted_by_event[[i]][j,]$event_time : megaplot_data_splitted_by_event[[i]][j,]$event_time_end)]+1
      }
      name <- (paste0(names(megaplot_data_splitted_by_event)[i]))
      df <- cbind(df,  name = numbers)
      names(df)[which(names(df) == "name")] <- name

    }

    max_y_range <- max(max_y_range, df %>% dplyr::select(-day)%>%max())

    fig <- plotly::plot_ly(data = df, x = ~ day)

    for(i in 2:ncol(df)) {
      fig <- fig %>%
        plotly::add_lines(
          #showlegend = ifelse(k == 1, TRUE, FALSE),
          y = df[,i],
          color = I(megaplot_filtered_data %>% dplyr::filter(event == names(df)[i]) %>% dplyr::pull(event_color) %>% unique()),
          line = list(shape = "linear"),
          name = names(df)[i],
          # legendgroup = names(df)[i],
          legendgroup = megaplot_filtered_data %>% dplyr::filter(event == names(df)[i]) %>% dplyr::pull(event_group) %>% unique(),

          # legendgroup = ~ event_group,
          legendgrouptitle = list(text = ~ " ")
        )
    }

    fig <- fig %>%
      plotly::layout(
        plot_bgcolor = "#404A4E",
        paper_bgcolor ='#404A4E',
        xaxis = list(
          color='#FFFFFF',
          title = "Study Day",
          zeroline = FALSE
        ),
        yaxis = list(
          color='#FFFFFF',
          showgrid = FALSE,
          title ="Event frequency",
          zeroline = FALSE,
          autotick = FALSE,
          showticklabels = FALSE
        ),

        font = list(family = "Agency FB", color = "#FFFFFF"),
        barmode = "overlay"
      )
    figure_list[[k]] <- fig
  }
  for(k in 1:number_group_levels) {
    figure_list[[k]] <- figure_list[[k]] %>%
      plotly::layout(yaxis = list(range = c(0,max_y_range)))
  }
  plotly::subplot(rev(figure_list), shareY = TRUE, nrows = number_group_levels)
}
