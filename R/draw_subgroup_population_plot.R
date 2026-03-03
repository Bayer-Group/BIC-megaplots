# draw_subgroup_population_plot <- function(
#     megaplot_prepared_data = megaplot_prepared_data(),
#     select_grouping = NULL,
#     select_subgroups
# ) {
#
#   if (is.null(select_subgroups)) {return(NULL)}
#   total_summary_data <- c()
#   figure_list <- list()
#
#   for (subgroup in select_subgroups) {
#
#     subgroup_data <- megaplot_prepared_data |>
#       dplyr::select(all_of(subgroup)) |>
#       dplyr::count(!!rlang::sym(subgroup),name="count") |>
#       dplyr::rename(subgroup_level = !!rlang::sym(subgroup)) |>
#       dplyr::mutate(subgroup = subgroup) |>
#       dplyr::mutate(percentage = count / sum(count)) |>
#       dplyr::mutate(subgroup_level_id = dplyr::row_number()) |>
#       dplyr::mutate()
#
#       # dplyr::group_by(subgroup_level) |>
#       # dplyr::rowwise() |>
#       # dplyr::mutate(subgroup_level_id = dplyr::row_number())
#       # dplyr::mutate(subgroup_level_id = dplyr::cur_group_id()) |>
#       # dplyr::ungroup()
#
#     subgroup_data <-  subgroup_data |>
#       dplyr::mutate(max_subgroup_levels = max(subgroup_data$subgroup_level_id))
#
#     subgroup_data <- subgroup_data |>
#       dplyr::rowwise() |>
#       dplyr::mutate(
#         subgroup_level_color = color_func2(.data$subgroup_level_id, 1, max(.data$max_subgroup_levels))
#       ) |>
#       dplyr::mutate(
#         percentage_label = dplyr::case_when(
#           percentage < 0.1 ~ "",
#           percentage >= 0.1 ~ paste0(round(percentage * 100, 1), "%")
#         )
#       )
#
#
#     total_summary_data <- rbind(total_summary_data, subgroup_data)
#   }
#
#
#   total_summary_data <- total_summary_data |> dplyr::ungroup()
#
#   # i <- 1
#   for(i in seq_along(select_subgroups)) {
#
#    p <- total_summary_data |>
#     dplyr::filter(.data$subgroup == select_subgroups[[i]]) |>
#       ggplot2::ggplot(ggplot2::aes(y = percentage, x = subgroup_level)) +
#       ggplot2::geom_bar(position = "dodge", stat = "identity", fill = "#0091DF") +
#     ggplot2::theme_minimal() +
#     ggplot2::geom_text(ggplot2::aes(label = percentage_label),
#                        position = ggplot2::position_stack(vjust = .5)) +
#     ggplot2::theme(
#       legend.position = "none",
#       axis.text.x = ggplot2::element_text(colour = "white"),
#       axis.text.y = ggplot2::element_text(colour = "white"),
#       axis.title.x = ggplot2::element_text(colour = "white"),
#       axis.title.y = ggplot2::element_text(colour = "white")
#     ) +
#     ggplot2::xlab(select_subgroups[[i]])+
#      ggplot2::ylab("Percentage (%)")
#
#
#
#   py <- plotly::ggplotly(p) %>%
#     plotly::layout(
#       plot_bgcolor = "#404A4E",
#       paper_bgcolor ='#404A4E',
#       font = list(family = "Agency FB",
#                   color = "#FFFFFF"
#       )
#       # barmode = "overlay"#,
#       # #hovermode = "x unified"
#     )
#   figure_list[[i]] <- py
#   }
#   g <- plotly::subplot(
#     rev(figure_list),
#     nrows = length(select_subgroups) # 2 if filtered data are selected
#   )
#   g
# }
