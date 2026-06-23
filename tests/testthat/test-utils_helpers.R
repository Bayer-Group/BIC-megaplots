test_that("Check if utils_helpers functions work", {

  hex_code <- NULL
  result <- font_color(hex_code)
  testthat::expect_equal(result, NULL)

  hex_code <- "red"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#ffffff")

  hex_code <- "#000000"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#ffffff")

  hex_code <- "#ffffff"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#000000")

  hex_code <- "#00ffff"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#000000")

  hex_code <- "#ff00ff"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#ffffff")

  hex_code <- "#ffff00"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#000000")

  ## transparent colors
  hex_code <- "#ffff0030"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#000000")

  hex_code <- "#00000030"
  result <- font_color(hex_code)
  #expect white
  testthat::expect_equal(result, "#ffffff")

  # color_func(.data$event_id, .data$event_group_id, .data$max_event_id, number_event_groups = number_event_groups)
  # used in create_unique_event_identifier
  #first rainbow color red
  result <- color_func(1,1,1,1)
  testthat::expect_all_equal(result, "#FF0000")

  #expect dark red
  result <- color_func(5,1,5,1)
  testthat::expect_all_equal(result, "#800000FF")

  result <- color_func(2,5,2,5)
  testthat::expect_all_equal(result, "#670080FF")

  result <- color_func(1,5,1,5)
  testthat::expect_all_equal(result, "#CC00FF")


  test_megaplot_data_raw <- data.frame(
    megaplots_selected_subjectid = rep(1:4,each=4),
    megaplots_selected_subjectid = rep(1:4,each=4),
    megaplots_selected_start_time =  rep(1:4,each=4),
    subjectid_n =  rep(1:4,each=4),
    megaplots_selected_end_time = rep(c(40,50,60,70),each=4),
    subjectid_n_end = rep(1:4,each=4),
    subjectid_n_jittered = rep(1:4,each=4),
    subjectid_n_jittered_end = rep(1:4,each=4),
    megaplots_selected_event = c("Event 1", "Event 1", "Event 2", "Event 3",
                                 "Event 1", "Event 2", "Event 3", "Event 4",
                                 "Event 4", "Event 5", "Event 6", "Event 7",
                                 "Event 1", "Event 8", "Event 8", "Event 8"),
    unique_event = c("Event 1", "Event 1", "Event 2", "Event 3",
                                 "Event 1", "Event 2", "Event 3", "Event 4 (Group 2)",
                                 "Event 4 (Group 3)", "Event 5", "Event 6", "Event 7",
                                 "Event 1 (Group 4)", "Event 8", "Event 8", "Event 8"),
    megaplots_selected_event_group = c("Group 1", "Group 1", "Group 1", "Group 2",
                                       "Group 1", "Group 1", "Group 2", "Group 2",
                                       "Group 3", "Group 3", "Group 3", "Group 3",
                                       "Group 4", "Group 4", "Group 4", "Group 4"),
    megaplots_selected_event_time =c(1:16),
    megaplots_selected_event_time_end = c(31:46),
    subgroup_1 = rep(c("Subgroup 1","Subgroup 2"), each = 8),
    subgroup_2 = rep(c("Subgroup A","Subgroup B", "Subgroup C", "Subgroup D"), each = 4),
    text_lines = NA,
    event_color ="#ff3322",
    text_events = NA
  )

  theme <- "dark"
  line_width_subjects <- 3
  line_width <- 3

  test_plotly_object <- test_megaplot_data_raw  |>
    plotly::plot_ly(                            #create empty plot_ly object
      source = "plotSource",
      #color = ~I(event_color),
      type ="scatter",
      mode = "lines+markers"
    ) |>
    plotly::add_segments(                       # create subject lines via add_segments
      y = ~subjectid_n,
      yend ~ subjectid_n_end,
      x  = ~ megaplots_selected_start_time,
      hoverinfo = "text",
      text = ~ text_lines,
      xend = ~ megaplots_selected_end_time,
      line = list(color = ifelse(theme =="dark", "#2c3336","#dedede"), width = line_width_subjects),
      showlegend = FALSE
    ) |>
    plotly::add_segments(
      legendgroup = ~ megaplots_selected_event_group,
      name = ~ unique_event,
      x = ~ megaplots_selected_event_time,
      xend = ~ megaplots_selected_event_time_end,
      y = ~ subjectid_n_jittered,
      yend = ~ subjectid_n_jittered_end,
      color = ~ I(event_color),
      line = list(color = ~ event_color, width = line_width),
      showlegend = TRUE,
      hoverinfo = "text",
      text = ~ text_events,
      hoverlabel = list(orientation = "h")
    )


  result <- get_trace_info(test_plotly_object)

  testthat::expect_equal(dim(result)[1], 12)
  testthat::expect_equal(dim(result)[2], 3)
  testthat::expect_equal(colnames(result), c("name","legendgroup","trace"))
  testthat::expect_equal(result$trace, 1:12)
  testthat::expect_equal(sort(result$name), sort(unique(test_megaplot_data_raw$unique_event)))
  testthat::expect_equal(sort(result$legendgroup), test_megaplot_data_raw |>
    dplyr::select(unique_event, megaplots_selected_event_group) |>
    dplyr::distinct() |>
    dplyr::pull(megaplots_selected_event_group) |>
    sort()
  )

  test_trace_info <- result

  result <- apply_trace_info(
    test_trace_info,
    test_plotly_object
  )

  testthat::expect_equal(class(result), c("plotly","htmlwidget"))

  result <- vrect(
    10,
    10,
    "#ff00ff"
  )
  testthat::expect_equal(class(result), "list")
  testthat::expect_equal(length(result), 9)
  testthat::expect_equal(names(result), c("type","fillcolor","y0","y1","yref","x0", "layer","x1", "line"))
  testthat::expect_equal(result$x0, 10)
  testthat::expect_equal(result$x1, 10)
  testthat::expect_equal(result$y0, 0)
  testthat::expect_equal(result$y1, 1)
  testthat::expect_equal(result$layer, "below")
  testthat::expect_equal(result$yref, "paper")
  testthat::expect_equal(result$fillcolor, "#ff00ff")

})
