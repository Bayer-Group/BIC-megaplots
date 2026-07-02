test_that("Check if creating unique events via function 'create_unique_event_identifier' works", {
  #unexpected cases
  #emtpy data
  result <- create_unique_event_identifier(
    NULL
  )

  testthat::expect_equal(result, NULL)

  test_data_megaplot <- data.frame(
    megaplots_selected_subjectid = rep(NA, 5),
    megaplots_selected_event = rep(NA, 5),
    megaplots_selected_event_group = c(
      "Group 1",
      "Group 1",
      "Group 2",
      "Group 2",
      "Group 3"
    ),
    megaplots_selected_start_time_chr = c("1", "1", "2", "2", "3"),
    megaplots_selected_start_time = c(1, 1, 2, 2, 3),
    megaplots_selected_start_time = c(10, 10, 20, 20, 30),
    megaplots_selected_event_time = c(1, 6, 2, 7, 10),
    megaplots_selected_event_time_end = c(1, 4, 18, 19, 12)
  )

  result <- create_unique_event_identifier(
    test_data_megaplot
  )

  result

  #no subjectid, event and event_group
  test_data_megaplot <- data.frame(
    megaplots_selected_subjectid = rep(NA, 5),
    megaplots_selected_event = rep(NA, 5),
    megaplots_selected_event_group = rep(NA, 5),
    megaplots_selected_start_time_chr = c("1", "1", "2", "2", "3"),
    megaplots_selected_start_time = c(1, 1, 2, 2, 3),
    megaplots_selected_start_time = c(10, 10, 20, 20, 30),
    megaplots_selected_event_time = c(1, 6, 2, 7, 10),
    megaplots_selected_event_time_end = c(1, 4, 18, 19, 12)
  )

  result <- create_unique_event_identifier(
    test_data_megaplot
  )

  testthat::expect_equal(result, NULL)

  test_data_megaplot <- data.frame(
    megaplots_selected_subjectid = c("123", "123", "456", "456", "789"),
    megaplots_selected_event = rep(NA, 5),
    megaplots_selected_event_group = rep(NA, 5),
    megaplots_selected_start_time_chr = c("1", "1", "2", "2", "3"),
    megaplots_selected_start_time = c(1, 1, 2, 2, 3),
    megaplots_selected_start_time = c(10, 10, 20, 20, 30),
    megaplots_selected_event_time = c(1, 6, 2, 7, 10),
    megaplots_selected_event_time_end = c(1, 4, 18, 19, 12)
  )

  result <- create_unique_event_identifier(
    test_data_megaplot
  )

  testthat::expect_equal(result, NULL)

  # as expected
  test_data_megaplot <- data.frame(
    megaplots_selected_subjectid = c("123", "123", "456", "456", "789"),
    megaplots_selected_event = c(
      "Event 1",
      "Event 2",
      "Event 1",
      "Event 1",
      "Event 2"
    ),
    megaplots_selected_event_group = c(
      "Group 1",
      "Group 1",
      "Group 2",
      "Group 2",
      "Group 3"
    ),
    megaplots_selected_start_time_chr = c("1", "1", "2", "2", "3"),
    megaplots_selected_start_time = c(1, 1, 2, 2, 3),
    megaplots_selected_start_time = c(10, 10, 20, 20, 30),
    megaplots_selected_event_time = c(1, 6, 2, 7, 10),
    megaplots_selected_event_time_end = c(1, 4, 18, 19, 12)
  )

  result <- create_unique_event_identifier(
    test_data_megaplot
  )

  #expect 7 rows
  # 2 rows for "Event 1" and "Event 2" in "Group 1"
  # 1 row for  "Group 1" (selected_event should be NA)
  # 1 row for "Event 1" in "Group 2"
  # 1 row for "Group 2"
  # 1 row for "Event 2" in "Group 3"
  # 1 row for "Group 3"

  testthat::expect_equal(dim(result), c(7, 12))
  testthat::expect_equal(
    sort(result$megaplots_selected_event_group),
    c(
      "Group 1",
      "Group 1",
      "Group 1",
      "Group 2",
      "Group 2",
      "Group 3",
      "Group 3"
    )
  )
  testthat::expect_equal(sort(result$event_group_id), c(1, 1, 1, 2, 2, 3, 3))
  testthat::expect_equal(
    sort(colnames(result)),
    sort(c(
      "megaplots_selected_event_group",
      "megaplots_selected_event",
      "event_id",
      "event_group_id",
      "max_event_id",
      "event_n",
      "n_flag",
      "event_color",
      "gradient_event_color_2",
      "gradient_event_color_1",
      "gradient_event_color_3",
      "jittered"
    ))
  )

  # as expected
  test_data_megaplot <- data.frame(
    megaplots_selected_subjectid = c("123", "123", "456", "456", "789"),
    megaplots_selected_event = c(
      "Event 1",
      "Event 1",
      "Event 1",
      "Event 1",
      "Event 1"
    ),
    megaplots_selected_event_group = c(
      "Group 1",
      "Group 1",
      "Group 1",
      "Group 1",
      "Group 1"
    ),
    megaplots_selected_start_time_chr = c("1", "1", "2", "2", "3"),
    megaplots_selected_start_time = c(1, 1, 2, 2, 3),
    megaplots_selected_start_time = c(10, 10, 20, 20, 30),
    megaplots_selected_event_time = c(1, 6, 2, 7, 10),
    megaplots_selected_event_time_end = c(1, 4, 18, 19, 12)
  )

  result <- create_unique_event_identifier(
    test_data_megaplot
  )

  testthat::expect_equal(dim(result), c(2, 12))
  testthat::expect_equal(
    sort(result$megaplots_selected_event_group),
    c("Group 1", "Group 1")
  )
  testthat::expect_equal(sort(result$event_group_id), c(1, 1))
  testthat::expect_equal(
    sort(colnames(result)),
    sort(c(
      "megaplots_selected_event_group",
      "megaplots_selected_event",
      "event_id",
      "event_group_id",
      "max_event_id",
      "event_n",
      "n_flag",
      "event_color",
      "gradient_event_color_2",
      "gradient_event_color_1",
      "gradient_event_color_3",
      "jittered"
    ))
  )

  # as expected
  test_data_megaplot <- data.frame(
    megaplots_selected_subjectid = c("123", "123", "456", "456", "789"),
    megaplots_selected_event = c(
      "Event 1",
      "Event 2",
      "Event 3",
      "Event 4",
      "Event 5"
    ),
    megaplots_selected_event_group = c(
      "Group 1",
      "Group 2",
      "Group 3",
      "Group 4",
      "Group 5"
    ),
    megaplots_selected_start_time_chr = c("1", "1", "2", "2", "3"),
    megaplots_selected_start_time = c(1, 1, 2, 2, 3),
    megaplots_selected_start_time = c(10, 10, 20, 20, 30),
    megaplots_selected_event_time = c(1, 6, 2, 7, 10),
    megaplots_selected_event_time_end = c(1, 4, 18, 19, 12)
  )

  result <- create_unique_event_identifier(
    test_data_megaplot
  )

  testthat::expect_equal(dim(result), c(10, 12))
  testthat::expect_equal(
    sort(result$megaplots_selected_event_group),
    c(
      "Group 1",
      "Group 1",
      "Group 2",
      "Group 2",
      "Group 3",
      "Group 3",
      "Group 4",
      "Group 4",
      "Group 5",
      "Group 5"
    )
  )
  testthat::expect_equal(
    sort(result$event_group_id),
    c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  )
  testthat::expect_equal(
    sort(colnames(result)),
    sort(c(
      "megaplots_selected_event_group",
      "megaplots_selected_event",
      "event_id",
      "event_group_id",
      "max_event_id",
      "event_n",
      "n_flag",
      "event_color",
      "gradient_event_color_2",
      "gradient_event_color_1",
      "gradient_event_color_3",
      "jittered"
    ))
  )
})
