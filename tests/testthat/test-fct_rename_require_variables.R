test_that("Check if renaming via function 'rename_require_variables' works", {

  test_data_megaplot <- data.frame(
    IDENTIFIER = c("123", "123", "456", "456", "789"),
    IDENTIFIER_2 = c("1-123", "1-123", "2-456","2-456","2-789"),
    IDENTIFIER_N = c(123,123,456,456,789),
    EVENT = c("Event 1", "Event 2", "Event 1", "Event 1", "Event 2"),
    EVENT_GROUP = c("Group 1", "Group 1", "Group 2", "Group 2", "Group 3"),
    START = c("1","1","2","2","3"),
    START_N = c(1,1,2,2,3),
    START_N_2 = c(1.5,1.5,2,2,3.2),
    END = c("10","10","20","20","30"),
    END_N = c(10,10,20,20,30),
    EVENT_TIME_START = c(1,6,2,7,10),
    EVENT_TIME_START_CHR = c("1","6","2","7", "10"),
    EVENT_TIME_END = c(1,4,18,19,12),
    EVENT_TIME_END_CHR = c("1","4","18","19","12")
  )

  # as expected
  result <- rename_require_variables(
    megaplot_data = test_data_megaplot,
    selected_subjectid = "IDENTIFIER",
    selected_start_time = "START_N",
    selected_end_time = "END_N",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START",
    selected_event_time_end = "EVENT_TIME_END"
  )

  #check if return value is data.frame
  expect_equal(is.data.frame(result), TRUE)
  expect_equal(test_data_megaplot$IDENTIFIER, result$megaplots_selected_subjectid)
  expect_equal(test_data_megaplot$START_N, result$megaplots_selected_start_time)
  expect_equal(test_data_megaplot$END_N, result$megaplots_selected_end_time)
  expect_equal(test_data_megaplot$EVENT, result$megaplots_selected_event)
  expect_equal(test_data_megaplot$EVENT_GROUP, result$megaplots_selected_event_group)
  expect_equal(test_data_megaplot$EVENT_TIME_START, result$megaplots_selected_event_time)
  expect_equal(test_data_megaplot$EVENT_TIME_END, result$megaplots_selected_event_time_end)

  # use character start and end variables (should be set to NA in this case)
  result <- rename_require_variables(
    megaplot_data = test_data_megaplot,
    selected_subjectid = "IDENTIFIER",
    selected_start_time = "START",
    selected_end_time = "END",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START",
    selected_event_time_end = "EVENT_TIME_END"
  )

  expect_equal(is.data.frame(result), TRUE)
  expect_equal(test_data_megaplot$IDENTIFIER, result$megaplots_selected_subjectid)
  expect_equal(rep(NA,5), result$megaplots_selected_start_time)
  expect_equal(rep(NA,5), result$megaplots_selected_end_time)
  expect_equal(test_data_megaplot$EVENT, result$megaplots_selected_event)
  expect_equal(test_data_megaplot$EVENT_GROUP, result$megaplots_selected_event_group)
  expect_equal(test_data_megaplot$EVENT_TIME_START, result$megaplots_selected_event_time)
  expect_equal(test_data_megaplot$EVENT_TIME_END, result$megaplots_selected_event_time_end)


  # use character start and end variables (should be set to NA in this case)
  result <- rename_require_variables(
    megaplot_data = test_data_megaplot,
    selected_subjectid = "IDENTIFIER",
    selected_start_time = "EVENT",
    selected_end_time = "EVENT",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START",
    selected_event_time_end = "EVENT_TIME_END"
  )

  expect_equal(is.data.frame(result), TRUE)
  expect_equal(test_data_megaplot$IDENTIFIER, result$megaplots_selected_subjectid)
  expect_equal(rep(NA,5), result$megaplots_selected_start_time)
  expect_equal(rep(NA,5), result$megaplots_selected_end_time)
  expect_equal(test_data_megaplot$EVENT, result$megaplots_selected_event)
  expect_equal(test_data_megaplot$EVENT_GROUP, result$megaplots_selected_event_group)
  expect_equal(test_data_megaplot$EVENT_TIME_START, result$megaplots_selected_event_time)
  expect_equal(test_data_megaplot$EVENT_TIME_END, result$megaplots_selected_event_time_end)

  # use character variables for event_time and event_time end (should return NAs)
  result <- rename_require_variables(
    megaplot_data = test_data_megaplot,
    selected_subjectid = "IDENTIFIER",
    selected_start_time = "START_N",
    selected_end_time = "END_N",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START_CHR",
    selected_event_time_end = "EVENT_TIME_END_CHR"
  )

  expect_equal(is.data.frame(result), TRUE)
  expect_equal(test_data_megaplot$IDENTIFIER, result$megaplots_selected_subjectid)
  expect_equal(test_data_megaplot$START_N, result$megaplots_selected_start_time)
  expect_equal(test_data_megaplot$END_N, result$megaplots_selected_end_time)
  expect_equal(test_data_megaplot$EVENT, result$megaplots_selected_event)
  expect_equal(test_data_megaplot$EVENT_GROUP, result$megaplots_selected_event_group)
  expect_equal(rep(NA,5), result$megaplots_selected_event_time)
  expect_equal(rep(NA,5), result$megaplots_selected_event_time_end)

  # check if megaplot_data is NULL
  result <- rename_require_variables(
    megaplot_data = NULL,
    selected_subjectid = "IDENTIFIER",
    selected_start_time = "START_N",
    selected_end_time = "END_N",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START_CHR",
    selected_event_time_end = "EVENT_TIME_END_CHR"
  )

  expect_equal(is.data.frame(result), FALSE)

  # Select Identifier Variable which "-" character inside
  result <- rename_require_variables(
    megaplot_data = test_data_megaplot,
    selected_subjectid = "IDENTIFIER_2",
    selected_start_time = "START_N",
    selected_end_time = "END_N",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START",
    selected_event_time_end = "EVENT_TIME_END"
  )

  #check if return value is data.frame
  expect_equal(is.data.frame(result), TRUE)
  expect_equal(test_data_megaplot$IDENTIFIER_2, result$megaplots_selected_subjectid)
  expect_equal(test_data_megaplot$START_N, result$megaplots_selected_start_time)
  expect_equal(test_data_megaplot$END_N, result$megaplots_selected_end_time)
  expect_equal(test_data_megaplot$EVENT, result$megaplots_selected_event)
  expect_equal(test_data_megaplot$EVENT_GROUP, result$megaplots_selected_event_group)
  expect_equal(test_data_megaplot$EVENT_TIME_START, result$megaplots_selected_event_time)
  expect_equal(test_data_megaplot$EVENT_TIME_END, result$megaplots_selected_event_time_end)


  # Select Event Start times with decimal numbers
  result <- rename_require_variables(
    megaplot_data = test_data_megaplot,
    selected_subjectid = "IDENTIFIER_2",
    selected_start_time = "START_N_2",
    selected_end_time = "END_N",
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = "EVENT_TIME_START",
    selected_event_time_end = "EVENT_TIME_END"
  )

  #check if return value is data.frame
  expect_equal(is.data.frame(result), TRUE)
  expect_equal(test_data_megaplot$IDENTIFIER_2, result$megaplots_selected_subjectid)
  #expect selected start time are rounded and are equal to START_N
  expect_equal(test_data_megaplot$START_N, result$megaplots_selected_start_time)
  expect_equal(test_data_megaplot$END_N, result$megaplots_selected_end_time)
  expect_equal(test_data_megaplot$EVENT, result$megaplots_selected_event)
  expect_equal(test_data_megaplot$EVENT_GROUP, result$megaplots_selected_event_group)
  expect_equal(test_data_megaplot$EVENT_TIME_START, result$megaplots_selected_event_time)
  expect_equal(test_data_megaplot$EVENT_TIME_END, result$megaplots_selected_event_time_end)


  result <- rename_require_variables(
    megaplot_data = NULL,
    selected_subjectid = "IDENTIFIER",
    selected_start_time = NULL,
    selected_end_time = NULL,
    selected_event = "EVENT",
    selected_event_group = "EVENT_GROUP",
    selected_event_time = NULL,
    selected_event_time_end = NULL
  )

  #requirement that either start_time/end time or event_time/event_time_end isn't
  #NULL is not met
  expect_equal(is.data.frame(result), FALSE)
})
