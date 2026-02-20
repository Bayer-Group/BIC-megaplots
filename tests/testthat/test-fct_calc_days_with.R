# Sample dataset for testing
test_data <- data.frame(
  subjectid = c(1, 1, 1, 2, 2, 3, 3),
  event_group = c("A", "A", "A", "B", "B", "C", "C"),
  event = c("Event1", "Event1", "Event2", "Event1", "Event2", "Event1", "Event2"),
  event_start_time = as.numeric(c(1, 1, 5, 2, 4, 1, 6)),  # Start days
  event_end_time = as.numeric(c(2, 3, 13, 53, 5, 4, 9))     # End days
)

# Tests
test_that("Function calculates days with individual events correctly", {
  result <- calc_days_with(test_data, calc_event = TRUE, calc_event_group = FALSE)

  expect_equal(result$dw_A_Event1[1], 3)
  expect_equal(result$dw_A_Event1[2], 0)
  expect_equal(result$dw_A_Event2[1], 9)
  expect_equal(result$dw_B_Event1[2], 52)
  expect_equal(result$dw_B_Event2[2], 2)
  expect_equal(result$dw_B_Event2[3], 0)
  expect_equal(result$dw_C_Event1[3], 4)
  expect_equal(result$dw_C_Event2[3], 4)
  expect_equal(result$dw_C_Event2[1], 0)
})

test_that("Function calculates days with event groups correctly", {
  result <- calc_days_with(data=test_data, calc_event = FALSE, calc_event_group = TRUE)

  expect_equal(result$dw_A[1], 12)
  expect_equal(result$dw_B[2], 52)
  expect_equal(result$dw_C[3], 8)
})

test_that("Function stops with both calc_event_group and calc_event as FALSE", {
  expect_error(calc_days_with(test_data, calc_event = FALSE, calc_event_group = FALSE),
               "Error: One of calc_event_group and calc_event must be TRUE.")
})

test_that("Function handles NA values correctly", {
  test_data_with_na <- rbind(test_data, data.frame(
    subjectid = c(4, 5, 5),
    event_group = c("A", NA, "A"),
    event = c("Event1", "Event1", NA),
    event_start_time = c(NA, 3, 5),
    event_end_time = c(NA, NA, 8)
  ))

  result <- calc_days_with(data=test_data_with_na, calc_event = TRUE, calc_event_group = TRUE)

  # Check that the result for subjectid 4 is NA for the specific event
  expect_equal(result$dw_A_Event1[4],0)
  expect_equal(result$dw_NA_Event1[4],0)
  expect_equal(result$dw_NA[4],0)
  expect_equal(result$dw_NA_Event1[5],1)
  expect_equal(result$dw_NA[5],1)
  expect_equal(result$dw_A_NA[5],4)
  expect_equal(result$dw_A[5],4)
})

test_that("Function handles overlapping events correctly", {
  overlapping_data <- data.frame(
    subjectid = c(1, 1),
    event_group = c("A", "A"),
    event = c("Event1", "Event2"),
    event_start_time = c(1, 2),
    event_end_time = c(3, 4)
  )

  result <- calc_days_with(overlapping_data, calc_event = TRUE, calc_event_group = TRUE)

  expect_equal(result$dw_A_Event1[1], 3)  # Event1 spans days 1 to 3
  expect_equal(result$dw_A_Event2[1], 3)  # Event2 spans days 2 to 4
  expect_equal(result$dw_A[1], 4)          # Group A spans days 1 to 4
})
