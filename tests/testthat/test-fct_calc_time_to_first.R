# Sample dataset for testing
test_data <- data.frame(
  subjectid = c(1, 1, 2, 2, 3, 3),
  event_group = c("A", "A", "B", "B", "C", "C"),
  event = c("Event1", "Event2", "Event1", "Event2", "Event1", "Event2"),
  event_time = as.POSIXct(c(2, 8,
                                  5, 3,
                                  1, 2)),
  event_time_end = as.POSIXct(c(2, 98,
                                12, 3,
                                5, 4))
)

# Tests
test_that("Function returns correct time to first event", {
  result <- calc_time_to_first(test_data, calc_event = TRUE, calc_event_group = FALSE)

  expect_equal(result$ttf_A_Event1[1], as.numeric(2))
  expect_equal(result$ttf_A_Event1[2], NA_integer_)
  expect_equal(result$ttf_A_Event2[1], as.numeric(8))
  expect_equal(result$ttf_B_Event1[2], as.numeric(5))
  expect_equal(result$ttf_B_Event2[2], as.numeric(3))
  expect_equal(result$ttf_C_Event1[2], NA_integer_)
  expect_equal(result$ttf_C_Event2[2], NA_integer_)
  expect_equal(result$ttf_C_Event1[3], as.numeric(1))
  expect_equal(result$ttf_C_Event2[3], as.numeric(2))
})

test_that("Function returns correct time to first event group", {
  result <- calc_time_to_first(test_data, calc_event = FALSE, calc_event_group = TRUE)

  expect_equal(result$ttf_A[1], as.numeric(2))
  expect_equal(result$ttf_A[3], NA_integer_)
  expect_equal(result$ttf_B[2], as.numeric(3))
  expect_equal(result$ttf_B[1], NA_integer_)
  expect_equal(result$ttf_C[3], as.numeric(1))
  expect_equal(result$ttf_C[1], NA_integer_)
})

test_that("Function stops with both calc_event_group and calc_event as FALSE", {
  expect_error(calc_time_to_first(test_data, calc_event = FALSE, calc_event_group = FALSE),
               "Error: One of calc_event_group and calc_event must be TRUE.")
})

test_that("Function handles NA values correctly", {
  test_data_with_na <- rbind(test_data, data.frame(
    subjectid = c(4, 5, 5),
    event_group = c("A", NA, "A"),
    event = c("Event1", "Event1", NA),
    event_time = c(NA, 3, 5),
    event_time_end = c(NA, NA, 8)
  ))

  result <- calc_time_to_first(test_data_with_na, calc_event = TRUE, calc_event_group = TRUE)

  # Check that the result for subjectid 4 is NA for the specific event
  expect_true(is.na(result$ttf_A_Event1[which(result$subjectid == 4)]))
  expect_equal(result$ttf_NA_Event1[4],NA_integer_)
  expect_equal(result$ttf_NA[4],NA_integer_)
  expect_equal(result$ttf_NA_Event1[5],3)
  expect_equal(result$ttf_NA[5],3)
  expect_equal(result$ttf_A_NA[5],5)
  expect_equal(result$ttf_A[5],5)
})
