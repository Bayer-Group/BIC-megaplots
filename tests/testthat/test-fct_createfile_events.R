# Sample subject-level data for testing
mp_data <- list(
  sl = data.frame(
    subjectid = c(1, 2, 3),
    start_time = c(1, 2, 3),
    end_time = c(10, 12, 15),
    ref_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01"))
  ),
  events = NULL
)

# Sample dataset for testing
test_data <- data.frame(
  USUBJID = c("001", "002", "003"),
  AEBODSYS = c("System A", "System B", "System C"),
  AEDECOD = c("Event A", "Event B", "Event C"),
  ASTDT = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
  AENDT = as.Date(c("2022-01-05", "2022-01-06", "2022-01-07")),
  stringsAsFactors = FALSE
)

# Test for processing event-level data correctly
test_that("Function processes event-level data correctly", {
  result <- createFile.events(mp_data, test_data, param = list(c("AEBODSYS", "AEDECOD")))
  expect_true(is.list(result))
  expect_true(is.data.frame(result$sl))
  expect_true(is.data.frame(result$events))
})

# Test for missing subject identifier
test_that("Function throws error for missing subject identifier", {
  expect_error(createFile.events(mp_data, test_data, param = list(c("AEBODSYS", "AEDECOD")), id = "NON_EXISTENT_ID"),
               "The specified id 'NON_EXISTENT_ID' is not a column in the dataset.")
})

# Test for filtering data
test_that("Function applies data filter correctly", {
  result <- createFile.events(mp_data, test_data, param = list(c("AEBODSYS", "AEDECOD")), data_filter = 'AEBODSYS == "System A"')
  expect_equal(nrow(result$events), 1)  # Expect one event for filtering
})

# Test for missing event columns
test_that("Function throws error for missing event columns", {
  test_data_no_event <- data.frame(
    USUBJID = c("001", "002", "003"),
    stringsAsFactors = FALSE
  )
  expect_error(createFile.events(mp_data, test_data_no_event, param = list(c("AEBODSYS", "AEDECOD"))),
               "None of the input parameters in param are present as column names in the data: AEBODSYS, AEDECOD")
})
