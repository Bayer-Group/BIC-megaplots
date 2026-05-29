adsl_three <- data.frame(
  USUBJID = c("001", "002", "003"),
  REFSTDT = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
  RFENDT = as.Date(c("2022-01-10", "2022-01-12", "2022-01-15")),
  TRTSTDT = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
  stringsAsFactors = FALSE
)

test_that("add_sl_data initializes builder when mp is omitted", {
  mp <- add_sl_data(adsl_three)
  expect_s3_class(mp, "mp_data_builder")
  expect_equal(nrow(mp$sl), 3L)
})

test_that("add_sl_data accepts a data frame and builds subject-level columns", {
  mp <- add_sl_data(adsl_three)

  expect_s3_class(mp, "mp_data_builder")
  expect_true(is.data.frame(mp$sl))
  expect_equal(nrow(mp$sl), 3L)
  expect_equal(
    names(mp$sl)[1:4],
    c("subjectid", "start_time", "end_time", "ref_date")
  )
})

test_that("add_sl_data applies data_filter", {
  mp <- add_sl_data(adsl_three, data_filter = "USUBJID == \"001\"")

  expect_equal(nrow(mp$sl), 1L)
  expect_equal(mp$sl$subjectid[[1]], 1)
})

test_that("add_sl_data errors when id is not a column", {
  expect_error(
    add_sl_data(adsl_three, id = "NON_EXISTENT_ID"),
    "The specified id 'NON_EXISTENT_ID' is not a column in the dataset."
  )
})

test_that("add_sl_data errors when mp is not mp_data_builder", {
  expect_error(
    add_sl_data(list(), adsl_three),
    "`mp_builder` must be a megaplots data builder"
  )
})

test_that("add_sl_data errors when no display_start_date column matches", {
  bad <- data.frame(
    USUBJID = "001",
    RFENDT = as.Date("2022-01-10"),
    stringsAsFactors = FALSE
  )

  expect_error(
    add_sl_data(bad),
    "None of the input parameters in display_start_date are present as column names in the data."
  )
})

test_that("add_sl_data errors when no display_end_date column matches", {
  bad <- data.frame(
    USUBJID = "001",
    REFSTDT = as.Date("2022-01-01"),
    TRTSTDT = as.Date("2022-01-01"),
    stringsAsFactors = FALSE
  )

  expect_error(
    add_sl_data(bad),
    "None of the input parameters in display_end_date are present as column names in the data."
  )
})

test_that("add_sl_data errors when relative_day_1 does not match any column", {
  expect_error(
    add_sl_data(adsl_three, relative_day_1 = "NON_EXISTENT_COLUMN"),
    "None of the input parameters in relative_day_1 are present as column names in the data."
  )
})

test_that("add_sl_data computes treatment_duration when trt columns are set", {
  trt <- data.frame(
    USUBJID = c("001", "002", "003"),
    REFSTDT = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
    RFENDT = as.Date(c("2022-01-10", "2022-01-12", "2022-01-15")),
    TRTSTDT = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
    TRTENDT = as.Date(c("2022-01-05", "2022-01-07", "2022-01-10")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(trt, trtstdt = "TRTSTDT", trtendt = "TRTENDT")

  expect_true("treatment_duration" %in% names(mp$sl))
  expect_equal(mp$sl$treatment_duration[[1]], 5L)
})
