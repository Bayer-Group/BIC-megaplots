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
    "None of these column names exist in the data for display_start_date"
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
    "None of these column names exist in the data for relative_day_1"
  )
})

test_that("add_sl_data skips all-NA earlier display_start_date candidate", {
  adsl <- data.frame(
    USUBJID = c("001", "002"),
    RFSTDT = c(NA, NA),
    TRTSTDT = as.Date(c("2022-01-01", "2022-01-02")),
    RFENDT = as.Date(c("2022-01-10", "2022-01-12")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(
    adsl,
    display_start_date = c("RFSTDT", "TRTSTDT"),
    relative_day_1 = "TRTSTDT"
  )

  expect_equal(mp$sl$start_time, c(1L, 1L))
  expect_false(all(is.na(mp$sl$start_time)))
})

test_that("add_sl_data errors when display_start_date candidates are all useless", {
  adsl <- data.frame(
    USUBJID = "001",
    RFSTDT = NA,
    TRTSTDT = as.Date("2022-01-01"),
    RFENDT = as.Date("2022-01-10"),
    stringsAsFactors = FALSE
  )

  expect_error(
    add_sl_data(adsl, display_start_date = "RFSTDT"),
    "exist but contain no non-missing values"
  )
})

test_that("add_sl_data pmax uses only useful display_end_date columns", {
  adsl <- data.frame(
    USUBJID = c("001", "002"),
    REFSTDT = as.Date(c("2022-01-01", "2022-01-02")),
    REFENDT = c(NA, NA),
    RFENDT = as.Date(c("2022-01-10", "2022-01-15")),
    TRTSTDT = as.Date(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(
    adsl,
    display_end_date = c("REFENDT", "RFENDT")
  )

  expect_equal(mp$sl$end_time, c(10L, 14L))
})

test_that("add_sl_data errors on unconverted SAS-style character dates", {
  adsl_sas <- data.frame(
    USUBJID = "001",
    REFSTDT = "01JAN2022",
    RFENDT = "10JAN2022",
    TRTSTDT = "01JAN2022",
    stringsAsFactors = FALSE
  )

  expect_error(
    add_sl_data(adsl_sas),
    regexp = "convert date columns to Date"
  )
})

test_that("add_sl_data accepts ISO character date columns", {
  adsl_iso <- data.frame(
    USUBJID = "001",
    REFSTDT = "2022-01-01",
    RFENDT = "2022-01-10",
    TRTSTDT = "2022-01-01",
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(adsl_iso)

  expect_equal(mp$sl$start_time[[1]], 1L)
  expect_equal(mp$sl$end_time[[1]], 10L)
})

test_that("add_sl_data accepts display_start_date column named start_time", {
  adsl <- data.frame(
    USUBJID = c("001", "002"),
    start_time = as.Date(c("2022-01-01", "2022-01-02")),
    RFENDT = as.Date(c("2022-01-10", "2022-01-12")),
    TRTSTDT = as.Date(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(
    adsl,
    display_start_date = "start_time",
    relative_day_1 = "TRTSTDT"
  )

  expect_equal(mp$sl$start_time, c(1L, 1L))
  expect_equal(mp$sl$end_time, c(10L, 11L))
})

test_that("add_sl_data accepts relative_day_1 column named ref_date", {
  adsl <- data.frame(
    USUBJID = c("001", "002"),
    REFSTDT = as.Date(c("2022-01-01", "2022-01-02")),
    RFENDT = as.Date(c("2022-01-10", "2022-01-12")),
    ref_date = as.Date(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(
    adsl,
    relative_day_1 = "ref_date"
  )

  expect_equal(mp$sl$start_time, c(1L, 1L))
  expect_equal(mp$sl$ref_date, as.Date(c("2022-01-01", "2022-01-02")))
})
