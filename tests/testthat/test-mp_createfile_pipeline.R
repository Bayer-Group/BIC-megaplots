# Minimal end-to-end checks for add_sl_data / add_events -> finalize_mp_object

test_that("builder pipeline with add_sl_data stacks events and finalize returns one row per event", {
  adsl <- data.frame(
    USUBJID = c("01-001", "01-002"),
    REFSTDT = as.Date(c("2020-01-01", "2020-01-05")),
    REFENDT = as.Date(c("2020-02-01", "2020-02-10")),
    TRTSTDT = as.Date(c("2020-01-01", "2020-01-05")),
    stringsAsFactors = FALSE
  )

  adae <- data.frame(
    USUBJID = c("01-001", "01-001", "01-002"),
    AEBODSYS = c("SOC1", "SOC1", "SOC2"),
    AEDECOD = c("PT1", "PT2", "PT1"),
    ASTDT = as.Date(c("2020-01-02", "2020-01-10", "2020-01-06")),
    AENDT = as.Date(c("2020-01-03", "2020-01-12", "2020-01-07")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(adsl) %>%
    add_events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD"
    )

  expect_s3_class(mp, "mp_data_builder")
  expect_equal(nrow(mp$sl), 2L)
  expect_true(all(c("start_time", "end_time") %in% names(mp$sl)))
  expect_equal(nrow(mp$events), 3L)

  out <- finalize_mp_object(mp)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true(all(
    c("event_group", "event", "event_time", "start_time", "end_time") %in%
      names(out)
  ))
})

test_that("data_filter on add_events restricts rows", {
  adsl <- data.frame(
    USUBJID = "01-001",
    REFSTDT = as.Date("2020-01-01"),
    REFENDT = as.Date("2020-02-01"),
    TRTSTDT = as.Date("2020-01-01"),
    stringsAsFactors = FALSE
  )

  adae <- data.frame(
    USUBJID = c("01-001", "01-001"),
    AEBODSYS = c("S", "S"),
    AEDECOD = c("keep", "drop"),
    ASTDT = as.Date(c("2020-01-02", "2020-01-03")),
    AENDT = as.Date(c("2020-01-02", "2020-01-03")),
    stringsAsFactors = FALSE
  )

  mp <- add_sl_data(adsl) %>%
    add_events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      data_filter = "AEDECOD == 'keep'"
    )

  expect_equal(nrow(mp$events), 1L)
})

test_that("builder pipeline without add_sl_data uses sl_ref_date and finalize returns one row per event", {
  adae <- data.frame(
    USUBJID = c("01-001", "01-001", "01-002"),
    TRTSTDT = as.Date(c("2020-01-01", "2020-01-01", "2020-01-05")),
    AEBODSYS = c("SOC1", "SOC1", "SOC2"),
    AEDECOD = c("PT1", "PT2", "PT1"),
    ASTDT = as.Date(c("2020-01-02", "2020-01-10", "2020-01-06")),
    AENDT = as.Date(c("2020-01-03", "2020-01-12", "2020-01-07")),
    stringsAsFactors = FALSE
  )

  mp <- add_events(
    adae,
    event_group = "AEBODSYS",
    event = "AEDECOD",
    sl_ref_date = "TRTSTDT"
  )

  expect_equal(nrow(mp$sl), 2L)
  expect_named(mp$sl, c("subjectid", "ref_date"))
  expect_false(any(c("start_time", "end_time") %in% names(mp$sl)))

  out <- finalize_mp_object(mp)
  expect_equal(nrow(out), 3L)
  expect_false(any(c("start_time", "end_time") %in% names(out)))
  expect_true(all(
    c("subjectid", "ref_date", "event_group", "event", "event_time") %in%
      names(out)
  ))
})
