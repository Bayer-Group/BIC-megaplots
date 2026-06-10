adsl_min <- data.frame(
  USUBJID = c("01-001", "01-002"),
  REFSTDT = as.Date(c("2020-01-01", "2020-01-05")),
  REFENDT = as.Date(c("2020-02-01", "2020-02-10")),
  TRTSTDT = as.Date(c("2020-01-01", "2020-01-05")),
  stringsAsFactors = FALSE
)

adae_min <- data.frame(
  USUBJID = c("01-001", "01-002"),
  AEBODSYS = c("SOC", "SOC"),
  AEDECOD = c("PTa", "PTb"),
  ASTDT = as.Date(c("2020-01-02", "2020-01-06")),
  AENDT = as.Date(c("2020-01-03", "2020-01-07")),
  stringsAsFactors = FALSE
)

# Event data with TRTSTDT only (enough for sl_ref_date when add_sl_data was not called)
adae_with_ref_col <- data.frame(
  USUBJID = c("01-001", "01-002"),
  TRTSTDT = as.Date(c("2020-01-01", "2020-01-05")),
  AEBODSYS = c("SOC", "SOC"),
  AEDECOD = c("PTa", "PTb"),
  ASTDT = as.Date(c("2020-01-02", "2020-01-06")),
  AENDT = as.Date(c("2020-01-03", "2020-01-07")),
  stringsAsFactors = FALSE
)

mp_with_sl <- function() {
  add_sl_data(adsl_min)
}

test_that("add_events initializes builder when mp is omitted", {
  mp <- add_events(
    events_data = adae_with_ref_col,
    event_group = "AEBODSYS",
    event = "AEDECOD",
    sl_ref_date = "TRTSTDT"
  )
  expect_s3_class(mp, "mp_data_builder")
  expect_gt(nrow(mp$events), 0L)
})

test_that("add_events appends rows and applies prefix_group / prefix_event", {
  mp <- mp_with_sl() %>%
    add_events(
      events_data = adae_min,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      prefix_group = "G:",
      prefix_event = "E:"
    )

  expect_equal(nrow(mp$events), 2L)
  expect_true(all(grepl("^G:", mp$events$event_group)))
  expect_true(all(grepl("^E:", mp$events$event)))
})

test_that("add_events resolves event column names case-insensitively", {
  adae <- adae_min
  names(adae) <- c("USUBJID", "aebodsys", "aedecod", "ASTDT", "AENDT")

  mp <- mp_with_sl() %>%
    add_events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD"
    )

  expect_equal(nrow(mp$events), 2L)
})

test_that("add_events pastes multiple columns for event_group and event", {
  adae <- adae_min
  adae$GRP2 <- c("X", "Y")
  adae$EV2 <- c("aa", "bb")

  mp <- mp_with_sl() %>%
    add_events(
      adae,
      event_group = c("AEBODSYS", "GRP2"),
      event = c("AEDECOD", "EV2")
    )

  expect_equal(mp$events$event_group, c("SOC; X", "SOC; Y"))
  expect_equal(mp$events$event, c("PTa; aa", "PTb; bb"))
})

test_that("add_events messages and warns for multiple event_group/event columns", {
  adae <- adae_min
  adae$GRP2 <- c("X", "Y")
  adae$GRP3 <- c("U", "V")
  adae$EV2 <- c("aa", "bb")
  adae$EV3 <- c("cc", "dd")

  expect_message(
    expect_message(
      expect_message(
        expect_message(
          mp_with_sl() %>%
            add_events(
              adae,
              event_group = c("AEBODSYS", "GRP2", "GRP3"),
              event = c("AEDECOD", "EV2", "EV3")
            ),
          regexp = "More than 2 `event_group` columns"
        ),
        regexp = "More than 2 `event` columns"
      ),
      regexp = "Multiple `event_group` columns provided"
    ),
    regexp = "Multiple `event` columns provided"
  )
})

test_that("add_events stacks multiple calls on mp$events", {
  mp <- mp_with_sl() %>%
    add_events(adae_min, event_group = "AEBODSYS", event = "AEDECOD") %>%
    add_events(adae_min, event_group = "AEBODSYS", event = "AEDECOD")

  expect_equal(nrow(mp$events), 4L)
})

test_that("add_events errors when mp is not mp_data_builder", {
  expect_error(
    add_events(list(), adae_min, event_group = "AEBODSYS", event = "AEDECOD"),
    "`mp_builder` must be a megaplots data builder"
  )
})

test_that("add_events builds minimal mp$sl from sl_ref_date column when add_sl_data was not used", {
  mp <- add_events(
    adae_with_ref_col,
    event_group = "AEBODSYS",
    event = "AEDECOD",
    sl_ref_date = "TRTSTDT"
  )

  expect_named(mp$sl, c("subjectid", "ref_date"))
  expect_equal(nrow(mp$sl), 2L)
  expect_equal(nrow(mp$events), 2L)
})

test_that("add_events uses numeric sl_ref_date with relative-day event columns", {
  adae_rel <- data.frame(
    USUBJID = c("01-001", "01-002"),
    AEBODSYS = c("SOC", "SOC"),
    AEDECOD = c("PTa", "PTb"),
    day_s = c(2L, 6L),
    day_e = c(3L, 7L),
    stringsAsFactors = FALSE
  )

  mp <- add_events(
    adae_rel,
    event_group = "AEBODSYS",
    event = "AEDECOD",
    event_start = "day_s",
    event_end = "day_e",
    sl_ref_date = 1L
  )

  expect_equal(mp$events$event_time, c(2L, 6L))
  expect_equal(mp$events$event_time_end, c(3L, 7L))
})

test_that("add_events errors when mp$sl is NULL and sl_ref_date is missing", {
  expect_error(
    add_events(adae_min, event_group = "AEBODSYS", event = "AEDECOD"),
    regexp = "sl_ref_date"
  )
})

test_that("add_events errors when sl_ref_date names a missing column", {
  expect_error(
    add_events(
      adae_min,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      sl_ref_date = "NOT_A_COLUMN"
    ),
    regexp = "NOT_A_COLUMN"
  )
})

test_that("add_events does not require sl_ref_date once mp$sl exists", {
  mp <- add_events(
    adae_with_ref_col,
    event_group = "AEBODSYS",
    event = "AEDECOD",
    sl_ref_date = "TRTSTDT"
  ) %>%
    add_events(adae_min, event_group = "AEBODSYS", event = "AEDECOD")

  expect_equal(nrow(mp$events), 4L)
})


test_that("add_events errors when id column is absent", {
  bad <- adae_min
  names(bad)[1] <- "SUBJ"

  expect_error(
    mp_with_sl() %>%
      add_events(bad, event_group = "AEBODSYS", event = "AEDECOD"),
    "The specified id 'USUBJID' is not a column in the dataset."
  )
})

test_that("add_events errors when event_group column is absent", {
  bad <- adae_min
  names(bad)[2] <- "WRONG"

  expect_error(
    mp_with_sl() %>%
      add_events(bad, event_group = "AEBODSYS", event = "AEDECOD"),
    "Column 'AEBODSYS' not found in dataset."
  )
})

test_that("add_events keeps keep_vars when present", {
  adae <- adae_min
  adae$EXTRA <- c("x", "y")

  mp <- mp_with_sl() %>%
    add_events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      keep_vars = "EXTRA"
    )

  expect_true("EXTRA" %in% names(mp$events))
})

test_that("add_events retains keep_vars across multiple calls and datasets", {
  adae_same <- adae_min
  adae_same$EXTRA_A <- c("a1", "a2")
  adae_same$EXTRA_B <- c("b1", "b2")

  adae_other <- adae_min
  adae_other$AEBODSYS <- c("NEURO", "CARD")
  adae_other$AEDECOD <- c("Headache", "Palpitation")
  adae_other$EXTRA_C <- c("c1", "c2")

  mp <- mp_with_sl() %>%
    add_events(
      adae_same,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      keep_vars = "EXTRA_A"
    ) %>%
    add_events(
      adae_same,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      keep_vars = "EXTRA_B"
    ) %>%
    add_events(
      adae_other,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      keep_vars = "EXTRA_C"
    )

  expect_true(all(c("EXTRA_A", "EXTRA_B", "EXTRA_C") %in% names(mp$events)))
  expect_true(!all(is.na(mp$events$EXTRA_A)))
  expect_true(!all(is.na(mp$events$EXTRA_B)))
  expect_true(!all(is.na(mp$events$EXTRA_C)))
})

test_that("add_events joins time-to-first columns when calc_time_to_first is TRUE", {
  mp <- mp_with_sl() %>%
    add_events(
      adae_min,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      calc_time_to_first = TRUE
    )

  ttf_cols <- grep("^ttf_", names(mp$sl), value = TRUE)
  expect_true(length(ttf_cols) > 0L)
})

test_that("add_events skips all-NA earlier event_start candidate", {
  adae <- data.frame(
    USUBJID = c("01-001", "01-002"),
    AEBODSYS = c("SOC", "SOC"),
    AEDECOD = c("PTa", "PTb"),
    ASTDT = c(NA, NA),
    AESTDT = as.Date(c("2020-01-02", "2020-01-06")),
    AENDT = as.Date(c("2020-01-03", "2020-01-07")),
    stringsAsFactors = FALSE
  )

  mp <- mp_with_sl() %>%
    add_events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      event_start = c("ASTDT", "AESTDT")
    )

  expect_equal(nrow(mp$events), 2L)
  expect_false(any(is.na(mp$events$event_time)))
})

test_that("add_events errors when event dates and ref_date scale are mixed", {
  adae_bad <- data.frame(
    USUBJID = "01-001",
    TRTSTDT = as.Date("2020-01-01"),
    AEBODSYS = "S",
    AEDECOD = "P",
    day_only = 5L,
    day_end = 6L,
    stringsAsFactors = FALSE
  )

  expect_error(
    add_events(
      adae_bad,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      event_start = "day_only",
      event_end = "day_end",
      sl_ref_date = "TRTSTDT"
    ),
    regexp = "mixed types"
  )
})

test_that("add_events errors on unconverted SAS-style character dates", {
  adae_sas <- data.frame(
    USUBJID = "01-001",
    TRTSTDT = "01JAN2020",
    AEBODSYS = "SOC",
    AEDECOD = "PTa",
    ASTDT = "02JAN2020",
    AENDT = "03JAN2020",
    stringsAsFactors = FALSE
  )

  expect_error(
    add_events(
      adae_sas,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      sl_ref_date = "TRTSTDT"
    ),
    regexp = "convert date columns to Date"
  )
})

test_that("add_events accepts ISO character date columns when mp$sl is NULL", {
  adae_iso <- data.frame(
    USUBJID = c("01-001", "01-002"),
    TRTSTDT = c("2020-01-01", "2020-01-05"),
    AEBODSYS = c("SOC", "SOC"),
    AEDECOD = c("PTa", "PTb"),
    ASTDT = c("2020-01-02", "2020-01-06"),
    AENDT = c("2020-01-03", "2020-01-07"),
    stringsAsFactors = FALSE
  )

  mp <- add_events(
    adae_iso,
    event_group = "AEBODSYS",
    event = "AEDECOD",
    sl_ref_date = "TRTSTDT"
  )

  expect_true(inherits(mp$sl$ref_date, "Date"))
  expect_equal(mp$events$event_time, c(2L, 2L))
  expect_equal(mp$events$event_time_end, c(3L, 3L))
})
