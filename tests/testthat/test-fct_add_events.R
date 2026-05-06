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

mp_with_sl <- function() {
  init_mp_object() %>% add.sl_data(adsl_min)
}

test_that("add.events appends rows and applies prefix_group / prefix_event", {
  mp <- mp_with_sl() %>%
    add.events(
      adae_min,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      prefix_group = "G:",
      prefix_event = "E:"
    )

  expect_equal(nrow(mp$events), 2L)
  expect_true(all(grepl("^G:", mp$events$event_group)))
  expect_true(all(grepl("^E:", mp$events$event)))
})

test_that("add.events resolves event column names case-insensitively", {
  adae <- adae_min
  names(adae) <- c("USUBJID", "aebodsys", "aedecod", "ASTDT", "AENDT")

  mp <- mp_with_sl() %>%
    add.events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD"
    )

  expect_equal(nrow(mp$events), 2L)
})

test_that("add.events stacks multiple calls on mp$events", {
  mp <- mp_with_sl() %>%
    add.events(adae_min, event_group = "AEBODSYS", event = "AEDECOD") %>%
    add.events(adae_min, event_group = "AEBODSYS", event = "AEDECOD")

  expect_equal(nrow(mp$events), 4L)
})

test_that("add.events errors when mp is not mp_data_builder", {
  expect_error(
    add.events(list(), adae_min, event_group = "AEBODSYS", event = "AEDECOD"),
    "`mp` must be an object created by init_mp_object()"
  )
})

test_that("add.events errors when subject-level data was not added", {
  expect_error(
    init_mp_object() %>%
      add.events(adae_min, event_group = "AEBODSYS", event = "AEDECOD"),
    "Subject-level data is missing; call add.sl_data() first."
  )
})

test_that("add.events errors when id column is absent", {
  bad <- adae_min
  names(bad)[1] <- "SUBJ"

  expect_error(
    mp_with_sl() %>%
      add.events(bad, event_group = "AEBODSYS", event = "AEDECOD"),
    "The specified id 'USUBJID' is not a column in the dataset."
  )
})

test_that("add.events errors when event_group column is absent", {
  bad <- adae_min
  names(bad)[2] <- "WRONG"

  expect_error(
    mp_with_sl() %>%
      add.events(bad, event_group = "AEBODSYS", event = "AEDECOD"),
    "Column 'AEBODSYS' not found in dataset."
  )
})

test_that("add.events keeps keep_vars when present", {
  adae <- adae_min
  adae$EXTRA <- c("x", "y")

  mp <- mp_with_sl() %>%
    add.events(
      adae,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      keep_vars = "EXTRA"
    )

  expect_true("EXTRA" %in% names(mp$events))
})

test_that("add.events joins time-to-first columns when calc_time_to_first is TRUE", {
  mp <- mp_with_sl() %>%
    add.events(
      adae_min,
      event_group = "AEBODSYS",
      event = "AEDECOD",
      calc_time_to_first = TRUE
    )

  ttf_cols <- grep("^ttf_", names(mp$sl), value = TRUE)
  expect_true(length(ttf_cols) > 0L)
})
