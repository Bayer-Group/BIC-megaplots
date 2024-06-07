test_that("data_specification file is as expected", {
  expect_snapshot(mod_data_specification_ui("id"))
})