require(here)

test_that("Function throws error for unsupported file formats", {
  expect_error(read_dataset(here("tests/testthat/unsupported_test_file.txt")),
               "Unsupported file format. Please provide a SAS, CSV, or RData file.")
})
