test_that("Check if creating megaplot data via function 'prepare_megaplot_data' works", {

  test_megaplot_data_raw <- NULL
  test_upload_data_w_ids <- NULL
  test_select_sorting <- "megaplots_selected_subjectid"
  test_select_grouping <- "treatment"
  test_arrange_groups <- c("Treatment","Placebo")

  result <- prepare_megaplot_data(
    megaplot_data_raw = test_megaplot_data_raw,
    uploaded_data_w_ids = test_upload_data_w_ids,
    select_sorting = test_select_sorting,
    select_grouping = test_select_grouping,
    arrange_groups = test_arrange_groups
  )
  testthat::expect_equal(
    result, NULL
  )
})
