test_that("Check if function 'create_jsTree_input' works", {

  test_data <- data.frame(
    megaplots_selected_event_group = c("Event 1 (A)", "Event 2 (A)", "Event 1 (B)" ,"Event 2 (B)", "Event 3"),
    megaplots_selected_event = c(rep("Group 1",2),rep("Group 2", 3))
  )

  result <- create_jsTree_input(test_data)

  testthat::expect_equal(class(result), "list")
  testthat::expect_equal(length(result), 5)
  testthat::expect_equal(result[[1]]$text, "Event 1 (A)")
  testthat::expect_equal(result[[2]]$text, "Event 1 (B)")
  testthat::expect_equal(result[[3]]$text, "Event 2 (A)")
  testthat::expect_equal(result[[4]]$text, "Event 2 (B)")
  testthat::expect_equal(result[[5]]$text, "Event 3")
  testthat::expect_equal(result[[1]]$type, "root")
  testthat::expect_equal(result[[2]]$type, "root")
  testthat::expect_equal(result[[3]]$type, "root")
  testthat::expect_equal(result[[4]]$type, "root")
  testthat::expect_equal(result[[5]]$type, "root")
  testthat::expect_equal(result[[1]]$children[[1]]$text, "Group 1")
  testthat::expect_equal(result[[2]]$children[[1]]$text, "Group 2")
  testthat::expect_equal(result[[3]]$children[[1]]$text, "Group 1")
  testthat::expect_equal(result[[4]]$children[[1]]$text, "Group 2")
  testthat::expect_equal(result[[5]]$children[[1]]$text, "Group 2")
})
