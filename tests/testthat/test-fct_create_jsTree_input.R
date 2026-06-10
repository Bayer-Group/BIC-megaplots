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

test_that("create_jsTree_input returns a list", {
  test_data <- data.frame(
    megaplots_selected_event_group = c("Group1", "Group1", "Group2"),
    megaplots_selected_event = c("Event1", "Event2", "Event3")
  )

  result <- create_jsTree_input(test_data)

  expect_type(result, "list")
})

test_that("create_jsTree_input produces correct structure", {
  test_data <- data.frame(
    megaplots_selected_event_group = c("Group1", "Group1", "Group2"),
    megaplots_selected_event = c("Event1", "Event2", "Event3")
  )

  result <- create_jsTree_input(test_data)

  expect_equal(length(result), 2)  # Should have 2 groups

  expect_equal(result[[1]]$text, "Group1")
  expect_equal(length(result[[1]]$children), 2)  # Group1 should have 2 children

  expect_equal(result[[2]]$text, "Group2")
  expect_equal(length(result[[2]]$children), 1)  # Group2 should have 1 child
})



test_that("create_jsTree_input handles empty input", {
  test_data <- data.frame(
    megaplots_selected_event_group = character(),
    megaplots_selected_event = character()
  )

  result <- create_jsTree_input(test_data)

  expect_equal(result, list())  # Should return an empty list
})

test_that("create_jsTree_input handles single group with multiple events", {
  test_data <- data.frame(
    megaplots_selected_event_group = c("Group1", "Group1", "Group1"),
    megaplots_selected_event = c("Event1", "Event2", "Event3")
  )

  result <- create_jsTree_input(test_data)

  expect_equal(length(result), 1)  # Should have 1 group
  expect_equal(result[[1]]$text, "Group1")
  expect_equal(length(result[[1]]$children), 3)  # Group1 should have 3 children
})

test_that("create_jsTree_input handles duplicate events", {
  test_data <- data.frame(
    megaplots_selected_event_group = c("Group1", "Group1", "Group2", "Group2"),
    megaplots_selected_event = c("Event1", "Event1", "Event2", "Event2")
  )

  result <- create_jsTree_input(test_data)

  expect_equal(length(result), 2)  # Should have 2 groups
  expect_equal(result[[1]]$text, "Group1")
  expect_equal(length(result[[1]]$children), 2)

  expect_equal(result[[2]]$text, "Group2")
  expect_equal(length(result[[2]]$children), 2)
})
