test_that("Check if function 'create_color_container' works", {


  test_upload_data_w_ids <- data.frame(
    megaplots_selected_event_group = c("Group 1", "Group 1", "Group 1","Group 2", "Group 2", "Group 2","Group 3", "Group 3", "Group 3","Group 3", "Group 3","Group 4", "Group 4", "Group 4"),
    megaplots_selected_event =c("Event 1", "Event 2", NA,"Event 3", "Event 4", NA,"Event 4", "Event 5","Event 6", "Event 7", NA, "Event 1", "Event 8", NA),
    event_id = c(1,2,0,1,2,0,1,2,3,4,0,1,2,0),
    event_group_id = c(1,1,1,2,2,2,3,3,3,3,3,4,4,4),
    max_event_id = c(2,2,2,2,2,2,4,4,4,4,4,2,2,2),
    event_n = rep(NA,14),
    n_flag = rep(NA,14),
    event_color = c("red","red","red","blue","blue","blue","yellow","yellow","yellow","yellow","yellow","green","green","green"),
    gradient_event_color_1 = c("red","red","#1D1F21" ,"blue","blue","#1D1F21","yellow","yellow","yellow","yellow","#1D1F21","green","green","#1D1F21"),
    gradient_event_color_2 = c(NA,NA,"#1D1F21" ,NA,NA,"#1D1F21",NA,NA,NA,NA,"#1D1F21", NA,NA, "#1D1F21"),
    gradient_event_color_3 = c(NA,NA,"#1D1F21" ,NA,NA,"#1D1F21",NA,NA,NA,NA,"#1D1F21", NA,NA, "#1D1F21"),
    jittered = TRUE
  )

  test_upload_data_w_ids


  result <- create_color_container(
    test_upload_data_w_ids |>
      dplyr::select(megaplots_selected_event, megaplots_selected_event_group) |>
      dplyr::distinct(),
    test_upload_data_w_ids,
    theme = "dark"
  )

  testthat::expect_equal("names_for_color_list" %in% colnames(result),TRUE)
  testthat::expect_equal("type_for_color" %in% colnames(result),TRUE)

})
