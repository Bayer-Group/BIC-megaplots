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


  test_megaplot_data_raw <- data.frame(
    megaplots_selected_subjectid = rep(1:4,each=4),
    megaplots_selected_start_time =  rep(1:4,each=4),
    megaplots_selected_end_time = rep(c(40,50,60,70),each=4),
    megaplots_selected_event = c("Event 1", "Event 1", "Event 2", "Event 3",
                                 "Event 1", "Event 2", "Event 3", "Event 4",
                                 "Event 4", "Event 5", "Event 6", "Event 7",
                                 "Event 1", "Event 8", "Event 8", "Event 8"),
    megaplots_selected_event_group = c("Group 1", "Group 1", "Group 1", "Group 2",
                                  "Group 1", "Group 1", "Group 2", "Group 2",
                                  "Group 3", "Group 3", "Group 3", "Group 3",
                                  "Group 4", "Group 4", "Group 4", "Group 4"),
    megaplots_selected_event_time =c(1:16),
    megaplots_selected_event_time_end = c(31:46),
    subgroup_1 = rep(c("Subgroup 1","Subgroup 2"), each = 8),
    subgroup_2 = rep(c("Subgroup A","Subgroup B", "Subgroup C", "Subgroup D"), each = 4)
  )

  ######
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
    gradient_event_color_3 = c(NA,NA,"#1D1F21" ,NA,NA,"#1D1F21",NA,NA,NA,NA,"#1D1F21", NA,NA, "#1D1F21")
  )

  test_select_sorting <- "megaplots_selected_subjectid"
  test_select_grouping <- NULL
  test_arrange_groups <- NULL

  result <- prepare_megaplot_data(
    megaplot_data_raw = test_megaplot_data_raw,
    uploaded_data_w_ids = test_upload_data_w_ids,
    select_sorting = test_select_sorting,
    select_grouping = test_select_grouping,
    arrange_groups = test_arrange_groups
  )

  testthat::expect_equal(dim(result)[1], 16)
  testthat::expect_equal(result$megaplots_selected_subjectid, rep(1:4, each = 4))
  #check if variable subject_index was created
  testthat::expect_equal("subject_index" %in% colnames(result), TRUE)
  testthat::expect_equal(result$subject_index, rep(1:4, each = 4))
  testthat::expect_equal("group_index" %in% colnames(result), TRUE)
  #check for selected groups (selection was NULL)
  testthat::expect_equal(result$group_index, rep(1,16))


  test_select_sorting <- "megaplots_selected_subjectid"
  test_select_grouping <- "subgroup_1"
  test_arrange_groups <- c("subgroup_1: Subgroup2", "subgroup_1: Subgroup1")

  result <- prepare_megaplot_data(
    megaplot_data_raw = test_megaplot_data_raw,
    uploaded_data_w_ids = test_upload_data_w_ids,
    select_sorting = test_select_sorting,
    select_grouping = test_select_grouping,
    arrange_groups = test_arrange_groups
  )

  testthat::expect_equal(dim(result)[1], 16)
  testthat::expect_equal(result$megaplots_selected_subjectid, rep(1:4, each = 4))
  #check if variable subject_index was created
  testthat::expect_equal("subject_index" %in% colnames(result), TRUE)
  testthat::expect_equal(result$subject_index, rep(1:4, each = 4))
  testthat::expect_equal("group_index" %in% colnames(result), TRUE)
  #check for selected groups (selection was NULL)
  testthat::expect_equal(result$group_index, rep(1:2, each = 8))

  test_select_grouping <- c("subgroup_1", "subgroup_2")

  test_arrange_groups <- c("subgroup_1: Subgroup1 & subgroup_2: SubgroupA",
    "subgroup_1: Subgroup1 & subgroup_2: SubgroupB",
    "subgroup_1: Subgroup2 & subgroup_2: SubgroupC",
    "subgroup_1: Subgroup2 & subgroup_2: SubgroupD"
  )

  result <- prepare_megaplot_data(
    megaplot_data_raw = test_megaplot_data_raw,
    uploaded_data_w_ids = test_upload_data_w_ids,
    select_sorting = test_select_sorting,
    select_grouping = test_select_grouping,
    arrange_groups = test_arrange_groups
  )
  testthat::expect_equal(dim(result)[1], 16)
  testthat::expect_equal(result$megaplots_selected_subjectid, rep(1:4, each = 4))
  #check if variable subject_index was created
  testthat::expect_equal("subject_index" %in% colnames(result), TRUE)
  testthat::expect_equal(sort(result$subject_index), rep(1:4, each = 4))
  testthat::expect_equal("group_index" %in% colnames(result), TRUE)
  #check for selected groups (selection was NULL)
  testthat::expect_equal(sort(result$group_index), rep(1:4, each = 4))

})

