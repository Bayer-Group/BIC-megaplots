test_that("Check if filtering megaplot data via function 'filter_megaplot_data' works", {

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

  test_select_sorting <- "megaplots_selected_subjectid"
  test_select_grouping <- NULL
  test_arrange_groups <- NULL

  test_megaplot_prepared_data <- prepare_megaplot_data(
    megaplot_data_raw = test_megaplot_data_raw,
    uploaded_data_w_ids = test_upload_data_w_ids,
    select_sorting = test_select_sorting,
    select_grouping = test_select_grouping,
    arrange_groups = test_arrange_groups
  )


  test_tree <- data.frame(
    megaplots_selected_event_group = c("Group 1", "Group 1", "Group 1"),
    megaplots_selected_event = c(NA, "Event 1", "Event 2")
  )
  test_color_data <-   test_upload_data_w_ids

  result <- filter_megaplot_data(
    tree = test_tree,
    megaplot_prepared_data = test_megaplot_prepared_data,
    color_data = test_color_data
  )


  testthat::expect_equal(dim(result)[1], 6)
  testthat::expect_equal(
    c("megaplots_selected_subjectid",  "megaplots_selected_start_time", "megaplots_selected_end_time",
       "megaplots_selected_event",          "megaplots_selected_event_group",    "megaplots_selected_event_time",
       "megaplots_selected_event_time_end", "subgroup_1",                        "subgroup_2",
       "subject_index",                     "event_group_id",                    "max_event_id",
       "event_n",                           "n_flag",                            "gradient_event_color_1",
       "gradient_event_color_2",            "gradient_event_color_3",            "group_index",
       "subjectid_n",                       "event_color" ,                      "jittered",
       "event_id",                         "jitter_event_time" ,                "subjectid_n_jittered" ,
       "unique_event") %in% colnames(result), rep(TRUE,25)
  )
  testthat::expect_equal(result$megaplots_selected_subjectid , c(1,1,1,2,2,NA))
  testthat::expect_equal(result$megaplots_selected_event , c("Event 1", "Event 1", "Event 2", "Event 1", "Event 2", NA))

})
