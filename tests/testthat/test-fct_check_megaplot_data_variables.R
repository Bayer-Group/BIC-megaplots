# test_that("function for varialbe checks work", {
  #function should only returns TRUE or FALSE

  # tmp <- data.frame(
  #   subject_id = 1:5,
  #   subject_id_char = as.character(101:105),
  #   subject_id_num = 1.5:5.5,
  #   subject_id_alpha = c("a","b","c","d","e"),
  #   start_tme = 1:5,
  #   start_tme_char = as.character(1:5),
  #   end_tme = 101:105,
  #   end_tme_char = as.character(101:105),
  #   evnt = c("Event 1","Event 2", "Event 3", "Event 4", "Event 5"),
  #   evnt_num = 1:5,
  #   evnt_group = c("Event Group 1", "Event Group 1", "Event Group 1", "Event Group 2", "Event Group 2"),
  #   evnt_group_num = c(1,1,1,2,2),
  #   ev_time_start = c(10,20,30,40,50),
  #   ev_time_start_char = as.character(c(10,20,30,40,50)),
  #   ev_time_end = c(60,70,80,90,100),
  #   ev_time_end_char = as.character(c(60,70,80,90,100)),
  #   logical_val = c(TRUE,FALSE,TRUE,FALSE,TRUE)
  #   )
  #
  # # megaplot_data should be a data.frame (or tibble)
  #
  # # REQUIRED
  # # subjectid can be numeric or character
  # # event can be numeric or character
  #
  # # OPTIONAL
  # # event_group
  #
  # # ONE OF THESE COMBINATIONS ARE REQUIRED
  # # start_time should be integer (or at least numeric -> will be rounded off)
  # # end_time should be integer (or at least numeric -> will be rounded off)
  #
  # # event_time should be integer (or at least numeric -> will be rounded off)
  # # event_time_end should be integer (or at least numeric -> will be rounded off)
  #
  #
  # # expect TRUE (fine variable check)
  # expect_equal(check_megaplot_data_variables(
  #   megaplot_data = tmp,
  #   subjectid = "subject_id",
  #   start_time = "start_tme",
  #   end_time = "end_tme",
  #   event = "evnt",
  #   event_group = "evnt_group",
  #   event_time = "ev_time_start",
  #   event_time_end = "ev_time_end"
  # ), TRUE)
  #
  #
  # # different subejctid formats
  # expect_equal(check_megaplot_data_variables(
  #   megaplot_data = tmp,
  #   subjectid = "subject_id_char",
  #   start_time = "start_time",
  #   end_time = "end_time",
  #   event = "event",
  #   event_group = "event_group",
  #   event_time = "ev_time_start",
  #   event_time_end = "ev_time_end"
  # ), TRUE)
# })
