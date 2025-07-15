test_that("Check if Megaplots is drawn correctly (with groups)", {
  set.seed(2006)
  test_B <- data.frame(
    megaplots_selected_subjectid = sample(c(10000,10001,10002,10003,10004,10005),125,replace = TRUE),
    megaplots_selected_event_time = sample(0:90, 125, replace = TRUE),
    pain_intensity = sample(c("1 - mild","2 - moderate", "3 - severe", NA),125,replace =TRUE),
    rm = sample(c("rescue medication",NA),125,replace = TRUE),
    sm = sample(c("study medication",NA),125, replace = TRUE)
  ) %>% dplyr::mutate(subject = (megaplots_selected_subjectid - 10000) + 1) %>%
    dplyr::arrange(megaplots_selected_subjectid, megaplots_selected_event_time) %>%
    dplyr::mutate(Group_ID = dplyr::case_when(subject %in% c(1,2,3) ~ 1,
                                              subject %in% c(4,5,6) ~ 2))

  test_megaplot_data <- list(
    A = data.frame(
      megaplots_selected_subjectid = c(10000,10001,10002,10003,10004,10005),
      megaplots_selected_start_time = c(-1,-1,-13,-2,-13,-6),
      megaplots_selected_end_time = c(104,54,98,96,98,90),
      sex = c("female","female","female","male","male","male"),
      trt = c("Megapill 2mg","Megapill 1mg","Megapill 2mg","Placebo","Megapill 1mg","Placebo"),
      SEQUENCING = c(10000,10001,10002,10003,10004,10005),
      subject = c(1,2,3,4,5,6),
      Group_ID = c(1,1,1,2,2,2),
      Group_ID_char = c("","female","","","male","")
    ),
    B = test_B,
    group = "sex",
    event = c("pain_intensity","rm","sm"),
    event.total = c("pain_intensity","rm","sm"),
    group.lev = list(sex = c("femaile", "male"), trt = c("Megapill 1mg","Megapill 2mg","Placebo")),
    event.lev = list(pain_intensity = c("1 - mild", "2 - moderate", "3 - severe"),
                     rm = c("rescue medication"),
                     sm = c("study medication")
                ),
    event.lev.n = list(pain_intensity = 3,
                     rm = 1,
                     sm = 1
                ),
    ai.tab = data.frame(),
    ai.initselect = c(""),
    sym.ev = c(15,18,16),
    char_A = c("sex", "trt"),
    char_B = c("pain_intensity","rm","sm"),
    nume_A = c("megaplots_selected_subjectid", "megaplots_selected_start_time", "megaplots_selected_end_time"),
    nume_B =  c("megaplots_selected_subjectid", "megaplots_selected_event_time"),
    max_sub2 = 200,
    col.ev = list(
        pain_intensity = c("1 - mild" = "#B3DE69", "2 - moderate" = "#FFED6F", "3 - severe" = "#FF7F00"),
        rm = c("rescue medication" = "#E31A1C"),
        sm = c("study medication" = "#CCEBC5")
    ),
    type.ev = list(
        pain_intensity = "#CAB2D6",
         rm = "#CAB2D6",
         sm = "#CAB2D6"
    )
  )


  tmp <- strsplit(c("female","male"), split = '::')
  grLab <- data.frame(
        'LABEL' = sapply(tmp,FUN = function(x) {paste(paste0("sex",'=',stringr::str_wrap(rev(x), width = 25, exdent = 5)), collapse = '\n')}),
        'POS' = plyr::ddply(test_megaplot_data$A,plyr::.(Group_ID), plyr::summarize,'POS' = mean(subject))$POS,
        stringsAsFactors = FALSE
      )


  p <- function() {
    draw_megaplot(
      megaplot_data = test_megaplot_data, #v
      select_color = c(
          'plot.bg' = '#404A4E',
          'plot.bg2' = '#40444a',
          'plot.lines' = '#000000',
          'plot.wp' = '#404A4E',
          'plot.id' = 'white',
          'axleg.bg' = '#222d32',
          'cont.bg' = '#222d32'
        ),
      par_settings = list(mar=c(0,5,0,12), grLab =  grLab),
      background_stripes = FALSE,
      background_stripes_length = 7,
      event_levels = c("pain_intensity = 1 - mild", "pain_intensity = 2 - moderate", "pain_intensity = 3 - severe",   "rm = rescue medication", "sm = study medication"),
      xlim =c(-15, 110),
      ylim = c(-0.5, 6.5),
      lines_instead_symbols = FALSE,
      line_width = 0.1,
      lines_options = "Adjacent",
      y_axis_label ="Subject identifier",
      reference_line_1 = TRUE,
      reference_line_1_value = 0,
      reference_line_2 = FALSE,
      reference_line_2_value =  0,
      reference_line_3 = FALSE,
      reference_line_3_value = 0,
      select_events = c("pain_intensity","rm","sm"),
      color_subject_line_by_first_event = FALSE
    )
  }
 vdiffr::expect_doppelganger("Mega plot grouped", p)
})


test_that("Check if Megaplots is drawn correctly (general)", {
  set.seed(2006)
  test_B <- data.frame(
    megaplots_selected_subjectid = sample(c(10000,10001,10002,10003,10004,10005),125,replace = TRUE),
    megaplots_selected_event_time = sample(0:90, 125, replace = TRUE),
    pain_intensity = sample(c("1 - mild","2 - moderate", "3 - severe", NA),125,replace =TRUE),
    rm = sample(c("rescue medication",NA),125,replace = TRUE),
    sm = sample(c("study medication",NA),125, replace = TRUE)
  ) %>% dplyr::mutate(subject = (megaplots_selected_subjectid - 10000) + 1) %>%
    dplyr::arrange(megaplots_selected_subjectid, megaplots_selected_event_time) %>%
    dplyr::mutate(Group_ID = dplyr::case_when(subject %in% c(1,2,3) ~ 1,
                                              subject %in% c(4,5,6) ~ 2))

  test_megaplot_data <- list(
    A = data.frame(
      megaplots_selected_subjectid = c(10000,10001,10002,10003,10004,10005),
      megaplots_selected_start_time = c(-1,-1,-13,-2,-13,-6),
      megaplots_selected_end_time = c(104,54,98,96,98,90),
      sex = c("female","female","female","male","male","male"),
      trt = c("Megapill 2mg","Megapill 1mg","Megapill 2mg","Placebo","Megapill 1mg","Placebo"),
      SEQUENCING = c(10000,10001,10002,10003,10004,10005),
      subject = c(1,2,3,4,5,6),
      Group_ID = c(1,1,1,1,1,1),
      Group_ID_char = c("","","","","","")
    ),
    B = test_B,
    group = "",
    event = c("pain_intensity","rm","sm"),
    event.total = c("pain_intensity","rm","sm"),
    group.lev = list(sex = c("femaile", "male"), trt = c("Megapill 1mg","Megapill 2mg","Placebo")),
    event.lev = list(pain_intensity = c("1 - mild", "2 - moderate", "3 - severe"),
                     rm = c("rescue medication"),
                     sm = c("study medication")
                ),
    event.lev.n = list(pain_intensity = 3,
                     rm = 1,
                     sm = 1
                ),
    ai.tab = data.frame(),
    ai.initselect = c(""),
    sym.ev = c(15,18,16),
    char_A = c("sex", "trt"),
    char_B = c("pain_intensity","rm","sm"),
    nume_A = c("megaplots_selected_subjectid", "megaplots_selected_start_time", "megaplots_selected_end_time"),
    nume_B =  c("megaplots_selected_subjectid", "megaplots_selected_event_time"),
    max_sub2 = 200,
    col.ev = list(
        pain_intensity = c("1 - mild" = "#B3DE69", "2 - moderate" = "#FFED6F", "3 - severe" = "#FF7F00"),
        rm = c("rescue medication" = "#E31A1C"),
        sm = c("study medication" = "#CCEBC5")
    ),
    type.ev = list(
        pain_intensity = "#CAB2D6",
         rm = "#CAB2D6",
         sm = "#CAB2D6"
    )
  )


  p <- function() {draw_megaplot(
    megaplot_data = test_megaplot_data, #v
    select_color = c(
        'plot.bg' = '#404A4E',
        'plot.bg2' = '#40444a',
        'plot.lines' = '#000000',
        'plot.wp' = '#404A4E',
        'plot.id' = 'white',
        'axleg.bg' = '#222d32',
        'cont.bg' = '#222d32'
      ),
    par_settings = par(mar=c(0,5,0,12)),
    background_stripes = FALSE,
    background_stripes_length = 7,
    event_levels = c("pain_intensity = 1 - mild", "pain_intensity = 2 - moderate", "pain_intensity = 3 - severe",   "rm = rescue medication", "sm = study medication"),
    xlim =c(-15, 110),
    ylim = c(-0.5, 6.5),
    lines_instead_symbols = FALSE,
    line_width = 0.1,
    lines_options = "Adjacent",
    y_axis_label ="Subject identifier",
    reference_line_1 = TRUE,
    reference_line_1_value = 0,
    reference_line_2 = FALSE,
    reference_line_2_value =  0,
    reference_line_3 = FALSE,
    reference_line_3_value = 0,
    select_events = c("pain_intensity","rm","sm"),
    color_subject_line_by_first_event = FALSE
)
  }
 vdiffr::expect_doppelganger("Mega plot general", p)
})
