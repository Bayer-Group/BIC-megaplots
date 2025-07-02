#' Draw Megaplot main graph
#'
#' @param megaplot_data list with megaplot data A and B
#' @param select_color character vector with color definition ("plot.bg","plot.lines","plot.wp","plot.id","axleg.bg","cont.bg")
#' @param par_settings list with par settings ("mar","grLab")
#' @param background_stripes logical value if background stripes should be drawn
#' @param background_stripes_length integer value of background stripes length
#' @param event_levels character vector with event levels
#' @param xlim numeric vector with x-axis range
#' @param ylim numeric vector with y-axis range
#' @param lines_instead_symbols logical if lines should be displayed for events
#' @param line_width numeric value for line width for subject time lines
#' @param lines_options character for line option (two options available: Adjecent or Overlaying)
#' @param y_axis_label character for y axis label
#' @param reference_line_1 logical if  vertical reference line should be displayed
#' @param reference_line_1_value integer where the vertical reference line should be displayed
#' @param reference_line_2 logical if  vertical reference line should be displayed
#' @param reference_line_2_value integer where the vertical reference line should be displayed
#' @param reference_line_3 logical if  vertical reference line should be displayed
#' @param reference_line_3_value integer where the vertical reference line should be displayed
#' @param select_events character vector with event names
#' @param color_subject_line_by_first_event logical if only first event should be displayed as colored line
#'
#'

draw_megaplot <- function(
    megaplot_data,
    select_color,
    par_settings,
    background_stripes,
    background_stripes_length,
    event_levels,
    xlim,
    ylim,
    lines_instead_symbols,
    line_width,
    lines_options,
    y_axis_label,
    reference_line_1,
    reference_line_1_value,
    reference_line_2,
    reference_line_2_value,
    reference_line_3,
    reference_line_3_value,
    select_events,
    color_subject_line_by_first_event
){

    #### Plot settings ####
    # set general plot settings in par function
    par(mar = par_settings$mar, bg = select_color['plot.bg'], lheight = 0.8)

    # x- and y-limits
    # xlim <- c(range[1], range[2])
    # ylim <- range(megaplot_data$A$subject) + c(-1.5, 1.5)

    #### Initialize plot ####
    plot(NULL, xlim = xlim, ylim = ylim, xlab = '', ylab = '', axes = FALSE, yaxs = 'i')

    #### Draw background stripes ####
    # draw background stripes if selected within app and length is a non-negative integer value
    if (!is.null(background_stripes)) {
      if (background_stripes & is.numeric(background_stripes_length) & background_stripes_length > 0 & background_stripes_length %% 1 == 0) {
        min_tmp  <- min(megaplot_data$A$megaplots_selected_start_time)
        max_tmp <- max(megaplot_data$A$megaplots_selected_end_time)
        tmp <- seq(min_tmp, max_tmp, by = background_stripes_length)
        while (!(0 %in% tmp)) {
          min_tmp <- min_tmp - 1
          tmp <- seq(min_tmp, max_tmp + background_stripes_length, by = background_stripes_length)
        }
        for (i in seq(1, length(tmp), 2)) {
          rect( tmp[i], -1000, tmp[i + 1], 1000, col = select_color['cont.bg'], xpd = NA, border = NA)
        }
      }
    }

    #### Calculation of font/point size and plot ratio ####
    # calculate point and font size depending on screen ratio and subject identifier length
    rowHeightY <- strheight('A', units = 'user', cex = par('cex'))
    rowHeightX <- strwidth('A', units = 'user', cex = par('cex'))
    yxRatio <- rowHeightY / rowHeightX

    cex.subjLab <- (1.1 - (median(nchar(as.character(megaplot_data$A$megaplots_selected_subjectid)))*0.04))  / max(rowHeightY,0.5)

    cex.point <- 1.5 * par('cex') / rowHeightY * min(c(1, 0.95 * yxRatio))

    cex.point <- c(1, 0.92, 0.47, 0.8, 0.9) * cex.point

    #### Draw time lines ####
    # draw time line for every subject depending on start and end time
    rect(
      xleft = megaplot_data$A$megaplots_selected_start_time,
      xright = megaplot_data$A$megaplots_selected_end_time,
      ybottom = megaplot_data$A$subject - line_width,
      ytop = megaplot_data$A$subject + line_width,
      col = select_color[3],
      border = NA
    )

    #### Draw events as symbols ####
    # add events based on app selection
    if (!is.null(select_events) & !is.null(event_levels) & !(lines_instead_symbols)) {
      #for - loop over selected events
      for (i in 1:length(megaplot_data$event)) {
        tmp <- na.exclude(megaplot_data$B[, c('subject', 'megaplots_selected_event_time', megaplot_data$event[i])])
        tmp.col <- megaplot_data$col.ev[[megaplot_data$event[i]]]
        if (i == 1 & color_subject_line_by_first_event) {
          rect(
            xleft = tmp$megaplots_selected_event_time - 0.5,
            xright = tmp$megaplots_selected_event_time + 0.5,
            ybottom = tmp$subject - line_width,
            ytop = tmp$subject + line_width,
            border = NA,
            col = tmp.col[as.character(tmp[, megaplot_data$event[i]])]
          )
        } else {
          points(
            tmp$megaplots_selected_event_time,
            tmp$subject,
            pch = megaplot_data$sym.ev[i],
            cex = cex.point[i],
            col = tmp.col[as.character(tmp[, megaplot_data$event[i]])]
          )
        }
      }
    }

    #### Draw events as lines ####
    if (!is.null(select_events) & !is.null(event_levels) & lines_instead_symbols) {
      # two options available: Adjecent or Overlaying (selection within app)
      if (lines_options == "Adjacent") {
        tmp.height <- seq(-line_width, line_width, length = (length(megaplot_data$event) + 1))
        for (i in 1:length(megaplot_data$event)) {
          tmp <- na.exclude(megaplot_data$B[, c('subject', 'megaplots_selected_event_time', megaplot_data$event[i])])
          tmp.col <- megaplot_data$col.ev[[megaplot_data$event[i]]]
          rect(
            xleft = tmp$megaplots_selected_event_time - 0.5,
            xright = tmp$megaplots_selected_event_time + 0.5,
            ybottom = tmp$subject + tmp.height[i],
            ytop = tmp$subject + tmp.height[i + 1],
            border = NA,
            col = tmp.col[as.character(tmp[, megaplot_data$event[i]])]
          )
        }
      }
      if (lines_options == "Overlaying"){
         tmp.height <- seq(-line_width, line_width, length = (length(megaplot_data$event) + 1))
        for (i in 1:length(megaplot_data$event)) {
          tmp <- na.exclude(megaplot_data$B[, c('subject', 'megaplots_selected_event_time', megaplot_data$event[i])])
          tmp.col <- megaplot_data$col.ev[[megaplot_data$event[i]]]
          rect(
            xleft = tmp$megaplots_selected_event_time - 0.5,
            xright = tmp$megaplots_selected_event_time + 0.5,
            ybottom = tmp$subject - line_width,
            ytop = tmp$subject + line_width,
            border = NA,
            col = tmp.col[as.character(tmp[, megaplot_data$event[i]])]
          )
        }
      }
    }

    #### Draw y-axis label ####
    #Draw subject identifier for each line
    text(
      x = grconvertX(0.001, from = 'npc', to = 'user'),
      y = megaplot_data$A$subject,
      xpd = NA,
      adj = c(1, 0.5),
      cex = cex.subjLab,
      labels = megaplot_data$A[, 'megaplots_selected_subjectid'],
      col = select_color[5]
    )

    # draw y-axis description
    text(
      x = grconvertX(0.001, from = 'npc', to = 'user'),
      y = c(0, megaplot_data$A$subject[length(megaplot_data$A$subject)] + 1),
      xpd = NA,
      adj = c(0.5, 0.5),
      cex = cex.subjLab,
      labels = y_axis_label,
      col = select_color['plot.id']
    )


    #### Draw reference line(s)####
    # up to three ref lines selectable within app
    for(i in 1:3) {
      if(eval(rlang::sym(paste0("reference_line_",i)))){
        rect(
          xleft = eval(rlang::sym(paste0("reference_line_",i,"_value"))) - 0.25,
          xright = eval(rlang::sym(paste0("reference_line_",i,"_value"))) + 0.25,
          ybottom = grconvertY(0, 'npc', 'user'),
          ytop = grconvertY(1, 'npc', 'user'),
          border = NA,
          col = rgb(1, 0, 0, alpha = 0.3)
        )
      }
    }

    #### Add grouping labels ####
      if (!is.null(par_settings$grLab)) {
        text(
          x = graphics::grconvertX(1.0025, from = 'npc', to = 'user'),
          y = par_settings$grLab$POS,
          xpd = NA,
          adj = c(0, 0.5),
          cex = 1.3,
          labels = par_settings$grLab$LABEL,
          col = select_color[5]
        )
      }
}
