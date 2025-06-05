#' Draw Megaplot Legend
#'
#' @param megaplot_data list with megaplot data A and B
#' @param select_color character vector with color definition ("plot.bg","plot.lines","plot.wp","plot.id","axleg.bg","cont.bg")
#' @param par_settings list with par settings ("mar","grLab")
#' @param background_stripes logical value if background stripes should be drawn
#' @param background_stripes_length integer value of background stripes length
#' @param event_levels character vector with event levels
#' @param range numeric vector with x-axis range
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

draw_megaplot_legend <- function(
    megaplot_data, #v
    select_color#v
){

  par(mar = c(0, 0, 0, 0), bg = select_color['plot.bg'])

  plot(0,0,xlim = c(0, 1) ,ylim = c(0, 1),xlab = '',ylab = '',type = 'n',axes = FALSE)

  if (megaplot_data$event[1] != "NULL") {
    # starting coordinates
    legY <- grconvertY(1 / (2* length(megaplot_data$event)), from = 'ndc', to = 'user')
    legY_num <-  1/ (4*length(megaplot_data$event))
    legY_num_o <- 1/ (2*length(megaplot_data$event))
    legYmax_num <- 1
    legYmax <- grconvertY(1, from = 'ndc', to = 'user')
    legX <- grconvertX(0, from = 'npc', to = 'user')
    # calculate 'cex'
    heightMar3 <- grconvertY(1, from = 'ndc', to = 'user') - grconvertY(0, from = 'ndc', to = 'user')
    leg.test <- legend(legX, legY, xpd = NA, pch = 15, legend = 'Why', plot = FALSE)

    for (i in 1:length(megaplot_data$event)) {
      # set text color ('grey' if not shown in the plot)
      tmp <- megaplot_data$event.lev[[megaplot_data$event[i]]]
      col.legtxt <- rep(select_color['plot.id'], length(tmp))
      names(col.legtxt) <- tmp
      col.legtxt[!tmp %in% megaplot_data$event.lev[[megaplot_data$event[i]]]] <- c('grey40', '#93a3ae', '#5D6A70', '#404A4E')[3]
      font.legtxt <- ifelse(col.legtxt == '#5D6A70', 3, 1)
      col.leg <- megaplot_data$col.ev[[megaplot_data$event[i]]]

      # event name
      ltitle <- legend(
        legX,
        grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
        xjust = 0,
        yjust = 0.5,
        xpd = NA,
        bty = 'n',
        pch = NA,
        horiz = TRUE,
        col = NA,
        legend = "",
        text.col = select_color['plot.id'],
        cex = 1.1,
        text.font = 2
      )
      text(
        x = legX,
        y = grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
        xpd = NA,
        cex = 1.1,
        font = 2,
        adj = c(0, 0.5),
        labels = paste0(megaplot_data$event[i], ': '),
        col = select_color['plot.id']
      )
      legY_num <- legY_num + legY_num_o
      # legend
      lleft <- legX

      for (j in 1:length(col.leg)) {
        l <-
          legend(
            lleft[j],
            grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
            xjust = 0,
            yjust = 0.5,
            xpd = NA,
            bty = 'n',
            pch = megaplot_data$sym.ev[i],
            horiz = TRUE,
            col = col.leg[j],
            legend = names(col.leg)[j],
            text.col = col.legtxt[j],
            pt.cex = 2.5,
            cex = 1.1,
            text.font = font.legtxt[j]
          )
        lleft[j + 1] <- l$rect$left + l$rect$w
      }
      # modify y-coordinate for next legend
      legY_num <- legY_num + legY_num_o

    }
  }
}
