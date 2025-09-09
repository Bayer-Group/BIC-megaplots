#' Draw Megaplot Legend
#'
#' @param megaplot_data list with megaplot data A and B
#' @param select_color character vector with color definition ("plot.bg","plot.lines","plot.wp","plot.id","axleg.bg","cont.bg")
#'

draw_megaplot_legend <- function(
    megaplot_data,
    select_color
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

    for (i in unique(megaplot_data$event)) {
      if (!identical(megaplot_data$event.lev[[i]], character(0))) {
      tmp <- megaplot_data$col.ev[[i]]
      col.legtxt <- rep(select_color['plot.id'], length(tmp))
      names(col.legtxt) <- names(tmp)
      col.legtxt[names(tmp)] <- '#5D6A70'
      font.legtxt <- ifelse(col.legtxt == '#5D6A70', 3, 1)
      col.leg <- megaplot_data$col.ev[[megaplot_data$event.total[i]]]

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
        text.col = "white",
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
        labels = paste0(i, ': '), ##
        col = select_color['plot.id']
      )
      legY_num <- legY_num + legY_num_o
      # legend
      lleft <- legX

      for (j in 1:length(megaplot_data$event.lev[[i]])) {
        l <-
          legend(
            lleft[j],
            grconvertY(legYmax_num - legY_num, from = 'ndc', to = 'user'),
            xjust = 0,
            yjust = 0.5,
            xpd = NA,
            bty = 'n',
            pch = megaplot_data$sym.ev[which(megaplot_data$event %in% i)],
            horiz = TRUE,
            col = megaplot_data$col.ev[[i]][j],
            legend = megaplot_data$event.lev[[i]][j],
            text.col = select_color['plot.id'],
            pt.cex = 2.5,
            cex = 1.1,
            text.font = 1
          )
        lleft[j + 1] <- l$rect$left + l$rect$w
      }
      # modify y-coordinate for next legend
      legY_num <- legY_num + legY_num_o
      }
    }
  }
}
