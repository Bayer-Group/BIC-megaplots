draw_megaplot_x_axis <- function(
      range,
      select_color,
      megaplot_axis_width,
      megaplot_width,
      plot_par_settings,
      axis_ticks,
      x_axis_label,
      reference_line_1,
      reference_line_1_value,
      reference_line_2,
      reference_line_2_value,
      reference_line_3,
      reference_line_3_value
){

  ax1.min <- shiny::req(range[1])
  ax1.max <- shiny::req(range[2])
  # calculate outer margin to account for the width of the scrollbar in the main plot
  scroll.px <- shiny::isolate(megaplot_axis_width) - shiny::isolate(megaplot_width)
  scroll.pct <- scroll.px / shiny::isolate(megaplot_axis_width)

  if (!is.na(scroll.pct)){
    if(scroll.pct <= 1 & scroll.pct >=0){
      par(
        mar = plot_par_settings$mar,
        bg = select_color['axleg.bg'],
        omd = c(0, 1 - scroll.pct, 0, 1)
      )
    } else {
      par(
        mar = plot_par_settings$mar,
        bg = select_color['axleg.bg'],
        omd = c(0, 1, 0, 1)
      )
    }
  }
  plot(
    0,
    0,
    xlim = c(ax1.min, ax1.max),
    ylim = c(0, 1),
    xlab = '',
    ylab = '',
    type = 'n',
    axes = FALSE
  )
  # use rectangle as background (the par() setting does not work on some devices)
  rect(
    xleft = grconvertX(0, 'ndc', 'user'),
    xright = grconvertX(1, 'ndc', 'user'),
    ybottom = grconvertY(0, 'ndc', 'user'),
    ytop = grconvertY(1, 'ndc', 'user'),
    xpd = NA,
    border = NA,
    col = select_color['axleg.bg']
  )

  ax1.ticks <- seq(ax1.min, ax1.max, 1)
  if (axis_ticks == TRUE)
    axis(
      1,
      at = ax1.ticks,
      labels = FALSE,
      tcl = -0.1,
      pos = grconvertY(0.9, from = 'nfc', to = 'user'),
      col = select_color['plot.id'],
      col.axis = select_color['plot.id']
    )
  axis(
    1,
    at = ax1.ticks[ax1.ticks %% 10 == 0],
    tcl = -0.4,
    pos = grconvertY(0.9, from = 'nfc', to = 'user'),
    col = select_color['plot.id'],
    col.axis = select_color['plot.id']
  )

  rowHeightY <- strheight('A', units = 'user', cex = par('cex'))
  cex.pt <-  0.3 * par('cex') / rowHeightY

  mtext(
    x_axis_label,
    side = 2,
    line = 1,
    adj = TRUE,
    las = 1,
    cex = cex.pt,
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
}
