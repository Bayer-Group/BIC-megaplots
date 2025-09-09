ggsurv <- function(
    s,
    CI = 'def',
    plot.cens = T,
    surv.col = 'gg.def',
    cens.col = 'black',
    lty.est = 1,
    lty.ci = 2,
    cens.shape = 3,
    back.white = F,
    xlab = 'Time',
    ylab = 'Survival',
    main = ''){

  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)

  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'black', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){

    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)

    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)

    pl <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = surv)) +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(main) +
      ggplot2::geom_step(col = col, lty = lty.est)

    pl <- if(CI == T | CI == 'def') {
      pl + ggplot2::geom_step(ggplot2::aes(y = up), color = "#f2f2f220", lty = lty.ci) +
         ggplot2::geom_step(ggplot2::aes(y = low), color = "#f2f2f220", lty = lty.ci) +
        ggplot2::geom_rect(ggplot2::aes(ymin=low, ymax= up, xmin = time, xmax = dplyr::lead(time)), fill = "#f2f2f2", alpha = 0.1)
    } else (pl)

    pl <- if (plot.cens == T & length(dat.cens) > 0) {
      pl + ggplot2::geom_point(data = dat.cens, ggplot2::aes(y = surv), shape = cens.shape,
                      col = col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)

    pl <- if(back.white == T) {pl + ggplot2::theme_bw()
    } else (pl)
    pl
  }
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = "black", lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata

    groups <- factor(unlist(strsplit(names(s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]

    for(i in 1:strata) {
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }

    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)

    pl <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = surv, group = group)) +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(main) +
      ggplot2::geom_step(ggplot2::aes(col = group, lty = group))

    col <- if(length(surv.col == 1)){
      ggplot2::scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      ggplot2::scale_colour_manual(name = gr.name, values = surv.col)
    }

    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + ggplot2::scale_colour_discrete(name = gr.name)}

    line <- if(length(lty.est) == 1){
      ggplot2::scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {
      ggplot2::scale_linetype_manual(name = gr.name, values = lty.est)
    }

    # pl <- pl + line

    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]) {
        # + ggplot2::geom_step(ggplot2::aes(y = up), color = "#f2f2f2", lty = lty.ci) +
        #   ggplot2::geom_step(ggplot2::aes(y = low), color = "#f2f2f2", lty = lty.ci) +
        #   ggplot2::geom_rect(ggplot2::aes(ymin=low, ymax= up, xmin = time, xmax = dplyr::lead(time)), fill = "#f2f2f2", alpha = 0.1)

        pl + #ggplot2::geom_rect(ggplot2::aes(ymin=low, ymax= up, xmin = time, xmax = dplyr::lead(time)), fill = unique(surv.col), alpha = 0.1) +
          ggplot2::geom_step(ggplot2::aes(y = up, color = 'black'),color = "#f2f2f220", lty = lty.ci) +
          ggplot2::geom_step(ggplot2::aes(y = low, color = 'black'),color = "#f2f2f220", lty = lty.ci)
      } else{
        pl +  ggplot2::geom_step(ggplot2::aes(y = up, lty = group), col = surv.col) +
          ggplot2::geom_step(ggplot2::aes(y = low,lty = group), col = surv.col)+
          ggplot2::geom_rect(ggplot2::aes(ymin=low, ymax= up, xmin = time, xmax = dplyr::lead(time)), fill = unique(surv.col), alpha = 0.2)
      }
    } else {pl}


    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + ggplot2::geom_point(data = dat.cens, ggplot2::aes(y = surv), shape = cens.shape,
                      col = unique(surv.col))
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)

    pl <- if(back.white == T) {pl + ggplot2::theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}
