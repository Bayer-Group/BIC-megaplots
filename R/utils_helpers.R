#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


font_color <- function (hex_code) {
  ifelse(
    ((grDevices::col2rgb(hex_code)[1] * 0.299) + (grDevices::col2rgb(hex_code)[2] * 0.587) + (grDevices::col2rgb(hex_code)[3] * 0.114) > 186),
    "#000000",
    "#ffffff"
  )
}


color_func <- function(x,y,z, megaplot_color =c(
  "#e43157", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
  "#a65628", "#f781bf", "#21d4de", "#91d95b", "#b8805f", "#cbbeeb",
  "#ffffff", "#999999", "#aaffc3", "#ffd8b1", "#4363d8", "#000075",
  "#469990", "#808000", "#800000", "#bfef45", "#f032e6", "#fffac8",
  "#fabed4", "#4263d8"
)) {
  if (x != 0 & z != 1) {
    return_colors <- colorRampPalette(
      c(colorRampPalette(c("white",megaplot_color[y]))(100)[50],
        megaplot_color[y],
        colorRampPalette(c(megaplot_color[y],"black"))(100)[50]
      )
    ) (z)[x]
  }
  if(x == 0 | z == 1) {
    return_colors <- megaplot_color[y]
  }
  return(return_colors)
}
