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


color_func <- function(x,y,z,number_event_groups) {
  megaplot_color <- grDevices::rainbow(number_event_groups)
  if (x != 0 & z != 1) {
    return_colors <- grDevices::colorRampPalette(
      c(grDevices::colorRampPalette(c("white",megaplot_color[y]))(100)[50],
        megaplot_color[y],
        grDevices::colorRampPalette(c(megaplot_color[y],"black"))(100)[50]
      )
    ) (z)[x]
  }
  if(x == 0 | z == 1) {
    return_colors <- megaplot_color[y]
  }
  return(return_colors)
}

color_func2 <- function(x,y,z,col) {
  if (x != 0 & z != 1) {
    return_colors <- grDevices::colorRampPalette(
      c(grDevices::colorRampPalette(c("white",col))(100)[50],
        col,
        grDevices::colorRampPalette(c(col,"black"))(100)[50]
      )
    ) (z)[x]
  }
  if(x == 0 | z == 1) {
    return_colors <- col
  }
  return(return_colors)
}

get_trace_info <- function(plotly_object) {
  plotly_build_p <- plotly::plotly_build(plotly_object)
  plotly_build_data <- plotly_build_p$x$data
  trace_number <- length(plotly_build_data)
  trace_info <- data.frame(name = character(trace_number))
  trace_info <- plotly_build_data %>%
    seq_along() %>%
    purrr::map_dfr(
      ~{
        if(is.null(plotly_build_data[[.x]]$name)){
          .name=NA
        } else {
          .name=plotly_build_data[[.x]]$name
        }
        trace_info_data <- data.frame(
          name=.name,
          legendgroup=c(NA)
        )
        if(!is.null(plotly_build_data[[.x]]$legendgroup)) trace_info_data$legendgroup=plotly_build_data[[.x]]$legendgroup
        trace_info_data
      }
    )
  trace_info$trace <- 1:length(plotly_build_data)
  trace_info
}

apply_trace_info <- function(trace_info, plotly_object) {
  split_trace_info <- trace_info %>%
    split(trace_info$legendgroup)

  c(list(plotly_object), split_trace_info) %>%
    purrr::reduce(
      ~{
        plotly::style(
          .x,
          legendgrouptitle=list(
            text=.y$legendgroup
          ),
          traces=.y$trace
        )}
    )
}


create_palette <- function(n, name) {
  if (name %in% c("Set2", "Pastel2", "Dark2", "Accent")) {
    max_n <- 8
  } else if (name %in% c("Pastel1","Set1")) {
    max_n <- 9
  } else if (name %in% c("Spectral")) {
    max_n <- 11
  } else if (name %in% c("Paired", "Set3")) {
    max_n <- 12
  } else {
    max_n <- NA
  }

  if (!is.na(max_n)) {
    selected_color_palette <- colorRampPalette(RColorBrewer::brewer.pal(max_n, name))(n)
  } else {
    if (name == "Rainbow") {
      selected_color_palette <- rainbow(n)
    } else {
    selected_color_palette <- NA
    }
  }
  return(selected_color_palette)
}

