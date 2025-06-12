suppressPackageStartupMessages(library(shiny))

colChoice <- list(
    'color palette 1'=list(
      'col'=c("#a6cee3","#cab2d6","#b2df8a","#fb9a99","#fdbf6f","#1f78b4","#6a3d9a","#ff7f00"),
      'gradient'=FALSE
    ),
    'color palette 2'=list(
      'col'=c("#e31a1c","#ffff99","#33a02c","#b15928","#8dd3c7","#ffffb3","#bebada","#fb8072"),
      'gradient'=FALSE
    ),
    'color palette 3'=list(
      'col'=c("#ccebc5","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#80b1d3","#ffed6f"),
      'gradient'=FALSE
    ),
    'color palette 4'=list(
      'col'=RColorBrewer::brewer.pal(8,'Pastel1'),
      'gradient'=FALSE
    ),
    'color gradient red'=list(
      'col'=colorRampPalette(c('firebrick1','firebrick4'))(8),
      'gradient'=TRUE
    ),
    'color gradient coral'=list(
      'col'=colorRampPalette(c('coral1','coral4'))(8),
      'gradient'=TRUE
    ),
    'color gradient pink'=list(
      'col'=colorRampPalette(c('deeppink1','deeppink4'))(8),
      'gradient'=TRUE
    ),
    'color gradient purple'=list(
      'col'=colorRampPalette(c('darkorchid1','darkorchid4'))(8),
      'gradient'=TRUE
    ),
    'color gradient yellow'=list(
      'col'=colorRampPalette(c('yellow1','yellow4'))(8),
      'gradient'=TRUE
    ),
    'color gradient gold'=list(
      'col'=colorRampPalette(c('gold1','gold4'))(8),
      'gradient'=TRUE
    ),
    'color gradient orange'=list(
      'col'=colorRampPalette(c('orange1','orange4'))(8),
      'gradient'=TRUE
    ),
    'color gradient green'=list(
      'col'=colorRampPalette(c('seagreen1','seagreen4'))(8),
      'gradient'=TRUE
    ),
    'color gradient bright green'=list(
      'col'=colorRampPalette(c('chartreuse1','chartreuse4'))(8),
      'gradient'=TRUE
    ),
    'color gradient bright palegreen'=list(
      'col'=colorRampPalette(c('green1','green4'))(8),
      'gradient'=TRUE
    ),
    'color gradient aquamarine'=list(
      'col'=colorRampPalette(c('aquamarine1','aquamarine4'))(8),
      'gradient'=TRUE
    ),
    'color gradient aquamarine'=list(
      'col'=colorRampPalette(c('cyan1','cyan4'))(8),
      'gradient'=TRUE
    ),
    'color gradient blue'=list(
      'col'=colorRampPalette(c('steelblue1','steelblue4'))(8),
      'gradient'=TRUE
    ),
    'color gradient blue'=list(
      'col'=colorRampPalette(c('royalblue1','royalblue4'))(8),
      'gradient'=TRUE
    ),
    'color gradient bright blue'=list(
      'col'=colorRampPalette(c('blue1','blue4'))(8),
      'gradient'=TRUE
    ),
    'color gradient antiquewh'=list(
      'col'=colorRampPalette(c('antiquewhite1','antiquewhite4'))(8),
      'gradient'=TRUE
    ),
    'color gradient snow'=list(
      'col'=colorRampPalette(c('snow1','snow4'))(8),
      'gradient'=TRUE
    ),
    'color gradient grey'=list(
      'col'=colorRampPalette(c('grey50','black'))(8),
      'gradient'=TRUE
    )
  )


dark_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#0091DF",
    black = "#222d32"#
  ),
  fresh::adminlte_sidebar(
    width = "275px",
    dark_bg = "#222d32",#Background color (dark mode).
    dark_hover_bg = "#0091DF",#Background hover color (dark mode).
    dark_color = "#dce4e8",#Text color (dark mode).
    dark_hover_color ="#dce4e8",#Text hover color (dark mode).
  ),
  fresh::adminlte_global(
    content_bg = "#404A4E", # Background color of the body.
    box_bg = "#222d32",#Default background color for boxes.
    info_box_bg = "#222d32"#Default background color for info boxes.
  )
)

light_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#0091DF"# "#0091DFFF"#v
  ),
  fresh::adminlte_sidebar(
    dark_bg = "#222d32",#Background color (dark mode).
    dark_hover_bg = "#0091DF",#Background hover color (dark mode).
    dark_color = "#dce4e8",#Text color (dark mode).
    dark_hover_color ="#dce4e8",#Text hover color (dark mode).
  ),
  fresh::adminlte_global(
    content_bg = "#dce4e8", # Background color of the body.
    box_bg = "#222d32",#Default background color for boxes.
    info_box_bg = "#222d32"#Default background color for info boxes.
  )
)


utils::globalVariables(c('colChoice','megaplots_demo_data','dark_theme'))
