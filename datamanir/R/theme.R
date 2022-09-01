#'
#' @title Corporate theme
#'
#' @description A theme compliant with the corporate identity. At first use, it may be necessary to import Windows font styles (see examples).
#'
#' @param ... See __theme__ from __ggplot2__
#'
#' @return A ggplot2 theme with default values compliant with the corporate identity.
#'
#' @import ggplot2
#' @import extrafont
#'
#' @examples library(ggplot2)
#' @examples ggplot() + aes(x = c(1,2,3), y = c(1,2,3)) + geom_point() + theme_art()
#' @examples # At first use, it may be necessary to import Windows font styles:
#' @examples library(extrafont)
#' @examples font_import()
#'
#' @export
theme <- function(
  text = element_text(colour = "#1d1d1b", family = "Century Gothic", size = 10),
  plot.title = element_text(colour = "#1d1d1b", size = 11),
  plot.subtitle = element_text(colour = "#1d1d1b", size = 9, vjust = 4),
  panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid.major.x = element_line(colour = "#E1DDDD", size = 0.15),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y =  element_line(colour = "#E1DDDD", size = 0.15),
  panel.grid.minor.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.title = element_text(colour = "#1d1d1b", size = 10),
  legend.position = "right",
  rect = element_blank(),
  axis.ticks = element_blank(),
  ...){

  loadfonts(device = "win", quiet = TRUE)

  ggplot2::theme(
    text = text,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    panel.border = panel.border,
    panel.background = panel.background,
    panel.grid.major.x = panel.grid.major.x,
    panel.grid.minor.x = panel.grid.minor.x,
    panel.grid.major.y =  panel.grid.major.y,
    panel.grid.minor.y = panel.grid.minor.y,
    axis.title.x = axis.title.x,
    axis.title.y = axis.title.y,
    legend.title = legend.title,
    legend.position = legend.position,
    rect = rect,
    axis.ticks = axis.ticks,
    ...
  )
}
