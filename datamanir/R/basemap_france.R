#'
#' @title Create a basemap of metropolitan France
#'
#' @description Create ggplot layer representing the metropolitan France.
#'
#' @param fill Fill color
#' @param color Borders color
#' @param corse If TRUE, Corse is included into the basemap (default: FALSE)
#'
#'
#' @return a ggplot layer representing the metropolitan France (WGS84 coords)
#'
#' @import ggplot2
#' @import sf
#' @import dplyr
#'
#' @examples library(ggplot2)
#' @examples data <- data.frame(x = c(2.3, -2.5), y = c(48.8, 48.5), couleur = c("red","green"))
#' @examples basemap_france() +
#' @examples   geom_point(data = data, aes(x = x, y = y, color = couleur))
#'
#' @export
basemap_france <- function(fill = "#F0F0F0", color = "white", corse = FALSE){

  # selectionner les regions à inclure
  if(corse == FALSE){list_region <- c("84","27","53","24","44","32","11","28","75","76","52","93")}
  if(corse == TRUE){list_region <- c("84","27","53","24","44","32","11","28","75","76","52","93","94")}

  # creer la visualisation ggplot
  ggplot() +
    geom_sf(data = datamanir::map_region_wgs84 %>%
              filter(INSEE_REG %in% list_region)
            , fill = fill, colour = color) +
    theme(
      legend.key = element_blank(),
      panel.background = element_rect(fill = "white"),
      legend.position = 'right',
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
}
