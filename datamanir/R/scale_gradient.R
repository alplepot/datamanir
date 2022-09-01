#'
#' @title Gradient color scales
#'
#' @description Create a 3 color gradient (low-mid-high) compliant with the corporate identity.
#'
#' @param low Color for low end of the gradient (default: lightgreen)
#' @param mid Color for the middle of the gradient (default: orange)
#' @param high Color for high end of the gradient (default: lightred)
#' @param ... Any other argument accepted by __scale_colour_gradient__ or __scale_fill_gradient__ from ggplot2
#'
#' @return A ggplot2 gradient scale, just like __scale_color_gradient__ or __scale_fill_gradient__
#'
#' @examples library(ggplot2)
#' @examples library(dplyr)
#' @examples mtcars %>%
#' @examples   ggplot() +
#' @examples   aes(x = mpg, y = disp, color = mpg) +
#' @examples   scale_color_gradient() +
#' @examples   geom_point() +
#' @examples   theme()
#'
#' @import ggplot2
#'
#' @name scale_gradient
NULL

#' @rdname scale_gradient
#' @export
scale_color_gradient <- function(low = "lightgreen", mid = "orange", high = "lightred", ...){
  scale_color_gradient2(low = datamanir::color_vector(low),
                        mid = datamanir::color_vector(mid),
                        high = datamanir::color_vector(high),
                        ...)
}

#' @rdname scale_gradient
#' @export
scale_fill_gradient <- function(low = "lightgreen", mid = "orange", high = "lightred", ...){
  scale_fill_gradient2(low = datamanir::color_vector(low),
                       mid = datamanir::color_vector(mid),
                       high = datamanir::color_vector(high),
                       ...)
}
