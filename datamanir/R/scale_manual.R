#'
#' @title Create your own discrete scale
#'
#' @description These functions allow you to specify your own set of mappings from levels in the data to aesthetic values, using corporate colors.
#'
#' @param values A vector of character strings corresponding to corporate colors or other hexadecimal values (default: a set of corporate colors)
#' @param ... Any other argument accepted by __scale_color_manual__ ou __scale_fill_manual__ from ggplot2
#'
#' @return A discrete scale, just like __scale_color_manual__ ou __scale_fill_manual__ from ggplot2
#'
#' @examples library(dplyr)
#' @examples library(ggplot2)
#' @examples #
#' @examples p <- ggplot(mtcars, aes(mpg, wt)) +
#' @examples geom_point(aes(colour = factor(cyl)))
#' @examples #
#' @examples colors <- c("red","blue","#762147")
#' @examples names(colors) <- c("6", "4", "8")
#' @examples p + scale_color_manual()
#' @examples #
#' @examples p + scale_color_manual(values = colors, labels = c("four", "six", "heigth"), name = "Cylinders")
#'
#' @import ggplot2
#'
#' @name scale_manual
NULL

#' @rdname scale_manual
#' @export
scale_color_manual <- function(values = unname(datamanir::color_vector()), ...){
  if(missing(values)){
    ggplot2::scale_color_manual(..., values = values)
  }else{
    values_names <- names(values)
    values <- unname(datamanir::color_vector(values))
    names(values) <- values_names
    ggplot2::scale_color_manual(values = values, ...)
  }
}

#' @rdname scale_manual
#' @export
scale_fill_manual <- function(values = unname(datamanir::color_vector()), ...){
  if(missing(values)){
    ggplot2::scale_fill_manual(..., values = values)
  }else{
    values_names <- names(values)
    values <- unname(datamanir::color_vector(values))
    names(values) <- values_names
    ggplot2::scale_fill_manual(values = values, ...)
  }
}

