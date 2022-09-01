#'
#' @title Point plot compliant with corporate identity
#'
#' @description A point plot compliant with the corporate identity.
#'
#' @param data A dataframe
#' @param x A numeric column of the dataframe used for x axis.
#' @param y A numeric column of the dataframe used for y axis.
#' @param color (optional) A categorical column of the dataframe used to create a color legend.
#' @param shape (optional) A categorical column of the dataframe used to create a shape legend.
#' @param size (optional) A numeric column of the dataframe used to create a size legend.
#' @param size.fixed Size of the points. Used only if size is missing (default: 1.5).
#' @param facet (optional) A categorical column of the dataframe. One chart will be created for each category.
#' @param color.values A vector of colors. If named, then the values will be matched based on the names.
#' @param x.accuracy A number to round the x axis text to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).
#' @param x.suffix A character string used as a suffix for x axis text.
#' @param y.accuracy A number to round the y axis text to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).
#' @param y.suffix A character string used as a suffix for y axis text.
#' @param label A categorical column of the dataframe used to label the points.
#' @param label.filter A character string used to filter the points to be labelled (e.g. if label.filter = "column_name == 2", then only observations where column_name is equal to 2 will be labelled).
#' @param label.size A numeric value indicating the labels size.
#' @param label.color A character string used to set the label color. By default, labels are the same color as points if __color__ is filled, or black if __color__ is missing.
#' @param label.hjust A numeric value used for the horizontal adjustment of the label position. The unit is the same as for the data unit on the x-axis.
#' @param label.vjust A numeric value used for the vertical adjustment of the label position. The unit is the same as for the data unit on the y-axis.
#' @param label.repel.force A numeric value used as force of repulsion between overlapping text labels (defaults to 1).
#' @param title A character string for the title of the chart.
#' @param subtitle A character string for the subtitle of the chart.
#' @param x.name A character string for the x axis title.
#' @param y.name A character string for the y axis title.
#' @param legend.color.name A character string for the color legend title.
#' @param legend.shape.name A character string for the shape legend title.
#' @param legend.size.name A character string for the size legend title.
#'
#' @return A ggplot2 pointplot (geom_point) compliant with the corporate identity.
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import ggrepel
#'
#' @examples library(dplyr)
#' @examples mtcars %>%
#' @examples   pointplot(x = mpg,
#' @examples                 y = qsec,
#' @examples                 x.name = "Miles per gallon",
#' @examples                 y.name = "1/4 mile time",
#' @examples                 title = "Mile time",
#' @examples                 subtitle = "According to miles per gallon")
#' @examples #
#' @examples mtcars %>%
#' @examples   mutate(am = case_when(am == 1 ~ "Automatic", TRUE ~ "Manual")) %>%
#' @examples   pointplot(x = mpg,
#' @examples                 y = qsec,
#' @examples                 color = cyl,
#' @examples                 label = gear,
#' @examples                 label.filter = "cyl == 6",
#' @examples                 x.name = "Miles per gallon",
#' @examples                 y.name = "1/4 mile time",
#' @examples                 legend.color.name = "Cylinders",
#' @examples                 title = "Mile time",
#' @examples                 subtitle = "According to miles per gallon")
#'
#' @export
pointplot <- function(data,
                      x,
                      y,
                      color,
                      shape,
                      size,
                      size.fixed = 1.5,
                      facet,
                      color.values = NULL,
                      x.accuracy = 1,
                      x.suffix = NULL,
                      y.accuracy = 1,
                      y.suffix = NULL,
                      label,
                      label.filter,
                      label.size = 3,
                      label.color = NULL,
                      label.hjust = 0,
                      label.vjust = 0,
                      label.repel.force = 1,
                      title = NULL,
                      subtitle = NULL,
                      x.name = NULL,
                      y.name = NULL,
                      legend.color.name = NULL,
                      legend.shape.name = NULL,
                      legend.size.name = NULL){

  # extract variables and variable names
  # mutate x, color and facet into factor variables
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)

  if(!missing(size)){size <- dplyr::enquo(size)}

  if(!missing(color)){
    color <- dplyr::enquo(color)
    color_quo_name <- dplyr::quo_name(color)
    data <- data %>% dplyr::mutate(!!color_quo_name := as.factor(!!color))}

  if(!missing(shape)){
    shape <- dplyr::enquo(shape)
    shape_quo_name <- dplyr::quo_name(shape)
    data <- data %>% dplyr::mutate(!!shape_quo_name := as.factor(!!shape))}

  if(!missing(facet)){
    facet <- dplyr::enquo(facet)
    facet_quo_name <- dplyr::quo_name(facet)
    data <- data %>% dplyr::mutate(!!facet_quo_name := as.factor(!!facet))}

  if(!missing(label)){
    label <- dplyr::enquo(label)
    label_quo_name <- dplyr::quo_name(label)
    data <- data %>% dplyr::mutate(!!label_quo_name := as.factor(!!label))}

  if(!missing(label.filter)){
    data_text <- data %>% dplyr::filter(eval(parse(text=label.filter)))
  }else{
    data_text <- data
  }

  # start ploting
  p <- data %>%
    ggplot2::ggplot()

  # set aesthetics
  if(missing(size) & missing(color) & missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y)}
  if(!missing(size) & missing(color) & missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, size = !!size)}
  if(missing(size) & !missing(color) & missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, color = !!color)}
  if(missing(size) & missing(color) & !missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, shape = !!shape)}
  if(!missing(size) & !missing(color) & missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, size = !!size, color = !!color)}
  if(!missing(size) & missing(color) & !missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, size = !!size, shape = !!shape)}
  if(missing(size) & !missing(color) & !missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, color = !!color, shape = !!shape)}
  if(!missing(size) & !missing(color) & !missing(shape)){p <- p + ggplot2::aes(x = !!x, y = !!y, size = !!size, color = !!color, shape = !!shape)}

  # plot points
  if(missing(size)){p <- p + geom_point(size = size.fixed)
  }else{p <- p + geom_point()}

  # plot labels
  if(!missing(label)){
    if(missing(color) | !is.null(label.color)){
      if(is.null(label.color)){label.color <- "black"}
      p <- p + ggrepel::geom_text_repel(data = data_text, aes(x = !!x, y = !!y, label = !!label), color = label.color, size = label.size, nudge_x = label.hjust, nudge_y = label.vjust, force = label.repel.force, max.overlaps = 3, min.segment.length = 3)
    }else{
      p <- p + ggrepel::geom_text_repel(data = data_text, aes(x = !!x, y = !!y, label = !!label, color = !!color), size = label.size, nudge_x = label.hjust, nudge_y = label.vjust, force = label.repel.force, max.overlaps = 3, show.legend = FALSE, min.segment.length = 3)
    }
  }

  # set facet_grid
  if(!missing(facet)){p <- p + ggplot2::facet_grid(cols = vars(!!facet))}

  # end ploting
  p <- p +
    # set scales
    datamanir::scale_color_manual(values = color.values) +
    ggplot2::scale_x_continuous(labels = scales::label_number(accuracy = x.accuracy, suffix = x.suffix)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = y.accuracy, suffix = y.suffix)) +
    # set theme
    datamanir::theme(axis.title.x = element_text(),
                     axis.title.y = element_text(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.major.y = element_blank(),
                     axis.line = element_line(colour = "#1d1d1b", size = 0.5)
    ) +
    # set title and subtitle
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    # set x, y and legend labels
    ggplot2::labs(x = x.name, y = y.name, color = legend.color.name, shape = legend.shape.name, size = legend.size.name)

  # return the chart
  p
}

