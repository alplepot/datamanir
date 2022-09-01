#'
#' @title Boxplot compliant with corporate identity
#'
#' @description A boxplot compliant with the corporate identity.
#'
#' @param data A dataframe
#' @param x A numeric column of the dataframe
#' @param weight (optional) A numeric column of the dataframe. It allows you to weight the observations.
#' @param color (optional) A categorical column of the dataframe (numeric variables are mutate into factors). One boxplot will be printed for each category.
#' @param facet (optional) A second categorical column of the dataframe (numeric variables are mutate into factors). One boxplot will be printed for each combination of __color__ and __facet__.
#' @param x.accuracy A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).
#' @param color.values A vector of colors. If named, then the values will be matched based on the names (default: NULL).
#' @param color.order (optional) When __color__ is filled, boxplots are ordered in descending order by default. If __color.order__ is equal to "reverse", the boxplots will be ordered reversely. If is equal to a vector of character string corresponding to the __color__ variable levels, the boxplots will be ordered according to the vector.
#' @param outlier.size Size of the points representing the outliers. Set it to -1 in order to remove the outliers.
#' @param title A character string for the title of the chart.
#' @param subtitle A character string for the subtitle of the chart.
#'
#' @return A ggplot2 boxplot compliant with the corporate identity.
#'
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import spatstat
#'
#' @examples library(dplyr)
#' @examples # one input variable: x
#' @examples mtcars %>% boxplot(x = mpg, title = "Miles per gallon")
#' @examples # two input variables: x and color
#' @examples mtcars %>% boxplot(x = mpg, color = cyl, title = "Miles per gallon",
#' @examples                        subtitle = "according to the number of cylinders")
#' @examples # three input variables: x, color and facet
#' @examples mtcars %>%
#' @examples   mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) %>%
#' @examples   boxplot(x = mpg,
#' @examples               color = cyl,
#' @examples               facet = am,
#' @examples               title = "Miles per gallon",
#' @examples               subtitle = "according to the transmission and number of cylinders")
#'
#' @export
boxplot <- function(data,
                        x,
                        weight,
                        color,
                        facet,
                        x.accuracy = 1,
                        color.values = NULL,
                        color.order,
                        outlier.size = 1,
                        title = NULL,
                        subtitle = NULL){

  # extract variables and variable names
  x = dplyr::enquo(x)

  if(!missing(color)){
    color = dplyr::enquo(color)
    color_quo_name <- dplyr::quo_name(color)
    data <- data %>% dplyr::mutate(!!color_quo_name := as.factor(!!color))
    # set color order
    if(!missing(weight)){
      weight = dplyr::enquo(weight)
      color_order <- data %>%
        dplyr::group_by(!!color) %>%
        dplyr::summarize(x_med = spatstat.geom::weighted.median(!!x, !!weight, na.rm = TRUE)) %>%
        dplyr::arrange(desc(x_med)) %>%
        dplyr::pull(!!color)}
  }

  if(missing(weight)){
    weight <- NULL
    # set color order
    if(!missing(color)){
      color_order <- data %>%
        dplyr::group_by(!!color) %>%
        dplyr::summarize(x_med = median(!!x) , na.rm = TRUE) %>%
        dplyr::arrange(desc(x_med)) %>%
        dplyr::pull(!!color)}
  }

  # set color order according to color.order
  if(!missing(color) & !missing(color.order)){
    if(color.order == "reverse"){color_order <- rev(color_order)
    }else if(color.order == "alphabetical"){color_order <- data %>% dplyr::distinct(!!color) %>% dplyr::arrange(!!color) %>% dplyr::pull(!!color)
    }else{color_order <- color.order}
  }

  if(!missing(facet)){
    facet = dplyr::enquo(facet)
    facet_quo_name <- dplyr::quo_name(facet)
    data <- data %>% dplyr::mutate(!!facet_quo_name := as.factor(!!facet))}

  # with only one numeric variable
  if(missing(color) & missing(facet)){
    # plot chart
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = !!x, weight = !!weight) +
      ggplot2::geom_boxplot(fill = datamanir::color_vector("blue"), outlier.size = outlier.size) +
      ggplot2::scale_x_continuous(labels = scales::label_number(accuracy = x.accuracy)) +
      datamanir::theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.grid.major.y =  element_blank())
  }

  # with one numeric variable and one categorical variable
  if(!missing(color) & missing(facet)){
    # plot chart
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = factor(!!color, levels = color_order), y = !!x, fill = !!color, weight = !!weight) +
      ggplot2::geom_boxplot(outlier.size = outlier.size) +
      ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = x.accuracy)) +
      datamanir::theme()
  }

  # with one numeric variable and two categorical variables
  if(!missing(color) & !missing(facet)){
    # plot
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = factor(!!color, levels = color_order), y = !!x, fill = !!facet, weight = !!weight) +
      ggplot2::facet_grid(cols = vars(!!facet)) +
      ggplot2::geom_boxplot(outlier.size = outlier.size) +
      ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = x.accuracy)) +
      datamanir::theme()
  }

  # add a title and a subtitle
  p <- p +
    # set color scale
    datamanir::scale_fill_manual(values = color.values) +
    # set title
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    # drop legend
    ggplot2::guides(fill = "none")

  # return the chart
  p
}
