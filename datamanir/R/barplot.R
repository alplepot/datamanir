#'
#' @title Barplot compliant with corporate identity
#'
#' @description A barplot compliant with the corporate identity.
#'
#' @param data A dataframe
#' @param x A categorical column of the dataframe (numeric variables are mutate into factors).
#' @param y (optional) A numeric column of the dataframe. It will be used to set the height of the bars.
#' @param color (optional, allowed if y is completed) A other categorical column of the dataframe. It will be used to create a color legend.
#' @param facet (optional, allowed if y is completed) A other categorical column of the dataframe. One chart will be created for each category.
#' @param weight (optional, allowed if y, color and facet are missing) A numeric column of the dataframe. It allows you to weight the observations.
#' @param x.order (optional) A vector of character string corresponding to the x variable levels. By default, bars will be sorted by descendind height.
#' @param y.accuracy A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).
#' @param y.suffix A character string used as a suffix for y axis text.
#' @param color.values A vector of colors. If named, then the values will be matched based on the names.
#' @param color.order A vector of character string corresponding to the color variable levels. By default, colors will be sorted by descending height.
#' @param facet.order (optional) A vector of character string corresponding to the facet variable levels. By default, facets will be sorted by descendind height.
#' @param label.accuracy A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).
#' @param label.suffix A character string used as a suffix for labels on the bars.
#' @param label.size A numeric value indicating the bar labels size.
#' @param title A character string for the title of the chart.
#' @param subtitle A character string for the subtitle of the chart.
#' @param x.name A character string for the x axis title.
#' @param y.name A character string for the y axis title.
#' @param legend.name A character string for the color legend title.
#'
#' @return A ggplot2 barplot (geom_bar or geom_col) compliant with the corporate identity.
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import forcats
#'
#' @examples library(dplyr)
#' @examples # one input variable: x
#' @examples mtcars %>% barplot(x = mpg,
#' @examples                        weight = cyl,
#' @examples                        title = "Distribution of cylinders according to miles per gallon",
#' @examples                        subtitle = "It doesn't mean anything, does it ?",
#' @examples                        x.name = "Miles per gallon",
#' @examples                        y.name = "Number of cylinders")
#' @examples # two input variables: x and y
#' @examples mtcars %>%
#' @examples   group_by(cyl) %>%
#' @examples   summarise(hp = mean(hp)) %>%
#' @examples   barplot(x = cyl,
#' @examples               y = hp,
#' @examples               title = "Gross horsepower",
#' @examples               subtitle = "according to the number of cylinders",
#' @examples               x.name = "Number of cylinders",
#' @examples              y.name = "Gross horsepower")
#' @examples # three input variables: x, y and color
#' @examples mtcars %>%
#' @examples   mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) %>%
#' @examples   group_by(cyl, am) %>%
#' @examples   summarise(nb_cars = n(), .groups = "keep") %>%
#' @examples   barplot(x = cyl,
#' @examples               y = nb_cars,
#' @examples               color = am,
#' @examples               title = "Distribution of cars",
#' @examples               subtitle = "according to the number of cylinders",
#' @examples               x.name = "Number of cylinders",
#' @examples               y.name = "NUmber of cars")
#' @examples # three input variables: x, y and facet
#' @examples mtcars %>%
#' @examples   mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) %>%
#' @examples   group_by(cyl, am) %>%
#' @examples   summarise(nb_cars = n(), .groups = "keep") %>%
#' @examples   barplot(x = cyl,
#' @examples               y = nb_cars,
#' @examples               facet = am,
#' @examples               title = "Distribution of cars",
#' @examples               subtitle = "according to the transmission and the number of cylinders",
#' @examples               x.name = "Number of cylinders",
#' @examples               y.name = "Number of cars")
#' @examples # four input variables: x, y, color and facet
#' @examples mtcars %>%
#' @examples   mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) %>%
#' @examples   group_by(cyl, am, gear) %>%
#' @examples   summarise(nb_cars = n(), .groups = "keep") %>%
#' @examples   barplot(x = cyl,
#' @examples               y = nb_cars,
#' @examples               color = gear,
#' @examples               facet = am,
#' @examples               title = "Distribution of cars",
#' @examples               subtitle = "according to the transmission and other things",
#' @examples               x.name = "Number of cylinders",
#' @examples               y.name = "Number of cars",
#' @examples               legend.name = "Number of gears")
#'
#' @export
barplot <- function(data,
                        x,
                        y,
                        color,
                        facet,
                        weight = NULL,
                        x.order,
                        y.accuracy = 1,
                        y.suffix = NULL,
                        color.values = NULL,
                        color.order,
                        facet.order,
                        label.accuracy = 1,
                        label.suffix = NULL,
                        label.size = 0,
                        title = NULL,
                        subtitle = NULL,
                        x.name = NULL,
                        y.name = NULL,
                        legend.name = NULL){

  # extract variables and variable names
  # mutate x, color and facet into factor variables
  x <- dplyr::enquo(x)
  x_quo_name <- dplyr::quo_name(x)
  data <- data %>% dplyr::mutate(!!x_quo_name := as.factor(!!x))

  if(!missing(y)){
    y <- dplyr::enquo(y)
    y_quo_name <- dplyr::quo_name(y)}

  if(!missing(color)){
    color <- dplyr::enquo(color)
    color_quo_name <- dplyr::quo_name(color)
    data <- data %>% dplyr::mutate(!!color_quo_name := as.factor(!!color))}

  if(!missing(facet)){
    facet <- dplyr::enquo(facet)
    facet_quo_name <- dplyr::quo_name(facet)
    data <- data %>% dplyr::mutate(!!facet_quo_name := as.factor(!!facet))}

  if(!missing(weight)){weight <- dplyr::enquo(weight)}

  # set the order of the stacked bars (color order)
  if(!missing(y) & !missing(color)){
    if(missing(color.order)){
      color_order <- data %>%
        group_by(!!color) %>%
        summarize(y_total = sum(!!y, na.rm = TRUE)) %>%
        arrange(desc(y_total)) %>%
        pull(!!color)
    }else{
      if(is.vector(color.order)){
        color_order <- color.order
      }
    }
  }

  # initialize the plot
  p <- data %>%
    ggplot2::ggplot()

  # plot chart with only one variable (numeric or categorical)
  if(missing(y) & missing(color) & missing(facet)){
    p <- p +
      ggplot2::aes(x = !!x, weight = as.numeric(!!weight)) +
      ggplot2::geom_bar(fill = datamanir::color_vector("blue"))
  }else{
    # for charts with two or more variables
    if(missing(x.order)){
      p <- p + ggplot2::aes(x = forcats::fct_reorder(!!x, desc(!!y)))
    }else{
      p <- p + ggplot2::aes(x = factor(!!x, levels = x.order))
    }
  }

  # for charts with facets
  if(!missing(facet)){
    if(missing(facet.order)){
      p <- p + ggplot2::facet_grid(cols = vars(forcats::fct_reorder(!!facet, desc(!!y))))
    }else{
      p <- p + ggplot2::facet_grid(cols = vars(factor(!!facet, levels = facet.order)))
    }
  }

  # set bar labels
  if(label.size > 0){
    p <- p + ggplot2::aes(label = paste0(scales::label_number(accuracy = label.accuracy)(!!y), label.suffix))
  }else{
    p <- p + ggplot2::aes(label = NA)
  }

  # plot chart with 2 input variables (one numeric and one categorical)
  if(!missing(y) & missing(color) & missing(facet)){
    p <- p +
      aes(y = !!y, fill = !!x) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_text(vjust = 1.1, color = "white", size = label.size) +
      ggplot2::guides(fill = "none")
    }

  # plot chart with 3 input variables (one numeric and two categorical: x and facet)
  if(!missing(y) & missing(color) & !missing(facet)){
    p <- p +
      ggplot2::aes(y = !!y, fill = forcats::fct_reorder(!!facet, !!y), alpha = !!x) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_text(vjust = 1.1, color = "white", size = label.size) +
      ggplot2::scale_alpha_discrete(range = c(0.6, 1)) +
      ggplot2::guides(fill = "none", alpha = "none")
  }

  # plot chart with 3 input variables (one numeric and two categorical: x and color)
  if(!missing(y) & !missing(color) & missing(facet)){
    p <- p +
      ggplot2::aes(y = !!y, fill = factor(!!color, levels = color_order)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_text(color = "white", size = label.size, position = position_stack(vjust = 0.5))
  }

  # plot chart with 4 input variables (one numeric and three categorical)
  if(!missing(y) & !missing(color) & !missing(facet)){
    p <- p +
      ggplot2::aes(y = !!y, fill = factor(!!color, levels = color_order)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_text(color = "white", size = label.size, position = position_stack(vjust = 0.5))
  }

  # add a title and a subtitle
  p <- p +
    datamanir::scale_fill_manual(values = color.values) +
    # set scale y
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = y.accuracy, suffix = y.suffix)) +
    # set title and subtitle
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    # set theme
    datamanir::theme(axis.title.x = element_text(),
                   axis.title.y = element_text(),
                   panel.grid.major.x = element_blank()) +
    # set x, y and legend labels
    ggplot2::labs(x = x.name, y = y.name, fill = legend.name)

  # return the chart
  p
}

