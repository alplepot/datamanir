#'
#' @title Piechart compliant with corporate identity
#'
#' @description A piechart compliant with the corporate identity.
#'
#' @param data A dataframe
#' @param x A categorical column of the dataframe (numeric variables are mutate into factors).
#' @param y A numeric column of the dataframe. It will be used to set the width of the slices.
#' @param facet (optional) A other categorical column of the dataframe. One chart will be created for each category.
#' @param label.accuracy A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL (default value), the default, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values.
#' @param label.size A numeric value indicating the bar labels size.
#' @param title A character string for the title of the chart.
#' @param subtitle A character string for the subtitle of the chart.
#' @param legend.name A character string for the color legend title.
#'
#' @return A ggplot2 piechart compliant with the corporate identity.
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import forcats
#'
#' @examples mtcars %>%
#' @examples   group_by(cyl) %>%
#' @examples   summarise(nb_cars = n(), .groups = "keep") %>%
#' @examples   piechart(x = cyl,
#' @examples                y = nb_cars,
#' @examples                label.accuracy = 0.1,
#' @examples                title = "Distribution of cars",
#' @examples                subtitle = "according to the number of gears")
#' @examples #
#' @examples mtcars %>%
#' @examples   mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) %>%
#' @examples   group_by(am, cyl) %>%
#' @examples   summarise(nb_cars = n(), .groups = "keep") %>%
#' @examples   piechart(x = cyl,
#' @examples                y = nb_cars,
#' @examples                facet = am,
#' @examples                title = "Distribution of cars",
#' @examples                subtitle = "according to the transmission and the number of gears",
#' @examples                legend.name = "Number of gears")
#'
#' @export
piechart <- function(data,
                        x,
                        y,
                        facet,
                        label.accuracy = 1,
                        label.size = 5,
                        title = NULL,
                        subtitle = NULL,
                        legend.name = NULL){

  # extract variables and variable names
  # mutate x into factor
  x <- dplyr::enquo(x)
  x_quo_name <- dplyr::quo_name(x)
  data <- data %>% dplyr::mutate(!!x_quo_name := as.factor(!!x))

  y <- dplyr::enquo(y)
  y_quo_name <- dplyr::quo_name(y)

  # mutate y into a ratio of the total
  if(missing(facet)){
    data <- data %>% dplyr::group_by()
  }else{
    facet <- dplyr::enquo(facet)
    facet_quo_name <- dplyr::quo_name(facet)
    data <- data %>% dplyr::group_by(!!facet)
  }
  data <- data %>%
    dplyr::mutate(!!y_quo_name := !!y / sum(!!y, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # plot one piechart (facet is missing)
  if(missing(facet)){
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::aes(x="",
                   y = !!y,
                   fill = !!x,
                   label = paste0(!!x, "\n", scales::label_percent(accuracy = label.accuracy)(!!y))) +
      ggplot2::guides(fill = FALSE)
  }

  # plot several piecharts (one for each category of facet)
  if(!missing(facet)){
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::aes(x="",
                   y = !!y,
                   fill = !!x,
                   label = scales::label_percent(accuracy = label.accuracy)(!!y)) +
      ggplot2::facet_grid(cols = vars(!!facet)) +
      ggplot2::labs(fill = legend.name)
  }

  # complete the plotting
  p <- p +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::geom_text(color = "white", fontface = "bold", size = label.size, position = position_stack(0.5)) +
    datamanir::scale_fill_manual() +
    datamanir::theme(panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) +
    ggplot2::labs(title = title, subtitle = subtitle)

  # return the ggplot chart
  p

}
