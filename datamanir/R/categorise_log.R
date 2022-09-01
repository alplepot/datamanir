#'
#' @title Logarithmic categorization of a numeric variable
#'
#' @description Add a column to a dataframe, defined as a logarithmic categorization of a numeric variable you chose in this dataframe.
#'
#' @param df Dataframe to complete
#' @param column_to_categorize A numeric column of the dataframe, containing only positive numbers
#' @param number_of_intervals Number of categories to create
#' @param new_column_name Name of the new column (default : categories)
#'
#' @return A dataframe equals to the input, with one more column
#'
#' @import dplyr
#'
#' @examples library(dplyr)
#' @examples data <- tibble(observations = 1:100, valeurs = rnorm(100, mean = 100, sd = 50))
#' @examples data <- data %>%
#' @examples categorize_log(column_to_categorize = observations,
#' @examples new_column_name = categories,
#' @examples number_of_intervals = 5)
#'
#'
#' @export
categorize_log <- function(df, column_to_categorize, number_of_intervals = 10, new_column_name = "categories"){

  column_to_categorize <- enquo(column_to_categorize)
  new_column_name <- enquo(new_column_name)

  maximum = df %>% summarise(max = max(!!column_to_categorize, na.rm = TRUE)) %>% pull()*1.5
  minimum = df %>% summarise(min = min(!!column_to_categorize, na.rm = TRUE)) %>% pull()

  breaks <- unique(
    signif(
      c(minimum,
        exp(seq(log(minimum+1), log(maximum), length.out = number_of_intervals))),1))
  breaks_no_max <- breaks[c(-1,-(length(breaks)-1),-length(breaks))]
  breaks_no_min <- breaks[c(-1,-2,-length(breaks))]

  df %>% mutate(!!new_column_name := cut(x = !!column_to_categorize,
                                         breaks = breaks,
                                         include.lowest = TRUE,
                                         labels = c(paste("<",breaks[2]),
                                                    paste(breaks_no_max," - ",breaks_no_min),
                                                    paste(">",breaks[length(breaks)-1]))))
}
