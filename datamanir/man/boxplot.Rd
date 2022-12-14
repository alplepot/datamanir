% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxplot.R
\name{boxplot}
\alias{boxplot}
\title{Boxplot compliant with corporate identity}
\usage{
boxplot(
  data,
  x,
  weight,
  color,
  facet,
  x.accuracy = 1,
  color.values = NULL,
  color.order,
  outlier.size = 1,
  title = NULL,
  subtitle = NULL
)
}
\arguments{
\item{data}{A dataframe}

\item{x}{A numeric column of the dataframe}

\item{weight}{(optional) A numeric column of the dataframe. It allows you to weight the observations.}

\item{color}{(optional) A categorical column of the dataframe (numeric variables are mutate into factors). One boxplot will be printed for each category.}

\item{facet}{(optional) A second categorical column of the dataframe (numeric variables are mutate into factors). One boxplot will be printed for each combination of \strong{color} and \strong{facet}.}

\item{x.accuracy}{A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).}

\item{color.values}{A vector of colors. If named, then the values will be matched based on the names (default: NULL).}

\item{color.order}{(optional) When \strong{color} is filled, boxplots are ordered in descending order by default. If \strong{color.order} is equal to "reverse", the boxplots will be ordered reversely. If is equal to a vector of character string corresponding to the \strong{color} variable levels, the boxplots will be ordered according to the vector.}

\item{outlier.size}{Size of the points representing the outliers. Set it to -1 in order to remove the outliers.}

\item{title}{A character string for the title of the chart.}

\item{subtitle}{A character string for the subtitle of the chart.}
}
\value{
A ggplot2 boxplot compliant with the corporate identity.
}
\description{
A boxplot compliant with the corporate identity.
}
\examples{
library(dplyr)
# one input variable: x
mtcars \%>\% boxplot(x = mpg, title = "Miles per gallon")
# two input variables: x and color
mtcars \%>\% boxplot(x = mpg, color = cyl, title = "Miles per gallon",
                       subtitle = "according to the number of cylinders")
# three input variables: x, color and facet
mtcars \%>\%
  mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) \%>\%
  boxplot(x = mpg,
              color = cyl,
              facet = am,
              title = "Miles per gallon",
              subtitle = "according to the transmission and number of cylinders")

}
