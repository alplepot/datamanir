% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barplot.R
\name{barplot}
\alias{barplot}
\title{Barplot compliant with corporate identity}
\usage{
barplot(
  data,
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
  legend.name = NULL
)
}
\arguments{
\item{data}{A dataframe}

\item{x}{A categorical column of the dataframe (numeric variables are mutate into factors).}

\item{y}{(optional) A numeric column of the dataframe. It will be used to set the height of the bars.}

\item{color}{(optional, allowed if y is completed) A other categorical column of the dataframe. It will be used to create a color legend.}

\item{facet}{(optional, allowed if y is completed) A other categorical column of the dataframe. One chart will be created for each category.}

\item{weight}{(optional, allowed if y, color and facet are missing) A numeric column of the dataframe. It allows you to weight the observations.}

\item{x.order}{(optional) A vector of character string corresponding to the x variable levels. By default, bars will be sorted by descendind height.}

\item{y.accuracy}{A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).}

\item{y.suffix}{A character string used as a suffix for y axis text.}

\item{color.values}{A vector of colors. If named, then the values will be matched based on the names.}

\item{color.order}{A vector of character string corresponding to the color variable levels. By default, colors will be sorted by descending height.}

\item{facet.order}{(optional) A vector of character string corresponding to the facet variable levels. By default, facets will be sorted by descendind height.}

\item{label.accuracy}{A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values (default: 1).}

\item{label.suffix}{A character string used as a suffix for labels on the bars.}

\item{label.size}{A numeric value indicating the bar labels size.}

\item{title}{A character string for the title of the chart.}

\item{subtitle}{A character string for the subtitle of the chart.}

\item{x.name}{A character string for the x axis title.}

\item{y.name}{A character string for the y axis title.}

\item{legend.name}{A character string for the color legend title.}
}
\value{
A ggplot2 barplot (geom_bar or geom_col) compliant with the corporate identity.
}
\description{
A barplot compliant with the corporate identity.
}
\examples{
library(dplyr)
# one input variable: x
mtcars \%>\% barplot(x = mpg,
                       weight = cyl,
                       title = "Distribution of cylinders according to miles per gallon",
                       subtitle = "It doesn't mean anything, does it ?",
                       x.name = "Miles per gallon",
                       y.name = "Number of cylinders")
# two input variables: x and y
mtcars \%>\%
  group_by(cyl) \%>\%
  summarise(hp = mean(hp)) \%>\%
  barplot(x = cyl,
              y = hp,
              title = "Gross horsepower",
              subtitle = "according to the number of cylinders",
              x.name = "Number of cylinders",
             y.name = "Gross horsepower")
# three input variables: x, y and color
mtcars \%>\%
  mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) \%>\%
  group_by(cyl, am) \%>\%
  summarise(nb_cars = n(), .groups = "keep") \%>\%
  barplot(x = cyl,
              y = nb_cars,
              color = am,
              title = "Distribution of cars",
              subtitle = "according to the number of cylinders",
              x.name = "Number of cylinders",
              y.name = "NUmber of cars")
# three input variables: x, y and facet
mtcars \%>\%
  mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) \%>\%
  group_by(cyl, am) \%>\%
  summarise(nb_cars = n(), .groups = "keep") \%>\%
  barplot(x = cyl,
              y = nb_cars,
              facet = am,
              title = "Distribution of cars",
              subtitle = "according to the transmission and the number of cylinders",
              x.name = "Number of cylinders",
              y.name = "Number of cars")
# four input variables: x, y, color and facet
mtcars \%>\%
  mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) \%>\%
  group_by(cyl, am, gear) \%>\%
  summarise(nb_cars = n(), .groups = "keep") \%>\%
  barplot(x = cyl,
              y = nb_cars,
              color = gear,
              facet = am,
              title = "Distribution of cars",
              subtitle = "according to the transmission and other things",
              x.name = "Number of cylinders",
              y.name = "Number of cars",
              legend.name = "Number of gears")

}
