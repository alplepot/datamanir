% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/piechart.R
\name{piechart}
\alias{piechart}
\title{Piechart compliant with corporate identity}
\usage{
piechart(
  data,
  x,
  y,
  facet,
  label.accuracy = 1,
  label.size = 5,
  title = NULL,
  subtitle = NULL,
  legend.name = NULL
)
}
\arguments{
\item{data}{A dataframe}

\item{x}{A categorical column of the dataframe (numeric variables are mutate into factors).}

\item{y}{A numeric column of the dataframe. It will be used to set the width of the slices.}

\item{facet}{(optional) A other categorical column of the dataframe. One chart will be created for each category.}

\item{label.accuracy}{A number to round the bar labels to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL (default value), the default, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values.}

\item{label.size}{A numeric value indicating the bar labels size.}

\item{title}{A character string for the title of the chart.}

\item{subtitle}{A character string for the subtitle of the chart.}

\item{legend.name}{A character string for the color legend title.}
}
\value{
A ggplot2 piechart compliant with the corporate identity.
}
\description{
A piechart compliant with the corporate identity.
}
\examples{
mtcars \%>\%
  group_by(cyl) \%>\%
  summarise(nb_cars = n(), .groups = "keep") \%>\%
  piechart(x = cyl,
               y = nb_cars,
               label.accuracy = 0.1,
               title = "Distribution of cars",
               subtitle = "according to the number of gears")
#
mtcars \%>\%
  mutate(am = case_when(am==0 ~ "Automatic", TRUE ~ "Manual")) \%>\%
  group_by(am, cyl) \%>\%
  summarise(nb_cars = n(), .groups = "keep") \%>\%
  piechart(x = cyl,
               y = nb_cars,
               facet = am,
               title = "Distribution of cars",
               subtitle = "according to the transmission and the number of gears",
               legend.name = "Number of gears")

}
