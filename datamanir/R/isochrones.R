#'
#' @title Compute isochrones around a set of points
#'
#' @description Compute isochrones around a set of points listed in a SF dataframe.
#'
#' @param data_sf a SF dataframe containing a set of points
#' @param breaks a vector of numeric values corresponding to the drive times of the isochrones (default: c(0,15,30,60)).
#' @param res number of points used to compute isochrones, one side of the square grid, the total number of points will be res*res (default: 30).
#'
#' @return a SF object containing one geometry for each break (the isochrones of all of the points are joined).
#'
#' @import osrm
#' @import sf
#' @import dplyr
#' @import ddpcr
#'
#' @examples library(dplyr)
#' @examples library(sf)
#' @examples library(ggplot2)
#' @examples data <- tibble(x = c(2.3, -2.5), y = c(48.8, 48.5)) %>%
#' @examples   st_as_sf(coords = c("x", "y"))
#' @examples isochrones <- data %>% isochrones(breaks = c(10,30,90))
#' @examples basemap_france() +
#' @examples   geom_sf(data = isochrones, aes(fill = max))
#'
#' @export
isochrones <- function(data_sf, breaks = c(0, 15, 30, 60), res = 30){
  sf_use_s2(FALSE)
  library(osrm)
  # set OSRM server (Open Source Route Machine)
  # options(osrm.server = "https://")
  # sub-function to calculate one isochrone with OSRM
  iso_func <- function(sf_loc){
    print(sf_loc)
    osrm::osrmIsochrone(loc = sf_loc, breaks = breaks, returnclass = "sf")
  }
  # list isochrones and bind them
  isochrones <- do.call(
    rbind,
    lapply(sf::st_geometry(data_sf),
           iso_func))

  quiet(isochrones <- isochrones %>%
          dplyr::group_by(min, max) %>%
          dplyr::summarize(.groups = "keep") %>%
          dplyr::ungroup())
  sf_use_s2(TRUE)
  return(isochrones)
}
