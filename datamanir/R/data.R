#' @title France regions map
#'
#' @description A SF dataframe containing the France regions geometries.
#'
#' @format A SF dataframe with 18 observations and 6 columns:
#' \describe{
#'   \item{ID}{region id}
#'   \item{INSEE_REG}{INSEE id of the region}
#'   \item{CHF_REG}{INSEE id of the region capital}
#'   \item{NOM_REG_M}{region name in capitals}
#'   \item{NOM_REG}{region name}
#'   \item{geometry}{region geometry: multipolygons}
#' }
#'
"map_region_wgs84"
