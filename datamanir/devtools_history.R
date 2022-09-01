

#install.package("usethis")
library(usethis)

# ignore files used for the package building
usethis::use_build_ignore("devtools_history.R")
usethis::use_build_ignore("datamanir.Rproj")
usethis::use_build_ignore(".Rhistory")

# remove the following files from the git versions
usethis::use_git_ignore(".Rhistory")
usethis::use_git_ignore("datamanir.Rproj")

# use MIT licence
usethis::use_mit_license()

# complete DESCRIPTION and NAMESPACE with the following dependencies
usethis::use_package("osrm")
usethis::use_package("RPostgres")
usethis::use_package("rpostgis")
usethis::use_package("DBI")
usethis::use_package("sys")
usethis::use_package("rstudioapi")
usethis::use_package("ggplot2")
usethis::use_package("sf")
usethis::use_package("extrafont")
usethis::use_package("dplyr")
usethis::use_package("scales")
usethis::use_package("forcats")
usethis::use_package("spatstat")
usethis::use_package("ggrepel")
usethis::use_package("ddpcr")
# save a shapefile in the /data folder as an .rda file
#install.package("sf")
library(sf)
map_region_wgs84 <- st_read(dsn = "path_to_shapefile", layer = "REGION", quiet = TRUE)
usethis::use_data(map_region_wgs84, internal = FALSE, overwrite = TRUE)

