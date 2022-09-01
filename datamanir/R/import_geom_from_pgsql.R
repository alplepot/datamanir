#'
#' @title Import geometry from PostgreSQL
#'
#' @description Import a table containing geometries from a PostgreSQL database.
#' @description By creating a variable environment **database_usr** for your database username and **database_pwd** for your database password, you will improve your experience !
#'
#' @param sql a SQL character string requesting the database
#' @param host a character string naming the host
#' @param port a character string setting the port number
#' @param dbname a character string naming the database
#' @param geom a character string naming the column containing the geometries (default: geometry)
#' @param env_var_usr Name of the environment variable storing your database username
#' @param env_var_pwd Name of the environment variable storing your database password
#'
#' @return a SF object
#'
#' @import RPostgres
#' @import rpostgis
#' @import DBI
#' @import rstudioapi
#' @import sf
#' @import sys
#' @import dplyr
#'
#' @examples data <- import_geom_from_pgsql("SELECT * FROM territoire.ign_region")
#'
#' @export
import_geom_from_pgsql <- function(sql,
                                   host,
                                   port,
                                   dbname,
                                   geom,
                                   env_var_usr = "database_usr",
                                   env_var_pwd = "database_pwd"){
  # connect to database
  conn = try(
    # using environment variables
    DBI::dbConnect(drv = RPostgres::Postgres(),
                   host = host, port = port, dbname = dbname,
                   user = Sys.getenv(env_var_usr),
                   password = Sys.getenv(env_var_pwd)),
    silent = TRUE)
  if(class(conn) == "try-error"){
    # or asking loging info
    conn = DBI::dbConnect(drv = RPostgres::Postgres(),
                          host = host, port = port, dbname = dbname,
                          user = rstudioapi::askForPassword("Your database usename:"),
                          password = rstudioapi::askForPassword("Your database password:"))
  }
  # import dataset and geometries
  data <- rpostgis::pgGetGeom(conn, query = sql, geom = geom) %>%
    sf::st_as_sf(data)
  # disconnect to database
  DBI::dbDisconnect(conn)
  # return dataset
  return(data)
}
