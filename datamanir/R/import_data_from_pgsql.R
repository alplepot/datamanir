#'
#' @title Import data from PostgreSQL
#'
#' @description Import a data table from a PostgreSQL database.
#' @description By creating two environment variables called **database_usr** storing your database username and **database_pwd** storing your database password, you will get much faster !
#'
#' @param sql A SQL character string requesting the database
#' @param host A character string naming the host
#' @param port A character string setting the port number
#' @param dbname A character string naming the database
#' @param env_var_usr Name of the environment variable storing your database username
#' @param env_var_pwd Name of the environment variable storing your database password
#'
#' @return a dataframe
#'
#' @import DBI
#' @import RPostgres
#' @import rstudioapi
#' @import dplyr
#' @import sys
#'
#' @examples data <- import_data_from_pgsql("SELECT * FROM information_schema.schemata")
#'
#' @export
# importing dataset function
import_data_from_pgsql <- function(sql,
                                   host,
                                   port,
                                   dbname,
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
                          user = rstudioapi::askForPassword("Your database user name:"),
                          password = rstudioapi::askForPassword("Your database password:"))
  }
  # import dataset
  data = DBI::dbGetQuery(conn = conn, sql) %>%
    # mutate integer64 into numeric
    dplyr::mutate_if(.predicate = function(x){class(x)=="integer64"}, .funs = function(x){as.numeric(x)})
  # disconnect to database
  DBI::dbDisconnect(conn)
  # return dataset
  return(data)
}
