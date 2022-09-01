#'
#' @title Import metadata from PostgreSQL
#'
#' @description Import columns description of a table stored in a PostgreSQL database.
#' @description By creating a variable environment **database_usr** for your database username and **database_pwd** for your database password, you will improve your experience !
#'
#' @param schema a character string naming the schema
#' @param table a character string naming the table
#' @param host a character string naming the host
#' @param port a character string setting the port number
#' @param dbname a character string naming the database
#' @param env_var_usr Name of the environment variable storing your database username
#' @param env_var_pwd Name of the environment variable storing your database password
#'
#' @return a dataframe
#'
#' @import RPostgres
#' @import DBI
#' @import rstudioapi
#' @import sys
#' @import dplyr
#'
#' @examples meta <- import_meta_from_pgsql("fer_circulation", "circulation_quotidienne")
#'
#' @export
import_meta_from_pgsql <- function(schema,
                                   table,
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
  # get metadata
  sql = paste0(
    "SELECT column_name
	    , data_type
	    , is_nullable
	    , pg_catalog.col_description(format('%s.%s',isc.table_schema,isc.table_name)::regclass::oid,isc.ordinal_position)
    FROM information_schema.columns isc
    WHERE table_schema = '", schema,"' AND table_name = '", table,"'")
  meta <- DBI::dbGetQuery(conn = conn, sql)

  # disconnect to database
  DBI::dbDisconnect(conn)
  # return dataset
  return(meta)
}
