#'
#' @title Insert data into PostgreSQL
#'
#' @description Insert data into an existing table in a PostgreSQL database.
#' @description By creating two environment variables called **database_usr** storing your database username and **database_pwd** storing your database password, you will get much faster !
#'
#' @param df a dataframe to be inserted into a PostgreSQL table
#' @param schema a character string naming the PostgreSQL schema
#' @param table a character string naming the PostgreSQL table
#' @param delete_previous_records TRUE if you want to delete previous records of the table
#' @param host a character string naming the host
#' @param port a character string setting the port number
#' @param dbname a character string naming the database
#' @param env_var_usr Name of the environment variable storing your database username
#' @param env_var_pwd Name of the environment variable storing your database password
#'
#' @return TRUE if the data is correctly inserted
#'
#' @import DBI
#' @import RPostgres
#' @import rstudioapi
#' @import dplyr
#' @import sys
#'
#' @examples library(dplyr)
#' @examples mtcars %>% insert_data_into_pgsql(schema = "public", table = "mtcars", delete_previous_records = TRUE)
#'
#' @export
insert_data_into_pgsql <- function(df,
                                   schema,
                                   table,
                                   delete_previous_records = FALSE,
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
  # check if table exists
  table_exists <- DBI::dbExistsTable(conn = conn, DBI::Id(schema = schema, table = table))
  if(!table_exists){
    print("WARNING : The table does not already exists. It is created.")
  }

  # delete previous data
  if(delete_previous_records & table_exists){
    DBI::dbExecute(conn = conn, statement = paste("DELETE FROM ", schema, ".", table))
  }
  # insert dataframe into PostgreSQL table
  insertion = DBI::dbWriteTable(conn = conn,
                           name = DBI::Id(schema = schema, table = table),
                           value = df,
                           overwrite = FALSE,
                           append = TRUE)
  # disconnect to database
  DBI::dbDisconnect(conn)
  return(insertion)
}
