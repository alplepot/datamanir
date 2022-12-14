% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_geom_from_pgsql.R
\name{import_geom_from_pgsql}
\alias{import_geom_from_pgsql}
\title{Import geometry from PostgreSQL}
\usage{
import_geom_from_pgsql(
  sql,
  host,
  port,
  dbname,
  geom,
  env_var_usr = "database_usr",
  env_var_pwd = "database_pwd"
)
}
\arguments{
\item{sql}{a SQL character string requesting the database}

\item{host}{a character string naming the host}

\item{port}{a character string setting the port number}

\item{dbname}{a character string naming the database}

\item{geom}{a character string naming the column containing the geometries (default: geometry)}

\item{env_var_usr}{Name of the environment variable storing your database username}

\item{env_var_pwd}{Name of the environment variable storing your database password}
}
\value{
a SF object
}
\description{
Import a table containing geometries from a PostgreSQL database.

By creating a variable environment \strong{database_usr} for your database username and \strong{database_pwd} for your database password, you will improve your experience !
}
\examples{
data <- import_geom_from_pgsql("SELECT * FROM territoire.ign_region")

}
