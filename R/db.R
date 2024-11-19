#' Create Database Connection
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @export
dbcon <- function() {
  DBI::dbConnect(RPostgres::Postgres(),
    dbname = "build_metadata", host = "r-binaries.devxy.io",
    port = 15432, user = "r_binaries", password = Sys.getenv("PGPASS")
  )
}

dbcon_mem <- memoise::memoise(dbcon)
