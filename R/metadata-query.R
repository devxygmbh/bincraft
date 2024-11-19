#' Query a specific table in the Postgres database containing the metadata
#' @param table Table name. See [list_metadata_tables()] for valid tables.
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom RPostgres Postgres
#' @importFrom dplyr tbl
#' @export
query_metadata_table <- function(table = "single_builds") {
  con <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = "build_metadata", host = "r-binaries.devxy.io",
    port = 15432, user = "r_binaries", password = Sys.getenv("PGPASS"),
    sslmode = "require"
  )
  metadata <- tbl(con, table)
  return(metadata)
}

#' List existing database tables
#' @importFrom DBI dbListTables
#' @export
list_metadata_tables <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = "build_metadata", host = "r-binaries.devxy.io",
    port = 15432, user = "r_binaries", password = Sys.getenv("PGPASS"),
    sslmode = "require"
  )
  dbListTables(con)
}
