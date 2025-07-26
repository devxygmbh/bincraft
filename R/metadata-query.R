#' Query a specific table in the Postgres database containing the metadata
#' @template param-table
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom dplyr tbl
#' @importFrom purrr insistently
#' @export
query_metadata_table <- function(table = "single_builds") {
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    connection <- insistently(~
      DBI::dbConnect(RPostgres::Postgres(),
        dbname = "build_metadata", host = "r-binaries.devxy.io",
        port = 15432L, user = "rpkgs", password = Sys.getenv("PGPASS"),
        sslmode = "require"
      ), rate = retry_config, quiet = FALSE)()

    # Use connection in tbl call
    metadata <- insistently(function() tbl(connection, table), rate = retry_config, quiet = FALSE)()
    return(metadata)
  }
}

#' List existing database tables
#' @importFrom DBI dbListTables
#' @importFrom purrr insistently
#' @export
list_metadata_tables <- function() {
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    connection <- insistently(~
      dbConnect(RPostgres::Postgres(),
        dbname = "build_metadata", host = "r-binaries.devxy.io",
        port = 15432L, user = "rpkgs", password = Sys.getenv("PGPASS"),
        sslmode = "require"
      ), rate = retry_config, quiet = FALSE)()

    # Use connection in dbListTables call
    insistently(function() dbListTables(connection), rate = retry_config, quiet = FALSE)()
  }
}
