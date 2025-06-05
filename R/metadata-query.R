#' Query a specific table in the Postgres database containing the metadata
#' @param table Table name. See [list_metadata_tables()] for valid tables.
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom dplyr tbl
#' @importFrom purrr insistently
#' @export
query_metadata_table <- function(table = "single_builds") {
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    con <- insistently(~
      DBI::dbConnect(RPostgres::Postgres(),
        dbname = "build_metadata", host = "r-binaries.devxy.io",
        port = 15432, user = "r_binaries", password = Sys.getenv("PGPASS"),
        sslmode = "require"
      ), rate = retry_config, quiet = FALSE)()
    metadata <- insistently(
      ~
        tbl(con, table),
      rate = retry_config, quiet = FALSE
    )()
    return(metadata)
  }
}

#' List existing database tables
#' @importFrom DBI dbListTables
#' @importFrom purrr insistently
#' @export
list_metadata_tables <- function() {
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    con <- insistently(~
      dbConnect(RPostgres::Postgres(),
        dbname = "build_metadata", host = "r-binaries.devxy.io",
        port = 15432L, user = "r_binaries", password = Sys.getenv("PGPASS"),
        sslmode = "require"
      ), rate = retry_config, quiet = FALSE)()
    insistently(
      ~
        dbListTables(con),
      rate = retry_config, quiet = FALSE
    )()
  }
}
