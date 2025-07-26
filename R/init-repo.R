#' Initialize a new R repository
#' @description
#' Creates a new repository (S3 bucket) and checks connectivity to the metadata database
#'
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#'
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-metadata_db_host
#' @template param-metadata_db_type
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#'
#' @importFrom utils menu
#'
#' @note
#' When using SQLITE, make sure that the file is also available in the desired built environment,
#' i.e. most likel in your CI/CD system. Usually it is easiest to store the file also in S3,
#' as it can be reached there easily from within CI and local sources.
#' @examples
#' \dontrun{
#' init_repo("https://hel1.your-objectstorage.com", "hel1", "devxy-r-package-binaries-hel1",
#'   Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"), Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
#'   metadata_db_type = "postgres", metadata_db_host = "r-binaries.devxy.io",
#'   metadata_db_name = "build_metadata", metadata_db_table = "deleteme",
#'   metadata_db_user = "rpkgs", metadata_db_password = Sys.getenv("PGPASS"),
#'   metadata_db_sslmode = "require",
#'   metadata_db_port = 15432
#' )
#' }
init_repo <- function(
    s3_endpoint,
    s3_region,
    s3_bucket,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL,
    metadata_db_type = "sqlite",
    metadata_db_host = NULL,
    metadata_db_name = NULL,
    metadata_db_table = NULL,
    metadata_db_port = NULL,
    metadata_db_user = NULL,
    metadata_db_password = NULL,
    metadata_db_sslmode = NULL) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  ### Check S3
  bucket_exists <- s3fs::s3_is_bucket(s3_bucket)
  if (bucket_exists) {
    cli::cli_alert_success("Bucket {.field {s3_bucket}} already exists.")
  }

  ### Check DB
  if (
    metadata_db_type == "sqlite" && requireNamespace("RSQLite", quietly = TRUE)
  ) {
    res <- DBI::dbCanConnect(RSQLite::SQLite(), metadata_db_host)
  } else if (
    metadata_db_type == "postgres" &&
      requireNamespace("RPostgres", quietly = TRUE)
  ) {
    res <- DBI::dbCanConnect(
      RPostgres::Postgres(),
      dbname = metadata_db_name,
      host = metadata_db_host,
      port = metadata_db_port,
      user = metadata_db_user,
      password = metadata_db_password,
      sslmode = metadata_db_sslmode
    )
    if (res) {
      cli::cli_alert_success(
        "Connectivity to DB {.field {metadata_db_host}}, database {.field {metadata_db_name}} established."
      )
    }
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = metadata_db_name,
      host = metadata_db_host,
      port = metadata_db_port,
      user = metadata_db_user,
      password = metadata_db_password,
      sslmode = metadata_db_sslmode
    )
    exists_table <- DBI::dbExistsTable(con, name = metadata_db_table)
    if (exists_table) {
      cli::cli_alert_success(
        "Table {.field {metadata_db_table}} exists. You're ready to build!"
      )
      cli::cli_alert_info(
        "You can now use {.fun {build_binary_package}} with the provided S3 and DB settings to build and",
        " publish packages."
      )
    } else {
      cli::cli_alert_warning(
        "Table {.field {metadata_db_table}} does not exist."
      )
      answer <- menu(c("Yes", "No"), title = "Should it be created?")
      if (answer == 1L) {
        DBI::dbCreateTable(con, metadata_db_table)
        cli::cli_alert_success(
          "Table {.field {metadata_db_table}} created. You're ready to build!"
        )
      }
    }
  }
}
