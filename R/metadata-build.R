#' Store build metadata of single binary builds
#' @template param-package_name
#' @template param-tag
#' @template param-arch
#' @template param-platform
#' @template param-force
#' @template param-error_occurred
#' @template param-build_duration
#' @template param-size
#' @template param-metadata_db_type
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_table
#' @template param-metadata_db_port
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-error
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbExecute
#' @importFrom purrr insistently
store_build_metadata <- function(
    package_name,
    tag,
    platform,
    arch,
    error_occurred,
    metadata_db_type = "postgres",
    metadata_db_host = NULL,
    metadata_db_name = NULL,
    metadata_db_table = NULL,
    metadata_db_port = NULL,
    metadata_db_user = NULL,
    metadata_db_password = NULL,
    metadata_db_sslmode = NULL,
    force = FALSE,
    error = NA,
    build_duration = NA,
    size = NA) {
  if (metadata_db_type == "postgres" && !requireNamespace("RPostgres", quietly = TRUE)) {
    log_info(paste0(
      "{.function store_build_metadata}: {.pkg RPostgres} must be installed ",
      "when ` metadata_db_type = 'postgres'`"
    ))
    return()
  }

  if (metadata_db_type == "postgres") {
    driver <- RPostgres::Postgres() # nolint
  }

  con <- purrr::insistently(~
    dbConnect(driver,
      dbname = metadata_db_name, host = metadata_db_host,
      port = metadata_db_port, user = metadata_db_user, password = metadata_db_password,
      sslmode = metadata_db_sslmode
    ), rate = retry_config, quiet = FALSE)()

  r_version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")

  table_name <- DBI::dbQuoteIdentifier(con, metadata_db_table)
  query <- paste0("SELECT * FROM ", table_name, " WHERE name = $1 AND tag = $2 AND platform = $3 AND arch = $4 and r_version = $5") # nolint
  existing_entries <- purrr::insistently(
    ~ dbGetQuery(con, query,
      params = list(package_name, tag, platform, arch, r_version)
    ),
    rate = retry_config, quiet = FALSE
  )()

  if (nrow(existing_entries) >= 1L && !force) {
    log_info(sprintf("{.fun store_build_metadata}: Build metadata for {.pkg %s} {.field %s} already exists.", package_name, tag)) # nolint
  } else if (nrow(existing_entries) >= 1L && force) {
    log_info(sprintf("{.fun store_build_metadata}: Force overwriting build metadata for {.pkg %s} {.field %s} (%s) because {.code force = TRUE} was set.", package_name, tag, platform)) # nolint

    query <- paste0(
      "UPDATE ", table_name, " SET timestamp = $1, error_occurred = $2, error_text = $3, ",
      paste0(
        "duration = $4, size = $5, removed = $6 WHERE name = $7 and tag = $8 ",
        "and platform = $9 and arch = $10 and r_version = $11"
      )
    )
    insistently(
      ~ dbExecute(
        con, query,
        params = list(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          error_occurred,
          error,
          build_duration,
          size,
          FALSE,
          package_name,
          tag,
          platform,
          arch,
          r_version
        )
      ),
      rate = retry_config, quiet = FALSE
    )()
  } else if (nrow(existing_entries) == 0L) {
    log_info(sprintf("Storing build metadata for {.pkg %s} {.field %s}.", package_name, tag))
    # if no entry exists already, we can insert the info via dbWriteTable by passing a DF
    metadata <- data.frame( # nolint
      name = package_name,
      tag = tag,
      platform = platform,
      arch = arch,
      error_occurred = error_occurred,
      error_text = error,
      size = size,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      duration = build_duration,
      removed = FALSE,
      r_version = r_version,
      stringsAsFactors = FALSE
    )
    # Write the data frame to the SQLite database
    purrr::insistently(
      ~
        dbWriteTable(con, metadata_db_table, metadata, append = TRUE),
      rate = retry_config, quiet = FALSE
    )()
  }
  dbDisconnect(con)

  return(invisible(TRUE))
}

#' Remove package from metadata table
#' @template param-package_name
#' @param metadata_db_type Database type. Denotes the DB driver to be used for connecting.
#' @param metadata_db_host Database host
#' @param metadata_db_name Database name
#' @param metadata_db_table Database table
#' @param metadata_db_port Database port
#' @param metadata_db_user Database user
#' @param metadata_db_password Database password
#' @param metadata_db_sslmode Database sslmode
#' @importFrom purrr insistently
remove_from_metadata <- function(
    package_name,
    metadata_db_type = "postgres",
    metadata_db_host = NULL,
    metadata_db_name = NULL,
    metadata_db_table = NULL,
    metadata_db_port = NULL,
    metadata_db_user = NULL,
    metadata_db_password = NULL,
    metadata_db_sslmode = NULL) {
  if (metadata_db_type == "postgres") {
    driver <- RPostgres::Postgres() # nolint
  }
  con <- purrr::insistently(~
    dbConnect(driver,
      dbname = metadata_db_name, host = metadata_db_host,
      port = metadata_db_port, user = metadata_db_user, password = metadata_db_password,
      sslmode = metadata_db_sslmode
    ), rate = retry_config, quiet = FALSE)()

  table_name <- DBI::dbQuoteIdentifier(con, metadata_db_table)
  query <- paste0("UPDATE ", table_name, " SET removed = true WHERE name = $1") # nolint
  purrr::insistently(
    ~ dbExecute(con, query, params = list(package_name)),
    rate = retry_config,
    quiet = FALSE
  )()
}
