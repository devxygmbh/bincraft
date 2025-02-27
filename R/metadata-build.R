#' Store build metadata of single binary builds
#' @template param-package_name
#' @template param-tag
#' @template param-arch
#' @template param-platform
#' @template param-force
#' @template param-error_occurred
#' @param build_duration Duration of binary build
#' @param size Size of binary package
#' @param db_type Database type. Denotes the DB driver to be used for connecting.
#' @param db_host Database host
#' @param db_name Database name
#' @param db_table Database table
#' @param db_port Database port
#' @param db_user Database user
#' @param db_password Database password
#' @param db_sslmode Database sslmode
#' @template param-error
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbExecute
#' @importFrom purrr insistently
store_build_metadata <- function(
    package_name,
    tag,
    platform,
    error_occurred,
    db_type = "postgres",
    db_host = NULL,
    db_name = NULL,
    db_table = NULL,
    db_port = NULL,
    db_user = NULL,
    db_password = NULL,
    db_sslmode = NULL,
    arch,
    force = FALSE,
    error = NA,
    build_duration = NA,
    size = NA) {
  if (db_type == "postgres") {
    driver <- RPostgres::Postgres()
  }

  con <- purrr::insistently(~
    dbConnect(driver,
      dbname = "build_metadata", host = db_host,
      port = db_port, user = db_user, password = db_password,
      sslmode = db_sslmode
    ), rate = retry_config, quiet = FALSE)()

  # Check if an entry with the same package_name and tag already exists
  existing_entries <- purrr::insistently(~
    dbGetQuery(con, "SELECT * FROM $5 WHERE name = $1 AND tag = $2 AND platform = $3 AND arch = $4", params = list(package_name, tag, platform, arch, db_table)), rate = retry_config, quiet = FALSE)()

  if (nrow(existing_entries) >= 1 && !force) {
    cli::cli_alert("{.fun store_build_metadata}: Build metadata for {.field {.pkg {package_name}}} {.field {tag}} already exists.")
  } else if (nrow(existing_entries) >= 1 && force) {
    cli::cli_alert_info("{.fun store_build_metadata}: Force overwriting build metadata for {.pkg {package_name}} {.field {tag}} ({platform}) because {.code force = TRUE} was set.")

    insistently(~
      dbExecute(con, "UPDATE $9 SET timestamp = $1, error_occurred = $2, error_text = $3, duration = $4, size = $5, removed = $9 WHERE name = $6 and tag = $7 and platform = $8",
        params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), error_occurred, error, build_duration, size, package_name, tag, platform, db_table, FALSE)
      ), rate = retry_config, quiet = FALSE)()
  } else if (nrow(existing_entries) == 0) {
    cli::cli_alert("{.fun store_build_metadata}: Storing build metadata for {.pkg {package_name}} {.field {tag}}.")
    # if no entry exists already, we can insert the info via dbWriteTable by passing a DF
    metadata <- data.frame(
      name = package_name,
      tag = tag,
      platform = platform,
      arch = arch,
      error_occurred = error_occurred,
      error_text = error,
      size = size,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      duration = build_duration,
      removed = FALSE
    )
    # Write the data frame to the SQLite database
    purrr::insistently(
      ~
        dbWriteTable(con, db_table, metadata, append = TRUE),
      rate = retry_config, quiet = FALSE
    )()
  }
  dbDisconnect(con)

  return(invisible(TRUE))
}

#' Remove package from metadata table
#' @template param-package_name
#' @param db_type Database type. Denotes the DB driver to be used for connecting.
#' @param db_host Database host
#' @param db_name Database name
#' @param db_table Database table
#' @param db_port Database port
#' @param db_user Database user
#' @param db_password Database password
#' @param db_sslmode Database sslmode
#' @importFrom purrr insistently
remove_from_metadata <- function(
    package_name,
    db_type = "postgres",
    db_host = NULL,
    db_name = NULL,
    db_table = NULL,
    db_port = NULL,
    db_user = NULL,
    db_password = NULL,
    db_sslmode = NULL) {
  con <- purrr::insistently(~
    dbConnect(driver,
      dbname = "build_metadata", host = db_host,
      port = db_port, user = db_user, password = db_password,
      sslmode = db_sslmode
    ), rate = retry_config, quiet = FALSE)()

  purrr::insistently(~
    dbExecute(con, "UPDATE $2 SET removed = true WHERE name = $1",
      params = list(package_name, db_table), rate = retry_config, quiet = FALSE
    ))()
}
