#' Store build metadata of single binary builds
#' @template param-package_name
#' @template param-tag
#' @template param-arch
#' @template param-platform
#' @template param-force
#' @template param-error_occurred
#' @param build_duration Duration of binary build
#' @param size Size of binary package
#' @template param-error
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbExecute
#' @importFrom purrr insistently
store_build_metadata <- function(
    package_name, tag, platform, error_occurred, arch,
    force = FALSE, error = NA, build_duration = NA, size = NA) {
  con <- insistently(~
    dbConnect(RPostgres::Postgres(),
      dbname = "build_metadata", host = "r-binaries.devxy.io",
      port = 15432, user = "r_binaries", password = Sys.getenv("PGPASS"),
      sslmode = "require"
    ), rate = retry_config, quiet = FALSE)()

  # Check if an entry with the same package_name and tag already exists
  existing_entries <- insistently(~
    dbGetQuery(con, "SELECT * FROM single_builds WHERE name = $1 AND tag = $2 AND platform = $3 AND arch = $4", params = list(package_name, tag, platform, arch)), rate = retry_config, quiet = FALSE)()

  if (nrow(existing_entries) >= 1 && !force) {
    cli::cli_alert("{.fun store_build_metadata}: Build metadata for {.field {.pkg {package_name}}} {.field {tag}} already exists.")
  } else if (nrow(existing_entries) >= 1 && force) {
    cli::cli_alert_info("{.fun store_build_metadata}: Force overwriting build metadata for {.pkg {package_name}} {.field {tag}} ({platform}) because {.code force = TRUE} was set.")

    insistently(~
      dbExecute(con, "UPDATE single_builds SET timestamp = $1, error_occurred = $2, error_text = $3, duration = $4, size = $5, removed = $9 WHERE name = $6 and tag = $7 and platform = $8",
        params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), error_occurred, error, build_duration, size, package_name, tag, platform, FALSE)
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
    insistently(
      ~
        dbWriteTable(con, "single_builds", metadata, append = TRUE),
      rate = retry_config, quiet = FALSE
    )()
  }
  dbDisconnect(con)

  return(invisible(TRUE))
}

#' Remove package from metadata table
#' @template param-package_name
#' @importFrom purrr insistently
remove_from_metadata <- function(package_name) {
  con <- insistently(~
    dbConnect(RPostgres::Postgres(),
      dbname = "build_metadata", host = "r-binaries.devxy.io",
      port = 15432, user = "r_binaries", password = Sys.getenv("PGPASS"),
      sslmode = "require"
    ), rate = retry_config, quiet = FALSE)()

  insistently(~
    dbExecute(con, "UPDATE single_builds SET removed = true WHERE name = $1",
      params = list(package_name), rate = retry_config, quiet = FALSE
    ))()
}
