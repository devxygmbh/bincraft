#' Returns all packages names of CRAN packages not present in the linked database
#' @importFrom dplyr filter pull
#' @keywords internal
#' @export
get_missing_pkgs_db <- function(
  platform = "ubuntu-2204",
  arch = "amd64",
  days_back = 2
) {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "build_metadata",
    host = "r-binaries.devxy.io",
    port = 15432L,
    user = "rpkgs",
    password = Sys.getenv("PGPASS"),
    sslmode = "require"
  )

  # Helper: not in
  `%nin%` <- Negate(`%in%`)

  # Get new and removed packages in the last X days
  interval_days <- lubridate::interval(
    lubridate::today() - days_back,
    lubridate::today()
  )
  new_packages <- get_new_cran_packages(interval_days)$name
  removed_pkgs <- get_removed_cran_packages(interval_days)$name

  # Get CRAN packages, filter out Windows-only and new packages
  cran_pkgs <- unique(
    tools::CRAN_package_db() |>
      dplyr::filter(is.na(OS_type) | OS_type != "windows") |>
      dplyr::filter(Package %nin% new_packages) |>
      dplyr::pull(Package)
  )

  # Get built packages from DB
  data <- DBI::dbGetQuery(
    con,
    "SELECT name FROM single_builds WHERE platform = $1 AND arch = $2 and removed = FALSE;",
    params = list(platform, arch)
  )
  pkgs_db <- setdiff(unique(data$name), removed_pkgs)

  # Find missing packages
  pkgs <- setdiff(cran_pkgs, pkgs_db)

  # Format output
  formatted_pkgs <- paste(shQuote(pkgs, type = "cmd"), collapse = " ")
  cat(formatted_pkgs, "\n")
  message("Number of missing packages: ", length(pkgs))

  invisible(pkgs)
}
