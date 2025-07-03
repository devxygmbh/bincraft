#' Returns all packages names of CRAN packages not present in the linked database
#' @importFrom dplyr filter pull
#' @keywords internal
#' @export
get_missing_pkgs_db <- function(
  platform = "ubuntu-2204",
  arch = "amd64",
  days_back = 2L
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

#' Returns packages that need to be archived (have multiple versions)
#' @param codename Operating system codename (e.g., "alpine322", "ubuntu-2204")
#' @param arch Architecture (e.g., "arm64", "amd64")
#' @param s3_endpoint S3 endpoint URL
#' @param s3_region S3 region
#' @param s3_bucket S3 bucket name
#' @param s3_access_key_id S3 access key ID
#' @param s3_secret_access_key S3 secret access key
#' @importFrom future plan
#' @keywords internal
#' @examples
#' \dontrun{
#' process_unarchived_pkgs("alpine322", "arm64")
#' }
#'
#' @export
process_unarchived_pkgs <- function(
  codename,
  arch,
  s3_endpoint = "https://hel1.your-objectstorage.com",
  s3_region = "hel1",
  s3_bucket = "devxy-r-package-binaries-hel1",
  s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
  s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
  workers = 1L
) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  repo_path <- file.path(s3_bucket, arch, codename, "latest", "src", "contrib")
  files <- s3fs::s3_dir_ls(repo_path, type = "file")

  package_names <- vapply(
    strsplit(basename(files), "_", fixed = TRUE),
    function(x) x[1L],
    character(1L)
  )

  # Find packages with multiple versions (duplicated names)
  duplicated_packages <- unique(package_names[duplicated(package_names)])
  # Filter out Windows-only packages
  non_archived <- setdiff(duplicated_packages, "RInno")
  formatted_pkgs <- paste(shQuote(non_archived, type = "cmd"), collapse = " ")
  cat(formatted_pkgs, "\n")
  message("Number of packages needing archiving: ", length(non_archived))

  if (length(non_archived) > 0L) {
    out = future.apply::future_lapply(
      non_archived,
      archive_package,
      codename = codename,
      arch = arch,
      s3_region = s3_region,
      s3_endpoint = s3_endpoint,
      s3_bucket = s3_bucket,
      s3_access_key_id = s3_access_key_id,
      s3_secret_access_key = s3_secret_access_key
    )
  }

  invisible(non_archived)
}

#' Returns packages that do not have any historic versions stored in S3
#' @param codename Operating system codename (e.g., "alpine322", "ubuntu-2204")
#' @param arch Architecture (e.g., "arm64", "amd64")
#' @param s3_endpoint S3 endpoint URL
#' @param s3_region S3 region
#' @param s3_bucket S3 bucket name
#' @param s3_access_key_id S3 access key ID
#' @param s3_secret_access_key S3 secret access key
#' @param workers Number of workers to use for parallel processing
#' @importFrom future plan
#' @keywords internal
#' @examples
#' \dontrun{
#' query_packages_without_historic_versions("alpine322", "arm64")
#' }
#'
#' @export
query_packages_without_historic_versions = function(
  codename,
  arch,
  s3_endpoint = "https://hel1.your-objectstorage.com",
  s3_region = "hel1",
  s3_bucket = "devxy-r-package-binaries-hel1",
  s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
  s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S")
) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  all_files <- s3fs::s3_dir_ls(
    sprintf(
      "s3://%s/%s/%s/latest/src/contrib/",
      s3_bucket,
      arch,
      codename
    ),
    recurse = TRUE,
    type = "file"
  )

  pattern <- ".*/src/contrib/([^/_]+)_.*"
  matches <- regmatches(all_files, regexec(pattern, all_files))
  pkg_names <- unique(
    vapply(
      matches,
      function(x) if (length(x) > 1) x[2] else NA_character_,
      character(1)
    )
  )
  pkg_names <- pkg_names[!is.na(pkg_names)]

  # For each package, check if Archive/<pkg>/ contains any files
  no_archive_files <- character(0)
  no_archive_files <- future.apply::future_lapply(pkg_names, function(pkg) {
    archive_dir <- sprintf(
      "s3://%s/%s/%s/latest/src/contrib/Archive/%s",
      s3_bucket,
      arch,
      codename,
      pkg
    )
    s3fs::s3_file_system(
      aws_access_key_id = s3_access_key_id,
      aws_secret_access_key = s3_secret_access_key,
      endpoint = s3_endpoint,
      region_name = s3_region,
      refresh = TRUE
    )
    archive_files <- tryCatch(
      s3fs::s3_dir_ls(archive_dir, recurse = TRUE),
      error = function(e) character(0)
    )
    if (length(archive_files) == 0) {
      pkg
    } else {
      NULL
    }
  })
  no_archive_files <- unlist(no_archive_files, use.names = FALSE)
  no_archive_files
}
