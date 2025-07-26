#' Set codename for Linux distribution
#' @template param-codename
#' @export
set_codename <- function(codename) {
  if (is.null(codename)) {
    if (Sys.info()["sysname"] == "Linux") {
      if (any(grepl("alpine", system2("cat", args = "/etc/os-release", stdout = TRUE), fixed = TRUE))) { # nolint
        os_version <- system2("grep",
          args = "'^VERSION_ID=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'", stdout = TRUE
        )
        version_stripped <- substr(gsub(".", "", os_version, fixed = TRUE), 1L, 3L)
        codename <- paste0("alpine", version_stripped)
      } else {
        dist_fam <- system2("grep",
          args = "'^ID_LIKE=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'", stdout = TRUE
        )
        if (dist_fam == "debian") {
          codename <- system2("grep",
            args = "'^VERSION_CODENAME=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'", stdout = TRUE
          )
        } else if (grepl("rhel|fedora", dist_fam)) {
          platform_id <- system2("grep",
            args = "'^PLATFORM_ID=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'", stdout = TRUE
          )
          if (platform_id == "platform:el9") {
            codename <- "rhel9"
          } else if (platform_id == "platform:el8") {
            codename <- "rhel8"
          }
        }
      }
    }
    codename
  } else {
    codename
  }
}

#' Set path for binary package outputs
#' @template param-codename
#' @template param-local_output_dir_root
#' @export
set_bin_path <- function(local_output_dir_root, codename) {
  local_arch <- Sys.info()[["machine"]]
  if (grepl("arm64", local_arch, fixed = TRUE) || grepl("aarch64", local_arch, fixed = TRUE)) {
    arch <- "arm64"
  } else if (grepl("amd64", local_arch, fixed = TRUE) || grepl("x86_64", local_arch, fixed = TRUE)) {
    arch <- "amd64"
  }

  if (is.null(codename)) {
    log_warn(sprintf(
      "{.function set_bin_path}: `codename` is `NULL`, setting it to the value of `R.version$platform`: '%s'",
      R.version$platform
    ))
    codename <- R.version$platform
  }

  file.path(
    local_output_dir_root, arch, codename, "latest", "src", "contrib"
  )
}

#' Checks whether a binary for the latest package version exists
#' @template param-package_name
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-codename
#' @template param-is_r_minor_sensitive
#' @template param-arch
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-version
#' @export
check_for_binary <- function(
    package_name,
    s3_endpoint = NULL,
    s3_region = NULL,
    s3_bucket = NULL,
    codename = NULL,
    is_r_minor_sensitive = FALSE,
    arch = NULL,
    version = "latest",
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  if (is.null(s3_endpoint)) {
    stop("s3_endpoint must be defined", call. = FALSE)
  }
  if (is.null(s3_region)) {
    stop("s3_region must be defined", call. = FALSE)
  }
  if (is.null(s3_bucket)) {
    stop("s3_bucket must be defined", call. = FALSE)
  }
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )
  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)
  os_version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1L]]$commit$message, "version ")[[1L]][2L] # nolint
  binary_exists <- s3fs::s3_file_exists(sprintf("s3://%s/%s_%s.tar.gz", remote_bin_path, package_name, os_version))
  return(binary_exists)
}

#' @importFrom purrr rate_backoff
retry_config <- purrr::rate_backoff(
  pause_base = 1L,
  pause_cap = 60L,
  pause_min = 1L,
  max_times = 10L,
  jitter = FALSE
)

download_source_tarball <- function(url, destfile) {
  tryCatch(
    {
      status_code <- download.file(url, destfile, quiet = TRUE)
      if (status_code != 0L) {
        # If download.file returns non-zero, treat it as an error condition
        stop(sprintf(
          "download.file failed for %s with status code %d",
          basename(url),
          status_code
        ), call. = FALSE)
      }
      TRUE
    },
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    }
  )
  invisible(TRUE)
}

insistent_downloader <- purrr::insistently(
  download_source_tarball,
  rate = purrr::rate_backoff(max_times = 3L + 1L, pause_base = 1L),
  quiet = FALSE
)

parse_bytes <- function(x) {
  x <- toupper(gsub("\\s+", "", x))
  num <- as.numeric(gsub("[^0-9.]", "", x))
  unit <- gsub("[0-9.]", "", x)
  multipliers <- c(
    B = 1L,
    KB = 1024L,
    MB = 1024L^2L,
    GB = 1024L^3L,
    TB = 1024L^4L,
    PB = 1024L^5L
  )
  # Default to bytes if no unit
  if (unit == "") unit <- "B"
  num * multipliers[unit]
}
