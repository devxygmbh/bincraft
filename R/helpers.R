#' Set codename for Linux distribution
#' @template param-codename
#' @export
set_codename <- function(codename) {
  if (is.null(codename)) {
    if (Sys.info()["sysname"] == "Linux") {
      if (any(grepl("alpine", system2("cat", args = c("/etc/os-release"), stdout = TRUE)))) {
        version <- system2("grep",
          args = c("'^VERSION_ID=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
        )
        version_stripped <- substr(gsub("\\.", "", version), 1, 3)
        codename <- paste0("alpine", version_stripped)
      } else {
        dist_fam <- system2("grep",
          args = c("'^ID_LIKE=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
        )
        if (dist_fam == "debian") {
          codename <- system2("grep",
            args = c("'^VERSION_CODENAME=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
          )
        } else if (grepl("rhel|fedora", dist_fam)) {
          platform_id <- system2("grep",
            args = c("'^PLATFORM_ID=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
          )
          if (platform_id == "platform:el9") {
            codename <- "rhel9"
          } else if (platform_id == "platform:el8") {
            codename <- "rhel8"
          }
        }
      }
    }
    return(codename)
  } else {
    return(codename)
  }
}

#' Set path for binary package outputs
#' @template param-codename
#' @template param-local_output_dir_root
#' @export
set_bin_path <- function(local_output_dir_root, codename) {
  local_arch <- Sys.info()[["machine"]]
  if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
    arch <- "arm64"
  } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
    arch <- "amd64"
  }

  path <- sprintf(
    "%s/%s/%s/latest/src/contrib",
    local_output_dir_root, arch, codename
  )
  return(path)
}

#' Checks whether a binary for the latest package version exists
#' @template param-package_name
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-codename
#' @template param-arch
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @param version Version to check for. Only "latest" is supported right now.
#' @export
check_for_binary <- function(
    package_name,
    s3_endpoint = NULL,
    s3_region = NULL,
    s3_bucket = NULL,
    codename = NULL,
    arch = NULL,
    version = "latest",
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  if (is.null(s3_endpoint)) {
    stop("s3_endpoint must be defined")
  }
  if (is.null(s3_region)) {
    stop("s3_region must be defined")
  }
  if (is.null(s3_bucket)) {
    stop("s3_bucket must be defined")
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
  version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1]]$commit$message, "version ")[[1]][2]
  exists <- s3fs::s3_file_exists(sprintf("s3://%s/%s_%s.tar.gz", remote_bin_path, package_name, version))
  return(exists)
}

#' @importFrom purrr rate_backoff
retry_config <- purrr::rate_backoff(
  pause_base = 1,
  pause_cap = 60,
  pause_min = 1,
  max_times = 10,
  jitter = FALSE
)

download_source_tarball <- function(url, destfile) {
  status_or_error <- tryCatch(
    {
      status_code <- download.file(url, destfile, quiet = TRUE)
      if (status_code != 0) {
        # If download.file returns non-zero, treat it as an error condition
        stop(sprintf(
          "download.file failed for %s with status code %d",
          basename(url),
          status_code
        ))
      }
      TRUE
    },
    error = function(e) {
      stop(conditionMessage(e))
    }
  )
  return(invisible(TRUE))
}

insistent_downloader <- purrr::insistently(
  download_source_tarball,
  rate = purrr::rate_backoff(max_times = 3 + 1, pause_base = 1),
  quiet = FALSE
)
