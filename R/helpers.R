#' Set codename for Linux distribution
#' @template param-codename
#' @export
set_codename <- function(codename) {
  codename %||% detect_linux_codename()
}

#' Detect Linux distribution codename from /etc/os-release
#' @return Character string with codename or NULL
#' @noRd
detect_linux_codename <- function() {
  if (Sys.info()["sysname"] != "Linux") {
    return(NULL)
  }

  os_release <- readLines("/etc/os-release", warn = FALSE)
  parse_os_field <- function(field) {
    line <- grep(paste0("^", field, "="), os_release, value = TRUE)
    if (length(line) == 0L) {
      return(NULL)
    }
    gsub('"', "", sub(paste0("^", field, "="), "", line[1L]))
  }

  if (any(grepl("alpine", os_release, fixed = TRUE))) {
    os_version <- parse_os_field("VERSION_ID")
    version_stripped <- substr(gsub(".", "", os_version, fixed = TRUE), 1L, 3L)
    return(paste0("alpine", version_stripped))
  }

  dist_fam <- parse_os_field("ID_LIKE")

  if (identical(dist_fam, "debian")) {
    return(parse_os_field("VERSION_CODENAME"))
  }

  if (!is.null(dist_fam) && grepl("rhel|fedora", dist_fam)) {
    platform_id <- parse_os_field("PLATFORM_ID")
    if (identical(platform_id, "platform:el9")) {
      return("rhel9")
    } else if (identical(platform_id, "platform:el8")) {
      return("rhel8")
    } else if (identical(platform_id, "platform:el10")) {
      return("rhel10")
    }
  }

  NULL
}

#' Set path for binary package outputs
#' @template param-codename
#' @template param-local_output_dir_root
#' @export
set_bin_path <- function(local_output_dir_root, codename) {
  local_arch <- Sys.info()[["machine"]]
  if (
    grepl("arm64", local_arch, fixed = TRUE) ||
      grepl("aarch64", local_arch, fixed = TRUE)
  ) {
    arch <- "arm64"
  } else if (
    grepl("amd64", local_arch, fixed = TRUE) ||
      grepl("x86_64", local_arch, fixed = TRUE)
  ) {
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
    local_output_dir_root,
    arch,
    codename,
    "latest",
    "src",
    "contrib"
  )
}

#' The `major.minor` of the running R
#'
#' @keywords internal
#' @noRd
current_r_minor <- function() {
  paste(
    R.version$major,
    strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L],
    sep = "."
  )
}

#' Version most recently published on the `cran` GitHub mirror
#'
#' The mirror commits once per CRAN release with `version <x.y.z>` as the
#' message, which is how the published version is read back.
#'
#' A 404 means the mirror has no repository for the package at all. That is
#' permanent - the mirror lags CRAN, so a package that has just appeared is
#' simply not there yet - and retrying it cannot succeed. Retried through
#' `retry_config` it costs ten attempts on a backoff capped at 60s, about five
#' minutes, and then aborts whatever job asked. `AsyPeer 0.0.1` took out an
#' entire build that way hours after being published.
#'
#' Transient failures are still retried.
#'
#' @param package_name Package to look up.
#' @param rate Retry policy for transient failures.
#'
#' @return The published version, or `NA_character_` when the mirror does not
#'   carry the package.
#' @keywords internal
#' @noRd
cran_mirror_version <- function(package_name, rate = retry_config) {
  commits <- purrr::insistently(
    function() {
      tryCatch(
        gh::gh(sprintf("GET /repos/cran/%s/commits", package_name)),
        http_error_404 = function(e) NULL
      )
    },
    rate = rate,
    quiet = FALSE
  )()

  if (is.null(commits) || length(commits) == 0L) {
    return(NA_character_)
  }
  strsplit(commits[[1L]]$commit$message, "version ")[[1L]][2L]
}

#' Checks whether a binary for the latest package version exists
#'
#' "A binary exists" is not the same as "the object exists". A package that
#' failed to build has its CRAN *source* published in the same place by
#' [handle_post_build_actions()], and treating that as a binary is what makes
#' the fallback permanent: the build is never retried and the missing-binaries
#' audit never reports it. The object's MD5 is compared against CRAN's to tell
#' the two apart. When the MD5 cannot be established the object counts as a
#' binary, so an unreadable ETag or an unreachable CRAN cannot trigger an
#' endless rebuild loop.
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
  s3_secret_access_key = NULL
) {
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
  os_version <- cran_mirror_version(package_name)
  if (is.na(os_version)) {
    # No repository on the mirror means no version to check against, and the
    # documented convention here is that an undeterminable answer counts as a
    # binary: that keeps an unreachable upstream from driving an endless
    # rebuild loop, and it keeps one unmirrored package from aborting a shard.
    log_info(sprintf(
      "{.fun check_for_binary}: the cran mirror has no repository for {.pkg %s}. Treating it as already built.",
      package_name
    ))
    return(TRUE)
  }
  # An r-minor-sensitive build lives in the per-minor slot, and that is where
  # the source fallback writes it too, so the flat path would never find it.
  remote_path <- if (isTRUE(is_r_minor_sensitive)) {
    sprintf(
      "s3://%s/%s/%s_%s.tar.gz",
      remote_bin_path,
      current_r_minor(),
      package_name,
      os_version
    )
  } else {
    sprintf(
      "s3://%s/%s_%s.tar.gz",
      remote_bin_path,
      package_name,
      os_version
    )
  }

  if (!s3fs::s3_file_exists(remote_path)) {
    return(FALSE)
  }

  md5 <- remote_object_md5(remote_path)
  if (is.na(md5)) {
    return(TRUE)
  }

  is_source <- is_cran_source_tarball(package_name, os_version, md5)
  if (isTRUE(is_source)) {
    log_info(sprintf(
      "{.fun check_for_binary}: {.pkg %s} {.field %s} is the CRAN source, not a binary. Reporting it as missing so the build is retried.",
      package_name,
      os_version
    ))
  }
  !isTRUE(is_source)
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
      status_code <- download.file(url, destfile, quiet = TRUE, mode = "wb")
      if (status_code != 0L) {
        # If download.file returns non-zero, treat it as an error condition
        stop(
          sprintf(
            "download.file failed for %s with status code %d",
            basename(url),
            status_code
          ),
          call. = FALSE
        )
      }
      TRUE
    },
    error = function(e) {
      # Escape braces in error message to prevent cli/glue interpretation
      stop(
        gsub(
          "}",
          "}}",
          gsub("{", "{{", conditionMessage(e), fixed = TRUE),
          fixed = TRUE
        ),
        call. = FALSE
      )
    }
  )
  invisible(TRUE)
}

insistent_downloader <- purrr::insistently(
  download_source_tarball,
  rate = purrr::rate_backoff(max_times = 3L + 1L, pause_base = 1L),
  quiet = FALSE
)

#' Retry an S3 operation with linear backoff
#' @param func A function to execute (no arguments).
#' @param label A descriptive label for log/error messages.
#' @param max_attempts Maximum number of attempts before failing.
#' @param base_delay Base delay in seconds (multiplied by attempt number).
#' @return The result of `func()` on success.
#' @noRd
retry_s3_operation <- function(
  func,
  label,
  max_attempts = 3L,
  base_delay = 5L
) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(func(), error = function(e) e)
    if (!inherits(result, "error")) {
      return(result)
    }
    if (attempt == max_attempts) {
      stop(
        sprintf(
          "Failed to %s after %d attempts. Last error: %s. This typically means an S3 object was removed between listing and querying.",
          label,
          max_attempts,
          conditionMessage(result)
        ),
        call. = FALSE
      )
    }
    log_warn(sprintf(
      "Attempt %d/%d to %s failed (%s). Retrying in %ds...",
      attempt,
      max_attempts,
      label,
      conditionMessage(result),
      attempt * base_delay
    ))
    Sys.sleep(attempt * base_delay)
  }
}

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
  if (unit == "") {
    unit <- "B"
  }
  num * multipliers[unit]
}
