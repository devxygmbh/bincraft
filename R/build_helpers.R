#' Build helper functions for package building operations

#' Parse package tag pairs from difference strings
#'
#' Extracts package names and version tags from package difference strings
#' in the format "package_version.tar.gz".
#'
#' @template param-pkg_differences
#' @return List of lists containing pkg and tag elements
parse_package_tag_pairs <- function(pkg_differences) {
  lapply(pkg_differences, function(x) {
    parts <- strsplit(x, "_", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) {
      return(list(pkg = NA, tag = NA))
    }
    pkg_name <- parts[1L]
    version_part <- parts[2L]
    tag_val <- strsplit(version_part, ".tar.gz", fixed = TRUE)[[1L]][1L]
    list(pkg = pkg_name, tag = tag_val)
  })
}

#' Check if a package has previous build errors
#'
#' Queries the metadata database to check if a package/tag combination
#' has recorded build errors for the specified platform and architecture.
#'
#' @template param-con
#' @template param-table_name
#' @template param-pair
#' @template param-platform
#' @template param-arch
#' @return Logical indicating if errors were found
check_package_error <- function(con, table_name, pair, platform, arch) {
  if (is.na(pair$pkg) || is.na(pair$tag)) {
    return(FALSE)
  }

  result <- purrr::insistently(
    ~ DBI::dbGetQuery(
      con,
      paste0(
        "SELECT error_occurred FROM ",
        table_name,
        " WHERE name = $1 AND tag = $2 AND platform = $3 AND arch = $4"
      ),
      params = list(pair$pkg, pair$tag, platform, arch)
    ),
    rate = retry_config,
    quiet = FALSE
  )()

  nrow(result) > 0L && any(result$error_occurred)
}

#' Get R minor version string
#'
#' Extracts the major.minor version string from the current R version.
#' For example, R 4.3.2 returns "4.3".
#'
#' @return Character string with major.minor version
get_minor_version <- function() {
  paste(
    R.version$major,
    strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L],
    sep = "."
  )
}

#' Check if root package exists in S3
#'
#' Verifies if the latest version of a package exists in the S3 repository,
#' accounting for R minor version sensitivity.
#'
#' @template param-remote_bin_path
#' @template param-package_name
#' @template param-last_version
#' @template param-is_r_minor_sensitive
#' @return Logical indicating if package exists
check_root_package_exists <- function(remote_bin_path, package_name, last_version, is_r_minor_sensitive) {
  # Handle case where is_r_minor_sensitive might be empty/NULL
  if (length(is_r_minor_sensitive) == 0L || is.null(is_r_minor_sensitive)) {
    is_r_minor_sensitive <- FALSE
  }

  if (is_r_minor_sensitive) {
    minor_version <- get_minor_version()
    s3fs::s3_file_exists(file.path(
      remote_bin_path,
      minor_version,
      sprintf("%s_%s.tar.gz", package_name, last_version)
    ))
  } else {
    s3fs::s3_file_exists(file.path(
      remote_bin_path,
      sprintf("%s_%s.tar.gz", package_name, last_version)
    ))
  }
}

#' List archived packages from S3
#'
#' Retrieves list of archived package files for a given package,
#' accounting for R minor version sensitivity.
#'
#' @template param-remote_bin_path
#' @template param-package_name
#' @template param-is_r_minor_sensitive
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @return Character vector of archived package filenames
list_archived_packages <- function(
    remote_bin_path, package_name, is_r_minor_sensitive,
    s3_access_key_id, s3_secret_access_key, s3_endpoint, s3_region) {
  # Establish S3 connection
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  # Handle case where is_r_minor_sensitive might be empty/NULL
  if (length(is_r_minor_sensitive) == 0L || is.null(is_r_minor_sensitive)) {
    is_r_minor_sensitive <- FALSE
  }

  if (is_r_minor_sensitive) {
    minor_version <- get_minor_version()
    basename(s3fs::s3_dir_ls(file.path(
      remote_bin_path,
      minor_version,
      "Archive",
      package_name
    )))
  } else {
    basename(s3fs::s3_dir_ls(file.path(
      remote_bin_path,
      "Archive",
      package_name
    )))
  }
}

#' Get system architecture information
#'
#' Determines the system architecture details needed for package building,
#' including Linux suffix (musl/gnu), tarball ID, and architecture.
#'
#' @template param-binary_output_path
#' @return List with linux_suffix, tarball_id, and tarball_arch elements
get_system_architecture_info <- function(binary_output_path) {
  if (
    any(grepl(
      "alpine",
      system2("cat", args = file.path("/etc", "os-release"), stdout = TRUE),
      fixed = TRUE
    ))
  ) {
    linux_suffix <- "musl"
  } else {
    linux_suffix <- "gnu"
  }

  local_arch <- Sys.info()[["machine"]]
  if (
    grepl("arm64", local_arch, fixed = TRUE) ||
      grepl("aarch64", local_arch, fixed = TRUE)
  ) {
    tarball_id <- "unknown"
    tarball_arch <- "aarch64"
  } else if (
    grepl("amd64", local_arch, fixed = TRUE) ||
      grepl("x86_64", local_arch, fixed = TRUE)
  ) {
    tarball_id <- "pc"
    tarball_arch <- "x86_64"
  }

  if (
    any(grepl(
      "-redhat-linux",
      list.files(binary_output_path, recursive = TRUE),
      fixed = TRUE
    ))
  ) {
    tarball_id <- "redhat"
  }

  list(
    linux_suffix = linux_suffix,
    tarball_id = tarball_id,
    tarball_arch = tarball_arch
  )
}

#' Check if root package exists in S3 for check_s3_packages
#'
#' Helper function to check if the latest package version exists in S3,
#' accounting for R minor version sensitivity.
#'
#' @template param-remote_bin_path
#' @template param-package_name
#' @template param-last_version
#' @template param-is_r_minor_sensitive
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @return Logical indicating if package exists
check_s3_root_package <- function(
    remote_bin_path, package_name, last_version, is_r_minor_sensitive,
    s3_access_key_id, s3_secret_access_key, s3_endpoint, s3_region) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  cli::cli_alert(
    "{.fun check_s3_root_package}: DEBUG {file.path(
      remote_bin_path,
      sprintf('%s_%s.tar.gz', package_name, last_version)
    )}."
  )

  # Handle case where is_r_minor_sensitive might be empty/NULL
  if (length(is_r_minor_sensitive) == 0L || is.null(is_r_minor_sensitive)) {
    is_r_minor_sensitive <- FALSE
  }

  if (is_r_minor_sensitive) {
    minor_version <- get_minor_version()
    s3fs::s3_file_exists(file.path(
      remote_bin_path,
      minor_version,
      sprintf("%s_%s.tar.gz", package_name, last_version)
    ))
  } else {
    s3fs::s3_file_exists(file.path(
      remote_bin_path,
      sprintf("%s_%s.tar.gz", package_name, last_version)
    ))
  }
}

#' Get all packages from S3 for comparison
#'
#' Retrieves both root and archived packages for a given package name,
#' accounting for R minor version sensitivity.
#'
#' @template param-remote_bin_path
#' @template param-package_name
#' @template param-last_version
#' @template param-is_r_minor_sensitive
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @return Character vector of all package filenames
get_all_s3_packages <- function(
    remote_bin_path, package_name, last_version, is_r_minor_sensitive,
    s3_access_key_id, s3_secret_access_key, s3_endpoint, s3_region) {
  # Handle case where is_r_minor_sensitive might be empty/NULL
  if (length(is_r_minor_sensitive) == 0L || is.null(is_r_minor_sensitive)) {
    is_r_minor_sensitive <- FALSE
  }

  archived_pkgs <- list_archived_packages(
    remote_bin_path, package_name, is_r_minor_sensitive,
    s3_access_key_id, s3_secret_access_key, s3_endpoint, s3_region
  )
  root_pkg_name <- sprintf("%s_%s.tar.gz", package_name, last_version)
  c(root_pkg_name, archived_pkgs)
}

#' Process tag filtering for check_s3_packages
#'
#' Determines which tags to process based on the tag parameter.
#'
#' @template param-tag
#' @template param-package_name
#' @template param-source_org_url
#' @template param-tag_limit
#' @return Character vector of filtered tags
process_tag_filtering <- function(tag, package_name, source_org_url, tag_limit) {
  if (length(tag) == 1L && (is.null(tag) || tag == "latest")) {
    filter_tags(package_name, tag = NULL, source_org_url, tag_limit)
  } else {
    tag
  }
}

#' Move and rename built tarball files
#'
#' Renames the built package tarball from the system-specific filename
#' to the standard package_version.tar.gz format.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-binary_output_path
#' @template param-system_info
#' @template param-is_debug
#' @return Invisible NULL
move_and_rename_tarball <- function(
    package_name,
    tag,
    binary_output_path,
    system_info,
    is_debug) {
  source_filename <- sprintf(
    "%s_%s_R_%s-%s-linux-%s.tar.gz",
    package_name,
    tag,
    system_info$tarball_arch,
    system_info$tarball_id,
    system_info$linux_suffix
  )
  source_path <- file.path(binary_output_path, source_filename)
  dest_path <- file.path(
    binary_output_path,
    sprintf("%s_%s.tar.gz", package_name, tag)
  )

  if (is_debug) {
    cli::cli_alert_info(
      "{.fun build_single_tag}: DEBUG: Moving package from {.path {source_path}} to {.path {dest_path}}"
    )
  }

  if (file.exists(source_path)) {
    file.rename(source_path, dest_path)
  } else {
    cli::cli_alert_info(paste0(
      "{.fun build_single_tag}: File for package {.pkg {package_name}} {.field {tag}} ",
      "at {.path {source_path}} does not exist - skipping."
    ))
    if (is_debug) {
      message(sprintf(
        "DEBUG: Listing dir 'binary_output_path': %s",
        binary_output_path
      ))
      message(list.files(binary_output_path))
    }
  }
}
