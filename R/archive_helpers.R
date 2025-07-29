#' Archive helper functions for package archiving operations

#' Get remote search path for package archives
#'
#' Constructs the S3 path for package archives, accounting for
#' R minor version sensitivity.
#'
#' @template param-remote_bin_dir
#' @template param-is_r_minor_sensitive
#' @template param-minor_version
#' @template param-package_name
#' @return Character string with archive search path
get_remote_search_path <- function(remote_bin_dir, is_r_minor_sensitive, minor_version, package_name) {
  if (is_r_minor_sensitive) {
    file.path(remote_bin_dir, minor_version, "Archive", package_name)
  } else {
    file.path(remote_bin_dir, "Archive", package_name)
  }
}

#' Get archive destination path
#'
#' Constructs the destination path for archiving old package versions,
#' accounting for R minor version sensitivity.
#'
#' @template param-remote_bin_dir
#' @template param-is_r_minor_sensitive
#' @template param-minor_version
#' @template param-package_name
#' @template param-old_versions
#' @return Character vector with archive destination paths
get_archive_path <- function(remote_bin_dir, is_r_minor_sensitive, minor_version, package_name, old_versions) {
  if (is_r_minor_sensitive) {
    file.path(remote_bin_dir, minor_version, "Archive", package_name, basename(old_versions))
  } else {
    file.path(remote_bin_dir, "Archive", package_name, basename(old_versions))
  }
}

#' Find old package versions to archive
#'
#' Identifies which package versions should be archived by comparing
#' against the latest CRAN version. Falls back to version history if needed.
#'
#' @template param-all_versions
#' @template param-package_name
#' @template param-package_name_local
#' @template param-last_version
#' @return List with old_versions and index elements
find_old_versions <- function(all_versions, package_name, package_name_local, last_version) {
  if (any(grepl(sprintf("%s_%s.tar.gz", package_name, last_version), all_versions))) {
    index <- grep(sprintf("_%s.tar.gz", last_version), all_versions, fixed = TRUE)
    return(list(old_versions = all_versions[-index], index = index))
  }

  # Fallback to version history
  versions <- purrr::insistently(
    ~ rev(pak::pkg_history(package_name_local)$Version),
    rate = retry_config,
    quiet = FALSE
  )()

  for (i in versions) {
    version_matches <- vapply(
      strsplit(vapply(strsplit(all_versions, "_", fixed = TRUE), function(x) x[2L], character(1L)),
        ".tar.gz",
        fixed = TRUE
      ),
      function(x) x[1L],
      character(1L)
    )
    if (any(grepl(paste0("^", i, "$"), version_matches))) {
      index <- grep(sprintf("_%s.tar.gz", i), all_versions)
      return(list(old_versions = all_versions[-index], index = index))
    }
  }

  list(old_versions = character(0L), index = integer(0L))
}

#' Clean duplicated package files
#'
#' Removes duplicate package files that may have been created due to
#' build errors or upload issues.
#'
#' @template param-old_versions
#' @return Character vector with duplicates removed
clean_duplicated_packages <- function(old_versions) {
  if (anyDuplicated(s3fs::s3_file_info(old_versions)$key) > 0L) {
    for (i in old_versions) {
      if (anyDuplicated(s3fs::s3_file_info(i)$key)) {
        log_error(sprintf("{.field %s} is duplicated, deleting it.", i))
        s3fs::s3_file_delete(i)
        old_versions <- setdiff(old_versions, i)
      }
    }
  }
  old_versions
}

#' Archive a single package
#'
#' Archives old versions of a package, keeping only the latest version
#' in the main repository and moving older versions to Archive directory.
#'
#' @template param-package_name
#' @template param-remote_bin_dir
#' @template param-is_r_minor_sensitive
#' @template param-minor_version
#' @template param-files
#' @return Invisible NULL
archive_single_package <- function(package_name, remote_bin_dir, is_r_minor_sensitive, minor_version, files) {
  log_header(sprintf("Archiving ({.pkg %s})", package_name))

  remote_search_path <- get_remote_search_path(remote_bin_dir, is_r_minor_sensitive, minor_version, package_name)

  if (!s3fs::s3_dir_exists(remote_search_path)) {
    s3fs::s3_dir_create(remote_search_path)
  }

  all_versions <- grep(sprintf("/%s_", package_name), files, value = TRUE)

  # only archive if more than one package exists in the root
  if (length(all_versions) <= 1L) {
    log_info(sprintf("Skipping {.pkg %s} as only one package versions exists.", package_name))
    return(invisible(NULL))
  }

  # get most recent version from CRAN
  last_version <- strsplit(
    gh::gh(sprintf("GET %s", paste("/repos", "cran", package_name, "commits", sep = "/")))[[1L]]$commit$message, # nolint paste_linter
    "version ",
    fixed = TRUE
  )[[1L]][2L]

  # Find old versions
  result <- find_old_versions(all_versions, package_name, package_name, last_version)
  old_versions <- result$old_versions
  latest_index <- result$index # Used in log message below

  old_versions <- clean_duplicated_packages(old_versions)

  if (length(old_versions) > 0L) {
    archive_path <- get_archive_path(remote_bin_dir, is_r_minor_sensitive, minor_version, package_name, old_versions)

    latest_files <- basename(all_versions[latest_index]) # nolint object_usage_linter
    log_info(sprintf("Archiving %s to %s, keeping %s.", toString(basename(old_versions)), toString(archive_path), toString(latest_files))) # nolint line_length_linter

    s3fs::s3_file_move(
      old_versions,
      archive_path,
      max_batch = parse_bytes("300MB"),
      overwrite = TRUE
    )

    log_success(sprintf("Successfully archived package {.pkg %s}.", package_name))
  }
}
