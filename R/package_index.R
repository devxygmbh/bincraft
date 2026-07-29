#' Construct a `Built` field value for the current build environment
#'
#' cranlike derives package metadata from the CRAN *source* `DESCRIPTION`, which
#' never carries a `Built:` field (R stamps that only when it builds a binary).
#' Our S3 tarballs *are* binaries, so we pass this stamp to
#' [cranlike::update_PACKAGES()]/[cranlike::add_PACKAGES()] to advertise them as
#' such. Binary-aware clients (e.g. `uvr`) key their binary-vs-source decision on
#' the `Built:` platform triple + R minor; without it they treat the repo as
#' source-only and compile everything, which needs system `-dev` libraries.
#'
#' The triple comes from `R.version$platform` and the version from
#' [getRversion()], so this must run in the same build environment that produced
#' the binaries (the surrounding arch/codename logic already assumes this).
#'
#' @keywords internal
built_stamp <- function(
  platform = R.version$platform,
  r_version = getRversion(),
  time = Sys.time()
) {
  sprintf(
    "R %s; %s; %s UTC; unix",
    r_version,
    platform,
    format(as.POSIXlt(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  )
}

#' Build the S3 remote contrib dir for a package index
#'
#' @param r_minor Optional `"major.minor"` string (e.g. `"4.4"`). When non-NULL
#'   the path points at the per-minor slot.
#' @keywords internal
package_index_remote_dir <- function(
  s3_bucket,
  arch,
  codename,
  r_minor = NULL
) {
  base <- file.path(s3_bucket, arch, codename, "latest", "src", "contrib")
  if (is.null(r_minor)) {
    base
  } else {
    file.path(base, r_minor)
  }
}

#' Add package to repository index
#' @template param-package_name
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-local_output_dir_root
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @param r_minor Optional `"major.minor"` string. When set, the index is
#'   written/read under the per-minor slot `…/contrib/<r_minor>/` instead of the
#'   generic `…/contrib/` slot.
#'
#' @importFrom cranlike add_PACKAGES
#' @importFrom s3fs s3_dir_ls s3_file_system
#' @export
add_to_package_index <- function(
  s3_endpoint,
  s3_region,
  s3_bucket,
  package_name = NULL,
  local_output_dir_root = file.path("mnt", "cache", "binaries"),
  codename = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  r_minor = NULL
) {
  codename <- set_codename(codename)

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

  local_bin_dir <- set_bin_path(local_output_dir_root, codename)
  remote_bin_dir <- package_index_remote_dir(s3_bucket, arch, codename, r_minor)

  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  # get latest PACKAGES file from S3
  if (!s3fs::s3_file_exists(file.path(remote_bin_dir, "PACKAGES"))) {
    s3fs::s3_file_download(
      file.path(remote_bin_dir, "PACKAGES"),
      file.path(local_bin_dir, "PACKAGES")
    )
  }
  file_names <- list.files(
    local_bin_dir,
    pattern = sprintf("%s*", package_name)
  )

  # list all tarballs for the given package
  cranlike::add_PACKAGES(file_names, local_bin_dir, built = built_stamp())

  invisible(TRUE)
}

#' Upload package index files to S3
#' @template param-package_name
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-local_output_dir_root
#' @template param-arch
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @param r_minor Optional `"major.minor"` string. When set, the index is
#'   written/read under the per-minor slot `…/contrib/<r_minor>/` instead of the
#'   generic `…/contrib/` slot.
#'
#' @importFrom s3fs s3_file_upload s3_dir_ls
#' @importFrom cranlike update_PACKAGES
#' @export
upload_package_index <- function(
  s3_endpoint,
  s3_region,
  s3_bucket,
  package_name = NULL,
  local_output_dir_root = ".",
  codename = NULL,
  arch = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  r_minor = NULL
) {
  log_info("Updating PACKAGES* files in S3.")

  codename <- set_codename(codename)

  if (is.null(arch)) {
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
  }

  remote_bin_dir <- package_index_remote_dir(s3_bucket, arch, codename, r_minor)

  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  log_info("Started listing remote packages")
  pkgs <- s3fs::s3_dir_ls(remote_bin_dir)
  log_success("Finished listing remote packages")
  # We remove 4 from the count as we don't want to count the PACKAGES* files + Archive/ dir
  pkg_count <- length(pkgs) - 5L
  unique_pkgs <- length(unique(vapply(
    strsplit(basename(pkgs), "_", fixed = TRUE),
    function(x) x[1L],
    character(1L)
  ))) -
    5L

  t1 <- Sys.time()
  built <- built_stamp()
  retry_s3_operation(
    function() {
      cranlike::update_PACKAGES(
        sprintf("s3://%s", remote_bin_dir),
        built = built
      )
    },
    label = "update PACKAGES"
  )

  # write Meta/archive.rds for remotes::install_version
  log_success(
    sprintf(
      "Started creating/updating {.path %s}",
      file.path("Meta", "archive.rds")
    )
  )
  retry_s3_operation(
    function() {
      files <- s3fs::s3_dir_ls(
        file.path(remote_bin_dir, "Archive"),
        recurse = TRUE,
        regexp = "*.tar.gz"
      )
      archive_rds <- write_archive_rds(files)
      tmp <- tempfile()
      saveRDS(archive_rds, tmp)
      s3fs::s3_file_upload(
        tmp,
        file.path(remote_bin_dir, "Meta", "archive.rds"),
        overwrite = TRUE,
        CacheControl = "no-store"
      )
    },
    label = file.path("update Meta", "archive.rds")
  )
  log_success(
    sprintf(
      "Successfully uploaded {.path %s}",
      file.path("Meta", "archive.rds")
    )
  )

  total_build_time <- round(Sys.time() - t1, 2L)
  time_units <- units(difftime(Sys.time(), t1))
  log_info(sprintf(
    "Time updating PACKAGES index for %s (%s unique) packages: %s %s.",
    pkg_count,
    unique_pkgs,
    total_build_time,
    time_units
  ))

  purrr::walk2(
    c("PACKAGES", "PACKAGES.db", "PACKAGES.rds", "PACKAGES.gz"),
    file.path(
      remote_bin_dir,
      c("PACKAGES", "PACKAGES.db", "PACKAGES.rds", "PACKAGES.gz")
    ),
    \(x, y) {
      s3fs::s3_file_upload(x, y, overwrite = TRUE, CacheControl = "no-store")
    }
  )

  log_success(
    "Successfully uploaded {.path PACKAGES}, {.path PACKAGES.db}, {.path PACKAGES.rds}, {.path PACKAGES.gz}"
  )

  invisible(TRUE)
}
