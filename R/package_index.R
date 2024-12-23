#' Add package to repository index
#' @template param-package_name
#' @template param-codename
#' @template param-endpoint
#' @template param-region
#' @template param-bucket
#' @template param-local_build_root
#' @template param-debug
#'
#' @importFrom cranlike add_PACKAGES
#' @importFrom s3fs s3_dir_ls s3_file_system
#' @export
add_to_package_index <- function(
    package_name = NULL,
    endpoint = "fsn1.your-objectstorage.com",
    region = "fsn1",
    bucket = "devxy-r-package-binaries",
    local_build_root = "/mnt/cache/binaries",
    codename = NULL,
    debug = FALSE) {
  codename <- set_codename(codename)

  local_arch <- Sys.info()[["machine"]]
  if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
    arch <- "arm64"
  } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
    arch <- "amd64"
  }

  local_bin_dir <- set_bin_path(local_build_root, codename)
  remote_bin_dir <- sprintf("%s/%s/%s/latest/src/contrib", bucket, arch, codename)

  s3fs::s3_file_system(
    aws_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
    aws_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
    endpoint = endpoint,
    region_name = region,
  )

  # get latest PACKAGES file from S3
  if (!s3fs::s3_file_exists(sprintf("%s/PACKAGES", remote_bin_dir))) {
    s3fs::s3_file_download(sprintf("%s/PACKAGES", remote_bin_dir), sprintf("%s/PACKAGES", local_bin_dir))
  }
  file_names <- list.files(local_bin_dir, pattern = sprintf("%s*", package_name))

  # list all tarballs for the given package
  cranlike::add_PACKAGES(file_names, local_bin_dir)

  return(invisible(TRUE))
}

#' Upload package index files to S3
#' @template param-package_name
#' @template param-codename
#' @template param-endpoint
#' @template param-region
#' @template param-bucket
#' @template param-debug
#' @template param-local_build_root
#' @template param-arch
#'
#' @importFrom s3fs s3_file_upload s3_dir_ls
#' @importFrom cranlike update_PACKAGES
#' @export
upload_package_index <- function(
    package_name = NULL,
    endpoint = "fsn1.your-objectstorage.com",
    region = "fsn1",
    bucket = "devxy-r-package-binaries",
    local_build_root = ".",
    codename = NULL,
    debug = FALSE,
    arch = NULL) {
  cli::cli_alert("{.fun upload_package_index}: Updating PACKAGES* files in S3.")

  codename <- set_codename(codename)

  if (is.null(arch)) {
    local_arch <- Sys.info()[["machine"]]
    if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
      arch <- "arm64"
    } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
      arch <- "amd64"
    }
  }

  local_bin_dir <- set_bin_path(local_build_root, codename)
  remote_bin_dir <- sprintf("%s/%s/%s/latest/src/contrib", bucket, arch, codename)

  s3fs::s3_file_system(
    aws_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
    aws_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
    endpoint = endpoint,
    region_name = region,
  )

  cli::cli_alert("{.fun upload_package_index}: Started listing remote packages")
  pkgs <- s3fs::s3_dir_ls(remote_bin_dir)
  cli::cli_alert_success("{.fun upload_package_index}: Finished listing remote packages")
  # We remove 4 from the count as we don't want to count the PACKAGES* files + Archive/ dir
  pkg_count <- length(pkgs) - 5
  unique_pkgs <- length(unique(sapply(strsplit(basename(pkgs), "_"), function(x) x[1]))) - 5

  t1 <- Sys.time()
  cranlike::update_PACKAGES(sprintf("s3://%s", remote_bin_dir))

  # write Meta/archive.rds for remotes::install_version
  cli::cli_alert_success("{.fun upload_package_index}: Started creating/updating {.path Meta/archive.rds}")
  files <- s3fs::s3_dir_ls(sprintf("%s/Archive", remote_bin_dir), recurse = TRUE, regexp = "*.tar.gz")
  archive_rds <- write_archive_rds(files)
  tmp <- tempfile()
  saveRDS(archive_rds, tmp)
  s3fs::s3_file_upload(tmp, sprintf("%s/Meta/archive.rds", remote_bin_dir), overwrite = TRUE, CacheControl = "no-store")
  cli::cli_alert_success("{.fun upload_package_index}: Successfully uploaded {.path Meta/archive.rds}")

  total_build_time <- round(Sys.time() - t1, 2)
  cli::cli_alert("{.fun upload_package_index}: Time updating PACKAGES index for {.field {pkg_count}} ({.field {unique_pkgs}} unique) packages: {.strong {total_build_time} {units(difftime(Sys.time(), t1))}}.")

  purrr::walk2(
    c("PACKAGES", "PACKAGES.db", "PACKAGES.rds", "PACKAGES.gz"),
    sprintf("%s/%s", remote_bin_dir, c("PACKAGES", "PACKAGES.db", "PACKAGES.rds", "PACKAGES.gz")),
    \(x, y) s3fs::s3_file_upload(x, y, overwrite = TRUE, CacheControl = "no-store")
  )

  return(invisible(TRUE))
}
