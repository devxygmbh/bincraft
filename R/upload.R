#' Upload binary to S3
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-is_debug
#' @template param-is_r_minor_sensitive
#' @template param-local_output_dir_root
#' @template param-force
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom s3fs s3_file_exists s3_file_upload s3_file_system
#' @export
upload_single_binary <- function(
    package_name,
    tag,
    s3_endpoint,
    s3_region,
    s3_bucket,
    local_output_dir_root = ".",
    codename = NULL,
    force = FALSE,
    is_r_minor_sensitive = FALSE,
    is_debug = FALSE,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  codename <- set_codename(codename)

  cli::cli_h2("Uploading ({.pkg {package_name[1]}})")

  local_bin_path <- set_bin_path(local_output_dir_root = local_output_dir_root, codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)

  tarball_name <- sprintf("%s_%s.tar.gz", package_name, tag)
  local_tarball_path <- file.path(local_bin_path, tarball_name)


  if (!file.exists(local_tarball_path)) {
    cli::cli_alert(
      "{.fun upload_single_binary}: File {.pkg {package_name}} {.field {tag}} does not exist locally - skipping upload." # nolint
    )
    return(TRUE)
  }

  if (is_debug) {
    cli::cli_alert_warning("DEBUG: local_bin_path: {local_bin_path}")
    cli::cli_alert_warning("DEBUG: remote_bin_path: {remote_bin_path}")
  }

  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  if (is_r_minor_sensitive) {
    minor_version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
    remote_tarball_path <- file.path(remote_bin_path, minor_version, tarball_name)
    file_exists <- s3fs::s3_file_exists(remote_tarball_path)
    archive_path <- file.path(remote_bin_path, minor_version, "Archive", package_name, tarball_name)
    archive_exists <- s3fs::s3_file_exists(archive_path)
  } else {
    remote_tarball_path <- file.path(remote_bin_path, tarball_name)
    file_exists <- s3fs::s3_file_exists(remote_tarball_path)
    archive_path <- file.path(remote_bin_path, "Archive", package_name, tarball_name)
    archive_exists <- s3fs::s3_file_exists(archive_path)
  }
  file_exists <- file_exists | archive_exists

  # don't parallelise
  future::plan("sequential")

  should_upload <- !file_exists || force

  if (should_upload) {
    if (file_exists && force) {
      cli::cli_alert_info(
        "{.fun upload_single_binary}: Force uploading package {.pkg {package_name}} {.field {tag}} to {.path {remote_tarball_path}} because {.code force = TRUE} was set." # nolint
      )
    } else {
      cli::cli_alert(
        "{.fun upload_single_binary}: Uploading {.pkg {package_name}} {.field {tag}} to {.path {remote_tarball_path}}."
      )
    }

    upload_args <- list(
      local_tarball_path,
      remote_tarball_path
    )

    if (file_exists && force) {
      upload_args$max_batch <- parse_bytes("300MB")
      upload_args$overwrite <- TRUE
    }

    do.call(s3fs::s3_file_upload, upload_args)

    cli::cli_alert_success("Successfully uploaded package {.pkg {package_name}} with tag {.field {tag}}.")
    cli::cli_alert(
      "{.fun upload_single_binary}: Deleting binary for {.pkg {package_name}} {.field {tag}} at path {.path {local_tarball_path}}." # nolint line_length_linter
    )
    file.remove(local_tarball_path)
  } else {
    cli::cli_alert(
      "{.fun upload_single_binary}: Package {.pkg {package_name}} {.field {tag}} already exists in S3. Skipping upload."
    )
  }
}

#' Uploads source tarballs to S3
#' @template param-package_name
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-codename
#' @template param-is_r_minor_sensitive
#' @template param-arch
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom utils download.file
#' @export
upload_source_tarball <- function(
    package_name,
    s3_endpoint,
    s3_region,
    s3_bucket,
    codename = NULL,
    arch = NULL,
    is_r_minor_sensitive = FALSE,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)
  version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1]]$commit$message, "version ")[[1]][2] # nolint

  tmpfile <- tempfile()
  # this can fail, e.g. if there was a new package published and shortly
  # after removed by CRAN again due to some hickups.
  # To account for it, we retry the download 3 times and then abort with a message
  # that does not let the whole process to be stopped with an error
  download_url <- sprintf(
    "https://cloud.r-project.org/src/contrib/%s_%s.tar.gz",
    package_name,
    version
  )
  download_successful <- FALSE
  tryCatch(
    {
      # Call the insistent function
      insistent_downloader(url = download_url, destfile = tmpfile)
      # If insistently succeeds without error, set flag to TRUE
      download_successful <- TRUE
      TRUE # Return TRUE from the tryCatch block on success
    },
    error = function(e) {
      # This block executes only if insistently gives up after all retries
      warning(sprintf(
        "Failed to download %s after %d retries: %s. Skipping this package.",
        basename(download_url),
        3L,
        conditionMessage(e) # Display the final error message
      ), call. = FALSE)
      # Set flag to FALSE and return FALSE from the tryCatch block
      download_successful <- FALSE # nolint
      FALSE
    }
  )
  if (!download_successful) {
    cli::cli_alert_warning(
      "Failure downloading source tarball for package {.pkg {package_name}} ({.field {version}})"
    )
    return(TRUE)
  }

  if (is_r_minor_sensitive) {
    minor_version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
    upload_path <- sprintf(
      "s3://%s/%s/%s_%s.tar.gz",
      remote_bin_path, minor_version, package_name, version
    )
  } else {
    upload_path <- sprintf(
      "s3://%s/%s_%s.tar.gz",
      remote_bin_path, package_name, version
    )
  }

  s3fs::s3_file_upload(tmpfile, upload_path, overwrite = TRUE)

  cli::cli_alert("Successfully uploaded source tarball for package
    {.pkg {package_name}} {.field {version}} to {.path {upload_path}}.")
}
