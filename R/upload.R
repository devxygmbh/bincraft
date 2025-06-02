#' Upload binary to S3
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-debug
#' @template param-local_output_dir_root
#' @template param-force
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom s3fs s3_file_exists s3_file_upload s3_file_system
#' @export
upload_single_binary <- function(
    s3_endpoint,
    s3_region,
    s3_bucket,
    local_output_dir_root = ".",
    codename = NULL,
    package_name,
    tag,
    force = FALSE,
    debug = FALSE,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  codename <- set_codename(codename)

  cli::cli_h2("Uploading ({.pkg {package_name[1]}})")

  local_bin_path <- set_bin_path(local_output_dir_root = local_output_dir_root, codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)

  tarball_name <- sprintf("%s_%s.tar.gz", package_name, tag)

  if (!file.exists(sprintf("%s/%s", local_bin_path, tarball_name))) {
    cli::cli_alert("{.fun upload_single_binary}: File {.pkg {package_name}} {.field {tag}} does not exist locally - skipping upload.")
    return(TRUE)
  }

  if (debug) {
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

  exists <- s3fs::s3_file_exists(sprintf("%s/%s", remote_bin_path, tarball_name))
  archive_exists <- s3fs::s3_file_exists(sprintf("%s/Archive/%s/%s", remote_bin_path, package_name, tarball_name))
  exists <- exists || archive_exists

  # suppress progressr output here
  progressr::handlers("void")
  # don't parallelise
  future::plan("sequential")

  if ((!exists && !force) || (!exists && force)) {
    cli::cli_alert("{.fun upload_single_binary}: Uploading {.pkg {package_name}} {.field {tag}} to {.path {sprintf('%s/%s', remote_bin_path, tarball_name)}}.")
    s3fs::s3_file_upload(
      sprintf("%s/%s", local_bin_path, tarball_name),
      sprintf("%s/%s", remote_bin_path, tarball_name)
    )
    cli::cli_alert_success("Successfully uploaded package {.pkg {package_name}} with tag {.field {tag}}.")
    cli::cli_alert("{.fun upload_single_binary}: Deleting binary for {.pkg {package_name}} {.field {tag}} at path {.path {sprintf('%s/%s', local_bin_path, tarball_name)}}.")
    file.remove(sprintf("%s/%s", local_bin_path, tarball_name))
  } else if (exists && force) {
    cli::cli_alert_info("{.fun upload_single_binary}: Force uploading package {.pkg {package_name}} {.field {tag}} to {.path {sprintf('%s/%s', remote_bin_path, tarball_name)}} because {.code force = TRUE} was set.")
    s3fs::s3_file_upload(
      sprintf("%s/%s", local_bin_path, tarball_name),
      sprintf("%s/%s", remote_bin_path, tarball_name),
      max_batch = fs::fs_bytes("300MB"),
      overwrite = TRUE
    )
    cli::cli_alert_success("Successfully uploaded package {.pkg {package_name}} with tag {.field {tag}}.")
    cli::cli_alert("{.fun upload_single_binary}: Deleting binary for {.pkg {package_name}} {.field {tag}} at path {.path {sprintf('%s/%s', local_bin_path, tarball_name)}}.")
    file.remove(sprintf("%s/%s", local_bin_path, tarball_name))
  } else if (exists && !force) {
    cli::cli_alert("{.fun upload_single_binary}: Package {.pkg {package_name}} {.field {tag}} already exists in S3. Skipping upload.")
  }
}

#' Uploads source tarballs to S3
#' @template param-package_name
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-codename
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
  version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1]]$commit$message, "version ")[[1]][2]

  tmpfile <- tempfile()
  # this can fail, e.g. if there was a new package published and shortly after removed by CRAN again due to some hickups. To account for it, we retry the download 3 times and then abort with a message that does not let the whole process to be stopped with an error
  download_url <- sprintf(
    "https://cloud.r-project.org/src/contrib/%s_%s.tar.gz",
    package_name,
    version
  )
  download_successful <- FALSE
  final_result <- tryCatch(
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
        3,
        conditionMessage(e) # Display the final error message
      ))
      # Set flag to FALSE and return FALSE from the tryCatch block
      download_successful <- FALSE
      FALSE
    }
  )
  if (!download_successful) {
    cli::cli_alert_warning(
      "Failure downloading source tarball for package {.pkg {package_name}} ({.field {version}})"
    )
    return(TRUE)
  }

  s3fs::s3_file_upload(tmpfile, sprintf("s3://%s/%s_%s.tar.gz", remote_bin_path, package_name, version), overwrite = TRUE)

  cli::cli_alert("Successfully uploaded source tarball for package {.pkg {package_name}} {.field {version}} to {.path {remote_bin_path}}.")
}
