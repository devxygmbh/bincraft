#' Upload binary to S3
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-endpoint
#' @template param-region
#' @template param-bucket
#' @template param-debug
#' @template param-local_build_root
#' @template param-force
#'
#' @importFrom s3fs s3_file_exists s3_file_upload s3_file_system
#' @export
upload_single_binary <- function(
    endpoint = "https://s3.eu-central-003.backblazeb2.com",
    region = "eu-central-003",
    bucket = "devxy-arm64-r-binaries",
    local_build_root = "/root",
    codename = NULL,
    package_name,
    tag,
    force = FALSE,
    debug = FALSE) {
  codename <- set_codename(codename)

  cli::cli_h2("Uploading ({.pkg {package_name[1]}})")

  local_bin_path <- set_bin_path(local_build_root = local_build_root, codename)
  remote_bin_path <- set_bin_path(local_build_root = bucket, codename)

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
    aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
    aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    endpoint = endpoint,
    region_name = region,
  )

  exists <- s3fs::s3_file_exists(sprintf("%s/%s", remote_bin_path, tarball_name))

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
#' @template param-endpoint
#' @template param-region
#' @template param-bucket
#' @template param-codename
#' @template param-arch
#' @importFrom utils download.file
#' @export
upload_source_tarball <- function(
    package_name,
    endpoint = "https://s3.eu-central-003.backblazeb2.com",
    region = "eu-central-003",
    bucket = "devxy-arm64-r-binaries",
    codename = NULL,
    arch = NULL) {
  s3fs::s3_file_system(
    aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
    aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    endpoint = endpoint,
    region_name = region,
  )

  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_build_root = bucket, codename)
  version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits/master", package_name))$commit$message, "version ")[[1]][2]

  tmpfile <- tempfile()
  download.file(sprintf("https://cloud.r-project.org/src/contrib/%s_%s.tar.gz", package_name, version), tmpfile, quiet = TRUE)

  s3fs::s3_file_upload(tmpfile, sprintf("s3://%s/%s_%s.tar.gz", remote_bin_path, package_name, version), overwrite = TRUE)

  cli::cli_alert("Successfully uploaded source tarball for package {.pkg {package_name}} {.field {version}} to {.path {remote_bin_path}}.")
}
