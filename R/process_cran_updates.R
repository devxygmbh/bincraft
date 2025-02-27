#' @title Process updated and new CRAN packages
#' @description
#' Packages which got removed from CRAN can be deleted by setting `prune = TRUE`.
#' Argument `interval` allows to specify a range which should be processed.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-platform
#' @template param-local_output_dir_root
#' @template param-local_clone_dir
#' @template param-interval
#' @template param-archive
#' @template param-metadata_db_host
#' @template param-metadata_db_type
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-store_build_metadata
#' @template param-upload
#' @template param-process_updated
#' @template param-process_new
#' @template param-process_removed
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom dplyr bind_rows pull filter
#' @importFrom purrr walk2
#' @importFrom withr with_options
#' @examples
#' \dontrun{
#' process_cran_updates(
#'   interval = lubridate::interval(lubridate::today() - 2, lubridate::today() - 20),
#' )
#' }
#'
#' @export
process_cran_updates <- function(
    package_name,
    tag,
    platform,
    local_clone_dir,
    interval = lubridate::today(),
    codename = NULL,
    local_output_dir_root = ".",
    s3_endpoint = NULL,
    s3_region = NULL,
    s3_bucket = NULL,
    store_build_metadata = FALSE,
    archive = FALSE,
    upload = FALSE,
    metadata_db_type = "postgres",
    metadata_db_host = NULL,
    metadata_db_name = NULL,
    metadata_db_table = NULL,
    metadata_db_port = NULL,
    metadata_db_user = NULL,
    metadata_db_password = NULL,
    metadata_db_sslmode = NULL,
    process_updated = TRUE,
    process_new = TRUE,
    process_removed = TRUE,
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
  # Get list of updated and new packages for a specific day
  updated_pkgs <- get_updated_cran_packages(interval)
  new_pkgs <- get_new_cran_packages(interval)

  all_pkgs <- dplyr::bind_rows(updated_pkgs, new_pkgs)

  if (any(c(process_updated, process_new))) {
    cli::cli_alert_success("{.fun process_cran_updates}: Updated packages:")
    print(updated_pkgs)
    cli::cli_alert_success("{.fun process_cran_updates}: New packages:")
    print(new_pkgs)

    # make R CMD Check happy
    OS_type <- NULL
    Package <- NULL
    name <- NULL

    `%nin%` <- Negate(`%in%`)
    win_only <- withr::with_options(list(
      repos = structure(c(CRAN = "https://cloud.r-project.org"))
    ), tools::CRAN_package_db()) |>
      filter(`OS_type` == "windows") |>
      pull(Package)

    all_pkgs <- all_pkgs |>
      filter(`name` %nin% win_only)

    purrr::walk2(all_pkgs$name, all_pkgs$version, ~ {
      build_binary_package(.x, .y, platform = platform, upload = upload, archive = archive, store_build_metadata = store_build_metadata, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key, metadata_db_type = metadata_db_type, metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name, metadata_db_table = metadata_db_table, metadata_db_port = metadata_db_port, metadata_db_user = metadata_db_user, metadata_db_password = metadata_db_password, metadata_db_sslmode = metadata_db_sslmode)
      if (archive) {
        archive_package(.x, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key, endpoint = s3_endpoint, bucket = bucket, region = region)
      }
    })
  }

  if (process_removed) {
    removed_pkgs <- get_removed_cran_packages(interval)
    cli::cli_alert_success("{.fun process_cran_updates}: Removed packages:")
    print(removed_pkgs)

    s3fs::s3_file_system(
      aws_access_key_id = s3_access_key_id,
      aws_secret_access_key = s3_secret_access_key,
      endpoint = s3_endpoint,
      region_name = s3_region,
      refresh = TRUE
    )

    codename <- set_codename(codename)

    local_arch <- Sys.info()[["machine"]]
    if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
      arch <- "arm64"
    } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
      arch <- "amd64"
    }

    local_bin_dir <- set_bin_path(local_output_dir_root, codename)
    remote_bin_dir <- sprintf("%s/%s/%s/latest/src/contrib", s3_bucket, arch, codename)

    files <- s3fs::s3_dir_ls(remote_bin_dir)

    foo <- lapply(removed_pkgs$name, function(.x) {
      cli::cli_alert("{.fun process_cran_updates}: Removing package {.pkg {.x}} from S3.")
      files_filtered <- grep(sprintf("/%s_", .x), files, value = TRUE)
      if (length(files_filtered) > 0) {
        s3fs::s3_file_delete(files_filtered)
        cli::cli_alert_success("{.fun process_cran_updates}: Successfully removed {.pkg {basename(files_filtered)}} from S3.")
        remove_from_metadata(.x)
        cli::cli_alert_success("{.fun process_cran_updates}: Successfully set {.pkg {.x}} as 'removed' in metadata table.")
      } else {
        cli::cli_alert("{.fun process_cran_updates}: No tarballs found for package {.pkg {.x}} - already removed?")
      }
    })
  }
}
