#' @title Process updated and new CRAN packages
#' @description
#' Packages which got removed from CRAN can be deleted by setting `prune = TRUE`.
#' Argument `interval` allows to specify a range which should be processed.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-endpoint
#' @template param-region
#' @template param-bucket
#' @template param-platform
#' @template param-local_build_root
#' @template param-local_clone_dir
#' @template param-interval
#' @template param-process_updated
#' @template param-process_new
#' @template param-process_removed
#'
#' @importFrom dplyr bind_rows pull filter
#' @importFrom purrr walk2
#' @importFrom withr with_options
#' @examples
#' \dontrun{
#' process_cran_updates(
#'   interval = lubridate::interval(lubridate::today() - 2, lubridate::today() - 20),
#'   process_updated = FALSE, process_new = FALSE
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
    local_build_root = ".",
    endpoint = "fsn1.your-objectstorage.com",
    region = "fsn1",
    bucket = "devxy-r-package-binaries",
    process_updated = TRUE,
    process_new = TRUE,
    process_removed = TRUE) {
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
      build_binary_package(.x, .y, platform = platform)
      archive_package(.x)
    })
  }

  if (process_removed) {
    removed_pkgs <- get_removed_cran_packages(interval)
    cli::cli_alert_success("{.fun process_cran_updates}: Removed packages:")
    print(removed_pkgs)

    s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
      aws_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
      endpoint = endpoint,
      region_name = region,
    )

    codename <- set_codename(codename)

    local_arch <- Sys.info()[["machine"]]
    if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
      arch <- "arm64"
    } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
      arch <- "amd64"
    }

    local_bin_dir <- set_bin_path(local_build_root, codename)
    remote_bin_dir <- sprintf("%s/%s/%s/latest/src/contrib", bucket, arch, codename)

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
