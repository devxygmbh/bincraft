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
#' @template param-force
#' @template param-filter_r_minor_sensitive
#' @template param-r_minor_packages_forge_type
#' @template param-r_minor_packages_issue_url
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
#'   interval = lubridate::interval(lubridate::today() - 3, lubridate::today() - 5L),
#'   s3_endpoint = "https://hel1.your-objectstorage.com", s3_region = "hel1",
#'   s3_bucket = "devxy-r-package-binaries-hel1",
#'   s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
#'   s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
#'   process_removed = FALSE,
#'   platform = "alpine-321"
#' )
#' }
#'
#' @export

#' Process removed packages from CRAN
#'
#' @noRd
process_removed_packages <- function(
  interval,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region,
  s3_bucket,
  codename,
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_table,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode
) {
  removed_pkgs <- get_removed_cran_packages(interval)

  cli::cli_par()
  cli::cli_end()
  log_success("{.fun process_cran_updates}: Removed packages:")
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

  remote_bin_dir <- file.path(
    s3_bucket,
    arch,
    codename,
    "latest",
    "src",
    "contrib"
  )
  files <- s3fs::s3_dir_ls(remote_bin_dir)

  lapply(removed_pkgs$name, function(.x) {
    log_info(sprintf(
      "{.fun process_cran_updates}: Removing package {.pkg %s} from S3.",
      .x
    ))
    files_filtered <- grep(sprintf("/%s_", .x), files, value = TRUE)

    if (length(files_filtered) > 0L) {
      s3fs::s3_file_delete(files_filtered)
      log_success(
        sprintf(
          "{.fun process_cran_updates}: Successfully removed {.pkg %s} from S3.",
          toString(basename(files_filtered))
        )
      )

      remove_from_metadata(
        .x,
        metadata_db_type = metadata_db_type,
        metadata_db_host = metadata_db_host,
        metadata_db_name = metadata_db_name,
        metadata_db_table = metadata_db_table,
        metadata_db_port = metadata_db_port,
        metadata_db_user = metadata_db_user,
        metadata_db_password = metadata_db_password,
        metadata_db_sslmode = metadata_db_sslmode
      )

      log_success(
        sprintf(
          "{.fun process_cran_updates}: Successfully set {.pkg %s} as 'removed' in metadata table.",
          .x
        )
      )
    } else {
      log_info(sprintf(
        "{.fun process_cran_updates}: No tarballs found for package {.pkg %s} - already removed?",
        .x
      ))
    }
  })
}

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
  force = FALSE,
  filter_r_minor_sensitive = FALSE,
  r_minor_packages_forge_type = "Forgejo",
  r_minor_packages_issue_url = NULL,
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

  if (process_removed) {
    process_removed_packages(
      interval = interval,
      s3_access_key_id = s3_access_key_id,
      s3_secret_access_key = s3_secret_access_key,
      s3_endpoint = s3_endpoint,
      s3_region = s3_region,
      s3_bucket = s3_bucket,
      codename = codename,
      metadata_db_type = metadata_db_type,
      metadata_db_host = metadata_db_host,
      metadata_db_name = metadata_db_name,
      metadata_db_table = metadata_db_table,
      metadata_db_port = metadata_db_port,
      metadata_db_user = metadata_db_user,
      metadata_db_password = metadata_db_password,
      metadata_db_sslmode = metadata_db_sslmode
    )
  }

  # Get list of updated and new packages for a specific day
  updated_pkgs <- get_updated_cran_packages(interval)
  new_pkgs <- get_new_cran_packages(interval)
  all_pkgs <- dplyr::bind_rows(updated_pkgs, new_pkgs)

  new_pkgs <- get_new_cran_packages(interval)

  all_pkgs <- dplyr::bind_rows(updated_pkgs, new_pkgs)

  if (filter_r_minor_sensitive) {
    all_pkgs <- get_r_minor_sensitive_packages(
      r_minor_packages_forge_type,
      r_minor_packages_issue_url,
      interval,
      updated_packages = updated_pkgs,
      new_packages = new_pkgs
    )
  }

  if (any(c(process_updated, process_new))) {
    cli::cli_par()
    cli::cli_end()

    if (filter_r_minor_sensitive) {
      log_success("{.fun process_cran_updates}: Packages to process:")
      print(all_pkgs)
    } else {
      log_success("{.fun process_cran_updates}: Updated packages:")
      print(updated_pkgs)

      cli::cli_par()
      cli::cli_end()
      log_success("{.fun process_cran_updates}: New packages:")
      print(new_pkgs)
    }

    # make R CMD Check happy
    name <- NULL

    cran_repos <- c(CRAN = "https://cloud.r-project.org")
    names(cran_repos) <- "CRAN"

    win_only <- purrr::insistently(
      ~ withr::with_options(
        list(repos = cran_repos),
        tools::CRAN_package_db()
      ) |>
        filter(`OS_type` == "windows") |>
        pull(Package),
      rate = retry_config,
      quiet = FALSE
    )()

    all_pkgs <- filter(all_pkgs, `name` %nin% win_only)

    if (nrow(all_pkgs) > 0L) {
      purrr::walk2(
        all_pkgs$name,
        all_pkgs$version,
        ~ {
          build_binary_package(
            .x,
            .y,
            platform = platform,
            upload = upload,
            archive = archive,
            force = force,
            store_build_metadata = store_build_metadata,
            s3_endpoint = s3_endpoint,
            s3_bucket = s3_bucket,
            s3_region = s3_region,
            s3_access_key_id = s3_access_key_id,
            s3_secret_access_key = s3_secret_access_key,
            is_r_minor_sensitive = filter_r_minor_sensitive,
            metadata_db_type = metadata_db_type,
            metadata_db_host = metadata_db_host,
            metadata_db_name = metadata_db_name,
            metadata_db_table = metadata_db_table,
            metadata_db_port = metadata_db_port,
            metadata_db_user = metadata_db_user,
            metadata_db_password = metadata_db_password,
            metadata_db_sslmode = metadata_db_sslmode
          )
        }
      )
    } else {
      log_info("No packages to process after filtering Windows-only packages")
    }
  }
}

#' Returns R minor sensitive R packages
#' @description
#' If `interval` is set, it checks for updates of these packages in the CRAN repository and returns a filtered response.
#'
#' @template param-r_minor_packages_forge_type
#' @template param-r_minor_packages_issue_url
#' @template param-interval
#' @template param-updated_packages
#' @template param-new_packages
#' @export
get_r_minor_sensitive_packages <- function(
  r_minor_packages_forge_type = "Forgejo", # nolint
  r_minor_packages_issue_url = NULL,
  interval = NULL,
  updated_packages = NULL,
  new_packages = NULL
) {
  if (!is.null(interval)) {
    if (is.null(updated_packages) && is.null(new_packages)) {
      # Get list of updated and new packages for a specific day
      updated_packages <- get_updated_cran_packages(interval)
      new_packages <- get_new_cran_packages(interval)
    }
    all_pkgs <- dplyr::bind_rows(updated_packages, new_packages)
  }

  # Get list of R minor sensitive packages
  if (r_minor_packages_forge_type == "Forgejo") {
    resp <- httr2::request(r_minor_packages_issue_url) |> httr2::req_perform()
    resp_body <- httr2::resp_body_json(resp)
    body_vector <- strsplit(resp_body$body, "\n", fixed = TRUE)[[1L]]
  } else if (r_minor_packages_forge_type == "GitHub") {
    resp <- httr2::request(r_minor_packages_issue_url) |>
      httr2::req_headers(
        Accept = "application/vnd.github.v3+json" # nolint
      ) |>
      httr2::req_perform()
    resp_body <- httr2::resp_body_json(resp)
    body_vector <- strsplit(resp_body$body, "\n", fixed = TRUE)[[1L]]
  }

  if (!is.null(interval)) {
    # Filter all_pkgs to only those that are R minor sensitive
    sensitive_pkgs_to_process <- intersect(all_pkgs$name, body_vector)
  }

  if (!is.null(interval)) {
    if (length(sensitive_pkgs_to_process) == 0L) {
      log_info(
        "No R sensitive packages found in packages to be processed. Exiting."
      )
      return(invisible(TRUE))
    }
    return(sensitive_pkgs_to_process)
  }
  body_vector
}
