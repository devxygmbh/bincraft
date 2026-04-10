#' Build R binary packages
#'
#' @description
#' Builds binary packages from a given URL for a specific architecture.
#' Git tags are used to determine all possible versions to build.
#'
#' System dependencies are automatically installed through \pkg{pak}.
#'
#' The function also automatically archives older versions into an `Archive/`
#' directory to keep only the most recent one in the repository root.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-tag_limit
#' @template param-codename
#' @template param-arch
#' @template param-platform
#' @template param-local_output_dir_root
#' @template param-local_clone_dir
#' @template param-install_system_dependencies
#' @template param-is_r_minor_sensitive
#' @template param-archive
#' @template param-store_build_metadata
#' @template param-metadata_db_host
#' @template param-metadata_db_type
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-upload
#' @template param-force
#' @template param-source_org_url
#'
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#'
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom future.apply future_mapply
#' @importFrom gert git_config_global_set git_clone
#' @importFrom pak local_install_dev_deps
#' @importFrom pkgbuild build
#'
#' @examples
#' \dontrun{
#' # build from cran
#' build_binary_package("brew", archive = FALSE)
#' build_binary_package("brew",
#'   source_org_url = "https://my.git.com/rpkgs/",
#'   archive = FALSE
#' )
#' }
#'
#' @export
build_binary_package <- function(
  package_name,
  tag = NULL,
  codename = NULL,
  source_org_url = "https://github.com/cran",
  tag_limit = 10L,
  local_output_dir_root = ".",
  local_clone_dir = "/tmp",
  platform = NULL,
  arch = NULL,
  is_r_minor_sensitive = FALSE,
  install_system_dependencies = TRUE,
  force = FALSE,
  upload = FALSE,
  archive = FALSE,
  store_build_metadata = FALSE,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  s3_endpoint = NULL,
  s3_region = NULL,
  s3_bucket = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL
) {
  # Initialize and prepare
  setup_result <- initialize_build_environment(
    package_name,
    codename,
    platform,
    arch,
    local_output_dir_root,
    force,
    s3_bucket,
    s3_access_key_id,
    s3_secret_access_key,
    s3_endpoint,
    s3_region
  )

  if (setup_result$should_skip) {
    return("skipped")
  }

  binary_output_path <- setup_result$binary_output_path
  local_bin_path <- setup_result$local_bin_path
  platform <- setup_result$platform
  arch <- setup_result$arch

  if (is.null(tag) || tag == "latest") {
    tag <- filter_tags(package_name, tag, source_org_url, tag_limit)
    package_name <- rep(package_name, length(tag))
  }

  # Determine packages to build
  pkg_info <- determine_packages_to_build(
    package_name,
    tag,
    source_org_url,
    tag_limit,
    codename,
    force,
    is_r_minor_sensitive,
    s3_bucket,
    s3_access_key_id,
    s3_secret_access_key,
    s3_endpoint,
    s3_region,
    binary_output_path,
    store_build_metadata,
    metadata_db_type,
    metadata_db_host,
    metadata_db_name,
    metadata_db_table,
    metadata_db_port,
    metadata_db_user,
    metadata_db_password,
    metadata_db_sslmode,
    platform,
    arch
  )

  if (pkg_info$should_skip) {
    return("skipped")
  }

  package_name <- pkg_info$package_name
  tag <- pkg_info$tag

  result <- execute_package_builds(
    package_name,
    tag,
    binary_output_path,
    source_org_url,
    local_clone_dir,
    platform,
    arch,
    codename,
    is_r_minor_sensitive,
    force,
    install_system_dependencies,
    store_build_metadata,
    metadata_db_host,
    metadata_db_name,
    metadata_db_port,
    metadata_db_table,
    metadata_db_password,
    metadata_db_user,
    metadata_db_sslmode,
    s3_endpoint,
    s3_bucket,
    s3_region,
    s3_access_key_id,
    s3_secret_access_key,
    local_bin_path
  )

  # Handle upload and archiving
  handle_post_build_actions(
    package_name,
    tag,
    result,
    codename,
    upload,
    archive,
    force,
    is_r_minor_sensitive,
    s3_endpoint,
    s3_bucket,
    s3_region,
    s3_access_key_id,
    s3_secret_access_key
  )

  invisible(TRUE)
}

#' Initialize build environment and setup paths
#'
#' Sets up the build environment including codename, platform detection,
#' architecture detection, and directory creation.
#'
#' @template param-package_name
#' @template param-codename
#' @template param-platform
#' @template param-arch
#' @template param-local_output_dir_root
#' @template param-force
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @return List with setup results
initialize_build_environment <- function(
  package_name,
  codename,
  platform,
  arch,
  local_output_dir_root,
  force,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region
) {
  cli::cli_h2(sprintf("Preparations ({.pkg %s})", package_name))
  codename <- set_codename(codename)

  if (is.null(platform)) {
    platform <- switch(
      codename,
      jammy = "ubuntu-2204",
      noble = "ubuntu-2404",
      rhel9 = "redhat-9",
      rhel8 = "redhat-8",
      alpine320 = "alpine-320",
      alpine321 = "alpine-321",
      alpine322 = "alpine-322",
      alpine323 = "alpine-323",
      alpine324 = "alpine-324",
      alpine325 = "alpine-325",
      alpine326 = "alpine-326"
    )
  }

  log_debug(sprintf("codename: %s", codename))

  binary_output_path <- set_bin_path(local_output_dir_root, codename)
  local_bin_path <- set_bin_path(
    local_output_dir_root = local_output_dir_root,
    codename
  )

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

  log_debug(sprintf("binary_output_path: %s.", binary_output_path))

  dir_out_src <- file.path(local_output_dir_root, "src", "contrib", "Archive")
  log_debug(
    sprintf(
      "{.fun build_binary_package}: Creating bin dir {.path %s}.",
      binary_output_path
    )
  )
  log_debug(
    sprintf(
      "{.fun build_binary_package}: Creating src dir {.path %s}.",
      dir_out_src
    )
  )
  dir.create(
    file.path(binary_output_path, "Archive"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(dir_out_src, "Archive"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  list(
    binary_output_path = binary_output_path,
    local_bin_path = local_bin_path,
    platform = platform,
    arch = arch,
    should_skip = FALSE
  )
}

#' Filter git tags for package versions
#'
#' Retrieves and filters git tags from a package repository, excluding
#' R-prefixed tags and applying tag limits.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-source_org_url
#' @template param-tag_limit
#' @return Character vector of filtered tags
filter_tags <- function(package_name, tag, source_org_url, tag_limit) {
  gert::git_config_global_set("advice.detachedHead", "false")

  gert::git_clone(
    sprintf("%s/%s", source_org_url, package_name),
    path = file.path(tempdir(), "tmp1"),
    verbose = FALSE
  )

  if (!is.null(tag) && tag == "latest") {
    tags <- withr::with_dir(
      file.path(tempdir(), "tmp1"),
      system("git tag --sort=-creatordate | head -1", intern = TRUE)
    )
  } else {
    if (!is.null(tag_limit)) {
      all_tags <- withr::with_dir(
        file.path(tempdir(), "tmp1"),
        system("git tag --sort=-creatordate", intern = TRUE)
      )
      all_tags <- all_tags[!grepl("R-", all_tags, fixed = TRUE)]
      if (length(all_tags) < tag_limit) {
        tag_limit <- length(all_tags)
      } else {
        log_info(
          sprintf(
            "{.fun filter_tags}: Filtered for the %s most recent tags (out of {.field %s} total)",
            tag_limit,
            length(all_tags)
          )
        )
      }
      tags <- all_tags[1L:tag_limit]
    } else {
      # gert does not support sorting so we cannot use it for the above condition
      all_tags <- gert::git_tag_list(repo = file.path(tempdir(), "tmp1"))
      all_tags <- all_tags[!grepl("R-", all_tags$name, fixed = TRUE), ]
      tags <- all_tags$name
    }
    unlink(file.path(tempdir(), "tmp1"), force = TRUE, recursive = TRUE)
  }
  tags
}

#' Filter packages with previous build errors
#'
#' Queries the metadata database to identify packages that have
#' previously failed to build and should be skipped.
#'
#' @template param-pkg_differences
#' @template param-metadata_db_type
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_table
#' @template param-metadata_db_port
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-platform
#' @template param-arch
#' @template param-pkgs_to_build
#' @return Filtered package list
filter_packages_with_errors <- function(
  pkg_differences,
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_table,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode,
  platform,
  arch,
  pkgs_to_build
) {
  pkg_tag_pairs <- parse_package_tag_pairs(pkg_differences)

  tryCatch(
    {
      if (
        metadata_db_type != "postgres" ||
          !requireNamespace("RPostgres", quietly = TRUE)
      ) {
        log_warn(
          "Error checking is currently only supported for postgres databases."
        )
        return(pkg_differences)
      }

      con <- purrr::insistently(
        ~ DBI::dbConnect(
          RPostgres::Postgres(),
          dbname = metadata_db_name,
          host = metadata_db_host,
          port = metadata_db_port,
          user = metadata_db_user,
          password = metadata_db_password,
          sslmode = metadata_db_sslmode
        ),
        rate = retry_config,
        quiet = FALSE
      )()

      table_name <- DBI::dbQuoteIdentifier(con, metadata_db_table)
      packages_with_errors <- NULL

      for (i in seq_along(pkg_tag_pairs)) {
        pair <- pkg_tag_pairs[[i]]
        if (check_package_error(con, table_name, pair, platform, arch)) {
          packages_with_errors <- c(packages_with_errors, pkg_differences[i])
          log_warn(
            sprintf(
              "Skipping {.pkg %s} {.field %s} due to previous build error recorded in metadata DB.",
              pair$pkg,
              pair$tag
            )
          )
        }
      }

      DBI::dbDisconnect(con)

      if (length(packages_with_errors) > 0L) {
        pkg_differences <- setdiff(pkg_differences, packages_with_errors)
        log_info(sprintf(
          "Filtered out %d/%d package(s) due to previous errors. %d package(s) remaining to build.",
          length(packages_with_errors),
          length(pkgs_to_build),
          length(pkg_differences)
        ))
      }
    },
    error = function(e) {
      log_warn(sprintf(
        "Could not check metadata DB for previous errors: %s",
        e$message
      ))
    }
  )

  pkg_differences
}

#' Execute package builds with parallel processing
#'
#' Handles the actual building of packages using either sequential
#' or parallel processing with proper error handling.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-binary_output_path
#' @template param-source_org_url
#' @template param-local_clone_dir
#' @template param-platform
#' @template param-arch
#' @template param-codename
#' @template param-is_r_minor_sensitive
#' @template param-force
#' @template param-install_system_dependencies
#' @template param-store_build_metadata
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_password
#' @template param-metadata_db_user
#' @template param-metadata_db_sslmode
#' @template param-s3_endpoint
#' @template param-s3_bucket
#' @template param-s3_region
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-local_bin_path
#' @return Build results
execute_package_builds <- function(
  package_name,
  tag,
  binary_output_path,
  source_org_url,
  local_clone_dir,
  platform,
  arch,
  codename,
  is_r_minor_sensitive,
  force,
  install_system_dependencies,
  store_build_metadata,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_table,
  metadata_db_password,
  metadata_db_user,
  metadata_db_sslmode,
  s3_endpoint,
  s3_bucket,
  s3_region,
  s3_access_key_id,
  s3_secret_access_key,
  local_bin_path
) {
  t1 <- Sys.time()
  cli::cli_h2(sprintf("Building ({.pkg %s})", package_name[1L]))

  log_info(
    sprintf(
      "[%s] Building binaries for %s with tags %s.",
      format(Sys.time(), format = "%H:%M:%S"),
      package_name[1L],
      toString(tag)
    )
  )

  worker_function <- function(x, y, debug_flag) {
    tryCatch(
      {
        result <- build_single_tag(
          package_name = x,
          tag = y,
          binary_output_path = binary_output_path,
          local_clone_dir = local_clone_dir,
          source_org_url = source_org_url,
          codename = codename,
          is_r_minor_sensitive = is_r_minor_sensitive,
          platform = platform,
          arch = arch,
          force = force,
          install_system_dependencies = install_system_dependencies,
          store_build_metadata = store_build_metadata,
          metadata_db_host = metadata_db_host,
          metadata_db_name = metadata_db_name,
          metadata_db_port = metadata_db_port,
          metadata_db_table = metadata_db_table,
          metadata_db_password = metadata_db_password,
          metadata_db_user = metadata_db_user,
          metadata_db_sslmode = metadata_db_sslmode,
          s3_endpoint = s3_endpoint,
          s3_bucket = s3_bucket,
          s3_region = s3_region,
          s3_access_key_id = s3_access_key_id,
          s3_secret_access_key = s3_secret_access_key
        )

        tarball_name <- sprintf("%s_%s.tar.gz", x, y)
        if (file.exists(file.path(local_bin_path, tarball_name))) {
          log_success(
            sprintf("Finished processing package %s with tag %s.", x, y)
          )
        } else if (result != "skipped") {
          log_warn(
            sprintf(
              "Error in building package %s with tag %s: Uncommon/unspecific error during build.",
              x,
              y
            )
          )
          store_build_metadata(
            x,
            y,
            platform,
            error_occurred = TRUE,
            force = TRUE,
            arch = arch,
            error = "Unspecific error during build",
            metadata_db_host = metadata_db_host,
            metadata_db_name = metadata_db_name,
            metadata_db_port = metadata_db_port,
            metadata_db_table = metadata_db_table,
            metadata_db_password = metadata_db_password,
            metadata_db_user = metadata_db_user,
            metadata_db_sslmode = metadata_db_sslmode
          )
        }
      },
      error = function(e) {
        log_warn(
          sprintf(
            "Error in building package %s with tag %s: %s",
            x,
            y,
            conditionMessage(e)
          )
        )
        local_clone_dir_single <- file.path(
          local_clone_dir,
          sprintf("%s_%s", x, y)
        )
        unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)
        store_build_metadata(
          x,
          y,
          platform,
          error_occurred = TRUE,
          arch = arch,
          force = TRUE,
          error = conditionMessage(e),
          metadata_db_host = metadata_db_host,
          metadata_db_name = metadata_db_name,
          metadata_db_port = metadata_db_port,
          metadata_db_table = metadata_db_table,
          metadata_db_password = metadata_db_password,
          metadata_db_user = metadata_db_user,
          metadata_db_sslmode = metadata_db_sslmode
        )
      }
    )
    result
  }

  # Use future_mapply for parallel plans, regular mapply for sequential
  if (future::nbrOfWorkers() <= 1L) {
    result <- mapply(worker_function, package_name, tag)
  } else {
    result <- future.apply::future_mapply(
      worker_function,
      package_name,
      tag,
      future.seed = TRUE
    )
  }

  total_build_time <- round(Sys.time() - t1, 2L)
  log_info(
    sprintf("Execution time (%s): %s.", package_name[1L], total_build_time)
  )

  result
}

#' Determine which packages need to be built
#'
#' Checks S3 for existing packages and determines which ones
#' need to be built based on force flag and existing binaries.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-source_org_url
#' @template param-tag_limit
#' @template param-codename
#' @template param-force
#' @template param-is_r_minor_sensitive
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-binary_output_path
#' @template param-store_build_metadata
#' @template param-metadata_db_type
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_table
#' @template param-metadata_db_port
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-platform
#' @template param-arch
#' @return List with package information
determine_packages_to_build <- function(
  package_name,
  tag,
  source_org_url,
  tag_limit,
  codename,
  force,
  is_r_minor_sensitive,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region,
  binary_output_path,
  store_build_metadata,
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_table,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode,
  platform,
  arch
) {
  # check whether any build attempts need to be made
  if (!force && !is.null(s3_bucket)) {
    s3_result <- check_s3_packages(
      package_name,
      tag,
      source_org_url,
      tag_limit,
      is_r_minor_sensitive,
      s3_bucket,
      s3_access_key_id,
      s3_secret_access_key,
      s3_endpoint,
      s3_region,
      store_build_metadata,
      metadata_db_type,
      metadata_db_host,
      metadata_db_name,
      metadata_db_table,
      metadata_db_port,
      metadata_db_user,
      metadata_db_password,
      metadata_db_sslmode,
      platform,
      arch,
      codename
    )

    if (s3_result$should_skip) {
      return(list(should_skip = TRUE))
    }

    if (!is.null(s3_result$filtered_tags)) {
      tag <- s3_result$filtered_tags
      if (length(tag) == 0L) {
        # If no tags remain after filtering, skip this package
        return(list(should_skip = TRUE))
      }
      package_name <- rep(package_name, length(tag))
    }
  }

  list(
    package_name = package_name,
    tag = tag,
    should_skip = FALSE
  )
}

#' Check S3 for existing packages
#'
#' Verifies which packages already exist in S3 storage and determines
#' which ones need to be built, including error filtering.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-source_org_url
#' @template param-tag_limit
#' @template param-is_r_minor_sensitive
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-store_build_metadata
#' @template param-metadata_db_type
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_table
#' @template param-metadata_db_port
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-platform
#' @template param-arch
#' @template param-codename
#' @return List with should_skip and filtered_tags
check_s3_packages <- function(
  package_name,
  tag,
  source_org_url,
  tag_limit,
  is_r_minor_sensitive,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region,
  store_build_metadata,
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_table,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode,
  platform,
  arch,
  codename = NULL
) {
  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)

  # sometimes the var arrives as a vector > 1L here
  package_name <- unique(package_name)

  # get last CRAN version to search for it in S3 root
  last_version <- strsplit(
    purrr::insistently(
      ~ gh::gh(sprintf(
        "GET %s",
        paste("/repos", "cran", package_name, "commits", sep = "/")
      )),
      rate = retry_config,
      quiet = FALSE
    )()[[
      1L
    ]]$commit$message,
    "version ",
    fixed = TRUE
  )[[1L]][2L]

  # Check if root package exists
  root_pkg <- check_s3_root_package(
    remote_bin_path,
    package_name,
    last_version,
    is_r_minor_sensitive,
    s3_access_key_id,
    s3_secret_access_key,
    s3_endpoint,
    s3_region
  )
  if (!root_pkg) {
    return(list(should_skip = FALSE))
  }

  # Get all packages (root + archived) for comparison
  all_pkgs_s3 <- get_all_s3_packages(
    remote_bin_path,
    package_name,
    last_version,
    is_r_minor_sensitive,
    s3_access_key_id,
    s3_secret_access_key,
    s3_endpoint,
    s3_region
  )

  tags_filtered <- process_tag_filtering(
    tag,
    package_name,
    source_org_url,
    tag_limit
  )

  pkgs_to_build <- sprintf("%s_%s.tar.gz", package_name, tags_filtered)

  if (all(pkgs_to_build %in% all_pkgs_s3)) {
    log_info(
      "{.fun build_binary_package}: All packages to be built already exist in the remote bucket. ",
      "Skipping due to {.code force = FALSE}."
    )
    return(list(should_skip = TRUE))
  }

  pkg_differences <- setdiff(pkgs_to_build, all_pkgs_s3)

  # Calculate values for message
  filtered_count <- length(pkgs_to_build) - length(pkg_differences)
  total_count <- length(pkgs_to_build)
  remaining_count <- length(pkg_differences)

  log_info(
    sprintf(
      "Filtered out %d/%d package(s) as they already exist in the remote bucket. %d package(s) potentially remaining to build.",
      filtered_count,
      total_count,
      remaining_count
    )
  )

  # check for possible errors in the metadata DB
  if (store_build_metadata) {
    pkg_differences <- filter_packages_with_errors(
      pkg_differences,
      metadata_db_type,
      metadata_db_host,
      metadata_db_name,
      metadata_db_table,
      metadata_db_port,
      metadata_db_user,
      metadata_db_password,
      metadata_db_sslmode,
      platform,
      arch,
      pkgs_to_build
    )
  }

  if (length(pkg_differences) == 0L) {
    log_info(
      "{.fun build_binary_package}: All packages were filtered out due to previous build errors being present in the metadata database. Skipping."
    )
    return(list(should_skip = TRUE))
  }

  filtered_tags <- vapply(
    pkg_differences,
    function(x) {
      parts <- strsplit(x, "_", fixed = TRUE)[[1L]]
      if (length(parts) < 2L) {
        return(NA_character_)
      }
      version_part <- parts[2L]
      strsplit(version_part, ".tar.gz", fixed = TRUE)[[1L]][1L]
    },
    character(1L)
  )

  # Calculate values for message
  building_count <- length(pkg_differences)
  total_pkg_count <- length(pkgs_to_build)

  log_info(
    sprintf(
      "Building %d/%d versions as they are not present in the remote bucket: %s",
      building_count,
      total_pkg_count,
      toString(pkg_differences)
    )
  )

  list(should_skip = FALSE, filtered_tags = filtered_tags)
}

#' Handle post-build actions
#'
#' Manages upload and archiving operations after package builds complete.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-result
#' @template param-codename
#' @template param-upload
#' @template param-archive
#' @template param-force
#' @template param-is_r_minor_sensitive
#' @template param-s3_endpoint
#' @template param-s3_bucket
#' @template param-s3_region
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @return Invisible NULL
handle_post_build_actions <- function(
  package_name,
  tag,
  result,
  codename,
  upload,
  archive,
  force,
  is_r_minor_sensitive,
  s3_endpoint,
  s3_bucket,
  s3_region,
  s3_access_key_id,
  s3_secret_access_key
) {
  if (upload && any(result != "skipped")) {
    Map(
      function(x, y) {
        tryCatch(
          {
            upload_single_binary(
              package_name = x,
              tag = y,
              force = force,
              codename = codename,
              is_r_minor_sensitive = is_r_minor_sensitive,
              s3_endpoint = s3_endpoint,
              s3_bucket = s3_bucket,
              s3_region = s3_region,
              s3_access_key_id = s3_access_key_id,
              s3_secret_access_key = s3_secret_access_key
            )
          },
          error = function(e) {
            message(sprintf(
              "Error in uploading package %s with tag %s: %s",
              x,
              y,
              e
            ))
          }
        )
      },
      package_name,
      tag
    )

    if (
      !check_for_binary(
        package_name[1L],
        codename = codename,
        is_r_minor_sensitive = is_r_minor_sensitive,
        s3_endpoint = s3_endpoint,
        s3_bucket = s3_bucket,
        s3_region = s3_region,
        s3_access_key_id = s3_access_key_id,
        s3_secret_access_key = s3_secret_access_key
      )
    ) {
      upload_source_tarball(
        package_name[1L],
        codename = codename,
        is_r_minor_sensitive = is_r_minor_sensitive,
        s3_endpoint = s3_endpoint,
        s3_bucket = s3_bucket,
        s3_region = s3_region,
        s3_access_key_id = s3_access_key_id,
        s3_secret_access_key = s3_secret_access_key
      )
    }
  }

  if (archive && any(result != "skipped")) {
    archive_package(
      package_name[1L],
      codename = codename,
      is_r_minor_sensitive = is_r_minor_sensitive,
      s3_endpoint = s3_endpoint,
      s3_bucket = s3_bucket,
      s3_region = s3_region,
      s3_access_key_id = s3_access_key_id,
      s3_secret_access_key = s3_secret_access_key
    )
  }
}

#' Build binary for a single tag
#' @template param-package_name
#' @template param-tag
#' @template param-arch
#' @template param-platform
#' @template param-source_org_url
#' @template param-local_clone_dir
#' @template param-install_system_dependencies
#' @template param-is_r_minor_sensitive
#' @template param-force
#' @template param-codename
#' @template param-binary_output_path
#' @template param-store_build_metadata
#' @template param-metadata_db_host
#' @template param-metadata_db_type
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_user
#' @template param-metadata_db_password
#' @template param-metadata_db_sslmode
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom cli cli_alert
#' @importFrom pkgbuild build
#' @export
build_single_tag <- function(
  package_name,
  platform,
  arch,
  binary_output_path,
  local_clone_dir,
  source_org_url,
  tag = NULL,
  codename = NULL,
  is_r_minor_sensitive = FALSE,
  s3_endpoint = NULL,
  s3_region = NULL,
  s3_bucket = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  force = FALSE,
  install_system_dependencies = TRUE,
  store_build_metadata = FALSE,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL
) {
  log_debug(
    sprintf(
      "{.fun build_single_tag}: Cloning package {.pkg %s} with tag {.field %s}.",
      package_name,
      tag
    )
  )

  local_clone_dir_single <- file.path(
    local_clone_dir,
    sprintf("%s_%s", package_name, tag)
  )

  # Clean up any existing clone directory before starting
  if (dir.exists(local_clone_dir_single)) {
    log_debug(
      sprintf(
        "{.fun build_single_tag}: Removing existing clone directory {.path %s}.",
        local_clone_dir_single
      )
    )
    unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)
  }

  # Check if build should be skipped
  skip_result <- check_build_skip_conditions(
    package_name,
    tag,
    binary_output_path,
    codename,
    s3_bucket,
    s3_access_key_id,
    s3_secret_access_key,
    s3_endpoint,
    s3_region,
    force
  )

  if (skip_result$should_skip) {
    return("skipped")
  }

  clone_repository(package_name, tag, source_org_url, local_clone_dir_single)

  if (install_system_dependencies) {
    install_deps_result <- handle_system_dependencies(
      package_name,
      tag,
      platform,
      local_clone_dir_single,
      arch,
      metadata_db_host,
      metadata_db_name,
      metadata_db_port,
      metadata_db_table,
      metadata_db_password,
      metadata_db_user,
      metadata_db_sslmode
    )
    if (!install_deps_result$success) {
      return("error")
    }
  }

  build_result <- execute_package_build(
    package_name,
    tag,
    local_clone_dir_single,
    binary_output_path,
    platform,
    arch,
    metadata_db_host,
    metadata_db_name,
    metadata_db_port,
    metadata_db_table,
    metadata_db_password,
    metadata_db_user,
    metadata_db_sslmode
  )

  if (!build_result$success) {
    return("error")
  }

  # Handle file naming and cleanup
  file_result <- handle_build_output_files(
    package_name,
    tag,
    binary_output_path,
    local_clone_dir_single
  )

  if (store_build_metadata && file_result$file_exists) {
    store_build_metadata(
      package_name,
      tag,
      platform,
      arch = arch,
      error_occurred = FALSE,
      force = force,
      build_duration = build_result$build_time,
      size = file_result$file_size,
      metadata_db_type = metadata_db_type,
      metadata_db_host = metadata_db_host,
      metadata_db_name = metadata_db_name,
      metadata_db_port = metadata_db_port,
      metadata_db_table = metadata_db_table,
      metadata_db_password = metadata_db_password,
      metadata_db_user = metadata_db_user,
      metadata_db_sslmode = metadata_db_sslmode
    )
  }

  invisible(TRUE)
}

#' Check if build should be skipped
#'
#' Evaluates conditions to determine if a package build should be skipped,
#' including existing files and S3 storage checks.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-binary_output_path
#' @template param-codename
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-force
#' @return List with should_skip and reason
check_build_skip_conditions <- function(
  package_name,
  tag,
  binary_output_path,
  codename,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region,
  force
) {
  if (
    file.exists(file.path(
      binary_output_path,
      sprintf("%s_%s.tar.gz", package_name, tag)
    ))
  ) {
    log_info(
      sprintf(
        "Tarball for package {.pkg %s} with tag {.field %s} already exists. Skipping build.",
        package_name,
        tag
      )
    )
    return(list(should_skip = TRUE, reason = "skipped"))
  }

  if (!is.null(s3_bucket)) {
    codename <- set_codename(codename)
    remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)
    tarball_name <- sprintf("%s_%s.tar.gz", package_name, tag)
    s3fs::s3_file_system(
      aws_access_key_id = s3_access_key_id,
      aws_secret_access_key = s3_secret_access_key,
      endpoint = s3_endpoint,
      region_name = s3_region,
      refresh = TRUE
    )

    if (
      !force &&
        (s3fs::s3_file_exists(sprintf(
          file.path("%s", "%s"),
          remote_bin_path,
          tarball_name
        )) ||
          s3fs::s3_file_exists(sprintf(
            file.path("%s", "Archive", "%s", "%s"),
            remote_bin_path,
            package_name,
            tarball_name
          )))
    ) {
      log_info(
        sprintf(
          "Package {.pkg %s} with tag {.field %s} already exists in S3 and {.code force = FALSE}. Skipping build.",
          package_name,
          tag
        )
      )
      list(should_skip = TRUE, reason = "skipped")
    }
  }

  list(should_skip = FALSE)
}

#' Clone package repository
#'
#' Clones a git repository for a specific package and tag.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-source_org_url
#' @template param-local_clone_dir_single
#' @return Invisible NULL
clone_repository <- function(
  package_name,
  tag,
  source_org_url,
  local_clone_dir_single
) {
  # Clean up any existing clone directory before cloning
  if (dir.exists(local_clone_dir_single)) {
    unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)
  }

  gert::git_config_global_set("advice.detachedHead", "false")

  system2(
    "git",
    args = c(
      "clone",
      "-q",
      sprintf("--branch=%s", tag),
      file.path(source_org_url, package_name),
      local_clone_dir_single
    )
  )
}

#' Handle system dependency installation
#'
#' Manages the installation of system dependencies for a package
#' with proper error handling and metadata storage.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-platform
#' @template param-local_clone_dir_single
#' @template param-arch
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_password
#' @template param-metadata_db_user
#' @template param-metadata_db_sslmode
#' @return List with success status
handle_system_dependencies <- function(
  package_name,
  tag,
  platform,
  local_clone_dir_single,
  arch,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_table,
  metadata_db_password,
  metadata_db_user,
  metadata_db_sslmode
) {
  log_header("Installing system dependencies")
  tryCatch(
    {
      install_pkg_sys_deps(
        package_name,
        tag,
        local_clone_dir_single,
        platform
      )
      return(list(success = TRUE))
    },
    error = function(e) {
      log_warn(
        sprintf(
          "Error in installing dependencies for package %s with tag %s: %s",
          package_name[1L],
          tag[1L],
          conditionMessage(e)
        )
      )
      store_build_metadata(
        package_name[1L],
        tag[1L],
        platform,
        arch = arch,
        error_occurred = TRUE,
        force = TRUE,
        error = conditionMessage(e),
        metadata_db_host = metadata_db_host,
        metadata_db_name = metadata_db_name,
        metadata_db_port = metadata_db_port,
        metadata_db_table = metadata_db_table,
        metadata_db_password = metadata_db_password,
        metadata_db_user = metadata_db_user,
        metadata_db_sslmode = metadata_db_sslmode
      )
      list(success = FALSE)
    }
  )
}

#' Execute the actual package build
#'
#' Runs the pkgbuild::build() command with proper error handling
#' and metadata storage for build results.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-local_clone_dir_single
#' @template param-binary_output_path
#' @template param-platform
#' @template param-arch
#' @template param-metadata_db_host
#' @template param-metadata_db_name
#' @template param-metadata_db_port
#' @template param-metadata_db_table
#' @template param-metadata_db_password
#' @template param-metadata_db_user
#' @template param-metadata_db_sslmode
#' @return List with success status and build time
execute_package_build <- function(
  package_name,
  tag,
  local_clone_dir_single,
  binary_output_path,
  platform,
  arch,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_table,
  metadata_db_password,
  metadata_db_user,
  metadata_db_sslmode
) {
  log_header(
    sprintf(
      "Building package {.pkg %s} with tag {.field %s}.",
      package_name,
      tag
    )
  )

  quiet <- TRUE # Always quiet since debug is handled by lgr
  t1 <- Sys.time()

  tryCatch(
    {
      log_debug(
        "'binary_output_path': %s",
        binary_output_path
      )
      pkgbuild::build(
        path = sprintf("%s", local_clone_dir_single),
        binary = TRUE,
        vignettes = FALSE,
        dest_path = binary_output_path,
        quiet = quiet
      )

      log_debug(sprintf(
        "Files in binary_output_path: %s",
        toString(list.files(binary_output_path))
      ))

      build_time <- round(
        as.numeric(difftime(Sys.time(), t1, units = "secs")),
        2L
      )
      return(list(success = TRUE, build_time = build_time))
    },
    error = function(e) {
      log_warn(
        sprintf(
          "Error in starting build command for package {.pkg %s} with tag {.field %s}: %s",
          package_name,
          tag,
          e
        )
      )
      unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)
      store_build_metadata(
        package_name,
        tag,
        platform,
        arch = arch,
        error_occurred = TRUE,
        force = TRUE,
        error = sprintf(
          "Error trying to initiate pkgbuild -
          likely a non-valid R package structure. Full error: %s",
          e
        ),
        metadata_db_host = metadata_db_host,
        metadata_db_name = metadata_db_name,
        metadata_db_port = metadata_db_port,
        metadata_db_table = metadata_db_table,
        metadata_db_password = metadata_db_password,
        metadata_db_user = metadata_db_user,
        metadata_db_sslmode = metadata_db_sslmode
      )
      list(success = FALSE)
    }
  )
}

#' Handle build output files and cleanup
#'
#' Manages the renaming of build output files, cleanup of temporary files,
#' and calculation of file sizes for metadata storage.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-binary_output_path
#' @template param-local_clone_dir_single
#' @return List with file existence and size information
handle_build_output_files <- function(
  package_name,
  tag,
  binary_output_path,
  local_clone_dir_single
) {
  system_info <- get_system_architecture_info(binary_output_path)

  final_tarball_path <- file.path(
    binary_output_path,
    sprintf("%s_%s.tar.gz", package_name, tag)
  )

  if (file.exists(final_tarball_path)) {
    log_warn(
      sprintf(
        "{.fun build_single_tag}: Binary %s_%s.tar.gz already exists. Skipping copy.",
        package_name,
        tag
      )
    )
  } else {
    move_and_rename_tarball(
      package_name,
      tag,
      binary_output_path,
      system_info
    )
  }

  # Clean up temporary files
  unlink(sprintf("%s/%s_%s_R*.tar.gz", binary_output_path, package_name, tag))

  log_info(
    sprintf(
      "{.fun build_single_tag}: Removing {.path %s}.",
      local_clone_dir_single
    )
  )
  unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)

  # Check if final file exists and get its size
  file_exists <- file.exists(final_tarball_path)
  file_size <- if (file_exists) {
    round(as.numeric(file.size(final_tarball_path)) / (1024L^2L), 2L)
  } else {
    NA
  }

  list(file_exists = file_exists, file_size = file_size)
}
