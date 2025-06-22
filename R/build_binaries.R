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
#' @template param-deps_verbose
#' @template param-is_debug
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
#' @param future_strategy future parallelization strategy
#' @param future_workers Parallel workers count
#'
#' @importFrom future future plan value
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
  deps_verbose = FALSE,
  is_debug = FALSE,
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
  future_strategy = "sequential",
  future_workers = 1L,
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
    is_debug,
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
    arch,
    is_debug
  )

  if (pkg_info$should_skip) {
    return("skipped")
  }

  package_name <- pkg_info$package_name
  tag <- pkg_info$tag

  # Build packages
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
    is_debug,
    force,
    install_system_dependencies,
    deps_verbose,
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
    future_strategy,
    future_workers,
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
    is_debug,
    s3_endpoint,
    s3_bucket,
    s3_region,
    s3_access_key_id,
    s3_secret_access_key
  )

  return(invisible(TRUE))
}

initialize_build_environment <- function(
  package_name,
  codename,
  platform,
  arch,
  is_debug,
  local_output_dir_root,
  force,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region
) {
  cli::cli_h2("Preparations ({.pkg {package_name}})")
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
      alpine322 = "alpine-322"
    )
  }

  if (is_debug) {
    cli::cli_alert_warning("DEBUG: codename {codename}.")
  }

  binary_output_path <- set_bin_path(local_output_dir_root, codename)
  local_bin_path <- set_bin_path(
    local_output_dir_root = local_output_dir_root,
    codename
  )

  # infer local architecture
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

  if (is_debug) {
    cli::cli_alert_warning("DEBUG: binary_output_path {binary_output_path}.")
  }

  dir_out_src <- file.path(local_output_dir_root, "src", "contrib", "Archive")
  if (is_debug) {
    cli::cli_alert(
      "{.fun build_binary_package}: Creating bin dir {.path {binary_output_path}}."
    )
    cli::cli_alert(
      "{.fun build_binary_package}: Creating src dir {.path {dir_out_src}}."
    )
  }
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

filter_tags <- function(package_name, tag, source_org_url, tag_limit) {
  gert::git_config_global_set("advice.detachedHead", "false")

  gert::git_clone(
    sprintf("%s/%s", source_org_url, package_name), # nolint
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
        cli::cli_alert(
          "{.fun filter_tags}: Filtered for the {tag_limit} most recent tags (out of {.field {length(all_tags)}} total)"
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
  pkg_tag_pairs <- lapply(pkg_differences, function(x) {
    parts <- strsplit(x, "_", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) {
      list(pkg = NA, tag = NA)
    }
    pkg_name <- parts[1L]
    version_part <- parts[2L]
    tag_val <- strsplit(version_part, ".tar.gz", fixed = TRUE)[[1L]][1L]
    list(pkg = pkg_name, tag = tag_val)
  })

  tryCatch(
    {
      if (
        metadata_db_type == "postgres" &&
          requireNamespace("RPostgres", quietly = TRUE)
      ) {
        con <- purrr::insistently(
          ~ DBI::dbConnect(
            RPostgres::Postgres(), # nolint
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
      } else {
        cli::cli_alert_warning(
          "Error checking is currently only supported for postgres databases."
        )
      }

      table_name <- DBI::dbQuoteIdentifier(con, metadata_db_table)
      packages_with_errors <- NULL

      for (i in seq_along(pkg_tag_pairs)) {
        pair <- pkg_tag_pairs[[i]]
        if (!is.na(pair$pkg) && !is.na(pair$tag)) {
          sql_query <- paste0(
            # nolint
            "SELECT error_occurred FROM ",
            table_name,
            " WHERE name = $1 AND tag = $2 AND platform = $3 AND arch = $4"
          )
          result <- purrr::insistently(
            ~ DBI::dbGetQuery(
              con,
              sql_query,
              params = list(pair$pkg, pair$tag, platform, arch)
            ),
            rate = retry_config,
            quiet = FALSE
          )()

          if (nrow(result) > 0L && any(result$error_occurred)) {
            packages_with_errors <- c(packages_with_errors, pkg_differences[i])
            cli::cli_alert_warning(
              "Skipping {.pkg {pair$pkg}} {.field {pair$tag}} due to previous build error recorded in metadata DB."
            )
          }
        }
      }

      DBI::dbDisconnect(con)

      if (length(packages_with_errors) > 0L) {
        pkg_differences <- setdiff(pkg_differences, packages_with_errors)
        cli::cli_alert_info(
          "Filtered out {length(packages_with_errors)}/{length(pkgs_to_build)} package(s) due to previous errors. {length(pkg_differences)} package(s) remaining to build." # nolint
        )
      }
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Could not check metadata DB for previous errors: {e$message}"
      )
    }
  )

  pkg_differences
}

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
  is_debug,
  force,
  install_system_dependencies,
  deps_verbose,
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
  future_strategy,
  future_workers,
  local_bin_path
) {
  t1 <- Sys.time()
  cli::cli_h2("Building ({.pkg {package_name[1]}})")

  cli::cli_alert(
    "[{format(Sys.time(), format='%H:%M:%S')}] Building binaries for {.pkg {package_name[1]}} with tags {.field {tag}}."
  ) # nolint

  future::plan(
    future_strategy,
    workers = future_workers,
    rscript_startup = quote(withr::with_options(crayon.enabled = TRUE))
  )

  worker_function <- {
    function(x, y, debug_flag) {
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
            is_debug = debug_flag,
            force = force,
            install_system_dependencies = install_system_dependencies,
            deps_verbose = deps_verbose,
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
            cli::cli_alert_success(
              "Finished processing package {.pkg {x}} with tag {.field {y}}."
            )
          } else if (result != "skipped") {
            cli::cli_alert_warning(
              "Error in building package {.pkg {x}} with tag {.field {y}}: Uncommon/unspecific error during build."
            ) # nolint
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
          cli::cli_alert_warning(
            "Error in building package {.pkg {x}} with tag {.field {y}}: {e}"
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
            error = e$stderr,
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
  }

  if (is_debug) {
    result <- Map(worker_function, package_name, tag, MoreArgs = list(is_debug))
  } else {
    result <- future.apply::future_mapply(
      worker_function,
      package_name,
      tag,
      future.seed = TRUE,
      MoreArgs = list(is_debug)
    )
  }

  total_build_time <- round(Sys.time() - t1, 2L) # nolint
  cli::cli_alert_info(
    "Execution time ({.pkg {package_name[1]}}): {.strong {total_build_time}}."
  )

  result
}

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
  arch,
  is_debug
) {
  pkgs_to_build_exists <- FALSE

  # check whether any build attempts need to be made
  if (!force && !is.null(s3_bucket)) {
    s3_result <- check_s3_packages(
      package_name,
      tag,
      source_org_url,
      tag_limit,
      codename,
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
      arch
    )

    if (s3_result$should_skip) {
      return(list(should_skip = TRUE))
    }

    if (!is.null(s3_result$filtered_tags)) {
      tag <- s3_result$filtered_tags
      package_name <- rep(package_name, length(tag))
      pkgs_to_build_exists <- TRUE
    }
  }

  list(
    package_name = package_name,
    tag = tag,
    should_skip = FALSE
  )
}

check_s3_packages <- function(
  package_name,
  tag,
  source_org_url,
  tag_limit,
  codename = NULL,
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
  arch
) {
  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  # sometimes the var arrives as a vector > 1L here
  package_name <- unique(package_name)

  # get last CRAN version to search for it in S3 root
  last_version <- strsplit(
    gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[
      1L
    ]]$commit$message, # nolint
    "version ",
    fixed = TRUE
  )[[1L]][2L]

  if (!is_r_minor_sensitive) {
    root_pkg <- s3fs::s3_file_exists(file.path(
      remote_bin_path,
      sprintf("%s_%s.tar.gz", package_name, last_version)
    ))
  } else {
    minor_version <- paste(
      R.version$major,
      strsplit(R.version$minor, "\\.")[[1]][1],
      sep = "."
    )
    root_pkg <- s3fs::s3_file_exists(file.path(
      remote_bin_path,
      sprintf("%s/%s_%s.tar.gz", minor_version, package_name, last_version)
    ))
  }

  if (!root_pkg) {
    return(list(should_skip = FALSE))
  }

  # list archived packages
  if (!is_r_minor_sensitive) {
    archived_pkgs <- basename(s3fs::s3_dir_ls(file.path(
      remote_bin_path,
      "Archive",
      package_name
    )))
  } else {
    minor_version <- paste(
      R.version$major,
      strsplit(R.version$minor, "\\.")[[1L]][1L],
      sep = "."
    )
    archived_pkgs <- basename(s3fs::s3_dir_ls(file.path(
      remote_bin_path,
      minor_version,
      "Archive",
      package_name
    )))
  }
  root_pkg_name <- sprintf("%s_%s.tar.gz", package_name, last_version)
  all_pkgs_s3 <- c(root_pkg_name, archived_pkgs)

  gert::git_config_global_set("advice.detachedHead", "false")

  # get all desired tags of package to compare with `all_pkgs_s3`, only if no tag is provided or set to special keyword "latest" # nolint
  if (length(tag) == 1L && (is.null(tag) || tag == "latest")) {
    tags_filtered <- filter_tags(
      package_name,
      tag = NULL,
      source_org_url,
      tag_limit
    )
  } else {
    tags_filtered <- tag
  }

  pkgs_to_build <- sprintf("%s_%s.tar.gz", package_name, tags_filtered)

  if (all(pkgs_to_build %in% all_pkgs_s3)) {
    cli::cli_alert_info(
      "{.fun build_binary_package}: All packages to be built already exist in the remote bucket. ",
      "Skipping due to {.code force = FALSE}."
    )
    return(list(should_skip = TRUE))
  }

  pkg_differences <- setdiff(pkgs_to_build, all_pkgs_s3)

  cli::cli_alert_info(
    "Filtered out {(length(pkgs_to_build) - length(pkg_differences))}/{length(pkgs_to_build)} package(s) as they already exist in the remote bucket. {length(pkg_differences)} package(s) potentially remaining to build." # nolint
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
    cli::cli_alert_info(
      "{.fun build_binary_package}: All packages were filtered out due to previous build errors being present in the metadata database. Skipping." # nolint
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

  cli::cli_alert(
    "Building {length(pkg_differences)}/{length(pkgs_to_build)} versions as they are not present in the remote bucket: {.field {pkg_differences}}" # nolint
  )

  list(should_skip = FALSE, filtered_tags = filtered_tags)
}

handle_post_build_actions <- function(
  package_name,
  tag,
  result,
  codename,
  upload,
  archive,
  force,
  is_r_minor_sensitive,
  is_debug,
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
              is_debug = is_debug,
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
      future::future(
        upload_source_tarball(
          package_name[1L],
          codename = codename,
          is_r_minor_sensitive = is_r_minor_sensitive,
          s3_endpoint = s3_endpoint,
          s3_bucket = s3_bucket,
          s3_region = s3_region,
          s3_access_key_id = s3_access_key_id,
          s3_secret_access_key = s3_secret_access_key
        ),
        seed = TRUE
      )
    }
  }

  if (archive && any(result != "skipped")) {
    archive_future <- future::future(
      archive_package(
        package_name[1L],
        codename = codename,
        is_r_minor_sensitive = is_r_minor_sensitive,
        is_debug = is_debug,
        s3_endpoint = s3_endpoint,
        s3_bucket = s3_bucket,
        s3_region = s3_region,
        s3_access_key_id = s3_access_key_id,
        s3_secret_access_key = s3_secret_access_key
      )
    )
    future::value(archive_future, seed = TRUE)
  }
}

#' Build binary for a single tag
#' @template param-package_name
#' @template param-tag
#' @template param-arch
#' @template param-platform
#' @template param-source_org_url
#' @template param-is_debug
#' @template param-local_clone_dir
#' @template param-install_system_dependencies
#' @template param-deps_verbose
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
  is_debug = FALSE,
  force = FALSE,
  install_system_dependencies = TRUE,
  deps_verbose = FALSE,
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
  cli::cli_par()
  cli::cli_end()
  cli::cli_rule("{package_name} {tag}")

  if (is_debug) {
    cli::cli_alert(
      "{.fun build_single_tag}: Cloning package {.pkg {package_name}} with tag {.field {tag}}."
    )
  }

  local_clone_dir_single <- file.path(
    local_clone_dir,
    sprintf("%s_%s", package_name, tag)
  )

  # Clean up any existing clone directory before starting
  if (dir.exists(local_clone_dir_single)) {
    if (is_debug) {
      cli::cli_alert(
        "{.fun build_single_tag}: Removing existing clone directory {.path {local_clone_dir_single}}."
      )
    }
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
    skip_result$reason
  }

  # Clone repository
  clone_repository(package_name, tag, source_org_url, local_clone_dir_single)

  # Install system dependencies
  if (install_system_dependencies) {
    install_deps_result <- handle_system_dependencies(
      package_name,
      tag,
      platform,
      local_clone_dir_single,
      deps_verbose,
      is_debug,
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

  # Build package
  build_result <- execute_package_build(
    package_name,
    tag,
    local_clone_dir_single,
    binary_output_path,
    is_debug,
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
    local_clone_dir_single,
    is_debug
  )

  # Store metadata if requested
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

  return(invisible(TRUE))
}

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
    cli::cli_alert_info(
      "Tarball for package {.pkg {package_name}} with tag {.field {tag}} already exists. Skipping build."
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
          "%s/%s",
          remote_bin_path,
          tarball_name
        )) ||
          s3fs::s3_file_exists(sprintf(
            "%s/Archive/%s/%s",
            remote_bin_path,
            package_name,
            tarball_name
          )))
    ) {
      # nolint
      cli::cli_alert_info(
        "Package {.pkg {package_name}} with tag {.field {tag}} already exists in S3 and {.code force = FALSE}. Skipping build."
      ) # nolint
      list(should_skip = TRUE, reason = "skipped")
    }
  }

  list(should_skip = FALSE)
}

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

  system2(
    "git",
    args = c(
      "clone",
      "-q",
      sprintf("--branch=%s", tag),
      sprintf("%s/%s", source_org_url, package_name),
      local_clone_dir_single # nolint
    )
  )
}

handle_system_dependencies <- function(
  package_name,
  tag,
  platform,
  local_clone_dir_single,
  deps_verbose,
  is_debug,
  arch,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_table,
  metadata_db_password,
  metadata_db_user,
  metadata_db_sslmode
) {
  cli::cli_h2("Installing system dependencies")
  tryCatch(
    {
      install_pkg_sys_deps(
        package_name,
        tag,
        local_clone_dir_single,
        platform,
        deps_verbose,
        is_debug
      )
      return(list(success = TRUE))
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Error in installing dependencies for package {.pkg {package_name[1]}} with tag {.field {tag[1]}}: {e}"
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

execute_package_build <- function(
  package_name,
  tag,
  local_clone_dir_single,
  binary_output_path,
  is_debug,
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
  cli::cli_alert(
    "Building package {.pkg {package_name}} with tag {.field {tag}}."
  )

  quiet <- !is_debug
  t1 <- Sys.time()

  tryCatch(
    {
      if (is_debug) {
        message(sprintf(
          "DEBUG1: Printing 'binary_output_path': %s",
          binary_output_path
        ))
      }
      pkgbuild::build(
        path = sprintf("%s", local_clone_dir_single),
        binary = TRUE,
        vignettes = FALSE,
        dest_path = binary_output_path,
        quiet = quiet
      )
      if (is_debug) {
        message(sprintf(
          "DEBUG: Listing dir 'binary_output_path': %s",
          binary_output_path
        ))
        print(list.files(binary_output_path))
      }

      build_time <- round(
        as.numeric(difftime(Sys.time(), t1, units = "secs")),
        2L
      )
      return(list(success = TRUE, build_time = build_time))
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Error in starting build command for package {.pkg {package_name}} with tag {.field {tag}}: {e}"
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

handle_build_output_files <- function(
  package_name,
  tag,
  binary_output_path,
  local_clone_dir_single,
  is_debug
) {
  # Determine system architecture and suffix
  system_info <- get_system_architecture_info(binary_output_path)

  final_tarball_path <- file.path(
    binary_output_path,
    sprintf("%s_%s.tar.gz", package_name, tag)
  )

  if (file.exists(final_tarball_path)) {
    cli::cli_alert_warning(
      '{.fun build_single_tag}: Binary {sprintf("%s_%s.tar.gz", package_name, tag)} already exists. Skipping copy.'
    ) # nolint
  } else {
    move_and_rename_tarball(
      package_name,
      tag,
      binary_output_path,
      system_info,
      is_debug
    )
  }

  # Clean up temporary files
  unlink(sprintf("%s/%s_%s_R*.tar.gz", binary_output_path, package_name, tag))

  if (is_debug) {
    cli::cli_alert(
      "{.fun build_single_tag}: Removing {.path {local_clone_dir_single}}."
    )
  }
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

get_system_architecture_info <- function(binary_output_path) {
  if (
    any(grepl(
      "alpine",
      system2("cat", args = "/etc/os-release", stdout = TRUE),
      fixed = TRUE
    ))
  ) {
    # nolint
    linux_suffix <- "musl"
  } else {
    linux_suffix <- "gnu"
  }

  local_arch <- Sys.info()[["machine"]]
  if (
    grepl("arm64", local_arch, fixed = TRUE) ||
      grepl("aarch64", local_arch, fixed = TRUE)
  ) {
    tarball_id <- "unknown"
    tarball_arch <- "aarch64"
  } else if (
    grepl("amd64", local_arch, fixed = TRUE) ||
      grepl("x86_64", local_arch, fixed = TRUE)
  ) {
    tarball_id <- "pc"
    tarball_arch <- "x86_64"
  }

  if (
    any(grepl(
      "-redhat-linux",
      list.files(binary_output_path, recursive = TRUE),
      fixed = TRUE
    ))
  ) {
    tarball_id <- "redhat"
  }

  list(
    linux_suffix = linux_suffix,
    tarball_id = tarball_id,
    tarball_arch = tarball_arch
  )
}

move_and_rename_tarball <- function(
  package_name,
  tag,
  binary_output_path,
  system_info,
  is_debug
) {
  source_filename <- sprintf(
    "%s_%s_R_%s-%s-linux-%s.tar.gz",
    package_name,
    tag,
    system_info$tarball_arch,
    system_info$tarball_id,
    system_info$linux_suffix
  )
  source_path <- file.path(binary_output_path, source_filename)
  dest_path <- file.path(
    binary_output_path,
    sprintf("%s_%s.tar.gz", package_name, tag)
  )

  if (is_debug) {
    cli::cli_alert_info(
      "{.fun build_single_tag}: DEBUG: Moving package from {.path {source_path}} to {.path {dest_path}}"
    )
  }

  if (file.exists(source_path)) {
    file.rename(source_path, dest_path)
  } else {
    cli::cli_alert_info(
      "{.fun build_single_tag}: File for package {.pkg {package_name}} {.field {tag}} at {.path {source_path}} does not exist - skipping."
    ) # nolint
    if (is_debug) {
      message(sprintf(
        "DEBUG: Listing dir 'binary_output_path': %s",
        binary_output_path
      ))
      message(list.files(binary_output_path))
    }
  }
}
