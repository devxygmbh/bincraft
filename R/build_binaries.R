#' Build R binary packages
#'
#' @description
#' Builds binary packages from a given URL for a specific architecture.
#' Git tags are used to determine all possible versions to build.
#'
#' System dependencies are automatically installed through [pak].
#'
#' The function also automatically archives older versions into an `Archive/` directory to keep only the most recent one in the repository root.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-arch
#' @template param-platform
#' @template param-local_output_dir_root
#' @template param-local_clone_dir
#' @template param-install_system_dependencies
#' @template param-deps_verbose
#' @template param-debug
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
#' @template param-url
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
#' @import progressr
#' @importFrom future plan
#' @importFrom future.apply future_mapply
#' @importFrom progressr with_progress progressor
#' @importFrom gert git_config_global_set git_clone
#' @importFrom pak local_install_dev_deps
#' @importFrom pkgbuild build
#'
#' @examples
#' \dontrun{
#' # build from cran
#' build_binary_package("brew", archive = FALSE)
#' build_binary_package("brew", url = "https://github.com/cran/brew", archive = FALSE)
#' }
#'
#' @export
build_binary_package <- function(
    package_name,
    tag = NULL,
    codename = NULL,
    url = NULL,
    local_output_dir_root = ".",
    local_clone_dir = "/tmp",
    platform = NULL,
    arch = NULL,
    install_system_dependencies = TRUE,
    deps_verbose = FALSE,
    debug = FALSE,
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
    future_workers = 1,
    s3_endpoint = NULL,
    s3_region = NULL,
    s3_bucket = NULL,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  cli::cli_h2("Preparations ({.pkg {package_name}})")
  codename <- set_codename(codename)

  # map the 'pak' platform names to the ones used in s3
  if (is.null(platform)) {
    platform <- switch(codename,
      "jammy" = "ubuntu-2204",
      "noble" = "ubuntu-2404",
      "rhel9" = "redhat-9",
      "rhel8" = "redhat-8",
      "alpine320" = "alpine-320",
      "alpine321" = "alpine-321"
    )
  }

  if (debug) {
    cli::cli_alert_warning("DEBUG: codename {codename}.")
  }

  binary_output_path <- set_bin_path(local_output_dir_root, codename)

  local_bin_path <- set_bin_path(local_output_dir_root = local_output_dir_root, codename)

  # infer local architecture
  local_arch <- Sys.info()[["machine"]]
  if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
    arch <- "arm64"
  } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
    arch <- "amd64"
  }

  if (debug) {
    cli::cli_alert_warning("DEBUG: binary_output_path {binary_output_path}.")
  }

  dir_out_src <- sprintf("%s/src/contrib/Archive", local_output_dir_root)
  if (debug) {
    cli::cli_alert("{.fun build_binary_package}: Creating bin dir {.path {binary_output_path}}.")
    cli::cli_alert("{.fun build_binary_package}: Creating src dir {.path {dir_out_src}}.")
  }
  dir.create(sprintf("%s/Archive", binary_output_path), sprintf("%s/Archive", dir_out_src),
    recursive = TRUE
  )

  # check whether any build attempts need to be made
  if (!force && !is.null(s3_bucket)) {
    codename <- set_codename(codename)
    remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)
    s3fs::s3_file_system(
      aws_access_key_id = s3_access_key_id,
      aws_secret_access_key = s3_secret_access_key,
      endpoint = s3_endpoint,
      region_name = s3_region,
      refresh = TRUE
    )
    # get last CRAN version to search for it in S3 root
    last_version <- strsplit(
      gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[
        1
      ]]$commit$message,
      "version "
    )[[1]][2]
    root_pkg <- s3fs::s3_file_exists(sprintf("%s/%s_%s.tar.gz", remote_bin_path, package_name, last_version))

    if (root_pkg) {
      # list archived packages
      archived_pkgs <- basename(s3fs::s3_dir_ls(sprintf("%s/Archive/%s", remote_bin_path, package_name)))
      root_pkg_name <- sprintf("%s_%s.tar.gz", package_name, last_version)
      pkgs_all <- c(root_pkg_name, archived_pkgs)

      gert::git_config_global_set("advice.detachedHead", "false")
      # get all pkgs to build
      if (is.null(tag) || tag == "latest") {
        if (is.null(url)) {
          url <- sprintf("https://github.com/cran/%s", package_name)
        }
        gert::git_clone(url,
          path = sprintf("%s/%s", tempdir(), "tmp1"),
          verbose = FALSE
        )
        # gert cannot sort by date (which is a problem for properly sorting tags like 1.0-10 and others)
        if (!is.null(tag) && tag == "latest") {
          tag <- system("git tag --sort=-creatordate | head -1", intern = TRUE)
        } else {
          # Retrieve all tags
          all_tags <- gert::git_tag_list(repo = sprintf("%s/%s", tempdir(), "tmp1"))
          # filter out tags that start with R- (= non-valid ones)
          all_tags <- all_tags[!grepl("R-", all_tags$name), ]

          unlink(sprintf("%s/%s", tempdir(), "tmp1"), force = TRUE, recursive = TRUE)
          tag <- all_tags$name
        }
        pkgs_to_build <- sprintf("%s_%s.tar.gz", package_name, tag)
        if (all(pkgs_to_build %in% pkgs_all)) {
          cli::cli_alert_info("{.fun build_binary_package}: All packages to be built already exist in the remote bucket. Skipping due to {.code force = FALSE}.")
          return("skipped")
        } else {
          diff <- setdiff(pkgs_to_build, pkgs_all)

          # check for possible errors in the metadata DB to avoid building versions which previously errored
          if (store_build_metadata) {
            # Extract package names and tags from diff for DB query
            pkg_tag_pairs <- lapply(diff, function(x) {
              parts <- strsplit(x, "_")[[1]]
              if (length(parts) < 2) {
                list(pkg = NA, tag = NA)
              }
              pkg_name <- parts[1]
              version_part <- parts[2]
              tag_val <- strsplit(version_part, ".tar.gz")[[1]][1]
              list(pkg = pkg_name, tag = tag_val)
            })

            # Connect to DB and check for previous errors
            tryCatch(
              {
                if (metadata_db_type == "postgres") {
                  driver <- RPostgres::Postgres()
                  con <- purrr::insistently(~
                    DBI::dbConnect(driver,
                      dbname = metadata_db_name, host = metadata_db_host,
                      port = metadata_db_port, user = metadata_db_user, password = metadata_db_password,
                      sslmode = metadata_db_sslmode
                    ), rate = retry_config, quiet = FALSE)()
                } else {
                  cli::cli_alert_warning("Error checking is currently only supported for postgres databases.")
                }

                table_name <- DBI::dbQuoteIdentifier(con, metadata_db_table)

                # Check each package/tag pair for previous errors
                packages_with_errors <- c()
                for (i in seq_along(pkg_tag_pairs)) {
                  pair <- pkg_tag_pairs[[i]]
                  if (!is.na(pair$pkg) && !is.na(pair$tag)) {
                    query <- paste0("SELECT error_occurred FROM ", table_name, " WHERE name = $1 AND tag = $2 AND platform = $3 AND arch = $4")
                    result <- purrr::insistently(~ DBI::dbGetQuery(con, query, params = list(pair$pkg, pair$tag, platform, arch)), rate = retry_config, quiet = FALSE)()

                    if (nrow(result) > 0 && any(result$error_occurred == TRUE)) {
                      packages_with_errors <- c(packages_with_errors, diff[i])
                      cli::cli_alert_warning("Skipping {.pkg {pair$pkg}} {.field {pair$tag}} due to previous build error recorded in metadata DB.")
                    }
                  }
                }

                DBI::dbDisconnect(con)

                # Remove packages with errors from diff
                if (length(packages_with_errors) > 0) {
                  diff <- setdiff(diff, packages_with_errors)
                  cli::cli_alert_info("Filtered out {length(packages_with_errors)} package(s) with previous errors. {length(diff)} package(s) remaining to build.")
                }
              },
              error = function(e) {
                cli::cli_alert_warning("Could not check metadata DB for previous errors: {e$message}")
              }
            )
          }

          # Check if all packages were filtered out due to previous errors
          if (length(diff) == 0) {
            cli::cli_alert_info("{.fun build_binary_package}: All packages were filtered out due to previous build errors. Skipping.")
            return("skipped")
          }

          tag <- sapply(diff, function(x) {
            parts <- strsplit(x, "_")[[1]]
            if (length(parts) < 2) {
              return(NA)
            }
            version_part <- parts[2]
            strsplit(version_part, ".tar.gz")[[1]][1]
          })



          cli::cli_alert("Building the following version(s) ({length(diff)}/{length(pkgs_to_build)}) as they are not present in the remote bucket: {.field {diff}}")
        }
      }
    }
  }

  cli::cli_h2("Installing system dependencies ({.pkg {package_name}})")

  if (!exists("pkgs_to_build")) {
    gert::git_config_global_set("advice.detachedHead", "false")

    if (is.null(tag) || tag == "latest") {
      if (is.null(url)) {
        url <- sprintf("https://github.com/cran/%s", package_name)
      }
      gert::git_clone(url,
        path = sprintf("%s/%s", tempdir(), "tmp1"),
        verbose = FALSE
      )
      # gert cannot sort by date (which is a problem for properly sorting tags like 1.0-10 and others)
      if (!is.null(tag) && tag == "latest") {
        tag <- system("git tag --sort=-creatordate | head -1", intern = TRUE)
      } else {
        # Retrieve all tags
        all_tags <- gert::git_tag_list(repo = sprintf("%s/%s", tempdir(), "tmp1"))
        # filter out tags that start with R- (= non-valid ones)
        all_tags <- all_tags[!grepl("R-", all_tags$name), ]

        unlink(sprintf("%s/%s", tempdir(), "tmp1"), force = TRUE, recursive = TRUE)
        tag <- all_tags$name
      }
      package_name <- rep(package_name, length(tag))
    }
  } else {
    package_name <- rep(package_name, length(tag))
  }

  t1 <- Sys.time()
  cli::cli_h2("Building ({.pkg {package_name[1]}})")

  cli::cli_alert("[{format(Sys.time(), format='%H:%M:%S')}] Building binaries for {.pkg {package_name[1]}} with tags {.field {tag}}.")

  future::plan(future_strategy,
    workers = future_workers,
    rscript_startup = quote(options(crayon.enabled = TRUE))
  )

  # 'cli' is slow -> https://github.com/HenrikBengtsson/progressr/issues/167
  if (debug) {
    progressr::handlers("debug")
  } else {
    progressr::handlers("progress")
  }
  p <- progressr::progressor(along = tag)

  worker_fun <- function(x, y, p, debug) {
    p(message = sprintf("Building '%s'", y))
    tryCatch(
      {
        result <- build_single_tag(x, y, binary_output_path, local_clone_dir,
          platform = platform, arch = arch, debug = debug, force = force,
          install_system_dependencies = install_system_dependencies,
          deps_verbose = deps_verbose, store_build_metadata = store_build_metadata,
          metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name,
          metadata_db_port = metadata_db_port, metadata_db_table = metadata_db_table,
          metadata_db_password = metadata_db_password, metadata_db_user = metadata_db_user,
          metadata_db_sslmode = metadata_db_sslmode,
          s3_endpoint = s3_endpoint, s3_bucket = s3_bucket, s3_region = s3_region, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key
        )

        p(message = sprintf("Done building '%s'", y))

        # if for some reason an underlying error didnt' get caught in the tryCatch calls, we check again here for the existence of the binary file on disk and mark the build as failed if it is not found
        tarball_name <- sprintf("%s_%s.tar.gz", x, y)
        if (fs::file_exists(sprintf("%s/%s", local_bin_path, tarball_name))) {
          cli::cli_alert_success("Finished processing package {.pkg {x}} with tag {.field {y}}.")
        } else if (result != "skipped") {
          cli::cli_alert_warning("Error in building package {.pkg {x}} with tag {.field {y}}: Uncommon/unspecific error during build.")
          store_build_metadata(x, y, platform,
            error_occurred = TRUE, force = TRUE, arch = arch, error = "Uncommon/unspecific error during build",
            metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name,
            metadata_db_port = metadata_db_port, metadata_db_table = metadata_db_table,
            metadata_db_password = metadata_db_password, metadata_db_user = metadata_db_user,
            metadata_db_sslmode = metadata_db_sslmode
          )
        }
      },
      error = function(e) {
        cli::cli_alert_warning("Error in building package {.pkg {package_name}} with tag {.field {tag}}: {e}")
        local_clone_dir_single <- sprintf("%s/%s_%s", local_clone_dir, x, y)
        unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)
        # only stderr contains the important information why the build failed
        store_build_metadata(x, y, platform,
          error_occurred = TRUE, arch = arch, force = TRUE, error = e$stderr, metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name,
          metadata_db_port = metadata_db_port, metadata_db_table = metadata_db_table,
          metadata_db_password = metadata_db_password, metadata_db_user = metadata_db_user,
          metadata_db_sslmode = metadata_db_sslmode
        )
      }
    )
    p(message = sprintf("Finished building %s %s", x, y))
    return(result)
  }

  if (debug) {
    result <- mapply(worker_fun, package_name, tag, MoreArgs = list(p, debug))
  } else {
    result <- future.apply::future_mapply(worker_fun, package_name, tag,
      future.seed = TRUE, MoreArgs = list(p, debug)
    )
  }

  total_build_time <- round(Sys.time() - t1, 2)
  cli::cli_alert_info("Execution time ({.pkg {package_name[1]}}) ({length(tag)} tag{?s}): {.strong {total_build_time} {units(difftime(Sys.time(), t1))}}.")

  if (upload && any(result != "skipped")) {
    # out <- progressr::with_progress({
    # p <- progressr::progressor(along = tag)
    # future.apply::future_mapply(function(x, y) {
    mapply(function(x, y) {
      tryCatch(
        {
          # p()
          dump <- upload_single_binary(package_name = x, tag = y, force = force, debug = debug, s3_endpoint = s3_endpoint, s3_bucket = s3_bucket, s3_region = s3_region, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key)
        },
        error = function(e) {
          message(sprintf("Error in uploading package %s with tag %s: %s", x, y, e))
        }
      )
      # }, package_name, tag, future.seed = TRUE)
    }, package_name, tag)

    # check if latest version has a binary. If not, upload the latest source tarball
    if (!check_for_binary(package_name[1], s3_endpoint = s3_endpoint, s3_bucket = s3_bucket, s3_region = s3_region, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key)) {
      upload_source_tarball(package_name[1], s3_endpoint = s3_endpoint, s3_bucket = s3_bucket, s3_region = s3_region, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key)
    }
  }

  if (archive && any(result != "skipped")) {
    archive_package(package_name[1], debug = debug, s3_endpoint = s3_endpoint, s3_bucket = s3_bucket, s3_region = s3_region, s3_access_key_id = s3_access_key_id, s3_secret_access_key = s3_secret_access_key)
  }

  return(invisible(TRUE))
}

#' Build binary for a single tag
#' @template param-package_name
#' @template param-tag
#' @template param-arch
#' @template param-platform
#' @template param-debug
#' @template param-local_clone_dir
#' @template param-install_system_dependencies
#' @template param-deps_verbose
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
#' @importFrom fs file_size file_move
#' @importFrom emoji emoji
#' @export
build_single_tag <- function(
    package_name,
    tag = NULL,
    platform,
    arch,
    binary_output_path,
    local_clone_dir,
    codename = NULL,
    s3_endpoint = NULL,
    s3_region = NULL,
    s3_bucket = NULL,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL,
    debug = FALSE,
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
    metadata_db_sslmode = NULL) {
  cli::cli_par()
  cli::cli_end()
  cli::cli_rule("{package_name} {tag}")

  if (debug) {
    cli::cli_alert("{.fun build_single_tag}: Cloning package {.pkg {package_name}} with tag {.field {tag}}.")
  }

  local_clone_dir_single <- sprintf("%s/%s_%s", local_clone_dir, package_name, tag)

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
  }

  if (file.exists(sprintf("%s/%s_%s.tar.gz", binary_output_path, package_name, tag))) {
    cli::cli_alert_info("Tarball for package {.pkg {package_name}} with tag {.field {tag}} already exists. Skipping build.")
    return("skipped")
  } else if (
    (!force && !is.null(s3_bucket) && (s3fs::s3_file_exists(sprintf("%s/%s", remote_bin_path, tarball_name))) ||
      s3fs::s3_file_exists(sprintf("%s/Archive/%s/%s", remote_bin_path, package_name, tarball_name)))
  ) {
    cli::cli_alert_info("Package {.pkg {package_name}} with tag {.field {tag}} already exists in S3 and {.code force = FALSE}. Skipping build.")
    return("skipped")
  }

  # Using system git here as {gert} does not provide this functionality to checkout a branch by tag
  system2("git", args = c(
    "clone", "-q", sprintf("--branch=%s", tag),
    sprintf("https://github.com/cran/%s", package_name), local_clone_dir_single
  ))

  ### Install system dependencies
  if (install_system_dependencies) {
    tryCatch(
      {
        install_package_system_dependencies(package_name, tag, platform, local_clone_dir_single, deps_verbose, debug)
      },
      # NB: here we need to use conditionMessage() to extract the actual error - as opposed to using $stderr for errors within the tryCatch used in the future* calls
      error = function(e) {
        cli::cli_alert_warning("Error in installing dependencies for package {.pkg {package_name[1]}} with tag {.field {tag[1]}}: {e}")
        store_build_metadata(package_name[1], tag[1], platform,
          arch = arch, error_occurred = TRUE, force = TRUE, error = conditionMessage(e),
          metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name,
          metadata_db_port = metadata_db_port, metadata_db_table = metadata_db_table,
          metadata_db_password = metadata_db_password, metadata_db_user = metadata_db_user,
          metadata_db_sslmode = metadata_db_sslmode
        )
        return(TRUE)
      }
    )
  }

  cli::cli_alert("{emoji('hammer')} Building package {.pkg {package_name}} with tag {.field {tag}}.")

  if (debug) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }
  t1 <- Sys.time()
  tryCatch(
    {
      if (debug) {
        message(sprintf("DEBUG1: Printing 'binary_output_path': %s", binary_output_path))
      }
      pkgbuild::build(
        path = sprintf("%s", local_clone_dir_single),
        binary = TRUE, vignettes = FALSE,
        dest_path = binary_output_path, quiet = quiet
      )
      if (debug) {
        message(sprintf("DEBUG: Listing dir 'binary_output_path': %s", binary_output_path))
        print(fs::dir_ls(binary_output_path))
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Error in starting build command for package {.pkg {package_name}} with tag {.field {tag}}: {e}")
      local_clone_dir_single <- sprintf("%s/%s_%s", local_clone_dir, package_name, tag)
      unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)
      store_build_metadata(package_name, tag, platform,
        arch = arch, error_occurred = TRUE, force = TRUE, error = sprintf("Error trying to initiate pkgbuild - likely a non-valid R package structure. Full error: %s", e), metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name,
        metadata_db_port = metadata_db_port, metadata_db_table = metadata_db_table,
        metadata_db_password = metadata_db_password, metadata_db_user = metadata_db_user,
        metadata_db_sslmode = metadata_db_sslmode
      )
      return(invisible(TRUE))
    }
  )

  if (any(grepl("alpine", system2("cat", args = c("/etc/os-release"), stdout = TRUE)))) {
    linux_suffix <- "musl"
  } else {
    linux_suffix <- "gnu"
  }

  # set tarball id for arch
  local_arch <- Sys.info()[["machine"]]
  if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
    tarball_id <- "unknown"
    tarball_arch <- "aarch64"
  } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
    tarball_id <- "pc"
    tarball_arch <- "x86_64"
  }
  # on some systems, the tarball_id is also sometimes 'redhat'
  if (any(grepl("-redhat-linux", fs::dir_ls(binary_output_path, recurse = TRUE)))) {
    tarball_id <- "redhat"
  }

  if (!file.exists(sprintf("%s/%s_%s.tar.gz", binary_output_path, package_name, tag))) {
    if (debug) {
      cli::cli_alert_info('{.fun build_single_tag}: DEBUG: Moving package from {.path {sprintf("%s/%s_%s_R_%s-%s-linux-%s.tar.gz", binary_output_path, package_name, tag, tarball_arch, tarball_id, linux_suffix)}} to {.path {sprintf("%s/%s_%s.tar.gz", binary_output_path, package_name, tag)}}')
    }
    # double-check that file exists (some packages like https://github.com/cran/BACCO/tree/1.0-14 don't include R/ and hence don't procude a valid binary)
    if (fs::file_exists(sprintf("%s/%s_%s_R_%s-%s-linux-%s.tar.gz", binary_output_path, package_name, tag, tarball_arch, tarball_id, linux_suffix))) {
      # remove _aarch64-unknown-linux-gnu/musl part in filename
      fs::file_move(
        sprintf("%s/%s_%s_R_%s-%s-linux-%s.tar.gz", binary_output_path, package_name, tag, tarball_arch, tarball_id, linux_suffix),
        sprintf("%s/%s_%s.tar.gz", binary_output_path, package_name, tag)
      )
    } else {
      cli::cli_alert_info('{.fun build_single_tag}: File for package {.pkg {package_name}} {.field {tag}} at {.path {sprintf("%s/%s_%s_R_%s-%s-linux-%s.tar.gz", binary_output_path, package_name, tag, tarball_arch, tarball_id, linux_suffix)}} does not exist - skipping.')
      if (debug) {
        message(sprintf("DEBUG: Listing dir 'binary_output_path': %s", binary_output_path))
        message(fs::dir_ls(binary_output_path))
      }
    }
  } else {
    cli::cli_alert_warning('{.fun build_single_tag}: Binary {sprintf("%s_%s.tar.gz", package_name, tag)} already exists. Skipping copy.')
  }
  unlink(sprintf("%s/%s_%s_R*.tar.gz", binary_output_path, package_name, tag))

  if (debug) {
    cli::cli_alert("{.fun build_single_tag}: Removing {.path {local_clone_dir_single}}.")
  }
  unlink(local_clone_dir_single, force = TRUE, recursive = TRUE)

  total_build_time <- round(as.numeric(difftime(Sys.time(), t1, units = "secs")), 2)

  # bytes to MB in binary format
  file_size <- round(as.numeric(fs::file_size(sprintf("%s/%s_%s.tar.gz", binary_output_path, package_name, tag))) / (1024^2), 2)

  if (debug) {
    cli::cli_alert_warning("DEBUG: total_build_time: {total_build_time}")
    cli::cli_alert_warning("DEBUG: file_size: {file_size}")
  }

  tarball_name <- sprintf("%s_%s.tar.gz", package_name, tag)

  if (store_build_metadata) {
    if (fs::file_exists(sprintf("%s/%s", binary_output_path, tarball_name))) {
      store_build_metadata(package_name, tag, platform,
        arch = arch, error_occurred = FALSE, force = force, build_duration = total_build_time, size = file_size,
        metadata_db_type = metadata_db_type,
        metadata_db_host = metadata_db_host, metadata_db_name = metadata_db_name,
        metadata_db_port = metadata_db_port, metadata_db_table = metadata_db_table,
        metadata_db_password = metadata_db_password, metadata_db_user = metadata_db_user,
        metadata_db_sslmode = metadata_db_sslmode
      )
    }
  }

  return(invisible(TRUE))
}
