#' Clean up stale pak cache lock files
#' @importFrom stats runif
#' @param cache_dir Cache directory path
#' @param max_age Maximum age of lock files in seconds (default: 300)
cleanup_stale_locks <- function(cache_dir = NULL, max_age = 300L) {
  if (is.null(cache_dir)) {
    # Try to find pak cache directory
    possible_paths <- c(
      "/mnt/cache/pkgcache/R/pkgcache/pkg",
      file.path(Sys.getenv("HOME"), ".cache", "R", "pkgcache", "pkg"),
      file.path(tempdir(), "pkgcache", "pkg")
    )

    for (path in possible_paths) {
      if (dir.exists(path)) {
        cache_dir <- path
        break
      }
    }
  }

  if (!is.null(cache_dir) && dir.exists(cache_dir)) {
    lock_files <- list.files(cache_dir, pattern = "\\.lock$", full.names = TRUE)

    for (lock_file in lock_files) {
      if (file.exists(lock_file)) {
        file_age <- as.numeric(difftime(
          Sys.time(),
          file.mtime(lock_file),
          units = "secs"
        ))
        if (file_age > max_age) {
          cli::cli_alert_info("Removing stale lock file: {basename(lock_file)}")
          unlink(lock_file, force = TRUE)
        }
      }
    }
  }
}

#' Acquire cache-aware mutex for pak operations
#' @param operation_type Type of operation: "install", "download", "cache_write"
#' @param timeout_seconds Maximum time to wait for lock (default: 120)
acquire_pak_mutex <- function(
  operation_type = "install",
  timeout_seconds = 120L
) {
  mutex_dir <- file.path(tempdir(), "pak_mutex")
  if (!dir.exists(mutex_dir)) {
    dir.create(mutex_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Use operation-specific locks to allow more concurrency
  mutex_file <- file.path(mutex_dir, paste0("pak_", operation_type, ".lock"))
  process_id <- Sys.getpid()
  current_time <- as.numeric(Sys.time())
  lock_content <- paste(process_id, current_time, sep = "|")

  start_time <- Sys.time()
  attempt <- 0L

  while (difftime(Sys.time(), start_time, units = "secs") < timeout_seconds) {
    attempt <- attempt + 1L

    # Clean up any stale locks first
    if (file.exists(mutex_file)) {
      tryCatch(
        {
          existing_content <- readLines(mutex_file, warn = FALSE)
          if (length(existing_content) > 0L) {
            parts <- strsplit(existing_content[1L], "\\|")[[1L]]
            if (length(parts) >= 2L) {
              lock_timestamp <- as.numeric(parts[2L])
              lock_age <- as.numeric(Sys.time()) - lock_timestamp

              # Remove locks older than 5 minutes
              if (lock_age > 300L) {
                unlink(mutex_file, force = TRUE)
                cli::cli_alert_info(
                  "Removed stale {operation_type} lock (age: {round(lock_age/60, 1)} min)"
                )
              }
            }
          }
        },
        error = function(e) {
          # If we can't read the lock file, assume it's corrupted and remove it
          unlink(mutex_file, force = TRUE)
        }
      )
    }

    # Try to acquire the lock
    if (!file.exists(mutex_file)) {
      tryCatch(
        {
          writeLines(lock_content, mutex_file)
          # Verify we got the lock
          Sys.sleep(0.01) # Brief pause to ensure file is written
          if (file.exists(mutex_file)) {
            verify_content <- readLines(mutex_file, warn = FALSE)
            if (
              length(verify_content) > 0L && verify_content[1L] == lock_content
            ) {
              return(mutex_file)
            }
          }
        },
        error = function(e) {
          # Lock acquisition failed, continue
        }
      )
    }

    # Adaptive wait time: shorter for first few attempts, longer for later ones
    wait_time <- min(0.1 * (1.5^(attempt - 1L)), 2.0) + runif(1L, 0L, 0.5)
    Sys.sleep(wait_time)
  }

  stop(
    sprintf("Failed to acquire %s mutex within timeout period", operation_type),
    call. = FALSE
  )
}

#' Release process-level mutex for pak operations
#' @param mutex_file Path to mutex file to release
release_pak_mutex <- function(mutex_file) {
  if (!is.null(mutex_file) && file.exists(mutex_file)) {
    unlink(mutex_file, force = TRUE)
  }
}


#' Retry a function with exponential backoff
#' @param func Function to retry
#' @param max_attempts Maximum number of attempts (default: 5)
#' @param base_delay Base delay in seconds (default: 1)
#' @param max_delay Maximum delay in seconds (default: 60)
retry_with_backoff <- function(
  func,
  max_attempts = 5L,
  base_delay = 1L,
  max_delay = 60L
) {
  for (attempt in seq_len(max_attempts)) {
    tryCatch(
      {
        return(func())
      },
      error = function(e) {
        # Classify the error type
        error_msg <- conditionMessage(e)

        is_dependency_error <- grepl(
          "Could not solve package dependencies|Can't install dependency|dependency.*not available|version.*not available|No suitable version|incompatible.*version", # nolint
          error_msg,
          ignore.case = TRUE
        )

        is_locking_error <- grepl(
          "Cannot lock file|I/O error|filelock.*lock|database.*locked|cache.*lock|timeout.*lock",
          error_msg,
          ignore.case = TRUE
        )

        is_network_error <- grepl(
          "download.*failed|network.*error|connection.*error|timeout.*download|curl.*error",
          error_msg,
          ignore.case = TRUE
        )

        is_subprocess_error <- grepl(
          "pak subprocess.*error|subprocess.*failed",
          error_msg,
          ignore.case = TRUE
        ) &&
          !is_dependency_error

        # Only retry for locking, network, or subprocess errors (not dependency errors)
        should_retry <- (is_locking_error ||
          is_network_error ||
          is_subprocess_error) &&
          !is_dependency_error

        if (should_retry) {
          if (attempt < max_attempts) {
            delay <- min(base_delay * (2L^(attempt - 1L)), max_delay)
            error_type_msg <- if (is_locking_error) {
              "locking error"
            } else if (is_network_error) {
              "network error"
            } else {
              "subprocess error"
            }
            cli::cli_alert_warning(
              "Attempt {attempt}/{max_attempts} failed with {error_type_msg}. Retrying in {delay} seconds..."
            )
            Sys.sleep(delay)
          } else {
            cli::cli_alert_danger(
              "All {max_attempts} attempts failed. Giving up."
            )
            stop(e, call. = FALSE)
          }
        } else {
          # For dependency resolution errors and other non-retryable errors, fail immediately
          if (is_dependency_error) {
            cli::cli_alert_danger(
              "Dependency resolution error (not retryable): {error_msg}"
            )
          }
          stop(e, call. = FALSE)
        }
      }
    )
  }
}


#' Install system dependencies for an R package
#'
#' This function uses a shared cache approach with safe concurrency controls:
#' - Preserves shared pak cache to avoid redundant downloads/builds
#' - Uses operation-specific mutexes to prevent cache corruption
#' - Implements retry logic with exponential backoff for transient failures
#' - Cleans up stale lock files automatically
#'
#' The shared cache approach is preferred over isolation because:
#' - Avoids redundant package downloads across builds
#' - Reduces storage requirements and build times
#' - Maintains cache benefits while ensuring thread safety
#'
#' @template param-package_name
#' @template param-tag
#' @template param-platform
#' @template param-deps_verbose
#' @template param-local_clone_dir
#' @template param-is_debug
#' @param aggressive_cleanup Perform additional cache cleanup before installation
#'
#' @export
install_pkg_sys_deps <- function(
  package_name,
  tag,
  local_clone_dir,
  platform = platform,
  deps_verbose = FALSE,
  is_debug = FALSE,
  aggressive_cleanup = FALSE
) {
  if (is_debug) {
    cli::cli_alert(
      "Cloning package {.pkg {package_name[1L]}} with tag {.field {tail(tag, 1L)}}."
    )
  }

  t1 <- Sys.time()

  local_clone_dir_single <- file.path(
    local_clone_dir,
    paste0(package_name[1L], "_", tail(tag, 1L))
  )

  if (!dir.exists(local_clone_dir_single)) {
    system2(
      "git",
      args = c(
        "clone",
        "-q",
        sprintf("--branch=%s", tail(tag, 1L)),
        sprintf("https://github.com/cran/%s", package_name[1L]),
        local_clone_dir_single
      )
    )
  }

  env_vars <- list(
    PKG_SYSREQS = TRUE,
    PKG_SYSREQS_VERBOSE = TRUE
  )

  if (grepl("alpine", platform, fixed = TRUE)) {
    platform <- "alpine"
    env_vars$PKG_SYSREQS_PLATFORM <- "alpine"
  }

  if (grepl("rhel-9", pak::system_r_platform(), fixed = TRUE)) {
    env_vars$PKG_SYSREQS_PLATFORM <- "redhat-9"
    env_vars$CURL_CA_BUNDLE <- "/etc/pki/tls/certs/ca-bundle.crt" # nolint
  } else if (grepl("rhel-8", pak::system_r_platform(), fixed = TRUE)) {
    env_vars$PKG_SYSREQS_PLATFORM <- "redhat-8"
    env_vars$CURL_CA_BUNDLE <- "/etc/pki/tls/certs/ca-bundle.crt" # nolint
  }

  cli::cli_alert("Installing R package dependencies")

  # Clean up any stale lock files before attempting installation
  cleanup_stale_locks()

  # Perform additional cleanup if requested
  if (aggressive_cleanup) {
    cli::cli_alert_info("Performing aggressive pak cache cleanup...")

    # Find and clean additional pak cache locations
    additional_cache_paths <- c(
      "/mnt/cache/pkgcache",
      file.path(Sys.getenv("HOME"), ".cache", "R", "pkgcache"),
      file.path(tempdir(), "pkgcache")
    )

    for (cache_path in additional_cache_paths) {
      if (dir.exists(cache_path)) {
        # Remove any lock files older than 1 minute
        lock_files <- list.files(
          cache_path,
          pattern = "\\.lock$",
          recursive = TRUE,
          full.names = TRUE
        )

        for (lock_file in lock_files) {
          if (file.exists(lock_file)) {
            file_age <- as.numeric(difftime(
              Sys.time(),
              file.mtime(lock_file),
              units = "secs"
            ))
            if (file_age > 60L) {
              unlink(lock_file, force = TRUE)
            }
          }
        }
      }
    }
  }

  # Acquire mutex to prevent concurrent pak cache operations
  mutex_file <- NULL
  tryCatch(
    {
      if (is_debug) {
        cli::cli_alert_info("Acquiring pak install mutex...")
      }
      mutex_file <- acquire_pak_mutex("install", timeout_seconds = 180)
      if (is_debug) {
        cli::cli_alert_success("Pak install mutex acquired")
      }

      tryCatch(
        {
          retry_with_backoff(function() {
            withr::with_envvar(env_vars, {
              if (deps_verbose) {
                pak::local_install_deps(sprintf("%s", local_clone_dir_single))
              } else {
                suppressMessages(pak::local_install_deps(sprintf(
                  "%s",
                  local_clone_dir_single
                )))
              }
            })
          })
        },
        error = function(e) {
          # Provide helpful error context
          error_msg <- conditionMessage(e)
          if (
            grepl(
              "Could not solve package dependencies|Can't install dependency",
              error_msg,
              ignore.case = TRUE
            )
          ) {
            cli::cli_alert_info(
              "This appears to be a dependency version conflict. Check if the package version is compatible with current R package versions." # nolint
            )
          }
          stop(e, call. = FALSE)
        }
      )
    },
    finally = {
      # Always release the mutex
      if (!is.null(mutex_file)) {
        release_pak_mutex(mutex_file)
        if (is_debug) {
          cli::cli_alert_info("Pak install mutex released")
        }
      }
    }
  )

  if (is_debug) {
    cli::cli_alert(
      "Removing temporary clone dir at {.path {local_clone_dir_single}}."
    )
  }

  total_build_time <- round(Sys.time() - t1, 2L) # nolint
  cli::cli_alert(
    "R package dependencies installation time ({.pkg {package_name[[1L]]}}): {.strong {total_build_time} {units(difftime(Sys.time(), t1))}}."
  ) # nolint

  unlink(sprintf("%s", local_clone_dir_single), recursive = TRUE, force = TRUE)
}
