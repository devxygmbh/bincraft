#' Clean up stale pak cache lock files
#'
#' Removes lock files that are older than the specified age to prevent
#' stale locks from blocking package installation operations.
#'
#' @template param-cache_dir
#' @template param-max_age
#' @return Invisible NULL
#' @importFrom stats runif
cleanup_stale_locks <- function(cache_dir = NULL, max_age = 300L) {
  if (is.null(cache_dir)) {
    # Try to find pak cache directory
    possible_paths <- c(
      file.path("/mnt", "cache", "pkgcache", "R", "pkgcache", "pkg"),
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
          log_info(sprintf("Removing stale lock file: %s", basename(lock_file)))
          unlink(lock_file, force = TRUE)
        }
      }
    }
  }
}

#' Acquire cache-aware mutex for pak operations
#'
#' Creates a process-level mutex to prevent concurrent pak operations that could
#' cause cache corruption. Uses operation-specific locks for better concurrency.
#'
#' @template param-operation_type
#' @template param-timeout_seconds
#' @return Path to mutex file if acquired successfully
#' @details Automatically cleans up stale locks older than 5 minutes
acquire_pak_mutex <- function(
    operation_type = "install",
    timeout_seconds = 120L) {
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
            parts <- strsplit(existing_content[1L], "|", fixed = TRUE)[[1L]]
            if (length(parts) >= 2L) {
              lock_timestamp <- as.numeric(parts[2L])
              lock_age <- as.numeric(Sys.time()) - lock_timestamp

              # Remove locks older than 5 minutes
              if (lock_age > 300L) {
                unlink(mutex_file, force = TRUE)
                log_info(
                  sprintf("Removed stale %s lock (age: %s min)", operation_type, round(lock_age / 60L, 1L))
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
#'
#' Removes the mutex file to release the lock acquired by acquire_pak_mutex.
#'
#' @template param-mutex_file
#' @return Invisible NULL
release_pak_mutex <- function(mutex_file) {
  if (!is.null(mutex_file) && file.exists(mutex_file)) {
    unlink(mutex_file, force = TRUE)
  }
}

#' Classify error types for retry decisions
#'
#' Determines if an error should trigger a retry based on error message content.
#'
#' @template param-error_msg
#' @return List with should_retry (logical) and error_type (character)
classify_error_for_retry <- function(error_msg) {
  is_dependency_error <- grepl(
    paste0(
      "Could not solve package dependencies|Can't install dependency|",
      "dependency.*not available|version.*not available|No suitable version|incompatible.*version"
    ),
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
  ) && !is_dependency_error

  should_retry <- (is_locking_error || is_network_error || is_subprocess_error) && !is_dependency_error

  error_type <- if (is_locking_error) {
    "locking error"
  } else if (is_network_error) {
    "network error"
  } else if (is_subprocess_error) {
    "subprocess error"
  } else {
    "dependency error"
  }

  list(should_retry = should_retry, error_type = error_type, is_dependency_error = is_dependency_error)
}

#' Setup environment variables for package installation
#'
#' Configures platform-specific environment variables needed for pak installation.
#'
#' @template param-platform
#' @return List of environment variables
setup_installation_env_vars <- function(platform) {
  env_vars <- list(
    PKG_SYSREQS = TRUE,
    PKG_SYSREQS_VERBOSE = TRUE
  )

  if (grepl("alpine", platform, fixed = TRUE)) {
    env_vars$PKG_SYSREQS_PLATFORM <- "alpine"
  }

  if (grepl("rhel-9", pak::system_r_platform(), fixed = TRUE)) {
    env_vars$PKG_SYSREQS_PLATFORM <- "redhat-9"
    env_vars$CURL_CA_BUNDLE <- file.path("/etc", "pki", "tls", "certs", "ca-bundle.crt")
  } else if (grepl("rhel-8", pak::system_r_platform(), fixed = TRUE)) {
    env_vars$PKG_SYSREQS_PLATFORM <- "redhat-8"
    env_vars$CURL_CA_BUNDLE <- file.path("/etc", "pki", "tls", "certs", "ca-bundle.crt")
  }

  env_vars
}

#' Perform aggressive cache cleanup
#'
#' Cleans up additional pak cache locations and removes old lock files.
#'
#' @return Invisible NULL
perform_aggressive_cleanup <- function() {
  log_info("Performing aggressive pak cache cleanup...")

  additional_cache_paths <- c(
    file.path("/mnt", "cache", "pkgcache"),
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

#' Clone package repository
#'
#' Clones a package repository if it doesn't already exist locally.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-local_clone_dir_single
#' @return Invisible NULL
clone_package_repo <- function(package_name, tag, local_clone_dir_single) {
  log_debug(
    sprintf("Cloning package {.pkg %s} with tag {.field %s}.", package_name[1L], tail(tag, 1L))
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
}

#' Run pak installation with mutex protection
#'
#' Handles the mutex acquisition, installation, and cleanup for package dependencies.
#'
#' @template param-local_clone_dir_single
#' @template param-env_vars
#' @return Invisible NULL
run_pak_install_with_mutex <- function(local_clone_dir_single, env_vars) {
  mutex_file <- NULL
  tryCatch(
    {
      log_debug("Acquiring pak install mutex...")
      mutex_file <- acquire_pak_mutex("install", timeout_seconds = 180L)
      log_debug("Pak install mutex acquired")

      tryCatch(
        {
          retry_with_backoff(function() {
            withr::with_envvar(env_vars, {
              # Default to non-verbose (suppressed messages)
              suppressMessages(pak::local_install_deps(sprintf(
                "%s",
                local_clone_dir_single
              )))
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
            log_info(paste0(
              "This appears to be a dependency version conflict. ",
              "Check if the package version is compatible with current R package versions."
            ))
          }
          stop(e, call. = FALSE)
        }
      )
    },
    finally = {
      # Always release the mutex
      if (!is.null(mutex_file)) {
        release_pak_mutex(mutex_file)
        log_debug("Pak install mutex released")
      }
    }
  )
}

#' Retry a function with exponential backoff
#'
#' Retries a function with exponential backoff strategy, classifying errors
#' to determine if retry is appropriate. Only retries locking, network, and
#' subprocess errors, not dependency resolution errors.
#'
#' @template param-func
#' @template param-max_attempts
#' @template param-base_delay
#' @template param-max_delay
#' @return Result of successful function execution
#' @details Exponential backoff: delay = min(base_delay * 2^(attempt-1), max_delay)
retry_with_backoff <- function(
    func,
    max_attempts = 5L,
    base_delay = 1L,
    max_delay = 60L) {
  for (attempt in seq_len(max_attempts)) {
    tryCatch(
      {
        return(func())
      },
      error = function(e) {
        error_msg <- conditionMessage(e)
        error_classification <- classify_error_for_retry(error_msg) # nolint: object_usage_linter

        if (!error_classification$should_retry) {
          # For dependency resolution errors and other non-retryable errors, fail immediately
          if (error_classification$is_dependency_error) {
            log_error(
              sprintf("Dependency resolution error (not retryable): %s", error_msg)
            )
          }
          stop(e, call. = FALSE)
        }

        if (attempt >= max_attempts) {
          log_error(
            sprintf("All %s attempts failed. Giving up.", max_attempts)
          )
          stop(e, call. = FALSE)
        }

        delay <- min(base_delay * (2L^(attempt - 1L)), max_delay)
        log_warn(sprintf(
          "Attempt %d/%d failed with %s. Retrying in %g seconds...", # nolint
          attempt, max_attempts, error_classification$error_type, delay
        ))
        Sys.sleep(delay)
      }
    )
  }
}
