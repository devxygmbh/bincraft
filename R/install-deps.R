# Helper functions are now in install_helpers.R

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
#' @template param-local_clone_dir
#' @template param-aggressive_cleanup
#' @template param-patches
#' @template param-arch
#'
#' @export
install_pkg_sys_deps <- function(
  package_name,
  tag,
  local_clone_dir,
  platform = platform,
  aggressive_cleanup = FALSE,
  patches = NULL,
  arch = NULL
) {
  t1 <- Sys.time()

  # Clone package repository
  local_clone_dir_single <- file.path(
    local_clone_dir,
    paste0(package_name[1L], "_", tail(tag, 1L))
  )
  clone_package_repo(package_name, tag, local_clone_dir_single)

  # Setup environment variables
  env_vars <- setup_installation_env_vars(platform)

  log_info("Installing R package dependencies")

  # Clean up any stale lock files before attempting installation
  cleanup_stale_locks()

  # Perform additional cleanup if requested
  if (aggressive_cleanup) {
    perform_aggressive_cleanup()
  }

  # Build a local repo of patched binaries (if any apply) and serve it to pak.
  r_minor <- paste(
    R.version$major,
    strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L],
    sep = "."
  )
  patched_repo <- tryCatch(
    prepare_patched_repo(patches, platform, arch, r_minor),
    error = function(e) {
      log_warn(sprintf("Patch preparation failed: %s", conditionMessage(e)))
      NULL
    }
  )

  # Run installation with mutex protection
  run_pak_install_with_mutex(
    local_clone_dir_single,
    env_vars,
    patched_repo = patched_repo
  )

  log_debug(
    sprintf(
      "Removing temporary clone dir at {.path %s}.",
      local_clone_dir_single
    )
  )

  total_build_time <- round(Sys.time() - t1, 2L)
  time_units <- units(difftime(Sys.time(), t1))
  log_info(sprintf(
    "R package dependencies installation time (%s): %s %s.",
    package_name[[1L]],
    total_build_time,
    time_units
  ))

  unlink(sprintf("%s", local_clone_dir_single), recursive = TRUE, force = TRUE)
}
