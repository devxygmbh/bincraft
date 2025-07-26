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
#' @template param-deps_verbose
#' @template param-local_clone_dir
#' @template param-is_debug
#' @template param-aggressive_cleanup
#'
#' @export
install_pkg_sys_deps <- function(
    package_name,
    tag,
    local_clone_dir,
    platform = platform,
    deps_verbose = FALSE,
    is_debug = FALSE,
    aggressive_cleanup = FALSE) {
  t1 <- Sys.time()

  # Clone package repository
  local_clone_dir_single <- file.path(
    local_clone_dir,
    paste0(package_name[1L], "_", tail(tag, 1L))
  )
  clone_package_repo(package_name, tag, local_clone_dir_single, is_debug) # nolint: object_usage_linter

  # Setup environment variables
  env_vars <- setup_installation_env_vars(platform) # nolint: object_usage_linter

  cli::cli_alert("Installing R package dependencies")

  # Clean up any stale lock files before attempting installation
  cleanup_stale_locks()

  # Perform additional cleanup if requested
  if (aggressive_cleanup) {
    perform_aggressive_cleanup() # nolint: object_usage_linter
  }

  # Run installation with mutex protection
  run_pak_install_with_mutex( # nolint: object_usage_linter
    local_clone_dir_single,
    env_vars,
    deps_verbose,
    is_debug
  )

  if (is_debug) {
    cli::cli_alert(
      "Removing temporary clone dir at {.path {local_clone_dir_single}}."
    )
  }

  total_build_time <- round(Sys.time() - t1, 2L)
  time_units <- units(difftime(Sys.time(), t1))
  cli::cli_alert(sprintf(
    "R package dependencies installation time (%s): %g %s.",
    package_name[[1L]], total_build_time, time_units
  ))

  unlink(sprintf("%s", local_clone_dir_single), recursive = TRUE, force = TRUE)
}
