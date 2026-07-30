# Helper functions are now in install_helpers.R

#' Install R dependencies and their system requirements for a package
#'
#' Clones the package source, then installs its dependency tree and the system
#' libraries those dependencies need via `uvr` (`uvr sync
#' --install-system-deps`), so a dependency needing a system library no longer
#' fails the build. Transient failures are retried with exponential backoff.
#'
#' When patches apply, their pre-built binaries are installed into the build
#' library first, so `uvr sync` keeps them instead of overwriting them with the
#' unpatched CRAN build.
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

  log_info("Installing R package dependencies")

  # Build a local repo of patched binaries (if any apply). They are installed
  # into the build library before `uvr sync`, which then leaves them untouched.
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

  # Install dependencies + system requirements via uvr, retrying transient
  # (network/IO) failures.
  retry_with_backoff(function() {
    run_uvr_install(
      clone_dir = local_clone_dir_single,
      library = .libPaths()[1L],
      patched_repo = patched_repo
    )
  })

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
