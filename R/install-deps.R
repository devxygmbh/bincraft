#' Install system dependencies for R package
#' @template param-package_name
#' @template param-tag
#' @template param-platform
#' @template param-deps_verbose
#' @template param-local_clone_dir
#' @template param-debug
#'
#' @export
install_package_system_dependencies <- function(package_name,
                                                tag,
                                                platform = platform,
                                                local_clone_dir,
                                                deps_verbose = FALSE,
                                                debug = FALSE) {
  if (debug) {
    cli::cli_alert("Cloning package {.pkg {package_name[1]}} with tag {.field {tail(tag, 1)}}.")
  }

  t1 <- Sys.time()

  local_clone_dir_single <- sprintf("%s/%s_%s", local_clone_dir, package_name[1], tail(tag, 1))

  if (!dir.exists(local_clone_dir_single)) {
    system2("git", args = c(
      "clone", "-q", sprintf("--branch=%s", tail(tag, 1)),
      sprintf("https://github.com/cran/%s", package_name[1]), local_clone_dir_single
    ))
  }

  if (grepl("alpine", platform)) {
    platform <- "alpine"
    Sys.setenv(PKG_SYSREQS_PLATFORM = "alpine")
  }

  cli::cli_alert("Installing R package dependencies")
  Sys.setenv(PKG_SYSREQS = TRUE)
  Sys.setenv(PKG_SYSREQS_VERBOSE = TRUE)
  # FIXME: https://github.com/r-lib/pak/issues/610
  if (grepl("rhel-9", pak::system_r_platform())) {
    Sys.setenv(PKG_SYSREQS_PLATFORM = "redhat-9")
    # otherwise pak::pkg_history() fails with 'error setting certificate verify locations'
    Sys.setenv(CURL_CA_BUNDLE = "/etc/pki/tls/certs/ca-bundle.crt")
  } else if (grepl("rhel-8", pak::system_r_platform())) {
    Sys.setenv(PKG_SYSREQS_PLATFORM = "redhat-8")
    # otherwise pak::pkg_history() fails with 'error setting certificate verify locations'
    Sys.setenv(CURL_CA_BUNDLE = "/etc/pki/tls/certs/ca-bundle.crt")
  }
  if (deps_verbose) {
    pak::local_install_deps(sprintf("%s", local_clone_dir_single))
  } else {
    suppressMessages(pak::local_install_deps(sprintf("%s", local_clone_dir_single)))
  }

  if (debug) {
    cli::cli_alert("Removing temporary clone dir at {.path {local_clone_dir_single}}.")
  }

  total_build_time <- round(Sys.time() - t1, 2)
  cli::cli_alert("R package dependencies installation time ({.pkg {package_name[[1]]}}): {.strong {total_build_time} {units(difftime(Sys.time(), t1))}}.")

  unlink(sprintf("%s", local_clone_dir_single), recursive = TRUE, force = TRUE)
}
