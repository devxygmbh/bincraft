#' Install system dependencies for an R package
#' @template param-package_name
#' @template param-tag
#' @template param-platform
#' @template param-deps_verbose
#' @template param-local_clone_dir
#' @template param-is_debug
#'
#' @export
install_pkg_sys_deps <- function(
    package_name,
    tag,
    local_clone_dir,
    platform = platform,
    deps_verbose = FALSE,
    is_debug = FALSE) {
  if (is_debug) {
    cli::cli_alert("Cloning package {.pkg {package_name[1L]}} with tag {.field {tail(tag, 1L)}}.")
  }

  t1 <- Sys.time()

  local_clone_dir_single <- file.path(local_clone_dir, paste0(package_name[1L], "_", tail(tag, 1L)))

  if (!dir.exists(local_clone_dir_single)) {
    system2("git", args = c(
      "clone", "-q", sprintf("--branch=%s", tail(tag, 1L)),
      sprintf("https://github.com/cran/%s", package_name[1L]), local_clone_dir_single
    ))
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

  withr::with_envvar(env_vars, {
    if (deps_verbose) {
      pak::local_install_deps(sprintf("%s", local_clone_dir_single))
    } else {
      suppressMessages(pak::local_install_deps(sprintf("%s", local_clone_dir_single)))
    }
  })

  if (is_debug) {
    cli::cli_alert("Removing temporary clone dir at {.path {local_clone_dir_single}}.")
  }

  total_build_time <- round(Sys.time() - t1, 2L) # nolint
  cli::cli_alert("R package dependencies installation time ({.pkg {package_name[[1L]]}}): {.strong {total_build_time} {units(difftime(Sys.time(), t1))}}.") # nolint

  unlink(sprintf("%s", local_clone_dir_single), recursive = TRUE, force = TRUE)
}
