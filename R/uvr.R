# Dependency and system-requirement installation via uvr.
#
# bincraft drives the `uvr` CLI to install a package's dependency tree and the
# system libraries those dependencies need in a single pass. uvr reads
# `/etc/os-release`, resolves system requirements per distro from its vendored
# Posit `r-system-requirements` rules, and installs them with the distro's
# package manager. This replaces pak, which installed R packages but not their
# system libraries.

# Base package names that are never installable from CRAN. `R` is included so a
# `Depends: R (>= x)` entry is dropped. Recommended packages (MASS, Matrix, ...)
# are intentionally not listed: they live on CRAN and a package may legitimately
# need a newer one than the one bundled with R.
BINCRAFT_BASE_PACKAGES <- c(
  "R",
  "base",
  "compiler",
  "datasets",
  "graphics",
  "grDevices",
  "grid",
  "methods",
  "parallel",
  "splines",
  "stats",
  "stats4",
  "tcltk",
  "tools",
  "utils"
)

#' Locate the uvr executable
#'
#' @return Absolute path to the `uvr` binary.
#' @keywords internal
uvr_bin <- function() {
  bin <- Sys.which("uvr")
  if (!nzchar(bin)) {
    stop(
      "`uvr` was not found on PATH. bincraft installs R dependencies and ",
      "their system requirements with uvr; ensure the build image ships it.",
      call. = FALSE
    )
  }
  unname(bin)
}

#' Extract a package's build dependencies from its DESCRIPTION
#'
#' Collects `Imports`, `Depends`, and `LinkingTo`, strips version constraints,
#' and drops base packages and `R`. `Suggests` is excluded, matching the scope
#' pak's `local_install_deps()` installed. `LinkingTo` is included explicitly
#' because uvr's own DESCRIPTION parser drops the root package's `LinkingTo`,
#' which would miss compile-time header dependencies (for example
#' `RcppArmadillo`).
#'
#' @param desc_path Path to a `DESCRIPTION` file.
#' @return Character vector of dependency package names.
#' @keywords internal
parse_description_deps <- function(desc_path) {
  dcf <- read.dcf(desc_path)
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(dcf))
  if (length(fields) == 0L) {
    return(character())
  }
  raw <- unlist(strsplit(paste(dcf[1L, fields], collapse = ","), ","))
  names <- trimws(sub("\\(.*", "", raw))
  names <- names[nzchar(names) & names != "NA"]
  setdiff(unique(names), BINCRAFT_BASE_PACKAGES)
}

#' Write a minimal uvr.toml derived from a package's DESCRIPTION
#'
#' @param clone_dir Directory containing the package `DESCRIPTION`; the
#'   `uvr.toml` is written here.
#' @param deps Optional pre-computed dependency vector; defaults to
#'   [parse_description_deps()] on the clone's `DESCRIPTION`.
#' @return Invisibly, the path to the written `uvr.toml`.
#' @keywords internal
write_uvr_manifest <- function(clone_dir, deps = NULL) {
  desc_path <- file.path(clone_dir, "DESCRIPTION")
  dcf <- read.dcf(desc_path)
  pkg <- if ("Package" %in% colnames(dcf)) {
    dcf[1L, "Package"]
  } else {
    "bincraft-deps"
  }
  if (is.null(deps)) {
    deps <- parse_description_deps(desc_path)
  }
  lines <- c(
    "[project]",
    sprintf('name = "%s"', pkg),
    "",
    "[dependencies]",
    vapply(deps, function(d) sprintf('%s = "*"', d), character(1L))
  )
  toml_path <- file.path(clone_dir, "uvr.toml")
  writeLines(lines, toml_path)
  invisible(toml_path)
}

#' Pin the R version uvr must target to the R running the build
#'
#' uvr resolves the R it targets on its own: `.r-version` first, then the
#' `uvr.toml` `r_version` constraint, then the newest R it can find (managed
#' installs before system R). In an image carrying more than one R -- for
#' example a `/opt/R/current` symlink already moved to a newer release than the
#' R that started the build -- "newest wins" selects an R other than the
#' session's. `uvr lock` then resolves per-R-minor binary URLs for the wrong
#' minor and `uvr sync` refuses to install anything at all: "Refusing to
#' install: uvr is running inside R 4.5 but the project pin/lockfile resolves to
#' R 4.6". Writing an exact pin binds uvr to the session driving the build.
#'
#' The pin is assembled the way uvr itself queries a version
#' (`R.version$major` + `R.version$minor`), so it matches the R uvr discovers
#' via `R_HOME` character for character; a non-matching pin is a hard uvr error
#' rather than a silent build against the wrong R.
#'
#' @param clone_dir Directory to write `.r-version` into; uvr reads it from the
#'   working directory of the `lock` and `sync` runs.
#' @return Invisibly, the path to the written `.r-version`.
#' @keywords internal
write_r_version_pin <- function(clone_dir) {
  path <- file.path(clone_dir, ".r-version")
  writeLines(paste(R.version$major, R.version$minor, sep = "."), path)
  invisible(path)
}

#' Run a uvr subcommand, capturing output and raising on failure
#'
#' @param args Character vector of arguments passed to `uvr`.
#' @param wd Working directory to run `uvr` in (the package clone).
#' @param env Named character vector of environment variables to set for the run.
#' @return Invisibly, the captured stdout+stderr lines.
#' @keywords internal
run_uvr <- function(args, wd, env = character()) {
  uvr <- uvr_bin()
  out <- withr::with_dir(
    wd,
    withr::with_envvar(
      env,
      suppressWarnings(system2(uvr, args, stdout = TRUE, stderr = TRUE))
    )
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      sprintf(
        "uvr %s failed (exit %s):\n%s",
        paste(args, collapse = " "),
        status,
        paste(out, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  invisible(out)
}

#' Install patched binaries into the build library so uvr skips them
#'
#' `uvr sync` leaves a package untouched when it is already present in the
#' target library at the locked version. Installing the patched binaries first
#' therefore preserves them: uvr sees the (matching-version) package installed
#' and does not overwrite it with the unpatched CRAN build. uvr cannot read a
#' `file://` repository, so this pre-install replaces the pak-era approach of
#' prepending a local patched repo to the resolver's repositories.
#'
#' @param patched_repo Path to a cranlike repo with `src/contrib/*.tar.gz`
#'   patched binary bundles (from [prepare_patched_repo()]).
#' @param library Target library to install into.
#' @return Invisibly, the installed tarball paths.
#' @keywords internal
install_patched_into_library <- function(patched_repo, library) {
  tarballs <- list.files(
    file.path(patched_repo, "src", "contrib"),
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )
  for (tb in tarballs) {
    status <- system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "INSTALL", sprintf("--library=%s", library), shQuote(tb)),
      stdout = FALSE,
      stderr = FALSE
    )
    if (!identical(as.integer(status), 0L)) {
      log_warn(sprintf(
        "Could not pre-install patched binary {.path %s} (status %s).",
        basename(tb),
        status
      ))
    }
  }
  invisible(tarballs)
}

#' Install a package's dependency tree and system requirements via uvr
#'
#' Pre-installs any patched binaries, generates a `uvr.toml` from the package's
#' `DESCRIPTION`, pins the R version with [write_r_version_pin()], resolves a
#' lockfile with `uvr lock`, and installs dependencies plus their system
#' requirements with `uvr sync --install-system-deps` into the active build
#' library. On a non-TTY (CI / build container) uvr installs the system packages
#' without prompting.
#'
#' @param clone_dir Directory containing the target package source (with its
#'   `DESCRIPTION`).
#' @param library Library to install dependencies into; defaults to the active
#'   build library `.libPaths()[1]`.
#' @param patched_repo Optional patched-binary repo to pre-install first.
#' @return Invisibly TRUE.
#' @keywords internal
run_uvr_install <- function(
  clone_dir,
  library = .libPaths()[1L],
  patched_repo = NULL
) {
  if (!is.null(patched_repo)) {
    install_patched_into_library(patched_repo, library)
  }
  write_uvr_manifest(clone_dir)
  write_r_version_pin(clone_dir)
  run_uvr("lock", clone_dir)
  run_uvr(
    c("sync", "--install-system-deps", "--library", library),
    clone_dir,
    env = c(UVR_INSTALL_SYSREQS = "1")
  )
  invisible(TRUE)
}
