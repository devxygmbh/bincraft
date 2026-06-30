# R/patches.R

#' Load and validate the patch registry
#'
#' Reads `registry.json` from `patches_dir` and returns normalized entries.
#'
#' @param patches_dir Directory containing `registry.json` and any patch files,
#'   or `NULL` to disable patching.
#' @return A list of normalized patch entries (possibly empty).
#' @keywords internal
load_patch_registry <- function(patches_dir) {
  if (is.null(patches_dir)) {
    return(list())
  }
  registry_file <- file.path(patches_dir, "registry.json")
  if (!file.exists(registry_file)) {
    log_warn(sprintf(
      "Patch directory {.path %s} has no registry.json; patching disabled.",
      patches_dir
    ))
    return(list())
  }
  raw <- jsonlite::fromJSON(registry_file, simplifyVector = FALSE)
  lapply(raw, normalize_patch_entry, patches_dir = patches_dir)
}

#' Normalize and validate a single patch registry entry
#'
#' @param entry A list parsed from `registry.json`.
#' @param patches_dir Directory used to resolve a relative `patch` path.
#' @return The entry with defaults filled and `patch_path` resolved.
#' @keywords internal
normalize_patch_entry <- function(entry, patches_dir) {
  required <- c("package", "versions", "platforms", "reason")
  missing <- setdiff(required, names(entry))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Patch entry is missing required field(s): %s",
        toString(missing)
      ),
      call. = FALSE
    )
  }
  entry$platforms <- as.character(unlist(entry$platforms))
  entry$env <- if (is.null(entry$env)) list() else entry$env
  entry$configure_args <- if (is.null(entry$configure_args)) {
    character(0L)
  } else {
    as.character(unlist(entry$configure_args))
  }
  entry$makevars <- if (is.null(entry$makevars)) list() else entry$makevars
  if (!is.null(entry$patch)) {
    patch_path <- file.path(patches_dir, entry$patch)
    if (!file.exists(patch_path)) {
      stop(
        sprintf(
          "Patch file '%s' for package '%s' does not exist.",
          patch_path,
          entry$package
        ),
        call. = FALSE
      )
    }
    entry$patch_path <- patch_path
  } else {
    entry$patch_path <- NULL
  }
  entry
}

#' Build platform tokens for patch matching
#' @keywords internal
build_platform_tokens <- function(platform, arch) {
  family <- sub("-.*$", "", platform)
  unique(c(platform, family, arch))
}

#' Does a patch entry apply to the current platform tokens?
#' @keywords internal
entry_matches_platform <- function(entry, tokens) {
  any(entry$platforms == "*") ||
    length(intersect(entry$platforms, tokens)) > 0L
}

#' Filter registry entries applicable to the current build
#' @keywords internal
match_patch_entries <- function(registry, platform, arch) {
  if (length(registry) == 0L) {
    return(list())
  }
  tokens <- build_platform_tokens(platform, arch)
  Filter(function(e) entry_matches_platform(e, tokens), registry)
}

#' Test whether a version satisfies a single constraint
#'
#' @param version A version string (CRAN style, may contain `-`).
#' @param constraint One of `"x.y.z"`, `"==x"`, `">=x"`, `"<=x"`, `">x"`, `"<x"`.
#' @keywords internal
version_satisfies <- function(version, constraint) {
  constraint <- trimws(constraint)
  parts <- regmatches(
    constraint,
    regexec("^(>=|<=|==|>|<)?\\s*(.+)$", constraint)
  )[[1L]]
  op <- parts[2L]
  target <- parts[3L]
  v <- package_version(version)
  t <- package_version(target)
  if (op == "" || op == "==") {
    return(v == t)
  }
  switch(op,
    ">=" = v >= t,
    "<=" = v <= t,
    ">" = v > t,
    "<" = v < t,
    FALSE
  )
}

#' Compute the cache key for a patched binary
#' @keywords internal
patch_cache_key <- function(entry, version, platform, arch, r_minor) {
  payload <- list(
    env = entry$env,
    configure_args = entry$configure_args,
    makevars = entry$makevars,
    patch = if (!is.null(entry$patch_path)) {
      readBin(entry$patch_path, "raw", file.size(entry$patch_path))
    } else {
      raw(0L)
    }
  )
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(payload, tmp)
  hash <- substr(unname(tools::md5sum(tmp)), 1L, 12L)
  sprintf(
    "%s_%s_%s_%s_%s_%s",
    entry$package,
    version,
    platform,
    arch,
    r_minor,
    hash
  )
}

#' Resolve the CRAN version to build for a patch entry
#'
#' Returns the latest CRAN version satisfying the entry's `versions` constraint,
#' or `NA_character_` when CRAN's latest does not satisfy it or lookup fails.
#' @keywords internal
resolve_patch_version <- function(entry) {
  latest <- tryCatch(
    pkgsearch::cran_package(entry$package)$Version,
    error = function(e) NA_character_
  )
  if (is.na(latest)) {
    return(NA_character_)
  }
  if (
    identical(entry$versions, "*") || version_satisfies(latest, entry$versions)
  ) {
    return(latest)
  }
  NA_character_
}

#' Short human label describing a patch entry's overrides
#' @keywords internal
describe_patch <- function(entry) {
  bits <- character(0L)
  if (length(entry$env) > 0L) {
    bits <- c(
      bits,
      sprintf(
        "env: %s",
        paste(
          names(entry$env),
          unlist(entry$env),
          sep = "=",
          collapse = ","
        )
      )
    )
  }
  if (length(entry$configure_args) > 0L) {
    bits <- c(bits, sprintf("configure: %s", toString(entry$configure_args)))
  }
  if (length(entry$makevars) > 0L) {
    bits <- c(bits, "makevars")
  }
  if (!is.null(entry$patch_path)) {
    bits <- c(bits, "source patch")
  }
  if (length(bits) == 0L) "no-op" else paste(bits, collapse = "; ")
}

#' Download a CRAN source tarball for an exact version
#' @keywords internal
download_cran_source <- function(
  package,
  version,
  dest_dir,
  cran = "https://cloud.r-project.org"
) {
  fname <- sprintf("%s_%s.tar.gz", package, version)
  urls <- c(
    sprintf("%s/src/contrib/%s", cran, fname),
    sprintf("%s/src/contrib/Archive/%s/%s", cran, package, fname)
  )
  dest <- file.path(dest_dir, fname)
  for (u in urls) {
    ok <- tryCatch(
      {
        utils::download.file(u, dest, mode = "wb", quiet = TRUE)
        file.exists(dest) && file.size(dest) > 0L
      },
      error = function(e) FALSE
    )
    if (isTRUE(ok)) {
      return(dest)
    }
  }
  log_warn(sprintf(
    "Could not download CRAN source for {.pkg %s} %s.",
    package,
    version
  ))
  NULL
}

#' Apply a unified diff to an unpacked source tree
#'
#' Uses `git apply` rather than the `patch` CLI, because `git` is always present
#' in the build environments (it clones every package) whereas `patch` is not
#' (e.g. minimal Alpine images). A non-applying or already-applied patch exits
#' non-zero, so this returns FALSE instead of corrupting the tree. `git apply`
#' does not require `pkg_src` to be a git repository.
#' @keywords internal
apply_source_patch <- function(patch_path, pkg_src) {
  status <- system2(
    "git",
    args = c(
      "-C",
      shQuote(pkg_src),
      "apply",
      "--whitespace=nowarn",
      "-p1",
      shQuote(normalizePath(patch_path))
    ),
    stdout = FALSE,
    stderr = FALSE
  )
  identical(status, 0L)
}

#' Format configure args for `pkgbuild::build(args = ...)`
#' @keywords internal
configure_args_to_build_args <- function(configure_args) {
  if (length(configure_args) == 0L) {
    return(character(0L))
  }
  sprintf("--configure-args=%s", paste(configure_args, collapse = " "))
}

#' Build a patched binary for one registry entry, in isolation
#'
#' Downloads CRAN source for `version`, applies the source patch (if any), and
#' builds a binary with the entry's env / configure / Makevars overrides scoped
#' to this build only. Returns the built tarball path, or `NULL` on any failure.
#' @keywords internal
build_patched_binary <- function(entry, version, dest_dir) {
  workdir <- tempfile("patch_build_")
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(workdir, recursive = TRUE, force = TRUE), add = TRUE)

  src_tarball <- download_cran_source(entry$package, version, workdir)
  if (is.null(src_tarball)) {
    return(NULL)
  }

  utils::untar(src_tarball, exdir = workdir)
  pkg_src <- file.path(workdir, entry$package)

  if (!is.null(entry$patch_path)) {
    if (!apply_source_patch(entry$patch_path, pkg_src)) {
      log_warn(sprintf(
        "Patch for {.pkg %s} %s did not apply cleanly; skipping patched build.",
        entry$package,
        version
      ))
      return(NULL)
    }
  }

  build_env <- entry$env
  if (length(entry$makevars) > 0L) {
    mk <- tempfile(fileext = ".mk")
    writeLines(
      vapply(
        names(entry$makevars),
        function(k) sprintf("%s=%s", k, entry$makevars[[k]]),
        character(1L)
      ),
      mk
    )
    build_env$R_MAKEVARS_USER <- mk
  }

  do_build <- function() {
    pkgbuild::build(
      path = pkg_src,
      binary = TRUE,
      vignettes = FALSE,
      dest_path = dest_dir,
      args = configure_args_to_build_args(entry$configure_args),
      quiet = TRUE
    )
  }

  tryCatch(
    # withr::with_envvar() errors on an empty list (is.named(list()) is FALSE),
    # so only wrap the build when there are env vars to set (e.g. a pure
    # source-diff patch sets none).
    if (length(build_env) > 0L) {
      withr::with_envvar(build_env, do_build())
    } else {
      do_build()
    },
    error = function(e) {
      log_warn(sprintf(
        "Isolated patched build of {.pkg %s} %s failed: %s",
        entry$package,
        version,
        conditionMessage(e)
      ))
      NULL
    }
  )
}

#' Prepare a local repo of patched binaries for the current build
#'
#' For each registry entry matching the current platform, ensures a patched
#' binary is present in `repo_dir` (from `cache_dir` if available, else built
#' and then cached) and writes a `PACKAGES` index over them.
#'
#' @param patches_dir Directory with `registry.json`, or `NULL`.
#' @param platform Build platform, e.g. `"ubuntu-2604"`.
#' @param arch Build arch, e.g. `"amd64"`.
#' @param r_minor R `"major.minor"` string, e.g. `"4.5"`.
#' @param cache_dir Persistent cache for patched binaries.
#' @param repo_dir Directory to assemble the local repo in.
#' @return `repo_dir` if at least one patched binary was produced, else `NULL`.
#' @keywords internal
prepare_patched_repo <- function(
  patches_dir,
  platform,
  arch,
  r_minor,
  cache_dir = file.path("/mnt", "cache", "patched-binaries"),
  repo_dir = tempfile("patched_repo_")
) {
  entries <- match_patch_entries(
    load_patch_registry(patches_dir),
    platform,
    arch
  )
  if (length(entries) == 0L) {
    return(NULL)
  }

  dir.create(repo_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  contrib <- file.path(repo_dir, "src", "contrib")
  dir.create(contrib, recursive = TRUE, showWarnings = FALSE)

  produced <- 0L
  for (entry in entries) {
    version <- resolve_patch_version(entry)
    if (is.na(version)) {
      log_warn(sprintf(
        "No CRAN version of {.pkg %s} satisfies '%s'; patch skipped.",
        entry$package,
        entry$versions
      ))
      next
    }

    key <- patch_cache_key(entry, version, platform, arch, r_minor)
    cached <- file.path(cache_dir, sprintf("%s.tar.gz", key))
    target <- file.path(
      contrib,
      sprintf("%s_%s.tar.gz", entry$package, version)
    )

    if (file.exists(cached)) {
      log_info(sprintf(
        "Using cached patched binary for {.pkg %s} %s.",
        entry$package,
        version
      ))
      file.copy(cached, target, overwrite = TRUE)
    } else {
      log_info(sprintf(
        "Applying patch to {.pkg %s} %s [%s]: %s",
        entry$package,
        version,
        describe_patch(entry),
        entry$reason
      ))
      built <- build_patched_binary(entry, version, contrib)
      if (is.null(built)) {
        next
      }
      if (
        !identical(
          normalizePath(built, mustWork = FALSE),
          normalizePath(target, mustWork = FALSE)
        )
      ) {
        file.copy(built, target, overwrite = TRUE)
      }
      file.copy(target, cached, overwrite = TRUE)
    }
    produced <- produced + 1L
  }

  if (produced == 0L) {
    return(NULL)
  }
  tools::write_PACKAGES(contrib, type = "source", fields = "Built")
  repo_dir
}
