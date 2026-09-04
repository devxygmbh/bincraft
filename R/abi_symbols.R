# ABI classification from symbols, rather than from source heuristics.
#
# `abi_classify()` answers "might this package be r-minor-sensitive?" from the
# source: it flags anything with a risky `LinkingTo` or a curated symbol name in
# its sources. That is a guess made before a binary exists, and it is wrong in
# both directions at once.
#
# Too broad: `LinkingTo: Rcpp` alone flags most compiled CRAN packages. On
# amd64/resolute the classifier called 93% of compiled packages sensitive.
#
# Too narrow: the curated list holds 15 symbols. Diffing what the installed R
# builds actually export shows 180 that 4.4/4.5 have and 4.6 does not, and 103
# the other way. `SET_FORMALS`, `SET_CLOENV`, `SET_TRUELENGTH` and the ALTREP
# accessors are all absent from the list, and packages using them break exactly
# like base64enc did on `SETLENGTH`.
#
# Once a binary exists the question stops being a guess. A package fails at
# `dyn.load()` under R minor M when its shared objects reference a symbol M does
# not export, which is decidable with `nm`. This file does that.
#
# The verdict degrades safely: without `nm`, without an `/opt/R` tree, or on any
# parse failure these return NULL and the caller keeps whatever the source
# classifier decided.

#' Symbols a shared library defines or references
#'
#' @param path Path to an ELF shared object.
#' @param undefined When `TRUE` list the symbols the object needs from
#'   elsewhere; when `FALSE` those it provides.
#' @return Character vector, or NULL when `nm` is unavailable or failed.
#' @keywords internal
#' @noRd
nm_symbols <- function(path, undefined = FALSE) {
  if (!nzchar(Sys.which("nm")) || !file.exists(path)) {
    return(NULL)
  }
  flag <- if (undefined) "--undefined-only" else "--defined-only"
  out <- tryCatch(
    suppressWarnings(system2(
      "nm",
      c("-D", flag, shQuote(path)),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(out) || !is.character(out)) {
    return(NULL)
  }
  parse_nm_output(out)
}

#' Extract symbol names from `nm` output
#'
#' Kept separate from the call so the parsing is testable without a toolchain.
#' `nm -D` prints `<address> <type> <name>`, with the address blank for
#' undefined symbols. Versioned names carry an `@GLIBC_2.2.5` suffix that is not
#' part of the symbol.
#'
#' @keywords internal
#' @noRd
parse_nm_output <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  names <- sub("^.*\\s(\\S+)$", "\\1", lines)
  names <- sub("@.*$", "", names)
  sort(unique(names[nzchar(names)]))
}

#' Symbols exported by an R installation
#'
#' @param r_home An R installation root, e.g. `/opt/R/4.6.0`.
#' @return Character vector, or NULL when the library cannot be read.
#' @keywords internal
#' @noRd
r_home_symbols <- function(r_home) {
  lib <- file.path(r_home, "lib", "R", "lib", "libR.so")
  if (!file.exists(lib)) {
    lib <- file.path(r_home, "lib", "libR.so")
  }
  if (!file.exists(lib)) {
    return(NULL)
  }
  nm_symbols(lib)
}

#' Exported-symbol sets of every R minor installed in this image
#'
#' The build images carry exactly the supported window, so the directories under
#' `/opt/R` define it without it being restated anywhere.
#'
#' @return Named list of character vectors keyed by `"major.minor"`, or NULL
#'   when fewer than two minors could be read (a diff needs two).
#' @keywords internal
#' @noRd
installed_r_symbol_sets <- function(root = "/opt/R") {
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[grepl("/[0-9]+\\.[0-9]+\\.[0-9]+$", dirs)]
  if (length(dirs) == 0L) {
    return(NULL)
  }
  minors <- sub("^([0-9]+\\.[0-9]+).*$", "\\1", basename(dirs))
  sets <- list()
  for (i in seq_along(dirs)) {
    if (!is.null(sets[[minors[[i]]]])) {
      next
    }
    syms <- r_home_symbols(dirs[[i]])
    if (!is.null(syms) && length(syms) > 0L) {
      sets[[minors[[i]]]] <- syms
    }
  }
  if (length(sets) < 2L) {
    return(NULL)
  }
  sets[order(names(sets))]
}

#' Symbols that are not available in every supported R minor
#'
#' The union of all the sets minus their intersection: a symbol at least one
#' minor exports and at least one does not. Referencing one of these is what
#' makes a binary unsafe to serve across minors, and deriving it from the
#' installed builds means a new R release updates it with no list to maintain.
#'
#' @param sets Output of `installed_r_symbol_sets()`.
#' @return Character vector, possibly empty; NULL when `sets` is unusable.
#' @keywords internal
#' @noRd
cross_minor_volatile_symbols <- function(sets = installed_r_symbol_sets()) {
  if (is.null(sets) || length(sets) < 2L) {
    return(NULL)
  }
  everywhere <- Reduce(intersect, sets)
  anywhere <- Reduce(union, sets)
  sort(setdiff(anywhere, everywhere))
}

#' The R minors a built package can be loaded under
#'
#' @param so_files Shared objects belonging to one installed package.
#' @param sets Output of `installed_r_symbol_sets()`.
#' @return List with `inspected`, `sensitive`, `symbols` (the volatile symbols
#'   referenced) and `unsupported` (the minors that lack at least one of them).
#'   `inspected = FALSE` means no judgement could be made.
#' @keywords internal
#' @noRd
shared_object_abi_verdict <- function(
  so_files,
  sets = installed_r_symbol_sets()
) {
  unknown <- list(
    inspected = FALSE,
    sensitive = NA,
    symbols = character(0),
    unsupported = character(0)
  )
  if (is.null(sets) || length(sets) < 2L) {
    return(unknown)
  }
  # A package with no compiled code cannot fail at dyn.load(), so absence of
  # shared objects is a real answer rather than a failure to inspect.
  if (length(so_files) == 0L) {
    return(list(
      inspected = TRUE,
      sensitive = FALSE,
      symbols = character(0),
      unsupported = character(0)
    ))
  }
  needed <- character(0)
  for (f in so_files) {
    syms <- nm_symbols(f, undefined = TRUE)
    if (is.null(syms)) {
      return(unknown)
    }
    needed <- union(needed, syms)
  }
  volatile <- cross_minor_volatile_symbols(sets)
  referenced <- sort(intersect(needed, volatile))
  unsupported <- names(sets)[vapply(
    sets,
    function(exported) !all(referenced %in% exported),
    logical(1L)
  )]
  list(
    inspected = TRUE,
    sensitive = length(unsupported) > 0L,
    symbols = referenced,
    unsupported = unsupported
  )
}

#' Verdict for a built binary tarball
#'
#' @param tarball Path to a built package tarball.
#' @inheritParams shared_object_abi_verdict
#' @keywords internal
#' @noRd
tarball_abi_verdict <- function(tarball, sets = installed_r_symbol_sets()) {
  unknown <- list(
    inspected = FALSE,
    sensitive = NA,
    symbols = character(0),
    unsupported = character(0)
  )
  if (!file.exists(tarball)) {
    return(unknown)
  }
  dest <- tempfile("abi_verdict_")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)

  members <- tryCatch(
    utils::untar(tarball, list = TRUE),
    error = function(e) NULL
  )
  if (is.null(members)) {
    return(unknown)
  }
  so <- grep("/libs/.*\\.so$", members, value = TRUE)
  if (length(so) == 0L) {
    return(shared_object_abi_verdict(character(0), sets))
  }
  extracted <- tryCatch(
    {
      utils::untar(tarball, files = so, exdir = dest)
      file.path(dest, so)
    },
    error = function(e) NULL
  )
  if (is.null(extracted) || !all(file.exists(extracted))) {
    return(unknown)
  }
  shared_object_abi_verdict(extracted, sets)
}
