#' Classify an R source package by its ABI-risk tier
#'
#' Inspects an unpacked R source package (or a `.tar.gz` of one) and labels it
#' as one of:
#'
#' - `"pure-r"`: no compilation needed — build once, serve for every
#'   R-version slot.
#' - `"safe-compiled"`: compiles, but only touches the stable R C-API — build
#'   once per major.minor.
#' - `"risky"`: links to a known-volatile dependency (e.g. `Rcpp`, `rlang`) or
#'   references internal R C-API symbols known to change across minor versions
#'   — build per R minor version.
#'
#' Decision rules (first match wins):
#'
#' 1. `pure-r` if `DESCRIPTION` has `NeedsCompilation: no`, or there is no
#'    `src/`, or `src/` contains no `.c`, `.cc`, `.cpp`, `.f`, `.f90` files.
#' 2. `risky` if `LinkingTo` mentions any package in
#'    [abi_risky_linking_deps()].
#' 3. `risky` if any file in `src/` mentions a symbol in
#'    [abi_volatile_symbols()] (plain regex grep, no C parsing).
#' 4. `safe-compiled` otherwise.
#'
#' @param path Path to an unpacked source-package directory (containing
#'   `DESCRIPTION`) or to a `.tar.gz` of one. Tarballs are untarred to a
#'   temporary directory.
#'
#' @return A list with elements:
#'
#' - `tier`: one of `"pure-r"`, `"safe-compiled"`, `"risky"`.
#' - `reason`: short string explaining the classification.
#' - `hits`: character vector of packages or symbols that triggered `"risky"`;
#'   empty otherwise.
#'
#' @seealso [abi_volatile_symbols()], [abi_risky_linking_deps()].
#' @export
abi_classify <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Path '%s' does not exist", path), call. = FALSE)
  }

  pkg_dir <- if (is_source_tarball(path)) {
    extract_source_tarball(path)
  } else {
    path
  }

  desc_path <- file.path(pkg_dir, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop(
      sprintf("No DESCRIPTION file found in '%s'", pkg_dir),
      call. = FALSE
    )
  }

  desc <- read.dcf(desc_path)
  needs_compilation <- dcf_field(desc, "NeedsCompilation")
  src_dir <- file.path(pkg_dir, "src")
  compilable <- list_compilable_sources(src_dir)

  if (
    identical(tolower(needs_compilation), "no") ||
      !dir.exists(src_dir) ||
      length(compilable) == 0L
  ) {
    return(list(
      tier = "pure-r",
      reason = "no compilation needed",
      hits = character()
    ))
  }

  matched_deps <- match_risky_linking_to(
    dcf_field(desc, "LinkingTo"),
    abi_risky_linking_deps()
  )
  if (length(matched_deps) > 0L) {
    return(list(
      tier = "risky",
      reason = sprintf(
        "LinkingTo %s",
        paste(matched_deps, collapse = ", ")
      ),
      hits = matched_deps
    ))
  }

  matched_symbols <- grep_volatile_symbols(src_dir, abi_volatile_symbols())
  if (length(matched_symbols) > 0L) {
    return(list(
      tier = "risky",
      reason = sprintf("uses %s", paste(matched_symbols, collapse = ", ")),
      hits = matched_symbols
    ))
  }

  list(
    tier = "safe-compiled",
    reason = "compiled but only stable R C-API detected",
    hits = character()
  )
}

#' Does this package need to be recompiled per R minor version?
#'
#' Thin convenience wrapper around [abi_classify()] that returns a single
#' logical: `TRUE` if the package must be recompiled per R minor version
#' (tier `"risky"`), `FALSE` otherwise.
#'
#' Note this is a strictly bincraft-internal "do I need to invoke the
#' compiler again per minor?" question — it is not the same as the
#' artifact-level "does one binary work across all R versions?" question
#' (which is TRUE only for `pure-r`). `safe-compiled` is FALSE here (one
#' compile is enough) but is still stored per-minor at the artifact level.
#'
#' The full classification is attached as attributes (`tier`, `reason`,
#' `hits`) so callers can inspect *why* without a second call.
#'
#' @inheritParams abi_classify
#'
#' @return A logical of length 1. Attributes:
#' - `tier`: the [abi_classify()] tier (`"pure-r"`, `"safe-compiled"`, or
#'   `"risky"`).
#' - `reason`: short human-readable string.
#' - `hits`: packages or symbols that triggered `"risky"`; empty otherwise.
#'
#' @seealso [abi_classify()] for the full structured result.
#' @export
#' @examples
#' \dontrun{
#' result <- needs_per_minor_recompile("path/to/pkg")
#' if (result) {
#'   message("Recompile per R minor; reason: ", attr(result, "reason"))
#' }
#' }
needs_per_minor_recompile <- function(path) {
  classification <- abi_classify(path)
  out <- classification$tier == "risky"
  attr(out, "tier") <- classification$tier
  attr(out, "reason") <- classification$reason
  attr(out, "hits") <- classification$hits
  out
}

#' ABI-risky curated lists
#'
#' The curated lists [abi_classify()] consults. Stored as plain-text files
#' under `inst/extdata/` so they can be updated without code changes.
#'
#' - [abi_volatile_symbols()] returns the R C-API symbols whose presence in
#'   package sources marks the package as ABI-risky.
#' - [abi_risky_linking_deps()] returns the packages whose `LinkingTo`
#'   presence marks a package as ABI-risky.
#'
#' @return Character vector.
#' @name abi_curated_lists
NULL

#' @rdname abi_curated_lists
#' @export
abi_volatile_symbols <- function() {
  read_curated_list("abi_volatile_symbols.txt")
}

#' @rdname abi_curated_lists
#' @export
abi_risky_linking_deps <- function() {
  read_curated_list("abi_risky_linking_deps.txt")
}

read_curated_list <- function(filename) {
  path <- system.file("extdata", filename, package = "bincraft")
  if (!nzchar(path)) {
    stop(
      sprintf("Curated list file '%s' not found in bincraft", filename),
      call. = FALSE
    )
  }
  lines <- trimws(readLines(path, warn = FALSE))
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

is_source_tarball <- function(path) {
  grepl("\\.tar\\.gz$|\\.tgz$", path, ignore.case = TRUE)
}

extract_source_tarball <- function(tarball) {
  extract_dir <- tempfile("abi_classify_")
  dir.create(extract_dir)
  utils::untar(tarball, exdir = extract_dir)
  desc_files <- list.files(
    extract_dir,
    pattern = "^DESCRIPTION$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(desc_files) == 0L) {
    stop(
      sprintf("No DESCRIPTION file found in tarball '%s'", tarball),
      call. = FALSE
    )
  }
  dirname(desc_files[[1L]])
}

dcf_field <- function(desc, field) {
  if (!field %in% colnames(desc)) {
    return(NA_character_)
  }
  desc[1L, field]
}

list_compilable_sources <- function(src_dir) {
  if (!dir.exists(src_dir)) {
    return(character())
  }
  list.files(
    src_dir,
    pattern = "\\.(c|cc|cpp|f|f90)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

match_risky_linking_to <- function(linking_to, risky_deps) {
  if (is.na(linking_to) || !nzchar(trimws(linking_to))) {
    return(character())
  }
  pkgs <- trimws(strsplit(linking_to, ",", fixed = TRUE)[[1L]])
  # strip version constraints like "(>= 1.0)"
  pkgs <- sub("\\s*\\(.*\\)$", "", pkgs)
  intersect(pkgs, risky_deps)
}

grep_volatile_symbols <- function(src_dir, symbols) {
  files <- list.files(
    src_dir,
    pattern = "\\.(c|cc|cpp|cxx|h|hpp|hxx|f|f90|f95|inl)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L || length(symbols) == 0L) {
    return(character())
  }
  content <- unlist(lapply(files, function(f) {
    tryCatch(
      readLines(f, warn = FALSE),
      error = function(e) character()
    )
  }))
  hits <- vapply(
    symbols,
    function(sym) any(grepl(sprintf("\\b%s\\b", sym), content)),
    logical(1L)
  )
  symbols[hits]
}
