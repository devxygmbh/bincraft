# Resolve CRAN package versions from CRAN's own metadata instead of the GitHub
# REST API. The GitHub `GET /repos/cran/{pkg}/tags` call bincraft used to make
# per package counts against the authenticated user's 5,000 requests/hour REST
# limit, which the weekly rebuild exhausted quickly. CRAN's `PACKAGES` index
# (current version) plus `Meta/archive.rds` (superseded versions) carry the same
# information with no GitHub involvement and no comparable rate limit.

# Session-lived cache for the (relatively large) CRAN indexes so a rebuild over
# thousands of packages parses them once, not once per package.
.cran_meta_cache <- new.env(parent = emptyenv())

#' Current CRAN source version of a package, from the live PACKAGES index
#' @keywords internal
cran_current_versions <- function(cran) {
  key <- paste0("avail:", cran)
  if (is.null(.cran_meta_cache[[key]])) {
    .cran_meta_cache[[key]] <- utils::available.packages(
      contriburl = utils::contrib.url(cran, type = "source"),
      # keep packages that require a newer R than the running one; we only want
      # the version string, not installability.
      filters = "duplicates"
    )
  }
  .cran_meta_cache[[key]]
}

#' Archived (superseded) versions of a package, newest-first, from archive.rds
#' @keywords internal
cran_archive_versions <- function(package_name, cran) {
  key <- paste0("archive:", cran)
  if (is.null(.cran_meta_cache[[key]])) {
    url <- sprintf("%s/src/contrib/Meta/archive.rds", cran)
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    .cran_meta_cache[[key]] <- readRDS(tmp)
  }
  entry <- .cran_meta_cache[[key]][[package_name]]
  if (is.null(entry)) {
    return(character())
  }
  # rownames look like "pkg/pkg_1.2.3.tar.gz"
  sub(
    sprintf("^%s/%s_(.*)\\.tar\\.gz$", package_name, package_name),
    "\\1",
    rownames(entry)
  )
}

#' Resolve the most recent CRAN version(s) of a package from CRAN metadata
#'
#' Drop-in replacement for a GitHub tag lookup on CRAN packages. Returns the
#' current version (from `PACKAGES`) plus, when more than one is requested,
#' archived versions (from `Meta/archive.rds`), newest first.
#'
#' @param package_name CRAN package name.
#' @param tag_limit Maximum number of versions to return, newest first.
#' @param cran CRAN mirror base URL.
#' @return Character vector of version strings (length `<= tag_limit`), or an
#'   empty character vector when CRAN knows nothing about the package.
#' @keywords internal
fetch_cran_versions <- function(
  package_name,
  tag_limit = 1L,
  cran = "https://cloud.r-project.org"
) {
  versions <- character()

  current <- tryCatch(
    {
      db <- cran_current_versions(cran)
      if (package_name %in% rownames(db)) {
        unname(db[package_name, "Version"])
      } else {
        NA_character_
      }
    },
    error = function(e) NA_character_
  )
  if (!is.na(current)) {
    versions <- current
  }

  if (tag_limit > 1L) {
    archived <- tryCatch(
      cran_archive_versions(package_name, cran),
      error = function(e) character()
    )
    versions <- unique(c(versions, archived))
  }

  if (length(versions) == 0L) {
    return(character())
  }

  ord <- tryCatch(
    order(numeric_version(versions), decreasing = TRUE),
    error = function(e) seq_along(versions)
  )
  utils::head(versions[ord], tag_limit)
}

#' Is this source URL a CRAN mirror (so versions come from CRAN, not a forge)?
#' @keywords internal
is_cran_source <- function(source_org_url) {
  identical(tolower(basename(source_org_url)), "cran") ||
    grepl(
      "cloud\\.r-project\\.org|cran\\.r-project\\.org",
      source_org_url,
      ignore.case = TRUE
    )
}
