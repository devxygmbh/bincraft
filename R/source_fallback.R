#' Recognising a package that fell back to its CRAN source
#'
#' When a package fails to build, `handle_post_build_actions()` publishes the
#' CRAN *source* tarball in its place so the package stays installable. That
#' object is byte-identical to CRAN's, which is what lets us recognise it later:
#' comparing an object's MD5 against CRAN's published `MD5sum` needs no
#' downloads and no extra bookkeeping.
#'
#' Two things depend on telling the two apart:
#'
#' - a source fallback is not a binary, so it must keep counting as missing, or
#'   it is never retried and never reported (`check_for_binary()`)
#' - a source fallback must not be stamped with a `Built` field, or clients that
#'   trust the stamp compile it while believing they fetched a binary
#'   (`clear_built_for_sources()`)
#'
#' @name source_fallback
NULL

# One CRAN index per session: the table is ~24k rows and every caller wants the
# same snapshot.
.cran_source_md5_cache <- new.env(parent = emptyenv())

#' MD5 sums of CRAN's current source tarballs, keyed `<package>_<version>`
#'
#' @param cran CRAN mirror to read `src/contrib/PACKAGES.gz` from.
#' @param refresh Re-read the index even if it is already cached.
#'
#' @return A named character vector of MD5 sums. Empty if CRAN is unreachable:
#'   callers must treat "unknown" as "not a source fallback", so an outage
#'   cannot mass-flag a repository.
#' @keywords internal
#' @noRd
cran_source_md5 <- function(
  cran = "https://cloud.r-project.org",
  refresh = FALSE
) {
  cached <- .cran_source_md5_cache[[cran]]
  if (!refresh && !is.null(cached)) {
    return(cached)
  }

  table <- tryCatch(
    {
      con <- gzcon(url(
        sprintf("%s/src/contrib/PACKAGES.gz", sub("/$", "", cran)),
        open = "rb"
      ))
      on.exit(close(con), add = TRUE)
      db <- read.dcf(con, fields = c("Package", "Version", "MD5sum"))
      keep <- !is.na(db[, "MD5sum"])
      stats::setNames(
        as.character(db[keep, "MD5sum"]),
        paste(db[keep, "Package"], db[keep, "Version"], sep = "_")
      )
    },
    error = function(e) {
      log_warn(sprintf(
        "{.fun cran_source_md5}: could not read CRAN's index (%s). Treating every object as a build.",
        conditionMessage(e)
      ))
      stats::setNames(character(), character())
    }
  )

  .cran_source_md5_cache[[cran]] <- table
  table
}

#' Is this object CRAN's source tarball rather than a build of it?
#'
#' @param package,version,md5 Equal-length vectors describing the objects.
#' @param md5_table Lookup from [cran_source_md5()].
#'
#' @return Logical vector. `FALSE` whenever the answer is unknown -- an
#'   unmatched version (an archived one, say) or a missing MD5 must not be
#'   mistaken for a source fallback.
#' @keywords internal
#' @noRd
is_cran_source_tarball <- function(
  package,
  version,
  md5,
  md5_table = cran_source_md5()
) {
  expected <- unname(md5_table[paste(package, version, sep = "_")])
  !is.na(expected) & !is.na(md5) & tolower(expected) == tolower(md5)
}

#' Drop the `Built` field from records that are CRAN sources
#'
#' The stamp is applied to a whole slot at once, so it lands on the source
#' fallbacks too and advertises them as binaries. `uvr` then installs one
#' without the system `-dev` libraries a source build needs, and paquetier
#' files it under a platform it was never built for.
#'
#' Older versions are left alone: CRAN's index carries only current releases, so
#' an archived version cannot be checked and is assumed to be a build.
#'
#' @param records Character matrix of index records.
#' @param md5_table Lookup from [cran_source_md5()].
#'
#' @return `records`, with `Built` set to `NA` on the source fallbacks.
#' @keywords internal
#' @noRd
clear_built_for_sources <- function(records, md5_table = cran_source_md5()) {
  required <- c("Package", "Version", "MD5sum", "Built")
  if (nrow(records) == 0L || !all(required %in% colnames(records))) {
    return(records)
  }

  sources <- is_cran_source_tarball(
    records[, "Package"],
    records[, "Version"],
    records[, "MD5sum"],
    md5_table
  )
  records[sources, "Built"] <- NA_character_
  records
}

#' What is actually published at a remote path?
#'
#' Every gate in the build path used to ask "does an object exist here", and a
#' package whose build failed has its CRAN source published under exactly the
#' binary's name. Those gates need three answers, not two: an object that is a
#' source fallback must not block a build, and must be *replaced* rather than
#' left in place when the rebuild succeeds.
#'
#' @param path `s3://` path.
#' @param package,version The package and version the path should hold.
#'
#' @return One of `"absent"`, `"source"` or `"binary"`. `"binary"` is the answer
#'   whenever the object exists but cannot be shown to be CRAN's source, so an
#'   unreadable ETag or an unreachable CRAN can never mass-schedule rebuilds.
#' @keywords internal
#' @noRd
remote_object_state <- function(path, package, version) {
  if (!s3fs::s3_file_exists(path)) {
    return("absent")
  }

  md5 <- remote_object_md5(path)
  if (is.na(md5)) {
    return("binary")
  }

  if (isTRUE(is_cran_source_tarball(package, version, md5))) {
    "source"
  } else {
    "binary"
  }
}

#' MD5 of a remote object, from its ETag
#'
#' @param path `s3://` path.
#'
#' @return The MD5, or `NA` when it cannot be established -- a multipart upload
#'   carries a compound ETag rather than an MD5.
#' @keywords internal
#' @noRd
remote_object_md5 <- function(path) {
  info <- tryCatch(s3fs::s3_file_info(path), error = function(e) NULL)

  # s3fs snake-cases the head_object response and renames `e_tag` to `etag`,
  # so the column is not the `ETag` that S3 itself returns. Reading the wrong
  # name yields NULL rather than an error, which silently turns every object
  # into "cannot tell, assume binary" -- accept either spelling.
  etag <- NULL
  for (column in c("etag", "e_tag", "ETag")) {
    if (!is.null(info) && column %in% names(info)) {
      etag <- info[[column]]
      break
    }
  }

  if (length(etag) == 0L || is.na(etag[[1L]])) {
    return(NA_character_)
  }

  etag <- gsub('"', "", as.character(etag[[1L]]), fixed = TRUE)
  if (grepl("-", etag, fixed = TRUE)) {
    return(NA_character_)
  }
  etag
}
