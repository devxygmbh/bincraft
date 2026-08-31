#' Construct a `Built` field value for the current build environment
#'
#' cranlike derives package metadata from the CRAN *source* `DESCRIPTION`, which
#' never carries a `Built:` field (R stamps that only when it builds a binary).
#' Our S3 tarballs *are* binaries, so we pass this stamp to
#' [cranlike::update_PACKAGES()]/[cranlike::add_PACKAGES()] to advertise them as
#' such. Binary-aware clients (e.g. `uvr`) key their binary-vs-source decision on
#' the `Built:` platform triple + R minor; without it they treat the repo as
#' source-only and compile everything, which needs system `-dev` libraries.
#'
#' The triple comes from `R.version$platform` and the version from
#' [getRversion()], so this must run in the same build environment that produced
#' the binaries (the surrounding arch/codename logic already assumes this).
#'
#' @keywords internal
built_stamp <- function(
  platform = R.version$platform,
  r_version = getRversion(),
  time = Sys.time()
) {
  check_stamp_field(platform, "platform")
  check_stamp_field(r_version, "r_version")

  sprintf(
    "R %s; %s; %s UTC; unix",
    r_version,
    platform,
    format(as.POSIXlt(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  )
}

#' Reject a `Built` component that must not reach the index
#'
#' The stamp is written verbatim into *every* entry of a slot's `PACKAGES`
#' index, and `uvr` decides binary-vs-source by matching the triple it carries.
#' A missing value therefore does not degrade gracefully: it stamps the literal
#' `"NA"`, no client matches it, and the whole slot silently reverts to
#' source-only until the next full re-index. Failing the index write is the far
#' cheaper outcome, so refuse anything that is not a single non-empty value.
#'
#' @keywords internal
#' @noRd
check_stamp_field <- function(value, name) {
  usable <- length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(as.character(value)))
  if (!usable) {
    stop(
      sprintf(
        "{.function built_stamp}: `%s` must be a single non-empty value, got '%s'. Refusing to stamp an unusable `Built` field into the index.",
        name,
        paste(format(value), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Build the S3 remote contrib dir for a package index
#'
#' @param r_minor Optional `"major.minor"` string (e.g. `"4.4"`). When non-NULL
#'   the path points at the per-minor slot.
#' @keywords internal
package_index_remote_dir <- function(
  s3_bucket,
  arch,
  codename,
  r_minor = NULL
) {
  base <- file.path(s3_bucket, arch, codename, "latest", "src", "contrib")
  if (is.null(r_minor)) {
    base
  } else {
    file.path(base, r_minor)
  }
}

#' Merge the generic slot's records into a per-minor index
#'
#' A per-minor slot holds only the ABI-sensitive packages, so on its own it is
#' not a usable repository: a client pointed at it loses every other package.
#' The index therefore has to carry both slots.
#'
#' Which directory a tarball comes from is decided inside the index, not by the
#' request path: `available.packages()` keeps the `contriburl` it *asked for*,
#' not the one a redirect served it, and folds a record's `Path` field into the
#' `Repository` column. Per-minor records get `Path = <r_minor>` so their
#' tarballs resolve into `…/src/contrib/<r_minor>/`; generic records get no
#' `Path` and resolve into `…/src/contrib/`. Nothing is copied or duplicated.
#'
#' A package built for this minor shadows the generic one entirely, including
#' its older versions, so a client never sees two provenances for one package.
#'
#' @param minor_records,flat_records Character matrices of index records, as
#'   stored in a slot's `PACKAGES.rds`.
#' @param r_minor `"major.minor"` string, e.g. `"4.5"`.
#'
#' @return A character matrix of both slots' records, with a `Path` column.
#' @keywords internal
#' @noRd
union_index_records <- function(minor_records, flat_records, r_minor) {
  usable_minor <- length(r_minor) == 1L &&
    !is.na(r_minor) &&
    grepl("^[0-9]+\\.[0-9]+$", r_minor)
  if (!usable_minor) {
    stop(
      sprintf(
        "{.function union_index_records}: `r_minor` must be a single \"major.minor\" string, got '%s'.",
        paste(format(r_minor), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(flat_records) == 0L) {
    stop(
      "{.function union_index_records}: the generic index is empty. Refusing to publish a per-minor index that would hide every package the generic slot carries.",
      call. = FALSE
    )
  }

  columns <- union(
    union(colnames(flat_records), colnames(minor_records)),
    "Path"
  )

  align <- function(records) {
    out <- matrix(
      NA_character_,
      nrow(records),
      length(columns),
      dimnames = list(NULL, columns)
    )
    if (nrow(records) > 0L) {
      out[, colnames(records)] <- records
    }
    out
  }

  minor <- align(minor_records)
  flat <- align(flat_records)

  if (nrow(minor) > 0L) {
    minor[, "Path"] <- r_minor
  }

  # A per-minor entry only earns the right to hide the generic one when it is
  # actually a binary. When the per-minor build fell back to source but the
  # generic slot holds a binary built under this very minor, that binary is the
  # better artifact by every measure: same ABI, no compile on the client.
  # Steering to the source there would make a working install slower for no
  # correctness gain.
  #
  # This deliberately does not apply when the generic binary was built under a
  # *different* minor. For an ABI-risky package that is the load-time failure
  # the per-minor slots exist to prevent, so the source fallback stays.
  # `Built` is absent from both inputs when neither slot carries a binary, so
  # read it defensively rather than widening the output schema.
  built_of <- function(records) {
    if ("Built" %in% colnames(records)) {
      records[, "Built"]
    } else {
      rep(NA_character_, nrow(records))
    }
  }

  minor_built <- built_of(minor)
  flat_built <- built_of(flat)

  minor_is_source <- is.na(minor_built) | !nzchar(minor_built)
  flat_matches_minor <- !is.na(flat_built) &
    nzchar(flat_built) &
    sub("^R ([0-9]+\\.[0-9]+).*$", "\\1", flat_built) == r_minor

  demoted <- minor[minor_is_source, "Package", drop = TRUE]
  promoted <- flat[, "Package"] %in% demoted & flat_matches_minor

  minor <- minor[
    !(minor[, "Package"] %in% flat[promoted, "Package"]),
    ,
    drop = FALSE
  ]

  shadowed <- flat[, "Package"] %in% minor[, "Package"]
  rbind(minor, flat[!shadowed, , drop = FALSE])
}

#' Rewrite a slot's index files from a set of records
#'
#' Operates on the `PACKAGES*` files [cranlike::update_PACKAGES()] just wrote
#' locally, before they are uploaded, so a failure here leaves the slot's
#' published index untouched.
#'
#' All three files are rewritten together: clients disagree about which one to
#' fetch (`available.packages()` prefers `PACKAGES.rds`, `uvr` reads the text),
#' and a slot whose index files disagree serves a different repository
#' depending on the client.
#'
#' @param dir Directory holding the freshly written `PACKAGES*` files.
#' @param records Character matrix of index records to write.
#'
#' @return The number of records written, invisibly.
#' @keywords internal
#' @noRd
write_index_files <- function(dir, records) {
  write.dcf(records, file.path(dir, "PACKAGES"))

  gz <- gzfile(file.path(dir, "PACKAGES.gz"), open = "wb")
  on.exit(close(gz), add = TRUE)
  write.dcf(records, gz)

  # cranlike writes the rds xz-compressed; match it so the file it replaces and
  # the file we write are interchangeable.
  saveRDS(records, file.path(dir, "PACKAGES.rds"), compress = "xz")

  invisible(nrow(records))
}

#' Read a slot's published index
#'
#' @param remote_dir S3 directory of the slot, as built by
#'   [package_index_remote_dir()].
#'
#' @return A character matrix of index records.
#' @keywords internal
#' @noRd
read_remote_index <- function(remote_dir) {
  local_copy <- tempfile(fileext = ".rds")
  on.exit(unlink(local_copy), add = TRUE)
  s3fs::s3_file_download(file.path(remote_dir, "PACKAGES.rds"), local_copy)
  readRDS(local_copy)
}

#' Add package to repository index
#' @template param-package_name
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-local_output_dir_root
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @param r_minor Optional `"major.minor"` string. When set, the index is
#'   written/read under the per-minor slot `…/contrib/<r_minor>/` instead of the
#'   generic `…/contrib/` slot.
#'
#' @importFrom cranlike add_PACKAGES
#' @importFrom s3fs s3_dir_ls s3_file_system
#' @export
add_to_package_index <- function(
  s3_endpoint,
  s3_region,
  s3_bucket,
  package_name = NULL,
  local_output_dir_root = file.path("mnt", "cache", "binaries"),
  codename = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  r_minor = NULL
) {
  codename <- set_codename(codename)

  local_arch <- Sys.info()[["machine"]]
  if (
    grepl("arm64", local_arch, fixed = TRUE) ||
      grepl("aarch64", local_arch, fixed = TRUE)
  ) {
    arch <- "arm64"
  } else if (
    grepl("amd64", local_arch, fixed = TRUE) ||
      grepl("x86_64", local_arch, fixed = TRUE)
  ) {
    arch <- "amd64"
  }

  local_bin_dir <- set_bin_path(local_output_dir_root, codename)
  remote_bin_dir <- package_index_remote_dir(s3_bucket, arch, codename, r_minor)

  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  # get latest PACKAGES file from S3
  if (!s3fs::s3_file_exists(file.path(remote_bin_dir, "PACKAGES"))) {
    s3fs::s3_file_download(
      file.path(remote_bin_dir, "PACKAGES"),
      file.path(local_bin_dir, "PACKAGES")
    )
  }
  file_names <- list.files(
    local_bin_dir,
    pattern = sprintf("%s*", package_name)
  )

  # list all tarballs for the given package
  cranlike::add_PACKAGES(file_names, local_bin_dir, built = built_stamp())

  invisible(TRUE)
}

#' Upload package index files to S3
#' @template param-package_name
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-local_output_dir_root
#' @template param-arch
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @param r_minor Optional `"major.minor"` string. When set, the index is
#'   written/read under the per-minor slot `…/contrib/<r_minor>/` instead of the
#'   generic `…/contrib/` slot.
#'
#' @importFrom s3fs s3_file_upload s3_dir_ls
#' @importFrom cranlike update_PACKAGES
#' @export
upload_package_index <- function(
  s3_endpoint,
  s3_region,
  s3_bucket,
  package_name = NULL,
  local_output_dir_root = ".",
  codename = NULL,
  arch = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  r_minor = NULL
) {
  log_info("Updating PACKAGES* files in S3.")

  codename <- set_codename(codename)

  if (is.null(arch)) {
    local_arch <- Sys.info()[["machine"]]
    if (
      grepl("arm64", local_arch, fixed = TRUE) ||
        grepl("aarch64", local_arch, fixed = TRUE)
    ) {
      arch <- "arm64"
    } else if (
      grepl("amd64", local_arch, fixed = TRUE) ||
        grepl("x86_64", local_arch, fixed = TRUE)
    ) {
      arch <- "amd64"
    }
  }

  remote_bin_dir <- package_index_remote_dir(s3_bucket, arch, codename, r_minor)

  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  log_info("Started listing remote packages")
  pkgs <- s3fs::s3_dir_ls(remote_bin_dir)
  log_success("Finished listing remote packages")
  # We remove 4 from the count as we don't want to count the PACKAGES* files + Archive/ dir
  pkg_count <- length(pkgs) - 5L
  unique_pkgs <- length(unique(vapply(
    strsplit(basename(pkgs), "_", fixed = TRUE),
    function(x) x[1L],
    character(1L)
  ))) -
    5L

  t1 <- Sys.time()
  built <- built_stamp()
  retry_s3_operation(
    function() {
      cranlike::update_PACKAGES(
        sprintf("s3://%s", remote_bin_dir),
        built = built
      )
    },
    label = "update PACKAGES"
  )

  # Post-process the index cranlike just wrote, before it is uploaded, so a
  # failure here leaves the published index untouched.
  records <- readRDS("PACKAGES.rds")

  # Clear the stamp before the union, not after. `built_stamp()` is applied to
  # the whole slot, so it also lands on packages whose build failed and were
  # published as their CRAN source. Advertising a source tarball as a binary
  # makes `uvr` install it without the system `-dev` libraries a source build
  # needs.
  #
  # The ordering matters beyond that: `union_index_records()` decides whether a
  # per-minor record may hide a generic binary by asking whether it is a binary
  # at all, and it reads `Built` to answer. Clearing afterwards would leave
  # every source fallback looking like a binary at merge time, and the union
  # would keep steering clients to sources.
  before <- sum(!is.na(records[, "Built"]))
  records <- clear_built_for_sources(records)
  cleared <- before - sum(!is.na(records[, "Built"]))
  if (cleared > 0L) {
    log_info(sprintf(
      "Cleared the {.field Built} stamp on %s CRAN-source records (failed builds served as source).",
      cleared
    ))
  }

  # A per-minor slot carries only the ABI-sensitive packages, so its index is
  # republished as a union with the generic slot. This is deliberately fatal:
  # leaving the previously published union in place is far better than replacing
  # it with an index that hides most of the repository.
  if (!is.null(r_minor)) {
    records <- union_index_records(
      records,
      flat_records = read_remote_index(
        package_index_remote_dir(s3_bucket, arch, codename)
      ),
      r_minor = r_minor
    )
    log_success(sprintf(
      "Merged the generic slot into the {.field %s} index: %s records.",
      r_minor,
      nrow(records)
    ))
  }

  write_index_files(".", records)

  # write Meta/archive.rds for remotes::install_version
  log_success(
    sprintf(
      "Started creating/updating {.path %s}",
      file.path("Meta", "archive.rds")
    )
  )
  retry_s3_operation(
    function() {
      files <- s3fs::s3_dir_ls(
        file.path(remote_bin_dir, "Archive"),
        recurse = TRUE,
        regexp = "*.tar.gz"
      )
      archive_rds <- write_archive_rds(files)
      tmp <- tempfile()
      saveRDS(archive_rds, tmp)
      s3fs::s3_file_upload(
        tmp,
        file.path(remote_bin_dir, "Meta", "archive.rds"),
        overwrite = TRUE,
        CacheControl = "no-store"
      )
    },
    label = file.path("update Meta", "archive.rds")
  )
  log_success(
    sprintf(
      "Successfully uploaded {.path %s}",
      file.path("Meta", "archive.rds")
    )
  )

  total_build_time <- round(Sys.time() - t1, 2L)
  time_units <- units(difftime(Sys.time(), t1))
  log_info(sprintf(
    "Time updating PACKAGES index for %s (%s unique) packages: %s %s.",
    pkg_count,
    unique_pkgs,
    total_build_time,
    time_units
  ))

  purrr::walk2(
    c("PACKAGES", "PACKAGES.db", "PACKAGES.rds", "PACKAGES.gz"),
    file.path(
      remote_bin_dir,
      c("PACKAGES", "PACKAGES.db", "PACKAGES.rds", "PACKAGES.gz")
    ),
    \(x, y) {
      s3fs::s3_file_upload(x, y, overwrite = TRUE, CacheControl = "no-store")
    }
  )

  log_success(
    "Successfully uploaded {.path PACKAGES}, {.path PACKAGES.db}, {.path PACKAGES.rds}, {.path PACKAGES.gz}"
  )

  invisible(TRUE)
}
