#' Upload binary to S3
#' @template param-package_name
#' @template param-tag
#' @template param-codename
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-is_r_minor_sensitive
#' @template param-local_output_dir_root
#' @template param-force
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom s3fs s3_file_exists s3_file_upload s3_file_system
#' @export
upload_single_binary <- function(
  package_name,
  tag,
  s3_endpoint,
  s3_region,
  s3_bucket,
  local_output_dir_root = ".",
  codename = NULL,
  force = FALSE,
  is_r_minor_sensitive = FALSE,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL
) {
  codename <- set_codename(codename)

  log_header(sprintf("Uploading ({.pkg %s})", package_name[1L]))

  local_bin_path <- set_bin_path(
    local_output_dir_root = local_output_dir_root,
    codename
  )
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)

  tarball_name <- sprintf("%s_%s.tar.gz", package_name, tag)
  local_tarball_path <- file.path(local_bin_path, tarball_name)

  if (!file.exists(local_tarball_path)) {
    log_info(
      sprintf(
        "{.fun upload_single_binary}: File {.pkg %s} {.field %s} does not exist locally - skipping upload.",
        package_name,
        tag
      )
    )
    return(TRUE)
  }

  log_debug(sprintf("DEBUG: local_bin_path: %s", local_bin_path))
  log_debug(sprintf("DEBUG: remote_bin_path: %s", remote_bin_path))

  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  # The caller's verdict is a source-level guess made before this binary
  # existed. Now that it does, the question is decidable: a package fails at
  # dyn.load() under an R minor exactly when its shared objects reference a
  # symbol that minor does not export.
  #
  # The guess is wrong in both directions. `LinkingTo: Rcpp` alone flagged 93%
  # of compiled packages on amd64/resolute, while the curated symbol list missed
  # SET_FORMALS, SET_CLOENV, SET_TRUELENGTH and the ALTREP accessors, so
  # packages using those were written to the flat slot and served across minors.
  # That is how base64enc reached R 4.6 clients.
  #
  # Only an actual inspection overrides the caller. Without `nm`, without an
  # /opt/R tree to diff, or on any parse failure the verdict is `inspected =
  # FALSE` and the caller's decision stands unchanged.
  verdict <- tarball_abi_verdict(local_tarball_path)
  if (
    isTRUE(verdict$inspected) &&
      !identical(verdict$sensitive, is_r_minor_sensitive)
  ) {
    log_info(sprintf(
      "{.fun upload_single_binary}: {.pkg %s} {.field %s}: symbol inspection says r_minor_sensitive=%s, caller said %s.%s",
      package_name,
      tag,
      verdict$sensitive,
      is_r_minor_sensitive,
      if (length(verdict$symbols) > 0L) {
        sprintf(
          " Volatile symbols referenced: %s (absent from R %s).",
          toString(utils::head(verdict$symbols, 5L)),
          toString(verdict$unsupported)
        )
      } else {
        " No volatile symbols referenced."
      }
    ))
    is_r_minor_sensitive <- verdict$sensitive
  }

  # Write the verdict down when the object is going to the flat slot. The index
  # build cannot inspect 20k tarballs, so without a record it must assume every
  # risky package's flat record is unsafe and drop it, which costs roughly 2000
  # packages per per-minor index. A recorded `safe` lets it keep exactly the
  # ones that provably load anywhere.
  if (isTRUE(verdict$inspected) && !is_r_minor_sensitive) {
    flat_safety_store(
      package_name,
      tag,
      codename,
      detect_arch(),
      safe = TRUE,
      unsupported = verdict$unsupported
    )
  }

  if (is_r_minor_sensitive) {
    minor_version <- paste(
      R.version$major,
      strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L],
      sep = "."
    )
    remote_tarball_path <- file.path(
      remote_bin_path,
      minor_version,
      tarball_name
    )
    archive_path <- file.path(
      remote_bin_path,
      minor_version,
      "Archive",
      package_name,
      tarball_name
    )
  } else {
    remote_tarball_path <- file.path(remote_bin_path, tarball_name)
    archive_path <- file.path(
      remote_bin_path,
      "Archive",
      package_name,
      tarball_name
    )
  }

  # A source fallback occupies the binary's key, so "the key is taken" would
  # refuse to publish the very binary that replaces it. Only a published binary
  # blocks the upload; a source has to be overwritten.
  root_state <- remote_object_state(remote_tarball_path, package_name, tag)
  archive_state <- remote_object_state(archive_path, package_name, tag)
  binary_exists <- any(c(root_state, archive_state) == "binary")
  replacing_source <- identical(root_state, "source")

  # don't parallelise
  future::plan("sequential")

  should_upload <- !binary_exists || force

  if (should_upload) {
    if (replacing_source) {
      log_info(
        sprintf(
          "{.fun upload_single_binary}: Replacing the CRAN source published for {.pkg %s} {.field %s} at {.path %s} with the binary.",
          package_name,
          tag,
          remote_tarball_path
        )
      )
    } else if (binary_exists && force) {
      log_info(
        sprintf(
          "{.fun upload_single_binary}: Force uploading package {.pkg %s} {.field %s} to {.path %s} because {.code force = TRUE} was set.",
          package_name,
          tag,
          remote_tarball_path
        )
      )
    } else {
      log_info(
        sprintf(
          "{.fun upload_single_binary}: Uploading {.pkg %s} {.field %s} to {.path %s}.",
          package_name,
          tag,
          remote_tarball_path
        )
      )
    }

    upload_args <- list(
      local_tarball_path,
      remote_tarball_path
    )

    # Overwrite whenever something already occupies the key: a forced re-upload
    # of a binary, or the source fallback this binary replaces.
    if (replacing_source || (binary_exists && force)) {
      upload_args$max_batch <- parse_bytes("300MB")
      upload_args$overwrite <- TRUE
    }

    do.call(s3fs::s3_file_upload, upload_args)

    if (!s3fs::s3_file_exists(remote_tarball_path)) {
      log_warn(sprintf(
        "{.fun upload_single_binary}: Upload of {.pkg %s} {.field %s} to {.path %s} could not be confirmed in S3.",
        package_name,
        tag,
        remote_tarball_path
      ))
      return(FALSE)
    }

    log_success(sprintf(
      "Successfully uploaded package {.pkg %s} with tag {.field %s}.",
      package_name,
      tag
    ))
    log_info(
      sprintf(
        "{.fun upload_single_binary}: Deleting binary for {.pkg %s} {.field %s} at path {.path %s}.",
        package_name,
        tag,
        local_tarball_path
      )
    )
    file.remove(local_tarball_path)
    return(TRUE)
  } else {
    log_info(
      sprintf(
        "{.fun upload_single_binary}: Package {.pkg %s} {.field %s} already exists in S3. Skipping upload.",
        package_name,
        tag
      )
    )
    return(TRUE)
  }
}

#' Uploads source tarballs to S3
#' @template param-package_name
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-codename
#' @template param-is_r_minor_sensitive
#' @template param-arch
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom utils download.file
#' @export
upload_source_tarball <- function(
  package_name,
  s3_endpoint,
  s3_region,
  s3_bucket,
  codename = NULL,
  arch = NULL,
  is_r_minor_sensitive = FALSE,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL
) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_output_dir_root = s3_bucket, codename)
  version <- cran_mirror_version(package_name)
  if (is.na(version)) {
    stop(
      sprintf(
        "The cran mirror has no repository for '%s', so its published version cannot be read.",
        package_name
      ),
      call. = FALSE
    )
  }

  tmpfile <- tempfile()
  # this can fail, e.g. if there was a new package published and shortly
  # after removed by CRAN again due to some hickups.
  # To account for it, we retry the download 3 times and then abort with a message
  # that does not let the whole process to be stopped with an error
  download_url <- sprintf(
    "https://cloud.r-project.org/src/contrib/%s_%s.tar.gz",
    package_name,
    version
  )
  download_successful <- FALSE
  tryCatch(
    {
      # Call the insistent function
      insistent_downloader(url = download_url, destfile = tmpfile)
      # If insistently succeeds without error, set flag to TRUE
      download_successful <- TRUE
      TRUE # Return TRUE from the tryCatch block on success
    },
    error = function(e) {
      # This block executes only if insistently gives up after all retries
      warning(
        sprintf(
          "Failed to download %s after %d retries: %s. Skipping this package.",
          basename(download_url),
          3L,
          conditionMessage(e) # Display the final error message
        ),
        call. = FALSE
      )
      # Set flag to FALSE and return FALSE from the tryCatch block
      download_successful <- FALSE
      FALSE
    }
  )
  if (!download_successful) {
    log_warn(
      sprintf(
        "Failure downloading source tarball for package %s (%s)",
        package_name,
        version
      )
    )
    return(TRUE)
  }

  if (is_r_minor_sensitive) {
    minor_version <- paste(
      R.version$major,
      strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L],
      sep = "."
    )
    upload_path <- sprintf(
      "s3://%s/%s/%s_%s.tar.gz",
      remote_bin_path,
      minor_version,
      package_name,
      version
    )
  } else {
    upload_path <- sprintf(
      "s3://%s/%s_%s.tar.gz",
      remote_bin_path,
      package_name,
      version
    )
  }

  s3fs::s3_file_upload(tmpfile, upload_path, overwrite = TRUE)

  log_success(sprintf(
    "Successfully uploaded source tarball for package %s %s to %s.",
    package_name,
    version,
    upload_path
  ))
}
