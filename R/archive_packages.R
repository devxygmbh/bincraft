#' Archive packages in CRAN-like repositories2
#' @template param-package_name
#' @template param-codename
#' @template param-arch
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-debug
#' @template param-local_output_dir_root
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#'
#' @importFrom stringr str_split
#' @importFrom utils available.packages tail
#' @importFrom gh gh
#' @export
#' @examples
#' \dontrun{
#' archive_package("AATtools", codename = "rhel9")
#' archive_package("adw", codename = "rhel8", arch = "amd64")
#' }
#'
archive_package <- function(
    package_name,
    codename = NULL,
    local_output_dir_root = ".",
    s3_endpoint,
    s3_region,
    s3_bucket,
    arch = NULL,
    debug = FALSE,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  if (debug) {
    message(sprintf("DEBUG archive_package: package_name: %s", package_name))
  }

  codename <- set_codename(codename)

  if (is.null(arch)) {
    local_arch <- Sys.info()[["machine"]]
    if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
      arch <- "arm64"
    } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
      arch <- "amd64"
    }
  }

  local_bin_dir <- set_bin_path(local_output_dir_root, codename)
  remote_bin_dir <- sprintf("%s/%s/%s/latest/src/contrib", s3_bucket, arch, codename)

  # suppress progressr output here
  progressr::handlers("void")
  # don't parallelise
  future::plan("sequential")

  files <- s3fs::s3_dir_ls(remote_bin_dir)

  foo <- lapply(package_name, function(pkgname) {
    cli::cli_h2("Archiving ({.pkg {pkgname}})")
    if (!s3fs::s3_dir_exists(sprintf("%s/Archive/%s", remote_bin_dir, pkgname))) {
      s3fs::s3_dir_create(sprintf("%s/Archive/%s", remote_bin_dir, pkgname))
    }
    all_versions <- grep(sprintf("/%s_", pkgname), files, value = TRUE)
    # only archive if more than one package exists in the root
    if (length(all_versions) > 1) {
      # get most recent version from CRAN

      last_version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1]]$commit$message, "version ")[[1]][2]
      # check if last version is available in repo
      if (any(grepl(sprintf("^%s_%s.tar.gz", package_name, last_version), all_versions))) {
        index <- which(grepl(sprintf("_%s.tar.gz", last_version), all_versions, fixed = TRUE))
        old_versions <- all_versions[-index]
      } else {
        # this often fails with
        # caused by error in `curl::curl_fetch_memory(url)`:
        # ! SSL peer certificate or SSH remote key was not OK: [crandb.r-pkg.org] SSL certificate problem: unable to get local issuer certificate
        versions <- insistently(
          ~
            rev(pak::pkg_history(pkgname)$Version),
          rate = retry_config, quiet = FALSE
        )()
        for (i in versions) {
          if (any(grepl(paste0("^", i, "$"), stringr::str_split(stringr::str_split(all_versions, "_", simplify = T)[, 2], ".tar.gz", simplify = TRUE)[, 1]))) {
            index <- which(grepl(sprintf("_%s.tar.gz", i), all_versions))
            old_versions <- all_versions[-index]
            break
          }
        }
      }
      cli::cli_alert("Archiving {.field {basename(old_versions)}}, keeping {.field {basename(all_versions[index])}}.")
      s3fs::s3_file_move(old_versions, sprintf("%s/Archive/%s/%s", remote_bin_dir, pkgname, basename(old_versions)), max_batch = fs::fs_bytes("300MB"), overwrite = TRUE)
      cli::cli_alert_success("Successfully archived package {.pkg {pkgname}}.")
    } else {
      cli::cli_alert("Skipping {.pkg {pkgname}} as only one package versions exists.")
    }
  })

  return(invisible(TRUE))
}

#' Create Meta/archive.rds for remotes package
#' @description
#' Inspired from <https://stackoverflow.com/questions/35584396/how-to-generate-meta-archive-rds-to-be-compatible-with-devtoolsinstall-version>
#' @param files Input files
#'
#' @importFrom data.table data.table tstrsplit as.data.table :=
#' @importFrom stats setNames
#' @export
write_archive_rds <- function(files) {
  # make R CMD Check happy
  row_name <- NULL
  package <- NULL
  file_path <- NULL

  dt <- data.table(file_path = basename(files))
  dt <- dt[grepl("\\.tar\\.gz$", file_path)]

  # split into package and version
  dt[, c("package", "version") := tstrsplit(sub("\\.tar\\.gz$", "", file_path), "_", fixed = TRUE)]

  # assign DF row names
  dt[, row_name := paste0(package, "/", package, "_", version, ".tar.gz")]

  # Group by package and create a list of data.tables
  result <- dt[, .(data_frame = list(as.data.table(setNames(list(row_name), c("row_name"))))), by = package]

  # Convert each grouped data.table to a data.frame and assign row names
  result_list <- lapply(result$data_frame, function(dt_group) {
    df <- as.data.frame(dt_group)
    rownames(df) <- df$row_name
    df[, 0] # Remove the column, leaving just row names
  })

  names(result_list) <- result$package

  return(result_list)
}
