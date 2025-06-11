#' Archive packages in CRAN-like repositories
#' @template param-package_name
#' @template param-codename
#' @template param-arch
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
#' @template param-is_debug
#' @template param-is_r_minor_sensitive
#' @template param-local_output_dir_root
#' @template param-s3-access-key-id
#' @template param-s3-secret-access-key
#' @importFrom utils available.packages tail
#' @importFrom purrr walk
#' @importFrom gh gh
#' @export
#' @examples
#' \dontrun{
#' archive_package("AATtools", codename = "rhel9")
#' archive_package("adw",
#'   codename = "rhel8", arch = "amd64",
#'   s3_endpoint = "https://hel1.your-objectstorage.com", s3_region = "hel1",
#'   s3_bucket = "devxy-r-package-binaries-hel1",
#'   s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
#'   s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S")
#' )
#' }
#'
archive_package <- function(
    package_name,
    s3_endpoint,
    s3_region,
    s3_bucket,
    codename = NULL,
    is_r_minor_sensitive = FALSE,
    local_output_dir_root = ".",
    arch = NULL,
    is_debug = FALSE,
    s3_access_key_id = NULL,
    s3_secret_access_key = NULL) {
  s3fs::s3_file_system(
    aws_access_key_id = s3_access_key_id,
    aws_secret_access_key = s3_secret_access_key,
    endpoint = s3_endpoint,
    region_name = s3_region,
    refresh = TRUE
  )

  if (is_debug) {
    message(sprintf("DEBUG archive_package: package_name: %s", package_name))
  }

  codename <- set_codename(codename)

  if (is.null(arch)) {
    local_arch <- Sys.info()[["machine"]]
    if (grepl("arm64", local_arch, fixed = TRUE) || grepl("aarch64", local_arch, fixed = TRUE)) {
      arch <- "arm64"
    } else if (grepl("amd64", local_arch, fixed = TRUE) || grepl("x86_64", local_arch, fixed = TRUE)) {
      arch <- "amd64"
    }
  }

  remote_bin_dir <- file.path(s3_bucket, arch, codename, "latest", "src", "contrib")

  # don't parallelise
  future::plan("sequential")

  if (!is_r_minor_sensitive) {
    files <- s3fs::s3_dir_ls(remote_bin_dir)
  } else {
    files <- s3fs::s3_dir_ls(file.path(remote_bin_dir, is_r_minor_sensitive))
  }

  if (!is_r_minor_sensitive) {
    remote_search_path <- file.path(remote_bin_dir, "Archive", package_name)
  } else {
    minor_version <- paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
    remote_search_path <- file.path(remote_bin_dir, minor_version, "Archive", package_name)
  }

  purrr::walk(package_name, function(pkgname) {
    cli::cli_h2("Archiving ({.pkg {pkgname}})")
    if (!is_r_minor_sensitive) {
      remote_search_path <- file.path(remote_bin_dir, "Archive", pkgname)
    } else {
      remote_search_path <- file.path(remote_bin_dir, minor_version, "Archive", pkgname)
    }
    if (
      !s3fs::s3_dir_exists(remote_search_path)
    ) {
      s3fs::s3_dir_create(remote_search_path)
    }
    all_versions <- grep(sprintf("/%s_", pkgname), files, value = TRUE)
    # only archive if more than one package exists in the root
    if (length(all_versions) > 1L) {
      # get most recent version from CRAN

      last_version <- strsplit(
        gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1L]]$commit$message, # nolint
        "version ",
        fixed = TRUE
      )[[1L]][2L]

      # check if last version is available in repo
      if (
        any(grepl(
          sprintf("%s_%s.tar.gz", package_name, last_version),
          all_versions
        ))
      ) {
        index <- grep(sprintf("_%s.tar.gz", last_version), all_versions, fixed = TRUE)
        old_versions <- all_versions[-index]
      } else {
        # this often fails with
        # caused by error in `curl::curl_fetch_memory(url)`:
        # ! SSL peer certificate or SSH remote key was not OK: [crandb.r-pkg.org]
        # SSL certificate problem: unable to get local issuer certificate
        versions <- purrr::insistently(
          ~ rev(pak::pkg_history(pkgname)$Version),
          rate = retry_config,
          quiet = FALSE
        )()
        for (i in versions) {
          if (
            any(grepl(
              paste0("^", i, "$"),
              vapply(strsplit(
                vapply(strsplit(all_versions, "_", fixed = TRUE), function(x) x[2L], character(1L)),
                ".tar.gz",
                fixed = TRUE
              ), function(x) x[1L], character(1L))
            ))
          ) {
            index <- grep(sprintf("_%s.tar.gz", i), all_versions)
            old_versions <- all_versions[-index]
            break
          }
        }
      }
      # account for duplicated (= faulty) packages
      if (anyDuplicated(s3fs::s3_file_info(old_versions)$key) > 0L) {
        for (i in old_versions) {
          if (anyDuplicated(s3fs::s3_file_info(i)$key)) {
            cli::cli_alert_danger("{.field {i}} is duplicated, deleting it.")
            s3fs::s3_file_delete(i)
            old_versions <- setdiff(old_versions, i)
          }
        }
      }
      if (length(old_versions) > 0L) {
        if (!is_r_minor_sensitive) {
          archive_path <- file.path(
            remote_bin_dir,
            "Archive",
            pkgname,
            basename(old_versions)
          )
        } else {
          archive_path <- file.path(
            remote_bin_dir,
            minor_version,
            "Archive",
            pkgname,
            basename(old_versions)
          )
        }
        cli::cli_alert(
          "Archiving {.field {basename(old_versions)}} to {.field {archive_path}}, keeping {.field {basename(all_versions[index])}}."
        )
        s3fs::s3_file_move(
          old_versions,
          archive_path,
          max_batch = parse_bytes("300MB"),
          overwrite = TRUE
        )
        cli::cli_alert_success(
          "Successfully archived package {.pkg {pkgname}}."
        )
      }
    } else {
      cli::cli_alert(
        "Skipping {.pkg {pkgname}} as only one package versions exists."
      )
    }
  })

  invisible(TRUE)
}

#' Create Meta/archive.rds for \{remotes\} package
#' @description
#' Inspired from <https://stackoverflow.com/questions/35584396/how-to-generate-meta-archive-rds-to-be-compatible-with-devtoolsinstall-version> # nolint
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

  dt_data <- data.table(file_path = basename(files))
  dt_data <- dt_data[endsWith(file_path, ".tar.gz")]

  # split into package and version
  dt_data[
    ,
    c("package", "version") := tstrsplit(
      sub("\\.tar\\.gz$", "", file_path),
      "_",
      fixed = TRUE
    )
  ]

  # assign DF row names
  dt_data[, row_name := paste0(package, "/", package, "_", version, ".tar.gz")]

  # Group by package and create a list of data.tables
  result <- dt_data[,
    .(
      data_frame = list(as.data.table(setNames(list(row_name), "row_name")))
    ),
    by = package
  ]

  # Convert each grouped data.table to a data.frame and assign row names
  result_list <- lapply(result$data_frame, function(dt_group) {
    df_data <- as.data.frame(dt_group)
    rownames(df_data) <- df_data$row_name
    df_data[, 0L] # Remove the column, leaving just row names
  })

  names(result_list) <- result$package

  result_list
}
