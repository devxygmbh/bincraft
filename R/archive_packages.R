#' Archive packages in CRAN-like repositories
#' @template param-package_name
#' @template param-codename
#' @template param-arch
#' @template param-s3_endpoint
#' @template param-s3_region
#' @template param-s3_bucket
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

  log_debug(sprintf("archive_package: package_name: %s", package_name))

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

  if (is_r_minor_sensitive) {
    files <- s3fs::s3_dir_ls(file.path(remote_bin_dir, is_r_minor_sensitive))
  } else {
    files <- s3fs::s3_dir_ls(remote_bin_dir)
  }

  minor_version <- NULL
  if (is_r_minor_sensitive) {
    minor_version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  }

  for (pkg in package_name) {
    archive_single_package( # nolint: object_usage_linter
      pkg, remote_bin_dir, is_r_minor_sensitive, minor_version, files
    )
  }

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
