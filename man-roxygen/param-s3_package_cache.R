#' @param s3_package_cache ([character] or `NULL`)\cr
#' Optional pre-fetched character vector of S3 package filenames (basenames like
#' `"pkg_1.0.0.tar.gz"`). When provided, S3 existence checks use in-memory set
#' lookup instead of per-package S3 API calls. Obtain via
#' `basename(s3fs::s3_dir_ls(..., recurse = TRUE))`.
