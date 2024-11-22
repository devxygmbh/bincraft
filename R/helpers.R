#' Set codename for Linux distribution
#' @template param-codename
#' @export
set_codename <- function(codename) {
  if (is.null(codename)) {
    if (Sys.info()["sysname"] == "Linux") {
      if (any(grepl("alpine", system2("cat", args = c("/etc/os-release"), stdout = TRUE)))) {
        version <- system2("grep",
          args = c("'^VERSION_ID=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
        )
        version_stripped <- substr(gsub("\\.", "", version), 1, 3)
        codename <- paste0("alpine", version_stripped)
      } else {
        dist_fam <- system2("grep",
          args = c("'^ID_LIKE=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
        )
        if (dist_fam == "debian") {
          codename <- system2("grep",
            args = c("'^VERSION_CODENAME=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
          )
        } else if (grepl("rhel|fedora", dist_fam)) {
          platform_id <- system2("grep",
            args = c("'^PLATFORM_ID=' /etc/os-release | cut -d'=' -f2 | tr -d '\"'"), stdout = TRUE
          )
          if (platform_id == "platform:el9") {
            codename <- "rhel9"
          } else if (platform_id == "platform:el8") {
            codename <- "rhel8"
          }
        }
      }
    }
    return(codename)
  } else {
    return(codename)
  }
}

#' Set path for binary package outputs
#' @template param-codename
#' @template param-local_build_root
#' @export
set_bin_path <- function(local_build_root, codename) {
  local_arch <- Sys.info()[["machine"]]
  if (grepl("arm64", local_arch) || grepl("aarch64", local_arch)) {
    arch <- "arm64"
  } else if (grepl("amd64", local_arch) || grepl("x86_64", local_arch)) {
    arch <- "amd64"
  }

  path <- sprintf(
    "%s/%s/%s/latest/src/contrib",
    local_build_root, arch, codename
  )
  return(path)
}

#' Checks whether a binary for the latest package version exists
#' @template param-package_name
#' @template param-endpoint
#' @template param-region
#' @template param-bucket
#' @template param-codename
#' @template param-arch
#' @param version Version to check for. Only "latest" is supported right now.
#' @export
check_for_binary <- function(
    package_name,
    endpoint = "https://s3.eu-central-003.backblazeb2.com",
    region = "eu-central-003",
    bucket = "devxy-arm64-r-binaries",
    codename = NULL,
    arch = NULL,
    version = "latest") {
  s3fs::s3_file_system(
    aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
    aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    endpoint = endpoint,
    region_name = region,
  )
  codename <- set_codename(codename)
  remote_bin_path <- set_bin_path(local_build_root = bucket, codename)
  version <- strsplit(gh::gh(sprintf("GET /repos/cran/%s/commits", package_name))[[1]]$commit$message, "version ")[[1]][2]
  exists <- s3fs::s3_file_exists(sprintf("s3://%s/%s_%s.tar.gz", remote_bin_path, package_name, version))
  return(exists)
}
