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
