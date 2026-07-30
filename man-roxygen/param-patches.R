#' @param patches Optional path to a patch registry directory containing a
#'   `registry.json` (and any referenced diff files). When set, matching
#'   packages are pre-built as patched binaries and installed into the build
#'   library before dependency installation. Defaults to `NULL` (no patching).
