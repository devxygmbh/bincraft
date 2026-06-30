# tools/verify-patch-mechanism.R
# Proves pak installs a patched binary from a prepended file:// repo instead of
# CRAN's, without recompiling. Run inside a Linux build-env container:
#   Rscript tools/verify-patch-mechanism.R
# Exits 0 on success, 1 on failure.

pkg <- "glue" # small, pure-R CRAN package
sentinel <- "PatchMechanismProof"

work <- tempfile("verify_")
repo <- file.path(work, "repo", "src", "contrib")
lib <- file.path(work, "lib")
dir.create(repo, recursive = TRUE)
dir.create(lib, recursive = TRUE)

# 1. Download CRAN source for the current version.
ap <- available.packages(repos = "https://cloud.r-project.org")
ver <- ap[pkg, "Version"]
src <- file.path(work, sprintf("%s_%s.tar.gz", pkg, ver))
download.file(
  sprintf("https://cloud.r-project.org/src/contrib/%s_%s.tar.gz", pkg, ver),
  src,
  mode = "wb"
)

# 2. Unpack, inject a sentinel field into DESCRIPTION, build a binary.
untar(src, exdir = work)
desc <- file.path(work, pkg, "DESCRIPTION")
writeLines(c(readLines(desc), sprintf("%s: yes", sentinel)), desc)
pkgbuild::build(
  file.path(work, pkg),
  binary = TRUE,
  vignettes = FALSE,
  dest_path = repo,
  quiet = TRUE
)
built <- list.files(
  repo,
  pattern = sprintf("^%s_.*\\.tar\\.gz$", pkg),
  full.names = TRUE
)
file.rename(built[1L], file.path(repo, sprintf("%s_%s.tar.gz", pkg, ver)))
cranlike::add_PACKAGES(sprintf("%s_%s.tar.gz", pkg, ver), repo)

# 3. Install with the local repo prepended; assert our patched build won.
withr::with_options(
  list(
    repos = c(
      patched = sprintf("file://%s", dirname(dirname(repo))),
      CRAN = "https://cloud.r-project.org"
    )
  ),
  pak::pkg_install(pkg, lib = lib, ask = FALSE, upgrade = FALSE)
)

installed_desc <- file.path(lib, pkg, "DESCRIPTION")
ok <- file.exists(installed_desc) &&
  any(grepl(sentinel, readLines(installed_desc)))

if (ok) {
  cat("PROOF PASSED: pak installed the patched local binary.\n")
  quit(status = 0L)
} else {
  cat("PROOF FAILED: pak did not install the patched local binary.\n")
  quit(status = 1L)
}
