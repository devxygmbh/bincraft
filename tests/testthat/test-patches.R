# tests/testthat/test-patches.R
test_that("load_patch_registry parses and normalizes entries", {
  dir <- withr::local_tempdir()
  writeLines("--- a patch ---", file.path(dir, "fix.patch"))
  jsonlite::write_json(
    list(list(
      package = "RcppParallel",
      versions = "*",
      platforms = list("alpine", "ubuntu-2604"),
      env = list(RCPP_PARALLEL_USE_TBB = "0"),
      patch = "fix.patch",
      reason = "bundled TBB fails"
    )),
    file.path(dir, "registry.json"),
    auto_unbox = TRUE
  )

  reg <- load_patch_registry(dir)

  expect_length(reg, 1L)
  expect_identical(reg[[1L]]$package, "RcppParallel")
  expect_identical(reg[[1L]]$platforms, c("alpine", "ubuntu-2604"))
  expect_identical(reg[[1L]]$env$RCPP_PARALLEL_USE_TBB, "0")
  expect_identical(reg[[1L]]$configure_args, character(0L))
  expect_true(file.exists(reg[[1L]]$patch_path))
})

test_that("load_patch_registry returns empty list when no registry", {
  expect_identical(load_patch_registry(NULL), list())
  expect_identical(load_patch_registry(withr::local_tempdir()), list())
})

test_that("normalize_patch_entry errors on missing required field", {
  expect_error(
    normalize_patch_entry(list(package = "x"), tempdir()),
    "missing required field"
  )
})

test_that("normalize_patch_entry errors on missing patch file", {
  expect_error(
    normalize_patch_entry(
      list(
        package = "x",
        versions = "*",
        platforms = "alpine",
        reason = "r",
        patch = "nope.patch"
      ),
      tempdir()
    ),
    "does not exist"
  )
})

test_that("build_platform_tokens expands family and arch", {
  expect_setequal(
    build_platform_tokens("ubuntu-2604", "amd64"),
    c("ubuntu-2604", "ubuntu", "amd64")
  )
  expect_setequal(
    build_platform_tokens("alpine-324", "arm64"),
    c("alpine-324", "alpine", "arm64")
  )
})

test_that("match_patch_entries matches family, codename, arch, and wildcard", {
  reg <- list(
    list(package = "A", versions = "*", platforms = "alpine", reason = "r"),
    list(
      package = "B",
      versions = "*",
      platforms = "ubuntu-2604",
      reason = "r"
    ),
    list(package = "C", versions = "*", platforms = "*", reason = "r"),
    list(package = "D", versions = "*", platforms = "redhat", reason = "r")
  )
  got <- vapply(
    match_patch_entries(reg, "ubuntu-2604", "amd64"),
    function(e) e$package,
    character(1L)
  )
  expect_setequal(got, c("B", "C"))
})

test_that("version_satisfies handles operators and hyphenated versions", {
  expect_true(version_satisfies("5.1.11-2", "5.1.11-2"))
  expect_true(version_satisfies("5.1.11-2", "==5.1.11-2"))
  expect_true(version_satisfies("5.1.12", ">=5.1.0"))
  expect_false(version_satisfies("5.0.0", ">=5.1.0"))
  expect_true(version_satisfies("5.1.11-2", "<=5.1.11-2"))
  expect_false(version_satisfies("5.1.12", "<=5.1.11-2"))
})

test_that("patch_cache_key is stable and sensitive to env/patch changes", {
  e1 <- list(
    package = "P",
    env = list(A = "1"),
    configure_args = character(0L),
    makevars = list(),
    patch_path = NULL
  )
  e2 <- e1
  e2$env <- list(A = "2")

  k1 <- patch_cache_key(e1, "1.0", "alpine-324", "amd64", "4.5")
  expect_identical(k1, patch_cache_key(e1, "1.0", "alpine-324", "amd64", "4.5"))
  expect_false(identical(
    k1,
    patch_cache_key(e2, "1.0", "alpine-324", "amd64", "4.5")
  ))
  expect_match(k1, "^P_1.0_alpine-324_amd64_4.5_[0-9a-f]{12}$")
})

test_that("resolve_patch_version returns latest for wildcard, NA when unmet", {
  local_mocked_bindings(
    cran_package = function(pkg) list(Version = "5.1.12"),
    .package = "pkgsearch"
  )
  expect_identical(
    resolve_patch_version(list(package = "RcppParallel", versions = "*")),
    "5.1.12"
  )
  expect_identical(
    resolve_patch_version(list(package = "RcppParallel", versions = ">=9.0")),
    NA_character_
  )
})

test_that("describe_patch summarizes the active overrides", {
  expect_match(
    describe_patch(list(
      env = list(RCPP_PARALLEL_USE_TBB = "0"),
      configure_args = character(0L),
      makevars = list(),
      patch_path = NULL
    )),
    "env: RCPP_PARALLEL_USE_TBB=0"
  )
  expect_match(
    describe_patch(list(
      env = list(),
      configure_args = character(0L),
      makevars = list(),
      patch_path = "/x/fix.patch"
    )),
    "source patch"
  )
})

test_that("configure_args_to_build_args formats configure args", {
  expect_identical(configure_args_to_build_args(character(0L)), character(0L))
  expect_identical(
    configure_args_to_build_args(c("--with-foo", "--no-bar")),
    "--configure-args=--with-foo --no-bar"
  )
})

test_that("apply_source_patch returns FALSE when patch does not apply", {
  skip_if_not(nzchar(Sys.which("git")))
  src <- withr::local_tempdir()
  writeLines("unrelated content", file.path(src, "file.txt"))
  bad_patch <- tempfile(fileext = ".patch")
  writeLines(
    c(
      "--- a/missing.txt",
      "+++ b/missing.txt",
      "@@ -1 +1 @@",
      "-nope",
      "+nope2"
    ),
    bad_patch
  )
  expect_false(apply_source_patch(bad_patch, src))
})

test_that("apply_source_patch applies a valid diff via git apply", {
  skip_if_not(nzchar(Sys.which("git")))
  src <- withr::local_tempdir()
  dir.create(file.path(src, "sub"))
  writeLines(c("line one", "line two"), file.path(src, "sub", "f.txt"))
  good_patch <- tempfile(fileext = ".patch")
  writeLines(
    c(
      "diff --git a/sub/f.txt b/sub/f.txt",
      "--- a/sub/f.txt",
      "+++ b/sub/f.txt",
      "@@ -1,2 +1,2 @@",
      " line one",
      "-line two",
      "+line two patched"
    ),
    good_patch
  )
  expect_true(apply_source_patch(good_patch, src))
  expect_true(any(grepl(
    "line two patched",
    readLines(file.path(src, "sub", "f.txt"))
  )))
})

test_that("build_patched_binary returns NULL when download fails", {
  local_mocked_bindings(download_cran_source = function(...) NULL)
  expect_null(
    build_patched_binary(
      list(
        package = "P",
        env = list(),
        configure_args = character(0L),
        makevars = list(),
        patch_path = NULL
      ),
      "1.0",
      withr::local_tempdir()
    )
  )
})

test_that("build_patched_binary builds with an empty env (no with_envvar error)", {
  skip_if_not_installed("mockery")
  dest <- withr::local_tempdir()
  mockery::stub(
    build_patched_binary,
    "download_cran_source",
    function(...) "fake.tar.gz"
  )
  mockery::stub(
    build_patched_binary,
    "utils::untar",
    function(tarfile, exdir) {
      dir.create(file.path(exdir, "P"), showWarnings = FALSE)
      0L
    }
  )
  mockery::stub(
    build_patched_binary,
    "pkgbuild::build",
    function(path, ...) {
      out <- file.path(dest, "P_1.0.tar.gz")
      writeLines("binary", out)
      out
    }
  )
  res <- build_patched_binary(
    list(
      package = "P",
      env = list(),
      configure_args = character(0L),
      makevars = list(),
      patch_path = NULL
    ),
    "1.0",
    dest
  )
  expect_identical(res, file.path(dest, "P_1.0.tar.gz"))
})

test_that("prepare_patched_repo returns NULL when no entries match", {
  dir <- withr::local_tempdir()
  jsonlite::write_json(
    list(list(
      package = "A",
      versions = "*",
      platforms = list("redhat"),
      reason = "r"
    )),
    file.path(dir, "registry.json"),
    auto_unbox = TRUE
  )
  expect_null(
    prepare_patched_repo(
      dir,
      "ubuntu-2604",
      "amd64",
      "4.5",
      cache_dir = withr::local_tempdir(),
      repo_dir = withr::local_tempdir()
    )
  )
})

test_that("run_pak_install_with_mutex prepends the patched repo to repos", {
  seen <- NULL
  local_mocked_bindings(
    acquire_pak_mutex = function(...) tempfile(),
    release_pak_mutex = function(...) invisible(NULL),
    retry_with_backoff = function(func, ...) func()
  )
  local_mocked_bindings(
    local_install_deps = function(...) {
      seen <<- getOption("repos")
      invisible(TRUE)
    },
    .package = "pak"
  )

  run_pak_install_with_mutex(
    tempfile(),
    list(),
    patched_repo = "/tmp/patched"
  )

  expect_true(any(grepl("file:///tmp/patched", seen)))
})

test_that("handle_system_dependencies forwards patches and arch", {
  captured <- list()
  local_mocked_bindings(
    install_pkg_sys_deps = function(
      package_name,
      tag,
      local_clone_dir_single,
      platform,
      patches = NULL,
      arch = NULL
    ) {
      captured <<- list(patches = patches, arch = arch)
      invisible(TRUE)
    }
  )
  handle_system_dependencies(
    "RcppParallel",
    "5.1.11-2",
    "ubuntu-2604",
    tempfile(),
    "amd64",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    patches = "local/patches"
  )
  expect_identical(captured$patches, "local/patches")
  expect_identical(captured$arch, "amd64")
})

test_that("prepare_patched_repo serves a cached binary and writes an index", {
  dir <- withr::local_tempdir()
  jsonlite::write_json(
    list(list(
      package = "glue",
      versions = "*",
      platforms = list("*"),
      env = list(A = "1"),
      reason = "r"
    )),
    file.path(dir, "registry.json"),
    auto_unbox = TRUE
  )
  cache <- withr::local_tempdir()
  repo <- withr::local_tempdir()

  local_mocked_bindings(
    resolve_patch_version = function(entry) "1.0.0",
    build_patched_binary = function(entry, version, dest_dir) {
      f <- file.path(dest_dir, sprintf("%s_%s.tar.gz", entry$package, version))
      writeLines("fake binary", f)
      f
    }
  )
  local_mocked_bindings(
    write_PACKAGES = function(dir, ...) {
      writeLines(character(0L), file.path(dir, "PACKAGES"))
      invisible(0L)
    },
    .package = "tools"
  )

  out <- prepare_patched_repo(
    dir,
    "ubuntu-2604",
    "amd64",
    "4.5",
    cache_dir = cache,
    repo_dir = repo
  )

  expect_identical(out, repo)
  expect_true(file.exists(file.path(
    repo,
    "src",
    "contrib",
    "glue_1.0.0.tar.gz"
  )))
  expect_true(file.exists(file.path(repo, "src", "contrib", "PACKAGES")))
  # The build result was cached under the key.
  expect_length(list.files(cache, pattern = "^glue_1.0.0_.*\\.tar\\.gz$"), 1L)
})

test_that("prepare_patched_repo src/contrib layout is resolvable by available.packages", {
  # Build a minimal valid source package tarball
  pkg_tmp <- withr::local_tempdir()
  pkg_dir <- file.path(pkg_tmp, "pkgfoo")
  dir.create(pkg_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "Package: pkgfoo",
      "Version: 1.0.0",
      "Title: T",
      "Description: d",
      "License: MIT",
      "Author: a",
      "Maintainer: a <a@b.c>",
      "Built: R 4.5.3; x86_64-pc-linux-gnu; 2026-06-30 00:00:00 UTC; unix"
    ),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines('exportPattern("^[[:alpha:]]+")', file.path(pkg_dir, "NAMESPACE"))
  tarball <- file.path(pkg_tmp, "pkgfoo_1.0.0.tar.gz")
  withr::with_dir(pkg_tmp, {
    utils::tar(tarball, files = "pkgfoo", compression = "gzip")
  })

  patches_dir <- withr::local_tempdir()
  jsonlite::write_json(
    list(list(
      package = "pkgfoo",
      versions = "*",
      platforms = list("*"),
      reason = "r"
    )),
    file.path(patches_dir, "registry.json"),
    auto_unbox = TRUE
  )

  cache <- withr::local_tempdir()
  repo <- withr::local_tempdir()

  local_mocked_bindings(
    resolve_patch_version = function(entry) "1.0.0",
    build_patched_binary = function(entry, version, dest_dir) {
      target <- file.path(
        dest_dir,
        sprintf("%s_%s.tar.gz", entry$package, version)
      )
      file.copy(tarball, target, overwrite = TRUE)
      target
    }
  )
  # cranlike::add_PACKAGES is NOT mocked — use the real indexer

  out <- prepare_patched_repo(
    patches_dir,
    "ubuntu-2604",
    "amd64",
    "4.5",
    cache_dir = cache,
    repo_dir = repo
  )

  # tarball must be under src/contrib
  expect_true(file.exists(file.path(
    out,
    "src",
    "contrib",
    "pkgfoo_1.0.0.tar.gz"
  )))

  # available.packages() must resolve pkgfoo from the file:// repo
  ap <- available.packages(
    repos = paste0("file://", out),
    type = "source"
  )
  expect_true("pkgfoo" %in% rownames(ap))
  expect_identical(ap["pkgfoo", "Version"], "1.0.0")

  # The PACKAGES index must preserve the Built field so pak installs the
  # prebuilt binary without recompiling.  This fails with
  # write_PACKAGES(type="source") (no fields) and passes with fields="Built".
  pkgs <- readLines(file.path(out, "src", "contrib", "PACKAGES"))
  expect_true(any(grepl("^Built:", pkgs)))
})

test_that("a patched dependency unblocks a dependent build (e2e)", {
  skip_if_not(nzchar(Sys.getenv("BINCRAFT_PATCH_E2E")))
  skip_if_offline()

  patches_dir <- withr::local_tempdir()
  jsonlite::write_json(
    list(list(
      package = "RcppParallel",
      versions = "*",
      platforms = list("*"),
      env = list(RCPP_PARALLEL_USE_TBB = "0"),
      reason = "bundled TBB fails on this toolchain"
    )),
    file.path(patches_dir, "registry.json"),
    auto_unbox = TRUE
  )

  out <- withr::local_tempdir()
  result <- build_binary_package(
    "rts2",
    tag = "latest",
    local_output_dir_root = out,
    upload = FALSE,
    archive = FALSE,
    patches = patches_dir
  )
  expect_true(isTRUE(result) || identical(result, "skipped"))
})
