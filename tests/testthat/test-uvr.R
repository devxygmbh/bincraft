test_that("parse_description_deps collects Imports, Depends, LinkingTo and drops base + R", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "Package: demo",
      "Depends: R (>= 4.1.0), methods",
      "Imports: Rcpp (>= 1.0.0), cli,",
      "    stats",
      "LinkingTo: Rcpp, RcppArmadillo",
      "Suggests: testthat"
    ),
    file.path(dir, "DESCRIPTION")
  )
  deps <- parse_description_deps(file.path(dir, "DESCRIPTION"))
  expect_setequal(deps, c("Rcpp", "cli", "RcppArmadillo"))
  expect_false("R" %in% deps)
  expect_false("methods" %in% deps)
  expect_false("stats" %in% deps)
  expect_false("testthat" %in% deps) # Suggests excluded
})

test_that("parse_description_deps returns empty when no dependency fields exist", {
  dir <- withr::local_tempdir()
  writeLines(c("Package: demo", "Title: x"), file.path(dir, "DESCRIPTION"))
  expect_length(parse_description_deps(file.path(dir, "DESCRIPTION")), 0L)
})

test_that("write_uvr_manifest emits valid [project]/[dependencies] TOML", {
  dir <- withr::local_tempdir()
  writeLines(
    c("Package: demo", "Imports: cli, Rcpp", "LinkingTo: RcppArmadillo"),
    file.path(dir, "DESCRIPTION")
  )
  path <- write_uvr_manifest(dir)
  toml <- readLines(path)
  expect_true(any(grepl("^\\[project\\]", toml)))
  expect_true(any(grepl('^name = "demo"', toml)))
  expect_true(any(grepl("^\\[dependencies\\]", toml)))
  expect_true(any(grepl('^"cli" = "\\*"', toml)))
  expect_true(any(grepl('^"Rcpp" = "\\*"', toml)))
  expect_true(any(grepl('^"RcppArmadillo" = "\\*"', toml)))
})

test_that("write_uvr_manifest quotes dependency names containing dots", {
  dir <- withr::local_tempdir()
  writeLines(
    c("Package: demo", "Imports: data.table, rpart, rpart.plot"),
    file.path(dir, "DESCRIPTION")
  )
  toml <- readLines(write_uvr_manifest(dir))
  # Bare `data.table = "*"` is a dotted TOML key (package `data`, sub-key
  # `table`), and `rpart.plot` after `rpart` fails the parse outright.
  expect_true(any(grepl('^"data\\.table" = "\\*"', toml)))
  expect_true(any(grepl('^"rpart" = "\\*"', toml)))
  expect_true(any(grepl('^"rpart\\.plot" = "\\*"', toml)))
  expect_false(any(grepl('^[[:alnum:]._]+ = "\\*"', toml)))
})

test_that("uvr_bin errors with an actionable message when uvr is absent", {
  mockery::stub(uvr_bin, "Sys.which", "")
  expect_error(uvr_bin(), "uvr")
})

test_that("uvr_bin returns the resolved path when uvr is present", {
  mockery::stub(uvr_bin, "Sys.which", c(uvr = "/usr/local/bin/uvr"))
  expect_equal(uvr_bin(), "/usr/local/bin/uvr")
})

test_that("run_uvr raises with captured output on non-zero exit", {
  mockery::stub(run_uvr, "uvr_bin", "/usr/bin/uvr")
  fake <- structure("boom: could not resolve", status = 1L)
  mockery::stub(run_uvr, "system2", fake)
  expect_error(run_uvr(c("lock"), tempdir()), "could not resolve")
})

test_that("run_uvr returns output and passes args through on success", {
  mockery::stub(run_uvr, "uvr_bin", "/usr/bin/uvr")
  seen <- new.env()
  mockery::stub(run_uvr, "system2", function(command, args, ...) {
    seen$command <- command
    seen$args <- args
    "ok"
  })
  out <- run_uvr(c("sync", "--library", "/lib"), tempdir())
  expect_equal(out, "ok")
  expect_equal(seen$command, "/usr/bin/uvr")
  expect_equal(seen$args, c("sync", "--library", "/lib"))
})

test_that("run_uvr_install locks, then syncs with sysreqs into the target library", {
  clone <- withr::local_tempdir()
  writeLines(
    c("Package: demo", "Imports: cli"),
    file.path(clone, "DESCRIPTION")
  )
  calls <- list()
  mockery::stub(
    run_uvr_install,
    "run_uvr",
    function(args, wd, env = character()) {
      calls[[length(calls) + 1L]] <<- list(args = args, wd = wd, env = env)
      invisible("ok")
    }
  )
  run_uvr_install(clone, library = "/build/lib")
  expect_equal(calls[[1]]$args, "lock")
  expect_equal(
    calls[[2]]$args,
    c("sync", "--install-system-deps", "--library", "/build/lib")
  )
  expect_equal(unname(calls[[2]]$env["UVR_INSTALL_SYSREQS"]), "1")
  expect_true(file.exists(file.path(clone, "uvr.toml")))
})

test_that("run_uvr_install pre-installs patched binaries before syncing", {
  clone <- withr::local_tempdir()
  writeLines(
    c("Package: demo", "Imports: cli"),
    file.path(clone, "DESCRIPTION")
  )
  order <- character()
  mockery::stub(
    run_uvr_install,
    "install_patched_into_library",
    function(...) order[[length(order) + 1L]] <<- "patched"
  )
  mockery::stub(run_uvr_install, "run_uvr", function(args, ...) {
    order[[length(order) + 1L]] <<- paste(args, collapse = " ")
    invisible("ok")
  })
  run_uvr_install(clone, library = "/build/lib", patched_repo = "/tmp/repo")
  expect_equal(order[[1]], "patched")
  expect_true(startsWith(order[[2]], "lock"))
})

test_that("write_r_version_pin records the running R exactly as uvr queries it", {
  dir <- withr::local_tempdir()
  path <- write_r_version_pin(dir)
  expect_equal(path, file.path(dir, ".r-version"))
  expect_equal(
    readLines(path),
    paste(R.version$major, R.version$minor, sep = ".")
  )
})

test_that("run_uvr_install pins the R version before locking", {
  clone <- withr::local_tempdir()
  writeLines(
    c("Package: demo", "Imports: cli"),
    file.path(clone, "DESCRIPTION")
  )
  pinned_at_call <- logical()
  mockery::stub(run_uvr_install, "run_uvr", function(args, ...) {
    pinned_at_call[[length(pinned_at_call) + 1L]] <<- file.exists(
      file.path(clone, ".r-version")
    )
    invisible("ok")
  })
  run_uvr_install(clone, library = "/build/lib")
  # The pin must exist for `uvr lock` too: the lockfile records per-R-minor
  # binary URLs and the Bioconductor release.
  expect_true(all(pinned_at_call))
  expect_equal(
    readLines(file.path(clone, ".r-version")),
    paste(R.version$major, R.version$minor, sep = ".")
  )
})

test_that("run_uvr_install skips patched pre-install when no repo is given", {
  clone <- withr::local_tempdir()
  writeLines(
    c("Package: demo", "Imports: cli"),
    file.path(clone, "DESCRIPTION")
  )
  called <- FALSE
  mockery::stub(
    run_uvr_install,
    "install_patched_into_library",
    function(...) called <<- TRUE
  )
  mockery::stub(run_uvr_install, "run_uvr", function(...) invisible("ok"))
  run_uvr_install(clone, library = "/build/lib", patched_repo = NULL)
  expect_false(called)
})

test_that("install_pkg_sys_deps installs deps through uvr with retry", {
  clone_root <- withr::local_tempdir()
  called <- new.env()
  mockery::stub(
    install_pkg_sys_deps,
    "clone_package_repo",
    function(...) invisible(NULL)
  )
  mockery::stub(install_pkg_sys_deps, "prepare_patched_repo", function(...) {
    NULL
  })
  mockery::stub(
    install_pkg_sys_deps,
    "run_uvr_install",
    function(clone_dir, library, patched_repo) {
      called$clone <- clone_dir
      called$patched <- patched_repo
      invisible(TRUE)
    }
  )
  install_pkg_sys_deps("brew", "v1.0", clone_root, platform = "ubuntu-2604")
  expect_true(!is.null(called$clone))
  expect_null(called$patched)
})

test_that("install_pkg_sys_deps forwards a prepared patched repo to uvr", {
  clone_root <- withr::local_tempdir()
  called <- new.env()
  mockery::stub(
    install_pkg_sys_deps,
    "clone_package_repo",
    function(...) invisible(NULL)
  )
  mockery::stub(
    install_pkg_sys_deps,
    "prepare_patched_repo",
    function(...) "/tmp/patched-repo"
  )
  mockery::stub(
    install_pkg_sys_deps,
    "run_uvr_install",
    function(clone_dir, library, patched_repo) {
      called$patched <- patched_repo
      invisible(TRUE)
    }
  )
  install_pkg_sys_deps(
    "brew",
    "v1.0",
    clone_root,
    platform = "ubuntu-2604",
    patches = "local/patches",
    arch = "amd64"
  )
  expect_equal(called$patched, "/tmp/patched-repo")
})

test_that("install_patched_into_library R CMD INSTALLs each tarball into the library", {
  repo <- withr::local_tempdir()
  contrib <- file.path(repo, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  file.create(file.path(contrib, "pkgA_1.0.tar.gz"))
  file.create(file.path(contrib, "pkgB_2.0.tar.gz"))
  seen <- list()
  mockery::stub(
    install_patched_into_library,
    "system2",
    function(command, args, ...) {
      seen[[length(seen) + 1L]] <<- args
      0L
    }
  )
  install_patched_into_library(repo, "/build/lib")
  installed <- vapply(seen, function(a) a[[length(a)]], character(1L))
  expect_true(any(grepl("pkgA_1.0.tar.gz", installed)))
  expect_true(any(grepl("pkgB_2.0.tar.gz", installed)))
  expect_true(all(vapply(
    seen,
    function(a) any(grepl("--library=/build/lib", a)),
    logical(1L)
  )))
})
