test_that("classify_r_minor_sensitive returns TRUE for a risky package", {
  skip_if_not_installed("mockery")
  mockery::stub(
    classify_r_minor_sensitive,
    "clone_repository",
    function(pkg, tag, url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      writeLines(
        c(
          "Package: dummy",
          "Version: 1.0",
          "NeedsCompilation: yes",
          "LinkingTo: Rcpp"
        ),
        file.path(dest, "DESCRIPTION")
      )
      dir.create(file.path(dest, "src"))
      writeLines("// Rcpp glue", file.path(dest, "src", "x.cpp"))
    }
  )
  expect_true(classify_r_minor_sensitive(
    "dummy",
    "1.0",
    "https://github.com/cran"
  ))
})

test_that("classify_r_minor_sensitive returns FALSE for a pure-r package", {
  skip_if_not_installed("mockery")
  mockery::stub(
    classify_r_minor_sensitive,
    "clone_repository",
    function(pkg, tag, url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      writeLines(
        c("Package: dummy", "Version: 1.0", "NeedsCompilation: no"),
        file.path(dest, "DESCRIPTION")
      )
    }
  )
  expect_false(classify_r_minor_sensitive(
    "dummy",
    "1.0",
    "https://github.com/cran"
  ))
})

test_that("classify_r_minor_sensitive fails safe to TRUE on clone error", {
  skip_if_not_installed("mockery")
  mockery::stub(classify_r_minor_sensitive, "clone_repository", function(...) {
    stop("boom")
  })
  expect_true(classify_r_minor_sensitive(
    "dummy",
    "1.0",
    "https://github.com/cran"
  ))
})

test_that("classifier mode passes per-package is_r_minor_sensitive to build", {
  skip_if_not_installed("mockery")
  recorded <- list()
  mockery::stub(
    process_cran_updates,
    "get_updated_cran_packages",
    function(...) {
      data.frame(
        name = c("riskypkg", "purepkg"),
        version = c("1.0", "2.0"),
        stringsAsFactors = FALSE
      )
    }
  )
  mockery::stub(process_cran_updates, "get_new_cran_packages", function(...) {
    data.frame(name = character(), version = character())
  })
  mockery::stub(process_cran_updates, "tools::CRAN_package_db", function(...) {
    data.frame(Package = character(), OS_type = character())
  })
  mockery::stub(
    process_cran_updates,
    "classify_r_minor_sensitive",
    function(name, tag, ...) name == "riskypkg"
  )
  mockery::stub(
    process_cran_updates,
    "build_binary_package",
    function(name, tag, ..., is_r_minor_sensitive) {
      recorded[[name]] <<- is_r_minor_sensitive
      invisible(TRUE)
    }
  )

  process_cran_updates(
    platform = "alpine-323",
    process_removed = FALSE,
    r_minor_detection = "classifier",
    s3_endpoint = "x",
    s3_region = "x",
    s3_bucket = "x"
  )

  expect_true(recorded[["riskypkg"]])
  expect_false(recorded[["purepkg"]])
})

test_that("r_minor_sensitive_only drops non-risky candidates", {
  skip_if_not_installed("mockery")
  built <- character()
  mockery::stub(
    process_cran_updates,
    "get_updated_cran_packages",
    function(...) {
      data.frame(
        name = c("riskypkg", "purepkg"),
        version = c("1.0", "2.0"),
        stringsAsFactors = FALSE
      )
    }
  )
  mockery::stub(process_cran_updates, "get_new_cran_packages", function(...) {
    data.frame(name = character(), version = character())
  })
  mockery::stub(process_cran_updates, "tools::CRAN_package_db", function(...) {
    data.frame(Package = character(), OS_type = character())
  })
  mockery::stub(
    process_cran_updates,
    "classify_r_minor_sensitive",
    function(name, tag, ...) name == "riskypkg"
  )
  mockery::stub(
    process_cran_updates,
    "build_binary_package",
    function(name, tag, ..., is_r_minor_sensitive) {
      built <<- c(built, name)
      invisible(TRUE)
    }
  )

  process_cran_updates(
    platform = "alpine-323",
    process_removed = FALSE,
    r_minor_detection = "classifier",
    r_minor_sensitive_only = TRUE,
    s3_endpoint = "x",
    s3_region = "x",
    s3_bucket = "x"
  )

  expect_identical(built, "riskypkg")
})

test_that("filter_r_minor_sensitive = TRUE (legacy) builds all as r-minor-sensitive via issue path", {
  skip_if_not_installed("mockery")
  recorded <- list()
  mockery::stub(
    process_cran_updates,
    "get_updated_cran_packages",
    function(...) data.frame(name = character(), version = character())
  )
  mockery::stub(process_cran_updates, "get_new_cran_packages", function(...) {
    data.frame(name = character(), version = character())
  })
  mockery::stub(
    process_cran_updates,
    "get_r_minor_sensitive_packages",
    function(...) {
      data.frame(
        name = c("a", "b"),
        version = c("1.0", "2.0"),
        stringsAsFactors = FALSE
      )
    }
  )
  mockery::stub(process_cran_updates, "tools::CRAN_package_db", function(...) {
    data.frame(Package = character(), OS_type = character())
  })
  mockery::stub(
    process_cran_updates,
    "build_binary_package",
    function(name, tag, ..., is_r_minor_sensitive) {
      recorded[[name]] <<- is_r_minor_sensitive
      invisible(TRUE)
    }
  )

  process_cran_updates(
    platform = "alpine-323",
    process_removed = FALSE,
    filter_r_minor_sensitive = TRUE,
    s3_endpoint = "x",
    s3_region = "x",
    s3_bucket = "x"
  )

  expect_true(recorded[["a"]])
  expect_true(recorded[["b"]])
})

test_that("default detection (none) builds everything as non-sensitive", {
  skip_if_not_installed("mockery")
  recorded <- list()
  mockery::stub(
    process_cran_updates,
    "get_updated_cran_packages",
    function(...) {
      data.frame(
        name = c("a", "b"),
        version = c("1.0", "2.0"),
        stringsAsFactors = FALSE
      )
    }
  )
  mockery::stub(process_cran_updates, "get_new_cran_packages", function(...) {
    data.frame(name = character(), version = character())
  })
  mockery::stub(process_cran_updates, "tools::CRAN_package_db", function(...) {
    data.frame(Package = character(), OS_type = character())
  })
  mockery::stub(
    process_cran_updates,
    "classify_r_minor_sensitive",
    function(...) stop("classifier must not be called in 'none' mode")
  )
  mockery::stub(
    process_cran_updates,
    "build_binary_package",
    function(name, tag, ..., is_r_minor_sensitive) {
      recorded[[name]] <<- is_r_minor_sensitive
      invisible(TRUE)
    }
  )

  process_cran_updates(
    platform = "alpine-323",
    process_removed = FALSE,
    s3_endpoint = "x",
    s3_region = "x",
    s3_bucket = "x"
  )

  expect_false(recorded[["a"]])
  expect_false(recorded[["b"]])
})
