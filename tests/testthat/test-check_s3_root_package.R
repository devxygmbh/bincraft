# `mockery::stub()` rebinds the function in the *calling* frame, so each test
# calls check_s3_root_package() directly: routing through a helper would resolve
# the unstubbed original and hit S3 for real.
#
# The stub target is `remote_object_state()`, which is the boundary this
# function now reasons about. What that helper does with an ETag is covered in
# test-source_fallback.R, against a realistic s3fs response.

root_args <- function(sensitive = FALSE) {
  list(
    "bucket/amd64/alpine324/latest/src/contrib",
    "curl",
    "7.1.0",
    sensitive,
    "fake",
    "fake",
    "https://fake.endpoint.com",
    "us-east-1"
  )
}

test_that("check_s3_root_package reports an absent object as absent", {
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "remote_object_state", "absent")

  expect_false(do.call(check_s3_root_package, root_args()))
})

test_that("check_s3_root_package reports a real binary as present", {
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "remote_object_state", "binary")

  expect_true(do.call(check_s3_root_package, root_args()))
})

test_that("check_s3_root_package reports a CRAN source fallback as absent", {
  # This is what decides whether a build runs. An object occupying the key is
  # not a binary: a failed build publishes the CRAN source under exactly that
  # name, and treating it as present made the rebuild skip every package it
  # exists to fix.
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "remote_object_state", "source")

  expect_false(do.call(check_s3_root_package, root_args()))
})

test_that("check_s3_root_package looks in the per-minor slot when sensitive", {
  skip_if_not_installed("mockery")

  seen <- NULL
  mockery::stub(
    check_s3_root_package,
    "remote_object_state",
    function(path, ...) {
      seen <<- path
      "absent"
    }
  )
  mockery::stub(check_s3_root_package, "get_minor_version", "4.5")

  expect_false(do.call(check_s3_root_package, root_args(sensitive = TRUE)))
  expect_match(seen, "/4\\.5/curl_7\\.1\\.0\\.tar\\.gz$")
})
