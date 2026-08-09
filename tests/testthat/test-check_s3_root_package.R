# `mockery::stub()` rebinds the function in the *calling* frame, so each test
# calls check_s3_root_package() directly: routing through a helper would resolve
# the unstubbed original and hit S3 for real.

test_that("check_s3_root_package reports an absent object as absent", {
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "s3fs::s3_file_exists", FALSE)

  expect_false(check_s3_root_package(
    "bucket/amd64/alpine324/latest/src/contrib",
    "curl",
    "7.1.0",
    FALSE,
    "fake",
    "fake",
    "https://fake.endpoint.com",
    "us-east-1"
  ))
})

test_that("check_s3_root_package reports a real binary as present", {
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "s3fs::s3_file_exists", TRUE)
  mockery::stub(check_s3_root_package, "remote_object_md5", "a-real-build")
  mockery::stub(check_s3_root_package, "is_cran_source_tarball", FALSE)

  expect_true(check_s3_root_package(
    "bucket/amd64/alpine324/latest/src/contrib",
    "curl",
    "7.1.0",
    FALSE,
    "fake",
    "fake",
    "https://fake.endpoint.com",
    "us-east-1"
  ))
})

test_that("check_s3_root_package reports a CRAN source fallback as absent", {
  # This is what decides whether a build runs. An object occupying the key is
  # not a binary: a failed build publishes the CRAN source under exactly that
  # name, and treating it as present made the rebuild skip every package it
  # exists to fix.
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "s3fs::s3_file_exists", TRUE)
  mockery::stub(
    check_s3_root_package,
    "remote_object_md5",
    "8af2ccbf5d85dc18866f45f1f26f348d"
  )
  mockery::stub(check_s3_root_package, "is_cran_source_tarball", TRUE)

  expect_false(check_s3_root_package(
    "bucket/amd64/alpine324/latest/src/contrib",
    "curl",
    "7.1.0",
    FALSE,
    "fake",
    "fake",
    "https://fake.endpoint.com",
    "us-east-1"
  ))
})

test_that("check_s3_root_package keeps an unknown MD5 as present", {
  # A multipart ETag or an unreachable CRAN must not make every package look
  # missing, which would trigger a full rebuild of the repository.
  skip_if_not_installed("mockery")

  mockery::stub(check_s3_root_package, "s3fs::s3_file_exists", TRUE)
  mockery::stub(check_s3_root_package, "remote_object_md5", NA_character_)

  expect_true(check_s3_root_package(
    "bucket/amd64/alpine324/latest/src/contrib",
    "curl",
    "7.1.0",
    FALSE,
    "fake",
    "fake",
    "https://fake.endpoint.com",
    "us-east-1"
  ))
})

test_that("check_s3_root_package looks in the per-minor slot when sensitive", {
  skip_if_not_installed("mockery")

  seen <- NULL
  mockery::stub(
    check_s3_root_package,
    "s3fs::s3_file_exists",
    function(path) {
      seen <<- path
      FALSE
    }
  )
  mockery::stub(check_s3_root_package, "get_minor_version", "4.5")

  expect_false(check_s3_root_package(
    "bucket/amd64/alpine324/latest/src/contrib",
    "curl",
    "7.1.0",
    TRUE,
    "fake",
    "fake",
    "https://fake.endpoint.com",
    "us-east-1"
  ))
  expect_match(seen, "/4\\.5/curl_7\\.1\\.0\\.tar\\.gz$")
})
