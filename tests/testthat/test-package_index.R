test_that("package_index_remote_dir builds the generic slot when r_minor is NULL", {
  expect_identical(
    package_index_remote_dir("bucket", "amd64", "alpine323"),
    file.path("bucket", "amd64", "alpine323", "latest", "src", "contrib")
  )
})

test_that("package_index_remote_dir appends the minor slot when r_minor is set", {
  expect_identical(
    package_index_remote_dir("bucket", "amd64", "alpine323", r_minor = "4.4"),
    file.path("bucket", "amd64", "alpine323", "latest", "src", "contrib", "4.4")
  )
})

test_that("built_stamp reproduces R's Built field format", {
  expect_identical(
    built_stamp(
      platform = "x86_64-pc-linux-musl",
      r_version = "4.5.3",
      time = as.POSIXct("2026-06-12 01:10:57", tz = "UTC")
    ),
    "R 4.5.3; x86_64-pc-linux-musl; 2026-06-12 01:10:57 UTC; unix"
  )
})

test_that("built_stamp rejects an unusable platform instead of stamping it", {
  # A stamp is written verbatim into every entry of a slot's PACKAGES index, and
  # uvr decides binary-vs-source by matching that triple. Stamping `NA` there
  # silently turns a whole slot source-only, so an unusable platform must fail
  # the index write rather than reach the index.
  expect_error(
    built_stamp(platform = NA, r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = NA_character_, r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = NULL, r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = "", r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = c("a", "b"), r_version = "4.5.0"),
    "platform"
  )
})

test_that("built_stamp rejects an unusable R version", {
  expect_error(
    built_stamp(platform = "x86_64-pc-linux-musl", r_version = NA),
    "r_version"
  )
  expect_error(
    built_stamp(platform = "x86_64-pc-linux-musl", r_version = ""),
    "r_version"
  )
})

test_that("built_stamp always formats the time in UTC", {
  # A non-UTC input time must still be rendered as its UTC wall-clock value so
  # the stamp is reproducible regardless of the build host's timezone.
  expect_match(
    built_stamp(
      platform = "aarch64-unknown-linux-musl",
      r_version = "4.4.2",
      time = as.POSIXct("2026-06-12 03:10:57", tz = "Europe/Berlin")
    ),
    "^R 4\\.4\\.2; aarch64-unknown-linux-musl; 2026-06-12 01:10:57 UTC; unix$"
  )
})
