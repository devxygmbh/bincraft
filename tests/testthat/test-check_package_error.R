test_that("a previous error is scoped to the R minor that hit it", {
  # A failure is a fact about one interpreter, not about the platform. Without
  # the scope, every non-primary pass inherits the primary's failures and skips
  # them, so a per-minor slot can never be filled by rebuilding: one resolute
  # run skipped 6988 of 7064 attempts and built nothing.
  seen <- NULL
  con <- structure(list(), class = "fake_con")
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params, ...) {
      seen <<- list(statement = statement, params = params)
      data.frame(error_occurred = TRUE)
    },
    .package = "DBI"
  )

  result <- check_package_error(
    con,
    "single_builds",
    list(pkg = "curl", tag = "7.1.0"),
    "ubuntu-2604",
    "amd64"
  )

  expect_true(result)
  expect_match(seen$statement, "r_version")
  expect_length(seen$params, 5L)
  expect_identical(seen$params[[5L]], get_minor_version())
})

test_that("a package with no recorded error for this minor is not skipped", {
  con <- structure(list(), class = "fake_con")
  local_mocked_bindings(
    dbGetQuery = function(...) data.frame(error_occurred = logical()),
    .package = "DBI"
  )

  expect_false(
    check_package_error(
      con,
      "single_builds",
      list(pkg = "curl", tag = "7.1.0"),
      "ubuntu-2604",
      "amd64"
    )
  )
})

test_that("an incomplete pair is never treated as a previous error", {
  expect_false(
    check_package_error(
      NULL,
      "single_builds",
      list(pkg = NA, tag = "1.0"),
      "p",
      "a"
    )
  )
  expect_false(
    check_package_error(
      NULL,
      "single_builds",
      list(pkg = "curl", tag = NA),
      "p",
      "a"
    )
  )
})
