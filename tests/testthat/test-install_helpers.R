test_that("classify_uvr_error retries transient failures, fails fast on the rest", {
  expect_true(
    classify_uvr_error("error sending request: connection reset")$should_retry
  )
  expect_true(
    classify_uvr_error("failed to download PACKAGES.gz: timed out")$should_retry
  )
  expect_true(
    classify_uvr_error("HTTP 503 Service Unavailable")$should_retry
  )
  expect_false(
    classify_uvr_error("could not resolve dependencies for pkg")$should_retry
  )
  expect_false(
    classify_uvr_error("ERROR: compilation failed for package 'x'")$should_retry
  )
})

test_that("classify_uvr_error labels error types", {
  expect_equal(classify_uvr_error("timed out")$error_type, "network error")
  expect_equal(
    classify_uvr_error("could not resolve foo")$error_type,
    "resolution error"
  )
  expect_equal(
    classify_uvr_error("fatal error: bar.h: No such file")$error_type,
    "compile error"
  )
})

test_that("a network error mixed with a resolution error is not retried", {
  # A resolution failure that also mentions a timeout must fail fast, never loop.
  msg <- "timed out; could not resolve dependencies"
  expect_false(classify_uvr_error(msg)$should_retry)
})
