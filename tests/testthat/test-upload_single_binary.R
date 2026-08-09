# The upload gate is where a source fallback is finally replaced, so the two
# things worth pinning down are that a source does not block the upload and that
# the upload is allowed to overwrite it.

upload_args_for <- function(states) {
  captured <- NULL

  mockery::stub(upload_single_binary, "set_codename", "alpine324")
  mockery::stub(upload_single_binary, "s3fs::s3_file_system", NULL)
  mockery::stub(upload_single_binary, "file.exists", TRUE)
  mockery::stub(upload_single_binary, "file.remove", TRUE)
  mockery::stub(upload_single_binary, "future::plan", NULL)
  mockery::stub(upload_single_binary, "s3fs::s3_file_exists", TRUE)
  mockery::stub(
    upload_single_binary,
    "remote_object_state",
    mockery::mock(states[[1L]], states[[2L]])
  )
  mockery::stub(
    upload_single_binary,
    "s3fs::s3_file_upload",
    function(...) {
      captured <<- list(...)
      TRUE
    }
  )

  result <- upload_single_binary(
    package_name = "curl",
    tag = "7.1.0",
    s3_endpoint = "https://fake",
    s3_region = "us-east-1",
    s3_bucket = "bucket",
    s3_access_key_id = "fake",
    s3_secret_access_key = "fake"
  )

  list(result = result, args = captured)
}

test_that("a published binary still blocks the upload", {
  skip_if_not_installed("mockery")

  out <- upload_args_for(list("binary", "absent"))
  expect_null(out$args)
})

test_that("a source fallback does not block the upload", {
  # The whole point of a rebuild: the key is taken, but by the wrong thing.
  skip_if_not_installed("mockery")

  out <- upload_args_for(list("source", "absent"))
  expect_false(is.null(out$args))
})

test_that("replacing a source fallback overwrites it", {
  # Without overwrite the upload would fail against the object it must replace,
  # leaving the source published and the rebuild silently pointless.
  skip_if_not_installed("mockery")

  out <- upload_args_for(list("source", "absent"))
  expect_true(isTRUE(out$args$overwrite))
})

test_that("a first upload does not ask to overwrite", {
  skip_if_not_installed("mockery")

  out <- upload_args_for(list("absent", "absent"))
  expect_false(is.null(out$args))
  expect_null(out$args$overwrite)
})
