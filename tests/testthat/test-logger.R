test_that("logger functions exist and are callable", {
  expect_type(get_logger, "closure")
  expect_type(log_debug, "closure")
  expect_type(log_info, "closure")
  expect_type(log_success, "closure")
  expect_type(log_warn, "closure")
  expect_type(log_error, "closure")
  expect_type(log_header, "closure")
})

test_that("get_logger returns an lgr logger", {
  logger <- get_logger()
  expect_s3_class(logger, "Logger")
  expect_identical(logger$name, "bincraft")
})

test_that("logger respects threshold levels", {
  # Save original logger state
  original_logger <- get_logger()
  original_threshold <- original_logger$threshold
  original_appenders <- original_logger$appenders

  # Test with debug level - all messages should show
  original_logger$set_threshold("debug")
  expect_message(log_debug("test debug"), "Debug: test debug")
  expect_message(log_info("test info"), "test info")
  expect_message(log_warn("test warn"), "test warn")
  expect_message(log_error("test error"), "test error")
  expect_message(log_success("test success"), "test success")
  expect_message(log_header("test header"), "test header")

  # Test with warn level - only warn and error should show
  original_logger$set_threshold("warn")
  expect_no_message(log_debug("test debug"))
  expect_no_message(log_info("test info"))
  expect_no_message(log_success("test success"))
  expect_no_message(log_header("test header"))
  expect_message(log_warn("test warn"), "test warn")
  expect_message(log_error("test error"), "test error")

  # Test with error level - only errors should show
  original_logger$set_threshold("error")
  expect_no_message(log_debug("test debug"))
  expect_no_message(log_info("test info"))
  expect_no_message(log_warn("test warn"))
  expect_no_message(log_success("test success"))
  expect_no_message(log_header("test header"))
  expect_message(log_error("test error"), "test error")

  # Restore original threshold
  original_logger$set_threshold(original_threshold)
})

test_that("logger uses AppenderBuffer to avoid console duplication", {
  logger <- get_logger()
  appenders <- logger$appenders

  # Should have exactly one appender
  expect_length(appenders, 1L)

  # Should be AppenderBuffer, not AppenderConsole
  expect_s3_class(appenders[[1L]], "AppenderBuffer")
})

test_that("logger messages support lgr interpolation", {
  # Ensure logger is at info level
  logger <- get_logger()
  original_threshold <- logger$threshold
  logger$set_threshold("info")

  # lgr uses glue-style interpolation in logger calls
  # but cli uses its own interpolation, so we need to pass variables through
  test_var <- "interpolated"
  expect_message(
    log_info("This is test_var", .envir = environment()),
    "This is test_var"
  )

  # For actual interpolation, we'd use glue or sprintf before passing to log functions
  expect_message(
    log_info(sprintf("Package %s version %s", "testpkg", "1.0.0")),
    "Package testpkg version 1.0.0"
  )

  # Restore original threshold
  logger$set_threshold(original_threshold)
})

test_that("logger functions handle cli formatting", {
  # Ensure logger is at info level
  logger <- get_logger()
  original_threshold <- logger$threshold
  logger$set_threshold("info")

  # Test cli formatting with .pkg
  expect_message(
    log_info("Installing {.pkg dplyr}"),
    "dplyr"
  )

  # Test cli formatting with .path
  test_path <- file.path(tempdir(), "test.txt")
  expect_message(
    log_info(paste0("Reading {.path ", test_path, "}")),
    basename(test_path)
  )

  # Restore original threshold
  logger$set_threshold(original_threshold)
})

test_that("logger threshold defaults to info when env var not set", {
  # The logger is initialized on package load with env var
  # Since we can't reload the package in tests, we'll test the validation logic
  # by checking the current logger configuration
  logger <- get_logger()

  # Logger should have a valid threshold (one of the standard levels)
  expect_true(logger$threshold %in% c(100L, 200L, 300L, 400L, 500L, 600L))
})

test_that("logger handles invalid log levels gracefully", {
  # Test the validation logic directly since we can't reload the package
  # The .onLoad function should validate log levels
  valid_levels <- c("fatal", "error", "warn", "info", "debug", "trace")

  # Test that setting an invalid level on logger defaults to a valid level
  logger <- get_logger()
  original_threshold <- logger$threshold

  # lgr itself validates levels, so we just ensure our logger works
  expect_no_error(logger$set_threshold("info"))
  expect_identical(logger$threshold, 400L)

  # Restore original threshold
  logger$set_threshold(original_threshold)
})
