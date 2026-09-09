test_that("a poisoned-cache sync is retried once without the cache", {
  # build_binary_package() runs many times in one container against a shared uvr
  # cache, so one bad download of a popular dependency breaks every later
  # package needing it. uvr names the remedy in its own error; take it.
  calls <- list()
  local_mocked_bindings(
    run_uvr = function(args, wd, env = character()) {
      calls[[length(calls) + 1L]] <<- args
      if (!("--ignore-cache" %in% args)) {
        stop(
          "uvr sync failed (exit 1):\n x ERROR  Failed to install arules\n  Archive for 'arules' is not a built binary package (no Meta/package.rds)",
          call. = FALSE
        )
      }
      invisible("ok")
    }
  )

  expect_no_error(sync_with_cache_retry("/tmp/pkg", "/tmp/lib"))
  expect_length(calls, 2L)
  expect_false("--ignore-cache" %in% calls[[1L]])
  expect_true("--ignore-cache" %in% calls[[2L]])
})

test_that("any other sync failure is not retried", {
  # A missing system library or a compile error will fail again identically.
  # Retrying only costs time and buries the real message.
  calls <- 0L
  local_mocked_bindings(
    run_uvr = function(args, wd, env = character()) {
      calls <<- calls + 1L
      stop(
        "uvr sync failed (exit 1):\n error: could not find -lgsl",
        call. = FALSE
      )
    }
  )

  expect_error(
    sync_with_cache_retry("/tmp/pkg", "/tmp/lib"),
    "could not find -lgsl"
  )
  expect_identical(calls, 1L)
})

test_that("a sync that works is not retried", {
  calls <- 0L
  local_mocked_bindings(
    run_uvr = function(args, wd, env = character()) {
      calls <<- calls + 1L
      invisible("ok")
    }
  )

  expect_no_error(sync_with_cache_retry("/tmp/pkg", "/tmp/lib"))
  expect_identical(calls, 1L)
})
