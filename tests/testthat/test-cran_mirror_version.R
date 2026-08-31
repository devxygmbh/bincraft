test_that("a 404 from the mirror returns NA without retrying", {
  # The mirror lags CRAN, so a package published hours ago has no repository
  # there. That 404 is permanent: retried through `retry_config` it costs ten
  # attempts on a backoff capped at 60s and then aborts the caller. AsyPeer
  # 0.0.1 took out an entire build that way.
  calls <- 0L
  local_mocked_bindings(
    gh = function(...) {
      calls <<- calls + 1L
      rlang::abort(
        "Not Found",
        class = c("github_error", "http_error_404")
      )
    },
    .package = "gh"
  )

  elapsed <- system.time(result <- cran_mirror_version("AsyPeer"))[["elapsed"]]

  expect_true(is.na(result))
  expect_identical(calls, 1L)
  # Ten retries would take minutes; one attempt is instant.
  expect_lt(elapsed, 5)
})

test_that("a transient failure is still retried", {
  calls <- 0L
  local_mocked_bindings(
    gh = function(...) {
      calls <<- calls + 1L
      if (calls < 3L) {
        rlang::abort("boom", class = c("github_error", "http_error_500"))
      }
      list(list(commit = list(message = "version 1.2.3")))
    },
    .package = "gh"
  )

  expect_identical(
    cran_mirror_version(
      "somepkg",
      rate = purrr::rate_backoff(
        pause_base = 0,
        pause_cap = 0,
        pause_min = 0,
        max_times = 5L,
        jitter = FALSE
      )
    ),
    "1.2.3"
  )
  expect_identical(calls, 3L)
})

test_that("the published version is read from the commit message", {
  local_mocked_bindings(
    gh = function(...) list(list(commit = list(message = "version 4.5.6"))),
    .package = "gh"
  )
  expect_identical(cran_mirror_version("somepkg"), "4.5.6")
})
