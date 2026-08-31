test_that("git runs with credential prompts disabled", {
  # A clone of a repository GitHub does not have returns 404, and git answers
  # by asking for a username. On a runner that surfaced as
  # "fatal: could not read Username for 'https://github.com'", which reads as a
  # permissions problem and is not one. Disabling prompts explicitly makes the
  # failure deterministic rather than dependent on the ambient git config.
  seen <- NULL
  local_mocked_bindings(
    system2 = function(command, args, env = character(), ...) {
      seen <<- env
      0L
    },
    .package = "base"
  )

  git_no_prompt(c("clone", "-q", "https://example.invalid/x", "/tmp/x"))

  expect_true("GIT_TERMINAL_PROMPT=0" %in% seen)
  expect_true("GIT_ASKPASS=echo" %in% seen)
})

test_that("cloning a package the mirror lacks fails with a message naming the cause", {
  # git exits 128 for a repository that does not exist. The exit status used to
  # be discarded, so the build carried on without a clone and failed later with
  # "cannot open the connection", pointing at the wrong thing.
  local_mocked_bindings(git_no_prompt = function(args, ...) 128L)

  expect_error(
    clone_package_repo("AsyPeer", "0.0.1", tempfile()),
    "cran mirror lags CRAN"
  )
})

test_that("a successful clone does not error", {
  local_mocked_bindings(git_no_prompt = function(args, ...) 0L)
  expect_no_error(clone_package_repo("jsonlite", "1.8.9", tempfile()))
})
