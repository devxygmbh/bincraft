test_that("find_old_versions falls back to pkgsearch::cran_package_history", {
  # When the latest version is not among the archived tarballs, the fallback
  # queries CRAN version history via pkgsearch and archives everything older
  # than the newest known release.
  all_versions <- c("demo_1.0.0.tar.gz", "demo_1.1.0.tar.gz")
  # Run the retried expression once, without forcing `rate` (so the test needs
  # neither purrr's backoff config nor a live network).
  mockery::stub(
    find_old_versions,
    "purrr::insistently",
    function(.f, ...) function() eval(.f[[2L]], environment(.f))
  )
  mockery::stub(
    find_old_versions,
    "pkgsearch::cran_package_history",
    function(pkg) {
      data.frame(
        Version = c("1.0.0", "1.1.0", "2.0.0"),
        stringsAsFactors = FALSE
      )
    }
  )
  res <- find_old_versions(
    all_versions = all_versions,
    package_name = "demo",
    package_name_local = "demo",
    last_version = "2.0.0"
  )
  # Newest known version is 1.1.0 among the tarballs; it is kept, older archived.
  expect_equal(res$old_versions, "demo_1.0.0.tar.gz")
  expect_equal(res$index, 2L)
})
