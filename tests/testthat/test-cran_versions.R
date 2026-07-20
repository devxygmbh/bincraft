test_that("is_cran_source recognises CRAN mirrors and rejects forges", {
  expect_true(is_cran_source("https://github.com/cran"))
  expect_true(is_cran_source("https://cloud.r-project.org"))
  expect_true(is_cran_source("https://cran.r-project.org"))
  expect_false(is_cran_source("https://codefloe.com/rpkgs"))
  expect_false(is_cran_source("https://github.com/tidyverse"))
})

test_that("fetch_cran_versions returns the current version for tag_limit = 1", {
  skip_on_cran()
  skip_if_offline()

  versions <- fetch_cran_versions("dplyr", tag_limit = 1L)

  expect_type(versions, "character")
  expect_length(versions, 1L)
  # looks like a version string
  expect_match(versions, "^[0-9]")
})

test_that("fetch_cran_versions returns newest-first, capped at tag_limit", {
  skip_on_cran()
  skip_if_offline()

  versions <- fetch_cran_versions("brew", tag_limit = 5L)

  expect_type(versions, "character")
  expect_true(length(versions) >= 1L && length(versions) <= 5L)
  # already sorted newest-first
  expect_identical(
    versions,
    versions[order(numeric_version(versions), decreasing = TRUE)]
  )
})

test_that("fetch_cran_versions returns empty for an unknown package", {
  skip_on_cran()
  skip_if_offline()

  versions <- fetch_cran_versions(
    "this_package_does_not_exist_on_cran_xyz",
    tag_limit = 1L
  )
  expect_length(versions, 0L)
})

test_that("filter_tags resolves CRAN packages without the GitHub API", {
  skip_on_cran()
  skip_if_offline()

  # source_org_url points at the CRAN mirror -> CRAN-metadata path, no gh::gh
  tags <- filter_tags(
    "dplyr",
    tag = "latest",
    source_org_url = "https://github.com/cran",
    tag_limit = 5L
  )
  expect_type(tags, "character")
  expect_length(tags, 1L)
  expect_false(any(grepl("^R-", tags)))
})
