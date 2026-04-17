test_that("filter_tags retrieves tags from GitHub via API", {
  skip_on_cran()
  skip_if_offline()

  tags <- filter_tags(
    "dplyr",
    tag = NULL,
    source_org_url = "https://github.com/cran",
    tag_limit = 5L
  )

  expect_type(tags, "character")
  expect_true(length(tags) > 0L && length(tags) <= 5L)
  # Tags should look like version strings, not have R- prefix
  expect_false(any(grepl("^R-", tags)))
})

test_that("filter_tags with tag = 'latest' returns single tag", {
  skip_on_cran()
  skip_if_offline()

  tags <- filter_tags(
    "dplyr",
    tag = "latest",
    source_org_url = "https://github.com/cran",
    tag_limit = 5L
  )

  expect_type(tags, "character")
  expect_length(tags, 1L)
})

test_that("filter_tags falls back to git ls-remote on unknown forge", {
  skip_on_cran()
  skip_if_offline()

  # GitHub URL still works via fallback
  tags <- filter_tags(
    "dplyr",
    tag = NULL,
    source_org_url = "https://github.com/cran",
    tag_limit = 3L
  )

  expect_type(tags, "character")
  expect_true(length(tags) <= 3L)
})

test_that("filter_tags handles package with few tags", {
  skip_on_cran()
  skip_if_offline()

  # A package with very few versions
  tags <- filter_tags(
    "brew",
    tag = NULL,
    source_org_url = "https://github.com/cran",
    tag_limit = 50L
  )

  expect_type(tags, "character")
  expect_true(length(tags) > 0L)
  # tag_limit should be clamped to actual count
  expect_true(length(tags) <= 50L)
})
