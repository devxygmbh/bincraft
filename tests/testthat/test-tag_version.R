test_that("tag_version returns the version a tag names", {
  expect_identical(tag_version("v5.0.0"), "5.0.0")
  expect_identical(tag_version("5.0.0"), "5.0.0")
  expect_identical(tag_version("4.5.2.9999"), "4.5.2.9999")
  expect_identical(tag_version("1.0.0-3"), "1.0.0-3")
})

test_that("tag_version returns NA for refs that name no version", {
  expect_true(is.na(tag_version("main")))
  expect_true(is.na(tag_version("4f3a2b1")))
  expect_true(is.na(tag_version("v2.0.0-rc1")))
  expect_true(is.na(tag_version("v1")))
  expect_true(is.na(tag_version("")))
})

test_that("tag_version is vectorised", {
  expect_identical(
    tag_version(c("v5.0.0", "main", "1.2.3")),
    c("5.0.0", NA_character_, "1.2.3")
  )
})

test_that("artifact_version falls back to the tag itself", {
  expect_identical(
    artifact_version(c("v5.0.0", "main")),
    c("5.0.0", "main")
  )
})

test_that("stamp_description_version rewrites the Version field", {
  clone_dir <- withr::local_tempdir()
  writeLines(
    c(
      "Package: bincraft",
      "Version: 4.5.2.9999",
      "Title: Toolbox",
      "Description: Spans",
      "    two lines."
    ),
    file.path(clone_dir, "DESCRIPTION")
  )

  old <- stamp_description_version(clone_dir, "5.0.0")

  expect_identical(old, "4.5.2.9999")
  expect_identical(
    readLines(file.path(clone_dir, "DESCRIPTION")),
    c(
      "Package: bincraft",
      "Version: 5.0.0",
      "Title: Toolbox",
      "Description: Spans",
      "    two lines."
    )
  )
})

test_that("stamp_description_version leaves a matching version alone", {
  clone_dir <- withr::local_tempdir()
  desc <- c("Package: bincraft", "Version: 5.0.0")
  writeLines(desc, file.path(clone_dir, "DESCRIPTION"))

  expect_identical(stamp_description_version(clone_dir, "5.0.0"), "5.0.0")
  expect_identical(readLines(file.path(clone_dir, "DESCRIPTION")), desc)
})

test_that("stamp_description_version is a no-op without a Version field", {
  clone_dir <- withr::local_tempdir()
  desc <- c("Package: bincraft", "Title: Toolbox")
  writeLines(desc, file.path(clone_dir, "DESCRIPTION"))

  expect_true(is.na(stamp_description_version(clone_dir, "5.0.0")))
  expect_identical(readLines(file.path(clone_dir, "DESCRIPTION")), desc)
})

test_that("stamp_description_version is a no-op without a DESCRIPTION", {
  clone_dir <- withr::local_tempdir()
  expect_true(is.na(stamp_description_version(clone_dir, "5.0.0")))
})
