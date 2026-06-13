test_that("package_index_remote_dir builds the generic slot when r_minor is NULL", {
  expect_identical(
    package_index_remote_dir("bucket", "amd64", "alpine323"),
    file.path("bucket", "amd64", "alpine323", "latest", "src", "contrib")
  )
})

test_that("package_index_remote_dir appends the minor slot when r_minor is set", {
  expect_identical(
    package_index_remote_dir("bucket", "amd64", "alpine323", r_minor = "4.4"),
    file.path("bucket", "amd64", "alpine323", "latest", "src", "contrib", "4.4")
  )
})
