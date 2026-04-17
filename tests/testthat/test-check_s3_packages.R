test_that("check_s3_packages skips S3 calls when s3_package_cache is provided and all exist", {
  skip_on_cran()

  # Simulate a cache that contains the package we're checking
  cache <- c("testpkg_1.0.0.tar.gz", "testpkg_0.9.0.tar.gz", "testpkg_0.8.0.tar.gz")

  result <- check_s3_packages(
    package_name = "testpkg",
    tag = c("1.0.0", "0.9.0", "0.8.0"),
    source_org_url = "https://github.com/cran",
    tag_limit = 3L,
    is_r_minor_sensitive = FALSE,
    s3_bucket = "fake-bucket",
    s3_access_key_id = "fake",
    s3_secret_access_key = "fake",
    s3_endpoint = "https://fake.endpoint.com",
    s3_region = "us-east-1",
    store_build_metadata = FALSE,
    metadata_db_type = "postgres",
    metadata_db_host = NULL,
    metadata_db_name = NULL,
    metadata_db_table = NULL,
    metadata_db_port = NULL,
    metadata_db_user = NULL,
    metadata_db_password = NULL,
    metadata_db_sslmode = NULL,
    platform = "alpine-321",
    arch = "arm64",
    codename = "alpine321",
    s3_package_cache = cache
  )

  expect_true(result$should_skip)
})

test_that("check_s3_packages returns missing versions when cache has partial matches", {
  skip_on_cran()

  # Cache only has 2 of 3 versions
  cache <- c("testpkg_1.0.0.tar.gz", "testpkg_0.9.0.tar.gz")

  result <- check_s3_packages(
    package_name = "testpkg",
    tag = c("1.0.0", "0.9.0", "0.8.0"),
    source_org_url = "https://github.com/cran",
    tag_limit = 3L,
    is_r_minor_sensitive = FALSE,
    s3_bucket = "fake-bucket",
    s3_access_key_id = "fake",
    s3_secret_access_key = "fake",
    s3_endpoint = "https://fake.endpoint.com",
    s3_region = "us-east-1",
    store_build_metadata = FALSE,
    metadata_db_type = "postgres",
    metadata_db_host = NULL,
    metadata_db_name = NULL,
    metadata_db_table = NULL,
    metadata_db_port = NULL,
    metadata_db_user = NULL,
    metadata_db_password = NULL,
    metadata_db_sslmode = NULL,
    platform = "alpine-321",
    arch = "arm64",
    codename = "alpine321",
    s3_package_cache = cache
  )

  expect_false(result$should_skip)
  expect_identical(result$filtered_tags, "0.8.0")
})
