test_that("abi_classifier_signature is a stable, non-empty scalar", {
  sig1 <- abi_classifier_signature()
  sig2 <- abi_classifier_signature()

  expect_type(sig1, "character")
  expect_length(sig1, 1L)
  expect_true(nzchar(sig1))
  expect_identical(sig1, sig2)
  # embeds the logic version so a logic bump invalidates the cache
  expect_true(startsWith(sig1, paste0(abi_classifier_logic_version, "-")))
})

test_that("cache lookup is a no-op (returns NULL) when no DB is configured", {
  expect_null(
    abi_cache_lookup(
      "dplyr",
      "1.1.4",
      abi_classifier_signature(),
      metadata_db_host = NULL,
      metadata_db_name = NULL
    )
  )
})

test_that("cache store is a no-op when no DB is configured", {
  expect_false(
    abi_cache_store(
      "dplyr",
      "1.1.4",
      abi_classifier_signature(),
      TRUE,
      metadata_db_host = NULL,
      metadata_db_name = NULL
    )
  )
})

test_that("abi_cache_connect returns NULL for a non-postgres db type", {
  expect_null(
    abi_cache_connect(
      metadata_db_type = "sqlite",
      metadata_db_host = "irrelevant",
      metadata_db_name = "irrelevant",
      metadata_db_port = NULL,
      metadata_db_user = NULL,
      metadata_db_password = NULL,
      metadata_db_sslmode = NULL
    )
  )
})
