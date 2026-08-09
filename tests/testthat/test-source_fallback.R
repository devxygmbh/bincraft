records <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  out <- matrix(
    NA_character_,
    length(rows),
    length(cols),
    dimnames = list(NULL, cols)
  )
  for (i in seq_along(rows)) {
    out[i, names(rows[[i]])] <- unname(rows[[i]])
  }
  out
}

md5_table <- c(
  "curl_7.1.0" = "8af2ccbf5d85dc18866f45f1f26f348d",
  "jsonlite_2.0.0" = "3e54e6fbc0c9063936e3d01e91419c14"
)

test_that("is_cran_source_tarball recognises an object byte-identical to CRAN's", {
  expect_true(is_cran_source_tarball(
    "curl",
    "7.1.0",
    "8af2ccbf5d85dc18866f45f1f26f348d",
    md5_table
  ))
  expect_false(is_cran_source_tarball(
    "curl",
    "7.1.0",
    "0000deadbeef",
    md5_table
  ))
})

test_that("is_cran_source_tarball ignores case in the MD5", {
  expect_true(is_cran_source_tarball(
    "curl",
    "7.1.0",
    toupper("8af2ccbf5d85dc18866f45f1f26f348d"),
    md5_table
  ))
})

test_that("is_cran_source_tarball answers FALSE whenever it cannot know", {
  # An archived version is absent from CRAN's index, and an object with no
  # recorded MD5 cannot be compared. Neither may be reported as a source
  # fallback: that would flag a real binary as missing and rebuild it forever.
  expect_false(is_cran_source_tarball(
    "curl",
    "6.0.0",
    "8af2ccbf5d85dc18866f45",
    md5_table
  ))
  expect_false(is_cran_source_tarball(
    "curl",
    "7.1.0",
    NA_character_,
    md5_table
  ))
  expect_false(is_cran_source_tarball("nosuchpkg", "1.0", "abc", md5_table))
})

test_that("is_cran_source_tarball is vectorised over records", {
  expect_identical(
    is_cran_source_tarball(
      c("curl", "jsonlite"),
      c("7.1.0", "2.0.0"),
      c("8af2ccbf5d85dc18866f45f1f26f348d", "not-the-cran-md5"),
      md5_table
    ),
    c(TRUE, FALSE)
  )
})

test_that("clear_built_for_sources unstamps only the CRAN sources", {
  index <- records(
    c(
      Package = "curl",
      Version = "7.1.0",
      MD5sum = "8af2ccbf5d85dc18866f45f1f26f348d",
      Built = "R 4.5.3; x86_64-pc-linux-musl; 2026-07-31 12:47:24 UTC; unix"
    ),
    c(
      Package = "jsonlite",
      Version = "2.0.0",
      MD5sum = "a-real-build",
      Built = "R 4.5.3; x86_64-pc-linux-musl; 2026-07-31 12:47:24 UTC; unix"
    )
  )

  out <- clear_built_for_sources(index, md5_table)

  expect_true(is.na(out[out[, "Package"] == "curl", "Built"]))
  expect_match(
    unname(out[out[, "Package"] == "jsonlite", "Built"]),
    "^R 4\\.5\\.3;"
  )
})

test_that("clear_built_for_sources leaves an index without the fields alone", {
  index <- records(c(Package = "curl", Version = "7.1.0"))
  expect_identical(clear_built_for_sources(index, md5_table), index)
})

test_that("clear_built_for_sources handles an empty index", {
  index <- records(
    c(Package = "curl", Version = "1", MD5sum = "x", Built = "y")
  )[0L, , drop = FALSE]
  expect_identical(nrow(clear_built_for_sources(index, md5_table)), 0L)
})

test_that("an empty CRAN table clears nothing", {
  # CRAN being unreachable must not strip the whole repository's stamps.
  index <- records(
    c(
      Package = "curl",
      Version = "7.1.0",
      MD5sum = "8af2ccbf5d85dc18866f45f1f26f348d",
      Built = "R 4.5.3; x86_64-pc-linux-musl; 2026-07-31 12:47:24 UTC; unix"
    )
  )

  out <- clear_built_for_sources(
    index,
    stats::setNames(character(), character())
  )

  expect_false(is.na(out[1L, "Built"]))
})

test_that("remote_object_md5 reads the column s3fs actually returns", {
  # s3fs snake-cases head_object's response and renames `e_tag` to `etag`, so
  # reading `ETag` yields NULL rather than an error. That turned every object
  # into "cannot tell, assume binary" and made the whole check inert.
  skip_if_not_installed("mockery")

  info <- data.frame(
    bucket_name = "b",
    key = "k",
    etag = "\"ea8127d953ca6a2f118ea49441772af6\"",
    stringsAsFactors = FALSE
  )
  mockery::stub(remote_object_md5, "s3fs::s3_file_info", info)

  expect_identical(
    remote_object_md5("s3://b/k"),
    "ea8127d953ca6a2f118ea49441772af6"
  )
})

test_that("remote_object_md5 still reads an ETag column", {
  skip_if_not_installed("mockery")

  info <- data.frame(
    ETag = "\"ea8127d953ca6a2f118ea49441772af6\"",
    stringsAsFactors = FALSE
  )
  mockery::stub(remote_object_md5, "s3fs::s3_file_info", info)

  expect_identical(
    remote_object_md5("s3://b/k"),
    "ea8127d953ca6a2f118ea49441772af6"
  )
})

test_that("remote_object_md5 reports a multipart ETag as unknown", {
  skip_if_not_installed("mockery")

  info <- data.frame(etag = "\"abc123-7\"", stringsAsFactors = FALSE)
  mockery::stub(remote_object_md5, "s3fs::s3_file_info", info)

  expect_true(is.na(remote_object_md5("s3://b/k")))
})

test_that("remote_object_md5 reports an unreadable object as unknown", {
  skip_if_not_installed("mockery")

  mockery::stub(remote_object_md5, "s3fs::s3_file_info", function(...) {
    stop("no credentials")
  })

  expect_true(is.na(remote_object_md5("s3://b/k")))
})

test_that("remote_object_state distinguishes absent, source and binary", {
  skip_if_not_installed("mockery")

  mockery::stub(remote_object_state, "s3fs::s3_file_exists", FALSE)
  expect_identical(remote_object_state("s3://b/k", "curl", "7.1.0"), "absent")
})

test_that("remote_object_state calls a matching MD5 a source", {
  skip_if_not_installed("mockery")

  mockery::stub(remote_object_state, "s3fs::s3_file_exists", TRUE)
  mockery::stub(
    remote_object_state,
    "remote_object_md5",
    "8af2ccbf5d85dc18866f45f1f26f348d"
  )
  mockery::stub(remote_object_state, "is_cran_source_tarball", TRUE)

  expect_identical(remote_object_state("s3://b/k", "curl", "7.1.0"), "source")
})

test_that("remote_object_state calls a differing MD5 a binary", {
  skip_if_not_installed("mockery")

  mockery::stub(remote_object_state, "s3fs::s3_file_exists", TRUE)
  mockery::stub(remote_object_state, "remote_object_md5", "a-real-build")
  mockery::stub(remote_object_state, "is_cran_source_tarball", FALSE)

  expect_identical(remote_object_state("s3://b/k", "curl", "7.1.0"), "binary")
})

test_that("remote_object_state calls an unknown MD5 a binary", {
  # Never let an unreadable ETag or an unreachable CRAN schedule a rebuild of
  # the whole repository.
  skip_if_not_installed("mockery")

  mockery::stub(remote_object_state, "s3fs::s3_file_exists", TRUE)
  mockery::stub(remote_object_state, "remote_object_md5", NA_character_)

  expect_identical(remote_object_state("s3://b/k", "curl", "7.1.0"), "binary")
})
