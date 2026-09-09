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

size_table <- c("Deriv_4.3.0" = 44850, "curl_6.0.0" = 1200)

test_that("is_archived_source_tarball matches an archived tarball by exact size", {
  expect_true(is_archived_source_tarball("Deriv", "4.3.0", 44850, size_table))
  expect_false(is_archived_source_tarball("Deriv", "4.3.0", 44851, size_table))
})

test_that("is_archived_source_tarball answers FALSE whenever it cannot know", {
  # A version CRAN still ships is not in the archive index, and an object whose
  # size was not listed cannot be compared. Guessing either way would clear the
  # stamp on a real binary and send every client to compile it from source.
  expect_false(is_archived_source_tarball("Deriv", "4.4.0", 44850, size_table))
  expect_false(is_archived_source_tarball(
    "Deriv",
    "4.3.0",
    NA_real_,
    size_table
  ))
})

test_that("clear_built_for_sources clears an archived source fallback given sizes", {
  # The case seen in production: `Deriv 4.3.0` published as CRAN's source under
  # a per-minor path, stamped as a binary because CRAN's current index knows
  # only 4.3.5, and rejected by uvr as "not a built binary package".
  index <- records(
    c(
      Package = "Deriv",
      Version = "4.3.0",
      MD5sum = "379732250a50bde145f56415bda818f6",
      File = "Deriv_4.3.0.tar.gz",
      Built = "R 4.4.0; x86_64-pc-linux-gnu; 2026-07-30; unix"
    ),
    c(
      Package = "curl",
      Version = "7.1.0",
      MD5sum = "0000deadbeef",
      File = "curl_7.1.0.tar.gz",
      Built = "R 4.4.0; x86_64-pc-linux-gnu; 2026-07-30; unix"
    )
  )

  out <- clear_built_for_sources(
    index,
    md5_table = md5_table,
    sizes = c("Deriv_4.3.0.tar.gz" = 44850, "curl_7.1.0.tar.gz" = 99999),
    size_table = size_table
  )

  expect_true(is.na(out[1L, "Built"]))
  expect_false(is.na(out[2L, "Built"]))
})

test_that("clear_built_for_sources leaves archived versions alone without sizes", {
  index <- records(c(
    Package = "Deriv",
    Version = "4.3.0",
    MD5sum = "379732250a50bde145f56415bda818f6",
    File = "Deriv_4.3.0.tar.gz",
    Built = "R 4.4.0; x86_64-pc-linux-gnu; 2026-07-30; unix"
  ))

  out <- clear_built_for_sources(
    index,
    md5_table = md5_table,
    size_table = size_table
  )

  expect_false(is.na(out[1L, "Built"]))
})
