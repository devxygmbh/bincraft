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

test_that("built_stamp reproduces R's Built field format", {
  expect_identical(
    built_stamp(
      platform = "x86_64-pc-linux-musl",
      r_version = "4.5.3",
      time = as.POSIXct("2026-06-12 01:10:57", tz = "UTC")
    ),
    "R 4.5.3; x86_64-pc-linux-musl; 2026-06-12 01:10:57 UTC; unix"
  )
})

test_that("built_stamp rejects an unusable platform instead of stamping it", {
  # A stamp is written verbatim into every entry of a slot's PACKAGES index, and
  # uvr decides binary-vs-source by matching that triple. Stamping `NA` there
  # silently turns a whole slot source-only, so an unusable platform must fail
  # the index write rather than reach the index.
  expect_error(
    built_stamp(platform = NA, r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = NA_character_, r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = NULL, r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = "", r_version = "4.5.0"),
    "platform"
  )
  expect_error(
    built_stamp(platform = c("a", "b"), r_version = "4.5.0"),
    "platform"
  )
})

test_that("built_stamp rejects an unusable R version", {
  expect_error(
    built_stamp(platform = "x86_64-pc-linux-musl", r_version = NA),
    "r_version"
  )
  expect_error(
    built_stamp(platform = "x86_64-pc-linux-musl", r_version = ""),
    "r_version"
  )
})

test_that("built_stamp always formats the time in UTC", {
  # A non-UTC input time must still be rendered as its UTC wall-clock value so
  # the stamp is reproducible regardless of the build host's timezone.
  expect_match(
    built_stamp(
      platform = "aarch64-unknown-linux-musl",
      r_version = "4.4.2",
      time = as.POSIXct("2026-06-12 03:10:57", tz = "Europe/Berlin")
    ),
    "^R 4\\.4\\.2; aarch64-unknown-linux-musl; 2026-06-12 01:10:57 UTC; unix$"
  )
})

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

test_that("union_index_records points per-minor records at their slot", {
  # `available.packages()` keeps the contriburl it requested, not the one a
  # redirect served it, so the only way to steer a per-minor tarball is the
  # `Path` field the client folds into the `Repository` column.
  union <- union_index_records(
    minor_records = records(c(Package = "curl", File = "curl_7.1.0.tar.gz")),
    flat_records = records(c(
      Package = "jsonlite",
      File = "jsonlite_2.0.0.tar.gz"
    )),
    r_minor = "4.5"
  )

  expect_identical(unname(union[union[, "Package"] == "curl", "Path"]), "4.5")
  expect_true(is.na(union[union[, "Package"] == "jsonlite", "Path"]))
})

test_that("union_index_records lets the per-minor build shadow the generic one", {
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Version = "7.1.0")),
    flat_records = records(
      c(Package = "curl", Version = "6.2.3"),
      c(Package = "curl", Version = "6.3.0"),
      c(Package = "jsonlite", Version = "2.0.0")
    ),
    r_minor = "4.5"
  )

  expect_identical(
    unname(union[union[, "Package"] == "curl", "Version"]),
    "7.1.0"
  )
  expect_identical(sort(unique(union[, "Package"])), c("curl", "jsonlite"))
})

test_that("a per-minor source fallback does not hide a generic binary of the same minor", {
  # The per-minor build fell back to source, but the generic slot holds a
  # binary built under this very minor. Same ABI, already compiled: steering
  # the client to the source would be slower for no correctness gain.
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Version = "7.1.0")),
    flat_records = records(c(
      Package = "curl",
      Version = "7.1.0",
      Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
    )),
    r_minor = "4.5"
  )

  expect_identical(nrow(union), 1L)
  expect_true(is.na(union[union[, "Package"] == "curl", "Path"]))
  expect_match(
    unname(union[union[, "Package"] == "curl", "Built"]),
    "^R 4\\.5\\.3"
  )
})

test_that("a per-minor source fallback still shadows a generic binary of another minor", {
  # Here the generic binary was built under 4.4 while the client is on 4.5. For
  # an ABI-risky package that is the load-time failure the per-minor slot exists
  # to prevent, so the source fallback must win.
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Version = "7.1.0")),
    flat_records = records(c(
      Package = "curl",
      Version = "7.1.0",
      Built = "R 4.4.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
    )),
    r_minor = "4.5"
  )

  expect_identical(nrow(union), 1L)
  expect_identical(unname(union[union[, "Package"] == "curl", "Path"]), "4.5")
})

test_that("a per-minor binary still shadows a generic binary of the same minor", {
  union <- union_index_records(
    minor_records = records(c(
      Package = "curl",
      Version = "7.1.0",
      Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-08-01 00:00:00 UTC; unix"
    )),
    flat_records = records(c(
      Package = "curl",
      Version = "6.2.3",
      Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
    )),
    r_minor = "4.5"
  )

  expect_identical(nrow(union), 1L)
  expect_identical(
    unname(union[union[, "Package"] == "curl", "Version"]),
    "7.1.0"
  )
  expect_identical(unname(union[union[, "Package"] == "curl", "Path"]), "4.5")
})

test_that("clearing source stamps before the union is what lets the union see them", {
  # Regression: upload_package_index() used to clear the Built stamp *after*
  # merging, so every source fallback still looked like a binary at merge time
  # and the union kept steering clients to sources. The stamp is applied to the
  # whole slot up front, so a fallback carries one until it is cleared, and the
  # union reads Built to decide whether a per-minor record may hide a generic
  # binary. Order the two the wrong way round and the demotion silently no-ops.
  md5 <- c("curl_7.1.0" = "cran-source-md5")

  # A per-minor record that is really CRAN's source tarball, still stamped.
  minor <- records(c(
    Package = "curl",
    Version = "7.1.0",
    MD5sum = "cran-source-md5",
    Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-08-01 00:00:00 UTC; unix"
  ))
  flat <- records(c(
    Package = "curl",
    Version = "7.1.0",
    MD5sum = "a-real-binary",
    Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
  ))

  # Wrong order: union first, so the fallback still looks like a binary.
  wrong <- union_index_records(minor, flat, r_minor = "4.5")
  wrong <- clear_built_for_sources(wrong, md5_table = md5)
  expect_identical(unname(wrong[wrong[, "Package"] == "curl", "Path"]), "4.5")

  # Right order: clear first, so the union sees a source and keeps the binary.
  right <- clear_built_for_sources(minor, md5_table = md5)
  right <- union_index_records(right, flat, r_minor = "4.5")
  expect_true(is.na(right[right[, "Package"] == "curl", "Path"]))
  expect_identical(
    unname(right[right[, "Package"] == "curl", "MD5sum"]),
    "a-real-binary"
  )
})

test_that("a risky package is dropped rather than served from another minor", {
  # rlang on amd64/resolute: built for 4.4 and 4.5, never for 4.6, so the 4.6
  # union carried the generic 4.5.3 binary through. It installs and then dies
  # with `undefined symbol: SETLENGTH`. Absent a 4.6 object to point at, the
  # honest answer is that the package is unavailable.
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Version = "7.1.0")),
    flat_records = records(
      c(
        Package = "rlang",
        Version = "1.3.0",
        Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
      ),
      c(
        Package = "jsonlite",
        Version = "2.0.0",
        Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
      )
    ),
    r_minor = "4.6",
    risky_packages = "rlang"
  )

  expect_false("rlang" %in% union[, "Package"])
  # jsonlite is not risky, so a generic binary of any minor is fine for it.
  expect_true("jsonlite" %in% union[, "Package"])
})

test_that("a risky package built for this very minor is kept", {
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Version = "7.1.0")),
    flat_records = records(c(
      Package = "rlang",
      Version = "1.3.0",
      Built = "R 4.6.0; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
    )),
    r_minor = "4.6",
    risky_packages = "rlang"
  )

  expect_true("rlang" %in% union[, "Package"])
})

test_that("an empty risky set leaves behaviour unchanged", {
  # A slot listing that failed must degrade to today's behaviour, never to
  # dropping packages wholesale.
  args <- list(
    minor_records = records(c(Package = "curl", Version = "7.1.0")),
    flat_records = records(c(
      Package = "rlang",
      Version = "1.3.0",
      Built = "R 4.5.3; x86_64-pc-linux-gnu; 2026-07-26 11:54:49 UTC; unix"
    )),
    r_minor = "4.6"
  )

  union <- do.call(union_index_records, args)
  expect_true("rlang" %in% union[, "Package"])
})

test_that("union_index_records keeps every column of both inputs", {
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Built = "R 4.5.3")),
    flat_records = records(c(Package = "jsonlite", MD5sum = "abc")),
    r_minor = "4.5"
  )

  expect_true(all(c("Package", "Built", "MD5sum", "Path") %in% colnames(union)))
})

test_that("union_index_records carries an empty per-minor slot", {
  union <- union_index_records(
    minor_records = records(c(Package = "curl", Version = "7.1.0"))[
      0L,
      ,
      drop = FALSE
    ],
    flat_records = records(c(Package = "jsonlite", Version = "2.0.0")),
    r_minor = "4.5"
  )

  expect_identical(nrow(union), 1L)
  expect_true(is.na(union[1L, "Path"]))
})

test_that("union_index_records refuses to union an empty generic index", {
  # An unreadable generic index must not be published as a per-minor-only one:
  # every client on that minor would silently lose the other ~21k packages.
  expect_error(
    union_index_records(
      minor_records = records(c(Package = "curl")),
      flat_records = records(c(Package = "curl"))[0L, , drop = FALSE],
      r_minor = "4.5"
    ),
    "generic index"
  )
})

test_that("union_index_records rejects an unusable r_minor", {
  minor <- records(c(Package = "curl"))
  flat <- records(c(Package = "jsonlite"))

  expect_error(union_index_records(minor, flat, "4.5.3"), "r_minor")
  expect_error(union_index_records(minor, flat, NA_character_), "r_minor")
  expect_error(union_index_records(minor, flat, c("4.5", "4.6")), "r_minor")
})

test_that("write_index_files rewrites all three index files in step", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  union <- union_index_records(
    minor_records = records(
      c(Package = "curl", Version = "7.1.0", File = "curl_7.1.0.tar.gz")
    ),
    flat_records = records(
      c(Package = "jsonlite", Version = "2.0.0", File = "jsonlite_2.0.0.tar.gz")
    ),
    r_minor = "4.5"
  )

  n <- write_index_files(dir, union)

  expect_identical(n, 2L)

  from_rds <- readRDS(file.path(dir, "PACKAGES.rds"))
  from_dcf <- read.dcf(file.path(dir, "PACKAGES"))
  from_gz <- read.dcf(gzfile(file.path(dir, "PACKAGES.gz")))

  # All three are read by different clients; they must agree, or a client's
  # view of the slot depends on which file it happened to fetch.
  expect_identical(sort(from_rds[, "Package"]), c("curl", "jsonlite"))
  expect_identical(sort(from_dcf[, "Package"]), c("curl", "jsonlite"))
  expect_identical(sort(from_gz[, "Package"]), c("curl", "jsonlite"))
  expect_identical(
    unname(from_rds[from_rds[, "Package"] == "curl", "Path"]),
    "4.5"
  )
  expect_identical(
    unname(from_dcf[from_dcf[, "Package"] == "curl", "Path"]),
    "4.5"
  )
})
