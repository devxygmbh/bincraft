test_that("nm output is reduced to bare symbol names", {
  # `nm -D` prints "<address> <type> <name>", with the address blank for
  # undefined symbols, and versioned names carry an @GLIBC suffix.
  lines <- c(
    "0000000000004010 T SETLENGTH",
    "                 U Rf_allocVector",
    "                 U __cxa_finalize@GLIBC_2.2.5",
    "0000000000004020 T R_getVar",
    "",
    "   "
  )
  expect_identical(
    parse_nm_output(lines),
    c("__cxa_finalize", "R_getVar", "Rf_allocVector", "SETLENGTH")
  )
})

test_that("the volatile set is what some minors export and others do not", {
  sets <- list(
    "4.4" = c("Rf_allocVector", "SETLENGTH", "R_ClosureEnv"),
    "4.5" = c("Rf_allocVector", "SETLENGTH"),
    "4.6" = c("Rf_allocVector", "R_getVar")
  )
  # Rf_allocVector is in all three, so it is stable. Everything else is missing
  # from at least one minor.
  expect_identical(
    cross_minor_volatile_symbols(sets),
    c("R_ClosureEnv", "R_getVar", "SETLENGTH")
  )
})

test_that("a diff needs at least two minors", {
  expect_null(cross_minor_volatile_symbols(list("4.6" = c("a", "b"))))
  expect_null(cross_minor_volatile_symbols(NULL))
})

test_that("a package with no compiled code is not sensitive", {
  # Absence of shared objects is an answer, not a failure to inspect: with
  # nothing to dyn.load() there is no ABI to mismatch.
  sets <- list("4.5" = c("a", "SETLENGTH"), "4.6" = "a")
  v <- shared_object_abi_verdict(character(0), sets)
  expect_true(v$inspected)
  expect_false(v$sensitive)
})

test_that("no judgement is made without symbol sets to compare", {
  # This is the fail-soft path: the caller's source-level verdict must stand
  # rather than be overridden by a guess.
  v <- shared_object_abi_verdict("/nonexistent.so", NULL)
  expect_false(v$inspected)
  expect_true(is.na(v$sensitive))
})

test_that("a missing tarball yields no judgement", {
  v <- tarball_abi_verdict(tempfile(), list("4.5" = "a", "4.6" = "a"))
  expect_false(v$inspected)
  expect_true(is.na(v$sensitive))
})

test_that("the verdict names the minors that cannot load the binary", {
  # base64enc's shape: it references SETLENGTH, which 4.4 and 4.5 export and 4.6
  # does not, so it is sensitive and 4.6 is the minor that cannot take it.
  skip_if_not(nzchar(Sys.which("nm")), "nm is unavailable")
  skip_if_not(nzchar(Sys.which("cc")) || nzchar(Sys.which("gcc")), "no C compiler")

  dir <- tempfile()
  dir.create(dir)
  src <- file.path(dir, "x.c")
  writeLines("void SETLENGTH(void); void f(void) { SETLENGTH(); }", src)
  so <- file.path(dir, "x.so")
  cc <- if (nzchar(Sys.which("cc"))) "cc" else "gcc"
  ok <- system2(cc, c("-shared", "-fPIC", "-o", shQuote(so), shQuote(src)),
                stdout = FALSE, stderr = FALSE)
  skip_if_not(identical(ok, 0L) && file.exists(so), "could not build a fixture")

  sets <- list(
    "4.4" = c("SETLENGTH", "Rf_error"),
    "4.5" = c("SETLENGTH", "Rf_error"),
    "4.6" = "Rf_error"
  )
  v <- shared_object_abi_verdict(so, sets)
  expect_true(v$inspected)
  expect_true(v$sensitive)
  expect_true("SETLENGTH" %in% v$symbols)
  expect_identical(v$unsupported, "4.6")
})

test_that("a binary touching only stable symbols is not sensitive", {
  skip_if_not(nzchar(Sys.which("nm")), "nm is unavailable")
  skip_if_not(nzchar(Sys.which("cc")) || nzchar(Sys.which("gcc")), "no C compiler")

  dir <- tempfile()
  dir.create(dir)
  src <- file.path(dir, "y.c")
  writeLines("void Rf_error(void); void g(void) { Rf_error(); }", src)
  so <- file.path(dir, "y.so")
  cc <- if (nzchar(Sys.which("cc"))) "cc" else "gcc"
  ok <- system2(cc, c("-shared", "-fPIC", "-o", shQuote(so), shQuote(src)),
                stdout = FALSE, stderr = FALSE)
  skip_if_not(identical(ok, 0L) && file.exists(so), "could not build a fixture")

  # This is the case the LinkingTo heuristic gets wrong: compiled, but touching
  # nothing that varies across minors, so one binary serves every minor.
  sets <- list(
    "4.4" = c("SETLENGTH", "Rf_error"),
    "4.5" = c("SETLENGTH", "Rf_error"),
    "4.6" = "Rf_error"
  )
  v <- shared_object_abi_verdict(so, sets)
  expect_true(v$inspected)
  expect_false(v$sensitive)
  expect_identical(v$symbols, character(0))
})
