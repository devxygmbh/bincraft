# Tiny on-disk source package fixtures.
# Spec: docs/superpowers/tasks/2026-05-23-bincraft-abi-classify.md.
make_pkg <- function(
  description_extra = NULL,
  src_files = NULL,
  needs_compilation = "no",
  envir = parent.frame()
) {
  pkg_dir <- withr::local_tempdir(.local_envir = envir)
  desc_lines <- c(
    "Package: dummy",
    "Title: Dummy",
    "Version: 0.0.1",
    sprintf("NeedsCompilation: %s", needs_compilation),
    description_extra
  )
  writeLines(desc_lines, file.path(pkg_dir, "DESCRIPTION"))

  if (length(src_files) > 0L) {
    dir.create(file.path(pkg_dir, "src"))
    for (name in names(src_files)) {
      writeLines(src_files[[name]], file.path(pkg_dir, "src", name))
    }
  }

  pkg_dir
}

test_that("pure-r: no src/ and NeedsCompilation: no", {
  pkg <- make_pkg(needs_compilation = "no")

  result <- abi_classify(pkg)

  expect_identical(result$tier, "pure-r")
  expect_identical(result$reason, "no compilation needed")
  expect_identical(result$hits, character())
})

test_that("pure-r: src/ exists but contains no compilable files", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(Makevars = "# nothing to build")
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "pure-r")
  expect_identical(result$reason, "no compilation needed")
})

test_that("risky by LinkingTo: matches curated risky dep", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    description_extra = "LinkingTo: Rcpp",
    src_files = list(foo.cpp = "// Rcpp glue")
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "risky")
  expect_identical(result$reason, "LinkingTo Rcpp")
  expect_identical(result$hits, "Rcpp")
})

test_that("risky by LinkingTo: multi-line LinkingTo with multiple risky deps", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    description_extra = c("LinkingTo: ", "    cpp11,", "    rlang"),
    src_files = list(foo.cpp = "// cpp11 glue")
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "risky")
  expect_setequal(result$hits, c("cpp11", "rlang"))
  expect_match(result$reason, "^LinkingTo ")
})

test_that("risky by symbol grep: src file mentions volatile symbol", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(
      foo.c = c(
        "#include <Rinternals.h>",
        "SEXP do_thing(SEXP x) {",
        "  return R_mkClosure(R_NilValue, R_NilValue, R_GlobalEnv);",
        "}"
      )
    )
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "risky")
  expect_identical(result$hits, "R_mkClosure")
  expect_match(result$reason, "^uses ")
  expect_match(result$reason, "R_mkClosure")
})

test_that("risky by symbol grep: multiple volatile symbols reported", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(
      foo.c = c(
        "#include <Rinternals.h>",
        "SEXP do_thing(SEXP x) {",
        "  R_LockBinding(install(\"x\"), R_GlobalEnv);",
        "  return R_NewEnv(R_GlobalEnv, FALSE, 0);",
        "}"
      )
    )
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "risky")
  expect_setequal(result$hits, c("R_LockBinding", "R_NewEnv"))
})

test_that("safe-compiled: src has C but only stable C-API", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(
      foo.c = c(
        "#include <Rinternals.h>",
        "SEXP do_thing(SEXP x) {",
        "  SEXP out = PROTECT(Rf_allocVector(INTSXP, 1));",
        "  INTEGER(out)[0] = 42;",
        "  UNPROTECT(1);",
        "  return out;",
        "}"
      )
    )
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "safe-compiled")
  expect_identical(result$reason, "compiled but only stable R C-API detected")
  expect_identical(result$hits, character())
})

test_that("LinkingTo precedes symbol grep: risky reported via LinkingTo only", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    description_extra = "LinkingTo: Rcpp",
    src_files = list(
      foo.cpp = c(
        "#include <Rcpp.h>",
        "// also mentions R_mkClosure"
      )
    )
  )

  result <- abi_classify(pkg)

  expect_identical(result$tier, "risky")
  expect_identical(result$hits, "Rcpp")
})

test_that("accepts .tar.gz input by untarring internally", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(
      foo.c = c(
        "#include <Rinternals.h>",
        "SEXP do_thing(void) { return R_NilValue; }"
      )
    )
  )

  tarball_dir <- withr::local_tempdir()
  tarball <- file.path(tarball_dir, "dummy_0.0.1.tar.gz")
  withr::with_dir(
    dirname(pkg),
    utils::tar(
      tarball,
      files = basename(pkg),
      compression = "gzip",
      tar = "internal"
    )
  )

  result <- abi_classify(tarball)

  expect_identical(result$tier, "safe-compiled")
})

test_that("errors on missing path", {
  expect_error(abi_classify("/no/such/path"), "does not exist")
})

test_that("errors on dir without DESCRIPTION", {
  dir <- withr::local_tempdir()
  expect_error(abi_classify(dir), "DESCRIPTION")
})

test_that("needs_per_minor_recompile: pure-r returns FALSE with reason attribute", {
  pkg <- make_pkg(needs_compilation = "no")

  result <- needs_per_minor_recompile(pkg)

  expect_false(as.logical(result))
  expect_identical(attr(result, "tier"), "pure-r")
  expect_identical(attr(result, "reason"), "no compilation needed")
  expect_identical(attr(result, "hits"), character())
})

test_that("needs_per_minor_recompile: safe-compiled returns FALSE", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(
      foo.c = c(
        "#include <Rinternals.h>",
        "SEXP do_thing(SEXP x) { return x; }"
      )
    )
  )

  result <- needs_per_minor_recompile(pkg)

  expect_false(as.logical(result))
  expect_identical(attr(result, "tier"), "safe-compiled")
})

test_that("needs_per_minor_recompile: risky (LinkingTo) returns TRUE with reason", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    description_extra = "LinkingTo: Rcpp",
    src_files = list(foo.cpp = "// Rcpp glue")
  )

  result <- needs_per_minor_recompile(pkg)

  expect_true(as.logical(result))
  expect_identical(attr(result, "tier"), "risky")
  expect_identical(attr(result, "reason"), "LinkingTo Rcpp")
  expect_identical(attr(result, "hits"), "Rcpp")
})

test_that("needs_per_minor_recompile: risky (symbol) returns TRUE with reason", {
  pkg <- make_pkg(
    needs_compilation = "yes",
    src_files = list(
      foo.c = c(
        "#include <Rinternals.h>",
        "SEXP do_thing(void) {",
        "  return R_mkClosure(R_NilValue, R_NilValue, R_GlobalEnv);",
        "}"
      )
    )
  )

  result <- needs_per_minor_recompile(pkg)

  expect_true(as.logical(result))
  expect_match(attr(result, "reason"), "R_mkClosure")
  expect_identical(attr(result, "hits"), "R_mkClosure")
})

test_that("a package reaching for SETLENGTH is risky", {
  # R 4.6 stopped exporting SETLENGTH. rlang 1.3.0 built under R 4.4.3 failed to
  # load there with `undefined symbol: SETLENGTH`; a package using it without
  # linking to a risky dependency would otherwise be classified safe-compiled
  # and served from the generic slot, where one build serves every R minor.
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "src"))
  writeLines(
    c("Package: setlengthy", "Version: 1.0", "NeedsCompilation: yes"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c("#include <Rinternals.h>", "void f(SEXP x) { SETLENGTH(x, 1); }"),
    file.path(pkg, "src", "f.c")
  )

  result <- needs_per_minor_recompile(pkg)
  expect_true(as.logical(result))
  expect_identical(attr(result, "tier"), "risky")
  expect_true("SETLENGTH" %in% attr(result, "hits"))
})

test_that("getters expose the curated lists", {
  symbols <- abi_volatile_symbols()
  expect_type(symbols, "character")
  expect_true("R_mkClosure" %in% symbols)
  expect_true(length(symbols) > 0L)

  deps <- abi_risky_linking_deps()
  expect_type(deps, "character")
  expect_true("Rcpp" %in% deps)
  expect_true("rlang" %in% deps)
})
