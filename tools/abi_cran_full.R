#!/usr/bin/env Rscript
# Self-contained ABI classifier run over the full set of CRAN packages with
# NeedsCompilation=yes. Mirrors bincraft::abi_classify() rules 1-4 inline so
# nothing needs to be installed beyond base R + parallel.

VOLATILE_SYMBOLS <- c(
  "R_mkClosure",
  "R_MakeMissingBinding",
  "R_HashtabSEXP",
  "R_ActiveBindingFunction",
  "R_ClosureExpr",
  "R_ClosureFormals",
  "R_ClosureEnv",
  "R_PromiseExpr",
  "R_PromiseValue",
  "R_PromiseEnv",
  "R_NewEnv",
  "R_BindingIsLocked",
  "R_LockBinding",
  "R_MakeActiveBinding"
)
RISKY_DEPS <- c("rlang", "Rcpp", "cpp11", "vctrs")

parse_linking_to <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(character())
  }
  pkgs <- trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
  sub("\\s*\\(.*\\)$", "", pkgs)
}

classify_extracted <- function(pkg_dir) {
  desc_path <- file.path(pkg_dir, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(list(
      tier = "<error>",
      reason = "no DESCRIPTION",
      hits = character()
    ))
  }
  desc <- read.dcf(desc_path)
  needs <- if ("NeedsCompilation" %in% colnames(desc)) {
    desc[1L, "NeedsCompilation"]
  } else {
    NA_character_
  }
  linking_to <- if ("LinkingTo" %in% colnames(desc)) {
    desc[1L, "LinkingTo"]
  } else {
    NA_character_
  }
  src_dir <- file.path(pkg_dir, "src")
  compilable <- if (dir.exists(src_dir)) {
    list.files(
      src_dir,
      pattern = "\\.(c|cc|cpp|f|f90)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }

  if (
    identical(tolower(needs), "no") ||
      !dir.exists(src_dir) ||
      length(compilable) == 0L
  ) {
    return(list(
      tier = "pure-r",
      reason = "no compilation needed",
      hits = character()
    ))
  }

  matched_deps <- intersect(parse_linking_to(linking_to), RISKY_DEPS)
  if (length(matched_deps) > 0L) {
    return(list(
      tier = "risky",
      reason = sprintf("LinkingTo %s", paste(matched_deps, collapse = ", ")),
      hits = matched_deps
    ))
  }

  files <- list.files(
    src_dir,
    pattern = "\\.(c|cc|cpp|cxx|h|hpp|hxx|f|f90|f95|inl)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  content <- unlist(lapply(files, function(f) {
    tryCatch(readLines(f, warn = FALSE), error = function(e) character())
  }))
  matched_symbols <- VOLATILE_SYMBOLS[vapply(
    VOLATILE_SYMBOLS,
    function(sym) any(grepl(sprintf("\\b%s\\b", sym), content)),
    logical(1L)
  )]
  if (length(matched_symbols) > 0L) {
    return(list(
      tier = "risky",
      reason = sprintf("uses %s", paste(matched_symbols, collapse = ", ")),
      hits = matched_symbols
    ))
  }

  list(
    tier = "safe-compiled",
    reason = "compiled but only stable R C-API detected",
    hits = character()
  )
}

classify_pkg <- function(pkg, ver) {
  url <- sprintf(
    "https://cloud.r-project.org/src/contrib/%s_%s.tar.gz",
    pkg,
    ver
  )
  tarball <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tarball))
  extract_dir <- tempfile("abi_")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)

  res <- tryCatch(
    {
      utils::download.file(url, tarball, quiet = TRUE, mode = "wb")
      utils::untar(tarball, exdir = extract_dir)
      desc_files <- list.files(
        extract_dir,
        pattern = "^DESCRIPTION$",
        recursive = TRUE,
        full.names = TRUE
      )
      if (length(desc_files) == 0L) {
        list(tier = "<error>", reason = "no DESCRIPTION", hits = character())
      } else {
        classify_extracted(dirname(desc_files[[1L]]))
      }
    },
    error = function(e) {
      list(tier = "<error>", reason = conditionMessage(e), hits = character())
    }
  )

  list(
    package = pkg,
    version = ver,
    tier = res$tier,
    reason = res$reason,
    hits = paste(res$hits, collapse = "|")
  )
}

# Fetch index
cat("Fetching CRAN PACKAGES index...\n")
ap <- utils::available.packages(
  repos = "https://cloud.r-project.org",
  filters = list()
)
needs_comp <- !is.na(ap[, "NeedsCompilation"]) &
  tolower(ap[, "NeedsCompilation"]) == "yes"
to_classify <- ap[needs_comp, c("Package", "Version"), drop = FALSE]
cat(sprintf(
  "Total packages with NeedsCompilation=yes: %d\n",
  nrow(to_classify)
))

# Parallel classification
ncores <- as.integer(Sys.getenv("ABI_CORES", "16"))
cat(sprintf("Classifying with %d parallel workers...\n", ncores))

t0 <- Sys.time()
results <- parallel::mcmapply(
  classify_pkg,
  to_classify[, "Package"],
  to_classify[, "Version"],
  SIMPLIFY = FALSE,
  USE.NAMES = FALSE,
  mc.cores = ncores
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("Done in %.1fs.\n", elapsed))

df <- do.call(rbind.data.frame, c(results, list(stringsAsFactors = FALSE)))

cat("\n==== Tier distribution (all NeedsCompilation=yes CRAN packages) ====\n")
print(table(df$tier))
cat(sprintf("\nTotal: %d\n", nrow(df)))

# Symbol-grep upgrades: rule-3 reclassifications among safe-LinkingTo
ap_linking <- lapply(ap[needs_comp, "LinkingTo"], parse_linking_to)
risky_via_linking <- vapply(
  ap_linking,
  function(p) length(intersect(p, RISKY_DEPS)) > 0L,
  logical(1L)
)
df$risky_linking <- risky_via_linking
safe_candidates <- df[!df$risky_linking, , drop = FALSE]
upgraded <- safe_candidates[safe_candidates$tier == "risky", , drop = FALSE]

cat(sprintf(
  "\nOf %d safe-LinkingTo candidates, %d (%.2f%%) reclassified as risky by symbol grep.\n",
  nrow(safe_candidates),
  nrow(upgraded),
  100 * nrow(upgraded) / max(nrow(safe_candidates), 1L)
))
if (nrow(upgraded) > 0L) {
  cat("\nReclassified-as-risky packages (top 50):\n")
  print(
    head(upgraded[order(upgraded$package), c("package", "hits")], 50L),
    row.names = FALSE
  )
}

# Errors
errs <- df[df$tier == "<error>", , drop = FALSE]
if (nrow(errs) > 0L) {
  cat(sprintf("\n%d errors during classification. First 10:\n", nrow(errs)))
  print(head(errs[, c("package", "reason")], 10L), row.names = FALSE)
}

# Save full results
saveRDS(df, "/tmp/abi_cran_full_results.rds")
cat("\nFull results saved to /tmp/abi_cran_full_results.rds\n")
