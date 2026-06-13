# One-off: project bincraft's ABI classifier rules 1+2 onto the live CRAN
# PACKAGES index. Rule 3 (volatile-symbol grep) needs actual source tarballs
# and is not applied here, so the "risky" count is a lower bound.

suppressMessages(devtools::load_all(quiet = TRUE))

risky_deps <- abi_risky_linking_deps()
cat(sprintf("Risky LinkingTo deps: %s\n", paste(risky_deps, collapse = ", ")))

ap <- utils::available.packages(
  repos = "https://cloud.r-project.org",
  filters = list()
)

total <- nrow(ap)
needs_comp <- !is.na(ap[, "NeedsCompilation"]) &
  tolower(ap[, "NeedsCompilation"]) == "yes"

parse_linking_to <- function(x) {
  if (is.na(x)) {
    return(character())
  }
  pkgs <- trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
  sub("\\s*\\(.*\\)$", "", pkgs)
}

linking_pkgs <- lapply(ap[, "LinkingTo"], parse_linking_to)
risky_via_linking <- vapply(
  linking_pkgs,
  function(p) length(intersect(p, risky_deps)) > 0L,
  logical(1L)
)

risky <- needs_comp & risky_via_linking
safe_compiled <- needs_comp & !risky_via_linking
pure_r <- !needs_comp

cat("\nCRAN PACKAGES projection (rule 1 + rule 2 only):\n")
cat(sprintf(
  "  total packages:           %5d\n  pure-r (no compile):      %5d  (%5.1f%%)\n  safe-compiled (compile,   %5d  (%5.1f%%)\n    no risky LinkingTo)\n  risky (LinkingTo only):   %5d  (%5.1f%%)\n",
  total,
  sum(pure_r),
  100 * sum(pure_r) / total,
  sum(safe_compiled),
  100 * sum(safe_compiled) / total,
  sum(risky),
  100 * sum(risky) / total
))

cat("\nBreakdown of risky-LinkingTo by dep:\n")
for (dep in risky_deps) {
  n <- sum(
    vapply(linking_pkgs, function(p) dep %in% p, logical(1L)) & needs_comp
  )
  cat(sprintf("  %-8s %5d  (%5.1f%%)\n", dep, n, 100 * n / total))
}
