# Sample N CRAN "safe-compiled candidates" (NeedsCompilation=yes, no risky
# LinkingTo), download their tarballs, and run the full abi_classify to see
# how many actually survive rule 3 (volatile-symbol grep) vs get reclassified
# as risky.

suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(42L)
N <- 100L

ap <- utils::available.packages(
  repos = "https://cloud.r-project.org",
  filters = list()
)
needs_comp <- !is.na(ap[, "NeedsCompilation"]) &
  tolower(ap[, "NeedsCompilation"]) == "yes"

parse_linking_to <- function(x) {
  if (is.na(x)) return(character())
  pkgs <- trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
  sub("\\s*\\(.*\\)$", "", pkgs)
}
linking_pkgs <- lapply(ap[, "LinkingTo"], parse_linking_to)
risky_deps <- abi_risky_linking_deps()
risky_via_linking <- vapply(
  linking_pkgs,
  function(p) length(intersect(p, risky_deps)) > 0L,
  logical(1L)
)

safe_candidates_idx <- which(needs_comp & !risky_via_linking)
cat(sprintf(
  "Safe-compiled candidates on CRAN: %d total. Sampling N = %d.\n",
  length(safe_candidates_idx),
  N
))

sample_idx <- sample(safe_candidates_idx, N)
sample_pkgs <- ap[sample_idx, , drop = FALSE]

tmpdir <- tempfile("abi_cran_sample_")
dir.create(tmpdir)

results <- vector("list", N)
for (i in seq_len(N)) {
  pkg <- sample_pkgs[i, "Package"]
  ver <- sample_pkgs[i, "Version"]
  url <- sprintf(
    "https://cloud.r-project.org/src/contrib/%s_%s.tar.gz",
    pkg,
    ver
  )
  dest <- file.path(tmpdir, sprintf("%s_%s.tar.gz", pkg, ver))

  res <- tryCatch(
    {
      utils::download.file(url, dest, quiet = TRUE, mode = "wb")
      abi_classify(dest)
    },
    error = function(e) {
      list(
        tier = "<error>",
        reason = conditionMessage(e),
        hits = character()
      )
    }
  )
  results[[i]] <- list(
    package = pkg,
    version = ver,
    tier = res$tier,
    reason = res$reason,
    hits = paste(res$hits, collapse = "|")
  )
  cat(sprintf("[%3d/%d] %-30s -> %s\n", i, N, pkg, res$tier))
}

df <- do.call(
  rbind.data.frame,
  c(results, list(stringsAsFactors = FALSE))
)

cat("\n==== Final tier distribution among safe-LinkingTo candidates ====\n")
print(table(df$tier))

risky_by_symbol <- df[df$tier == "risky", , drop = FALSE]
n_ok <- sum(df$tier %in% c("safe-compiled", "pure-r"))
n_risky <- nrow(risky_by_symbol)
n_err <- sum(df$tier == "<error>")

cat(sprintf(
  "\nOf %d sampled candidates: %d truly safe-compiled/pure-r, %d reclassified as risky by symbol grep, %d errors.\n",
  N - n_err,
  n_ok,
  n_risky,
  n_err
))
cat(sprintf(
  "Symbol-grep upgrade rate: %.1f%% of safe-LinkingTo candidates -> risky\n",
  100 * n_risky / max(N - n_err, 1L)
))

if (n_risky > 0L) {
  cat("\nReclassified-as-risky packages and their hits:\n")
  print(risky_by_symbol[, c("package", "hits")], row.names = FALSE)
}
