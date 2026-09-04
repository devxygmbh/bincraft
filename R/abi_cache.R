# Persistent cache for the ABI r-minor-sensitivity classification.
#
# The verdict (`needs_per_minor_recompile()`) is a property of a package
# version's *source* -- it does not depend on the target OS, arch or R minor --
# so it only needs to be computed once per (package, version) and can be reused
# across every platform build and every weekly rebuild. Caching it in the same
# Postgres metadata database bincraft already uses means a full rebuild for a
# new OS release reuses existing verdicts and touches CRAN/GitHub zero times.
#
# The cache key includes a signature of the curated ABI lists (and a manual
# logic version), so editing `abi_risky_linking_deps.txt` / `abi_volatile_symbols.txt`
# -- or the classifier logic -- transparently invalidates stale verdicts.

# Bump when the abi_classify() decision logic itself changes in a way that is
# not captured by the curated-list contents.
abi_classifier_logic_version <- "1"

#' Signature of the current classifier, for cache invalidation
#' @keywords internal
abi_classifier_signature <- function() {
  files <- c(
    system.file("extdata", "abi_volatile_symbols.txt", package = "bincraft"),
    system.file("extdata", "abi_risky_linking_deps.txt", package = "bincraft")
  )
  files <- files[nzchar(files) & file.exists(files)]
  digests <- if (length(files) > 0L) {
    unname(tools::md5sum(files))
  } else {
    "no-lists"
  }
  paste(c(abi_classifier_logic_version, digests), collapse = "-")
}

# Open a connection to the classification cache, or return NULL when caching is
# not configured (no host/name) or RPostgres is unavailable. Callers treat NULL
# as "cache disabled" and fall back to computing the verdict.
abi_cache_connect <- function(
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode
) {
  if (!identical(metadata_db_type, "postgres")) {
    return(NULL)
  }
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(metadata_db_host) || is.null(metadata_db_name)) {
    return(NULL)
  }
  purrr::insistently(
    ~ DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = metadata_db_name,
      host = metadata_db_host,
      port = metadata_db_port,
      user = metadata_db_user,
      password = metadata_db_password,
      sslmode = metadata_db_sslmode
    ),
    rate = retry_config,
    quiet = FALSE
  )()
}

ensure_abi_cache_table <- function(con, table) {
  tbl <- DBI::dbQuoteIdentifier(con, table)
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE IF NOT EXISTS ",
      tbl,
      " (package text NOT NULL, version text NOT NULL, ",
      "classifier_sig text NOT NULL, r_minor_sensitive boolean NOT NULL, ",
      "classified_at timestamptz NOT NULL DEFAULT now(), ",
      "PRIMARY KEY (package, version, classifier_sig))"
    )
  )
  invisible(TRUE)
}

#' Look up a cached r-minor-sensitivity verdict
#'
#' @return `TRUE`/`FALSE` on a cache hit, or `NULL` on a miss / when caching is
#'   disabled or unreachable (so the caller computes the verdict).
#' @keywords internal
abi_cache_lookup <- function(
  package_name,
  tag,
  classifier_sig,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  metadata_db_cache_table = "abi_classification"
) {
  con <- tryCatch(
    abi_cache_connect(
      metadata_db_type,
      metadata_db_host,
      metadata_db_name,
      metadata_db_port,
      metadata_db_user,
      metadata_db_password,
      metadata_db_sslmode
    ),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(NULL)
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ensure_abi_cache_table(con, metadata_db_cache_table)
  tbl <- DBI::dbQuoteIdentifier(con, metadata_db_cache_table)
  res <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT r_minor_sensitive FROM ",
      tbl,
      " WHERE package = $1 AND version = $2 AND classifier_sig = $3"
    ),
    params = list(package_name, tag, classifier_sig)
  )
  if (nrow(res) == 0L) {
    return(NULL)
  }
  isTRUE(as.logical(res$r_minor_sensitive[[1L]]))
}

#' Store an r-minor-sensitivity verdict in the cache (upsert)
#' @keywords internal
abi_cache_store <- function(
  package_name,
  tag,
  classifier_sig,
  r_minor_sensitive,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  metadata_db_cache_table = "abi_classification"
) {
  con <- abi_cache_connect(
    metadata_db_type,
    metadata_db_host,
    metadata_db_name,
    metadata_db_port,
    metadata_db_user,
    metadata_db_password,
    metadata_db_sslmode
  )
  if (is.null(con)) {
    return(invisible(FALSE))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ensure_abi_cache_table(con, metadata_db_cache_table)
  tbl <- DBI::dbQuoteIdentifier(con, metadata_db_cache_table)
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      tbl,
      " (package, version, classifier_sig, r_minor_sensitive, classified_at) ",
      "VALUES ($1, $2, $3, $4, now()) ",
      "ON CONFLICT (package, version, classifier_sig) DO UPDATE SET ",
      "r_minor_sensitive = EXCLUDED.r_minor_sensitive, classified_at = now()"
    ),
    params = list(package_name, tag, classifier_sig, r_minor_sensitive)
  )
  invisible(TRUE)
}

#' Packages the classifier has judged ABI-risky, from the cache
#'
#' `risky_packages_across_minors()` can only see packages that already have a
#' per-minor tarball, which makes the risky set circular: a package counts as
#' risky only once it has been built per-minor, and one that never entered the
#' build list is never classified, never built per-minor, and so never counts.
#' Its flat binary is then carried into every per-minor index, which is how a
#' 4.4-built `base64enc` reached R 4.6 clients and died at `dyn.load()` on
#' `SETLENGTH`.
#'
#' The verdict is a property of the source, independent of OS, arch and R minor,
#' so a verdict cached by any slot's build applies to every slot.
#'
#' Fails soft: with no cache configured, no `RPostgres`, or any query error this
#' returns `character(0)`, leaving the caller with the tarball-derived set it
#' had before.
#'
#' @param classifier_sig Cache key from [abi_classifier_signature()]. Verdicts
#'   stored under a different signature are ignored, so editing the curated
#'   lists does not resurrect stale answers.
#' @inheritParams abi_cache_lookup
#' @return Character vector of package names, possibly empty.
#' @keywords internal
#' @noRd
abi_cache_risky_packages <- function(
  classifier_sig = abi_classifier_signature(),
  metadata_db_type = "postgres",
  metadata_db_host = "r-binaries.devxy.io",
  metadata_db_name = "build_metadata",
  metadata_db_port = 15432L,
  metadata_db_user = "rpkgs",
  metadata_db_password = Sys.getenv("PGPASS"),
  metadata_db_sslmode = "require",
  metadata_db_cache_table = "abi_classification"
) {
  con <- tryCatch(
    abi_cache_connect(
      metadata_db_type,
      metadata_db_host,
      metadata_db_name,
      metadata_db_port,
      metadata_db_user,
      metadata_db_password,
      metadata_db_sslmode
    ),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(character(0))
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  tryCatch(
    {
      ensure_abi_cache_table(con, metadata_db_cache_table)
      tbl <- DBI::dbQuoteIdentifier(con, metadata_db_cache_table)
      rows <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT package FROM ",
          tbl,
          " WHERE r_minor_sensitive AND classifier_sig = $1"
        ),
        params = list(classifier_sig)
      )
      unique(as.character(rows$package))
    },
    error = function(e) {
      log_info(sprintf(
        "{.fun abi_cache_risky_packages}: cache unavailable (%s); using the tarball-derived risky set only.",
        conditionMessage(e)
      ))
      character(0)
    }
  )
}
