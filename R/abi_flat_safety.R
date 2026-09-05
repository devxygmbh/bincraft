# Which flat-slot objects are safe to serve to any R minor.
#
# `risky_packages` marks a *package*: one that needs a per-minor build under at
# least one minor. Safety is a property of a *specific object*. Those are not
# the same thing, and conflating them is expensive.
#
# `union_index_records()` drops every carried flat record whose package is
# risky, because the `Built` stamp it used to consult is slot-wide and cannot
# say what a given object needs. With the classifier calling 93% of compiled
# packages risky, that removed roughly 2000 packages per per-minor index, all
# resolving to source for clients the routing works perfectly well for.
#
# Yet a risky package can have a perfectly portable flat binary. base64enc built
# under 4.6 references no symbol that varies across 4.4, 4.5 and 4.6, so one
# object serves every minor -- while the same package built under 4.4 does need
# its own slot. `tarball_abi_verdict()` already decides this at upload time; it
# was simply not written down anywhere the index build could read it.
#
# This records that verdict per object, so the index can keep what is provably
# safe and drop only what is not. An object with no recorded verdict stays
# dropped: unknown must not mean "assume portable".

#' Create the flat-object safety table if it does not exist
#' @keywords internal
#' @noRd
ensure_flat_safety_table <- function(con, table) {
  tbl <- DBI::dbQuoteIdentifier(con, table)
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE IF NOT EXISTS ",
      tbl,
      " (package text NOT NULL, version text NOT NULL, ",
      "codename text NOT NULL, arch text NOT NULL, ",
      "safe boolean NOT NULL, unsupported text, ",
      "checked_at timestamptz NOT NULL DEFAULT now(), ",
      "PRIMARY KEY (package, version, codename, arch))"
    )
  )
  invisible(TRUE)
}

#' Record whether one flat-slot object loads under every supported R minor
#'
#' @param safe `TRUE` when the object references no symbol that varies across
#'   the installed minors.
#' @param unsupported Minors that cannot load it, for diagnosis.
#' @keywords internal
#' @noRd
flat_safety_store <- function(
  package_name,
  tag,
  codename,
  arch,
  safe,
  unsupported = character(0),
  metadata_db_type = "postgres",
  metadata_db_host = "r-binaries.devxy.io",
  metadata_db_name = "build_metadata",
  metadata_db_port = 15432L,
  metadata_db_user = "rpkgs",
  metadata_db_password = Sys.getenv("PGPASS"),
  metadata_db_sslmode = "require",
  metadata_db_table = "abi_flat_safety"
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
    return(invisible(FALSE))
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  tryCatch(
    {
      ensure_flat_safety_table(con, metadata_db_table)
      tbl <- DBI::dbQuoteIdentifier(con, metadata_db_table)
      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO ",
          tbl,
          " (package, version, codename, arch, safe, unsupported) ",
          "VALUES ($1, $2, $3, $4, $5, $6) ",
          "ON CONFLICT (package, version, codename, arch) DO UPDATE SET ",
          "safe = EXCLUDED.safe, unsupported = EXCLUDED.unsupported, ",
          "checked_at = now()"
        ),
        params = list(
          package_name,
          tag,
          codename,
          arch,
          isTRUE(safe),
          paste(unsupported, collapse = ",")
        )
      )
      invisible(TRUE)
    },
    error = function(e) {
      log_debug(sprintf(
        "{.fun flat_safety_store}: could not record %s %s (%s).",
        package_name,
        tag,
        conditionMessage(e)
      ))
      invisible(FALSE)
    }
  )
}

#' Flat-slot objects recorded as safe for every supported R minor
#'
#' @return Character vector of `"<package>_<version>"`, possibly empty. Empty on
#'   any failure, which leaves the caller with the conservative behaviour of
#'   dropping every risky carried record.
#' @keywords internal
#' @noRd
flat_safety_safe_set <- function(
  codename,
  arch,
  metadata_db_type = "postgres",
  metadata_db_host = "r-binaries.devxy.io",
  metadata_db_name = "build_metadata",
  metadata_db_port = 15432L,
  metadata_db_user = "rpkgs",
  metadata_db_password = Sys.getenv("PGPASS"),
  metadata_db_sslmode = "require",
  metadata_db_table = "abi_flat_safety"
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
      ensure_flat_safety_table(con, metadata_db_table)
      tbl <- DBI::dbQuoteIdentifier(con, metadata_db_table)
      rows <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT package, version FROM ",
          tbl,
          " WHERE safe AND codename = $1 AND arch = $2"
        ),
        params = list(codename, arch)
      )
      if (nrow(rows) == 0L) {
        return(character(0))
      }
      unique(sprintf("%s_%s", rows$package, rows$version))
    },
    error = function(e) {
      log_info(sprintf(
        "{.fun flat_safety_safe_set}: unavailable (%s); every risky carried record will be dropped.",
        conditionMessage(e)
      ))
      character(0)
    }
  )
}
