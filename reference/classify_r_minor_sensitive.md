# Classify a single CRAN package for R-minor sensitivity

Determines whether a package version must be recompiled per R minor
version via
[`needs_per_minor_recompile()`](https://bincraft.doc.rpkgs.com/reference/needs_per_minor_recompile.md).
The source is the CRAN source tarball
([`download_cran_source()`](https://bincraft.doc.rpkgs.com/reference/download_cran_source.md)),
not a GitHub clone, so classification does not touch GitHub.

## Usage

``` r
classify_r_minor_sensitive(
  package_name,
  tag,
  local_clone_dir = tempdir(),
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  metadata_db_cache_table = "abi_classification"
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- local_clone_dir:

  (character)  
  Path to clone git repos into

- metadata_db_type:

  (character)  
  Type of metadata database

- metadata_db_host:

  (character)  
  Host value of metadata database

- metadata_db_name:

  (character)  
  Name of metadata database

- metadata_db_port:

  (integer)  
  Port of metadata database

- metadata_db_user:

  (character)  
  User value of metadata database

- metadata_db_password:

  (character)  
  User password of metadata database

- metadata_db_sslmode:

  (character)  
  SSL mode for metadata database

- metadata_db_cache_table:

  Name of the table holding cached ABI classification verdicts. Created
  on first use.

## Value

A logical of length 1: `TRUE` if the package must be recompiled per R
minor version.

## Details

The verdict is a property of the package *source* – independent of OS,
arch and R minor – so it is cached in the metadata database keyed on
`(package, version)` plus a signature of the curated ABI lists. A cache
hit skips the download and classification entirely; a full rebuild for a
new OS release therefore reuses existing verdicts and touches CRAN zero
times. Pass the `metadata_db_*` arguments to enable caching; omit them
to always compute (the cache is transparently skipped).

Fails safe to `TRUE` (build per minor) if the download or classification
errors, so a possibly-ABI-fragile binary is never served from the
cross-minor generic slot by mistake.
