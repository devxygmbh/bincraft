# Process updated and new CRAN packages

Packages which got removed from CRAN can be deleted by setting
`prune = TRUE`. Argument `interval` allows to specify a range which
should be processed.

## Usage

``` r
process_cran_updates(
  package_name,
  tag,
  platform,
  local_clone_dir = tempdir(),
  interval = lubridate::today(),
  codename = NULL,
  local_output_dir_root = ".",
  s3_endpoint = NULL,
  s3_region = NULL,
  s3_bucket = NULL,
  store_build_metadata = FALSE,
  archive = FALSE,
  upload = FALSE,
  force = FALSE,
  filter_r_minor_sensitive = FALSE,
  r_minor_detection = c("none", "issue", "classifier"),
  r_minor_sensitive_only = FALSE,
  r_minor_packages_forge_type = "Forgejo",
  r_minor_packages_issue_url = NULL,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  process_updated = TRUE,
  process_new = TRUE,
  process_removed = TRUE,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  patches = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- platform:

  (character)  
  Platform identifier

- local_clone_dir:

  (character)  
  Path to clone git repos into

- interval:

  (`lubridate::Interval`)  
  Date interval

- codename:

  (character)  
  Linux distribution identifier

- local_output_dir_root:

  (character)  
  Path to local build root

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

- s3_bucket:

  (character)  
  S3 bucket name

- store_build_metadata:

  (logical)  
  Whether to store the build metadata in the referenced database.

- archive:

  (logical)  
  Whether to archive packages.

- upload:

  (logical)  
  Whether to upload the built packages to the specified remote.

- force:

  (logical)  
  Whether to force-build packages

- filter_r_minor_sensitive:

  (logical)  
  If TRUE, filters the packages to be processed by a list of packages
  known to be minor version specific. These packages must be stored as
  plain text and line separated in an issue of a Git repository. Forgejo
  and GitHub repositories are supported.

- r_minor_detection:

  How to decide which packages are R-minor-sensitive. `"none"` (default)
  builds everything into the generic slot. `"issue"` uses the curated
  tracking issue (the legacy `filter_r_minor_sensitive` path).
  `"classifier"` classifies each candidate via
  [`needs_per_minor_recompile()`](https://bincraft.doc.rpkgs.com/reference/needs_per_minor_recompile.md)
  and routes only `risky` packages to the per-minor slot.

- r_minor_sensitive_only:

  When `TRUE`, only R-minor-sensitive packages are built (used for the
  additional per-minor passes under non-primary R versions).

- r_minor_packages_forge_type:

  (character)  
  Forge type to query R minor sensitive packages from. Available options
  are "Forgejo" and "GitHub".

- r_minor_packages_issue_url:

  (character)  
  URL to issue containing R minor sensitive packages.

- metadata_db_type:

  (character)  
  Type of metadata database

- metadata_db_host:

  (character)  
  Host value of metadata database

- metadata_db_name:

  (character)  
  Name of metadata database

- metadata_db_table:

  (character)  
  Table name of metadata database

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

- process_updated:

  (logical)  
  Whether to process updated packages on CRAN

- process_new:

  (logical)  
  Whether to process newly added packages on CRAN

- process_removed:

  (logical)  
  Whether to process packages that have been removed from CRAN

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

- patches:

  Optional path to a patch registry directory containing a
  `registry.json` (and any referenced diff files). When set, matching
  packages are pre-built as patched binaries and installed into the
  build library before dependency installation. Defaults to `NULL` (no
  patching).

## Examples

``` r
if (FALSE) { # \dontrun{
process_cran_updates(
  interval = lubridate::interval(lubridate::today() - 3, lubridate::today() - 5L),
  s3_endpoint = "https://hel1.your-objectstorage.com", s3_region = "hel1",
  s3_bucket = "devxy-r-package-binaries-hel1",
  s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
  s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
  process_removed = FALSE,
  platform = "alpine-321"
)
} # }
```
