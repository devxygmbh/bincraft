# Build R binary packages

Builds binary packages from a given URL for a specific architecture. Git
tags are used to determine all possible versions to build.

System dependencies are automatically installed through uvr.

The function also automatically archives older versions into an
`Archive/` directory to keep only the most recent one in the repository
root.

## Usage

``` r
build_binary_package(
  package_name,
  tag = NULL,
  codename = NULL,
  source_org_url = "https://github.com/cran",
  tag_limit = 10L,
  local_output_dir_root = ".",
  local_clone_dir = "/tmp",
  platform = NULL,
  arch = NULL,
  is_r_minor_sensitive = FALSE,
  install_system_dependencies = TRUE,
  force = FALSE,
  upload = FALSE,
  archive = FALSE,
  store_build_metadata = FALSE,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  s3_endpoint = NULL,
  s3_region = NULL,
  s3_bucket = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  s3_package_cache = NULL,
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

- codename:

  (character)  
  Linux distribution identifier

- source_org_url:

  (character)  
  Git organization URL in which to search for the package sources. The
  final URL will a combination from this argument and the package name.
  This also implies that the package source must exist in a repository
  with the same name. Must be a https:// URL, local paths are not
  supported.

- tag_limit:

  (integer)  
  The most recent `n` tags to consider.

- local_output_dir_root:

  (character)  
  Path to local build root

- local_clone_dir:

  (character)  
  Path to clone git repos into

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

- install_system_dependencies:

  (logical)  
  Whether to infer and install system dependencies of packages

- force:

  (logical)  
  Whether to force-build packages

- upload:

  (logical)  
  Whether to upload the built packages to the specified remote.

- archive:

  (logical)  
  Whether to archive packages.

- store_build_metadata:

  (logical)  
  Whether to store the build metadata in the referenced database.

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

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

- s3_bucket:

  (character)  
  S3 bucket name

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

- s3_package_cache:

  (character or `NULL`)  
  Optional pre-fetched character vector of S3 package filenames
  (basenames like `"pkg_1.0.0.tar.gz"`). When provided, S3 existence
  checks use in-memory set lookup instead of per-package S3 API calls.
  Obtain via `basename(s3fs::s3_dir_ls(..., recurse = TRUE))`.

- patches:

  Optional path to a patch registry directory containing a
  `registry.json` (and any referenced diff files). When set, matching
  packages are pre-built as patched binaries and installed into the
  build library before dependency installation. Defaults to `NULL` (no
  patching).

## Examples

``` r
if (FALSE) { # \dontrun{
# build from cran
build_binary_package("brew", archive = FALSE)
build_binary_package("brew",
  source_org_url = "https://my.git.com/rpkgs/",
  archive = FALSE
)
} # }
```
