# Build binary for a single tag

Build binary for a single tag

## Usage

``` r
build_single_tag(
  package_name,
  platform,
  arch,
  binary_output_path,
  local_clone_dir,
  source_org_url,
  tag = NULL,
  codename = NULL,
  is_r_minor_sensitive = FALSE,
  s3_endpoint = NULL,
  s3_region = NULL,
  s3_bucket = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  force = FALSE,
  install_system_dependencies = TRUE,
  upload = FALSE,
  store_build_metadata = FALSE,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  patches = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- binary_output_path:

  (character)  
  Local output path for binaries

- local_clone_dir:

  (character)  
  Path to clone git repos into

- source_org_url:

  (character)  
  Git organization URL in which to search for the package sources. The
  final URL will a combination from this argument and the package name.
  This also implies that the package source must exist in a repository
  with the same name. Must be a https:// URL, local paths are not
  supported.

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- codename:

  (character)  
  Linux distribution identifier

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

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

- force:

  (logical)  
  Whether to force-build packages

- install_system_dependencies:

  (logical)  
  Whether to infer and install system dependencies of packages

- upload:

  (logical)  
  Whether to upload the built packages to the specified remote.

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

- patches:

  Optional path to a patch registry directory containing a
  `registry.json` (and any referenced diff files). When set, matching
  packages are pre-built as patched binaries and installed into the
  build library before dependency installation. Defaults to `NULL` (no
  patching).
