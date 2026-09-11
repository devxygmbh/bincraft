# Execute package builds with parallel processing

Handles the actual building of packages using either sequential or
parallel processing with proper error handling.

## Usage

``` r
execute_package_builds(
  package_name,
  tag,
  binary_output_path,
  source_org_url,
  local_clone_dir,
  platform,
  arch,
  codename,
  is_r_minor_sensitive,
  force,
  install_system_dependencies,
  store_build_metadata,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_table,
  metadata_db_password,
  metadata_db_user,
  metadata_db_sslmode,
  s3_endpoint,
  s3_bucket,
  s3_region,
  s3_access_key_id,
  s3_secret_access_key,
  local_bin_path,
  upload = FALSE,
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

- binary_output_path:

  (character)  
  Local output path for binaries

- source_org_url:

  (character)  
  Git organization URL in which to search for the package sources. The
  final URL will a combination from this argument and the package name.
  This also implies that the package source must exist in a repository
  with the same name. Must be a https:// URL, local paths are not
  supported.

- local_clone_dir:

  (character)  
  Path to clone git repos into

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- codename:

  (character)  
  Linux distribution identifier

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

- force:

  (logical)  
  Whether to force-build packages

- install_system_dependencies:

  (logical)  
  Whether to infer and install system dependencies of packages

- store_build_metadata:

  (logical)  
  Whether to store the build metadata in the referenced database.

- metadata_db_host:

  (character)  
  Host value of metadata database

- metadata_db_name:

  (character)  
  Name of metadata database

- metadata_db_port:

  (integer)  
  Port of metadata database

- metadata_db_table:

  (character)  
  Table name of metadata database

- metadata_db_password:

  (character)  
  User password of metadata database

- metadata_db_user:

  (character)  
  User value of metadata database

- metadata_db_sslmode:

  (character)  
  SSL mode for metadata database

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_bucket:

  (character)  
  S3 bucket name

- s3_region:

  (character)  
  S3 region

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

- local_bin_path:

  (character)  
  Local binary path for output

- upload:

  (logical)  
  Whether to upload the built packages to the specified remote.

- patches:

  Optional path to a patch registry directory containing a
  `registry.json` (and any referenced diff files). When set, matching
  packages are pre-built as patched binaries and installed into the
  build library before dependency installation. Defaults to `NULL` (no
  patching).

## Value

Build results
