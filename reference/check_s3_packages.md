# Check S3 for existing packages

Verifies which packages already exist in S3 storage and determines which
ones need to be built, including error filtering.

## Usage

``` r
check_s3_packages(
  package_name,
  tag,
  source_org_url,
  tag_limit,
  is_r_minor_sensitive,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region,
  store_build_metadata,
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_table,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode,
  platform,
  arch,
  codename = NULL,
  s3_package_cache = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

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

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

- s3_bucket:

  (character)  
  S3 bucket name

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

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

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- codename:

  (character)  
  Linux distribution identifier

- s3_package_cache:

  (character or `NULL`)  
  Optional pre-fetched character vector of S3 package filenames
  (basenames like `"pkg_1.0.0.tar.gz"`). When provided, S3 existence
  checks use in-memory set lookup instead of per-package S3 API calls.
  Obtain via `basename(s3fs::s3_dir_ls(..., recurse = TRUE))`.

## Value

List with should_skip and filtered_tags
