# Upload binary to S3

Upload binary to S3

## Usage

``` r
upload_single_binary(
  package_name,
  tag,
  s3_endpoint,
  s3_region,
  s3_bucket,
  local_output_dir_root = ".",
  codename = NULL,
  force = FALSE,
  is_r_minor_sensitive = FALSE,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

- s3_bucket:

  (character)  
  S3 bucket name

- local_output_dir_root:

  (character)  
  Path to local build root

- codename:

  (character)  
  Linux distribution identifier

- force:

  (logical)  
  Whether to force-build packages

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key
