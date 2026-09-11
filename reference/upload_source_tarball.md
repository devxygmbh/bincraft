# Uploads source tarballs to S3

Uploads source tarballs to S3

## Usage

``` r
upload_source_tarball(
  package_name,
  s3_endpoint,
  s3_region,
  s3_bucket,
  codename = NULL,
  arch = NULL,
  is_r_minor_sensitive = FALSE,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

- s3_bucket:

  (character)  
  S3 bucket name

- codename:

  (character)  
  Linux distribution identifier

- arch:

  (character)  
  Architecture

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
