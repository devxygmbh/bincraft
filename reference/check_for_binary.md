# Checks whether a binary for the latest package version exists

Checks whether a binary for the latest package version exists

## Usage

``` r
check_for_binary(
  package_name,
  s3_endpoint = NULL,
  s3_region = NULL,
  s3_bucket = NULL,
  codename = NULL,
  is_r_minor_sensitive = FALSE,
  arch = NULL,
  version = "latest",
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

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

- arch:

  (character)  
  Architecture

- version:

  (character)  
  Version to check for. Only "latest" is supported right now.

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key
