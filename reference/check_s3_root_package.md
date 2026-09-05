# Check if root package exists in S3 for check_s3_packages

Helper function to check if the latest package version exists in S3,
accounting for R minor version sensitivity.

## Usage

``` r
check_s3_root_package(
  remote_bin_path,
  package_name,
  last_version,
  is_r_minor_sensitive,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region
)
```

## Arguments

- remote_bin_path:

  (character)  
  Path to remote binary directory

- package_name:

  (character)  
  Package name

- last_version:

  (character)  
  Latest version string from CRAN

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

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

## Value

Logical indicating if package exists
