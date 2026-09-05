# Check if root package exists in S3

Verifies if the latest version of a package exists in the S3 repository,
accounting for R minor version sensitivity.

## Usage

``` r
list_archived_packages(
  remote_bin_path,
  package_name,
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

Character vector of archived package filenames

## Details

List archived packages from S3

Retrieves list of archived package files for a given package, accounting
for R minor version sensitivity.
