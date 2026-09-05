# Check if build should be skipped

Evaluates conditions to determine if a package build should be skipped,
including existing files and S3 storage checks.

## Usage

``` r
check_build_skip_conditions(
  package_name,
  tag,
  binary_output_path,
  codename,
  s3_bucket,
  s3_access_key_id,
  s3_secret_access_key,
  s3_endpoint,
  s3_region,
  force
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

- codename:

  (character)  
  Linux distribution identifier

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

- force:

  (logical)  
  Whether to force-build packages

## Value

List with should_skip and reason
