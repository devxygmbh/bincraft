# Handle post-build actions

Manages upload and archiving operations after package builds complete.

## Usage

``` r
handle_post_build_actions(
  package_name,
  tag,
  result,
  codename,
  upload,
  archive,
  force,
  is_r_minor_sensitive,
  s3_endpoint,
  s3_bucket,
  s3_region,
  s3_access_key_id,
  s3_secret_access_key
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- result:

  (list)  
  Build results from package building operations

- codename:

  (character)  
  Linux distribution identifier

- upload:

  (logical)  
  Whether to upload the built packages to the specified remote.

- archive:

  (logical)  
  Whether to archive packages.

- force:

  (logical)  
  Whether to force-build packages

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

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

## Value

Invisible NULL
