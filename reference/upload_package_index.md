# Upload package index files to S3

Upload package index files to S3

## Usage

``` r
upload_package_index(
  s3_endpoint,
  s3_region,
  s3_bucket,
  package_name = NULL,
  local_output_dir_root = ".",
  codename = NULL,
  arch = NULL,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  r_minor = NULL
)
```

## Arguments

- s3_endpoint:

  (character)  
  S3 endpoint

- s3_region:

  (character)  
  S3 region

- s3_bucket:

  (character)  
  S3 bucket name

- package_name:

  (character)  
  Package name

- local_output_dir_root:

  (character)  
  Path to local build root

- codename:

  (character)  
  Linux distribution identifier

- arch:

  (character)  
  Architecture

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

- r_minor:

  Optional `"major.minor"` string. When set, the index is written/read
  under the per-minor slot `…/contrib/<r_minor>/` instead of the generic
  `…/contrib/` slot.
