# Archive packages in CRAN-like repositories

Archive packages in CRAN-like repositories

## Usage

``` r
archive_package(
  package_name,
  s3_endpoint,
  s3_region,
  s3_bucket,
  codename = NULL,
  is_r_minor_sensitive = FALSE,
  local_output_dir_root = ".",
  arch = NULL,
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

- local_output_dir_root:

  (character)  
  Path to local build root

- arch:

  (character)  
  Architecture

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

## Examples

``` r
if (FALSE) { # \dontrun{
archive_package("AATtools", codename = "rhel9")
archive_package("adw",
  codename = "rhel8", arch = "amd64",
  s3_endpoint = "https://hel1.your-objectstorage.com", s3_region = "hel1",
  s3_bucket = "devxy-r-package-binaries-hel1",
  s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
  s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S")
)
} # }
```
