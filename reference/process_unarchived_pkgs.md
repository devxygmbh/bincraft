# Returns packages that need to be archived (have multiple versions)

Returns packages that need to be archived (have multiple versions)

## Usage

``` r
process_unarchived_pkgs(
  codename,
  arch,
  s3_endpoint = "https://hel1.your-objectstorage.com",
  s3_region = "hel1",
  s3_bucket = "devxy-r-package-binaries-hel1",
  s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
  s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
  workers = 1L
)
```

## Arguments

- codename:

  Operating system codename (e.g., "alpine322", "ubuntu-2204")

- arch:

  Architecture (e.g., "arm64", "amd64")

- s3_endpoint:

  S3 endpoint URL

- s3_region:

  S3 region

- s3_bucket:

  S3 bucket name

- s3_access_key_id:

  S3 access key ID

- s3_secret_access_key:

  S3 secret access key

## Examples

``` r
if (FALSE) { # \dontrun{
process_unarchived_pkgs("alpine322", "arm64")
} # }
```
