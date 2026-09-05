# Returns packages that do not have any historic versions stored in S3

Returns packages that do not have any historic versions stored in S3

## Usage

``` r
query_packages_without_historic_versions(
  codename,
  arch,
  s3_endpoint = "https://hel1.your-objectstorage.com",
  s3_region = "hel1",
  s3_bucket = "devxy-r-package-binaries-hel1",
  s3_access_key_id = Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"),
  s3_secret_access_key = Sys.getenv("HETZNER_S3_SECRET_KEY_K3S")
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
query_packages_without_historic_versions("alpine322", "arm64")
} # }
```
