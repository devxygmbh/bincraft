# Initialize a new R repository

Creates a new repository (S3 bucket) and checks connectivity to the
metadata database

## Usage

``` r
init_repo(
  s3_endpoint,
  s3_region,
  s3_bucket,
  s3_access_key_id = NULL,
  s3_secret_access_key = NULL,
  metadata_db_type = "sqlite",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL
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

- s3_access_key_id:

  (character)  
  S3 access key ID

- s3_secret_access_key:

  (character)  
  S3 secret access key

- metadata_db_type:

  (character)  
  Type of metadata database

- metadata_db_host:

  (character)  
  Host value of metadata database

- metadata_db_name:

  (character)  
  Name of metadata database

- metadata_db_table:

  (character)  
  Table name of metadata database

- metadata_db_port:

  (integer)  
  Port of metadata database

- metadata_db_user:

  (character)  
  User value of metadata database

- metadata_db_password:

  (character)  
  User password of metadata database

- metadata_db_sslmode:

  (character)  
  SSL mode for metadata database

## Note

When using SQLITE, make sure that the file is also available in the
desired built environment, i.e. most likel in your CI/CD system. Usually
it is easiest to store the file also in S3, as it can be reached there
easily from within CI and local sources.

## Examples

``` r
if (FALSE) { # \dontrun{
init_repo("https://hel1.your-objectstorage.com", "hel1", "devxy-r-package-binaries-hel1",
  Sys.getenv("HETZNER_S3_ACCESS_KEY_K3S"), Sys.getenv("HETZNER_S3_SECRET_KEY_K3S"),
  metadata_db_type = "postgres", metadata_db_host = "r-binaries.devxy.io",
  metadata_db_name = "build_metadata", metadata_db_table = "deleteme",
  metadata_db_user = "rpkgs", metadata_db_password = Sys.getenv("PGPASS"),
  metadata_db_sslmode = "require",
  metadata_db_port = 15432
)
} # }
```
