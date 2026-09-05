# Store build metadata of single binary builds

Store build metadata of single binary builds

## Usage

``` r
store_build_metadata(
  package_name,
  tag,
  platform,
  arch,
  error_occurred,
  metadata_db_type = "postgres",
  metadata_db_host = NULL,
  metadata_db_name = NULL,
  metadata_db_table = NULL,
  metadata_db_port = NULL,
  metadata_db_user = NULL,
  metadata_db_password = NULL,
  metadata_db_sslmode = NULL,
  force = FALSE,
  error = NA,
  build_duration = NA,
  size = NA
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- error_occurred:

  (logical)  
  Whether or not an error occurred during build

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

- force:

  (logical)  
  Whether to force-build packages

- error:

  (character)  
  Error information

- build_duration:

  (numeric)  
  Duration of binary build in seconds

- size:

  (numeric)  
  Size of binary package in MB
