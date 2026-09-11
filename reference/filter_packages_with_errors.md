# Filter packages with previous build errors

Queries the metadata database to identify packages that have previously
failed to build and should be skipped.

## Usage

``` r
filter_packages_with_errors(
  pkg_differences,
  metadata_db_type,
  metadata_db_host,
  metadata_db_name,
  metadata_db_table,
  metadata_db_port,
  metadata_db_user,
  metadata_db_password,
  metadata_db_sslmode,
  platform,
  arch,
  pkgs_to_build
)
```

## Arguments

- pkg_differences:

  (character)  
  Character vector of package difference strings to check

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

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- pkgs_to_build:

  (character)  
  Original list of packages to build

## Value

Filtered package list
