# Check if a package has previous build errors

Queries the metadata database to check if a package/tag combination has
recorded build errors for the specified platform and architecture.

## Usage

``` r
check_package_error(con, table_name, pair, platform, arch)
```

## Arguments

- con:

  (`DBI::DBIConnection`)  
  Database connection object

- table_name:

  (character)  
  Quoted table name for metadata operations

- pair:

  (list)  
  List with pkg and tag elements for package identification

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

## Value

Logical indicating if errors were found
