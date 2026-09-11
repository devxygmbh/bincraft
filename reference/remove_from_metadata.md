# Remove package from metadata table

Remove package from metadata table

## Usage

``` r
remove_from_metadata(
  package_name,
  metadata_db_type = "postgres",
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

- package_name:

  (character)  
  Package name

- metadata_db_type:

  Database type. Denotes the DB driver to be used for connecting.

- metadata_db_host:

  Database host

- metadata_db_name:

  Database name

- metadata_db_table:

  Database table

- metadata_db_port:

  Database port

- metadata_db_user:

  Database user

- metadata_db_password:

  Database password

- metadata_db_sslmode:

  Database sslmode
