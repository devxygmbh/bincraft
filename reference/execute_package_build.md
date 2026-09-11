# Execute the actual package build

Runs the pkgbuild::build() command with proper error handling and
metadata storage for build results.

## Usage

``` r
execute_package_build(
  package_name,
  tag,
  local_clone_dir_single,
  binary_output_path,
  platform,
  arch,
  metadata_db_host,
  metadata_db_name,
  metadata_db_port,
  metadata_db_table,
  metadata_db_password,
  metadata_db_user,
  metadata_db_sslmode,
  patches = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- local_clone_dir_single:

  (character)  
  Directory path for single package clone

- binary_output_path:

  (character)  
  Local output path for binaries

- platform:

  (character)  
  Platform identifier

- arch:

  (character)  
  Architecture

- metadata_db_host:

  (character)  
  Host value of metadata database

- metadata_db_name:

  (character)  
  Name of metadata database

- metadata_db_port:

  (integer)  
  Port of metadata database

- metadata_db_table:

  (character)  
  Table name of metadata database

- metadata_db_password:

  (character)  
  User password of metadata database

- metadata_db_user:

  (character)  
  User value of metadata database

- metadata_db_sslmode:

  (character)  
  SSL mode for metadata database

- patches:

  Optional path to a patch registry directory containing a
  `registry.json` (and any referenced diff files). When set, matching
  packages are pre-built as patched binaries and installed into the
  build library before dependency installation. Defaults to `NULL` (no
  patching).

## Value

List with success status and build time
