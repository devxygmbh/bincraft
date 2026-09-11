# Archive helper functions for package archiving operations Get remote search path for package archives

Constructs the S3 path for package archives, accounting for R minor
version sensitivity.

## Usage

``` r
get_remote_search_path(
  remote_bin_dir,
  is_r_minor_sensitive,
  minor_version,
  package_name
)
```

## Arguments

- remote_bin_dir:

  (character)  
  Remote binary directory path

- is_r_minor_sensitive:

  (logical)  
  If TRUE, treats the package to be minor version specific and injects a
  version identifier into the file path.

- minor_version:

  (character)  
  R minor version string (e.g., "4.3")

- package_name:

  (character)  
  Package name

## Value

Character string with archive search path
