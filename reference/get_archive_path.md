# Get archive destination path

Constructs the destination path for archiving old package versions,
accounting for R minor version sensitivity.

## Usage

``` r
get_archive_path(
  remote_bin_dir,
  is_r_minor_sensitive,
  minor_version,
  package_name,
  old_versions
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

- old_versions:

  (character)  
  Character vector of old version file paths

## Value

Character vector with archive destination paths
