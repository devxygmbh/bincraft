# Archive a single package

Archives old versions of a package, keeping only the latest version in
the main repository and moving older versions to Archive directory.

## Usage

``` r
archive_single_package(
  package_name,
  remote_bin_dir,
  is_r_minor_sensitive,
  minor_version,
  files
)
```

## Arguments

- package_name:

  (character)  
  Package name

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

- files:

  (character)  
  Character vector of all files in repository

## Value

Invisible NULL
