# Find old package versions to archive

Identifies which package versions should be archived by comparing
against the latest CRAN version. Falls back to version history if
needed.

## Usage

``` r
find_old_versions(all_versions, package_name, package_name_local, last_version)
```

## Arguments

- all_versions:

  (character)  
  Character vector of all available package versions

- package_name:

  (character)  
  Package name

- package_name_local:

  (character)  
  Package name for local operations

- last_version:

  (character)  
  Latest version string from CRAN

## Value

List with old_versions and index elements
