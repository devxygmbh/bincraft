# Build helper functions for package building operations Parse package tag pairs from difference strings

Extracts package names and version tags from package difference strings
in the format "package_version.tar.gz".

## Usage

``` r
parse_package_tag_pairs(pkg_differences)
```

## Arguments

- pkg_differences:

  (character)  
  Character vector of package difference strings to check

## Value

List of lists containing pkg and tag elements
