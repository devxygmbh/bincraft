# Resolve the CRAN version to build for a patch entry

Returns the latest CRAN version satisfying the entry's `versions`
constraint, or `NA_character_` when CRAN's latest does not satisfy it or
lookup fails.

## Usage

``` r
resolve_patch_version(entry)
```
