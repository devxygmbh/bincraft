# Version used in artifact names for a git tag

[`tag_version()`](https://bincraft.doc.rpkgs.com/reference/tag_version.md)
with the tag itself as the fallback, for the paths that need a name for
every ref rather than only for version tags.

## Usage

``` r
artifact_version(tag)
```

## Arguments

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

## Value

Character vector of versions.
