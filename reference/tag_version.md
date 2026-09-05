# Package version named by a git tag

A tag is a release identity: whatever `v5.0.0` names has to be built and
published as version `5.0.0`. Strips an optional leading `v` and returns
`NA` for refs that name no version R would accept in a `DESCRIPTION`
`Version` field: branch names, commit SHAs, and pre-release tags such as
`v2.0.0-rc1`, whose suffix is not an integer.

## Usage

``` r
tag_version(tag)
```

## Arguments

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

## Value

Character vector of versions, `NA` where the tag names none.
