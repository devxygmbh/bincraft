# Process tag filtering for check_s3_packages

Determines which tags to process based on the tag parameter.

## Usage

``` r
process_tag_filtering(tag, package_name, source_org_url, tag_limit)
```

## Arguments

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- package_name:

  (character)  
  Package name

- source_org_url:

  (character)  
  Git organization URL in which to search for the package sources. The
  final URL will a combination from this argument and the package name.
  This also implies that the package source must exist in a repository
  with the same name. Must be a https:// URL, local paths are not
  supported.

- tag_limit:

  (integer)  
  The most recent `n` tags to consider.

## Value

Character vector of filtered tags
