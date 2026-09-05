# Filter git tags for package versions

Retrieves and filters git tags from a package repository, excluding
R-prefixed tags and applying tag limits. Uses forge HTTP APIs (GitHub,
Forgejo/Gitea) with git ls-remote as fallback.

## Usage

``` r
filter_tags(package_name, tag, source_org_url, tag_limit)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

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
