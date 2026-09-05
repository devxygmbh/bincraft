# Clone package repository

Clones a git repository for a specific package and tag.

## Usage

``` r
clone_repository(package_name, tag, source_org_url, local_clone_dir_single)
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

- local_clone_dir_single:

  (character)  
  Directory path for single package clone

## Value

Invisible NULL
