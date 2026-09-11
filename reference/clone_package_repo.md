# Clone package repository

Clones a package repository if it doesn't already exist locally.

## Usage

``` r
clone_package_repo(package_name, tag, local_clone_dir_single)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- local_clone_dir_single:

  (character)  
  Directory path for single package clone

## Value

Invisible NULL
