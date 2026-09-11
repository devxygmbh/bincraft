# Move and rename built tarball files

Renames the built package tarball from the system-specific filename to
the standard package_version.tar.gz format.

## Usage

``` r
move_and_rename_tarball(package_name, tag, binary_output_path, system_info)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- binary_output_path:

  (character)  
  Local output path for binaries

- system_info:

  (list)  
  List with system architecture information

## Value

Invisible NULL
