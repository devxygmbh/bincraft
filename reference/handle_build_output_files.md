# Handle build output files and cleanup

Manages the renaming of build output files, cleanup of temporary files,
and calculation of file sizes for metadata storage.

## Usage

``` r
handle_build_output_files(
  package_name,
  tag,
  binary_output_path,
  local_clone_dir_single
)
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

- local_clone_dir_single:

  (character)  
  Directory path for single package clone

## Value

List with file existence and size information
