# Get system architecture information

Determines the system architecture details needed for package building,
including Linux suffix (musl/gnu), tarball ID, and architecture.

## Usage

``` r
get_system_architecture_info(binary_output_path)
```

## Arguments

- binary_output_path:

  (character)  
  Local output path for binaries

## Value

List with linux_suffix, tarball_id, and tarball_arch elements
