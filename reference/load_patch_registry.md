# Load and validate the patch registry

Reads `registry.json` from `patches_dir` and returns normalized entries.

## Usage

``` r
load_patch_registry(patches_dir)
```

## Arguments

- patches_dir:

  Directory containing `registry.json` and any patch files, or `NULL` to
  disable patching.

## Value

A list of normalized patch entries (possibly empty).
