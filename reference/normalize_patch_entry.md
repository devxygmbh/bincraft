# Normalize and validate a single patch registry entry

Normalize and validate a single patch registry entry

## Usage

``` r
normalize_patch_entry(entry, patches_dir)
```

## Arguments

- entry:

  A list parsed from `registry.json`.

- patches_dir:

  Directory used to resolve a relative `patch` path.

## Value

The entry with defaults filled and `patch_path` resolved.
