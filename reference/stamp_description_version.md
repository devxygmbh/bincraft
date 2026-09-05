# Stamp a version into a cloned DESCRIPTION

Rewrites the `Version` field of `clone_dir/DESCRIPTION` in place and
returns the value it replaced. Returns `NA` and leaves the clone
untouched when there is no `DESCRIPTION` or it carries no `Version`
field; the build fails on its own with a better message than a silently
invented version would produce.

## Usage

``` r
stamp_description_version(clone_dir, version)
```

## Arguments

- clone_dir:

  (character)  
  Directory holding the cloned package sources.

- version:

  (character)  
  Version to write into the `Version` field.

## Value

The replaced version, or `NA` when nothing was rewritten.
