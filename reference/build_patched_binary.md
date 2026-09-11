# Build a patched binary for one registry entry, in isolation

Downloads CRAN source for `version`, applies the source patch (if any),
and builds a binary with the entry's env / configure / Makevars
overrides scoped to this build only. Returns the built tarball path, or
`NULL` on any failure.

## Usage

``` r
build_patched_binary(entry, version, dest_dir)
```
