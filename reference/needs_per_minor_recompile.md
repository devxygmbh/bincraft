# Does this package need to be recompiled per R minor version?

Thin convenience wrapper around
[`abi_classify()`](https://bincraft.doc.rpkgs.com/reference/abi_classify.md)
that returns a single logical: `TRUE` if the package must be recompiled
per R minor version (tier `"risky"`), `FALSE` otherwise.

## Usage

``` r
needs_per_minor_recompile(path)
```

## Arguments

- path:

  Path to an unpacked source-package directory (containing
  `DESCRIPTION`) or to a `.tar.gz` of one. Tarballs are untarred to a
  temporary directory.

## Value

A logical of length 1. Attributes:

- `tier`: the
  [`abi_classify()`](https://bincraft.doc.rpkgs.com/reference/abi_classify.md)
  tier (`"pure-r"`, `"safe-compiled"`, or `"risky"`).

- `reason`: short human-readable string.

- `hits`: packages or symbols that triggered `"risky"`; empty otherwise.

## Details

Note this is a strictly bincraft-internal "do I need to invoke the
compiler again per minor?" question — it is not the same as the
artifact-level "does one binary work across all R versions?" question
(which is TRUE only for `pure-r`). `safe-compiled` is FALSE here (one
compile is enough) but is still stored per-minor at the artifact level.

The full classification is attached as attributes (`tier`, `reason`,
`hits`) so callers can inspect *why* without a second call.

## See also

[`abi_classify()`](https://bincraft.doc.rpkgs.com/reference/abi_classify.md)
for the full structured result.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- needs_per_minor_recompile("path/to/pkg")
if (result) {
  message("Recompile per R minor; reason: ", attr(result, "reason"))
}
} # }
```
