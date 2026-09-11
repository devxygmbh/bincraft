# Classify an R source package by its ABI-risk tier

Inspects an unpacked R source package (or a `.tar.gz` of one) and labels
it as one of:

## Usage

``` r
abi_classify(path)
```

## Arguments

- path:

  Path to an unpacked source-package directory (containing
  `DESCRIPTION`) or to a `.tar.gz` of one. Tarballs are untarred to a
  temporary directory.

## Value

A list with elements:

- `tier`: one of `"pure-r"`, `"safe-compiled"`, `"risky"`.

- `reason`: short string explaining the classification.

- `hits`: character vector of packages or symbols that triggered
  `"risky"`; empty otherwise.

## Details

- `"pure-r"`: no compilation needed — build once, serve for every
  R-version slot.

- `"safe-compiled"`: compiles, but only touches the stable R C-API —
  build once per major.minor.

- `"risky"`: links to a known-volatile dependency (e.g. `Rcpp`, `rlang`)
  or references internal R C-API symbols known to change across minor
  versions — build per R minor version.

Decision rules (first match wins):

1.  `pure-r` if `DESCRIPTION` has `NeedsCompilation: no`, or there is no
    `src/`, or `src/` contains no `.c`, `.cc`, `.cpp`, `.f`, `.f90`
    files.

2.  `risky` if `LinkingTo` mentions any package in
    [`abi_risky_linking_deps()`](https://bincraft.doc.rpkgs.com/reference/abi_curated_lists.md).

3.  `risky` if any file in `src/` mentions a symbol in
    [`abi_volatile_symbols()`](https://bincraft.doc.rpkgs.com/reference/abi_curated_lists.md)
    (plain regex grep, no C parsing).

4.  `safe-compiled` otherwise.

## See also

[`abi_volatile_symbols()`](https://bincraft.doc.rpkgs.com/reference/abi_curated_lists.md),
[`abi_risky_linking_deps()`](https://bincraft.doc.rpkgs.com/reference/abi_curated_lists.md).
