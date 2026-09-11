# ABI-risky curated lists

The curated lists
[`abi_classify()`](https://bincraft.doc.rpkgs.com/reference/abi_classify.md)
consults. Stored as plain-text files under `inst/extdata/` so they can
be updated without code changes.

## Usage

``` r
abi_volatile_symbols()

abi_risky_linking_deps()
```

## Value

Character vector.

## Details

- `abi_volatile_symbols()` returns the R C-API symbols whose presence in
  package sources marks the package as ABI-risky.

- `abi_risky_linking_deps()` returns the packages whose `LinkingTo`
  presence marks a package as ABI-risky.
