# Construct a `Built` field value for the current build environment

cranlike derives package metadata from the CRAN *source* `DESCRIPTION`,
which never carries a `Built:` field (R stamps that only when it builds
a binary). Our S3 tarballs *are* binaries, so we pass this stamp to
[`cranlike::update_PACKAGES()`](https://rdrr.io/pkg/cranlike/man/update_PACKAGES.html)/[`cranlike::add_PACKAGES()`](https://rdrr.io/pkg/cranlike/man/add_PACKAGES.html)
to advertise them as such. Binary-aware clients (e.g. `uvr`) key their
binary-vs-source decision on the `Built:` platform triple + R minor;
without it they treat the repo as source-only and compile everything,
which needs system `-dev` libraries.

## Usage

``` r
built_stamp(
  platform = R.version$platform,
  r_version = getRversion(),
  time = Sys.time()
)
```

## Arguments

- platform:

  Platform triple, defaulting to `R.version$platform`.

- r_version:

  R version, defaulting to `getRversion()`.

- time:

  Build time, defaulting to `Sys.time()`.

## Details

The triple comes from `R.version$platform` and the version from
`getRversion()`, so this must run in the same build environment that
produced the binaries (the surrounding arch/codename logic already
assumes this).
