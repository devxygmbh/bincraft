# Build the S3 remote contrib dir for a package index

Build the S3 remote contrib dir for a package index

## Usage

``` r
package_index_remote_dir(s3_bucket, arch, codename, r_minor = NULL)
```

## Arguments

- r_minor:

  Optional `"major.minor"` string (e.g. `"4.4"`). When non-NULL the path
  points at the per-minor slot.
