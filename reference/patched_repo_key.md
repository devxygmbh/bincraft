# Content-addressed key for an assembled patched repo

Derives a stable hash from the resolved plan (each entry's
`patch_cache_key`, which already folds in package, version, platform,
arch, R minor, and patch content). Identical patch sets yield an
identical key regardless of call order or invocation, so the repo can
live at a stable path.

## Usage

``` r
patched_repo_key(keys)
```
