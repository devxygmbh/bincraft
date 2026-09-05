# Apply a unified diff to an unpacked source tree

Uses `git apply` rather than the `patch` CLI, because `git` is always
present in the build environments (it clones every package) whereas
`patch` is not (e.g. minimal Alpine images). A non-applying or
already-applied patch exits non-zero, so this returns FALSE instead of
corrupting the tree. `git apply` does not require `pkg_src` to be a git
repository.

## Usage

``` r
apply_source_patch(patch_path, pkg_src)
```
