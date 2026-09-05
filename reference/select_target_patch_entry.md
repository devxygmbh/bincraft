# Select the single registry entry for a target package build

Returns the entry matching `package` on the current platform whose
version constraint is satisfied by `version`, or `NULL` when none
applies. Used to patch the target package's own source before
[`pkgbuild::build()`](https://pkgbuild.r-lib.org/reference/build.html)
(dependency patching goes through
[`prepare_patched_repo()`](https://bincraft.doc.rpkgs.com/reference/prepare_patched_repo.md)
instead).

## Usage

``` r
select_target_patch_entry(patches_dir, package, version, platform, arch)
```
