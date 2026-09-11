# Prepare a local repo of patched binaries for the current build

For each registry entry matching the current platform, ensures a patched
binary is present in `repo_dir` (from `cache_dir` if available, else
built and then cached) and writes a `PACKAGES` index over them.

The repo is assembled at a content-addressed path derived from the
resolved patch set. Because every
[`build_binary_package()`](https://bincraft.doc.rpkgs.com/reference/build_binary_package.md)
resolution in a run passes the same patches/platform/arch/R, they all
resolve to the *same* `file://` repo URL, so `{pkgcache}` reuses one
`_metadata` snapshot instead of minting a fresh ~70 MB one per
resolution. See <https://codefloe.com/rpkgs/bincraft/issues/62>.

## Usage

``` r
prepare_patched_repo(
  patches_dir,
  platform,
  arch,
  r_minor,
  cache_dir = file.path("/mnt", "cache", "patched-binaries"),
  repo_dir = NULL
)
```

## Arguments

- patches_dir:

  Directory with `registry.json`, or `NULL`.

- platform:

  Build platform, e.g. `"ubuntu-2604"`.

- arch:

  Build arch, e.g. `"amd64"`.

- r_minor:

  R `"major.minor"` string, e.g. `"4.5"`.

- cache_dir:

  Persistent cache for patched binaries.

- repo_dir:

  Directory to assemble the local repo in. Defaults to a
  content-addressed path under `cache_dir` so identical patch sets reuse
  a stable repo URL; pass an explicit path to override.

## Value

`repo_dir` if at least one patched binary was produced, else `NULL`.
