# bincraft (development version)

- `uvr` is now pinned to the R version that runs the build.
  `run_uvr_install()` writes a `.r-version` file into the package clone before `uvr lock`, so both `lock` and `sync` target the session's R.
  Previously bincraft told uvr nothing about the R version, and uvr picked the newest R it could find (managed installs first, then system R).
  On an image carrying a second, newer R, for example a `/opt/R/current` symlink already moved to 4.6 while the build runs in 4.5, uvr resolved the lockfile for the wrong R minor and `uvr sync` then refused to install anything: "Refusing to install: uvr is running inside R 4.5 but the project pin/lockfile resolves to R 4.6".
  That aborted every dependency install on the affected images.
- The S3 package index now advertises its binaries via the `Built` field.
  `upload_package_index()` and `add_to_package_index()` pass a `built` stamp
  (build R version plus `R.version$platform` triple) to cranlike, which
  otherwise reads metadata from the CRAN source `DESCRIPTION` and leaves `Built`
  empty for every entry. Without it, binary-aware clients such as `uvr` treat
  `cran.rpkgs.com` as source-only and compile every package from source, which
  fails on Alpine when a system `-dev` library is missing (e.g. `openssl`).
  `install.packages()` was unaffected because it reads `Built:` from each
  tarball's own `DESCRIPTION`. Existing indexes need a one-time full rebuild to
  backfill `Built` on entries already in the database.
- Dependencies and their system requirements are now installed with `uvr`
  instead of pak during `build_binary_package()`.
  uvr reads `/etc/os-release`, resolves each dependency's system requirements
  per distro, and installs them with the distro package manager (`apk` / `dnf` /
  `apt-get`) in the same pass as the R packages, so a build no longer fails when
  a dependency needs a system library the image does not ship (for example
  `gert` needing `libgit2`).
  Patched binaries are installed into the build library first so `uvr sync`
  keeps them, and pak is no longer a dependency (#83).
- Patched binary caches are now validated before reuse and written atomically.
  `prepare_patched_repo()` verifies each cached/assembled tarball reads cleanly
  and carries the package `DESCRIPTION` plus a shared object (when it ships a
  `libs/` dir) before serving it, and rebuilds instead of reusing a corrupt
  entry. Fresh binaries are staged to a temp file and renamed into place, so a
  build killed mid-copy can no longer leave a truncated cache entry that made
  every dependent build fail with `tar: A lone zero block` or a missing
  `RcppParallel.so`.

- CRAN package versions are now resolved from CRAN's own metadata
  (`available.packages()` plus `Meta/archive.rds`) instead of the GitHub REST
  tags API. The old `gh::gh("GET /repos/cran/{pkg}/tags")` call counted against
  the authenticated user's 5,000 requests/hour REST limit, which the weekly
  rebuild exhausted (HTTP 403 "API rate limit exceeded for user ID ..."). The
  GitHub API is still used for genuine non-CRAN forge sources.
- `classify_r_minor_sensitive()` now downloads the CRAN source tarball
  (`download_cran_source()`) instead of cloning `github.com/cran`, and caches
  its verdict in the metadata database (table `abi_classification`, created on
  first use) keyed on `(package, version)` plus a signature of the curated ABI
  lists. Because the r-minor-sensitivity verdict is a property of the package
  source (independent of OS, arch and R minor), a full rebuild for a new OS
  release reuses cached verdicts and performs no downloads. The function is now
  exported and takes `metadata_db_*` arguments; pass them to enable caching.

# bincraft 4.4.7

- The local patched-binary repo served to `pak` is now assembled at a stable,
  content-addressed path under `cache_dir` instead of a fresh `tempfile()` per
  call. Every `build_binary_package()` resolution in a run passes the same
  patches/platform/arch/R, so they now resolve to the same `file://` repo URL
  and `{pkgcache}` reuses a single `_metadata` snapshot. Previously each
  resolution minted a new repo path, so `{pkgcache}` wrote a fresh ~70 MB
  `_metadata/patched-<hash>` snapshot that never repeated and never evicted,
  growing unboundedly during full-platform builds (up to ~165 GB observed).
  A fully assembled repo is now also reused as-is on subsequent resolutions.

# bincraft 4.4.3

- Registry patches (and their `env` / `configure_args` / `makevars` overrides)
  are now applied to the target package's own source before `pkgbuild::build()`,
  not only when building patched dependencies. Previously a package that was the
  build target had its patch applied to a throwaway dependency repo and ignored
  for the uploaded binary, so source-diff/override patches silently had no
  effect on the shipped binary. A target patch that fails to apply now aborts
  the build instead of uploading an unpatched binary.

# bincraft 4.4.2

- `build_patched_binary()` no longer fails with `is.named(envs) is not TRUE`
  for a patch entry that sets no environment variables (e.g. a pure
  source-diff patch). `withr::with_envvar()` errors on an empty list, so it is
  now skipped when the entry's env is empty.

# bincraft 4.4.1

- Source patches are now applied with `git apply` instead of the `patch` CLI,
  which is not present in all build environments (e.g. minimal Alpine images).
  `git` is always available, so registry source diffs apply reliably across
  platforms.
- `prepare_patched_repo()` no longer emits a spurious
  `normalizePath: No such file` warning when staging a freshly built patched
  binary.
- `build_patched_binary()` no longer fails with `is.named(envs) is not TRUE`
  for a patch entry that sets no env vars (e.g. a pure source-diff patch); the
  `withr::with_envvar()` wrapper is now skipped when the env is empty.

# bincraft 4.3.1

- `process_cran_updates()` gains a `patches` argument that it forwards to
  `build_binary_package()`, so the daily update pipeline applies the same
  package patches (e.g. RcppParallel) as the bulk and targeted build paths.

# bincraft 4.3.0

- `build_binary_package()` gains a `patches` argument: a registry of
  per-package env / configure / Makevars overrides and source diffs that are
  pre-built into patched binaries and served to pak, fixing compiler- and
  OS-specific failures (e.g. RcppParallel) including for transitive deps.

# bincraft 4.2.1

- `write_archive_rds()` (and thus `upload_package_index()`) no longer errors when a
  slot has no archived versions yet — it returns an empty index instead. Fixes the
  `Meta/archive.rds` failure the first time a package lands in a fresh per-minor slot.

# bincraft 4.2.0

- `process_cran_updates()` gains `r_minor_detection` (`"none"`/`"issue"`/`"classifier"`)
  and `r_minor_sensitive_only`, classifying each candidate via the ABI classifier and
  routing only `risky` packages to per-minor slots.
- `upload_package_index()` / `add_to_package_index()` gain an `r_minor` argument to
  write/serve a per-minor `PACKAGES*` index under `…/contrib/<x.y>/`.
