# Changelog

## bincraft (development version)

- The two remaining existence gates now tell a source fallback from a
  binary. Fixing
  [`check_s3_root_package()`](https://bincraft.doc.rpkgs.com/reference/check_s3_root_package.md)
  let a rebuild get as far as the build step, where a second gate in
  [`build_single_tag()`](https://bincraft.doc.rpkgs.com/reference/build_single_tag.md)
  skipped it again with “already exists in S3 and `force = FALSE`”, and
  a third in
  [`upload_single_binary()`](https://bincraft.doc.rpkgs.com/reference/upload_single_binary.md)
  would then have refused to publish the binary because the source
  occupied its key. All three now share one helper,
  `remote_object_state()`, which answers `"absent"`, `"source"` or
  `"binary"` rather than a bare yes/no, and a source fallback is
  overwritten rather than left in place. Unknown stays “binary”
  throughout, so an unreadable ETag or an unreachable CRAN still cannot
  schedule a rebuild of the repository.

- The source-fallback detection now actually reads the object’s MD5.
  `remote_object_md5()` looked for an `ETag` column, but
  [`s3fs::s3_file_info()`](https://rdrr.io/pkg/s3fs/man/info.html)
  snake-cases the `head_object` response and renames `e_tag` to `etag`.
  Reading a missing column yields `NULL` rather than an error, so every
  object came back as “MD5 unknown”, every caller took the conservative
  “assume it is a binary” branch, and the checks added in 5.1.1 and
  5.1.2 were inert. A 13,389-package rebuild of `amd64/alpine324`
  running 5.1.2 still skipped every package with “All packages to be
  built already exist in the remote bucket”. Both spellings are now
  accepted, and the regression is covered by tests that feed
  `remote_object_md5()` a realistic `s3fs` return value instead of
  stubbing it out.

- The pre-build skip now also tells a source fallback from a binary.
  [`check_s3_root_package()`](https://bincraft.doc.rpkgs.com/reference/check_s3_root_package.md)
  decides whether a build runs at all, and it tested only that
  *something* occupied the key. A package whose build failed has its
  CRAN source published under exactly that name, so a rebuild skipped
  every one of them with “All packages to be built already exist in the
  remote bucket” – precisely the set a rebuild exists to fix. Observed
  on `arm64/alpine324`, where a 13,647-package rebuild skipped every
  package it was given. The previous release fixed the same blind spot
  in
  [`check_for_binary()`](https://bincraft.doc.rpkgs.com/reference/check_for_binary.md),
  which decides whether to *publish* a fallback, but not this one, which
  decides whether to *build*. An object whose MD5 matches CRAN’s
  published `MD5sum` is now reported as absent, and an MD5 that cannot
  be established still counts as a binary so an outage cannot trigger a
  full rebuild.

- A package that failed to build is no longer mistaken for a binary.
  When a build fails,
  [`handle_post_build_actions()`](https://bincraft.doc.rpkgs.com/reference/handle_post_build_actions.md)
  publishes the CRAN *source* tarball in its place so the package stays
  installable, but
  [`check_for_binary()`](https://bincraft.doc.rpkgs.com/reference/check_for_binary.md)
  only tested that an object existed at the path. The fallback therefore
  became permanent: the build was never retried, and the
  missing-binaries audit never reported it, because the object was
  there. On `amd64/alpine324` that left 13,547 of 24,134 comparable
  objects (56%) as CRAN sources, all published during the June 2026
  bootstrap; `amd64/noble` sits at 4.6% for comparison.
  [`check_for_binary()`](https://bincraft.doc.rpkgs.com/reference/check_for_binary.md)
  now compares the object’s MD5 against CRAN’s published `MD5sum`, which
  costs no downloads, and reports a source fallback as missing. When the
  MD5 cannot be established (a multipart ETag, or CRAN unreachable) the
  object still counts as a binary, so neither can trigger an endless
  rebuild.

- [`check_for_binary()`](https://bincraft.doc.rpkgs.com/reference/check_for_binary.md)
  now honours `is_r_minor_sensitive`. It looked in the generic slot even
  for per-minor packages, while the source fallback wrote them to the
  per-minor slot, so it never found an existing per-minor build.

- The `Built` stamp is no longer written onto packages served as their
  CRAN source. The stamp is applied to a whole slot at once, so it
  landed on the source fallbacks too and advertised them as binaries:
  `uvr` then installs one without the system `-dev` libraries a source
  build needs, and paquetier files it under a platform it was never
  built for.
  [`upload_package_index()`](https://bincraft.doc.rpkgs.com/reference/upload_package_index.md)
  now clears `Built` on records whose MD5 matches CRAN’s. Archived
  versions are left alone, since CRAN’s index carries only current
  releases.

- A per-minor `PACKAGES*` index is now republished as a union with the
  generic slot. A per-minor slot carries only the ABI-sensitive
  packages, and `contrib.url()` can only ever address
  `<repos>/src/contrib`, so those packages were unreachable from
  `install.packages()`: on `amd64/alpine324` 2,886 packages, `curl`
  among them, existed only in the per-minor slot and were absent from
  the index base R reads.
  [`upload_package_index()`](https://bincraft.doc.rpkgs.com/reference/upload_package_index.md)
  now merges the generic slot’s records into the per-minor index before
  uploading it, tagging the per-minor records with `Path: <r_minor>` so
  their tarballs still resolve into `…/src/contrib/<r_minor>/` while the
  generic ones resolve into `…/src/contrib/`. Nothing is copied or
  duplicated, and a package built for this minor shadows the generic one
  entirely. An unreadable or empty generic index is an error rather than
  a partial publish, because replacing a published union with a
  per-minor-only index would hide most of the repository from every
  client on that minor.

- [`built_stamp()`](https://bincraft.doc.rpkgs.com/reference/built_stamp.md)
  now refuses an unusable platform or R version instead of stamping it.
  The stamp is written verbatim into every entry of a slot’s `PACKAGES`
  index and `uvr` decides binary-vs-source by matching the triple it
  carries, so a missing value does not degrade gracefully: it stamped
  the literal `NA`, no client matched it, and the whole slot silently
  reverted to source-only. This is how `arm64/alpine321` (22,930
  entries, `Built: R 4.4.0; NA`) and `arm64/alpine322` (24,696 entries,
  `Built: R 4.5.0; NA`) were written during the one-time re-index on
  2026-07-31; both slots need another full re-index to recover. Failing
  the index write is the cheaper outcome, so an absent, `NA`, empty or
  non-scalar `platform`/`r_version` is now an error.

- Dependency names are now written as quoted keys in the generated
  `uvr.toml`. R package names may contain dots, and a bare
  `data.table = "*"` is a *dotted* TOML key: it parses as package `data`
  with a sub-key `table` rather than as `data.table`. Where the parent
  name is itself a dependency, for example `rpart` alongside
  `rpart.plot`, the parse fails outright with “dotted key `rpart`
  attempted to extend non-table type (string)”. Either way `uvr lock`
  aborted and the build failed with “Error in installing dependencies
  for package …”, so every package depending on a dotted name was
  unbuildable.

- `uvr` is now pinned to the R version that runs the build.
  `run_uvr_install()` writes a `.r-version` file into the package clone
  before `uvr lock`, so both `lock` and `sync` target the session’s R.
  Previously bincraft told uvr nothing about the R version, and uvr
  picked the newest R it could find (managed installs first, then system
  R). On an image carrying a second, newer R, for example a
  `/opt/R/current` symlink already moved to 4.6 while the build runs in
  4.5, uvr resolved the lockfile for the wrong R minor and `uvr sync`
  then refused to install anything: “Refusing to install: uvr is running
  inside R 4.5 but the project pin/lockfile resolves to R 4.6”. That
  aborted every dependency install on the affected images.

- The S3 package index now advertises its binaries via the `Built`
  field.
  [`upload_package_index()`](https://bincraft.doc.rpkgs.com/reference/upload_package_index.md)
  and
  [`add_to_package_index()`](https://bincraft.doc.rpkgs.com/reference/add_to_package_index.md)
  pass a `built` stamp (build R version plus `R.version$platform`
  triple) to cranlike, which otherwise reads metadata from the CRAN
  source `DESCRIPTION` and leaves `Built` empty for every entry. Without
  it, binary-aware clients such as `uvr` treat `cran.rpkgs.com` as
  source-only and compile every package from source, which fails on
  Alpine when a system `-dev` library is missing (e.g. `openssl`).
  `install.packages()` was unaffected because it reads `Built:` from
  each tarball’s own `DESCRIPTION`. Existing indexes need a one-time
  full rebuild to backfill `Built` on entries already in the database.

- Dependencies and their system requirements are now installed with
  `uvr` instead of pak during
  [`build_binary_package()`](https://bincraft.doc.rpkgs.com/reference/build_binary_package.md).
  uvr reads `/etc/os-release`, resolves each dependency’s system
  requirements per distro, and installs them with the distro package
  manager (`apk` / `dnf` / `apt-get`) in the same pass as the R
  packages, so a build no longer fails when a dependency needs a system
  library the image does not ship (for example `gert` needing
  `libgit2`). Patched binaries are installed into the build library
  first so `uvr sync` keeps them, and pak is no longer a dependency
  (#83).

- Patched binary caches are now validated before reuse and written
  atomically.
  [`prepare_patched_repo()`](https://bincraft.doc.rpkgs.com/reference/prepare_patched_repo.md)
  verifies each cached/assembled tarball reads cleanly and carries the
  package `DESCRIPTION` plus a shared object (when it ships a `libs/`
  dir) before serving it, and rebuilds instead of reusing a corrupt
  entry. Fresh binaries are staged to a temp file and renamed into
  place, so a build killed mid-copy can no longer leave a truncated
  cache entry that made every dependent build fail with
  `tar: A lone zero block` or a missing `RcppParallel.so`.

- CRAN package versions are now resolved from CRAN’s own metadata
  (`available.packages()` plus `Meta/archive.rds`) instead of the GitHub
  REST tags API. The old `gh::gh("GET /repos/cran/{pkg}/tags")` call
  counted against the authenticated user’s 5,000 requests/hour REST
  limit, which the weekly rebuild exhausted (HTTP 403 “API rate limit
  exceeded for user ID …”). The GitHub API is still used for genuine
  non-CRAN forge sources.

- [`classify_r_minor_sensitive()`](https://bincraft.doc.rpkgs.com/reference/classify_r_minor_sensitive.md)
  now downloads the CRAN source tarball
  ([`download_cran_source()`](https://bincraft.doc.rpkgs.com/reference/download_cran_source.md))
  instead of cloning `github.com/cran`, and caches its verdict in the
  metadata database (table `abi_classification`, created on first use)
  keyed on `(package, version)` plus a signature of the curated ABI
  lists. Because the r-minor-sensitivity verdict is a property of the
  package source (independent of OS, arch and R minor), a full rebuild
  for a new OS release reuses cached verdicts and performs no downloads.
  The function is now exported and takes `metadata_db_*` arguments; pass
  them to enable caching.

## bincraft 4.4.7

- The local patched-binary repo served to `pak` is now assembled at a
  stable, content-addressed path under `cache_dir` instead of a fresh
  `tempfile()` per call. Every
  [`build_binary_package()`](https://bincraft.doc.rpkgs.com/reference/build_binary_package.md)
  resolution in a run passes the same patches/platform/arch/R, so they
  now resolve to the same `file://` repo URL and
  [pkgcache](https://r-lib.github.io/pkgcache/) reuses a single
  `_metadata` snapshot. Previously each resolution minted a new repo
  path, so [pkgcache](https://r-lib.github.io/pkgcache/) wrote a fresh
  ~70 MB `_metadata/patched-<hash>` snapshot that never repeated and
  never evicted, growing unboundedly during full-platform builds (up to
  ~165 GB observed). A fully assembled repo is now also reused as-is on
  subsequent resolutions.

## bincraft 4.4.3

- Registry patches (and their `env` / `configure_args` / `makevars`
  overrides) are now applied to the target package’s own source before
  [`pkgbuild::build()`](https://pkgbuild.r-lib.org/reference/build.html),
  not only when building patched dependencies. Previously a package that
  was the build target had its patch applied to a throwaway dependency
  repo and ignored for the uploaded binary, so source-diff/override
  patches silently had no effect on the shipped binary. A target patch
  that fails to apply now aborts the build instead of uploading an
  unpatched binary.

## bincraft 4.4.2

- [`build_patched_binary()`](https://bincraft.doc.rpkgs.com/reference/build_patched_binary.md)
  no longer fails with `is.named(envs) is not TRUE` for a patch entry
  that sets no environment variables (e.g. a pure source-diff patch).
  [`withr::with_envvar()`](https://withr.r-lib.org/reference/with_envvar.html)
  errors on an empty list, so it is now skipped when the entry’s env is
  empty.

## bincraft 4.4.1

- Source patches are now applied with `git apply` instead of the `patch`
  CLI, which is not present in all build environments (e.g. minimal
  Alpine images). `git` is always available, so registry source diffs
  apply reliably across platforms.
- [`prepare_patched_repo()`](https://bincraft.doc.rpkgs.com/reference/prepare_patched_repo.md)
  no longer emits a spurious `normalizePath: No such file` warning when
  staging a freshly built patched binary.
- [`build_patched_binary()`](https://bincraft.doc.rpkgs.com/reference/build_patched_binary.md)
  no longer fails with `is.named(envs) is not TRUE` for a patch entry
  that sets no env vars (e.g. a pure source-diff patch); the
  [`withr::with_envvar()`](https://withr.r-lib.org/reference/with_envvar.html)
  wrapper is now skipped when the env is empty.

## bincraft 4.3.1

- [`process_cran_updates()`](https://bincraft.doc.rpkgs.com/reference/process_cran_updates.md)
  gains a `patches` argument that it forwards to
  [`build_binary_package()`](https://bincraft.doc.rpkgs.com/reference/build_binary_package.md),
  so the daily update pipeline applies the same package patches
  (e.g. RcppParallel) as the bulk and targeted build paths.

## bincraft 4.3.0

- [`build_binary_package()`](https://bincraft.doc.rpkgs.com/reference/build_binary_package.md)
  gains a `patches` argument: a registry of per-package env / configure
  / Makevars overrides and source diffs that are pre-built into patched
  binaries and served to pak, fixing compiler- and OS-specific failures
  (e.g. RcppParallel) including for transitive deps.

## bincraft 4.2.1

- [`write_archive_rds()`](https://bincraft.doc.rpkgs.com/reference/write_archive_rds.md)
  (and thus
  [`upload_package_index()`](https://bincraft.doc.rpkgs.com/reference/upload_package_index.md))
  no longer errors when a slot has no archived versions yet — it returns
  an empty index instead. Fixes the `Meta/archive.rds` failure the first
  time a package lands in a fresh per-minor slot.

## bincraft 4.2.0

- [`process_cran_updates()`](https://bincraft.doc.rpkgs.com/reference/process_cran_updates.md)
  gains `r_minor_detection` (`"none"`/`"issue"`/`"classifier"`) and
  `r_minor_sensitive_only`, classifying each candidate via the ABI
  classifier and routing only `risky` packages to per-minor slots.
- [`upload_package_index()`](https://bincraft.doc.rpkgs.com/reference/upload_package_index.md)
  /
  [`add_to_package_index()`](https://bincraft.doc.rpkgs.com/reference/add_to_package_index.md)
  gain an `r_minor` argument to write/serve a per-minor `PACKAGES*`
  index under `…/contrib/<x.y>/`.
