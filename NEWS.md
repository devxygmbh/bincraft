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
