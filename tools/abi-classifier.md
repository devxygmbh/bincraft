# ABI classifier — design and consumer guide

`bincraft::abi_classify()` classifies an R source package by its ABI-risk tier so build pipelines can pick the right rebuild strategy.
This document explains the model, the API, the curated data files, and how a downstream consumer (e.g. paquetier) should use it.

## Why

A naive build pipeline compiles every package against every supported R minor version.
But most CRAN packages don't actually need that:

- Most are pure R — no compilation at all.
- Another large slice compiles but only touches the long-stable R C-API (`Rf_allocVector`, `INTEGER()`, `PROTECT`/`UNPROTECT`, `R_NilValue`, ...).
  A binary built against just that surface loads cleanly across every R minor since R 3.x.
- Only a minority reaches into volatile internals (`R_mkClosure`, `R_MakeMissingBinding`, closure/promise/binding accessors, `USE_RINTERNALS`-gated struct layouts) and genuinely has to be rebuilt per R minor.

Empirically (full-CRAN scan, 2026-05-23, ~23,752 packages):

| tier            |     share | strategy                                                    |
| --------------- | --------: | ----------------------------------------------------------- |
| `pure-r`        | **78.6%** | build/serve once for every R-version slot                   |
| `safe-compiled` |  **7.7%** | compile once, replicate the artifact across per-minor slots |
| `risky`         | **13.6%** | compile per R minor (current behavior)                      |

→ ~86% of CRAN can skip per-minor recompiles.
That is the prize the classifier exists to unlock.

## Tiers

The classifier produces exactly one of three tiers per package.

### `pure-r`

No compilation needed.
The "binary" is the source tarball plus install-time bytecode.
Single artifact serves every R-version slot.

### `safe-compiled`

Has compiled code but only touches the long-stable R C-API.
A binary built under R _x.y_ is expected to load cleanly under R _x.z_ for any other minor _z_.
bincraft compiles **once** and uploads N identical copies (one per supported minor slot).

> Important: from a client requesting `bin/<os>/contrib/4.4/foo.tgz`, a `safe-compiled` artifact and a `risky` artifact are indistinguishable — both live in the 4.4 slot, both work under R 4.4.
> The "cheap build vs. fresh build" distinction matters for bincraft, _not_ for the artifact consumer.
> See [Where this fits with paquetier](#where-this-fits-with-paquetier).

### `risky`

Either links to a known-volatile dependency (`Rcpp`, `cpp11`, `rlang`, `vctrs`) or grep-positive on a curated set of volatile R C-API symbols.
Must be recompiled per R minor version.

## Decision rules

First match wins.
The order matters: rule 2 catches the bulk of risky packages cheaply (no source scan); rule 3 only runs on packages that survived rule 2.

### Rule 1 — `pure-r`

Match if any of:

- `DESCRIPTION` has `NeedsCompilation: no`, _or_
- there is no `src/` directory, _or_
- `src/` contains no `.c`, `.cc`, `.cpp`, `.f`, `.f90` files.

Reason: `"no compilation needed"`. `hits = character()`.

### Rule 2 — `risky` via `LinkingTo`

Match if `LinkingTo` references any package in [`abi_risky_linking_deps()`](#data-files).

Reason: `"LinkingTo <pkg>[, <pkg>...]"`. `hits = <matched package names>`.

### Rule 3 — `risky` via symbol grep

Match if any source file under `src/` mentions a symbol in [`abi_volatile_symbols()`](#data-files). Plain regex/grep over file contents — no real C parsing.

Reason: `"uses <sym>[, <sym>...]"`. `hits = <matched symbol names>`.

### Rule 4 — `safe-compiled`

Match otherwise.

Reason: `"compiled but only stable R C-API detected"`. `hits = character()`.

## API surface

```r
abi_classify(path)
```

`path` is a directory containing `DESCRIPTION`, or a path to a `.tar.gz` of one (untarred internally to a temp dir).
Returns a list:

```r
list(
  tier   = c("pure-r", "safe-compiled", "risky"),   # exactly one
  reason = "<short string>",
  hits   = character()                              # symbols/packages, empty otherwise
)
```

Errors only when the path doesn't exist or doesn't contain a `DESCRIPTION`.
No side effects, no network calls.

```r
needs_per_minor_recompile(path)
```

Thin convenience wrapper.
Returns a length-1 logical: `TRUE` iff `tier == "risky"`.
Attaches `tier`, `reason`, `hits` as attributes so a single call answers both "yes/no" and "why":

```r
result <- needs_per_minor_recompile("path/to/pkg")
if (result) message("Recompile per R minor; reason: ", attr(result, "reason"))
```

Note this is a strictly _bincraft-internal_ "do I need to invoke the compiler again per minor?" question.
It is _not_ the same as the artifact-level "does one binary work across all R versions?" question (which is TRUE only for `pure-r`).
`safe-compiled` is FALSE here (one compile is enough) but is still stored per-minor at the artifact level.

```r
abi_volatile_symbols()
abi_risky_linking_deps()
```

Read-only getters that return the curated lists driving rules 2 and 3.
Exposed for inspection and for downstream pipelines that want to verify their own logic against bincraft's.

## Data files

The curated lists live as plain-text files under `inst/extdata/` so they can be edited without code changes.

### `abi_risky_linking_deps.txt`

```text
rlang
Rcpp
cpp11
vctrs
```

In practice, ~97% of all risky-via-LinkingTo CRAN packages link to `Rcpp` alone.
`cpp11` and `vctrs` cover ~3%.
`rlang` as a `LinkingTo` entry catches effectively zero CRAN packages today — kept as forward-compatible guard; `rlang` itself is caught by rule 3 against its own source.

### `abi_volatile_symbols.txt`

```text
R_mkClosure
R_MakeMissingBinding
R_HashtabSEXP
R_ActiveBindingFunction
R_ClosureExpr
R_ClosureFormals
R_ClosureEnv
R_PromiseExpr
R_PromiseValue
R_PromiseEnv
R_NewEnv
R_BindingIsLocked
R_LockBinding
R_MakeActiveBinding
```

Source of truth: paquetier's ABI-matrix workflow, which emits `LOAD_ERROR=undefined symbol: ...` lines when a binary built under one R minor fails to load under another.
Append new symbols from those lines as they appear in future matrix runs.
The current entries were derived from a paquetier ABI-matrix run across R 4.2 – 4.6 using `rlang` and `ragg` as canaries.

**Two symbols deliberately excluded** after the 2026-05-23 full-CRAN audit:

- `R_GetCCallable` — stable public API since R 2.3 for cross-package C calls.
  Failures attributed to it actually came from the _provider_ package's exported function disappearing, not from the lookup mechanism being volatile.
- `USE_RINTERNALS` — a `#define` toggle, not a usage.
  Its presence merely permits struct-layout-dependent access; it doesn't prove it.

Together those two had caused 59/90 (66%) of rule-3 upgrades to be false positives, sweeping in stable packages like Hmisc, stringi, digest, Rcpp itself.
The rationale is also captured as a comment in `inst/extdata/abi_volatile_symbols.txt` so it isn't silently re-added on a future `LOAD_ERROR` diff.

## Where this fits with paquetier

bincraft's 3-tier model is a build-strategy concept, not a paquetier UI surface.
The right boundary:

| bincraft tier   | paquetier artifact scope                                 | paquetier UI          |
| --------------- | -------------------------------------------------------- | --------------------- |
| `pure-r`        | universal — one artifact for every R version             | `is_universal: true`  |
| `safe-compiled` | per-minor slot, but cross-version compatible in practice | `is_universal: false` |
| `risky`         | per-minor slot                                           | `is_universal: false` |

At the paquetier artifact level the right primitive is a single `is_universal: bool` — TRUE only for `pure-r`.
Source vs. binary distinction belongs in a separate `artifact_type` field, not in compatibility scope.

The two booleans bincraft exposes answer different questions and collapse the 3 tiers differently:

|                                                                         | `pure-r` | `safe-compiled` | `risky`  |
| ----------------------------------------------------------------------- | -------- | --------------- | -------- |
| `needs_per_minor_recompile()` (bincraft: "must I recompile per minor?") | FALSE    | FALSE           | **TRUE** |
| `is_universal` (artifact: "one artifact, all R versions?")              | **TRUE** | FALSE           | FALSE    |

`safe-compiled` is FALSE for both — same package, two valid projections.
That is exactly why `abi_classify()` keeps the 3-tier enum on the bincraft side.

### Consumer pattern in paquetier's build pipeline

```r
classification <- bincraft::abi_classify(pkg_source_dir)

switch(classification$tier,
  "pure-r"        = build_once_serve_everywhere(pkg),
  "safe-compiled" = compile_once_replicate_per_minor(pkg),
  "risky"         = compile_per_minor(pkg)
)
```

Or, if the pipeline only needs to know whether to enter the per-minor loop:

```r
if (bincraft::needs_per_minor_recompile(pkg_source_dir)) {
  for (minor in supported_r_minors) compile_for(pkg, minor)
} else {
  compile_once_then_distribute(pkg)
}
```

## Known limitations

**Volatile-symbol list is conservative-by-necessity.**
It can only catch symbols we know to look for.
When a future R minor introduces a new ABI break, the paquetier matrix's `LOAD_ERROR` lines will name new symbols — append them to `inst/extdata/abi_volatile_symbols.txt`.

**Rule 3 uses plain regex/grep on file contents.**
Macros, conditionally compiled blocks, and comments are not filtered.
In exchange the classifier needs no C toolchain to run.
A future `bincraft::abi_required_symbols(so_path)` using `nm -u` on the built `.so` would be more accurate; tracked as a separate task.

**The `LinkingTo` curated list is small.**
It catches the volatility-amplifying packages we have evidence for.
Extend conservatively — only when matrix evidence shows the dep itself drags packages into ABI-risky territory.

## Reproducing the CRAN audit

Three exploratory scripts live in `tools/`, kept out of the package build via `.Rbuildignore`.

| script                    | purpose                                                                                     | runtime            |
| ------------------------- | ------------------------------------------------------------------------------------------- | ------------------ |
| `tools/abi_cran_count.R`  | rules 1+2 projection over the live CRAN `PACKAGES` index. No downloads.                     | ~10 s              |
| `tools/abi_cran_sample.R` | full classifier on a random N-sample of safe-LinkingTo candidates (downloads each tarball). | ~2–3 min for N=100 |
| `tools/abi_cran_full.R`   | full classifier on every `NeedsCompilation: yes` CRAN package, parallel downloads.          | ~100 s on 16 cores |

Run any of them via `Rscript tools/<name>.R`.
`abi_cran_full.R` writes its results table to `/tmp/abi_cran_full_results.rds`.
Set `ABI_CORES=<n>` to control parallelism.

These scripts are intentionally self-contained — they inline the same curated lists and rule logic so they can run without a bincraft install on the target host.
If you change the curated lists in `inst/extdata/`, update the inlined copies in `tools/abi_cran_full.R` and re-run for fresh numbers.

## Tests

`tests/testthat/test-abi_classify.R` exercises every rule path and both error conditions plus the `needs_per_minor_recompile` wrapper.
On-disk fixtures are built per-test from `withr::local_tempdir` so they don't leak between runs.

Run via `devtools::test(filter = "abi_classify")`.
