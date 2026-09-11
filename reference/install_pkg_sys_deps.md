# Install R dependencies and their system requirements for a package

Clones the package source, then installs its dependency tree and the
system libraries those dependencies need via `uvr`
(`uvr sync --install-system-deps`), so a dependency needing a system
library no longer fails the build. Transient failures are retried with
exponential backoff.

## Usage

``` r
install_pkg_sys_deps(
  package_name,
  tag,
  local_clone_dir,
  platform = platform,
  aggressive_cleanup = FALSE,
  patches = NULL,
  arch = NULL
)
```

## Arguments

- package_name:

  (character)  
  Package name

- tag:

  (character)  
  Tag/version. Tags starting with "R-" are filtered out.

- local_clone_dir:

  (character)  
  Path to clone git repos into

- platform:

  (character)  
  Platform identifier

- aggressive_cleanup:

  (logical)  
  Perform additional cache cleanup before installation

- patches:

  Optional path to a patch registry directory containing a
  `registry.json` (and any referenced diff files). When set, matching
  packages are pre-built as patched binaries and installed into the
  build library before dependency installation. Defaults to `NULL` (no
  patching).

- arch:

  (character)  
  Architecture

## Details

When patches apply, their pre-built binaries are installed into the
build library first, so `uvr sync` keeps them instead of overwriting
them with the unpatched CRAN build.
