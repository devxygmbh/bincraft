# bincraft

{bincraft} provides the ability to build R package binaries for Linux.
It is the core engine of the [rpkgs.com](https://rpkgs.com) project.

## Highlights ✨️

- Auto-resolve package and system dependencies during installation
- Archive multiple versions of a package into a CRAN-like archive
  structure
- Support storing packages in S3 instead of on disk
- Process CRAN repository updates
- Support building binaries for Ubuntu, RHEL and Alpine Linux
- Allows building packages for specific R minor versions
  (`is_r_minor_sensitive = TRUE`)
- ABI-risk classifier ([design
  doc](https://bincraft.doc.rpkgs.com/tools/abi-classifier.md)) tells
  the build pipeline whether a package needs per-R-minor recompiles,
  building-once-then-replicating, or no compile at all
- Creation of custom/personal repositories

Besides building binaries, the package optionally allows to store the
build metadata (duration, version, name, errors, etc.) in a (Postgres)
database.

> \[!NOTE\] The package functions must be run within the desired
> distribution for which binaries should be built for. See
> [rpkgs/build-cran-binaries/docker](https://codefloe.com/rpkgs/build-cran-binaries/src/branch/main/docker)
> for containerfiles which have been built for this purpose.

See the [pkgdown documentation](https://bincraft.doc.rpkgs.com) for more
details.
