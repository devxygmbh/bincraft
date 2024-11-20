# bincraftR

{bincraftR} provides a skeleton to build CRAN R package binaries in Linux.
It does the heavy lifting behind the R package binary builds of devXY.

While this package helps with all the necessary actions, it requires additional infrastructure for building and hosting, especially on scale:

- S3 storage
- Target-architecture native servers (amd64, arm64)
- Postgres DB for build metadata

As of now, the package only allows building CRAN packages, i.e. packages being mirrored on <https://github.com/cran>.
There are no current plans to extend this right now.
Also, the existence of a Postgres DB to store build metadata is a hard requirement at the moment.

## Essential functions

The package covers the complete flow of building R package binaries:

1. Clone package source from GitHub mirror
1. Checkout a tag (=release)
1. Install R package and system dependencies
1. Build binary
1. Upload binary to S3
1. Archive possible older versions (to `Archive/`)
1. Storing build metadata in a Postgres DB
