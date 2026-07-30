#!/bin/sh
# Reprex: uvr does not warn about missing system dependencies on Alpine
# xml2 requires libxml2-dev but uvr never warns — the install just fails.
# Image: devxygmbh/r-alpine:4-3.23 (Alpine 3.23, R 4.5)

docker run --rm devxygmbh/r-alpine:4-3.23 sh -c '
set -ex
curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | UVR_INSTALL_DIR=/usr/local/bin sh
mkdir /tmp/testpkg && cd /tmp/testpkg
cat > DESCRIPTION <<EOF
Package: testpkg
Title: Test
Version: 0.0.1
Imports: xml2
EOF
uvr init
uvr lock
uvr sync
'
