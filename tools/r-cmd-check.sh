#!/usr/bin/env bash
set -euo pipefail

# Pinned to the exec-env's R 4.5.3: R 4.6.0 there ships an rlang built against a
# different minor (SET_GROWABLE_BIT relocation error on load). That binary only
# exists inside the exec-env, so when it's absent (local machines, CI runners
# without it) skip the check — .crow/rcmdcheck.yaml runs the authoritative
# R CMD check on every push to the default branch.
RSCRIPT=/opt/R/4.5.3/bin/Rscript

if [ ! -x "$RSCRIPT" ]; then
  echo "r-cmd-check: $RSCRIPT not found; skipping local check (CI runs R CMD check)." >&2
  exit 0
fi

exec "$RSCRIPT" -e "devtools::check(error_on = 'warning')"
