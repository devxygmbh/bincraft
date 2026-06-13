#' @param r_minor_detection How to decide which packages are R-minor-sensitive.
#'   `"none"` (default) builds everything into the generic slot. `"issue"` uses
#'   the curated tracking issue (the legacy `filter_r_minor_sensitive` path).
#'   `"classifier"` classifies each candidate via [needs_per_minor_recompile()]
#'   and routes only `risky` packages to the per-minor slot.
