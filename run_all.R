# ============================================================================
# run_all.R — Driver script
# ----------------------------------------------------------------------------
# Run from the project root (S:/RProj_MSc/MScThesis) or open the .Rproj file
# so that here::here() resolves to this directory.
#
# Workflow:
#   1. 03_load_and_aggregate.R   bridges existing pre-aggregated RDS files
#      into the cache format used by H1 and H2.  The first time this runs it
#      takes a few seconds.  On subsequent runs the cache is already present
#      and can be skipped by commenting out the source() call.
#   2. 04_H1.R                   H1a (JSD + bootstrap) + H1b (directional bars)
#   3. 05_H2.R                   H2a (per-bucket) + H2b (moderation) + H2c (dispersion)
#
# Bootstrap CIs for H1a:
#   By default, 03 uses the pre-computed jsd_permutation.rds as a fallback.
#   To run the full sentence-level bootstrap, open R/03_load_and_aggregate.R
#   and set BUILD_BUCKET_MATRIX <- TRUE (reads all parquet, ~5–20 min).
# ============================================================================

# ---- Check / install required packages -------------------------------------
needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel", "arrow")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

library(here)

# ---- Run pipeline ----------------------------------------------------------
source(here::here("R/03_load_and_aggregate.R"))
source(here::here("R/04_H1.R"))
source(here::here("R/05_H2.R"))

message("\nAll done. Outputs are in: ", file.path("results", "empirics"))
