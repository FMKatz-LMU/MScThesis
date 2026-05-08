# ============================================================================
# run_all.R — Driver script
# ----------------------------------------------------------------------------
# Expected runtime on RTX 5070 laptop: ~10 min
#   - 03_load_and_aggregate.R streams 598 parquet chunks (~4–5 min)
#   - 04..07 read from cache, mostly seconds
#   - 08..10 robustness blocks read from cache, ~10–30 s each
# ============================================================================

needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel",
            "arrow", "viridisLite", "data.table")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(here)

RUN_PROBE      <- FALSE
RUN_ROBUSTNESS <- TRUE

if (RUN_PROBE) source(here::here("R/01_probe_data.R"))

# ---- Main pipeline ---------------------------------------------------------
source(here::here("R/03_load_and_aggregate.R"))   # cache: speech_dists, manifesto_dists
source(here::here("R/04_H1.R"))                   # H1a, H1b
source(here::here("R/05_H2.R"))                   # H2a, H2b, H2c (Dalton-weighted)
source(here::here("R/06_H3.R"))                   # H3a, H3b (ownership heatmaps)
source(here::here("R/07_H4.R"))                   # H4 (system polarization, was H6)

# ---- Robustness blocks -----------------------------------------------------
if (RUN_ROBUSTNESS) {
  source(here::here("R/08_robustness_temperature.R"))   # τ ∈ {0.5, 1.0, 2.0}
  source(here::here("R/09_robustness_threshold.R"))     # argmax + threshold {0.4, 0.5}
  source(here::here("R/10_robustness_prefilter.R"))     # tightened pre-filter for H2a
}

message("\nAll done. Outputs are in: ", file.path("results", "empirics"))
