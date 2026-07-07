# ============================================================================
# run_all.R — Driver script (v2 clean pipeline)
# ----------------------------------------------------------------------------
# Source order mirrors the rebuilt numbered chain. NOTE the new numbering:
#   - the issue-ownership hypothesis (old H3a/H3b) is CUT;
#   - old H4 (system polarization) is renamed H3 (script 06_H3.R);
#   - there is NO 07;
#   - script 10 is the intervention-effect decomposition (filter × 305),
#     NOT the old tight-length-filter proxy (removed; superseded by the real
#     procedural filter upstream);
#   - script 11 is the per-code DPS decomposition + 305-exclusion admissibility.
#
# Runtime (RTX 5070 laptop): TWO parquet passes now —
#   03_load_and_aggregate.R streams the v2 chunks once (~4–5 min), and
#   11_dps_code_decomposition.R streams them again for per-code 56-dim cell
#   means (~4–5 min). Everything else reads from cache (seconds each).
# Expect ~12–15 min end to end.
# ============================================================================

needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel",
            "arrow", "viridisLite", "data.table")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(here)

RUN_ROBUSTNESS <- TRUE   # set FALSE to run only the headline hypotheses (04–06)

# Each script sources 00_config.R + 02_helpers.R itself, so they need not be
# pre-sourced here. Paths use here::here() WITHOUT an "R/" prefix, matching the
# rebuilt scripts (00/02/03 and 04–11 all use the same convention).

# ---- Main pipeline ---------------------------------------------------------
source(here::here("03_load_and_aggregate.R"))   # caches: speech_soft, speech_method, manifesto_dists, bootstrap_ci, cell_diag
source(here::here("04_H1.R"))                    # H1a (bootstrap band), H1b (exploratory), between-party benchmarks
source(here::here("05_H2.R"))                    # H2a (per-bucket, excl), H2b/H2c (directional, exploratory)
source(here::here("06_H3.R"))                    # H3 system polarization over time (descriptive; formerly H4)

# ---- Robustness / data-quality blocks --------------------------------------
if (RUN_ROBUSTNESS) {
  source(here::here("08_robustness_temperature.R"))   # τ ∈ {0.5, 1.0, 2.0} sweep (Spearman ρ ≥ 0.9)
  source(here::here("09_robustness_threshold.R"))     # soft vs hard argmax+threshold {0.4, 0.5} (Spearman ρ ≥ 0.85)
  source(here::here("10_robustness_prefilter.R"))     # intervention-effect decomposition: filter × code305 (hosts H2a before/after)
  source(here::here("11_dps_code_decomposition.R"))   # per-code DPS decomposition + 305-exclusion admissibility (own parquet pass)
}

message("\nAll done. Outputs are in: ", file.path("results", "empirics"))

source(here::here("13_goldstandard_realign_FINAL.R"))
source(here::here("14_silver_validation.R"))
source(here::here("15_draw_control_sample.R"))
source(here::here("17_control_validation.R"))

