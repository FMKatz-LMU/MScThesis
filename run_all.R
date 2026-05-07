# ============================================================================
# run_all.R — Driver script
# ============================================================================

needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel", "arrow")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(here)

RUN_PROBE <- FALSE
if (RUN_PROBE) source(here::here("R/01_probe_data.R"))

source(here::here("R/03_load_and_aggregate.R"))
source(here::here("R/04_H1.R"))
source(here::here("R/05_H2.R"))
source(here::here("R/06_H6.R"))

message("\nAll done. Outputs are in: ", file.path("results", "empirics"))
