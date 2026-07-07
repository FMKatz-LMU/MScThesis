# ============================================================================
# run_all_v2_3-11.R — ANALYSIS STAGE driver (depends on 03's caches: 04 → 11)
# ----------------------------------------------------------------------------
# Pair file: run_all_v2_0-3.R is the BUILD stage (runs 03, writes the caches).
# Run THIS one to iterate on the hypotheses + robustness without paying for the
# expensive 03 parquet pass each time. It reads the caches 0-3 wrote:
#   speech_soft / speech_method / manifesto_dists / bootstrap_ci / cell_diag.
#
# Numbering (unchanged):
#   - issue-ownership (old H3a/H3b) is CUT; old H4 (system polarization) -> H3 (06_H3.R);
#   - there is NO 07;
#   - 10 = intervention-effect decomposition (filter × code305): now a 3×2 grid
#     incl. filtered_000, and it HOSTS the 000-vs-305 redundancy / double-correction check;
#   - 11 = per-code DPS decomposition + 305 asymmetry, run on the filtered_000 primary
#     (its own parquet pass over the _000 corpus).
#
# PRE-FLIGHT TEST GATE — the logic-test harnesses (test_04..test_11_logic.R) are
#   self-contained base-R/data.table checks of the core algorithms on synthetic
#   data (they do NOT read the caches). They run FIRST, each in its own Rscript
#   subprocess (a failing harness calls quit(status=1L); sourcing in-process would
#   kill this session). The pipeline proceeds only if every harness passes.
#   NOTE (000-filter): if test_10_logic.R hard-codes the 2×2 intervention grid,
#   bump it to 3×2 (it now also carries filtered_000).
#
# REBUILD_03 toggle: FALSE (default) = read existing caches (fast). TRUE = run 03
#   first (self-contained full run). If the caches are missing and REBUILD_03 is
#   FALSE, this script stops with a clear hint to run run_all_v2_0-3.R.
#
# PREREQUISITE: PROJECT_ROOT set in 00_config.R (line 17) to this machine's path.
# ============================================================================

needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel",
            "arrow", "viridisLite", "data.table")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(here)

# ---- toggles ---------------------------------------------------------------
RUN_TESTS         <- TRUE     # pre-flight logic-test gate
RUN_ROBUSTNESS    <- TRUE     # 08-11 robustness / data-quality blocks
STOP_ON_TEST_FAIL <- TRUE     # abort the pipeline if any logic test actually fails
REBUILD_03        <- FALSE    # TRUE = (re)build caches via 03 before the analysis
TEST_DIR          <- "tests"  # folder (relative to project root) holding test_*_logic.R

# ============================================================================
# Pre-flight: logic-test gate (each harness in its own isolated subprocess)
# ============================================================================
if (RUN_TESTS) {
  test_files <- c("test_04_logic.R", "test_05_logic.R", "test_06_logic.R",
                  "test_08_logic.R", "test_09_logic.R", "test_10_logic.R",
                  "test_11_logic.R")

  rscript <- Sys.which("Rscript")
  if (!nzchar(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
  qpath <- function(p) shQuote(p, type = if (.Platform$OS.type == "windows") "cmd" else "sh")

  run_one <- function(tf) {
    p <- here::here(TEST_DIR, tf)
    if (!file.exists(p)) { message(sprintf("  [skip] %-22s (not found in %s/)", tf, TEST_DIR)); return(NA_integer_) }
    out <- tryCatch(suppressWarnings(system2(rscript, qpath(p), stdout = TRUE, stderr = TRUE)),
                    error = function(e) { message("  [skip] ", tf, " (could not launch Rscript: ",
                                                  conditionMessage(e), ")"); NULL })
    if (is.null(out)) return(NA_integer_)
    st  <- attr(out, "status"); if (is.null(st)) st <- 0L
    res <- grep("RESULT:", out, value = TRUE)
    message(sprintf("  [%s] %-22s %s", if (st == 0L) "pass" else "FAIL", tf,
                    if (length(res)) trimws(gsub("=", "", res[1])) else ""))
    as.integer(st)
  }

  message("== Pre-flight: ", length(test_files), " logic-test harnesses ==")
  statuses <- vapply(test_files, run_one, integer(1))
  n_failed  <- sum(!is.na(statuses) & statuses != 0L)
  n_skipped <- sum(is.na(statuses))
  n_passed  <- sum(!is.na(statuses) & statuses == 0L)
  message(sprintf("== %d passed, %d failed, %d skipped ==", n_passed, n_failed, n_skipped))
  if (n_skipped > 0)
    message(sprintf("   (missing harnesses expected in %s/ relative to the project root)", TEST_DIR))
  if (n_failed > 0L) {
    msg <- sprintf("[run_all] %d logic test(s) FAILED -- the core algorithms are broken; fix before running the pipeline.", n_failed)
    if (STOP_ON_TEST_FAIL) stop(msg, call. = FALSE) else warning(msg)
  }
  cat("\n")
}

# ============================================================================
# Cache gate: build via 03 if asked, else verify the 0-3 caches are present
# ============================================================================
if (REBUILD_03) {
  message("REBUILD_03 = TRUE -> running 03 to (re)build caches ...")
  source(here::here("03_load_and_aggregate.R"))
} else {
  .cache_dir <- here::here("results", "empirics", "cache")
  .need <- c("speech_soft.rds", "speech_method.rds", "manifesto_dists.rds",
             "bootstrap_ci.rds", "cell_diag.rds")
  .miss <- .need[!file.exists(file.path(.cache_dir, .need))]
  if (length(.miss) > 0)
    stop("Fehlende Caches (", paste(.miss, collapse = ", "), ") in ", .cache_dir,
         ".\n  -> Zuerst run_all_v2_0-3.R laufen lassen, oder REBUILD_03 <- TRUE setzen.",
         call. = FALSE)
  message("Caches gefunden in ", .cache_dir, " -> Analyse startet.")
}

# ============================================================================
# Main pipeline (reads caches; 06 = H3, formerly H4; there is NO 07)
# ============================================================================
source(here::here("04_H1.R"))   # H1a (bootstrap primary band = filtered_000 × excl), H1b, between-party benchmarks
source(here::here("05_H2.R"))   # H2a (per-bucket, excl on filtered_000), H2b/H2c (directional, exploratory)
source(here::here("06_H3.R"))   # H3 system polarization over time (descriptive; formerly H4)

# ============================================================================
# Robustness / data-quality blocks
# ============================================================================
if (RUN_ROBUSTNESS) {
  source(here::here("08_robustness_temperature.R"))   # τ ∈ {0.5,1.0,2.0} sweep (Spearman ρ ≥ 0.9)
  source(here::here("09_robustness_threshold.R"))     # soft vs hard argmax+threshold {0.4,0.5} on filtered_000 (ρ ≥ 0.85)
  source(here::here("10_robustness_prefilter.R"))     # 3×2 filter × code305 grid + 000-vs-305 redundancy / double-correction check
  source(here::here("11_dps_code_decomposition.R"))   # per-code DPS decomposition + 305 asymmetry on filtered_000 (own parquet pass)
}

message("\nrun_all_v2_3-11: fertig. Outputs in: ", file.path("results", "empirics"))
