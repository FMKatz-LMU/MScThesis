# ============================================================================
# run_all_hypotheses_v2.R — Driver for the v2 clean pipeline (with test gate)
# ----------------------------------------------------------------------------
# Source order mirrors the rebuilt numbered chain. NOTE the numbering:
#   - issue-ownership (old H3a/H3b) is CUT;
#   - old H4 (system polarization) -> H3 (06_H3.R);
#   - there is NO 07;
#   - 10 = intervention-effect decomposition (filter x code305), NOT the old
#     tight-length-filter proxy (removed; superseded by the real procedural filter);
#   - 11 = per-code DPS decomposition + 305-exclusion admissibility.
#
# PRE-FLIGHT TEST GATE
#   The seven logic-test harnesses (test_04..test_11_logic.R) are self-contained
#   base-R/data.table checks of the core algorithms (synthetic data: they verify
#   the algorithm logic, they do NOT read the caches or the scripts' real output).
#   They run FIRST, as a gate. Each runs in its OWN Rscript subprocess, because a
#   failing harness calls quit(status=1L) -- sourcing it in-process would kill this
#   whole session. We only read the subprocess exit code; the pipeline proceeds
#   only if every harness passes. A missing harness is skipped (not a failure); a
#   harness that runs and fails aborts the pipeline (STOP_ON_TEST_FAIL).
#
# Runtime (RTX 5070 laptop): tests ~seconds; then TWO parquet passes
#   (03 stream + 11 per-code stream), ~12-15 min total.
#
# PREREQUISITE: set PROJECT_ROOT in 00_config.R (line 17) to this machine's path,
#   e.g. PROJECT_ROOT <- "C:/RProj_MSc/MScThesis"  -- or, robustly,
#        PROJECT_ROOT <- here::here()  (works as long as Data/ and results/ sit
#   inside the .Rproj folder). All data + output paths derive from PROJECT_ROOT.
# ============================================================================

needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel",
            "arrow", "viridisLite", "data.table")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(here)

# ---- toggles ---------------------------------------------------------------
RUN_TESTS         <- TRUE    # pre-flight logic-test gate
RUN_ROBUSTNESS    <- TRUE    # 08-11 robustness / data-quality blocks
STOP_ON_TEST_FAIL <- TRUE    # abort the pipeline if any logic test actually fails
TEST_DIR          <- "tests" # folder (relative to project root) holding test_*_logic.R

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
# Main pipeline
# ============================================================================
#source(here::here("03_load_and_aggregate.R"))   # caches: speech_soft, speech_method, manifesto_dists, bootstrap_ci, cell_diag
source(here::here("04_H1.R"))                    # H1a (bootstrap band), H1b (exploratory), between-party benchmarks
source(here::here("05_H2.R"))                    # H2a (per-bucket, excl), H2b/H2c (directional, exploratory)
source(here::here("06_H3.R"))                    # H3 system polarization over time (descriptive; formerly H4)

# ============================================================================
# Robustness / data-quality blocks
# ============================================================================
if (RUN_ROBUSTNESS) {
  source(here::here("08_robustness_temperature.R"))   # tau in {0.5,1.0,2.0} sweep (Spearman rho >= 0.9)
  source(here::here("09_robustness_threshold.R"))     # soft vs hard argmax+threshold {0.4,0.5} (Spearman rho >= 0.85)
  source(here::here("10_robustness_prefilter.R"))     # intervention-effect decomposition: filter x code305 (hosts H2a before/after)
  source(here::here("11_dps_code_decomposition.R"))   # per-code DPS decomposition + 305-exclusion admissibility (own parquet pass)
}

message("\nAll done. Outputs are in: ", file.path("results", "empirics"))
