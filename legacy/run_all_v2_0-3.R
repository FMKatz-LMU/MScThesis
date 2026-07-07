# ============================================================================
# run_all_v2_0-3.R — BUILD STAGE driver (00 → 03)
# ----------------------------------------------------------------------------
# Pair file: run_all_v2_3-11.R runs the ANALYSIS stage (04 → 11) on the caches
# this script writes. Use this one whenever the CORPUS changes (e.g. after the
# 000-filter re-run): it (1) runs the 03 logic-test gate, then (2) streams the
# 000-augmented v2 parquet and writes the caches that everything downstream reads.
#
# Lives in the PROJECT ROOT (MScThesis/). All scripts + test files live there too.
#   PART 1  TESTS    — correctness gate that runs BEFORE any real computation.
#   PART 2  BUILD     — 03 only (writes speech_soft / speech_method /
#                       manifesto_dists / bootstrap_ci / cell_diag).
#
# PREREQUISITES (one-time):
#   * 00_config.R: the v2 axis constants incl. the 000-filter block
#     (FILTERS now has filtered_000; FILTER000_THRESHOLD; COL_P000/COL_IS000).
#   * 03 reads the 000-AUGMENTED corpus: it derives PARQUET_DIR_000 =
#     paste0(PATHS$parquet_dir, "_000") and HARD-REQUIRES p_000/is_000 columns,
#     so 20_apply_000_text.py must have run first.
#   * In EVERY script the internal source() calls point at the ROOT
#     (here::here("...") WITHOUT an "R/" prefix). 03/04–11 already ship this way.
#
# Run from the project (open the .Rproj, then source this file) so here::here()
# resolves to the project root.
# ============================================================================

# ---- packages --------------------------------------------------------------
needed <- c("tidyverse", "here", "fs", "glue", "scales", "ggrepel",
            "arrow", "viridisLite", "data.table")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
library(here)

# ---- locations -------------------------------------------------------------
PIPE_DIR <- here::here()                 # project root = where the scripts live
TEST_DIR <- here::here("tests")          # test_*.R live in tests/ (change if you move them)

# ---- switches --------------------------------------------------------------
RUN_TESTS         <- TRUE         # PART 1 gate
RUN_REALFILE_TEST <- FALSE        # OFF until test_03_realfile.R builds a _000 synthetic corpus
                                  # (with p_000/is_000, or sets .PARQUET_DIR_000_OVERRIDE); else it only warns
RUN_PROBE         <- FALSE        # optional 01_probe_data.R

src <- function(file) source(file.path(PIPE_DIR, file))

# ============================================================================
# PART 1 — TESTS  (correctness gate)
# ============================================================================
if (RUN_TESTS) {
  message("\n############### PART 1: TESTS (gate) ###############")

  # (a) Self-contained LOGIC tests — HARD gate. Each runs in its own env; any
  #     failure stops the driver before the real run.
  #     NOTE (000-filter): test_03_bootstrap.R historically checked 4 CI bands;
  #     03 now emits 6 (3 filters × 2 code305). If that harness asserts a band
  #     count, bump its expectation 4 -> 6. Likewise any test_03 that hard-codes
  #     the 12-spec soft cube should now expect 18 (3 × 3 × 2).
  logic_tests <- c("test_03_math.R",          # code305 identity, aggregation, jsd, bootstrap eq.
                   "test_03_dt.R",            # data.table idioms (finalize, t(BS), merge gate)
                   "test_03_stream.R",        # multi-chunk accumulation vs independent colMeans
                   "test_03_manifesto_hard.R",# manifesto block + hard-threshold routing
                   "test_03_bootstrap.R",     # bootstrap assembly + 6 CI bands (was 4 pre-000-filter)
                   "test_03_boot_lpmatch.R")  # bootstrap matches each cell to its OWN-LP manifesto
  for (t in logic_tests) {
    message("\n========== TEST: ", t, " ==========")
    ok <- tryCatch({ source(file.path(TEST_DIR, t), local = new.env()); TRUE },
                   error = function(e) { message("  >>> FAILED: ", conditionMessage(e)); FALSE })
    if (!isTRUE(ok))
      stop("Gate-Test fehlgeschlagen: ", t, " — Pipeline NICHT gestartet.")
  }
  message("\n>>> Alle Logik-Tests bestanden.")

  # (b) END-TO-END test of the real 03 on synthetic parquet. NOT pre-verified in
  #     Claude's sandbox (no R-arrow there) -> WARN-don't-halt.
  #     NOTE (000-filter): the synthetic corpus this harness builds must now carry
  #     p_000/is_000 columns and live under a "_000" sibling dir, OR the harness
  #     must set .PARQUET_DIR_000_OVERRIDE to point 03 at it. Until updated, this
  #     test is EXPECTED to warn here — it does not block the build.
  if (RUN_REALFILE_TEST) {
    message("\n========== END-TO-END: test_03_realfile.R (real 03 + synthetic parquet) ==========")
    tryCatch(
      source(file.path(TEST_DIR, "test_03_realfile.R")),   # local = FALSE on purpose
      error = function(e)
        message("  >>> WARNUNG: Real-File-Test fehlgeschlagen: ", conditionMessage(e),
                "\n      (Build läuft trotzdem weiter; bei echtem Lauf prüft 03 die 000-Spalten selbst.)")
    )
  }

  # Defensive: drop any leftover override globals so PART 2 uses the REAL paths.
  # NB: ls() hides dot-prefixed names by default -> all.names = TRUE is REQUIRED,
  # else these overrides leak into PART 2 and 03 reads the test's temp _000 dir.
  .ovr <- intersect(
    c(".PARQUET_DIR_OVERRIDE", ".PARQUET_DIR_000_OVERRIDE", ".MANIFESTO_RDS_OVERRIDE",
      ".CACHE_DIR_OVERRIDE", ".N_BOOT_OVERRIDE_EXT"),
    ls(all.names = TRUE, envir = globalenv()))
  if (length(.ovr)) rm(list = .ovr, envir = globalenv())
}

# ============================================================================
# PART 2 — BUILD  (03 only; writes the caches the 3-11 driver consumes)
# ============================================================================
message("\n############### PART 2: BUILD-LAUF (03) ###############")

if (RUN_PROBE) src("01_probe_data.R")

# 03 streams the 000-augmented v2 parquet ONCE and writes:
#   speech_soft (18-spec cube incl. filtered_000), speech_method (on filtered_000),
#   manifesto_dists, bootstrap_ci (6 bands; primary = filtered_000 × excl), cell_diag.
# It sources 00_config + 02_helpers itself. For a fast smoke run set
# N_BOOT_OVERRIDE <- 50L inside 03. Re-thresholding the 000-filter later only needs
# FILTER000_THRESHOLD in 00_config + a re-run of THIS script (no GPU re-run of 20).
src("03_load_and_aggregate.R")

message("\nrun_all_v2_0-3: BUILD fertig. Caches in: ", file.path("results", "empirics", "cache"),
        "\n  -> jetzt run_all_v2_3-11.R für die Analyse (04-11).")
