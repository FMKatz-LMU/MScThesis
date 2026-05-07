# ============================================================================
# 01_probe_data.R — Inspect speech and manifesto RDS files
# ----------------------------------------------------------------------------
# Run this FIRST. It does no analysis — it just prints structure so we can
# verify the join keys and column conventions before running the pipeline.
# Outputs a short report to results/empirics/data_probe.txt.
# ============================================================================

source(here::here("R/00_config.R"))

sink_path <- file.path(PATHS$out_dir, "data_probe.txt")
sink(sink_path, split = TRUE)

cat("================================================================\n")
cat("DATA PROBE — ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat("================================================================\n\n")

# ---- 1. SPEECH RDS ---------------------------------------------------------
cat("### SPEECH RDS\n")
cat("Path: ", PATHS$speech_rds, "\n", sep = "")
if (!file.exists(PATHS$speech_rds)) {
  stop("Speech RDS not found at the configured path. Edit PATHS in 00_config.R.")
}
sp <- readRDS(PATHS$speech_rds)

cat("Class:          ", paste(class(sp), collapse = ", "), "\n")
cat("Dimensions:     ", nrow(sp), " rows  x  ", ncol(sp), " columns\n", sep = "")
cat("Object size:    ", format(object.size(sp), units = "auto"), "\n", sep = "")

cat("\nColumn names (first 30):\n")
print(head(colnames(sp), 30))
if (ncol(sp) > 30) cat("... (", ncol(sp) - 30, " more columns)\n", sep = "")

cat("\nProbability columns present?\n")
prob_present <- intersect(PROB_COLS, colnames(sp))
cat("  Found ", length(prob_present), "/56 of expected p_<code> columns.\n", sep = "")
if (length(prob_present) != 56L) {
  cat("  MISSING: ", paste(setdiff(PROB_COLS, prob_present), collapse = ", "), "\n", sep = "")
  cat("  EXTRAS (p_*): ",
      paste(setdiff(grep("^p_\\d+$", colnames(sp), value = TRUE), PROB_COLS), collapse = ", "),
      "\n", sep = "")
}

# Look for likely metadata columns
meta_candidates <- c("speech_id", "sentence_id", "sentence_nr", "speaker", "party",
                     "date", "lp", "legislative_period", "legislaturperiode", "debate_topic")
meta_found <- intersect(meta_candidates, colnames(sp))
cat("\nMetadata columns found: ", paste(meta_found, collapse = ", "), "\n", sep = "")

# Party-column values
party_col <- intersect(c("party", "partei", "Partei", "Party"), colnames(sp))[1]
if (!is.na(party_col)) {
  cat("\nParty values (column '", party_col, "'):\n", sep = "")
  print(sp %>% count(.data[[party_col]], sort = TRUE))
}

# LP-column values
lp_col <- intersect(c("lp", "legislative_period", "legislaturperiode", "LP"), colnames(sp))[1]
if (!is.na(lp_col)) {
  cat("\nLegislative period values (column '", lp_col, "'):\n", sep = "")
  print(sp %>% count(.data[[lp_col]]))
}

# Quick sanity check: do probability rows sum to ~1?
if (length(prob_present) >= 50) {
  rs <- rowSums(sp[sample(nrow(sp), min(1000, nrow(sp))), prob_present, drop = FALSE])
  cat("\nProbability rowSum check on 1000-row sample:\n")
  cat(sprintf("  min = %.4f, mean = %.4f, max = %.4f\n", min(rs), mean(rs), max(rs)))
  cat("  (Expect mean ~ 1.000; deviations indicate logits not softmax.)\n")
}

# ---- 2. MANIFESTO RDS ------------------------------------------------------
cat("\n\n### MANIFESTO RDS\n")
cat("Path: ", PATHS$manifesto_rds, "\n", sep = "")
if (!file.exists(PATHS$manifesto_rds)) {
  stop("Manifesto RDS not found at the configured path. Edit PATHS in 00_config.R.")
}
mf <- readRDS(PATHS$manifesto_rds)

cat("Class:          ", paste(class(mf), collapse = ", "), "\n")
cat("Dimensions:     ", nrow(mf), " rows  x  ", ncol(mf), " columns\n", sep = "")

cat("\nAll column names:\n")
print(colnames(mf))

cat("\nFirst 5 rows:\n")
print(head(mf, 5))

# Heuristics for what kind of manifesto file this is
cat("\nFile-type heuristics:\n")

per_cols_found <- intersect(MARPOR_PER_COLS, colnames(mf))
cat("  - 'per<code>' columns found: ", length(per_cols_found), "/56\n", sep = "")

bucket_cols_A <- intersect(BUCKETS_A, colnames(mf))
bucket_cols_B <- intersect(BUCKETS_B, colnames(mf))
cat("  - Aggregation A bucket columns found (as colnames): ", length(bucket_cols_A), "\n", sep = "")
cat("  - Aggregation B bucket columns found (as colnames): ", length(bucket_cols_B), "\n", sep = "")

if ("bucket_A" %in% colnames(mf) || "bucket" %in% colnames(mf)) {
  cat("  - Long-format bucket column present (already aggregated long).\n")
}
if ("cmp_code" %in% colnames(mf) || "code" %in% colnames(mf)) {
  cat("  - Code column present (long-format raw codes).\n")
}

# Diagnose
fmt <- if (length(per_cols_found) >= 40)            "RAW_WIDE"
       else if ("bucket_A" %in% colnames(mf) ||
                "bucket"   %in% colnames(mf))       "LONG_AGGREGATED"
       else if (length(bucket_cols_A) >= 5 ||
                length(bucket_cols_B) >= 5)         "WIDE_AGGREGATED"
       else if ("cmp_code" %in% colnames(mf))       "QUASI_SENTENCE_LEVEL"
       else                                         "UNKNOWN"

cat("\n>>> DIAGNOSIS: format appears to be '", fmt, "'\n", sep = "")
cat("    RAW_WIDE         = MARPOR Main Dataset (party x election rows; per101..per706 cols)\n")
cat("    WIDE_AGGREGATED  = pre-aggregated to A/B buckets, wide\n")
cat("    LONG_AGGREGATED  = pre-aggregated to A/B buckets, long\n")
cat("    QUASI_SENTENCE_LEVEL = one row per quasi-sentence with cmp_code\n")
cat("    UNKNOWN          = need to inspect manually\n")

# Party-column values on manifesto side
mf_party_col <- intersect(c("party", "partyname", "partei", "Partei"), colnames(mf))[1]
if (!is.na(mf_party_col)) {
  cat("\nParty values on manifesto side (column '", mf_party_col, "'):\n", sep = "")
  print(mf %>% count(.data[[mf_party_col]], sort = TRUE))
}

# Election date / year column
mf_date_col <- intersect(c("date", "election_date", "edate", "year"), colnames(mf))[1]
if (!is.na(mf_date_col)) {
  cat("\nElection-date values:\n")
  print(mf %>% distinct(.data[[mf_date_col]]) %>% arrange(.data[[mf_date_col]]))
}

cat("\n================================================================\n")
cat("END OF PROBE — review above, then proceed to 02_load_and_aggregate.R\n")
cat("================================================================\n")

sink()
message("\nProbe report written to: ", sink_path)
