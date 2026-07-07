# ============================================================================
# 13_goldstandard_realign_FINAL.R — carry the coded gold onto the v2 corpus
# ----------------------------------------------------------------------------
# WHAT IT DOES
#   The gold sentences (consolidated FINAL gold: batch-1 non-Fehltrennung +
#   batch-2 done + the v2 top-up) carry onto the SoMaJo-resegmented v2 corpus by
#   matching the *sentence text* within speech_id. Native-v2 top-up rows carry
#   trivially; the v1-drawn rows carry wherever the text still exists verbatim.
#
#     1. Read the coded sheet(s): gs_id + sentence text + your codes
#        (agg_A, agg_B, dps_fine, flag_unsure, note).
#     2. Read the key (gs_id -> OLD speech_id). speech_id is INVARIANT under
#        re-segmentation (SoMaJo runs per speech), so we match WITHIN speech_id.
#     3. Match each gold sentence to v2 by whitespace-insensitive text within its
#        speech_id (robust against the raw-vs-SoMaJo spacing difference and
#        against the same sentence recurring in other speeches).
#     4. For matches: keep the human code, attach the FRESH v2 prediction
#        (argmax of the 56 probs -> code -> bucket_A / domain_B / bucket_B),
#        v2 sentence_nr, v2 context, and the is_procedural flag.
#     5. For non-matches (boundary changed -> sentence no longer exists verbatim):
#        list them as GAPS to be redrawn / recoded on the clean corpus.
#
#   The carried set is v2-native: human code vs the model's ACTUAL v2 prediction.
#   Caveat to disclose: carried sentences were drawn under v1 strata and coded
#   with v1 context; the text is identical, context is only an aid.
#
# PREREQUISITE: the v2 run (03) has finished and Data/sentences_classified_v2/
#   exists. Edit the PARAMETERS block, then source the whole file.
# ============================================================================

source(here::here("00_config.R"))    # PATHS, AGG_A, AGG_B, MARPOR_CODES_56, PARTIES_KEEP
source(here::here("02_helpers.R"))   # normalize_party()

suppressPackageStartupMessages({
  library(arrow); library(data.table); library(stringr)
})
if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("Package 'openxlsx' is required (install.packages('openxlsx')).")

# ---- PARAMETERS (edit these) ----------------------------------------------
GS_DIR <- file.path(PATHS$out_dir, "goldstandard")
# DEFAULT = the consolidated FINAL gold (batch1 non-Fehltrennung + batch2 done +
# topup, in ONE sheet + ONE key). The stacking machinery below still accepts a
# vector, so to reproduce the old batch layout just list both files again.
# The coding sheet must have gs_id, sentence, agg_A, agg_B, dps_fine, flag_unsure,
# note in a worksheet named CODING_SHEET; uncoded rows (empty agg_A) are dropped.
CODED_XLSX   <- file.path(GS_DIR, "goldstandard_codingsheet_FINAL_merged.xlsx")     # consolidated FINAL gold
CODING_SHEET <- "Coding"
KEY_CSV      <- file.path(GS_DIR, "goldstandard_key_slim_FINAL.csv")         # consolidated FINAL key (slim)
OUT_DIR      <- GS_DIR
fs::dir_create(OUT_DIR)

# whitespace-insensitive content key (raw old text vs SoMaJo-detok v2 text),
# hardened against two transport corruptions that silently break the match for
# ~70% of German sentences (everything with an umlaut):
#   (1) XML numeric char refs: some xlsx writers (openpyxl) store ä as "&#228;";
#       R/openxlsx reads that LITERALLY -> decode it back to the character first.
#   (2) NFD: Excel / OS round-trips can decompose ä -> a + combining diaeresis;
#       normalise both sides to NFC.
.decode_ncr <- function(x) {
  x <- as.character(x)
  if (!any(grepl("&#", x, fixed = TRUE))) return(x)
  one <- function(s) {
    m <- gregexpr("&#x?[0-9A-Fa-f]+;", s, perl = TRUE)[[1]]
    if (m[1] == -1L) return(s)
    len <- attr(m, "match.length")
    for (i in rev(seq_along(m))) {
      body <- sub(";$", "", sub("^&#", "", substr(s, m[i], m[i] + len[i] - 1L)))
      hex  <- grepl("^x", body, ignore.case = TRUE)
      code <- suppressWarnings(strtoi(sub("^x", "", body, ignore.case = TRUE),
                                      base = if (hex) 16L else 10L))
      if (!is.na(code))
        s <- paste0(substr(s, 1, m[i] - 1L), intToUtf8(code),
                    substr(s, m[i] + len[i], nchar(s)))
    }
    s
  }
  vapply(x, one, character(1), USE.NAMES = FALSE)
}
.nfc <- if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_nfc else
        if (requireNamespace("utf8",    quietly = TRUE)) function(x) utf8::utf8_normalize(x, map_case = FALSE) else
        function(x) x   # NFC unavailable: NCR-decode still applies; install.packages('stringi') for full robustness
nrm <- function(x) gsub("[[:space:]]+", "", .nfc(.decode_ncr(as.character(x))), perl = TRUE)

# ============================================================================
# 1. Load coded sentences + key, join on gs_id
# ============================================================================
read_coded <- function(path) {
  d <- as.data.table(openxlsx::read.xlsx(path, sheet = CODING_SHEET))
  need <- c("gs_id", "sentence", "agg_A", "agg_B", "dps_fine", "flag_unsure", "note")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop(sprintf("%s: missing columns %s", basename(path),
                                 paste(miss, collapse = ", ")))
  d[, .(gs_id, sentence, agg_A, agg_B, dps_fine, flag_unsure, note,
        src = basename(path))]
}
coded <- rbindlist(lapply(CODED_XLSX, read_coded), use.names = TRUE)
coded <- coded[!is.na(sentence) & nzchar(trimws(sentence))]
# keep only rows you actually coded (agg_A filled); comment out to carry blanks too
coded <- coded[!is.na(agg_A) & nzchar(trimws(agg_A))]
message(sprintf("[13] coded sentences to carry: %d (from %d sheet(s))",
                nrow(coded), length(CODED_XLSX)))

key <- rbindlist(lapply(KEY_CSV, fread), use.names = TRUE, fill = TRUE)
key[, gs_id := as.character(gs_id)]
key[, speech_id := as.character(speech_id)]
dup_key <- unique(key$gs_id[duplicated(key$gs_id)])           # same gs_id in >1 key file
if (length(dup_key))
  warning(sprintf("[13] %d gs_id appear in MULTIPLE key files (kept first — possible batch id collision): %s",
                  length(dup_key), paste(head(dup_key, 5), collapse = ", ")))
key <- unique(key, by = "gs_id")
coded[, gs_id := as.character(gs_id)]
dup_coded <- unique(coded$gs_id[duplicated(coded$gs_id)])     # same gs_id coded in >1 sheet -> double-match
if (length(dup_coded))
  warning(sprintf("[13] %d gs_id coded in MULTIPLE sheets (would double-match): %s",
                  length(dup_coded), paste(head(dup_coded, 5), collapse = ", ")))

b1 <- merge(coded, key[, .(gs_id, speech_id,
                           old_lp = lp, old_party = party, old_date = date,
                           old_stratum = if ("stratum" %in% names(key)) stratum else NA_character_)],
            by = "gs_id", all.x = TRUE)
if (anyNA(b1$speech_id))
  warning(sprintf("[13] %d coded gs_id had no speech_id in the key (cannot match).",
                  sum(is.na(b1$speech_id))))
b1[, nrm_sentence := nrm(sentence)]
need_ids <- unique(stats::na.omit(b1$speech_id))
message(sprintf("[13] distinct speeches to scan in v2: %d", length(need_ids)))

# ============================================================================
# 2. Pull the matching speeches from the v2 parquet (filter pushdown)
# ============================================================================
chunk_files <- list.files(PATHS$parquet_dir, pattern = "\\.parquet$", full.names = TRUE)
if (!length(chunk_files)) stop("No v2 parquet chunks at ", PATHS$parquet_dir)

# discover & order the 56 prob columns (named "NNN - Title")
.s    <- arrow::read_parquet(chunk_files[1], as_data_frame = FALSE)$schema
.nms  <- .s$names
prob_cols <- grep("^[0-9]{3} - ", .nms, value = TRUE)
stopifnot(length(prob_cols) == 56L)
prob_codes <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
prob_cols  <- prob_cols[match(MARPOR_CODES_56, prob_codes)]   # -> MARPOR order

meta_cols <- intersect(
  c("speech_id", "sentence_nr", "sentence", "context_before", "context_after",
    "is_procedural", "party", "legislative_period", "date"), .nms)

message("[13] scanning v2 parquet for the needed speeches ...")
v2 <- arrow::open_dataset(PATHS$parquet_dir) |>
  dplyr::filter(speech_id %in% need_ids) |>
  dplyr::select(dplyr::all_of(c(meta_cols, prob_cols))) |>
  dplyr::collect() |>
  as.data.table()
v2[, speech_id := as.character(speech_id)]
message(sprintf("[13] v2 rows in those speeches: %d", nrow(v2)))

# ============================================================================
# 3. v2 model prediction per sentence (argmax of the 56 probs)
# ============================================================================
P  <- as.matrix(v2[, ..prob_cols])                # n × 56, MARPOR order
am <- max.col(P, ties.method = "first")
v2[, pred_code  := MARPOR_CODES_56[am]]
v2[, pred_label := prob_cols[am]]
v2[, pred_score := P[cbind(seq_len(.N), am)]]
a_lk  <- setNames(AGG_A$bucket_A, AGG_A$code)
bd_lk <- setNames(AGG_B$domain_B, AGG_B$code)
bb_lk <- setNames(AGG_B$bucket_B, AGG_B$code)
v2[, pred_bucket_A := a_lk[as.character(pred_code)]]
v2[, pred_domain_B := bd_lk[as.character(pred_code)]]
v2[, pred_bucket_B := bb_lk[as.character(pred_code)]]
v2[, nrm_sentence := nrm(sentence)]

# ============================================================================
# 4. Match within speech_id (whitespace-insensitive). Flag duplicate hits.
# ============================================================================
v2[, mkey := paste(speech_id, nrm_sentence, sep = "\u0001")]
b1[, mkey := paste(speech_id, nrm_sentence, sep = "\u0001")]

dup_keys <- v2[, .N, by = mkey][N > 1L, mkey]          # same text twice in a speech
v2_first <- v2[order(speech_id, sentence_nr)][, .SD[1L], by = mkey]   # take first hit

# v2 columns to carry (rename the v2 text to avoid colliding with the old text,
# which b1 keeps for the gaps list; speech_id stays from b1).
v2sub <- copy(v2_first)
setnames(v2sub, "sentence", "v2_sentence")
carry_cols <- c("sentence_nr", "v2_sentence", "context_before", "context_after",
                "is_procedural", "pred_code", "pred_label", "pred_score",
                "pred_bucket_A", "pred_domain_B", "pred_bucket_B")
v2sub <- v2sub[, c("mkey", intersect(carry_cols, names(v2sub))), with = FALSE]

m <- merge(b1, v2sub, by = "mkey", all.x = TRUE)
m[, matched   := !is.na(sentence_nr)]
m[, ambiguous := mkey %in% dup_keys]

carried <- m[matched == TRUE]
gaps    <- m[matched == FALSE]

# ============================================================================
# 5. Assemble outputs
# ============================================================================
carried_out <- carried[, .(
  gs_id, speech_id, sentence_nr,
  sentence = v2_sentence,                      # v2 text (== old up to whitespace)
  # --- your manual code (carried over) ---
  agg_A, agg_B, dps_fine, flag_unsure, note,
  # --- fresh v2 model prediction (to validate against) ---
  pred_code, pred_label, pred_score,
  pred_bucket_A, pred_domain_B, pred_bucket_B,
  is_procedural,
  context_before, context_after,
  ambiguous, old_lp, old_party, old_date, old_stratum, src
)]

gaps_out <- gaps[, .(gs_id, sentence, agg_A, agg_B, dps_fine, flag_unsure, note,
                     speech_id, old_lp, old_party, old_date, old_stratum, src)]

# carried set with full prob vector (for later soft metrics)
carried_probs <- merge(
  carried[, .(gs_id, mkey)],
  v2_first[, c("mkey", prob_cols), with = FALSE], by = "mkey", all.x = TRUE)
carried_probs[, mkey := NULL]

fwrite(carried_out, file.path(OUT_DIR, "goldstandard_v2_carried.csv"))
fwrite(gaps_out,    file.path(OUT_DIR, "goldstandard_v2_gaps.csv"))
saveRDS(carried_probs, file.path(OUT_DIR, "goldstandard_v2_carried_probs.rds"))

# also a tidy xlsx of the carried set (handy to eyeball)
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "carried"); openxlsx::writeData(wb, "carried", carried_out)
openxlsx::addWorksheet(wb, "gaps");    openxlsx::writeData(wb, "gaps",    gaps_out)
openxlsx::freezePane(wb, "carried", firstRow = TRUE); openxlsx::freezePane(wb, "gaps", firstRow = TRUE)
openxlsx::saveWorkbook(wb, file.path(OUT_DIR, "goldstandard_v2_carried.xlsx"), overwrite = TRUE)

# ============================================================================
# 6. Report
# ============================================================================
N <- nrow(m); nc <- nrow(carried); ng <- nrow(gaps)
cat("\n==================== GOLD CARRY-OVER (v2) ====================\n")
cat(sprintf("coded sentences        : %d\n", N))
cat(sprintf("carried onto v2        : %d  (%.1f%%)\n", nc, 100*nc/N))
cat(sprintf("gaps (redraw/recode)   : %d  (%.1f%%)\n", ng, 100*ng/N))
cat(sprintf("ambiguous (dup text)   : %d\n", sum(carried$ambiguous)))
if (anyNA(b1$speech_id))
  cat(sprintf("unmatchable (no speech_id in key): %d\n", sum(is.na(b1$speech_id))))

cat("\n-- carry-over by agg_A bucket --\n")
print(m[, .(coded = .N, carried = sum(matched),
            pct = round(100*mean(matched), 1)), by = agg_A][order(pct)], row.names = FALSE)

cat("\n-- carried set: procedural vs not (v2 flag) --\n")
print(carried_out[, .N, by = is_procedural], row.names = FALSE)

cat("\n-- gaps (first 15; these need redrawing/recoding) --\n")
print(gaps_out[, .(gs_id, agg_A, snippet = substr(sentence, 1, 70))][1:min(15, .N)],
      row.names = FALSE)

cat("\nFiles written:\n")
cat("  carried (csv) :", file.path(OUT_DIR, "goldstandard_v2_carried.csv"), "\n")
cat("  carried (xlsx):", file.path(OUT_DIR, "goldstandard_v2_carried.xlsx"), "\n")
cat("  gaps (csv)    :", file.path(OUT_DIR, "goldstandard_v2_gaps.csv"), "\n")
cat("  probs (rds)   :", file.path(OUT_DIR, "goldstandard_v2_carried_probs.rds"), "\n")
cat("=============================================================\n")

# ============================================================================
# 7. VALIDATION SUITE on the carried set (human code vs the model's v2 argmax)
# ----------------------------------------------------------------------------
#   Runs on whatever was carried; gaps are excluded until you recode them. Every
#   metric reports its n. Three layers + the 305 artefact check:
#     (a) hard kappa_A  — the conservative validity floor (top-1 label)
#     (b) confidence-stratified kappa_A — does agreement track model confidence?
#     (c) soft agreement — model mass on the human's bucket (full posterior)
#   Why (b)+(c): the analysis pipeline uses the full posterior, not the argmax,
#   so the hard kappa under-credits the model on barely-committed sentences.
#   (b)+(c) give the fairer fitness-for-the-soft-pipeline picture; (a) stays the
#   headline. Label scheme verified: human agg_A == BUCKETS_A, human agg_B ==
#   AGG_B$bucket_B, so no remapping. Code 000 is not in the 56, so the model
#   never predicts 'Nicht zuordenbar' (asymmetric category in kappa_A_raw).
#   CAVEAT: the gold sample is bucket-STRATIFIED (DPS over-sampled n=50, others
#   n=10 each), so every kappa here is a per-bucket reliability, NOT a corpus-
#   frequency-weighted one. Report it as such; do not read it as the agreement
#   you'd get on a random corpus draw (rare buckets are over-represented).
# ============================================================================
val <- copy(carried_out)

# 7.0 — Cohen's kappa (manual; no package dependency), over the union of levels
cohen_kappa <- function(human, ai) {
  human <- as.character(human); ai <- as.character(ai)
  ok <- !is.na(human) & !is.na(ai); human <- human[ok]; ai <- ai[ok]
  if (!length(human)) return(list(kappa = NA_real_, po = NA_real_, pe = NA_real_, n = 0L))
  lv  <- sort(unique(c(human, ai)))
  tab <- table(factor(human, lv), factor(ai, lv)); N <- sum(tab)
  po  <- sum(diag(tab)) / N
  pe  <- sum((rowSums(tab) / N) * (colSums(tab) / N))
  list(kappa = if (pe < 1) (po - pe) / (1 - pe) else NA_real_, po = po, pe = pe, n = as.integer(N))
}
# fixed, interpretable confidence bins (model top-1 prob)
conf_cut <- function(p) cut(p, breaks = c(-Inf, 0.5, 0.8, Inf),
                            labels = c("<0.5", "0.5-0.8", ">=0.8"))
# code -> bucket_A in MARPOR_CODES_56 order (for the soft mass)
bucketA_of_code <- AGG_A$bucket_A[match(MARPOR_CODES_56, AGG_A$code)]

# 7.1 — evaluable / codeable subsets
val[, is_segfault  := grepl("Feh", agg_A, ignore.case = TRUE)]   # segmentation errors (Fehl-/Fehtrennung)
val[, is_notassign := grepl("zuordenbar|000", agg_A)]            # human 'Nicht zuordenbar (000)'
val[, is_placeholder := tolower(trimws(agg_A)) %in% c("none","na")]  # uncoded placeholder rows (NOT a human judgement) -> out of kappa
eval_set <- val[is_segfault == FALSE & is_placeholder == FALSE & !is.na(agg_A) & nzchar(trimws(agg_A))]
code_set <- eval_set[is_notassign == FALSE]                      # 9 real buckets only

if (nrow(eval_set) == 0L) {
  message("[13] no evaluable carried sentences -> validation skipped.")
} else {
  message(sprintf("[13] validation: %d carried | %d evaluable (seg-faults out) | %d codeable",
                  nrow(val), nrow(eval_set), nrow(code_set)))

  # 7.2 — Aggregation A salience kappa (the headline validity floor)
  kA_raw  <- cohen_kappa(eval_set$agg_A, eval_set$pred_bucket_A)   # incl. Nicht-zuordenbar
  kA_code <- cohen_kappa(code_set$agg_A, code_set$pred_bucket_A)   # 9 buckets only
  perparty_A <- code_set[, {k <- cohen_kappa(agg_A, pred_bucket_A)
                            .(n = k$n, agree = round(k$po, 3), kappa = round(k$kappa, 3))},
                         by = old_party][order(kappa)]

  # 7.2b — per-bucket BALANCE of the codeable set: is every A-bucket >= floor?
  #        seg-faults/gaps do NOT thin buckets evenly, so check coverage explicitly.
  MIN_PER_BUCKET <- 10L
  bucket_balance <- code_set[, .(n_codeable = .N,
                                 recall = round(mean(agg_A == pred_bucket_A), 3)),  # of human-X, share the model also says X
                             by = .(bucket = agg_A)]
  bucket_balance <- merge(data.table(bucket = BUCKETS_A), bucket_balance, by = "bucket", all.x = TRUE)
  bucket_balance[is.na(n_codeable), `:=`(n_codeable = 0L, recall = NA_real_)]
  bucket_balance[, short_by := pmax(0L, MIN_PER_BUCKET - n_codeable)]
  setorder(bucket_balance, n_codeable)
  fwrite(bucket_balance, file.path(OUT_DIR, "goldstandard_v2_bucket_balance.csv"))

  # 7.3 — Aggregation B directional kappa (EXPLORATORY) — within-domain
  dirlabels  <- unique(AGG_B$bucket_B)
  eval_set[, ai_dir := fifelse(is.na(pred_bucket_B), "(residual)", as.character(pred_bucket_B))]
  within_dom <- eval_set[agg_B %in% dirlabels]                    # human saw a clear direction
  kB <- cohen_kappa(within_dom$agg_B, within_dom$ai_dir)

  # 7.4 — CONFIDENCE-STRATIFIED kappa_A  (confidence methodology #1)
  code_set[, conf_bin := conf_cut(pred_score)]
  strat <- code_set[, {k <- cohen_kappa(agg_A, pred_bucket_A)
                       .(n = k$n, agree = round(k$po, 3), kappa = round(k$kappa, 3))},
                    by = conf_bin][order(conf_bin)]

  # 7.5 — SOFT AGREEMENT: model probability mass on the human's bucket_A
  #       (confidence methodology #2 — uses the full posterior, matches pipeline)
  cp <- merge(code_set[, .(gs_id, agg_A, pred_score, conf_bin)], carried_probs, by = "gs_id")
  if (nrow(cp)) {
    pm <- as.matrix(cp[, ..prob_cols])
    cp[, soft_mass_human := vapply(seq_len(.N), function(i)
        sum(pm[i, bucketA_of_code == cp$agg_A[i]]), numeric(1))]
    soft_overall <- mean(cp$soft_mass_human)
    soft_by_bin  <- cp[, .(n = .N, mean_mass_on_human_bucket = round(mean(soft_mass_human), 3)),
                       by = conf_bin][order(conf_bin)]
  } else { soft_overall <- NA_real_; soft_by_bin <- data.table() }

  # 7.6 — model confidence distribution (top-1 prob), context for all above
  conf_dist <- eval_set[, .(n = .N, mean = round(mean(pred_score), 3),
                            median = round(median(pred_score), 3),
                            q10 = round(quantile(pred_score, .10), 3),
                            q90 = round(quantile(pred_score, .90), 3),
                            share_ge_0.5 = round(mean(pred_score >= .5), 3),
                            share_ge_0.8 = round(mean(pred_score >= .8), 3))]

  # 7.7 — 305 precision on the FILTERED corpus (decisive artefact check for 11)
  #       dps_fine is essentially unused by the human, so 'confirmed' is the
  #       bucket-level judgement: human placed the AI-305 sentence in the DPS
  #       bucket (vs 'not assignable'). pred_code is the v2 prediction.
  DPS_BUCKET <- BUCKET_OF_305_A
  ai305      <- eval_set[pred_code == 305L]
  ai305_filt <- if ("is_procedural" %in% names(ai305)) ai305[is_procedural == FALSE] else ai305
  n305 <- nrow(ai305_filt)
  conf_share <- if (n305) mean(ai305_filt$agg_A == DPS_BUCKET)            else NA_real_
  na_share   <- if (n305) mean(grepl("zuordenbar|000", ai305_filt$agg_A)) else NA_real_
  if (n305 > 0) {
    fwrite(data.table(precision_305 = conf_share, share_confirmed = conf_share,
                      share_not_assignable = na_share, n_ai305_filtered = n305,
                      n_ai305_all = nrow(ai305), bucket = DPS_BUCKET),
           file.path(OUT_DIR, "gold_305_precision_v2.csv"))     # <- read by 11 PART 3
  } else {
    message("[13] no v2-AI-305 in the filtered carried set -> gold_305_precision_v2.csv NOT written (11 stays PENDING).")
  }

  # 7.8 — write summaries
  fwrite(data.table(
    metric = c("kappa_A_raw_incl_notassignable", "kappa_A_codeable_only",
               "kappa_B_within_domain_EXPLORATORY", "soft_agreement_mass_on_human_bucket",
               "gold305_precision_filtered"),
    n     = c(kA_raw$n, kA_code$n, kB$n, nrow(cp), n305),
    value = round(c(kA_raw$kappa, kA_code$kappa, kB$kappa, soft_overall, conf_share), 3)),
    file.path(OUT_DIR, "goldstandard_v2_validation_summary.csv"))
  fwrite(perparty_A, file.path(OUT_DIR, "goldstandard_v2_kappaA_by_party.csv"))
  fwrite(strat,      file.path(OUT_DIR, "goldstandard_v2_kappaA_by_confidence.csv"))

  # 7.9 — report
  cat("\n==================== VALIDATION (carried gold, v2) ====================\n")
  cat(sprintf("carried %d | evaluable %d (seg-faults dropped) | codeable %d\n",
              nrow(val), nrow(eval_set), nrow(code_set)))
  cat("\n-- Aggregation A (salience) — headline validity floor (hard argmax) --\n")
  cat(sprintf("  kappa_A incl. 'Nicht zuordenbar' : %.3f  (agree %.3f, n=%d)\n", kA_raw$kappa, kA_raw$po, kA_raw$n))
  cat(sprintf("  kappa_A codeable buckets only    : %.3f  (agree %.3f, n=%d)\n", kA_code$kappa, kA_code$po, kA_code$n))
  cat("\n  per party (codeable, low->high kappa):\n"); print(perparty_A, row.names = FALSE)
  cat(sprintf("\n  per-bucket balance (codeable; floor = %d) -> %s:\n", MIN_PER_BUCKET,
              if (any(bucket_balance$short_by > 0)) "*** BELOW FLOOR ***" else "all buckets OK"))
  print(bucket_balance, row.names = FALSE)
  if (any(bucket_balance$short_by > 0))
    cat(sprintf("  -> short: %s\n",
                paste(sprintf("%s (n=%d, +%d)", bucket_balance[short_by > 0]$bucket,
                              bucket_balance[short_by > 0]$n_codeable,
                              bucket_balance[short_by > 0]$short_by), collapse = "; ")))
  cat("\n-- Aggregation B (directional, EXPLORATORY) — within-domain --\n")
  cat(sprintf("  kappa_B : %.3f  (agree %.3f, n=%d human-domain sentences)\n", kB$kappa, kB$po, kB$n))
  cat("\n-- CONFIDENCE-STRATIFIED kappa_A (does agreement track confidence?) --\n")
  print(strat, row.names = FALSE)
  cat("\n-- SOFT AGREEMENT (model mass on the human's bucket; full posterior) --\n")
  cat(sprintf("  overall mean mass on human's bucket : %.3f  (n=%d codeable)\n", soft_overall, nrow(cp)))
  if (nrow(soft_by_bin)) print(soft_by_bin, row.names = FALSE)
  cat("\n-- model confidence distribution (top-1 prob, evaluable) --\n")
  print(conf_dist, row.names = FALSE)
  cat("\n-- 305 precision on the FILTERED corpus (artefact check for 11) --\n")
  if (n305 > 0) {
    cat(sprintf("  v2-AI-305 (filtered) n=%d : %.0f%% confirmed (%s), %.0f%% 'not assignable'\n",
                n305, 100 * conf_share, DPS_BUCKET, 100 * na_share))
    cat(sprintf("  -> %s\n", if (isTRUE(conf_share < 0.30))
        "low precision corroborates the procedural-artefact reading; excl admissible."
        else "non-trivial precision; lean on the asymmetry bound, treat excl cautiously."))
    cat(sprintf("  wrote: %s\n", file.path(OUT_DIR, "gold_305_precision_v2.csv")))
  } else cat("  no v2-AI-305 in the filtered carried set — not estimable from this sample.\n")
  cat("=======================================================================\n")
}
