# ============================================================================
# 06_gold_realign_validate.R — carry the coded gold onto v2, then validate it
# ----------------------------------------------------------------------------
# MERGE (pipeline reorg 2026-06): vereint
#   * 13_goldstandard_realign_FINAL.R  (Gold auf den v2-Korpus tragen + Human-vs-
#                                       BERT-v2-Argmax-Validierung, 305-Precision)
#   * 14_silver_validation.R           (Drei-Kodierer-Validierung Human/BERT/
#                                       CLAUDE: Domaene A, Richtung B, Feincode F,
#                                       305=000-Sensitivitaet, Soft-Agreement)
#
# Beide Validierungs-Perspektiven bleiben ERHALTEN: 14 spiegelt bewusst 13's
# codeable-only / soft / konfidenz-stratifizierten Kappa auf den rds-Daten
# (Cross-Check, keine Redundanz). Jeder Teil laeuft in seinem EIGENEN
# local({})-Scope, damit die gleichnamigen, aber unterschiedlichen Helfer
# (v.a. cohen_kappa: 13 -> Liste, 14 -> Skalar) einander NICHT ueberschreiben.
# Rueckgaben fuer interaktives Nachsehen:
#   gold_carry$carried / $gaps / $probs   (Teil 1)
#   silver_val$data / $metrics            (Teil 2)
# Alle eigentlichen Ergebnisse werden ohnehin als CSV/RDS/XLSX nach
# results/empirics/goldstandard/ geschrieben.
#
# source(here::here("00_config.R")) zieht jetzt auch die Helfer (ex 02_helpers.R).
# PREREQUISITE: aggregate (ex 03) ist gelaufen und Data/sentences_classified_v2/
#   existiert; die Gold-Inputs liegen in results/empirics/goldstandard/.
# ============================================================================

source(here::here("00_config.R"))   # PATHS, AGG_A/B, MARPOR_CODES_56, BUCKETS_A, BUCKET_OF_305_A + Helfer
suppressPackageStartupMessages({
  library(arrow); library(data.table); library(stringr)   # Teil 1 (realign)
  library(tidyverse); library(readxl); library(jsonlite)   # Teil 2 (silver)
})
if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("Package 'openxlsx' is required (install.packages('openxlsx')).")


# ############################################################################
# ##  TEIL 1 — REALIGN + v2-VALIDIERUNG   (formerly 13_goldstandard_realign)  ##
# ############################################################################
gold_carry <- local({
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
message(sprintf("[06a] coded sentences to carry: %d (from %d sheet(s))",
                nrow(coded), length(CODED_XLSX)))

key <- rbindlist(lapply(KEY_CSV, fread), use.names = TRUE, fill = TRUE)
key[, gs_id := as.character(gs_id)]
key[, speech_id := as.character(speech_id)]
dup_key <- unique(key$gs_id[duplicated(key$gs_id)])           # same gs_id in >1 key file
if (length(dup_key))
  warning(sprintf("[06a] %d gs_id appear in MULTIPLE key files (kept first — possible batch id collision): %s",
                  length(dup_key), paste(head(dup_key, 5), collapse = ", ")))
key <- unique(key, by = "gs_id")
coded[, gs_id := as.character(gs_id)]
dup_coded <- unique(coded$gs_id[duplicated(coded$gs_id)])     # same gs_id coded in >1 sheet -> double-match
if (length(dup_coded))
  warning(sprintf("[06a] %d gs_id coded in MULTIPLE sheets (would double-match): %s",
                  length(dup_coded), paste(head(dup_coded, 5), collapse = ", ")))

b1 <- merge(coded, key[, .(gs_id, speech_id,
                           old_lp = lp, old_party = party, old_date = date,
                           old_stratum = if ("stratum" %in% names(key)) stratum else NA_character_)],
            by = "gs_id", all.x = TRUE)
if (anyNA(b1$speech_id))
  warning(sprintf("[06a] %d coded gs_id had no speech_id in the key (cannot match).",
                  sum(is.na(b1$speech_id))))
b1[, nrm_sentence := nrm(sentence)]
need_ids <- unique(stats::na.omit(b1$speech_id))
message(sprintf("[06a] distinct speeches to scan in v2: %d", length(need_ids)))

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

message("[06a] scanning v2 parquet for the needed speeches ...")
v2 <- arrow::open_dataset(PATHS$parquet_dir) |>
  dplyr::filter(speech_id %in% need_ids) |>
  dplyr::select(dplyr::all_of(c(meta_cols, prob_cols))) |>
  dplyr::collect() |>
  as.data.table()
v2[, speech_id := as.character(speech_id)]
message(sprintf("[06a] v2 rows in those speeches: %d", nrow(v2)))

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
  message("[06a] no evaluable carried sentences -> validation skipped.")
} else {
  message(sprintf("[06a] validation: %d carried | %d evaluable (seg-faults out) | %d codeable",
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
           file.path(OUT_DIR, "gold_305_precision_v2.csv"))     # <- read by 13 PART 3
  } else {
    message("[06a] no v2-AI-305 in the filtered carried set -> gold_305_precision_v2.csv NOT written (13 stays PENDING).")
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

  # local() return (carried_out/gaps_out/carried_probs werden vor der Validierung
  # unbedingt erzeugt -> immer vorhanden, auch wenn die Validierung uebersprungen wird)
  invisible(list(carried = carried_out, gaps = gaps_out, probs = carried_probs))
})


# ############################################################################
# ##  TEIL 2 — DREI-KODIERER-VALIDIERUNG   (formerly 14_silver_validation)    ##
# ############################################################################
silver_val <- local({
# ----------------------------------------------------------------------------
# 0) Konfiguration  -- bei Bedarf hier anpassen
# ----------------------------------------------------------------------------
GS_DIR <- here::here("results", "empirics", "goldstandard")          # Projektwurzel/results/empirics/goldstandard

cfg <- list(
  base_dir         = GS_DIR,                                      # Ordner mit den Input-Dateien
  human_xlsx       = "goldstandard_codingsheet_FINAL_merged.xlsx",
  human_sheet      = "Coding",
  bert_rds         = "goldstandard_key_FINAL.rds",
  claude_json      = "marpor_coding_GS0001-GS0425.json",          # <- ggf. an deinen Dateinamen anpassen
  out_dir          = GS_DIR,                                      # Ausgaben flach in denselben Ordner (wie TEIL 1)
  make_plots       = TRUE,                                        # ggplot2-Heatmaps + Confidence-Plot
  # Domänen, in denen agg_B (Richtung) überhaupt definiert ist:
  directional_domains = c("Economy", "Welfare & Social Policy",
                          "Migration", "European Integration"),
  none_label       = "(keine Richtung)"                           # Platzhalter für NA auf B-Ebene
)

p   <- function(...)  file.path(cfg$base_dir, ...)   # Pfad zu Input-Dateien
out <- function(file) file.path(cfg$out_dir,  file)  # Pfad zu Output-Dateien
dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------------
# 1) Feincode -> Bucket-Crosswalk (kanonisch, an die Pipeline angelehnt)
# ----------------------------------------------------------------------------
#  bucket_A  = Domäne (Aggregation A)
#  bucket_B  = Richtung (Aggregation B); NA = nicht-gerichtet bzw. Domäne ohne Richtung
#  Die 46 von BERT tatsächlich vergebenen Codes stammen 1:1 aus goldstandard_key_FINAL.rds
#  (siehe Validierung unten); die übrigen Codes (000,101,103,203,304,401,404,405,409,412,704)
#  sind ergänzt nach Buckets-Sheet + Standard-Markt/Staat-Logik (MARPOR).
crosswalk <- tibble::tribble(
  ~code, ~bucket_A,                          ~bucket_B,
  "000", "Nicht zuordenbar (000)",           NA,
  "101", "Foreign Policy & Defence",         NA,
  "102", "Foreign Policy & Defence",         NA,
  "103", "Foreign Policy & Defence",         NA,
  "104", "Foreign Policy & Defence",         NA,
  "105", "Foreign Policy & Defence",         NA,
  "106", "Foreign Policy & Defence",         NA,
  "107", "Foreign Policy & Defence",         NA,
  "108", "European Integration",             "Pro-EU",
  "109", "Foreign Policy & Defence",         NA,
  "110", "European Integration",             "Contra-EU",
  "201", "Democracy & Political System",     NA,
  "202", "Democracy & Political System",     NA,
  "203", "Democracy & Political System",     NA,
  "204", "Democracy & Political System",     NA,
  "301", "Democracy & Political System",     NA,
  "302", "Democracy & Political System",     NA,
  "303", "Democracy & Political System",     NA,
  "304", "Democracy & Political System",     NA,
  "305", "Democracy & Political System",     NA,
  "401", "Economy",                          "Marktliberalismus",
  "402", "Economy",                          "Marktliberalismus",
  "403", "Economy",                          "Staatsintervention",
  "404", "Economy",                          "Staatsintervention",
  "405", "Economy",                          "Staatsintervention",
  "406", "Economy",                          "keine klare Richtung",  # 406/415 = "Economy (sonstige)" -> auf Human-Label gemappt
  "407", "Economy",                          "Marktliberalismus",
  "408", "Economy",                          "Wirtschaft Allgemein",
  "409", "Economy",                          "Staatsintervention",
  "410", "Economy",                          "Wirtschaft Allgemein",
  "411", "Economy",                          "Wirtschaft Allgemein",
  "412", "Economy",                          "Staatsintervention",
  "413", "Economy",                          "Staatsintervention",
  "414", "Economy",                          "Marktliberalismus",
  "415", "Economy",                          "keine klare Richtung",
  "416", "Environment",                      NA,
  "501", "Environment",                      NA,
  "502", "Welfare & Social Policy",          "keine klare Richtung",  # 502 = "Welfare (sonstige)" -> auf Human-Label gemappt
  "503", "Welfare & Social Policy",          "Sozialstaat Ausbau",
  "504", "Welfare & Social Policy",          "Sozialstaat Ausbau",
  "505", "Welfare & Social Policy",          "Sozialstaat Begrenzung",
  "506", "Welfare & Social Policy",          "Sozialstaat Ausbau",
  "507", "Welfare & Social Policy",          "Sozialstaat Begrenzung",
  "601", "Migration",                        "Migration restriktiv",
  "602", "Migration",                        "Migration liberal",
  "603", "Law & Order and National Identity", NA,
  "604", "Law & Order and National Identity", NA,
  "605", "Law & Order and National Identity", NA,
  "606", "Law & Order and National Identity", NA,
  "607", "Migration",                        "Migration liberal",
  "608", "Migration",                        "Migration restriktiv",
  "701", "Social Groups",                    NA,
  "702", "Social Groups",                    NA,
  "703", "Social Groups",                    NA,
  "704", "Social Groups",                    NA,
  "705", "Social Groups",                    NA,
  "706", "Social Groups",                    NA
)

# ----------------------------------------------------------------------------
# 2) Hilfsfunktionen (Metriken; bewusst ohne Zusatzpakete)
# ----------------------------------------------------------------------------

## Richtung normalisieren: gemeinsame Label-Vokabular über alle Kodierer.
## - "Andere"/"" -> NA  (nicht-gerichtete Domänen)
## - in Economy/Welfare: fehlende Richtung -> "keine klare Richtung"
##   (damit Human "keine klare Richtung" und Pipeline-NaN dasselbe Label tragen)
normalize_direction <- function(domain, direction) {
  d <- as.character(direction)
  d[d %in% c("Andere", "", "NA")] <- NA
  econ_welf <- domain %in% c("Economy", "Welfare & Social Policy")
  d[econ_welf & is.na(d)] <- "keine klare Richtung"
  d
}

## Cohen's Kappa (zwei Vektoren, ungeordnet kategorial; paarweise vollständige Fälle)
cohen_kappa <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) == 0) return(NA_real_)
  lev <- sort(union(unique(as.character(a)), unique(as.character(b))))
  a <- factor(as.character(a), levels = lev)
  b <- factor(as.character(b), levels = lev)
  tab <- table(a, b)
  n   <- sum(tab)
  po  <- sum(diag(tab)) / n
  pe  <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (abs(1 - pe) < .Machine$double.eps) return(NA_real_)
  (po - pe) / (1 - pe)
}

## Accuracy (Anteil exakter Übereinstimmungen über vollständige Fälle)
accuracy <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) == 0) return(NA_real_)
  mean(as.character(a[ok]) == as.character(b[ok]))
}

## Per-Klasse Precision/Recall/F1 (truth = Referenz, pred = Vorhersage)
class_prf <- function(truth, pred) {
  ok <- !is.na(truth) & !is.na(pred)
  truth <- as.character(truth[ok]); pred <- as.character(pred[ok])
  lev <- sort(union(unique(truth), unique(pred)))
  truth <- factor(truth, lev); pred <- factor(pred, lev)
  cm <- table(truth, pred)
  tp        <- diag(cm)
  support   <- rowSums(cm)     # echte Fälle je Klasse
  pred_pos  <- colSums(cm)     # vorhergesagte Fälle je Klasse
  precision <- ifelse(pred_pos > 0, tp / pred_pos, NA_real_)
  recall    <- ifelse(support  > 0, tp / support,  NA_real_)
  f1        <- ifelse((precision + recall) > 0,
                      2 * precision * recall / (precision + recall), 0)
  tibble(class = lev,
         support   = as.integer(support),
         precision = as.numeric(precision),
         recall    = as.numeric(recall),
         f1        = as.numeric(f1))
}

## Headline-Kennzahlen aus einem PRF-Frame (Macro- und Support-gewichtetes F1)
prf_headline <- function(truth, pred) {
  prf <- class_prf(truth, pred)
  tibble(
    n          = sum(!is.na(truth) & !is.na(pred)),
    accuracy   = accuracy(truth, pred),
    kappa      = cohen_kappa(truth, pred),
    macro_f1   = mean(prf$f1, na.rm = TRUE),
    weighted_f1= sum(prf$f1 * prf$support, na.rm = TRUE) / sum(prf$support)
  )
}

## Confusion-Matrix als langes Tibble (truth-Zeilen x pred-Spalten)
confusion_long <- function(truth, pred, truth_name = "truth", pred_name = "pred") {
  ok <- !is.na(truth) & !is.na(pred)
  truth <- as.character(truth[ok]); pred <- as.character(pred[ok])
  lev <- sort(union(unique(truth), unique(pred)))
  tab <- table(factor(truth, lev), factor(pred, lev))
  as.data.frame(tab, stringsAsFactors = FALSE) |>
    rlang::set_names(c(truth_name, pred_name, "n")) |>
    tibble::as_tibble()
}

write_out <- function(x, file) {
  readr::write_csv(x, out(file))
  invisible(x)
}

# ----------------------------------------------------------------------------
# 3) Daten laden
# ----------------------------------------------------------------------------

## (1) HUMAN
human <- read_excel(p(cfg$human_xlsx), sheet = cfg$human_sheet) |>
  transmute(
    gs_id,
    human_A     = as.character(agg_A),
    human_B_raw = as.character(agg_B),
    human_dps   = as.character(dps_fine),
    human_flag  = suppressWarnings(as.logical(flag_unsure)),
    human_note  = as.character(note)
  )

## (2) BERT  (rds enthält Feincode + Buckets + volle Wahrscheinlichkeiten + pred_score)
bert_raw <- readRDS(p(cfg$bert_rds))
stopifnot(all(c("gs_id","pred_code","pred_score","bucket_A","bucket_B") %in% names(bert_raw)))
bert <- bert_raw |>
  transmute(
    gs_id,
    party      = as.character(party),
    lp         = suppressWarnings(as.integer(lp)),
    bert_code  = sprintf("%03d", as.integer(round(as.numeric(pred_code)))),
    bert_score = as.numeric(pred_score),
    bert_A     = as.character(bucket_A),
    bert_B     = as.character(bucket_B)
  )

## (3) CLAUDE  (JSON; Feincode in Feld "marpor")
if (!file.exists(p(cfg$claude_json))) {
  cand <- list.files(cfg$base_dir, pattern = "\\.json$", full.names = FALSE)
  stop("Claude-JSON '", cfg$claude_json, "' nicht gefunden. Gefundene .json im Ordner: ",
       if (length(cand)) paste(cand, collapse = ", ") else "(keine)",
       "\n-> cfg$claude_json entsprechend setzen.")
}
claude <- jsonlite::fromJSON(p(cfg$claude_json)) |>
  as_tibble() |>
  transmute(
    gs_id,
    claude_code  = stringr::str_pad(as.character(marpor), 3, pad = "0"),
    claude_fehlt = as.logical(fehltrennung),
    claude_flag  = as.logical(flag_unsure),
    claude_conf  = as.character(confidence),
    claude_note  = as.character(note)
  )

# ----------------------------------------------------------------------------
# 4) Crosswalk gegen BERT-Key validieren (keine widersprüchlichen Mappings)
# ----------------------------------------------------------------------------
bert_cw <- bert_raw |>
  transmute(code = sprintf("%03d", as.integer(round(as.numeric(pred_code)))),
            bucket_A_emp = as.character(bucket_A),
            bucket_B_emp = as.character(bucket_B)) |>
  distinct()

check <- bert_cw |>
  left_join(crosswalk, by = "code") |>
  mutate(
    # in Economy/Welfare hat die Pipeline NaN, wo der Crosswalk "keine klare Richtung" setzt -> ok
    b_norm_emp = normalize_direction(bucket_A_emp, bucket_B_emp),
    b_norm_cw  = normalize_direction(bucket_A,     bucket_B),
    A_ok = bucket_A_emp == bucket_A,
    B_ok = (is.na(b_norm_emp) & is.na(b_norm_cw)) | (b_norm_emp == b_norm_cw)
  )
if (!all(check$A_ok & check$B_ok, na.rm = TRUE)) {
  warning("Crosswalk weicht vom BERT-Key ab — bitte prüfen:")
  print(filter(check, !(A_ok & B_ok)))
} else {
  message("Crosswalk-Validierung ok: stimmt fuer alle ", nrow(bert_cw),
          " von BERT vergebenen Codes mit dem Key ueberein.")
}

# ----------------------------------------------------------------------------
# 5) Zusammenführen + abgeleitete Spalten
# ----------------------------------------------------------------------------
cw_claude <- crosswalk |> rename(claude_code = code, claude_A = bucket_A, claude_B = bucket_B)

dat <- human |>
  full_join(bert,   by = "gs_id") |>
  full_join(claude, by = "gs_id") |>
  left_join(cw_claude, by = "claude_code") |>
  mutate(
    # Richtung (B) für alle drei Kodierer ins gemeinsame Vokabular bringen
    human_B  = normalize_direction(human_A,  human_B_raw),
    bert_B   = normalize_direction(bert_A,   bert_B),
    claude_B = normalize_direction(claude_A, claude_B),
    # Übereinstimmungs-Flags (Domäne A)
    agree_A_bert_human   = human_A == bert_A,
    agree_A_claude_human = human_A == claude_A,
    agree_A_bert_claude  = bert_A  == claude_A
  ) |>
  arrange(gs_id)

# Vollständigkeit prüfen
n_total <- nrow(dat)
missing <- dat |> filter(is.na(human_A) | is.na(bert_A) | is.na(claude_A))
if (nrow(missing) > 0) {
  warning(nrow(missing), " Satz/Sätze ohne vollständiges Tripel (Join-Lücke) — siehe join_gaps.csv")
  write_out(missing |> select(gs_id, human_A, bert_A, claude_A, claude_code), "join_gaps.csv")
}
message("Zeilen gesamt: ", n_total,
        " | vollständige Tripel: ", sum(complete.cases(dat[c("human_A","bert_A","claude_A")])))

# Lange Vergleichstabelle als Hauptartefakt
comparison_long <- dat |>
  select(gs_id, party, lp,
         human_A, bert_A, claude_A,
         human_B_raw, human_B, bert_B, claude_B,
         bert_code, claude_code, bert_score,
         claude_conf, claude_fehlt, claude_flag, human_flag,
         agree_A_bert_human, agree_A_claude_human, agree_A_bert_claude)
write_out(comparison_long, "comparison_long.csv")

# ----------------------------------------------------------------------------
# 6) EBENE A — Domäne (10 Klassen)
# ----------------------------------------------------------------------------
pairs_A <- tibble::tribble(
  ~vergleich,        ~truth,     ~pred,
  "BERT_vs_HUMAN",   "human_A",  "bert_A",
  "CLAUDE_vs_HUMAN", "human_A",  "claude_A",
  "BERT_vs_CLAUDE",  "claude_A", "bert_A"     # hier ist "truth" nur Referenzachse, kein Goldstandard
)

metrics_A <- pairs_A |>
  mutate(res = purrr::map2(truth, pred, ~prf_headline(dat[[.x]], dat[[.y]]))) |>
  tidyr::unnest(res) |>
  mutate(ebene = "A_Domaene", .before = 1) |>
  select(ebene, vergleich, n, accuracy, kappa, macro_f1, weighted_f1)

# Per-Klasse PRF (nur Vergleiche gegen HUMAN sind als Validierung sinnvoll)
prf_A_bert   <- class_prf(dat$human_A, dat$bert_A)   |> mutate(vergleich="BERT_vs_HUMAN",   .before=1)
prf_A_claude <- class_prf(dat$human_A, dat$claude_A) |> mutate(vergleich="CLAUDE_vs_HUMAN", .before=1)
write_out(bind_rows(prf_A_bert, prf_A_claude), "prf_A_domaene.csv")

# Confusion-Matrizen (Domäne)
write_out(confusion_long(dat$human_A, dat$bert_A,   "human","bert"),   "confusion_A_bert_vs_human.csv")
write_out(confusion_long(dat$human_A, dat$claude_A, "human","claude"), "confusion_A_claude_vs_human.csv")
write_out(confusion_long(dat$claude_A, dat$bert_A,  "claude","bert"),  "confusion_A_bert_vs_claude.csv")

# ----------------------------------------------------------------------------
# 7) EBENE B — Richtung (STRENG/roh; nur auf gerichteten Domänen laut HUMAN)
# ----------------------------------------------------------------------------
# Teilmenge: HUMAN ordnet den Satz einer gerichteten Domäne zu.
# NA-Richtung wird als eigenes Label (none_label) behandelt -> strenger Vergleich.
datB <- dat |>
  filter(human_A %in% cfg$directional_domains) |>
  mutate(
    human_Bx  = tidyr::replace_na(human_B,  cfg$none_label),
    bert_Bx   = tidyr::replace_na(bert_B,   cfg$none_label),
    claude_Bx = tidyr::replace_na(claude_B, cfg$none_label)
  )

pairs_B <- tibble::tribble(
  ~vergleich,        ~truth,      ~pred,
  "BERT_vs_HUMAN",   "human_Bx",  "bert_Bx",
  "CLAUDE_vs_HUMAN", "human_Bx",  "claude_Bx",
  "BERT_vs_CLAUDE",  "claude_Bx", "bert_Bx"
)
metrics_B <- pairs_B |>
  mutate(res = purrr::map2(truth, pred, ~prf_headline(datB[[.x]], datB[[.y]]))) |>
  tidyr::unnest(res) |>
  mutate(ebene = "B_Richtung_streng", .before = 1) |>
  select(ebene, vergleich, n, accuracy, kappa, macro_f1, weighted_f1)

prf_B_bert   <- class_prf(datB$human_Bx, datB$bert_Bx)   |> mutate(vergleich="BERT_vs_HUMAN",   .before=1)
prf_B_claude <- class_prf(datB$human_Bx, datB$claude_Bx) |> mutate(vergleich="CLAUDE_vs_HUMAN", .before=1)
write_out(bind_rows(prf_B_bert, prf_B_claude), "prf_B_richtung.csv")

write_out(confusion_long(datB$human_Bx, datB$bert_Bx,   "human","bert"),   "confusion_B_bert_vs_human.csv")
write_out(confusion_long(datB$human_Bx, datB$claude_Bx, "human","claude"), "confusion_B_claude_vs_human.csv")

# ----------------------------------------------------------------------------
# 8) EBENE F — Feincode (56 Klassen; nur BERT vs. CLAUDE)
# ----------------------------------------------------------------------------
metrics_F <- prf_headline(dat$claude_code, dat$bert_code) |>
  mutate(ebene = "F_Feincode", vergleich = "BERT_vs_CLAUDE", .before = 1)

# Top-Diskrepanzen Feincode (wo BERT und Claude am häufigsten auseinanderliegen)
fine_disagree <- dat |>
  filter(bert_code != claude_code) |>
  count(claude_code, bert_code, name = "n") |>
  arrange(desc(n))
write_out(fine_disagree, "fine_disagreement_bert_vs_claude.csv")

# ----------------------------------------------------------------------------
# 8b) SENSITIVITÄT — BERT-Code 305 ("Political Authority") als Äquivalent zu 000
# ----------------------------------------------------------------------------
# Motivation (Befund 2): ManifestoBERTa hat keine Restkategorie und leitet
# prozedurale 000-Sätze überwiegend auf 305 um. Diese Analyse wertet BERTs 305
# generell als 000 und prüft, was das für die Übereinstimmung bedeutet.
#
# WICHTIG: Das Remapping ist ein TRADE-OFF, kein Gratis-Gewinn. Ein Teil der
# 305-Sätze sind echte Demokratie-Aussagen, die durch die Regel fälschlich zu
# 000 werden. Die Diagnostik unten macht beide Seiten explizit (Recovery der
# 000-Fälle vs. Kollateralschaden an der Demokratie-Domäne).

remap_code <- "305"   # bei Bedarf anpassbar

# (a) BERT mit remappten Feincodes (305 -> 000), erneut über DENSELBEN Crosswalk
#     auf Domäne/Richtung abgebildet (konsistent mit der Hauptanalyse).
cw_bert_rm <- crosswalk |>
  rename(bert_code_rm = code, bert_A_rm = bucket_A, bert_B_rm_raw = bucket_B)
dat <- dat |>
  mutate(bert_code_rm = if_else(bert_code == remap_code, "000", bert_code)) |>
  left_join(cw_bert_rm, by = "bert_code_rm") |>
  mutate(bert_B_rm = normalize_direction(bert_A_rm, bert_B_rm_raw)) |>
  select(-bert_B_rm_raw)

# (b) Diagnostik 1 — wohin gehen die BERT-305-Sätze laut HUMAN bzw. CLAUDE?
b_rm <- dat |> filter(bert_code == remap_code)
write_out(b_rm |> count(human_A, name = "n") |> arrange(desc(n)) |>
            mutate(anteil = round(n / sum(n), 3)),
          sprintf("diag_bert%s_to_human.csv", remap_code))
write_out(b_rm |> count(claude_code, name = "n") |> arrange(desc(n)) |>
            mutate(anteil = round(n / sum(n), 3)),
          sprintf("diag_bert%s_to_claude.csv", remap_code))

# (c) Diagnostik 2 — Recovery (Treffer) vs. Kollateralschaden der 305=000-Regel
n_rm           <- nrow(b_rm)
n_human000     <- sum(dat$human_A     == "Nicht zuordenbar (000)",        na.rm = TRUE)
n_claude000    <- sum(dat$claude_code == "000",                           na.rm = TRUE)
hit_human000   <- sum(b_rm$human_A     == "Nicht zuordenbar (000)",       na.rm = TRUE)
hit_claude000  <- sum(b_rm$claude_code == "000",                          na.rm = TRUE)
collateral_dem <- sum(b_rm$human_A     == "Democracy & Political System", na.rm = TRUE)

diag_summary <- tibble::tibble(
  kennzahl = c(
    sprintf("BERT-%s gesamt", remap_code),
    "  davon HUMAN-000 (Recovery)",
    "  davon CLAUDE-000",
    "  davon HUMAN-Democracy (Kollateral -> faelschlich 000)",
    "HUMAN-000 gesamt",
    sprintf("  davon von BERT als %s kodiert (-> Recovery)", remap_code),
    "CLAUDE-000 gesamt",
    sprintf("  davon von BERT als %s kodiert", remap_code)
  ),
  wert = c(n_rm, hit_human000, hit_claude000, collateral_dem,
           n_human000, hit_human000, n_claude000, hit_claude000),
  anteil = c(NA_real_,
             round(hit_human000 / n_rm, 3), round(hit_claude000 / n_rm, 3),
             round(collateral_dem / n_rm, 3),
             NA_real_, round(hit_human000 / n_human000, 3),
             NA_real_, round(hit_claude000 / n_claude000, 3))
)
write_out(diag_summary, sprintf("diag_bert%s_equiv000_summary.csv", remap_code))

# (d) Neuberechnung der Übereinstimmung MIT der Regel (Domäne A + Feincode),
#     je 'original' vs. 'remap_305eq000', gegen HUMAN und CLAUDE.
mk <- function(truth, pred, ebene, vergleich, variante) {
  prf_headline(truth, pred) |>
    mutate(ebene = ebene, vergleich = vergleich, variante = variante, .before = 1)
}
metrics_305 <- bind_rows(
  mk(dat$human_A,     dat$bert_A,        "A_Domaene",  "BERT_vs_HUMAN",  "original"),
  mk(dat$human_A,     dat$bert_A_rm,     "A_Domaene",  "BERT_vs_HUMAN",  "remap_305eq000"),
  mk(dat$claude_A,    dat$bert_A,        "A_Domaene",  "BERT_vs_CLAUDE", "original"),
  mk(dat$claude_A,    dat$bert_A_rm,     "A_Domaene",  "BERT_vs_CLAUDE", "remap_305eq000"),
  mk(dat$claude_code, dat$bert_code,     "F_Feincode", "BERT_vs_CLAUDE", "original"),
  mk(dat$claude_code, dat$bert_code_rm,  "F_Feincode", "BERT_vs_CLAUDE", "remap_305eq000")
) |>
  select(ebene, vergleich, variante, n, accuracy, kappa, macro_f1, weighted_f1)
write_out(metrics_305, sprintf("metrics_bert%s_equiv000.csv", remap_code))

# Delta-Tabelle (remap minus original) je Vergleich/Ebene/Metrik
delta_305 <- metrics_305 |>
  tidyr::pivot_longer(c(accuracy, kappa, macro_f1, weighted_f1),
                      names_to = "metrik", values_to = "wert") |>
  tidyr::pivot_wider(names_from = variante, values_from = wert) |>
  mutate(delta = remap_305eq000 - original) |>
  select(ebene, vergleich, metrik, original, remap_305eq000, delta)
write_out(delta_305, sprintf("metrics_bert%s_equiv000_delta.csv", remap_code))

# (e) Per-Klasse-Effekt auf die zwei betroffenen Domänen (Referenz HUMAN):
#     000 (gewinnt Recall) und Democracy & Political System (verliert Recall).
prf_affected <- bind_rows(
  class_prf(dat$human_A, dat$bert_A)    |> mutate(variante = "original",       .before = 1),
  class_prf(dat$human_A, dat$bert_A_rm) |> mutate(variante = "remap_305eq000", .before = 1)
) |>
  filter(class %in% c("Nicht zuordenbar (000)", "Democracy & Political System")) |>
  arrange(class, variante)
write_out(prf_affected, sprintf("prf_bert%s_equiv000_affected.csv", remap_code))

# ----------------------------------------------------------------------------
# 8c) ABGLEICH MIT SKRIPT 13 — sophistizierte Maße
#     (codeable-only Kappa, konfidenz-stratifizierter Kappa, Soft Agreement)
# ----------------------------------------------------------------------------
# WARUM dieser Block (Erklärung der Kappa-Differenz zu TEIL 1):
#   TEIL 2 berichtet als Headline den ROH-Kappa über ALLE Sätze inkl. der
#   menschlichen 000-Fälle ("Nicht zuordenbar"). ManifestoBERTa kann 000
#   strukturell NIE vorhersagen (000 ist nicht unter den 56 Codes) — die
#   Human-000-Sätze sind damit ein asymmetrischer Strafterm, der den Kappa
#   nach unten zieht. TEIL 1 stellt eine andere Frage und berichtet:
#     (i)  kappa_A_codeable_only  — nur die 9 echten Buckets, Human-000 raus,
#     (ii) soft_agreement         — Modell-Posterior-Masse auf dem Human-Bucket
#          über die volle 56-dim Verteilung (passt zur weichen Aggregation),
#     (iii) konfidenz-stratifizierter Kappa (3 Bins) statt Accuracy-Bins.
#   Hier werden alle drei ergänzt, damit TEIL 2 dieselbe Frage beantwortet.
#
#   RESIDUUM: TEIL 1 nutzt zusätzlich FRISCHE v2-Vorhersagen (argmax auf dem
#   re-segmentierten v2-Korpus) und das konsolidierte FINAL-Gold abzüglich
#   Carry-Gaps; deshalb wird der codeable-only-Kappa hier (auf den rds-Daten)
#   die Skript-13-Zahl nicht auf die dritte Stelle treffen. Der definitorische
#   Haupthebel — Human-000 ausschließen — ist aber genau dieser.
#
#   CAVEAT (wie in 13): Das Gold-Sample ist bucket-STRATIFIZIERT (DPS über-
#   gewichtet). Jeder Kappa hier ist eine Per-Bucket-Reliabilität, KEINE
#   korpus-frequenz-gewichtete; nicht als Zufallsziehung lesen.

# (a) Evaluable / codeable Subsets (Seg-Faults & Platzhalter raus, analog 13)
dat <- dat |>
  mutate(
    is_segfault    = grepl("Feh", human_A, ignore.case = TRUE),        # Fehl-/Fehltrennung im HUMAN-Code
    is_placeholder = tolower(trimws(human_A)) %in% c("none", "na", ""),
    is_notassign   = grepl("zuordenbar|000", human_A)                  # HUMAN '000'
  )
eval_set <- dat |> filter(!is_segfault, !is_placeholder,
                          !is.na(human_A), nzchar(trimws(human_A)))
code_set <- eval_set |> filter(!is_notassign)                          # nur die 9 echten Buckets

# zentrale Kappa-Skalare (wiederverwendet in Tabelle + Konsole)
ka_raw_bert  <- cohen_kappa(eval_set$human_A, eval_set$bert_A)
ka_code_bert <- cohen_kappa(code_set$human_A, code_set$bert_A)

# (b) Kappa_A: roh (inkl. 000) vs. codeable-only — BERT UND CLAUDE gegen HUMAN
kappa_flavors <- tibble::tibble(
  vergleich = c("BERT_vs_HUMAN","BERT_vs_HUMAN","CLAUDE_vs_HUMAN","CLAUDE_vs_HUMAN"),
  subset    = c("raw_incl_000","codeable_only","raw_incl_000","codeable_only"),
  n         = c(nrow(eval_set), nrow(code_set), nrow(eval_set), nrow(code_set)),
  accuracy  = c(accuracy(eval_set$human_A, eval_set$bert_A),
                accuracy(code_set$human_A, code_set$bert_A),
                accuracy(eval_set$human_A, eval_set$claude_A),
                accuracy(code_set$human_A, code_set$claude_A)),
  kappa     = c(ka_raw_bert, ka_code_bert,
                cohen_kappa(eval_set$human_A, eval_set$claude_A),
                cohen_kappa(code_set$human_A, code_set$claude_A))
)
write_out(kappa_flavors, "agreement_kappa_flavors.csv")

# (c) Per-Party Kappa_A (codeable; BERT vs. HUMAN), wie in 13
perparty_A <- code_set |>
  group_by(party) |>
  summarise(n     = sum(!is.na(human_A) & !is.na(bert_A)),
            agree = round(accuracy(human_A, bert_A), 3),
            kappa = round(cohen_kappa(human_A, bert_A), 3),
            .groups = "drop") |>
  arrange(kappa)
write_out(perparty_A, "agreement_kappaA_by_party.csv")

# (d) Konfidenz-stratifizierter Kappa_A (3 Bins wie in 13: <0.5 / 0.5-0.8 / >=0.8)
code_set <- code_set |>
  mutate(conf_bin3 = cut(bert_score, breaks = c(-Inf, 0.5, 0.8, Inf),
                         labels = c("<0.5", "0.5-0.8", ">=0.8")))
kappa_by_conf <- code_set |>
  group_by(conf_bin3) |>
  summarise(n     = sum(!is.na(human_A) & !is.na(bert_A)),
            agree = round(accuracy(human_A, bert_A), 3),
            kappa = round(cohen_kappa(human_A, bert_A), 3),
            .groups = "drop") |>
  arrange(conf_bin3)
write_out(kappa_by_conf, "agreement_kappaA_by_confidence_3bin.csv")

# (e) SOFT AGREEMENT — Modell-Posterior-Masse auf dem HUMAN-Bucket (codeable).
#     Nutzt die 56 Wahrscheinlichkeits-Spalten ("NNN - Titel") aus dem rds.
prob_cols <- grep("^[0-9]{3} - ", names(bert_raw), value = TRUE)
if (length(prob_cols) == 0L) {
  message("[06b] Keine 56 Prob-Spalten im rds gefunden -> Soft Agreement übersprungen.")
  soft_overall <- NA_real_; soft_by_bin <- tibble::tibble()
} else {
  prob_code   <- sprintf("%03d", as.integer(substr(prob_cols, 1, 3)))
  prob_bucket <- crosswalk$bucket_A[match(prob_code, crosswalk$code)]   # Bucket je Prob-Spalte
  bert_probs  <- bert_raw |> select(gs_id, dplyr::all_of(prob_cols))

  soft <- code_set |>
    select(gs_id, human_A, conf_bin3) |>
    left_join(bert_probs, by = "gs_id")
  Pmat <- as.matrix(soft[, prob_cols])
  soft$soft_mass_human <- vapply(seq_len(nrow(soft)), function(i)
    sum(Pmat[i, prob_bucket == soft$human_A[i]], na.rm = TRUE), numeric(1))
  soft_overall <- mean(soft$soft_mass_human)
  soft_by_bin  <- soft |>
    group_by(conf_bin3) |>
    summarise(n = n(),
              mean_mass_on_human_bucket = round(mean(soft_mass_human), 3),
              .groups = "drop") |>
    arrange(conf_bin3)
  write_out(soft_by_bin, "agreement_soft_by_confidence.csv")
}

# (f) Sammel-Summary im Stil von TEIL 1 (goldstandard_v2_validation_summary)
agreement_sophisticated <- tibble::tibble(
  metric = c("kappa_A_raw_incl_notassignable", "kappa_A_codeable_only",
             "soft_agreement_mass_on_human_bucket"),
  n      = c(nrow(eval_set), nrow(code_set),
             if (exists("soft")) nrow(soft) else 0L),
  value  = round(c(ka_raw_bert, ka_code_bert, soft_overall), 3)
)
write_out(agreement_sophisticated, "agreement_sophisticated_summary.csv")

# ----------------------------------------------------------------------------
# 9) BERT-Confidence-Analyse (Agreement vs. pred_score, Referenz HUMAN, Ebene A)
# ----------------------------------------------------------------------------
# Hinweis: Dies ist die 5-Bin-ACCURACY-Sicht über ALLE Sätze (inkl. 000) und
# dient als ergänzende Farbe. Die Skript-13-konforme Sicht (konfidenz-
# stratifizierter KAPPA auf dem codeable Set, 3 Bins) steht in Sektion 8c.
conf <- dat |>
  filter(!is.na(bert_score)) |>
  mutate(score_bin = cut(bert_score,
                         breaks = c(0, .2, .4, .6, .8, 1.0),
                         include.lowest = TRUE,
                         labels = c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1.0")))

agreement_by_confidence <- conf |>
  group_by(score_bin) |>
  summarise(
    n              = n(),
    mean_score     = mean(bert_score),
    acc_bert_human = mean(agree_A_bert_human, na.rm = TRUE),
    .groups = "drop"
  )
write_out(agreement_by_confidence, "agreement_by_confidence.csv")

# Mittlerer Score bei Treffer vs. Fehler (BERT vs. HUMAN, Domäne)
score_by_hit <- conf |>
  group_by(treffer_bert_human = agree_A_bert_human) |>
  summarise(n = n(), mean_score = mean(bert_score), median_score = median(bert_score),
            .groups = "drop")
write_out(score_by_hit, "score_by_hit.csv")

# ----------------------------------------------------------------------------
# 10) Headline-Tabelle + Konsolen-Report
# ----------------------------------------------------------------------------
metrics_summary <- bind_rows(metrics_A, metrics_B, metrics_F)
write_out(metrics_summary, "metrics_summary.csv")

cat("\n=========================================================\n")
cat(" GOLDSTANDARD-VERGLEICH  (N =", n_total, "Sätze)\n")
cat("=========================================================\n\n")
cat("— Ebene A: Domäne (Aggregation A) —\n")
print(metrics_A   |> mutate(across(c(accuracy,kappa,macro_f1,weighted_f1), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Ebene B: Richtung (streng, nur gerichtete Domänen; N =", nrow(datB), ") —\n")
print(metrics_B   |> mutate(across(c(accuracy,kappa,macro_f1,weighted_f1), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Ebene F: Feincode (BERT vs. Claude) —\n")
print(metrics_F   |> select(vergleich,n,accuracy,kappa,macro_f1,weighted_f1) |>
        mutate(across(c(accuracy,kappa,macro_f1,weighted_f1), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Sensitivität: BERT-", remap_code, " \u2261 000 —\n", sep = "")
cat(sprintf("  BERT-%s gesamt: %d | Recovery (HUMAN-000): %d (%.0f%%) | Kollateral (echte Democracy): %d (%.0f%%)\n",
            remap_code, n_rm, hit_human000, 100*hit_human000/n_rm,
            collateral_dem, 100*collateral_dem/n_rm))
cat(sprintf("  000-Recall (BERT vs. HUMAN) steigt 0.00 -> %.2f (von %d HUMAN-000)\n",
            hit_human000/n_human000, n_human000))
print(delta_305 |> mutate(across(c(original, remap_305eq000, delta), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Abgleich mit TEIL 1: codeable-only Kappa + Soft Agreement (BERT vs. HUMAN) —\n")
cat(sprintf("  kappa_A roh (inkl. 000, n=%d)         : %.3f   <- Skript-14-Headline\n",
            nrow(eval_set), ka_raw_bert))
cat(sprintf("  kappa_A codeable-only (ohne 000, n=%d): %.3f   <- die Frage, die TEIL 1 stellt\n",
            nrow(code_set), ka_code_bert))
cat(sprintf("  soft agreement (Posterior-Masse, n=%d): %.3f\n",
            if (exists("soft")) nrow(soft) else 0L, soft_overall))
cat("\n  konfidenz-stratifizierter kappa_A (codeable, 3 Bins):\n")
print(kappa_by_conf |> as.data.frame(), row.names = FALSE)
if (nrow(soft_by_bin)) {
  cat("\n  soft agreement nach Konfidenz:\n")
  print(soft_by_bin |> as.data.frame(), row.names = FALSE)
}
cat("\n— Agreement BERT vs. HUMAN (Domäne) nach BERT-Confidence —\n")
print(agreement_by_confidence |> mutate(across(c(mean_score,acc_bert_human), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\nAlle Tabellen geschrieben nach: ", normalizePath(cfg$out_dir, mustWork = FALSE), "\n\n")

# ----------------------------------------------------------------------------
# 11) Plots (optional, ggplot2)
# ----------------------------------------------------------------------------
if (cfg$make_plots && requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  heat <- function(cm_long, truth_col, pred_col, title) {
    ggplot(cm_long, aes(.data[[pred_col]], .data[[truth_col]], fill = n)) +
      geom_tile() +
      geom_text(aes(label = ifelse(n > 0, n, "")), size = 3) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = title, x = paste(pred_col, "(Vorhersage)"),
           y = paste(truth_col, "(Referenz)")) +
      theme_minimal(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank())
  }

  ggsave(out("plot_confusion_A_bert_vs_human.png"),
         heat(confusion_long(dat$human_A, dat$bert_A, "human","bert"),
              "human","bert","Domäne (A): BERT vs. HUMAN"),
         width = 8, height = 6, dpi = 150)
  ggsave(out("plot_confusion_A_claude_vs_human.png"),
         heat(confusion_long(dat$human_A, dat$claude_A, "human","claude"),
              "human","claude","Domäne (A): CLAUDE vs. HUMAN"),
         width = 8, height = 6, dpi = 150)

  pc <- ggplot(agreement_by_confidence, aes(score_bin, acc_bert_human)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "BERT-Confidence vs. Übereinstimmung mit HUMAN (Domäne)",
         x = "pred_score (Bin)", y = "Accuracy BERT vs. HUMAN") +
    theme_minimal(base_size = 10)
  ggsave(out("plot_agreement_by_confidence.png"), pc,
         width = 7, height = 5, dpi = 150)

  message("Plots geschrieben (PNG) in ", cfg$out_dir, "/")
} else if (cfg$make_plots) {
  message("ggplot2 nicht installiert — Plots übersprungen (Tabellen sind vollständig).")
}

invisible(list(data = dat, metrics = metrics_summary))
})
