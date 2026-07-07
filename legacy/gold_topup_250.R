# ============================================================================
# gold_topup_250.R — ONE-OFF patch (throwaway; NOT for the submission).
# ----------------------------------------------------------------------------
# Brings the gold to a total of TARGET_TOTAL sentences with every Aggregation-A
# bucket (MODEL label pred_bucket_A) >= FLOOR. The floor is already met, so the
# extra draws just reach the total; they are water-filled onto the thinnest
# model-label buckets (which incidentally also samples the human-thin buckets).
#
# Draws from the FILTERED corpus (is_procedural == FALSE), excludes EVERY
# existing gold sentence, reproducible by SEED. The paste table holds three
# content columns (context_before, sentence, context_after); you add agg_A /
# agg_B yourself. Alongside it the script writes a SLIM key and a FAT key for
# the new sentences (same schema as the existing keys, new gs_ids from GS0401,
# row-aligned with the paste table) so the top-up can be stacked into the keys
# and folded through script 13.
# ============================================================================
source(here::here("00_config.R"))
source(here::here("02_helpers.R"))
suppressPackageStartupMessages({ library(arrow); library(dplyr); library(data.table) })
if (!requireNamespace("openxlsx", quietly = TRUE)) stop("install.packages('openxlsx')")

# ---- PARAMETERS (edit paths if needed) ------------------------------------
GS_DIR       <- file.path(PATHS$out_dir, "goldstandard")
CARRIED_CSV  <- file.path(GS_DIR, "goldstandard_v2_carried.csv")        # current table (has pred_bucket_A)
EXCLUDE_KEYS <- c(file.path(GS_DIR, "goldstandard_key.rds"),            # fat keys: speech_id + sentence
                  file.path(GS_DIR, "goldstandard_key_batch2.rds"))     # (skipped with a warning if missing)
TARGET_TOTAL <- 250L
FLOOR        <- 10L
SEED         <- 20260614L
nrm <- function(x) gsub("[[:space:]]+", "", x, perl = TRUE)

# ---- 1. current model-label counts + the existing-gold exclusion set -------
car <- fread(CARRIED_CSV, encoding = "UTF-8")
stopifnot(all(c("pred_bucket_A", "speech_id", "sentence") %in% names(car)))
cur <- merge(data.table(pred_bucket_A = BUCKETS_A),
             car[, .N, by = pred_bucket_A], by = "pred_bucket_A", all.x = TRUE)
cur[is.na(N), N := 0L]
n_have <- sum(cur$N); n_add <- TARGET_TOTAL - n_have
if (n_add <= 0L) stop(sprintf("already %d sentences (>= %d) — nothing to draw.", n_have, TARGET_TOTAL))

excl <- car[, paste(as.character(speech_id), nrm(sentence), sep = "\u0001")]
gsids_existing <- as.character(car$gs_id)
for (p in EXCLUDE_KEYS) {
  if (file.exists(p)) {
    k <- as.data.table(readRDS(p))
    excl <- c(excl, k[, paste(as.character(speech_id), nrm(sentence), sep = "\u0001")])
    if ("gs_id" %in% names(k)) gsids_existing <- c(gsids_existing, as.character(k$gs_id))
  } else warning("exclude key missing (skipped): ", p)
}
excl <- unique(excl)

# ---- 2. allocate n_add: guarantee the floor, then water-fill the smallest ---
counts <- setNames(as.integer(cur$N), cur$pred_bucket_A)
alloc  <- setNames(integer(length(counts)), names(counts))
for (b in names(counts)) { d <- max(0L, FLOOR - counts[[b]]); alloc[[b]] <- d; counts[[b]] <- counts[[b]] + d }
rem <- n_add - sum(alloc)
if (rem < 0L) stop(sprintf("floor alone needs %d draws but only %d available; raise TARGET_TOTAL.",
                           sum(alloc), n_add))
while (rem > 0L) { b <- names(counts)[which.min(counts)]; alloc[[b]] <- alloc[[b]] + 1L
                   counts[[b]] <- counts[[b]] + 1L; rem <- rem - 1L }

# ---- 3. read v2 (filtered), classify (argmax), exclude gold, draw -----------
ds <- arrow::open_dataset(PATHS$parquet_dir); .nms <- ds$schema$names
prob_cols <- grep("^[0-9]{3} - ", .nms, value = TRUE); stopifnot(length(prob_cols) == 56L)
prob_cols <- prob_cols[match(MARPOR_CODES_56, as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols)))]
message("[topup] scanning v2 (filtered corpus) ...")
v2 <- ds |>
  dplyr::filter(is_procedural == FALSE) |>
  dplyr::select(dplyr::all_of(c("speech_id", "sentence_nr", "date", "sentence",
                                "context_before", "context_after",
                                "party", "legislative_period", prob_cols))) |>
  dplyr::collect() |> as.data.table()
v2[, party := normalize_party(party)]
v2 <- v2[!is.na(party) & party %in% PARTIES_KEEP & legislative_period %in% LEGISLATIVE_PERIODS]
P  <- as.matrix(v2[, ..prob_cols]); am <- max.col(P, ties.method = "first")
a_lk  <- setNames(AGG_A$bucket_A, as.character(AGG_A$code))
db_lk <- setNames(AGG_B$domain_B, as.character(AGG_B$code))
bb_lk <- setNames(AGG_B$bucket_B, as.character(AGG_B$code))
v2[, pred_code     := MARPOR_CODES_56[am]]
v2[, pred_label    := prob_cols[am]]
v2[, pred_score    := P[cbind(seq_len(.N), am)]]
v2[, pred_bucket_A := a_lk[as.character(pred_code)]]
v2[, pred_domain_B := db_lk[as.character(pred_code)]]
v2[, pred_bucket_B := bb_lk[as.character(pred_code)]]
v2[, ckey := paste(speech_id, nrm(sentence), sep = "\u0001")]
pool <- v2[!(ckey %in% excl)]

set.seed(SEED)
drawn <- rbindlist(lapply(names(alloc)[alloc > 0L], function(b) {
  cand <- pool[pred_bucket_A == b]; n <- min(alloc[[b]], nrow(cand))
  if (n < alloc[[b]]) warning(sprintf("bucket '%s': only %d candidates (< %d requested).", b, n, alloc[[b]]))
  cand[sample(.N, n)]
}), use.names = TRUE)

# ---- 4. assign new gs_ids (row order == paste-table order) -----------------
maxnum <- suppressWarnings(max(c(0L, as.integer(sub("^GS", "", gsids_existing))), na.rm = TRUE))
drawn[, gs_id   := sprintf("GS%04d", maxnum + seq_len(.N))]
drawn[, uid     := paste(speech_id, sentence_nr, sep = "_")]
drawn[, lp      := legislative_period]
drawn[, stratum := pred_bucket_A]            # sampling axis here = predicted A-bucket
drawn[is.na(context_before), context_before := ""]
drawn[is.na(context_after),  context_after  := ""]

# ---- 5. key documents (row-aligned with the paste table; stackable) --------
slim_cols <- c("gs_id","speech_id","sentence_nr","party","lp","date",
               "pred_label","pred_code","pred_score",
               "pred_bucket_A","pred_domain_B","pred_bucket_B","stratum")
key_slim <- drawn[, ..slim_cols]
setnames(key_slim, c("pred_bucket_A","pred_domain_B","pred_bucket_B"),
                   c("bucket_A","domain_B","bucket_B"))
fwrite(key_slim, file.path(GS_DIR, "gold_topup_250_key_slim.csv"))

fat_head <- c("gs_id","uid","speech_id","sentence_nr","party","lp","date",
              "pred_label","pred_code","pred_score",
              "pred_bucket_A","pred_domain_B","pred_bucket_B","stratum",
              "sentence","context_before","context_after")
key_fat <- drawn[, c(fat_head, prob_cols), with = FALSE]
setnames(key_fat, c("pred_bucket_A","pred_domain_B","pred_bucket_B"),
                  c("bucket_A","domain_B","bucket_B"))
saveRDS(key_fat, file.path(GS_DIR, "gold_topup_250_key_fat.rds"))

# ---- 6. output: ONLY the three content columns -----------------------------
out <- drawn[, .(context_before, sentence, context_after)]
fwrite(out, file.path(GS_DIR, "gold_topup_250.csv"))
openxlsx::write.xlsx(out, file.path(GS_DIR, "gold_topup_250.xlsx"))

# ---- report ----------------------------------------------------------------
rep <- data.table(bucket = names(alloc),
                  have   = cur$N[match(names(alloc), cur$pred_bucket_A)],
                  draw   = as.integer(alloc))
rep[, final := have + draw][order(-draw)]
cat(sprintf("\nhave %d  ->  target %d  ->  drew %d  (pool after exclusion: %d)\n",
            n_have, TARGET_TOTAL, nrow(out), nrow(pool)))
cat("\nper MODEL-label A-bucket:\n"); print(rep[order(-draw)], row.names = FALSE)
cat(sprintf("\nwrote 3-column paste table -> %s  (and .xlsx)\n",
            file.path(GS_DIR, "gold_topup_250.csv")))
cat(sprintf("wrote slim key            -> %s\n", file.path(GS_DIR, "gold_topup_250_key_slim.csv")))
cat(sprintf("wrote fat key             -> %s\n", file.path(GS_DIR, "gold_topup_250_key_fat.rds")))
cat(sprintf("new gs_ids: %s .. %s  (row-aligned with the paste table)\n",
            drawn$gs_id[1], drawn$gs_id[nrow(drawn)]))
cat("paste columns context_before / sentence / context_after into your coding sheet; code agg_A/agg_B yourself.\n")
