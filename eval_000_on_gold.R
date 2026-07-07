# ============================================================================
# eval_000_on_gold.R
# ----------------------------------------------------------------------------
# VALIDITAETS-Check des gbert-000-Klassifikators gegen die MENSCHLICHEN Gold-000-
# Labels (Ergaenzung zum silber-relativen Holdout/CV). Beantwortet: stimmt gberts
# is_000 mit dem menschlichen Nicht-Codierbarkeits-Urteil ueberein?
#
#   human_000 := (gold agg_A == "Nicht zuordenbar (000)")   -> 23 Positive von 259
#   model     := gberts p_000 / is_000 aus dem v2_000-Korpus, ueber Gold-Match geholt
#
# Match wie Skript 13: speech_id + whitespace-insensitiver Satztext.
# Rechnet Precision / Recall / F1 @ Schwelle 0.50 + ROC-AUC (rank-basiert, ohne Paket).
#
# CAVEATS, die der Output ausweist:
#   - kleines n (23 Positive) -> breite CIs, Spot-Check, keine korpus-gewichtete Rate.
#   - Gold ist auf MODELL-Buckets stratifiziert + 305-geboostet -> 000 unterrepraesentiert
#     (8,9 % statt ~22,9 %); die 23 sind v.a. Saetze, die das Modell in einen Inhalts-
#     Bucket legte, der Mensch aber als 000 las -> genau die harten Faelle fuer gbert.
#   - Trainings-Overlap: Gold-Saetze, die im 20k-gbert-Trainingssample lagen, werden
#     (falls die Sample-Datei angegeben ist) ausgeschlossen -> sauber held-out.
# ============================================================================
source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table); library(stringr) })
if (!requireNamespace("openxlsx", quietly = TRUE)) stop("install.packages('openxlsx')")

# ---- PARAMETER (Pfade ggf. anpassen) --------------------------------------
GS_DIR        <- file.path(PATHS$out_dir, "goldstandard")
CODING_XLSX   <- file.path(GS_DIR, "goldstandard_codingsheet_FINAL_merged.xlsx")
CODING_SHEET  <- "Coding"
KEY_SLIM      <- file.path(GS_DIR, "goldstandard_key_slim_FINAL.csv")   # gs_id -> speech_id
PARQUET_000   <- "C:/RProj_MSc/MScThesis/Data/sentences_classified_v2_000"
TRAIN_SAMPLE  <- file.path(GS_DIR, "control_sample_10k.parquet")        # 20k-gbert-Train; "" = kein Overlap-Check
THR           <- 0.50
HUMAN_000_RX  <- "000"          # nur das Nicht-zuordenbar-Label enthaelt "000"
nrm <- function(x) tolower(gsub("[[:space:]]+", "", x, perl = TRUE))

# ---- 1. menschliche Labels + Key ------------------------------------------
cod <- as.data.table(openxlsx::read.xlsx(CODING_XLSX, sheet = CODING_SHEET))
stopifnot(all(c("gs_id", "sentence", "agg_A") %in% names(cod)))
cod <- cod[!is.na(agg_A) & trimws(agg_A) != ""]
cod[, human_000 := grepl(HUMAN_000_RX, agg_A)]
key <- fread(KEY_SLIM, encoding = "UTF-8")[, .(gs_id, speech_id)]
gold <- merge(cod[, .(gs_id, sentence, agg_A, human_000)], key, by = "gs_id", all.x = TRUE)
gold[, key_txt := nrm(sentence)]
message(sprintf("[eval] Gold: %d Saetze, davon human_000 = %d (%.1f %%).",
                nrow(gold), sum(gold$human_000), 100 * mean(gold$human_000)))

# ---- 2. gbert-Vorhersagen aus v2_000 ueber den Match holen -----------------
sids <- unique(gold$speech_id)
ds <- open_dataset(PARQUET_000)
need <- intersect(c("speech_id", "sentence", "sentence_nr", "p_000", "is_000"), names(ds))
v <- ds %>% filter(speech_id %in% sids) %>% select(all_of(need)) %>% collect() %>% as.data.table()
v[, key_txt := nrm(sentence)]
# eindeutiger Match pro (speech_id, normalisierter Text); bei Mehrfachvorkommen 1. nehmen
vu <- v[, .SD[1L], by = .(speech_id, key_txt), .SDcols = c("p_000", "is_000", "sentence_nr")]
m <- merge(gold, vu, by = c("speech_id", "key_txt"), all.x = TRUE)
carried <- m[!is.na(p_000)]
message(sprintf("[eval] gematcht (carried): %d/%d (%.1f %%) — Carry-Kanarie >=96 %% erwartet.",
                nrow(carried), nrow(gold), 100 * nrow(carried) / nrow(gold)))

# ---- 3. optional: gbert-Trainings-Overlap ausschliessen --------------------
if (nzchar(TRAIN_SAMPLE) && file.exists(TRAIN_SAMPLE)) {
  tr <- as.data.table(read_parquet(TRAIN_SAMPLE, col_select = c("speech_id", "sentence")))
  tr[, key_txt := nrm(sentence)]
  before <- nrow(carried)
  carried <- carried[!paste(speech_id, key_txt) %in% tr[, paste(speech_id, key_txt)]]
  message(sprintf("[eval] Trainings-Overlap entfernt: %d Saetze (held-out: %d).",
                  before - nrow(carried), nrow(carried)))
}

# ---- 4. Metriken @ Schwelle ------------------------------------------------
# konsistent aus p_000 schwellen (is_000 koennte mit anderer Schwelle gebacken sein)
carried[, pred_000 := p_000 >= THR]
TP <- carried[human_000 == TRUE  & pred_000 == TRUE,  .N]
FP <- carried[human_000 == FALSE & pred_000 == TRUE,  .N]
FN <- carried[human_000 == TRUE  & pred_000 == FALSE, .N]
TN <- carried[human_000 == FALSE & pred_000 == FALSE, .N]
prec <- TP / max(1, TP + FP)
rec  <- TP / max(1, TP + FN)
f1   <- if (prec + rec > 0) 2 * prec * rec / (prec + rec) else 0
spec <- TN / max(1, TN + FP)

# ROC-AUC rank-basiert (Mann-Whitney), ohne Paket
pos <- carried[human_000 == TRUE,  p_000]
neg <- carried[human_000 == FALSE, p_000]
auc <- if (length(pos) && length(neg)) {
  r <- rank(c(pos, neg)); (sum(r[seq_along(pos)]) - length(pos)*(length(pos)+1)/2) / (length(pos)*length(neg))
} else NA_real_

# ---- 5. Report -------------------------------------------------------------
cat("\n================ gbert-000 vs. GOLD (human) ================\n")
cat(sprintf("n (held-out, gematcht) : %d   | human_000 = %d, codeable = %d\n",
            nrow(carried), TP + FN, TN + FP))
cat(sprintf("Schwelle               : p_000 >= %.2f\n", THR))
cat(sprintf("Confusion              : TP=%d  FP=%d  FN=%d  TN=%d\n", TP, FP, FN, TN))
cat(sprintf("PRECISION              : %.3f   <- von gbert-000-Vorhersagen sind %d/%d wirklich human-000\n",
            prec, TP, TP + FP))
cat(sprintf("Recall                 : %.3f   (%d/%d der human-000 gefunden)\n", rec, TP, TP + FN))
cat(sprintf("F1                     : %.3f\n", f1))
cat(sprintf("Specificity            : %.3f\n", spec))
cat(sprintf("ROC-AUC (p_000)        : %.3f\n", auc))
cat("\nCaveat: kleines n (23 Positive im Vollgold) -> breite CIs; stratifizierter,\n")
cat("nicht korpus-gewichteter Spot-Check. Als VALIDITAETS-Punkt berichten, nicht als Rate.\n")
fwrite(carried[, .(gs_id, speech_id, sentence, agg_A, human_000, p_000, is_000, pred_000)],
       file.path(GS_DIR, "eval_000_on_gold_rows.csv"))
cat(sprintf("\nZeilen -> %s\n", file.path(GS_DIR, "eval_000_on_gold_rows.csv")))
