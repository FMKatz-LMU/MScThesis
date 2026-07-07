# ============================================================================
# 07_control_sample.R — Zufallsstichprobe + gezielter Welfare-Abbau-Booster
# ----------------------------------------------------------------------------
# Zieht (a) eine korpus-gewichtete Zufallsstichprobe UND (b) einen separaten
# Booster von BERT-505-Sätzen ("Welfare Abbau"), um die im ersten Sample sehr
# dünne Achse Sozialstaat-Begrenzung (n=33, F1 0,148) robuster zu vermessen.
#
# WICHTIG: Beide Teile stehen in EINER Datei, aber mit der Spalte `sample_type`
#   - "random"               -> korpus-gewichtet; NUR diese für Korpus-κ/JSD nutzen
#   - "booster_welfare_cut"   -> gezielter Oversample; nur für die 505-Achse
# Ohne den Flag würde der Booster die Korpus-Schätzungen verzerren.
#
# Da wir noch keine Sonnet-Labels haben, kann der Booster nur BERTs Sicht
# ansteuern (pred_code == 505). Das fixiert robust BERTs 505-PRECISION; die
# Recall-Seite trägt v.a. das Zufallssample.
#
# >>> Konfiguriert für das ZWEITE Sample (_b): random 10k + 150er-Booster,
#     inkl. 56 Probs, kein Overlap mit dem ersten Sample. <<<
#
# Aufruf:  source(here::here("07_control_sample.R"))
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table) })

# ---- PARAMETER -------------------------------------------------------------
SAMPLE_TAG         <- "_b"
N_SAMPLE           <- 10000L               # zufälliger Teil
BOOST_N            <- 150L                  # Welfare-Abbau-Booster
BOOST_CODE         <- 505L                  # MARPOR 505 = Sozialstaat-Begrenzung
SEED               <- 20260616L
CS_ID_START        <- 10000L               # IDs ab CS10001 (erstes Sample endete CS10000)
EXCLUDE_PROCEDURAL <- FALSE
KEEP_PROBS         <- TRUE                  # 56 Probs mitschreiben (für den 000-Klassifikator)

GS_DIR       <- file.path(PATHS$out_dir, "goldstandard")
EXCLUDE_PATH <- file.path(GS_DIR, "control_sample_10k.parquet")   # erstes Sample ausschließen
OUT_BASE     <- paste0(sprintf("control_sample_%dk", as.integer(round(N_SAMPLE / 1000))), SAMPLE_TAG)
fs::dir_create(GS_DIR)

# ---- 1) Chunks + Prob-Spalten ----------------------------------------------
chunk_files <- list.files(PATHS$parquet_dir, pattern = "\\.parquet$", full.names = TRUE)
if (!length(chunk_files)) stop("Keine Parquet-Chunks unter ", PATHS$parquet_dir)

.nms      <- arrow::read_parquet(chunk_files[1], as_data_frame = FALSE)$schema$names
prob_cols <- grep("^[0-9]{3} - ", .nms, value = TRUE)
stopifnot(length(prob_cols) == 56L)
prob_codes <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
prob_cols  <- prob_cols[match(MARPOR_CODES_56, prob_codes)]
COL505     <- match(BOOST_CODE, MARPOR_CODES_56)
stopifnot(!is.na(COL505))

meta_cols <- intersect(
  c("speech_id", "sentence_nr", "sentence", "context_before", "context_after",
    "is_procedural", "party", "legislative_period", "date"), .nms)
a_lk  <- setNames(AGG_A$bucket_A, AGG_A$code)
bd_lk <- setNames(AGG_B$domain_B, AGG_B$code)
bb_lk <- setNames(AGG_B$bucket_B, AGG_B$code)

# ---- 1b) Bereits gezogene Sätze (Overlap vermeiden) ------------------------
excl_keys <- character(0)
if (!is.null(EXCLUDE_PATH) && nzchar(EXCLUDE_PATH) && file.exists(EXCLUDE_PATH)) {
  ex <- as.data.table(arrow::read_parquet(EXCLUDE_PATH, col_select = all_of(c("speech_id", "sentence_nr"))))
  excl_keys <- ex[, paste(speech_id, sentence_nr)]
  message(sprintf("[07] Schließe %s bereits gezogene Sätze aus.", format(length(excl_keys), big.mark = ".")))
}

# ---- 2) Row-Counts + proportionale Allokation (zufälliger Teil) ------------
message("[07] zähle Zeilen je Chunk ...")
counts <- vapply(chunk_files, function(f) as.numeric(arrow::open_dataset(f)$num_rows), numeric(1))
total  <- sum(counts)
message(sprintf("[07] Korpus: %s Sätze in %d Chunks", format(total, big.mark = "."), length(chunk_files)))
set.seed(SEED)
draw_chunk <- sample(seq_along(chunk_files), size = N_SAMPLE, replace = TRUE, prob = counts / total)
n_per      <- tabulate(draw_chunk, nbins = length(chunk_files))

# ---- 3) Je Chunk: pred auf VOLLEM Chunk; random ziehen + Booster sammeln ----
keep_cols  <- c(meta_cols, prob_cols)
rnd_list   <- vector("list", length(chunk_files))
boost_list <- vector("list", length(chunk_files))
message("[07] verarbeite Chunks (random + Booster) ...")

for (i in seq_along(chunk_files)) {
  dt <- as.data.table(arrow::read_parquet(chunk_files[i], col_select = all_of(keep_cols)))
  if (length(excl_keys)) dt <- dt[!(paste(speech_id, sentence_nr) %in% excl_keys)]
  if (EXCLUDE_PROCEDURAL && "is_procedural" %in% names(dt)) dt <- dt[is_procedural == FALSE]
  if (nrow(dt) == 0L) next

  P  <- as.matrix(dt[, ..prob_cols])
  am <- max.col(P, ties.method = "first")
  dt[, pred_code     := MARPOR_CODES_56[am]]
  dt[, pred_score    := P[cbind(seq_len(.N), am)]]
  dt[, p505          := P[, COL505]]
  dt[, pred_bucket_A := a_lk[as.character(pred_code)]]
  dt[, pred_domain_B := bd_lk[as.character(pred_code)]]
  dt[, pred_bucket_B := bb_lk[as.character(pred_code)]]
  if (!KEEP_PROBS) dt[, (prob_cols) := NULL]

  if (n_per[i] > 0L) rnd_list[[i]] <- dt[sample(.N, min(n_per[i], .N))]   # zufälliger Teil
  bp <- dt[pred_code == BOOST_CODE]                                       # Booster-Kandidaten
  if (nrow(bp)) boost_list[[i]] <- bp
  if (i %% 100L == 0L) message(sprintf("  ... %d/%d Chunks", i, length(chunk_files)))
}

# ---- 4) Random + Booster zusammenführen, Overlap zwischen beiden vermeiden -
random_dt <- rbindlist(rnd_list, use.names = TRUE, fill = TRUE)
random_dt[, sample_type := "random"]

boost_all <- rbindlist(boost_list, use.names = TRUE, fill = TRUE)
rnd_keys  <- random_dt[, paste(speech_id, sentence_nr)]
boost_all <- unique(boost_all, by = c("speech_id", "sentence_nr"))
boost_all <- boost_all[!(paste(speech_id, sentence_nr) %in% rnd_keys)]
set.seed(SEED + 2L)
boost_dt  <- boost_all[sample(.N, min(BOOST_N, .N))]                      # zufällig aus dem BERT-505-Pool
boost_dt[, sample_type := "booster_welfare_cut"]

# random mischen, Booster anhängen, fortlaufende cs_id
set.seed(SEED + 1L)
sample_dt <- rbind(random_dt[sample(.N)], boost_dt, use.names = TRUE, fill = TRUE)
sample_dt[, cs_id := sprintf("CS%05d", CS_ID_START + .I)]
if ("legislative_period" %in% names(sample_dt)) setnames(sample_dt, "legislative_period", "lp")

lead <- intersect(c("cs_id", "sample_type", "speech_id", "sentence_nr", "party", "lp", "date",
                    "is_procedural", "sentence", "context_before", "context_after",
                    "pred_code", "pred_score", "p505", "pred_bucket_A", "pred_domain_B", "pred_bucket_B"),
                  names(sample_dt))
setcolorder(sample_dt, c(lead, setdiff(names(sample_dt), lead)))

# ---- 5) Schreiben ----------------------------------------------------------
csv_cols <- intersect(c("cs_id", "sample_type", "speech_id", "sentence_nr", "party", "lp", "date",
                        "is_procedural", "sentence", "context_before", "context_after",
                        "pred_code", "pred_score", "p505", "pred_bucket_A", "pred_domain_B", "pred_bucket_B"),
                      names(sample_dt))
fwrite(sample_dt[, ..csv_cols], file.path(GS_DIR, paste0(OUT_BASE, ".csv")))
arrow::write_parquet(sample_dt,  file.path(GS_DIR, paste0(OUT_BASE, ".parquet")))   # inkl. 56 Probs

# ---- 6) Report -------------------------------------------------------------
n_rand  <- sum(sample_dt$sample_type == "random")
n_boost <- sum(sample_dt$sample_type == "booster_welfare_cut")
cat("\n==================== KONTROLLSTICHPROBE", SAMPLE_TAG, "====================\n")
cat(sprintf("random  : %d Sätze (korpus-gewichtet)\n", n_rand))
cat(sprintf("Booster : %d Sätze (BERT-505 'Welfare Abbau'; verfügbarer Pool: %d)\n", n_boost, nrow(boost_all)))
cat(sprintf("gesamt  : %d | cs_id %s … %s | Seed %d | Probs: %s | Overlap-Ausschluss: %d\n",
            nrow(sample_dt), sample_dt$cs_id[1], sample_dt$cs_id[nrow(sample_dt)], SEED, KEEP_PROBS, length(excl_keys)))
cat("\nFiles:\n")
cat("  ", file.path(GS_DIR, paste0(OUT_BASE, ".csv")),     " <- liest 16 (SAMPLE_TAG='", SAMPLE_TAG, "')\n", sep = "")
cat("  ", file.path(GS_DIR, paste0(OUT_BASE, ".parquet")), " <- inkl. Probs (für den 000-Klassifikator)\n", sep = "")
cat("HINWEIS: Für Korpus-κ/JSD nur sample_type=='random' verwenden!\n")
cat("================================================================\n")
