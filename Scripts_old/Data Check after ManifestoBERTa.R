# analyze_classified.R
# --------------------
# Loads classified speech sentences (parquet chunks) and runs quality
# diagnostics on the ManifestoBERTa output before moving to manifesto-side
# matching and JSD computation.
#
# Sections:
#   0. Setup
#   1. Load data (lazy via arrow)
#   2. Coverage & completeness
#   3. Confidence-score diagnostics
#   4. Predicted-label distribution
#   5. The "305 inflation" sanity check
#   6. Party x legislative period coverage
#   7. Save aggregated probability vectors per (party, LP)

# ---- 0. Setup -----------------------------------------------------------

library(arrow)
library(data.table)
library(dplyr)        # only used for arrow's lazy verbs
library(ggplot2)

setwd("C:/masterarbeit")  # adjust if needed
classified_dir <- file.path(getwd(), "Data", "sentences_classified")
stopifnot(dir.exists(classified_dir))


# ---- 1. Load data -------------------------------------------------------

chunk_files <- sort(list.files(
  classified_dir, pattern = "^chunk_\\d+\\.parquet$", full.names = TRUE
))
cat("Found", length(chunk_files), "parquet chunks.\n")

# Lazy dataset: queries push down to arrow, only collected results land in R.
ds <- arrow::open_dataset(chunk_files, format = "parquet")
cat("Schema columns:", length(ds$schema$names), "\n")
cat("Total rows:", format(ds$num_rows, big.mark = ","), "\n\n")

# Identify the 56 probability columns by their pattern: "NNN - <label>"
prob_cols <- grep("^[0-9]{3} - ", ds$schema$names, value = TRUE)
stopifnot(length(prob_cols) == 56)

meta_cols <- c("speech_id", "legislative_period", "speaker", "party",
               "date", "sentence_nr", "sentence")


# ---- 2. Coverage & completeness ----------------------------------------

cat("=== 2. Coverage & completeness ===\n")

# Pull only the small metadata columns into memory for the per-row checks.
meta_dt <- as.data.table(ds %>%
                           select(all_of(c(meta_cols, "pred_label", "pred_score"))) %>%
                           collect())

cat("Rows loaded:", format(nrow(meta_dt), big.mark = ","), "\n")
cat("Unique speeches:", format(uniqueN(meta_dt$speech_id), big.mark = ","), "\n")
cat("Unique speakers:", format(uniqueN(meta_dt$speaker), big.mark = ","), "\n")
cat("Date range:", as.character(min(meta_dt$date, na.rm = TRUE)),
    "to", as.character(max(meta_dt$date, na.rm = TRUE)), "\n")
cat("Missing pred_label:", sum(is.na(meta_dt$pred_label)), "\n")
cat("Missing pred_score:", sum(is.na(meta_dt$pred_score)), "\n")
cat("Sentence length stats (chars):\n")
print(summary(nchar(meta_dt$sentence)))


# ---- 3. Confidence-score diagnostics -----------------------------------

cat("\n=== 3. Confidence-score diagnostics ===\n")
cat("Overall pred_score distribution:\n")
print(summary(meta_dt$pred_score))

# Confidence buckets — useful for deciding if you'll later filter low-confidence
# sentences from JSD calculations.
meta_dt[, conf_bucket := cut(
  pred_score,
  breaks = c(0, 0.3, 0.5, 0.7, 0.9, 1.0),
  labels = c("very_low(<0.3)", "low(0.3-0.5)", "med(0.5-0.7)",
             "high(0.7-0.9)", "very_high(>0.9)"),
  include.lowest = TRUE
)]
cat("\nConfidence buckets:\n")
print(meta_dt[, .(N = .N, share = round(.N / nrow(meta_dt), 3)),
              by = conf_bucket][order(conf_bucket)])

# Confidence by legislative period — drift here would be suspicious
# (e.g. systematic confidence drop in later LPs might indicate vocabulary drift
# the model wasn't trained on).
cat("\nMean confidence by legislative period:\n")
print(meta_dt[, .(
  n          = .N,
  mean_score = round(mean(pred_score), 3),
  median     = round(median(pred_score), 3)
), by = legislative_period][order(legislative_period)])


# ---- 4. Predicted-label distribution -----------------------------------

cat("\n=== 4. Predicted-label distribution ===\n")
label_dist <- meta_dt[, .(n = .N), by = pred_label][order(-n)]
label_dist[, share := round(n / sum(n), 4)]
cat("Top 15 predicted labels:\n")
print(label_dist[1:15])
cat("\nBottom 5 predicted labels:\n")
print(label_dist[(.N - 4):.N])

# Concentration: Herfindahl index on predicted labels.
# H close to 1/56 (~0.018) means uniform; closer to 1 means dominated by few codes.
H <- sum((label_dist$share)^2)
cat(sprintf("\nHerfindahl index across predicted labels: %.4f", H),
    sprintf("(uniform reference = %.4f)\n", 1 / 56))


# ---- 5. The "305 inflation" sanity check -------------------------------

cat("\n=== 5. 305 - Political Authority inflation check ===\n")
share_305 <- mean(meta_dt$pred_label == "305 - Political Authority")
cat(sprintf("Share of sentences predicted as 305: %.1f%%\n", 100 * share_305))

# Is 305 driven by short/procedural sentences? If yes, mean sentence length
# of 305-predicted should be noticeably shorter than the rest.
meta_dt[, len := nchar(sentence)]
len_by_305 <- meta_dt[, .(
  mean_len    = round(mean(len), 1),
  median_len  = round(median(len), 1),
  mean_score  = round(mean(pred_score), 3),
  n           = .N
), by = .(is_305 = pred_label == "305 - Political Authority")]
cat("\nSentence length & confidence: 305 vs. other labels\n")
print(len_by_305)

# Confidence on 305: if the model is "dumping" procedural sentences here,
# we'd expect lower mean confidence on 305 than on substantive labels.
cat("\nMean confidence by predicted label (top 10 by frequency):\n")
top10_labels <- label_dist$pred_label[1:10]
print(meta_dt[pred_label %in% top10_labels,
              .(mean_score = round(mean(pred_score), 3),
                median_len = round(median(len), 1),
                n          = .N),
              by = pred_label][order(-n)])


# ---- 6. Party x legislative period coverage ----------------------------

cat("\n=== 6. Party x legislative period coverage ===\n")
# Parties that should appear in your panel: CDU, CSU, SPD, FDP, GRUENE, LINKE,
# PDS (early LPs), AfD (LP 19+). Anything weird (typos, NA) shows up here.
party_lp <- meta_dt[, .N, by = .(legislative_period, party)][order(legislative_period, -N)]
cat("Party breakdown by LP (only top 6 per LP shown):\n")
print(party_lp[, head(.SD, 6), by = legislative_period])


# ---- 7. Save aggregated probability vectors per (party, LP) ------------
# This is the workhorse output for the JSD calculation: for each
# (party, legislative_period) we sum the soft probability vectors across
# all sentences and renormalize. Result is a 56-d distribution per cell.

cat("\n=== 7. Aggregating probability vectors per (party, LP) ===\n")

# Using arrow's lazy compute to keep memory under control: group_by then
# summarise(across(prob_cols, sum)) pushed down to arrow.
agg <- ds %>%
  group_by(legislative_period, party) %>%
  summarise(
    n_sentences = n(),
    across(all_of(prob_cols), sum)
  ) %>%
  collect() %>%
  as.data.table()

# Renormalize each row's 56-d prob vector to sum to 1.
agg[, (prob_cols) := lapply(.SD, function(x) x / n_sentences), .SDcols = prob_cols]

# Sanity: every row's prob vector should sum to ~1.0
row_sums <- rowSums(agg[, ..prob_cols])
cat("Renormalized row sums range:", range(row_sums), "\n")

cat("Aggregated table dimensions:", dim(agg), "\n")
print(agg[, .(legislative_period, party, n_sentences)][order(legislative_period, -n_sentences)][1:20])

# Save for later JSD work
saveRDS(agg, file.path(getwd(), "Data", "speech_distributions_party_lp.rds"))
cat("\nSaved: Data/speech_distributions_party_lp.rds\n")


# ---- Optional: visual diagnostics ---------------------------------------
# Uncomment to plot.

# # Confidence histogram
 ggplot(meta_dt, aes(pred_score)) +
   geom_histogram(bins = 50, fill = "steelblue", alpha = 0.8) +
   labs(title = "ManifestoBERTa confidence distribution",
        x = "Top-1 softmax probability", y = "Sentence count") +
   theme_minimal()

 # Top labels by share
 ggplot(label_dist[1:20], aes(x = reorder(pred_label, share), y = share)) +
   geom_col(fill = "steelblue") + coord_flip() +
   labs(title = "Top 20 predicted Manifesto codes (sentence-level)",
        x = NULL, y = "Share of all sentences") +
   theme_minimal()
 