# reload_session.R
# ----------------
# Restores the working state of the thesis project after closing RStudio.
# Run this from a fresh R session. After it finishes, the variables and
# tables needed to continue analysis (JSD comparisons, plotting, hypothesis
# testing) are all in scope.
#
# What this loads:
#   - All saved .rds tables (speech & manifesto aggregates, JSD results,
#     bootstrap CIs, manifesto distributions, raw speech aggregate)
#   - A lazy arrow Dataset over the parquet chunks (no full load)
#   - Reference definitions: bucket maps, exclusion rules, distance funcs
#
# What this does NOT do (intentionally — re-run only if needed):
#   - Re-aggregate from parquet (apply_aggregation_AB.R, ~15 min)
#   - Recompute JSD (compute_jsd.R, ~5 min without bootstrap)
#   - Re-pull MARPOR (pull_marpor_align.R, ~30 sec but needs API key)
#
# If any of the .rds files below are missing, the corresponding upstream
# script needs to be re-run. The script will tell you which.

# ---- 0. Working directory & libraries ----------------------------------

setwd("S:/RProj_MSc/MScThesis")

#install.packages(c("arrow","data.table", "dplyr", "ggplot2"))
#install.packages("arrow")
#install.packages("rlang")
library(arrow)
library(data.table)
library(dplyr)
library(ggplot2)


# ---- 1. Load saved tables ----------------------------------------------

required_files <- c(
  speech_classified_parquet = "Data/sentences_classified",
  speech_56d_aggregate      = "Data/speech_distributions_party_lp.rds",
  manifesto_distributions   = "Data/manifesto_distributions.rds",
  agg_A_speech              = "Data/agg_A_speech.rds",
  agg_B_speech              = "Data/agg_B_speech.rds",
  agg_A_manifesto           = "Data/agg_A_manifesto.rds",
  agg_B_manifesto           = "Data/agg_B_manifesto.rds",
  jsd_results               = "Data/jsd_results.rds",
  jsd_permutation           = "Data/jsd_permutation.rds"
)
missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
  cat("WARNING: missing files (corresponding upstream script must be re-run):\n")
  for (m in names(missing)) cat("  ", m, " -> ", missing[m], "\n", sep = "")
  cat("\n")
}

# Lazy parquet dataset over the 598 classified chunks (no full materialization).
classified_dir <- "Data/sentences_classified"
if (dir.exists(classified_dir)) {
  chunk_files <- sort(list.files(classified_dir,
                                  pattern = "^chunk_\\d+\\.parquet$",
                                  full.names = TRUE))
  classified_ds <- arrow::open_dataset(chunk_files, format = "parquet")
  cat("Lazy parquet dataset attached: ",
      length(chunk_files), " chunks, ",
      format(classified_ds$num_rows, big.mark = ","), " sentences.\n", sep = "")
}

# Materialized .rds tables.
load_rds_safely <- function(path) {
  if (file.exists(path)) readRDS(path) else NULL
}
speech_56d_aggregate <- load_rds_safely("Data/speech_distributions_party_lp.rds")
manifesto_distributions <- load_rds_safely("Data/manifesto_distributions.rds")
agg_A_speech    <- load_rds_safely("Data/agg_A_speech.rds")
agg_B_speech    <- load_rds_safely("Data/agg_B_speech.rds")
agg_A_manifesto <- load_rds_safely("Data/agg_A_manifesto.rds")
agg_B_manifesto <- load_rds_safely("Data/agg_B_manifesto.rds")
jsd_results     <- load_rds_safely("Data/jsd_results.rds")
jsd_permutation <- load_rds_safely("Data/jsd_permutation.rds")

cat("\nLoaded tables:\n")
loaded <- list(
  speech_56d_aggregate = speech_56d_aggregate,
  manifesto_distributions = manifesto_distributions,
  agg_A_speech = agg_A_speech, agg_B_speech = agg_B_speech,
  agg_A_manifesto = agg_A_manifesto, agg_B_manifesto = agg_B_manifesto,
  jsd_results = jsd_results, jsd_permutation = jsd_permutation
)
for (n in names(loaded)) {
  v <- loaded[[n]]
  if (is.null(v)) {
    cat("  ", n, ": <missing>\n", sep = "")
  } else {
    cat("  ", n, ": ", nrow(v), " rows x ", ncol(v), " cols\n", sep = "")
  }
}


# ---- 2. Reference definitions ------------------------------------------

# Aggregation A: 9 thematic buckets (broad salience). Partition of all 56
# Handbook 4 codes; no Andere bucket since soft aggregation eliminates the
# need for a residual.
agg_A_def <- list(
  "Foreign Policy & Defence"          = c("101","102","103","104","105","106","107","109"),
  "European Integration"              = c("108","110"),
  "Democracy & Political System"      = c("201","202","203","204","301","302","303","304","305"),
  "Economy"                           = c("401","402","403","404","405","406","407","408",
                                          "409","410","411","412","413","414","415"),
  "Welfare & Social Policy"           = c("502","503","504","505","506","507"),
  "Environment"                       = c("416","501"),
  "Law & Order and National Identity" = c("603","604","605","606"),
  "Migration"                         = c("601","602","607","608"),
  "Social Groups"                     = c("701","702","703","704","705","706")
)
agg_A_buckets <- names(agg_A_def)

# Aggregation B: directional sub-labels within four key domains + Andere
# for codes outside the key domains.
agg_B_def <- list(
  "Economy: Marktliberalismus"      = c("401","402","407","414"),
  "Economy: Staatsintervention"     = c("403","404","405","409","412","413"),
  "Economy: Wirtschaft Allgemein"   = c("408","410","411"),
  "Welfare: Sozialstaat Ausbau"     = c("503","504","506"),
  "Welfare: Sozialstaat Begrenzung" = c("505","507"),
  "Migration: Restriktiv"           = c("601","608"),
  "Migration: Liberal"              = c("602","607"),
  "Europe: Pro-EU"                  = c("108"),
  "Europe: Contra-EU"               = c("110")
)
agg_B_sublabels <- c(names(agg_B_def), "Andere")

# Robustness: softmax temperatures applied during speech-side aggregation.
TEMPERATURES <- c(sharp = 0.5, native = 1.0, flat = 2.0)


# ---- 3. Cell-exclusion rules (used in compute_jsd.R) -------------------

MIN_SENTENCES <- 1000

is_excluded <- function(party, lp) {
  party %in% c("parteilos", "NA", NA_character_) |
    (party == "PDS" & lp %in% 13:15) |
    (party == "FDP" & lp == 18L)
}


# ---- 4. Distance / divergence helpers ----------------------------------

# JSD in bits (log base 2). Bounded in [0, 1].
jsd <- function(p, q, eps = 1e-12) {
  stopifnot(length(p) == length(q))
  p <- p + eps;  p <- p / sum(p)
  q <- q + eps;  q <- q / sum(q)
  m <- 0.5 * (p + q)
  kl_pm <- sum(p * (log2(p) - log2(m)))
  kl_qm <- sum(q * (log2(q) - log2(m)))
  0.5 * (kl_pm + kl_qm)
}

cosine_sim <- function(p, q) {
  sum(p * q) / sqrt(sum(p^2) * sum(q^2))
}

rank_corr <- function(p, q) {
  if (length(p) < 3) return(NA_real_)
  suppressWarnings(cor(p, q, method = "spearman"))
}

# Strip the bare 3-digit code from a "NNN - <Label>" column header.
strip_code <- function(col) sub("^([0-9]{3}).*", "\\1", col)


# ---- 5. Quick-glance summaries -----------------------------------------

if (!is.null(jsd_results)) {
  cat("\n=== Quick-glance: JSD summary (native, M1, Aggregation A) ===\n")
  print(jsd_results[tau_name == "native" & mapping == "M1_entering" &
                      aggregation == "A",
                    .(mean_jsd  = round(mean(jsd), 4),
                      median_jsd = round(median(jsd), 4),
                      n_cells = .N),
                    by = party][order(mean_jsd)])
}

cat("\nSession restored. Ready to continue.\n")
