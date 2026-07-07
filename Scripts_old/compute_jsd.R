# compute_jsd.R   (v2 — soft-aggregation inputs)
# -----------------------------------------------
# Computes Jensen-Shannon Divergence between speech-side and manifesto-side
# bucket distributions per (party, legislative_period), under both
# aggregations (A: 9-bucket salience, B: directional within 4 key
# domains + Andere), under both manifesto-LP mappings (M1 entering, M2
# bracketing average), and under the three softmax temperatures
# (sharp / native / flat).
#
# Inputs:
#   Data/agg_A_speech.rds      cols: legislative_period, party, tau_name,
#                                    bucket_A, share, n_sentences
#   Data/agg_B_speech.rds      cols: legislative_period, party, tau_name,
#                                    bucket_B, share, n_sentences
#   Data/agg_A_manifesto.rds   cols: legislative_period, party_label,
#                                    mapping, bucket_A, share
#   Data/agg_B_manifesto.rds   cols: legislative_period, party_label,
#                                    mapping, bucket_B, share
#   Data/sentences_classified/*.parquet   (only for permutation if enabled)
#
# Outputs:
#   Data/jsd_results.rds       long-format JSD/cosine/rank-corr per
#                              (party, LP, mapping, tau, aggregation)
#   Data/jsd_permutation.rds   per-cell bootstrap CIs (Aggregation A,
#                              native temperature only)
#
# Three metrics per cell, per Section 5.2 of the methodology document:
# JSD (bits, log base 2, in [0,1]), cosine similarity, Spearman rank corr.

# ---- 0. Setup -----------------------------------------------------------

library(data.table)
library(arrow)
library(dplyr)

setwd("S:/RProj_MSc/MScThesis")

DO_PERMUTATION  <- TRUE
N_PERMUTATIONS  <- 1000


# ---- Cell-exclusion rules ------------------------------------------------
# All exclusions are documented in one place for the methods chapter.
#
#   1. parteilos / NA parties: independents have no manifesto to compare
#      against.
#   2. PDS in LPs 13-15: MARPOR has no annotated PDS manifesto for the
#      1994/1998/2002 elections (verified in diagnose_marpor_v3.R).
#   3. FDP in LP 18: the FDP failed the 5% threshold in the 2013 federal
#      election and was absent from the 2013-2017 Bundestag. The 320
#      sentences attributed to FDP in this LP are mis-attributions
#      (Bundesrat speakers, invited guests, etc.) and produce a JSD ~2x
#      higher than every other FDP cell with a CI to match.
#   4. Cells with fewer than MIN_SENTENCES speech sentences: too few for
#      a stable bucket-share estimate.

MIN_SENTENCES <- 1000

is_excluded <- function(party, lp) {
  party %in% c("parteilos", "NA", NA_character_) |
  (party == "PDS" & lp %in% 13:15) |
  (party == "FDP" & lp == 18L)
}


# ---- 1. Distance / divergence functions --------------------------------

# JSD in bits (log base 2). Bounded in [0,1] for finite probability
# distributions: 0 = identical, 1 = disjoint support.
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


# ---- 2. JSD computation, per cell --------------------------------------

compute_jsd_table <- function(speech_long, manifesto_long, bucket_col,
                              bucket_order) {
  # Apply explicit exclusion rules BEFORE pivoting.
  speech_long <- speech_long[!is_excluded(party, legislative_period)]

  # Apply MIN_SENTENCES filter (sample-size rule). Speech-long has the
  # n_sentences column attached (it's the same value across all bucket
  # rows for a given (LP, party, tau)).
  speech_long <- speech_long[n_sentences >= MIN_SENTENCES]

  # Pivot speech and manifesto sides to wide format.
  speech_wide <- dcast(speech_long,
                       legislative_period + party + tau_name + n_sentences ~
                         get(bucket_col),
                       value.var = "share", fill = 0)
  manifesto_wide <- dcast(manifesto_long,
                          legislative_period + party_label + mapping ~
                            get(bucket_col),
                          value.var = "share", fill = 0)

  # Make sure both wides have all bucket columns present in bucket_order
  # (dcast omits buckets that never appear, which would break the join).
  for (b in bucket_order) {
    if (!b %in% names(speech_wide))    speech_wide[, (b)    := 0]
    if (!b %in% names(manifesto_wide)) manifesto_wide[, (b) := 0]
  }
  setcolorder(speech_wide,
              c("legislative_period","party","tau_name","n_sentences",
                bucket_order))
  setcolorder(manifesto_wide,
              c("legislative_period","party_label","mapping", bucket_order))

  # CDU/CSU split: manifesto side has CDU_CSU_joint; speech side has
  # CDU and CSU separately. Per prior decision, both speech rows are
  # compared against the joint manifesto.
  m_cdu <- copy(manifesto_wide[party_label == "CDU_CSU_joint"])
  m_csu <- copy(manifesto_wide[party_label == "CDU_CSU_joint"])
  m_cdu[, party_label := "CDU"]
  m_csu[, party_label := "CSU"]
  m_other <- manifesto_wide[party_label != "CDU_CSU_joint"]
  manifesto_wide <- rbindlist(list(m_other, m_cdu, m_csu))
  setnames(manifesto_wide, "party_label", "party")

  # Inner join: drops cells without a manifesto match (e.g. PDS LPs 13-15
  # already excluded; AfD LPs 13-18 have no AfD manifesto in those years
  # except 2013, which is correctly aligned to LP 19 entering).
  joined <- merge(speech_wide, manifesto_wide,
                  by = c("legislative_period", "party"),
                  suffixes = c("_speech", "_manifesto"),
                  allow.cartesian = TRUE)

  speech_cols    <- paste0(bucket_order, "_speech")
  manifesto_cols <- paste0(bucket_order, "_manifesto")

  joined[, c("jsd","cosine","rank_corr") := {
    p <- as.numeric(.SD[, ..speech_cols])
    q <- as.numeric(.SD[, ..manifesto_cols])
    list(jsd(p, q), cosine_sim(p, q), rank_corr(p, q))
  }, by = .(legislative_period, party, tau_name, mapping)]

  joined[, .(legislative_period, party, tau_name, mapping,
             n_speech_sentences = n_sentences,
             jsd, cosine, rank_corr)]
}


# ---- 3. Run JSD for both aggregations ----------------------------------

cat("=== Loading aggregated tables ===\n")
agg_A_speech    <- readRDS("Data/agg_A_speech.rds")
agg_B_speech    <- readRDS("Data/agg_B_speech.rds")
agg_A_manifesto <- readRDS("Data/agg_A_manifesto.rds")
agg_B_manifesto <- readRDS("Data/agg_B_manifesto.rds")

agg_A_buckets <- c(
  "Foreign Policy & Defence", "European Integration",
  "Democracy & Political System", "Economy", "Welfare & Social Policy",
  "Environment", "Law & Order and National Identity", "Migration",
  "Social Groups"
)
agg_B_sublabels <- c(
  "Economy: Marktliberalismus", "Economy: Staatsintervention",
  "Economy: Wirtschaft Allgemein", "Welfare: Sozialstaat Ausbau",
  "Welfare: Sozialstaat Begrenzung", "Migration: Restriktiv",
  "Migration: Liberal", "Europe: Pro-EU", "Europe: Contra-EU", "Andere"
)

cat("\n=== Computing JSD: Aggregation A ===\n")
jsd_A <- compute_jsd_table(agg_A_speech, agg_A_manifesto,
                           bucket_col = "bucket_A",
                           bucket_order = agg_A_buckets)
jsd_A[, aggregation := "A"]

cat("=== Computing JSD: Aggregation B ===\n")
jsd_B <- compute_jsd_table(agg_B_speech, agg_B_manifesto,
                           bucket_col = "bucket_B",
                           bucket_order = agg_B_sublabels)
jsd_B[, aggregation := "B"]

jsd_results <- rbindlist(list(jsd_A, jsd_B), use.names = TRUE)
setkey(jsd_results, aggregation, mapping, tau_name,
       legislative_period, party)

cat("\n=== JSD summary (native temperature, M1 mapping) ===\n")
print(jsd_results[tau_name == "native" & mapping == "M1_entering",
                  .(mean_jsd  = round(mean(jsd), 4),
                    median_jsd = round(median(jsd), 4),
                    n = .N),
                  by = .(aggregation, party)][order(aggregation, mean_jsd)])

cat("\n=== Robustness: JSD by temperature (Aggregation A, M1) ===\n")
print(jsd_results[aggregation == "A" & mapping == "M1_entering",
                  .(mean_jsd = round(mean(jsd), 4)),
                  by = .(party, tau_name)][order(party, tau_name)])

saveRDS(jsd_results, "Data/jsd_results.rds")
cat("\nSaved: Data/jsd_results.rds (", nrow(jsd_results), "rows)\n")


# ---- 4. Permutation/bootstrap CIs for H1a ------------------------------
# Speech-side bootstrap: resample sentences (with replacement) within
# each (LP, party) cell, recompute the soft-aggregated bucket
# distribution, compute JSD against the (fixed) manifesto distribution.
# 1000 draws -> 95% interval on speech-side bucket distribution
# uncertainty.
#
# This is NOT the within-source null distribution the methodology
# document originally specified (which would require manifesto
# quasi-sentence-level data we don't have for SPD post-2002). It IS a
# bootstrap CI on the speech-side estimate, which still informs how
# precisely we know each cell's JSD.

if (DO_PERMUTATION) {

  cat("\n=== Bootstrap CIs (Aggregation A, native temperature) ===\n")
  cat("Loading sentence-level probability data...\n")

  chunk_files <- sort(list.files("Data/sentences_classified",
                                 pattern = "^chunk_\\d+\\.parquet$",
                                 full.names = TRUE))

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
  strip_code <- function(col) sub("^([0-9]{3}).*", "\\1", col)

  # Load lazily, then for each (LP, party) cell we'll need the per-sentence
  # bucket-share matrix. Build it once across all chunks (memory cost:
  # 6M sentences * 9 buckets * 8 bytes ~= 432 MB; tractable).
  cat("Building per-sentence bucket-share matrix (Aggregation A)...\n")

  all_sentences <- vector("list", length(chunk_files))
  for (i in seq_along(chunk_files)) {
    chunk <- as.data.table(arrow::read_parquet(chunk_files[i]))
    prob_cols_chunk <- grep("^[0-9]{3} - ", names(chunk), value = TRUE)
    P <- as.matrix(chunk[, ..prob_cols_chunk])

    A_mat <- sapply(agg_A_def, function(codes) {
      cols <- prob_cols_chunk[strip_code(prob_cols_chunk) %in% codes]
      idx  <- match(cols, prob_cols_chunk)
      if (length(idx) == 1) P[, idx] else rowSums(P[, idx, drop = FALSE])
    })
    all_sentences[[i]] <- data.table(
      legislative_period = chunk$legislative_period,
      party              = chunk$party,
      A_mat              # 9 numeric columns, one per bucket
    )
    if (i %% 100 == 0 || i == length(chunk_files))
      cat(sprintf("  chunk %d/%d\n", i, length(chunk_files)))
  }
  sentences_dt <- rbindlist(all_sentences)
  rm(all_sentences); gc()
  cat("Done. Total rows:", format(nrow(sentences_dt), big.mark = ","), "\n")

  # Manifesto-side reference distributions (M1, native, Aggregation A).
  m_wide <- dcast(agg_A_manifesto[mapping == "M1_entering"],
                   legislative_period + party_label ~ bucket_A,
                   value.var = "share", fill = 0)
  for (b in agg_A_buckets)
    if (!b %in% names(m_wide)) m_wide[, (b) := 0]
  setcolorder(m_wide, c("legislative_period","party_label", agg_A_buckets))

  m_cdu <- copy(m_wide[party_label == "CDU_CSU_joint"])[, party_label := "CDU"]
  m_csu <- copy(m_wide[party_label == "CDU_CSU_joint"])[, party_label := "CSU"]
  m_other <- m_wide[party_label != "CDU_CSU_joint"]
  m_wide <- rbindlist(list(m_other, m_cdu, m_csu))
  setnames(m_wide, "party_label", "party")

  # Iterate over (LP, party) cells.
  cells <- unique(sentences_dt[, .(legislative_period, party)])
  cells <- merge(cells, m_wide[, .(legislative_period, party)],
                 by = c("legislative_period", "party"))
  cells <- cells[!is_excluded(party, legislative_period)]
  cell_sizes <- sentences_dt[, .N, by = .(legislative_period, party)]
  cells <- merge(cells, cell_sizes, by = c("legislative_period", "party"))
  cells <- cells[N >= MIN_SENTENCES]

  cat("Bootstrapping", N_PERMUTATIONS, "draws across",
      nrow(cells), "(LP, party) cells...\n")

  perm_results <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    lp <- cells$legislative_period[i]
    pt <- cells$party[i]
    cell_mat <- as.matrix(sentences_dt[legislative_period == lp & party == pt,
                                        ..agg_A_buckets])
    n <- nrow(cell_mat)
    m_row  <- m_wide[legislative_period == lp & party == pt]
    m_dist <- as.numeric(m_row[, ..agg_A_buckets])

    samples <- numeric(N_PERMUTATIONS)
    for (b in seq_len(N_PERMUTATIONS)) {
      idx <- sample.int(n, size = n, replace = TRUE)
      boot_dist <- colMeans(cell_mat[idx, , drop = FALSE])
      samples[b] <- jsd(boot_dist, m_dist)
    }

    perm_results[[i]] <- data.table(
      legislative_period = lp,
      party              = pt,
      n_speech_sentences = n,
      jsd_lower_95       = quantile(samples, 0.025),
      jsd_upper_95       = quantile(samples, 0.975),
      jsd_mean_boot      = mean(samples)
    )
    if (i %% 5 == 0) cat("  ", i, "/", nrow(cells), " cells done\n", sep = "")
  }
  perm_results <- rbindlist(perm_results)

  saveRDS(perm_results, "Data/jsd_permutation.rds")
  cat("\nSaved: Data/jsd_permutation.rds (", nrow(perm_results), "rows)\n")

  cat("\n=== Bootstrap CI summary ===\n")
  print(perm_results[order(legislative_period, party)])
}

cat("\n=== Done ===\n")
