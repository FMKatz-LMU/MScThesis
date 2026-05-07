# apply_aggregation_AB.R   (v3 — soft aggregation)
# -------------------------------------------------
# Builds (party, legislative_period) bucket-distributions for the
# speech-side and (party, LP, mapping) bucket-distributions for the
# manifesto-side, under two aggregation schemes:
#
#   Aggregation A: 9 thematic buckets (broad salience).
#                  Every one of the 56 MARPOR codes lands in exactly one
#                  bucket. No residual / Andere bucket is needed.
#
#   Aggregation B: directional sub-labels within four key domains
#                  (Economy, Welfare, Migration, Europe), plus an
#                  Andere bucket that collects mass from codes outside
#                  the four key domains.
#
# Aggregation strategy: SOFT.
#   For every sentence we have a 56-d softmax probability vector. The
#   bucket share for sentence i and bucket B is the sum of probabilities
#   on the codes in B. The cell-level distribution is the mean of those
#   per-sentence bucket shares across the cell. No argmax, no confidence
#   threshold -- the model's full output distribution is used.
#
# Both speech and manifesto sides now use exactly the same operation:
# sum soft probability mass per bucket and average. JSD between the two
# is therefore an apples-to-apples comparison.
#
# Robustness: instead of confidence-threshold sweeps (which presuppose
# argmax+threshold), we sweep temperature tau over {0.5, 1.0, 2.0} on
# the speech-side softmax, reporting how much JSD changes. tau=1.0 is
# the model's native output, tau<1 sharpens, tau>1 flattens.
#
# Inputs:
#   Data/sentences_classified/chunk_*.parquet
#   Data/manifesto_distributions.rds
#
# Outputs:
#   Data/agg_A_speech.rds          (party, LP, tau) x 9 buckets
#   Data/agg_B_speech.rds          (party, LP, tau) x 10 buckets (incl. Andere)
#   Data/agg_A_manifesto.rds       (party, LP, mapping) x 9 buckets
#   Data/agg_B_manifesto.rds       (party, LP, mapping) x 10 buckets


# ---- 0. Setup -----------------------------------------------------------

library(arrow)
library(data.table)
library(dplyr)

setwd("C:/masterarbeit")

# Temperatures for robustness. tau=1 is the model's native softmax.
TEMPERATURES <- c(sharp = 0.5, native = 1.0, flat = 2.0)


# ---- 1. Aggregation definitions ----------------------------------------

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

# Verify partition: every code appears in exactly one bucket, all 56 covered.
all_codes_A <- unlist(agg_A_def, use.names = FALSE)
stopifnot(length(all_codes_A) == 56,
          length(unique(all_codes_A)) == 56)

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


# ---- 2. Helpers --------------------------------------------------------

strip_code <- function(col) sub("^([0-9]{3}).*", "\\1", col)

# For a list of (bucket -> codes), return a named list of column-name
# vectors mapping each bucket to its corresponding speech-side prob columns.
codes_to_cols <- function(def_list, all_prob_cols) {
  out <- lapply(def_list, function(codes) {
    all_prob_cols[strip_code(all_prob_cols) %in% codes]
  })
  # Sanity: each code list should match at least one column.
  stopifnot(all(lengths(out) > 0))
  out
}

# Apply softmax temperature scaling to a probability matrix.
# tau < 1 sharpens, tau > 1 flattens. tau = 1 returns input unchanged.
# Implementation: re-derive logits via log(p), divide by tau, re-softmax.
apply_temperature <- function(P, tau, eps = 1e-12) {
  if (abs(tau - 1) < 1e-12) return(P)
  logits <- log(P + eps) / tau
  # Row-wise softmax (numerically stable).
  m <- apply(logits, 1, max)
  exp_l <- exp(logits - m)
  exp_l / rowSums(exp_l)
}


# ---- 3. Load speech-side classified data -------------------------------

cat("=== Loading speech-side classified data ===\n")
chunk_files <- sort(list.files("Data/sentences_classified",
                               pattern = "^chunk_\\d+\\.parquet$",
                               full.names = TRUE))
ds <- arrow::open_dataset(chunk_files, format = "parquet")
cat("Found", length(chunk_files), "chunks,",
    format(ds$num_rows, big.mark = ","), "sentences total.\n")

# Identify the 56 probability columns from the actual schema.
prob_cols <- grep("^[0-9]{3} - ", ds$schema$names, value = TRUE)
stopifnot(length(prob_cols) == 56)

# Build per-bucket column lookups for both aggregations.
A_cols <- codes_to_cols(agg_A_def, prob_cols)   # 9 buckets
B_cols <- codes_to_cols(agg_B_def, prob_cols)   # 9 substantive sub-labels

# Codes outside the four key domains -> manifesto-side and speech-side
# Andere bucket under Aggregation B.
in_domain_codes_B <- unlist(agg_B_def, use.names = FALSE)
B_andere_cols <- prob_cols[!strip_code(prob_cols) %in% in_domain_codes_B]
B_cols[["Andere"]] <- B_andere_cols
stopifnot(sum(lengths(B_cols)) == 56)


# ---- 4. Soft aggregation, speech side ----------------------------------
# For each (legislative_period, party) cell and each temperature tau:
#   1. Collect the cell's full 56-d probability matrix.
#   2. Apply temperature tau to each row.
#   3. For each bucket, sum the columns belonging to that bucket
#      (gives per-sentence bucket share that already sums to 1 across
#      buckets when aggregated over the partition; for Aggregation B this
#      includes Andere as the residual).
#   4. Average across sentences in the cell -> bucket distribution for
#      the cell.
#
# We process in chunks to keep memory bounded. ~6M sentences x 56 floats
# would be ~3 GB in dense doubles; chunked, we never hold more than ~30 MB
# of probabilities in RAM at once.

cat("\n=== Building speech-side bucket distributions (soft, 3 temperatures) ===\n")

# We accumulate, per (LP, party, tau), a numeric vector of bucket-summed
# probability mass and a sentence count. After processing all chunks we
# divide by the count to get the cell distribution.
init_acc <- function(buckets) {
  list(sum_mass = numeric(length(buckets)), n = 0L,
       buckets  = buckets)
}

# Two-level lookup: acc_A[[paste0(lp, "|", party, "|", tau_name)]]
acc_A <- new.env(hash = TRUE)
acc_B <- new.env(hash = TRUE)

key_of <- function(lp, party, tau_name) paste(lp, party, tau_name, sep = "|")

cells_seen <- new.env(hash = TRUE)

for (i in seq_along(chunk_files)) {
  chunk <- as.data.table(arrow::read_parquet(chunk_files[i]))

  # Probability matrix for this chunk.
  P <- as.matrix(chunk[, ..prob_cols])

  for (tau_name in names(TEMPERATURES)) {
    tau <- TEMPERATURES[tau_name]
    P_tau <- if (tau == 1) P else apply_temperature(P, tau)

    # Per-sentence bucket shares for Aggregation A.
    A_mat <- sapply(A_cols, function(cols) {
      idx <- match(cols, prob_cols)
      if (length(idx) == 1) P_tau[, idx] else rowSums(P_tau[, idx, drop = FALSE])
    })
    # Per-sentence bucket shares for Aggregation B.
    B_mat <- sapply(B_cols, function(cols) {
      idx <- match(cols, prob_cols)
      if (length(idx) == 1) P_tau[, idx] else rowSums(P_tau[, idx, drop = FALSE])
    })

    # Group-sum within (LP, party).
    grp <- chunk[, .(legislative_period, party)]
    A_sums <- rowsum(A_mat, group = paste(grp$legislative_period, grp$party, sep = "|"))
    B_sums <- rowsum(B_mat, group = paste(grp$legislative_period, grp$party, sep = "|"))
    counts <- table(paste(grp$legislative_period, grp$party, sep = "|"))

    # Accumulate.
    for (k in rownames(A_sums)) {
      key <- paste(k, tau_name, sep = "|")
      cells_seen[[key]] <- TRUE

      cur <- acc_A[[key]]
      if (is.null(cur)) cur <- init_acc(agg_A_buckets)
      cur$sum_mass <- cur$sum_mass + A_sums[k, ]
      cur$n <- cur$n + counts[[k]]
      acc_A[[key]] <- cur

      cur <- acc_B[[key]]
      if (is.null(cur)) cur <- init_acc(agg_B_sublabels)
      cur$sum_mass <- cur$sum_mass + B_sums[k, ]
      cur$n <- cur$n + counts[[k]]
      acc_B[[key]] <- cur
    }
  }

  if (i %% 50 == 0 || i == length(chunk_files)) {
    cat(sprintf("  chunk %d/%d done\n", i, length(chunk_files)))
  }
}


# Materialize accumulators into long data.tables.
finalize_acc <- function(acc_env, bucket_col_name) {
  keys <- ls(acc_env)
  rows <- lapply(keys, function(key) {
    parts <- strsplit(key, "\\|")[[1]]
    cur   <- acc_env[[key]]
    data.table(
      legislative_period = as.integer(parts[1]),
      party              = parts[2],
      tau_name           = parts[3],
      bucket             = cur$buckets,
      share              = cur$sum_mass / cur$n,
      n_sentences        = cur$n
    )
  })
  out <- rbindlist(rows)
  setnames(out, "bucket", bucket_col_name)
  out
}

agg_A_speech <- finalize_acc(acc_A, "bucket_A")
agg_B_speech <- finalize_acc(acc_B, "bucket_B")

# Renormalize so each (LP, party, tau) row's bucket shares sum to exactly 1.
# Drift from rowsum() accumulating millions of float32 values is ~1e-9; this
# step removes that drift so downstream code sees clean unit-sum vectors.
normalize_shares <- function(dt, group_cols) {
  totals <- dt[, .(total = sum(share)), by = group_cols]
  dt <- merge(dt, totals, by = group_cols)
  dt[, share := share / total]
  dt[, total := NULL]
  dt[]
}
agg_A_speech <- normalize_shares(agg_A_speech,
                                  c("legislative_period","party","tau_name"))
agg_B_speech <- normalize_shares(agg_B_speech,
                                  c("legislative_period","party","tau_name"))


# Sanity: bucket shares per (LP, party, tau) sum to 1. Tolerance 1e-6
# accommodates residual float arithmetic noise; 1e-9 was too tight.
stopifnot(all(abs(agg_A_speech[, sum(share),
                              by = .(legislative_period, party, tau_name)]$V1 - 1)
              < 1e-6))
stopifnot(all(abs(agg_B_speech[, sum(share),
                              by = .(legislative_period, party, tau_name)]$V1 - 1)
              < 1e-6))

cat("\n=== Speech-side Aggregation A (native temperature) ===\n")
cat("Mean bucket share across all (party, LP) cells:\n")
print(agg_A_speech[tau_name == "native",
                   .(mean_share = round(mean(share), 4)),
                   by = bucket_A][order(-mean_share)])

cat("\n=== Speech-side Aggregation B (native temperature) ===\n")
print(agg_B_speech[tau_name == "native",
                   .(mean_share = round(mean(share), 4)),
                   by = bucket_B][order(-mean_share)])

cat("\nSpeech-side Andere share across (party, LP) cells (B, native):\n")
print(agg_B_speech[tau_name == "native" & bucket_B == "Andere",
                   .(legislative_period, party, share = round(share, 4))][
                   order(legislative_period, -share)][1:15])


# ---- 5. Soft aggregation, manifesto side -------------------------------
# Manifesto side already has soft probability vectors (per-category
# percentages). We sum within buckets and the result is already on the
# same operational footing as the speech side.

cat("\n=== Loading and aggregating manifesto side ===\n")
manifesto_dt <- readRDS("Data/manifesto_distributions.rds")
manifesto_prob_cols <- grep("^[0-9]{3} - ", names(manifesto_dt), value = TRUE)
stopifnot(length(manifesto_prob_cols) == 56)
stopifnot(setequal(manifesto_prob_cols, prob_cols))   # column-name parity

aggregate_manifesto <- function(dt, bucket_def, bucket_order, bucket_col_name) {
  out_rows <- list()
  for (b in names(bucket_def)) {
    codes <- bucket_def[[b]]
    cols  <- manifesto_prob_cols[strip_code(manifesto_prob_cols) %in% codes]
    if (length(cols) == 1L) {
      out_rows[[b]] <- dt[, .(legislative_period, party_label, mapping,
                              bucket = b,
                              share  = .SD[[1]]),
                          .SDcols = cols]
    } else {
      out_rows[[b]] <- dt[, .(legislative_period, party_label, mapping,
                              bucket = b,
                              share  = rowSums(.SD)),
                          .SDcols = cols]
    }
  }
  # Add Andere if it's part of the bucket_order but not in bucket_def.
  if ("Andere" %in% bucket_order && !"Andere" %in% names(bucket_def)) {
    in_domain <- manifesto_prob_cols[strip_code(manifesto_prob_cols) %in%
                                     unlist(bucket_def, use.names = FALSE)]
    out_of_domain <- setdiff(manifesto_prob_cols, in_domain)
    out_rows[["Andere"]] <- dt[, .(legislative_period, party_label, mapping,
                                    bucket = "Andere",
                                    share  = rowSums(.SD)),
                                .SDcols = out_of_domain]
  }
  out <- rbindlist(out_rows)
  setnames(out, "bucket", bucket_col_name)
  out
}

agg_A_manifesto <- aggregate_manifesto(manifesto_dt, agg_A_def,
                                       agg_A_buckets, "bucket_A")
agg_B_manifesto <- aggregate_manifesto(manifesto_dt, agg_B_def,
                                       agg_B_sublabels, "bucket_B")

stopifnot(all(abs(agg_A_manifesto[, sum(share),
                                   by = .(legislative_period, party_label, mapping)]$V1 - 1)
              < 1e-9))
stopifnot(all(abs(agg_B_manifesto[, sum(share),
                                   by = .(legislative_period, party_label, mapping)]$V1 - 1)
              < 1e-9))

cat("\n=== Manifesto-side Aggregation A bucket shares (averaged) ===\n")
print(agg_A_manifesto[, .(mean_share = round(mean(share), 4)),
                       by = bucket_A][order(-mean_share)])

cat("\n=== Manifesto-side Aggregation B bucket shares (averaged) ===\n")
print(agg_B_manifesto[, .(mean_share = round(mean(share), 4)),
                       by = bucket_B][order(-mean_share)])


# ---- 6. Save -----------------------------------------------------------

saveRDS(agg_A_speech,    "Data/agg_A_speech.rds")
saveRDS(agg_B_speech,    "Data/agg_B_speech.rds")
saveRDS(agg_A_manifesto, "Data/agg_A_manifesto.rds")
saveRDS(agg_B_manifesto, "Data/agg_B_manifesto.rds")

cat("\nSaved:\n",
    "  Data/agg_A_speech.rds     (rows: ", nrow(agg_A_speech),    ", buckets: ",
                                            length(agg_A_buckets),    ")\n",
    "  Data/agg_B_speech.rds     (rows: ", nrow(agg_B_speech),    ", buckets: ",
                                            length(agg_B_sublabels), ")\n",
    "  Data/agg_A_manifesto.rds  (rows: ", nrow(agg_A_manifesto), ")\n",
    "  Data/agg_B_manifesto.rds  (rows: ", nrow(agg_B_manifesto), ")\n",
    sep = "")
