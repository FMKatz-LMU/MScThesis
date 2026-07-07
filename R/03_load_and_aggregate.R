# ============================================================================
# 03_load_and_aggregate.R — Build the multi-specification cell-level cache
# ----------------------------------------------------------------------------
# Produces a single named-list cache keyed by `<spec>__<scheme>`, where
#
#   <spec>  ∈ {soft_tau0.5, soft_tau1.0, soft_tau2.0,
#              hard_thr0.4, hard_thr0.5,
#              tightprefilter}
#   <scheme>∈ {A, B}
#
# 12 entries total. Two backward-compatibility shims `$A` and `$B` point at
# the primary specification (soft_tau1.0).
#
# Sources (current pipeline reality):
#   * Soft τ-sweep aggregations come from `Data/agg_*_speech.rds`, produced
#     upstream by `apply_aggregation_AB.R`. We re-key into the new schema.
#   * Hard-threshold (0.4 / 0.5) and tight-prefilter aggregations are NOT
#     pre-computed; we stream `Data/sentences_classified/chunk_*.parquet`
#     once and accumulate them in this script.
#
# Outputs (in PATHS$cache_dir):
#   speech_dists.rds       — named list, 12 entries + legacy $A / $B shims.
#                            Each element is a long tibble
#                            (party, lp, bucket, share, n_sent).
#   manifesto_dists.rds    — list($A, $B) — long tibbles
#                            (party, election_date, bucket, share)
#   speech_prob_matrix.rds — list(bucket_mat, cell_meta) or NULLs.
#                            Enabled by BUILD_BUCKET_MATRIX flag below.
#   cell_diag.rds          — per-(party, lp) n_sent + diagnostics.
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
})

# ---- Flags -----------------------------------------------------------------
# BUILD_BUCKET_MATRIX = TRUE keeps the per-sentence Aggregation-A bucket-share
# matrix in memory and saves it to speech_prob_matrix.rds for use by H1a's
# proper sentence-level bootstrap. ~432 MB at 6M sentences. Default FALSE;
# H1a falls back to jsd_permutation.rds when this is FALSE.
BUILD_BUCKET_MATRIX <- TRUE

# ---- Lookup tables ---------------------------------------------------------
# Manifesto party_label (from pull_marpor_align.R) -> PARTIES_KEEP
MANIFESTO_PARTY_MAP <- c(
  "CDU_CSU_joint" = "CDU/CSU",
  "SPD"           = "SPD",
  "FDP"           = "FDP",
  "GRUENE"        = "Grüne",
  "DIE LINKE"     = "Linke",
  "AfD"           = "AfD"
)

# agg_B_speech.rds bucket names -> our canonical BUCKETS_B
BUCKET_B_RENAME <- c(
  "Economy: Marktliberalismus"      = "Marktliberalismus",
  "Economy: Staatsintervention"     = "Staatsintervention",
  "Economy: Wirtschaft Allgemein"   = "Wirtschaft Allgemein",
  "Welfare: Sozialstaat Ausbau"     = "Sozialstaat Ausbau",
  "Welfare: Sozialstaat Begrenzung" = "Sozialstaat Begrenzung",
  "Migration: Restriktiv"           = "Migration restriktiv",
  "Migration: Liberal"              = "Migration liberal",
  "Europe: Pro-EU"                  = "Pro-EU",
  "Europe: Contra-EU"               = "Contra-EU",
  "Andere"                          = "Andere"
)

# Output bucket orders (with Andere appended for hard / B variants).
BUCKETS_A_W_ANDERE <- c(BUCKETS_A, "Andere")
BUCKETS_B_W_ANDERE <- c(BUCKETS_B, "Andere")

# ============================================================================
# 1. Soft τ-sweep aggregations from upstream RDS
# ============================================================================
message("[03] Loading speech-side soft aggregations from agg_*_speech.rds ...")
agg_A_sp_raw <- as.data.table(readRDS(PATHS$agg_A_speech))
agg_B_sp_raw <- as.data.table(readRDS(PATHS$agg_B_speech))

# tau_name in agg_*_speech.rds matches names(TEMPERATURES); each tau yields
# its own soft aggregation. We build one tibble per (tau, scheme).
normalize_and_pool_tau <- function(dt, bucket_col, tau_name_) {
  dt %>%
    filter(tau_name == tau_name_) %>%
    mutate(party = normalize_party(party)) %>%
    filter(!is.na(party), party %in% PARTIES_KEEP) %>%
    rename(lp = legislative_period,
           bucket = all_of(bucket_col),
           n_sent = n_sentences) %>%
    group_by(party, lp, bucket) %>%
    summarise(share  = weighted.mean(share, n_sent),
              n_sent = sum(n_sent),
              .groups = "drop")
}

soft_speech_dists <- list()
for (tn in names(TEMPERATURES)) {
  tau <- TEMPERATURES[[tn]]
  key_A <- sprintf("soft_tau%s__A", format(tau, nsmall = 1))
  key_B <- sprintf("soft_tau%s__B", format(tau, nsmall = 1))
  soft_speech_dists[[key_A]] <- normalize_and_pool_tau(agg_A_sp_raw, "bucket_A", tn)
  soft_speech_dists[[key_B]] <- normalize_and_pool_tau(agg_B_sp_raw, "bucket_B", tn) %>%
    mutate(bucket = recode(bucket, !!!BUCKET_B_RENAME))
  message(sprintf("[03]   %s : %d cells   |   %s : %d cells",
                  key_A, nrow(distinct(soft_speech_dists[[key_A]], party, lp)),
                  key_B, nrow(distinct(soft_speech_dists[[key_B]], party, lp))))
}

primary_key_A <- sprintf("soft_tau%s__A", format(TAU_PRIMARY, nsmall = 1))
primary_key_B <- sprintf("soft_tau%s__B", format(TAU_PRIMARY, nsmall = 1))
stopifnot(primary_key_A %in% names(soft_speech_dists))
stopifnot(primary_key_B %in% names(soft_speech_dists))

# ============================================================================
# 2. Manifesto-side aggregations (unchanged shape, both schemes)
# ============================================================================
message("[03] Loading manifesto-side aggregations ...")
agg_A_mf_raw <- readRDS(PATHS$agg_A_manifesto)
agg_B_mf_raw <- readRDS(PATHS$agg_B_manifesto)

process_manifesto <- function(dt, bucket_col) {
  dt %>%
    filter(mapping == "M1_entering") %>%
    mutate(party = recode(party_label, !!!MANIFESTO_PARTY_MAP,
                          .default = NA_character_)) %>%
    filter(!is.na(party), party %in% PARTIES_KEEP) %>%
    rename(lp = legislative_period, bucket = all_of(bucket_col)) %>%
    inner_join(LP_ELECTION, by = "lp") %>%
    select(party, election_date, bucket, share)
}

manifesto_long_A <- process_manifesto(agg_A_mf_raw, "bucket_A")
manifesto_long_B <- process_manifesto(agg_B_mf_raw, "bucket_B") %>%
  mutate(bucket = recode(bucket, !!!BUCKET_B_RENAME))

message(sprintf("[03] Manifesto A: %d cells; Manifesto B: %d cells",
                nrow(distinct(manifesto_long_A, party, election_date)),
                nrow(distinct(manifesto_long_B, party, election_date))))

# ============================================================================
# 3. Hard-threshold and tight-prefilter aggregations from parquet stream
# ----------------------------------------------------------------------------
# One pass over chunk_*.parquet to compute:
#   * hard_thr0.4__A / __B
#   * hard_thr0.5__A / __B
#   * tightprefilter__A / __B   (native tau, sentence-length filter)
# Optionally (BUILD_BUCKET_MATRIX) accumulate per-row Aggregation-A
# bucket-share matrix for the H1a sentence-level bootstrap.
# ============================================================================
chunk_files <- sort(list.files(PATHS$parquet_dir,
                               pattern = "^chunk_\\d+\\.parquet$",
                               full.names = TRUE))
if (length(chunk_files) == 0L)
  stop("No parquet chunks found in: ", PATHS$parquet_dir)
message(sprintf("[03] Streaming %d parquet chunks for hard/tight aggregations ...",
                length(chunk_files)))

# Discover the 56 prob columns from the first chunk's schema.
.first_schema  <- arrow::read_parquet(chunk_files[1], col_select = NULL)
prob_cols      <- grep("^[0-9]{3} - ", names(.first_schema), value = TRUE)
stopifnot(length(prob_cols) == 56L)
prob_codes_int <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
stopifnot(setequal(prob_codes_int, MARPOR_CODES_56))
# Reorder prob_cols to match MARPOR_CODES_56 order (helper expects this).
.col_order     <- match(MARPOR_CODES_56, prob_codes_int)
prob_cols      <- prob_cols[.col_order]
rm(.first_schema, .col_order)

# code -> bucket lookups (NA for unmapped codes).
code_to_A <- setNames(rep(NA_character_, 56L), as.character(MARPOR_CODES_56))
for (k in seq_len(nrow(AGG_A))) {
  code_to_A[as.character(AGG_A$code[k])] <- AGG_A$bucket_A[k]
}
code_to_B <- setNames(rep(NA_character_, 56L), as.character(MARPOR_CODES_56))
for (k in seq_len(nrow(AGG_B))) {
  code_to_B[as.character(AGG_B$code[k])] <- AGG_B$bucket_B[k]
}
# Code-position vector aligned with MARPOR_CODES_56 order (= prob-mat columns).
A_for_col <- code_to_A[as.character(MARPOR_CODES_56)]
B_for_col <- code_to_B[as.character(MARPOR_CODES_56)]

# Soft per-row bucket-share helper: compute n × n_bucket from prob mat.
# Uses precomputed indicator matrices (56 × n_bucket).
soft_indicator <- function(bucket_for_col, all_buckets_with_andere) {
  M <- matrix(0, nrow = 56L, ncol = length(all_buckets_with_andere),
              dimnames = list(NULL, all_buckets_with_andere))
  for (i in seq_len(56L)) {
    bk <- bucket_for_col[i]
    if (is.na(bk)) bk <- "Andere"
    M[i, bk] <- 1
  }
  M
}
indA_soft <- soft_indicator(A_for_col, BUCKETS_A_W_ANDERE)
indB_soft <- soft_indicator(B_for_col, BUCKETS_B_W_ANDERE)

# Per-cell accumulators: env keyed by "lp|party".
# Each entry holds a numeric vector of length n_bucket (for each spec) and a
# count n_sent. We keep them as simple lists keyed by spec.
SPEC_DEFS <- list(
  list(name = "hard_thr0.4__A", scheme = "A", kind = "hard", thr = 0.4,
       buckets = BUCKETS_A_W_ANDERE),
  list(name = "hard_thr0.5__A", scheme = "A", kind = "hard", thr = 0.5,
       buckets = BUCKETS_A_W_ANDERE),
  list(name = "hard_thr0.4__B", scheme = "B", kind = "hard", thr = 0.4,
       buckets = BUCKETS_B_W_ANDERE),
  list(name = "hard_thr0.5__B", scheme = "B", kind = "hard", thr = 0.5,
       buckets = BUCKETS_B_W_ANDERE),
  list(name = "tightprefilter__A", scheme = "A", kind = "tight",
       buckets = BUCKETS_A_W_ANDERE),
  list(name = "tightprefilter__B", scheme = "B", kind = "tight",
       buckets = BUCKETS_B_W_ANDERE)
)

# Each accumulator: env[key] -> list(sum_mass = numeric, n = int).
acc_envs <- setNames(lapply(SPEC_DEFS, function(.) new.env(hash = TRUE)),
                     vapply(SPEC_DEFS, `[[`, "", "name"))
# Per-cell cumulative n_sent (for cell_diag).
n_sent_env <- new.env(hash = TRUE)
# Aggregate diagnostics across the corpus.
mean_argmax_env <- new.env(hash = TRUE)   # sum of pred_score by cell
mean_argmax_n   <- new.env(hash = TRUE)
# Optional: per-row Aggregation-A bucket-share storage.
if (BUILD_BUCKET_MATRIX) {
  bucket_mat_chunks <- vector("list", length(chunk_files))
  cell_meta_chunks  <- vector("list", length(chunk_files))
}

bump_acc <- function(env, key, vec, n) {
  cur <- env[[key]]
  if (is.null(cur)) {
    env[[key]] <- list(sum_mass = vec, n = n)
  } else {
    env[[key]] <- list(sum_mass = cur$sum_mass + vec, n = cur$n + n)
  }
  invisible(NULL)
}

bump_scalar <- function(env, key, x) {
  cur <- env[[key]]
  env[[key]] <- if (is.null(cur)) x else cur + x
  invisible(NULL)
}

t0 <- Sys.time()
for (i in seq_along(chunk_files)) {
  chunk <- as.data.table(arrow::read_parquet(chunk_files[i]))
  # Normalize party, drop rows outside our scope.
  party_norm <- normalize_party(chunk$party)
  keep <- !is.na(party_norm) &
          party_norm %in% PARTIES_KEEP &
          chunk$legislative_period %in% LEGISLATIVE_PERIODS
  if (!any(keep)) next
  chunk <- chunk[keep]
  party_norm <- party_norm[keep]
  P <- as.matrix(chunk[, ..prob_cols])     # n x 56 in MARPOR_CODES_56 order

  # argmax + max-prob (used for hard-threshold variants and pred_score diag).
  argmax_idx  <- max.col(P, ties.method = "first")
  max_prob    <- P[cbind(seq_len(nrow(P)), argmax_idx)]
  argmax_A    <- A_for_col[argmax_idx]     # NA if unmapped
  argmax_B    <- B_for_col[argmax_idx]

  # Per-row word count (computed from text — see TIGHT_FILTER comment).
  n_words <- lengths(strsplit(chunk$sentence, "\\s+"))
  tight_mask <- n_words >= TIGHT_FILTER$min_words_per_sentence

  group_key <- paste(chunk$legislative_period, party_norm, sep = "|")

  # ---- Hard A / B at each threshold --------------------------------------
  for (thr in CONF_THRESHOLDS) {
    # Aggregation A
    assigned_A <- ifelse(!is.na(argmax_A) & max_prob >= thr, argmax_A, "Andere")
    M_A <- matrix(0, nrow = nrow(P), ncol = length(BUCKETS_A_W_ANDERE))
    colnames(M_A) <- BUCKETS_A_W_ANDERE
    M_A[cbind(seq_along(assigned_A), match(assigned_A, BUCKETS_A_W_ANDERE))] <- 1
    grp_A <- rowsum(M_A, group = group_key, reorder = FALSE)
    cnt_A <- as.integer(table(group_key)[rownames(grp_A)])
    spec_name_A <- sprintf("hard_thr%s__A", format(thr, nsmall = 1))
    env_A <- acc_envs[[spec_name_A]]
    for (k in seq_len(nrow(grp_A)))
      bump_acc(env_A, rownames(grp_A)[k], grp_A[k, ], cnt_A[k])

    # Aggregation B
    assigned_B <- ifelse(!is.na(argmax_B) & max_prob >= thr, argmax_B, "Andere")
    M_B <- matrix(0, nrow = nrow(P), ncol = length(BUCKETS_B_W_ANDERE))
    colnames(M_B) <- BUCKETS_B_W_ANDERE
    M_B[cbind(seq_along(assigned_B), match(assigned_B, BUCKETS_B_W_ANDERE))] <- 1
    grp_B <- rowsum(M_B, group = group_key, reorder = FALSE)
    cnt_B <- as.integer(table(group_key)[rownames(grp_B)])
    spec_name_B <- sprintf("hard_thr%s__B", format(thr, nsmall = 1))
    env_B <- acc_envs[[spec_name_B]]
    for (k in seq_len(nrow(grp_B)))
      bump_acc(env_B, rownames(grp_B)[k], grp_B[k, ], cnt_B[k])
  }

  # ---- Tight pre-filter (soft, native τ, length-filtered) ----------------
  if (any(tight_mask)) {
    P_t <- P[tight_mask, , drop = FALSE]
    gk_t <- group_key[tight_mask]
    A_share <- P_t %*% indA_soft       # n_t x 10 (with Andere = 0 for A)
    B_share <- P_t %*% indB_soft       # n_t x 10
    sum_A <- rowsum(A_share, group = gk_t, reorder = FALSE)
    sum_B <- rowsum(B_share, group = gk_t, reorder = FALSE)
    cnt_t <- as.integer(table(gk_t)[rownames(sum_A)])
    env_tA <- acc_envs[["tightprefilter__A"]]
    env_tB <- acc_envs[["tightprefilter__B"]]
    for (k in seq_len(nrow(sum_A))) {
      bump_acc(env_tA, rownames(sum_A)[k], sum_A[k, ], cnt_t[k])
      bump_acc(env_tB, rownames(sum_B)[k], sum_B[k, ], cnt_t[k])
    }
  }

  # ---- n_sent and pred_score diagnostics ---------------------------------
  cnt_chunk <- as.integer(table(group_key))
  for (k in seq_along(cnt_chunk)) {
    key <- names(table(group_key))[k]
    bump_scalar(n_sent_env, key, cnt_chunk[k])
  }
  for (k in seq_along(max_prob)) {
    key <- group_key[k]
    bump_scalar(mean_argmax_env, key, max_prob[k])
    bump_scalar(mean_argmax_n,   key, 1L)
  }

  # ---- Optional per-row bucket matrix (Aggregation A, native τ) ----------
  if (BUILD_BUCKET_MATRIX) {
    A_share_full <- P %*% indA_soft       # n x 10 (Andere col = 0 for A)
    bucket_mat_chunks[[i]] <- A_share_full[, BUCKETS_A, drop = FALSE]
    cell_meta_chunks[[i]]  <- data.frame(
      party = party_norm, lp = chunk$legislative_period,
      stringsAsFactors = FALSE)
  }

  if (i %% 50L == 0L || i == length(chunk_files))
    message(sprintf("[03]   chunk %d / %d  (%.1fs elapsed)",
                    i, length(chunk_files),
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
rm(chunk, P, party_norm); invisible(gc(verbose = FALSE))

# ---- Materialize accumulators into long tibbles ----------------------------
finalize_acc <- function(acc_env, buckets) {
  keys <- ls(acc_env)
  if (!length(keys)) return(tibble(party = character(0), lp = integer(0),
                                   bucket = character(0), share = numeric(0),
                                   n_sent = integer(0)))
  rows <- lapply(keys, function(k) {
    parts <- strsplit(k, "\\|", fixed = FALSE)[[1]]
    cur   <- acc_env[[k]]
    shares <- cur$sum_mass / cur$n
    tibble(party = parts[2], lp = as.integer(parts[1]),
           bucket = buckets, share = shares, n_sent = cur$n)
  })
  bind_rows(rows)
}

new_speech_dists <- list()
for (sd in SPEC_DEFS) {
  tibble_long <- finalize_acc(acc_envs[[sd$name]], sd$buckets)
  # Renormalize to defend against floating drift.
  tibble_long <- tibble_long %>%
    group_by(party, lp) %>%
    mutate(share = if (sum(share) > 0) share / sum(share) else share) %>%
    ungroup()
  new_speech_dists[[sd$name]] <- tibble_long
  message(sprintf("[03]   %-20s : %d cells",
                  sd$name, nrow(distinct(tibble_long, party, lp))))
}

# ============================================================================
# 4. Combine into the canonical named-list cache
# ============================================================================
speech_dists <- c(soft_speech_dists, new_speech_dists)
# Backward-compat shims (spec §3.3.1).
speech_dists$A <- speech_dists[[primary_key_A]]
speech_dists$B <- speech_dists[[primary_key_B]]

# ============================================================================
# 5. Cell diagnostics (n_sent + mean argmax probability)
# ============================================================================
diag_keys <- ls(n_sent_env)
cell_diag <- bind_rows(lapply(diag_keys, function(k) {
  parts <- strsplit(k, "\\|", fixed = FALSE)[[1]]
  tibble(
    lp     = as.integer(parts[1]),
    party  = parts[2],
    n_sent = n_sent_env[[k]],
    mean_argmax_prob = mean_argmax_env[[k]] / mean_argmax_n[[k]],
    mean_entropy     = NA_real_   # entropy not computed here; cheap to add later
  )
})) %>% select(party, lp, n_sent, mean_argmax_prob, mean_entropy) %>%
  arrange(party, lp)

# ============================================================================
# 6. Sanity checks (spec §6 acceptance items)
# ============================================================================
check_row_sums <- function(tib, label) {
  rs <- tib %>% group_by(party, lp) %>%
    summarise(s = sum(share, na.rm = TRUE), .groups = "drop")
  off <- rs %>% filter(abs(s - 1) > 1e-6)
  if (nrow(off) > 0)
    warning(sprintf("[03] Row-sum check FAILED for %s on %d cells (max dev = %.2e)",
                    label, nrow(off), max(abs(off$s - 1))))
}
for (nm in names(speech_dists)) {
  if (nm %in% c("A", "B")) next   # shims, already checked
  check_row_sums(speech_dists[[nm]], nm)
}

# Low-n_sent cells per spec §7.1.
low_n <- cell_diag %>% filter(n_sent < 1000L)
if (nrow(low_n) > 0) {
  message("[03] WARNING: cells with n_sent < 1000 (review for exclusion):")
  print(low_n %>% select(party, lp, n_sent))
}

# ============================================================================
# 7. Save cache
# ============================================================================
saveRDS(speech_dists,
        file.path(PATHS$cache_dir, "speech_dists.rds"))
saveRDS(list(A = manifesto_long_A, B = manifesto_long_B),
        file.path(PATHS$cache_dir, "manifesto_dists.rds"))
saveRDS(cell_diag,
        file.path(PATHS$cache_dir, "cell_diag.rds"))

if (BUILD_BUCKET_MATRIX) {
  bucket_mat <- do.call(rbind, bucket_mat_chunks)
  cell_meta  <- do.call(rbind, cell_meta_chunks)
  cell_meta$.row <- seq_len(nrow(cell_meta))
  saveRDS(list(prob_mat = bucket_mat, cells = cell_meta),
          file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))
  message(sprintf("[03] Saved speech_prob_matrix.rds (%s rows)",
                  format(nrow(bucket_mat), big.mark = ",")))
} else {
  saveRDS(list(bucket_mat = NULL, cell_meta = NULL),
          file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))
  message("[03] BUILD_BUCKET_MATRIX = FALSE — H1a will fall back to jsd_permutation.rds.")
}

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message(sprintf("[03] All done. (%.1f s total — %d named cache entries.)",
                elapsed, length(setdiff(names(speech_dists), c("A", "B")))))
