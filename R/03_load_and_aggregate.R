# ============================================================================
# 03_load_and_aggregate.R — Build cell-level distributions for H1 and H2
# ----------------------------------------------------------------------------
# Reads the pre-aggregated speech and manifesto distributions produced by
# apply_aggregation_AB.R, converts naming conventions, normalizes party labels
# (pooling CDU + CSU into CDU/CSU), and writes four cache RDS files that the
# H1 and H2 scripts read.
#
# Optionally also builds a per-sentence bucket-share matrix from the raw
# parquet chunks for proper sentence-level bootstrap in H1a (slow: reads all
# ~6M sentences). Set BUILD_BUCKET_MATRIX <- TRUE to enable. When FALSE,
# 04_H1.R falls back to jsd_permutation.rds for bootstrap CIs.
#
# Outputs (in PATHS$cache_dir):
#   speech_dists.rds       list($A, $B) — long tibbles (party, lp, bucket, share, n_sent)
#   manifesto_dists.rds    list($A, $B) — long tibbles (party, election_date, lp, bucket, share)
#   cell_diag.rds          per-cell n_sent (entropy/argmax left as NA — parquet not required)
#   speech_prob_matrix.rds list(bucket_mat, cell_meta) or NULLs if BUILD_BUCKET_MATRIX = FALSE
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Flag ------------------------------------------------------------------
# TRUE  → reads all parquet chunks (~5–10 min, high RAM) for proper H1a bootstrap.
# FALSE → uses pre-computed bootstrap CIs from Data/jsd_permutation.rds instead.
BUILD_BUCKET_MATRIX <- FALSE

# ---- Name-mapping tables ---------------------------------------------------

# Manifesto party_label (from pull_marpor_align.R) -> PARTIES_KEEP
MANIFESTO_PARTY_MAP <- c(
  "CDU_CSU_joint" = "CDU/CSU",
  "SPD"           = "SPD",
  "FDP"           = "FDP",
  "GRUENE"        = "Grüne",
  "DIE LINKE"     = "Linke",
  "AfD"           = "AfD"
)

# Aggregation B bucket names used in agg_B_*.rds -> BUCKETS_B used in H1/H2
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

# ============================================================================
# 1. Speech-side distributions
# ============================================================================
message("[03] Loading speech-side aggregated distributions ...")
agg_A_sp_raw <- readRDS(PATHS$agg_A_speech)
agg_B_sp_raw <- readRDS(PATHS$agg_B_speech)

# Normalize party names and pool CDU + CSU weighted by n_sentences.
normalize_and_pool <- function(dt, bucket_col) {
  dt %>%
    filter(tau_name == "native") %>%
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

speech_long_A <- normalize_and_pool(agg_A_sp_raw, "bucket_A")
speech_long_B <- normalize_and_pool(agg_B_sp_raw, "bucket_B") %>%
  mutate(bucket = recode(bucket, !!!BUCKET_B_RENAME))

message(sprintf("[03] Speech A: %d (party, LP) cells",
                nrow(distinct(speech_long_A, party, lp))))
message(sprintf("[03] Speech B: %d (party, LP) cells",
                nrow(distinct(speech_long_B, party, lp))))

# ============================================================================
# 2. Manifesto-side distributions
# ============================================================================
message("[03] Loading manifesto-side aggregated distributions ...")
agg_A_mf_raw <- readRDS(PATHS$agg_A_manifesto)
agg_B_mf_raw <- readRDS(PATHS$agg_B_manifesto)

# Filter to M1, rename parties, add election_date from LP_ELECTION.
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

message(sprintf("[03] Manifesto A: %d (party, election_date) cells",
                nrow(distinct(manifesto_long_A, party, election_date))))

# ============================================================================
# 3. Cell diagnostics (n_sent only; argmax/entropy require parquet)
# ============================================================================
cell_diag <- speech_long_A %>%
  group_by(party, lp) %>%
  summarise(n_sent = first(n_sent),
            mean_argmax_prob = NA_real_,
            mean_entropy     = NA_real_,
            .groups = "drop")

# ============================================================================
# 4. Save core cache files
# ============================================================================
saveRDS(list(A = speech_long_A,    B = speech_long_B),
        file.path(PATHS$cache_dir, "speech_dists.rds"))
saveRDS(list(A = manifesto_long_A, B = manifesto_long_B),
        file.path(PATHS$cache_dir, "manifesto_dists.rds"))
saveRDS(cell_diag,
        file.path(PATHS$cache_dir, "cell_diag.rds"))

message("[03] Saved: speech_dists.rds, manifesto_dists.rds, cell_diag.rds")

# ============================================================================
# 5. Per-sentence bucket matrix (optional, for H1a bootstrap)
# ============================================================================
if (BUILD_BUCKET_MATRIX) {
  message("[03] Building per-sentence bucket-share matrix from parquet ...")
  chunk_files <- sort(list.files(PATHS$parquet_dir,
                                 pattern = "^chunk_\\d+\\.parquet$",
                                 full.names = TRUE))
  if (length(chunk_files) == 0)
    stop("No parquet chunks found in: ", PATHS$parquet_dir)
  message(sprintf("    Found %d parquet chunks", length(chunk_files)))

  # Aggregation A code groups (codes as 3-char strings matching column prefixes)
  agg_A_codes <- list(
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

  all_rows <- vector("list", length(chunk_files))
  for (i in seq_along(chunk_files)) {
    chunk <- arrow::read_parquet(chunk_files[i])
    prob_cols_chunk <- grep("^[0-9]{3} - ", names(chunk), value = TRUE)
    P <- as.matrix(chunk[, prob_cols_chunk])

    bucket_mat_chunk <- sapply(agg_A_codes, function(codes) {
      cols <- prob_cols_chunk[strip_code(prob_cols_chunk) %in% codes]
      idx  <- match(cols, prob_cols_chunk)
      if (length(idx) == 1L) P[, idx] else rowSums(P[, idx, drop = FALSE])
    })

    all_rows[[i]] <- data.frame(
      party = normalize_party(chunk$party),
      lp    = as.integer(chunk$legislative_period),
      bucket_mat_chunk,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (i %% 100L == 0L || i == length(chunk_files))
      message(sprintf("    chunk %d / %d", i, length(chunk_files)))
  }

  combined <- dplyr::bind_rows(all_rows) %>%
    filter(!is.na(party), party %in% PARTIES_KEEP, lp %in% LEGISLATIVE_PERIODS)

  bucket_mat <- as.matrix(combined[, BUCKETS_A])
  cell_meta  <- combined %>% transmute(party, lp, .row = row_number())

  saveRDS(list(bucket_mat = bucket_mat, cell_meta = cell_meta),
          file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))
  message("[03] Saved speech_prob_matrix.rds  (rows: ",
          format(nrow(bucket_mat), big.mark = ","), ")")
} else {
  message("[03] Skipping bucket matrix (BUILD_BUCKET_MATRIX = FALSE).")
  message("    H1a will use jsd_permutation.rds for bootstrap CIs if available.")
  saveRDS(list(bucket_mat = NULL, cell_meta = NULL),
          file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))
}

message("[03] All done.")
