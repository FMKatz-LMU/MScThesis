# ============================================================================
# 03_load_and_aggregate.R — Build cell-level distributions for both schemes
# ----------------------------------------------------------------------------
# Outputs (cached as RDS in PATHS$cache_dir so subsequent hypothesis scripts
# don't have to re-aggregate the speech matrix):
#
#   speech_dists.rds    list with $A, $B; each is a long tibble:
#                       (party, lp, bucket, share, n_sent)
#   manifesto_dists.rds list with $A, $B; each long tibble:
#                       (party, election_date, bucket, share)
#   cell_diag.rds       per-cell mean argmax probability and predictive entropy
#
# IMPORTANT: this script assumes the manifesto RDS is the MARPOR Main Dataset
# in WIDE format (per101..per706). If your probe shows a different format,
# adapt the 'load_manifesto_wide()' function below.
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- 1. Load speech RDS ----------------------------------------------------
message("[03] Loading speech RDS …")
sp <- readRDS(PATHS$speech_rds)

# Standardize column names we depend on
party_col <- intersect(c("party", "partei", "Partei"), colnames(sp))[1]
lp_col    <- intersect(c("lp", "legislative_period", "legislaturperiode", "LP"), colnames(sp))[1]
stopifnot(!is.na(party_col), !is.na(lp_col))

sp <- sp %>%
  mutate(party = normalize_party(.data[[party_col]]),
         lp    = as.integer(.data[[lp_col]])) %>%
  filter(party %in% PARTIES_KEEP, lp %in% LEGISLATIVE_PERIODS)

# Probability columns (in MARPOR_CODES_56 order)
stopifnot(all(PROB_COLS %in% colnames(sp)))
message(sprintf("[03] Speech sentences after party/LP filter: %s", format(nrow(sp), big.mark = ",")))

# ---- 2. Load manifesto RDS (WIDE format expected) --------------------------
load_manifesto_wide <- function(path) {
  mf <- readRDS(path)
  party_col_mf <- intersect(c("party", "partyname", "partei"), colnames(mf))[1]
  date_col_mf  <- intersect(c("date", "edate", "election_date"), colnames(mf))[1]
  stopifnot(!is.na(party_col_mf), !is.na(date_col_mf))

  mf <- mf %>%
    mutate(party = normalize_party(.data[[party_col_mf]]),
           election_date = as.Date(.data[[date_col_mf]])) %>%
    filter(party %in% PARTIES_KEEP)

  # Restrict to the elections that produced LPs 13..20
  mf <- mf %>% filter(election_date %in% LP_ELECTION$election_date)
  mf
}

message("[03] Loading manifesto RDS …")
mf <- load_manifesto_wide(PATHS$manifesto_rds)
message(sprintf("[03] Manifesto rows after filter: %d", nrow(mf)))

# ---- 3. Manifesto-side aggregation (per (party, election)) -----------------
message("[03] Aggregating manifesto distributions …")
agg_one_manifesto <- function(row, scheme) {
  if (scheme == "A") {
    aggregate_manifesto_row(row, AGG_A, "bucket_A", BUCKETS_A)
  } else if (scheme == "B") {
    # B includes an implicit "Andere" — everything not in AGG_B$code
    full_buckets <- c(BUCKETS_B, "Andere")
    raw <- aggregate_manifesto_row(row, AGG_B, "bucket_B", full_buckets)
    # Compute Andere as 1 - sum of mapped buckets, floored at 0
    mapped_sum <- sum(raw[BUCKETS_B])
    andere_mass <- max(0, 1 - mapped_sum)
    raw["Andere"] <- andere_mass
    raw
  }
}

manifesto_long_A <- mf %>%
  rowwise() %>%
  mutate(dist = list(agg_one_manifesto(cur_data(), "A"))) %>%
  ungroup() %>%
  select(party, election_date, dist) %>%
  unnest_longer(dist, indices_to = "bucket", values_to = "share")

manifesto_long_B <- mf %>%
  rowwise() %>%
  mutate(dist = list(agg_one_manifesto(cur_data(), "B"))) %>%
  ungroup() %>%
  select(party, election_date, dist) %>%
  unnest_longer(dist, indices_to = "bucket", values_to = "share")

# ---- 4. Speech-side aggregation per (party, LP) ----------------------------
message("[03] Aggregating speech distributions …")
prob_mat_full <- as.matrix(sp[, PROB_COLS])
storage.mode(prob_mat_full) <- "double"

cells <- sp %>% transmute(party, lp, .row = row_number())

speech_dists_A <- list()
speech_dists_B <- list()
diag_rows <- list()

for (key in cells %>% distinct(party, lp) %>% transmute(k = paste(party, lp, sep = "|")) %>% pull(k)) {
  parts <- strsplit(key, "|", fixed = TRUE)[[1]]
  pty <- parts[1]; lp_i <- as.integer(parts[2])

  rows <- cells %>% filter(party == pty, lp == lp_i) %>% pull(.row)
  pm <- prob_mat_full[rows, , drop = FALSE]

  vA <- soft_aggregate(pm, AGG_A, "bucket_A", BUCKETS_A)
  full_B_buckets <- c(BUCKETS_B, "Andere")
  vB_raw <- soft_aggregate(pm, AGG_B, "bucket_B", full_B_buckets)
  # Andere = 1 - sum of mapped B buckets
  vB_raw["Andere"] <- max(0, 1 - sum(vB_raw[BUCKETS_B]))

  speech_dists_A[[key]] <- tibble(party = pty, lp = lp_i,
                                  bucket = names(vA), share = unname(vA),
                                  n_sent = length(rows))
  speech_dists_B[[key]] <- tibble(party = pty, lp = lp_i,
                                  bucket = names(vB_raw), share = unname(vB_raw),
                                  n_sent = length(rows))

  # Per-cell diagnostics
  argmax_p <- apply(pm, 1, max)
  ent      <- -rowSums(pm * log2(pmax(pm, .Machine$double.eps)))
  diag_rows[[key]] <- tibble(party = pty, lp = lp_i,
                             n_sent = length(rows),
                             mean_argmax_prob = mean(argmax_p),
                             mean_entropy = mean(ent))
}

speech_long_A <- bind_rows(speech_dists_A)
speech_long_B <- bind_rows(speech_dists_B)
cell_diag     <- bind_rows(diag_rows)

# ---- 5. Save caches --------------------------------------------------------
saveRDS(list(A = speech_long_A,    B = speech_long_B),    file.path(PATHS$cache_dir, "speech_dists.rds"))
saveRDS(list(A = manifesto_long_A, B = manifesto_long_B), file.path(PATHS$cache_dir, "manifesto_dists.rds"))
saveRDS(cell_diag, file.path(PATHS$cache_dir, "cell_diag.rds"))

# Also keep the speech probability matrix + cell index around for bootstrap.
# We save them together so 04_H1.R can reload without re-reading 8 LP-worth of data.
saveRDS(list(prob_mat = prob_mat_full, cells = cells),
        file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))

message("[03] Done. Cached:")
message("  - speech_dists.rds")
message("  - manifesto_dists.rds")
message("  - cell_diag.rds")
message("  - speech_prob_matrix.rds")
