# ============================================================================
# 11_dps_code_decomposition.R — Per-code decomposition within DPS
# ----------------------------------------------------------------------------
# QUESTION: Of the 9 MARPOR codes that constitute the Democracy & Political
# System bucket (201, 202, 203, 204, 301, 302, 303, 304, 305), is code 305
# (Political Authority) responsible for substantially more of the bucket's
# JSD contribution than the other 8 codes combined?
#
# Why this matters: H2a identifies DPS as the dominant JSD contributor (mean
# 0.0375 per cell, ~51% of total JSD per cell). The pre-filter robustness
# check (script 10) classifies this as an intrinsic cross-domain artefact.
# H2a's substantive diagnosis names code 305 specifically as the procedural-
# language sink. If 305 dominates within DPS, a 305-only exclusion is the
# surgical fix; if mass is spread across 201–305, a bucket-level exclusion
# is the cleaner write-up choice.
#
# METHOD:
# 1. Build per-(party, LP) cell speech distributions over a 17-component
#    scheme: 8 non-DPS Aggregation A buckets + 9 individual DPS codes (201,
#    202, 203, 204, 301, 302, 303, 304, 305) treated as their own pseudo-
#    buckets. The 17 components partition the 56 MARPOR codes, so the cell
#    distribution sums to 1 and the total JSD equals H1a.
# 2. Build the same per-cell distribution on the manifesto side, reading the
#    per-code shares directly from Data/manifesto_distributions.rds
#    (mapping = "M1_entering", which already carries legislative_period).
# 3. Compute per-cell JSD on the 17-component distribution and decompose
#    via per_bucket_jsd() (sum of contributions == total JSD).
# 4. Within DPS, compare contribution of 305 against the sum of 201–304.
#
# OUTPUTS:
#   results/empirics/H2a_dps_code_decomposition_long.csv
#       per (party, LP, dps_code) JSD contribution
#   results/empirics/H2a_dps_code_decomposition_summary.csv
#       per-code mean, sd, share within DPS, across cells
#   results/empirics/H2a_dps_305_vs_rest.csv
#       per-cell: total JSD, DPS contrib, 305 contrib, rest-of-DPS contrib,
#                 305 share of DPS, 305 share of total JSD
#   results/empirics/figures/H2a_dps_code_decomposition_bar.{pdf,png}
#       horizontal bar of mean per-code contribution, 305 highlighted
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
})

# ---- Configuration ---------------------------------------------------------

# The 9 MARPOR codes that constitute the DPS bucket in Aggregation A.
DPS_CODES <- c("201", "202", "203", "204", "301", "302", "303", "304", "305")

# Human-readable labels for the DPS codes (MARPOR Handbook 4).
DPS_CODE_LABELS <- c(
  "201" = "Freedom and Human Rights",
  "202" = "Democracy",
  "203" = "Constitutionalism: Positive",
  "204" = "Constitutionalism: Negative",
  "301" = "Decentralisation",
  "302" = "Centralisation",
  "303" = "Governmental and Administrative Efficiency",
  "304" = "Political Corruption",
  "305" = "Political Authority"
)
stopifnot(setequal(names(DPS_CODE_LABELS), DPS_CODES))

# The 8 non-DPS Aggregation A buckets (kept as bucket-level entries).
NON_DPS_BUCKETS_A <- setdiff(BUCKETS_A, "Democracy & Political System")
stopifnot(length(NON_DPS_BUCKETS_A) == 8L)

# 17-component scheme: 8 non-DPS buckets + 9 individual DPS codes.
SCHEME_17 <- c(NON_DPS_BUCKETS_A, paste0("dps_", DPS_CODES))

# Code-to-component mapping: each of the 56 MARPOR codes lands in exactly one
# of the 17 components. DPS codes get their own pseudo-bucket; everything
# else uses the standard Aggregation A bucket assignment.
code_to_component <- function(code) {
  ccode <- as.character(code)
  if (ccode %in% DPS_CODES) return(paste0("dps_", ccode))
  bk <- AGG_A$bucket_A[match(as.integer(ccode), AGG_A$code)]
  if (is.na(bk)) return(NA_character_)
  bk
}

# Speech-side accumulator cache (skips the ~70s parquet stream on re-runs).
SPEECH_CACHE <- file.path(PATHS$cache_dir, "speech_dps_17.rds")

# ---- 1. Speech side: aggregate parquet stream into the 17-component scheme

if (file.exists(SPEECH_CACHE)) {
  message("[11] Loading cached speech-side 17-component distribution from ",
          SPEECH_CACHE)
  speech_17 <- readRDS(SPEECH_CACHE)
} else {
  message("[11] Streaming parquet for 17-component speech-side distribution ...")

  chunk_files <- sort(list.files(PATHS$parquet_dir,
                                 pattern = "^chunk_\\d+\\.parquet$",
                                 full.names = TRUE))
  if (length(chunk_files) == 0L)
    stop("No parquet chunks found in: ", PATHS$parquet_dir)

  # Discover the 56 prob columns and reorder to MARPOR_CODES_56.
  .first_schema <- arrow::read_parquet(chunk_files[1], col_select = NULL)
  prob_cols     <- grep("^[0-9]{3} - ", names(.first_schema), value = TRUE)
  stopifnot(length(prob_cols) == 56L)
  prob_codes_int <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
  stopifnot(setequal(prob_codes_int, MARPOR_CODES_56))
  .col_order <- match(MARPOR_CODES_56, prob_codes_int)
  prob_cols  <- prob_cols[.col_order]
  rm(.first_schema, .col_order)

  # Code-to-component vector aligned with the prob-matrix column order.
  component_for_col <- vapply(MARPOR_CODES_56, code_to_component, "")
  stopifnot(!anyNA(component_for_col),
            all(component_for_col %in% SCHEME_17))

  # Indicator matrix (56 × 17) — each row of the prob matrix gets multiplied
  # by this to produce its 17-component shares. Each MARPOR code maps to
  # exactly one component, so this is a partition.
  ind17 <- matrix(0, nrow = 56L, ncol = length(SCHEME_17),
                  dimnames = list(NULL, SCHEME_17))
  for (i in seq_len(56L)) ind17[i, component_for_col[i]] <- 1
  stopifnot(all(rowSums(ind17) == 1L))

  # Per-cell accumulator: env keyed by "lp|party" -> list(sum_mass, n_sent).
  acc <- new.env(hash = TRUE)
  bump <- function(key, vec, n) {
    cur <- acc[[key]]
    if (is.null(cur)) {
      acc[[key]] <- list(sum_mass = vec, n = n)
    } else {
      acc[[key]] <- list(sum_mass = cur$sum_mass + vec, n = cur$n + n)
    }
  }

  t0 <- Sys.time()
  for (i in seq_along(chunk_files)) {
    chunk <- as.data.table(arrow::read_parquet(chunk_files[i]))
    party_norm <- normalize_party(chunk$party)
    keep <- !is.na(party_norm) &
            party_norm %in% PARTIES_KEEP &
            chunk$legislative_period %in% LEGISLATIVE_PERIODS
    if (!any(keep)) next
    chunk <- chunk[keep]
    party_norm <- party_norm[keep]

    P <- as.matrix(chunk[, ..prob_cols])   # n × 56 in MARPOR_CODES_56 order
    shares_17 <- P %*% ind17               # n × 17, each row sums to 1

    cells <- data.table(lp = chunk$legislative_period, party = party_norm)
    cells[, row_id := .I]
    for (cell_idx in split(cells$row_id,
                           list(cells$lp, cells$party), drop = TRUE)) {
      lp_i    <- cells$lp[cell_idx[1]]
      party_i <- cells$party[cell_idx[1]]
      key     <- paste(lp_i, party_i, sep = "|")
      bump(key, colSums(shares_17[cell_idx, , drop = FALSE]), length(cell_idx))
    }

    if (i %% 25L == 0L || i == length(chunk_files))
      message(sprintf("[11] chunk %d / %d (%.1fs elapsed)",
                      i, length(chunk_files),
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  # Flatten accumulator -> tibble.
  speech_17 <- bind_rows(lapply(ls(acc), function(key) {
    parts  <- strsplit(key, "|", fixed = TRUE)[[1]]
    entry  <- acc[[key]]
    shares <- entry$sum_mass / entry$n
    tibble(lp        = as.integer(parts[1]),
           party     = parts[2],
           n_sent    = entry$n,
           component = SCHEME_17,
           share     = unname(shares))
  }))

  # Drop low-N cells (consistent with N_SENT_MIN; FDP LP 18 etc.).
  cells_keep <- speech_17 %>%
    distinct(party, lp, n_sent) %>%
    filter(n_sent >= N_SENT_MIN)
  speech_17 <- speech_17 %>% semi_join(cells_keep, by = c("party", "lp"))

  # Sanity: each (party, lp) cell's 17 components sum to ~1.
  cell_sums <- speech_17 %>%
    group_by(party, lp) %>%
    summarise(s = sum(share), .groups = "drop")
  stopifnot(all(abs(cell_sums$s - 1) < 1e-6))
  message(sprintf("[11] Speech-side: %d cells, all row-sums in [1-1e-6, 1+1e-6].",
                  nrow(cell_sums)))

  saveRDS(speech_17, SPEECH_CACHE)
  message("[11] Cached speech-side aggregation to ", SPEECH_CACHE)
}

# ---- 2. Manifesto side: 17-component aggregation from MARPOR Main ----------

message("[11] Aggregating manifesto-side per-code shares ...")

manifesto_raw_path <- file.path(PROJECT_ROOT, "Data/manifesto_distributions.rds")
if (!file.exists(manifesto_raw_path))
  stop("Missing: ", manifesto_raw_path, " (produced by pull_marpor_align.R)")
manifesto_raw <- as.data.table(readRDS(manifesto_raw_path))

# manifesto_distributions.rds carries legislative_period, party_label, mapping,
# n_manifestos, and the 56 prob columns. No `date` column — that's why an
# earlier draft of this script failed: data.table's by = .(date, ...) was
# resolving `date` to base::date() instead of finding a column.
manifesto_prob_cols <- grep("^[0-9]{3} - ", names(manifesto_raw), value = TRUE)
stopifnot(length(manifesto_prob_cols) == 56L)

# Restrict to M1 (primary mapping) and the parties / LPs of interest.
manifesto_m1 <- manifesto_raw[mapping == "M1_entering" &
                              party_label %in% PARTIES_KEEP &
                              legislative_period %in% LEGISLATIVE_PERIODS]
stopifnot(nrow(manifesto_m1) > 0L)

# Long form: one row per (party, LP, code) with share in [0, 1].
manifesto_long <- melt(manifesto_m1,
                       id.vars       = c("legislative_period", "party_label"),
                       measure.vars  = manifesto_prob_cols,
                       variable.name = "prob_col",
                       value.name    = "share_code")
manifesto_long[, code := sub("^([0-9]{3}).*", "\\1", as.character(prob_col))]
manifesto_long[, component := vapply(code, code_to_component, "")]
stopifnot(!anyNA(manifesto_long$component))

manifesto_17_lp <- manifesto_long[, .(share_manifesto = sum(share_code)),
                                   by = c("legislative_period", "party_label",
                                          "component")]
setnames(manifesto_17_lp,
         old = c("legislative_period", "party_label"),
         new = c("lp", "party"))
manifesto_17_lp <- as_tibble(manifesto_17_lp)

# Sanity: each (party, lp) manifesto's 17 components sum to ~1.
ms <- manifesto_17_lp %>%
  group_by(party, lp) %>%
  summarise(s = sum(share_manifesto), .groups = "drop")
stopifnot(all(abs(ms$s - 1) < 1e-6))
message(sprintf("[11] Manifesto-side: %d cells, all row-sums in [1-1e-6, 1+1e-6].",
                nrow(ms)))

# ---- 3. Join and compute per-cell JSD + per-component contributions --------

message("[11] Computing per-cell JSD and per-component contributions ...")

joined <- speech_17 %>%
  rename(share_speech = share) %>%
  select(party, lp, component, share_speech) %>%
  inner_join(manifesto_17_lp, by = c("party", "lp", "component"))

# Verify the join didn't drop components for any cell.
comp_counts <- joined %>%
  group_by(party, lp) %>%
  summarise(n_components = n(), .groups = "drop")
stopifnot(all(comp_counts$n_components == length(SCHEME_17)))

# Per-cell wide vectors for jsd() and per_bucket_jsd().
contrib_long <- joined %>%
  group_by(party, lp) %>%
  group_modify(~ {
    p <- setNames(.x$share_speech,    .x$component)
    q <- setNames(.x$share_manifesto, .x$component)
    contribs <- per_bucket_jsd(p, q)
    tibble(component   = names(contribs),
           jsd_contrib = unname(contribs))
  }) %>%
  ungroup()

# Sanity: sum of contributions == jsd(p, q) per cell.
sanity <- joined %>%
  group_by(party, lp) %>%
  summarise(total_jsd = jsd(share_speech, share_manifesto), .groups = "drop") %>%
  inner_join(
    contrib_long %>%
      group_by(party, lp) %>%
      summarise(s = sum(jsd_contrib), .groups = "drop"),
    by = c("party", "lp")
  ) %>%
  mutate(dev = abs(total_jsd - s))
stopifnot(all(sanity$dev < 1e-9))
message(sprintf("[11] Per-cell decomposition sanity OK (max |total - sum_contrib| = %.2e).",
                max(sanity$dev)))

# ---- 4. DPS-internal summary tables ----------------------------------------

# Long table: per (party, lp, dps_code) contribution.
dps_long <- contrib_long %>%
  filter(grepl("^dps_", component)) %>%
  mutate(dps_code       = sub("^dps_", "", component),
         dps_code_label = unname(DPS_CODE_LABELS[dps_code])) %>%
  select(party, lp, dps_code, dps_code_label, jsd_contrib)

write_csv(dps_long,
          file.path(PATHS$out_dir, "H2a_dps_code_decomposition_long.csv"))

# Summary: per code, mean / sd / share-within-DPS across cells.
dps_summary <- dps_long %>%
  group_by(dps_code, dps_code_label) %>%
  summarise(mean_contrib   = mean(jsd_contrib),
            median_contrib = median(jsd_contrib),
            sd_contrib     = sd(jsd_contrib),
            n_cells        = n(),
            .groups = "drop") %>%
  mutate(share_within_dps_mean = mean_contrib / sum(mean_contrib)) %>%
  arrange(desc(mean_contrib))

write_csv(dps_summary,
          file.path(PATHS$out_dir, "H2a_dps_code_decomposition_summary.csv"))

# Per-cell 305-vs-rest table.
per_cell_305 <- dps_long %>%
  mutate(is_305 = dps_code == "305") %>%
  group_by(party, lp, is_305) %>%
  summarise(contrib = sum(jsd_contrib), .groups = "drop") %>%
  pivot_wider(names_from = is_305, values_from = contrib,
              names_prefix = "is_305_") %>%
  rename(contrib_305      = is_305_TRUE,
         contrib_rest_dps = is_305_FALSE) %>%
  inner_join(sanity %>% select(party, lp, total_jsd),
             by = c("party", "lp")) %>%
  mutate(contrib_dps      = contrib_305 + contrib_rest_dps,
         share_305_in_dps = contrib_305 / contrib_dps,
         share_305_in_jsd = contrib_305 / total_jsd,
         share_dps_in_jsd = contrib_dps / total_jsd) %>%
  select(party, lp, total_jsd,
         contrib_dps, contrib_305, contrib_rest_dps,
         share_dps_in_jsd, share_305_in_dps, share_305_in_jsd) %>%
  arrange(party, lp)

write_csv(per_cell_305,
          file.path(PATHS$out_dir, "H2a_dps_305_vs_rest.csv"))

# ---- 5. Cross-check against H2a_per_bucket_summary.csv ---------------------

h2a_check_path <- file.path(PATHS$out_dir, "H2a_per_bucket_summary.csv")
if (file.exists(h2a_check_path)) {
  h2a_check     <- read_csv(h2a_check_path, show_col_types = FALSE)
  dps_total_here <- sum(dps_summary$mean_contrib)
  dps_in_h2a     <- h2a_check %>%
    filter(bucket == "Democracy & Political System") %>%
    pull(mean_contrib)
  message(sprintf(
    "[11] Cross-check: sum of per-code DPS contributions = %.5f; H2a bucket-level DPS = %.5f; diff = %.2e",
    dps_total_here, dps_in_h2a, abs(dps_total_here - dps_in_h2a)))
}

# ---- 6. Plot --------------------------------------------------------------

p_dps <- dps_summary %>%
  mutate(label  = paste0(dps_code, " — ", dps_code_label),
         label  = fct_reorder(label, mean_contrib),
         is_305 = dps_code == "305") %>%
  ggplot(aes(x = mean_contrib, y = label, fill = is_305)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.4f", mean_contrib)),
            hjust = -0.15, size = 3) +
  scale_fill_manual(values = c(`FALSE` = "#264653", `TRUE` = "#E76F51"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title    = "Per-code JSD contribution within Democracy & Political System",
       subtitle = paste0(
         "Mean across (party, LP) cells. Code 305 (Political Authority) highlighted.\n",
         "Substantive question: does the H2a bucket-level DPS effect concentrate on 305?"),
       x = "Mean JSD contribution per cell",
       y = NULL,
       caption  = "Source: 17-component decomposition (8 non-DPS buckets + 9 DPS codes).") +
  theme_minimal(base_size = 11)

ggsave(file.path(PATHS$fig_dir, "H2a_dps_code_decomposition_bar.pdf"),
       p_dps, width = 9, height = 5.5)
ggsave(file.path(PATHS$fig_dir, "H2a_dps_code_decomposition_bar.png"),
       p_dps, width = 9, height = 5.5, dpi = 200, bg = "white")

# ---- 7. Console summary ----------------------------------------------------

cat("\n==================== DPS PER-CODE DECOMPOSITION ====================\n")
print(dps_summary %>%
        mutate(across(c(mean_contrib, median_contrib, sd_contrib,
                        share_within_dps_mean),
                      ~ signif(., 4))))

cat("\n----- 305 share of DPS, summary across cells -----\n")
print(per_cell_305 %>%
        summarise(mean_share_305_in_dps   = mean(share_305_in_dps),
                  median_share_305_in_dps = median(share_305_in_dps),
                  min_share_305_in_dps    = min(share_305_in_dps),
                  max_share_305_in_dps    = max(share_305_in_dps),
                  mean_share_305_in_jsd   = mean(share_305_in_jsd),
                  mean_share_dps_in_jsd   = mean(share_dps_in_jsd),
                  n_cells                 = n()) %>%
        mutate(across(where(is.numeric), ~ signif(., 4))))

cat("\n----- Pre-committed reading rule (write before looking) -----\n",
    "If 305 carries > 80% of within-DPS contribution: surgical 305-only\n",
    "exclusion is the clean H1a-extension move; report it as the primary\n",
    "8-of-9-DPS-plus-others spec, with bucket-level exclusion as a\n",
    "robustness check.\n\n",
    "If 305 carries 50%–80%: bucket-level exclusion (8-bucket H1a) is\n",
    "the parsimonious choice for the write-up; note the 305-concentration\n",
    "qualitatively.\n\n",
    "If 305 carries < 50%: the artefact is genuinely bucket-wide, not\n",
    "305-specific; H2a's substantive diagnosis (single code) needs\n",
    "softening to 'the DPS bucket as a whole reflects procedural-genre\n",
    "spillover across multiple codes'.\n", sep = "")

message("\n[11] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H2a_dps_code_decomposition_long.csv"))
message("  - ", file.path(PATHS$out_dir, "H2a_dps_code_decomposition_summary.csv"))
message("  - ", file.path(PATHS$out_dir, "H2a_dps_305_vs_rest.csv"))
message("  - ", file.path(PATHS$fig_dir, "H2a_dps_code_decomposition_bar.pdf"))
