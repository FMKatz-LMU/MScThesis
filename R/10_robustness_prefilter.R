# ============================================================================
# 10_robustness_prefilter.R — Tightened pre-filter robustness for H2a
# ----------------------------------------------------------------------------
# Reads from cache. Compares the per-bucket JSD contribution under the
# primary specification (soft τ = 1.0 with sentence-level pre-filter) against
# the tightened pre-filter variant (sentence length ≥ TIGHT_FILTER$
# min_words_per_sentence).
#
# Particular focus: the Democracy & Political System bucket — does its mean
# JSD contribution shrink under the tighter filter? Pre-committed
# interpretation rules per MIv2 §6.4 are printed to console.
#
# Outputs:
#   results/empirics/H2a_prefilter_comparison.csv
#   results/empirics/H2a_prefilter_shifts.csv
#   results/empirics/H2a_prefilter_dropstats.csv
#   results/empirics/figures/H2a_prefilter_comparison.{pdf,png}
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))
manifesto_A <- md_list$A
m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

primary_A <- sd_list[[sprintf("soft_tau%s__A", format(TAU_PRIMARY, nsmall = 1))]]
tight_A   <- sd_list[["tightprefilter__A"]]

# Both speech tibbles may include an Andere column for B-stream variants —
# the A stream from the soft τ pipeline is a partition (no Andere). The
# tightprefilter__A from the parquet pass DOES include an Andere column
# (always 0 for A by construction); drop it here for safety.
primary_A <- primary_A %>% filter(bucket %in% BUCKETS_A)
tight_A   <- tight_A   %>% filter(bucket %in% BUCKETS_A) %>%
  group_by(party, lp) %>%
  mutate(share = if (sum(share) > 0) share / sum(share) else share) %>%
  ungroup()

# ============================================================================
# Per-(party, lp, bucket) JSD contribution under each variant
# ============================================================================
manifesto_A_wide <- manifesto_A %>%
  filter(bucket %in% BUCKETS_A) %>%
  inner_join(m1_map, by = "election_date") %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

per_bucket_long <- function(speech_long, label) {
  sp_w <- speech_long %>%
    pivot_wider(id_cols = c(party, lp, n_sent),
                names_from = bucket, values_from = share)
  joined <- sp_w %>%
    inner_join(manifesto_A_wide, by = c("party", "lp"), suffix = c("_S", "_M"))
  rows <- list()
  for (i in seq_len(nrow(joined))) {
    s   <- as.numeric(joined[i, paste0(BUCKETS_A, "_S")])
    m_v <- as.numeric(joined[i, paste0(BUCKETS_A, "_M")])
    s[is.na(s)] <- 0; m_v[is.na(m_v)] <- 0
    if (sum(s) == 0 || sum(m_v) == 0) next
    contrib <- per_bucket_jsd(setNames(s, BUCKETS_A),
                              setNames(m_v, BUCKETS_A))
    rows[[length(rows) + 1L]] <- tibble(
      variant = label,
      party = joined$party[i], lp = joined$lp[i],
      n_sent = joined$n_sent[i],
      bucket = names(contrib), jsd_contrib = unname(contrib)
    )
  }
  bind_rows(rows)
}

primary_per_bucket <- per_bucket_long(primary_A, "primary")
tight_per_bucket   <- per_bucket_long(tight_A,   "tight")

prefilter_long <- bind_rows(primary_per_bucket, tight_per_bucket) %>%
  filter(!(party == "FDP" & lp == 18L)) %>%
  mutate(party  = factor(party,  levels = PARTIES_KEEP),
         bucket = factor(bucket, levels = BUCKETS_A))

# ============================================================================
# Mean per-bucket contribution per variant
# ============================================================================
comparison <- prefilter_long %>%
  group_by(bucket, variant) %>%
  summarise(mean_contrib = mean(jsd_contrib, na.rm = TRUE),
            n_cells      = sum(!is.na(jsd_contrib)),
            .groups = "drop")

write_csv(comparison,
          file.path(PATHS$out_dir, "H2a_prefilter_comparison.csv"))

shifts <- comparison %>%
  pivot_wider(names_from = variant, values_from = c(mean_contrib, n_cells)) %>%
  rename(primary = mean_contrib_primary,
         tight   = mean_contrib_tight) %>%
  mutate(delta      = tight - primary,
         pct_change = if_else(primary > 0,
                              (tight - primary) / primary * 100,
                              NA_real_)) %>%
  arrange(desc(primary))

write_csv(shifts %>% select(bucket, primary, tight, delta, pct_change),
          file.path(PATHS$out_dir, "H2a_prefilter_shifts.csv"))

# ============================================================================
# Per-cell drop statistics (tight n_sent vs primary n_sent)
# ============================================================================
n_primary <- distinct(primary_per_bucket, party, lp, n_sent) %>%
  rename(n_sent_primary = n_sent)
n_tight   <- distinct(tight_per_bucket, party, lp, n_sent) %>%
  rename(n_sent_tight = n_sent)
dropstats <- n_primary %>%
  full_join(n_tight, by = c("party", "lp")) %>%
  mutate(drop_pct = if_else(!is.na(n_sent_primary) & n_sent_primary > 0,
                            (n_sent_primary - n_sent_tight) / n_sent_primary * 100,
                            NA_real_)) %>%
  arrange(party, lp)

write_csv(dropstats, file.path(PATHS$out_dir, "H2a_prefilter_dropstats.csv"))

# ============================================================================
# Plot — paired bars per bucket
# ============================================================================
plot_dat <- comparison %>%
  mutate(bucket  = factor(bucket,
                          levels = comparison %>%
                            filter(variant == "primary") %>%
                            arrange(mean_contrib) %>%
                            pull(bucket)),
         variant = factor(variant, levels = c("primary", "tight"),
                          labels = c("Primary (length ≥ 1 word)",
                                     sprintf("Tight (length ≥ %d words)",
                                             TIGHT_FILTER$min_words_per_sentence))))

p_pre <- ggplot(plot_dat,
                aes(y = bucket, x = mean_contrib, fill = variant)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = c("#264653", "#E9C46A"), name = NULL) +
  labs(title    = "H2a — Tightened pre-filter robustness",
       subtitle = sprintf(
         "Mean per-bucket JSD contribution under the primary vs. tightened pre-filter\n(sentence length ≥ %d words; per spec §3.1.3 the tightened filter is sentence-level only).",
         TIGHT_FILTER$min_words_per_sentence),
       x = "Mean JSD contribution",
       y = NULL,
       caption  = "Per-cell drop statistics in H2a_prefilter_dropstats.csv. Pre-committed interpretation: see console output.") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H2a_prefilter_comparison.pdf"),
       p_pre, width = 9, height = 6)
ggsave(file.path(PATHS$fig_dir, "H2a_prefilter_comparison.png"),
       p_pre, width = 9, height = 6, dpi = 200, bg = "white")

# ============================================================================
# Pre-committed interpretation block (MIv2 §6.4)
# ============================================================================
DPS <- "Democracy & Political System"
dps_row <- shifts %>% filter(bucket == DPS)
cat("\n==================== H2a PRE-FILTER COMPARISON ====================\n")
print(shifts %>% mutate(across(where(is.numeric), ~ signif(., 3))))
cat("\n----- Per-cell drop rates -----\n")
print(dropstats %>%
        mutate(across(c(n_sent_primary, n_sent_tight), ~ format(., big.mark = ",")),
               drop_pct = round(drop_pct, 1)))

if (nrow(dps_row) == 1L && !is.na(dps_row$pct_change)) {
  pct <- dps_row$pct_change
  cat(sprintf("\n----- Democracy & Political System bucket -----\n"))
  cat(sprintf("  Primary mean JSD contribution: %.4f\n", dps_row$primary))
  cat(sprintf("  Tight   mean JSD contribution: %.4f\n", dps_row$tight))
  cat(sprintf("  Δ JSD contribution:            %.4f  (%.1f%% change)\n",
              dps_row$delta, pct))
  abs_pct_drop <- -pct
  cat("\n----- Pre-committed verdict (MIv2 §6.4) -----\n")
  if (abs_pct_drop < 25) {
    cat("[VERDICT] Filter robustness check confirms the procedural-language artefact\n",
        "          is intrinsic. H2a finding stands; report as cross-domain artefact.\n", sep = "")
  } else if (abs_pct_drop < 50) {
    cat("[VERDICT] Pre-filter material; report both specifications side by side and\n",
        "          discuss the artefact diagnosis as partial.\n", sep = "")
  } else {
    cat("[VERDICT] Original pre-filter was too permissive; H2a interpretation requires\n",
        "          substantial revision; tighter filter becomes the primary spec.\n", sep = "")
  }
} else {
  message("[10] DPS bucket row missing or pct_change NA — cannot evaluate verdict.")
}

message("\n[10] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H2a_prefilter_comparison.csv"))
message("  - ", file.path(PATHS$out_dir, "H2a_prefilter_shifts.csv"))
message("  - ", file.path(PATHS$out_dir, "H2a_prefilter_dropstats.csv"))
message("  - ", file.path(PATHS$fig_dir, "H2a_prefilter_comparison.pdf"))
