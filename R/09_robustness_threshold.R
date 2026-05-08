# ============================================================================
# 09_robustness_threshold.R — Argmax-with-threshold robustness on H1a / H1b
# ----------------------------------------------------------------------------
# Reads from cache. Reports H1a / H1b under hard aggregation at thresholds
# CONF_THRESHOLDS (0.4, 0.5) alongside the soft (τ = 1.0) primary spec.
#
# H1a (Aggregation A): A is a partition of the 56 codes; manifesto-A has no
# Andere bucket. Per spec §3.5: when comparing hard-A speech distributions
# to manifesto-A distributions, drop any speech-side mass on Andere and
# renormalize the speech distribution before computing JSD. This corresponds
# to "keep only sentences whose argmax is above threshold" — the standard
# interpretation of the threshold rule.
#
# H1b (Aggregation B): the speech-side hard aggregation produces large
# Andere mass at high thresholds (most sentences fall outside the four
# directional domains anyway). To keep the comparison comparable, both
# manifesto and speech are renormalized within the four key domains
# (Andere excluded) before JSD.
#
# Per MIv2 §3.5, the threshold spec is expected to (a) preserve cross-cell
# ordering of JSD values (Spearman ≥ 0.85), (b) shift absolute magnitudes
# upward, especially via the Democracy & Political System bucket on A.
# Per-bucket attribution of the soft-vs-hard shift is reported.
#
# Outputs:
#   results/empirics/H1a_threshold_robustness.csv
#   results/empirics/H1b_threshold_robustness.csv
#   results/empirics/figures/H1a_threshold_scatter.{pdf,png}
#   results/empirics/figures/H1b_threshold_scatter.{pdf,png}
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))
manifesto_A <- md_list$A
manifesto_B <- md_list$B
m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

soft_A <- sd_list[[sprintf("soft_tau%s__A", format(TAU_PRIMARY, nsmall = 1))]]
soft_B <- sd_list[[sprintf("soft_tau%s__B", format(TAU_PRIMARY, nsmall = 1))]]

# ============================================================================
# H1a — soft vs hard JSD on Aggregation A
# ============================================================================
message("[09] Computing H1a JSDs at soft, hard 0.4, hard 0.5 ...")

manifesto_A_wide <- manifesto_A %>%
  filter(bucket %in% BUCKETS_A) %>%
  inner_join(m1_map, by = "election_date") %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

# Per-cell JSD on a long-format speech distribution. Drops Andere mass and
# renormalizes (decision documented above; spec §3.5).
jsd_A_per_cell <- function(speech_long) {
  sp_renorm <- speech_long %>%
    filter(bucket %in% BUCKETS_A) %>%
    group_by(party, lp) %>%
    mutate(share_renorm = if (sum(share) > 0) share / sum(share) else 0) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(party, lp), names_from = bucket,
                values_from = share_renorm)
  joined <- sp_renorm %>%
    inner_join(manifesto_A_wide, by = c("party", "lp"), suffix = c("_S", "_M"))
  rows <- list()
  for (i in seq_len(nrow(joined))) {
    s   <- as.numeric(joined[i, paste0(BUCKETS_A, "_S")])
    m_v <- as.numeric(joined[i, paste0(BUCKETS_A, "_M")])
    s[is.na(s)] <- 0; m_v[is.na(m_v)] <- 0
    if (sum(s) == 0 || sum(m_v) == 0) next
    rows[[length(rows) + 1L]] <- tibble(
      party = joined$party[i], lp = joined$lp[i],
      jsd = jsd(s, m_v),
      contrib = list(per_bucket_jsd(setNames(s, BUCKETS_A),
                                    setNames(m_v, BUCKETS_A)))
    )
  }
  bind_rows(rows)
}

jsd_a_soft <- jsd_A_per_cell(soft_A) %>% mutate(spec = "soft_tau1.0")
jsd_a_h04  <- jsd_A_per_cell(sd_list[["hard_thr0.4__A"]]) %>% mutate(spec = "hard_thr0.4")
jsd_a_h05  <- jsd_A_per_cell(sd_list[["hard_thr0.5__A"]]) %>% mutate(spec = "hard_thr0.5")

h1a_threshold <- bind_rows(jsd_a_soft, jsd_a_h04, jsd_a_h05) %>%
  filter(!(party == "FDP" & lp == 18L)) %>%
  mutate(party = factor(party, levels = PARTIES_KEEP),
         spec  = factor(spec,  levels = c("soft_tau1.0", "hard_thr0.4", "hard_thr0.5")))

# Per-bucket attribution: how much of the hard-soft difference comes from
# the Democracy & Political System bucket?
attribution_A <- h1a_threshold %>%
  rowwise() %>%
  mutate(dps_contrib = contrib[["Democracy & Political System"]],
         total_contrib = sum(contrib),
         dps_share_of_jsd = if (total_contrib > 0) dps_contrib / total_contrib else NA_real_) %>%
  ungroup() %>%
  select(party, lp, spec, dps_contrib, total_contrib, dps_share_of_jsd)

# Wide form: jsd_soft, jsd_h04, jsd_h05; rank correlations.
h1a_wide <- h1a_threshold %>%
  select(party, lp, spec, jsd) %>%
  pivot_wider(names_from = spec, values_from = jsd) %>%
  filter(!is.na(soft_tau1.0), !is.na(hard_thr0.4), !is.na(hard_thr0.5))

h1a_rho <- tibble(
  pair = c("soft_vs_hard0.4", "soft_vs_hard0.5", "hard0.4_vs_hard0.5"),
  spearman_rho = c(
    cor(h1a_wide$soft_tau1.0, h1a_wide$hard_thr0.4, method = "spearman"),
    cor(h1a_wide$soft_tau1.0, h1a_wide$hard_thr0.5, method = "spearman"),
    cor(h1a_wide$hard_thr0.4, h1a_wide$hard_thr0.5, method = "spearman")
  )
)

h1a_shift <- h1a_threshold %>%
  pivot_wider(id_cols = c(party, lp), names_from = spec, values_from = jsd) %>%
  mutate(delta_h04 = hard_thr0.4 - soft_tau1.0,
         delta_h05 = hard_thr0.5 - soft_tau1.0)

party_shift <- h1a_shift %>%
  group_by(party) %>%
  summarise(mean_delta_h04 = mean(delta_h04, na.rm = TRUE),
            mean_delta_h05 = mean(delta_h05, na.rm = TRUE),
            .groups = "drop")

write_csv(h1a_threshold %>% select(-contrib),
          file.path(PATHS$out_dir, "H1a_threshold_robustness.csv"))
write_csv(attribution_A,
          file.path(PATHS$out_dir, "H1a_threshold_attribution.csv"))

# ---- H1a scatter plot ------------------------------------------------------
h1a_scatter_dat <- h1a_threshold %>%
  select(party, lp, spec, jsd) %>%
  pivot_wider(names_from = spec, values_from = jsd) %>%
  pivot_longer(c(hard_thr0.4, hard_thr0.5),
               names_to = "threshold_spec", values_to = "jsd_hard") %>%
  rename(jsd_soft = soft_tau1.0) %>%
  mutate(threshold_spec = factor(threshold_spec,
                                 levels = c("hard_thr0.4", "hard_thr0.5"),
                                 labels = c("threshold 0.4", "threshold 0.5")))

p_h1a_thr <- ggplot(h1a_scatter_dat,
                    aes(x = jsd_soft, y = jsd_hard, colour = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.4, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)),
                            size = 2.5, max.overlaps = 10, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  facet_wrap(~ threshold_spec, ncol = 2) +
  labs(title    = "H1a — Soft vs hard-threshold JSD (Aggregation A)",
       subtitle = "Each point is a (party, LP) cell. Dashed line: 45° reference.",
       x = "Soft JSD (τ = 1.0)",
       y = "Hard JSD (argmax + threshold; speech-side Andere dropped & renormalized)",
       caption  = "Rank correlations and per-party shifts in H1a_threshold_robustness.csv.") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H1a_threshold_scatter.pdf"),
       p_h1a_thr, width = 11, height = 6)
ggsave(file.path(PATHS$fig_dir, "H1a_threshold_scatter.png"),
       p_h1a_thr, width = 11, height = 6, dpi = 200, bg = "white")

# ============================================================================
# H1b — soft vs hard JSD on Aggregation B (within-domain renormalized)
# ============================================================================
message("[09] Computing H1b within-domain JSDs at soft, hard 0.4, hard 0.5 ...")

manifesto_B_renorm <- manifesto_B %>%
  filter(bucket != "Andere") %>%
  inner_join(m1_map, by = "election_date") %>%
  group_by(party, lp) %>%
  mutate(share_renorm = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  select(party, lp, bucket, share_renorm_m = share_renorm)

jsd_B_per_cell <- function(speech_long) {
  speech_long %>%
    filter(bucket != "Andere") %>%
    group_by(party, lp) %>%
    mutate(share_renorm = if (sum(share) > 0) share / sum(share) else 0) %>%
    ungroup() %>%
    select(party, lp, bucket, share_renorm) %>%
    inner_join(manifesto_B_renorm, by = c("party", "lp", "bucket")) %>%
    group_by(party, lp) %>%
    summarise(jsd = jsd(share_renorm, share_renorm_m), .groups = "drop")
}

jsd_b_soft <- jsd_B_per_cell(soft_B) %>% mutate(spec = "soft_tau1.0")
jsd_b_h04  <- jsd_B_per_cell(sd_list[["hard_thr0.4__B"]]) %>% mutate(spec = "hard_thr0.4")
jsd_b_h05  <- jsd_B_per_cell(sd_list[["hard_thr0.5__B"]]) %>% mutate(spec = "hard_thr0.5")

h1b_threshold <- bind_rows(jsd_b_soft, jsd_b_h04, jsd_b_h05) %>%
  filter(!(party == "FDP" & lp == 18L)) %>%
  mutate(party = factor(party, levels = PARTIES_KEEP),
         spec  = factor(spec,  levels = c("soft_tau1.0", "hard_thr0.4", "hard_thr0.5")))

h1b_wide <- h1b_threshold %>%
  pivot_wider(id_cols = c(party, lp), names_from = spec, values_from = jsd) %>%
  filter(!is.na(soft_tau1.0), !is.na(hard_thr0.4), !is.na(hard_thr0.5))

h1b_rho <- tibble(
  pair = c("soft_vs_hard0.4", "soft_vs_hard0.5", "hard0.4_vs_hard0.5"),
  spearman_rho = c(
    cor(h1b_wide$soft_tau1.0, h1b_wide$hard_thr0.4, method = "spearman"),
    cor(h1b_wide$soft_tau1.0, h1b_wide$hard_thr0.5, method = "spearman"),
    cor(h1b_wide$hard_thr0.4, h1b_wide$hard_thr0.5, method = "spearman")
  )
)

write_csv(h1b_threshold,
          file.path(PATHS$out_dir, "H1b_threshold_robustness.csv"))

h1b_scatter_dat <- h1b_threshold %>%
  pivot_wider(id_cols = c(party, lp), names_from = spec, values_from = jsd) %>%
  pivot_longer(c(hard_thr0.4, hard_thr0.5),
               names_to = "threshold_spec", values_to = "jsd_hard") %>%
  rename(jsd_soft = soft_tau1.0) %>%
  mutate(threshold_spec = factor(threshold_spec,
                                 levels = c("hard_thr0.4", "hard_thr0.5"),
                                 labels = c("threshold 0.4", "threshold 0.5")))

p_h1b_thr <- ggplot(h1b_scatter_dat,
                    aes(x = jsd_soft, y = jsd_hard, colour = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.4, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)),
                            size = 2.5, max.overlaps = 10, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  facet_wrap(~ threshold_spec, ncol = 2) +
  labs(title    = "H1b — Soft vs hard-threshold within-domain JSD (Aggregation B)",
       subtitle = "Both sides renormalized within domain (Andere excluded). Dashed line: 45° reference.",
       x = "Soft JSD (τ = 1.0)",
       y = "Hard JSD (argmax + threshold)",
       caption  = "Rank correlations in H1b_threshold_robustness.csv.") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H1b_threshold_scatter.pdf"),
       p_h1b_thr, width = 11, height = 6)
ggsave(file.path(PATHS$fig_dir, "H1b_threshold_scatter.png"),
       p_h1b_thr, width = 11, height = 6, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H1a SOFT vs HARD RANK CORRELATIONS ====================\n")
print(h1a_rho %>% mutate(spearman_rho = round(spearman_rho, 3)))
cat("\nPer-party mean ΔJSD (hard − soft):\n")
print(party_shift %>% mutate(across(where(is.numeric), ~ round(., 4))))

cat("\n==================== H1b SOFT vs HARD RANK CORRELATIONS ====================\n")
print(h1b_rho %>% mutate(spearman_rho = round(spearman_rho, 3)))

cat("\n----- H1a per-bucket attribution: DPS share of JSD by spec -----\n")
print(attribution_A %>%
        group_by(spec) %>%
        summarise(mean_dps_share = round(mean(dps_share_of_jsd, na.rm = TRUE), 3),
                  median_dps_share = round(median(dps_share_of_jsd, na.rm = TRUE), 3),
                  .groups = "drop"))

if (any(c(h1a_rho$spearman_rho, h1b_rho$spearman_rho) < 0.85)) {
  message("[09] ⚠  At least one rank correlation below 0.85 — flag for the writeup.")
} else {
  message("[09] All soft-hard rank correlations ≥ 0.85 (MIv2 §3.5 acceptance OK).")
}

message("\n[09] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H1a_threshold_robustness.csv"))
message("  - ", file.path(PATHS$out_dir, "H1a_threshold_attribution.csv"))
message("  - ", file.path(PATHS$out_dir, "H1b_threshold_robustness.csv"))
message("  - ", file.path(PATHS$fig_dir, "H1a_threshold_scatter.pdf"))
message("  - ", file.path(PATHS$fig_dir, "H1b_threshold_scatter.pdf"))
