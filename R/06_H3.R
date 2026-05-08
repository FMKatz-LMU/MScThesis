# ============================================================================
# 06_H3.R — Hypotheses 3a (salience ownership) and 3b (directional ownership)
# ----------------------------------------------------------------------------
# H3a — Issue ownership in salience: parties devote disproportionate speech
#       attention to their owned policy domains, narrowing manifesto-speech
#       divergence on those buckets.
#
# H3b — Issue ownership in direction: within each owned directional domain,
#       parties' parliamentary speech aligns more closely (lower JSD on the
#       within-domain renormalized distribution) with their manifesto
#       directional position than non-owned domains.
#
# Inputs (from cache):
#   speech_dists.rds  $soft_tau1.0__A   (Aggregation A, primary tau)
#                     $soft_tau1.0__B   (Aggregation B, primary tau)
#   manifesto_dists.rds $A, $B
#   OWNERSHIP_A, OWNERSHIP_B from 00_config.R
#
# Outputs:
#   results/empirics/H3a_per_bucket_table.csv
#   results/empirics/H3a_ownership_test.csv
#   results/empirics/H3b_per_domain_table.csv
#   results/empirics/H3b_ownership_test.csv
#   results/empirics/figures/H3a_ownership_heatmap.{pdf,png}
#   results/empirics/figures/H3b_ownership_heatmap.{pdf,png}
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))

.primary_A_key <- sprintf("soft_tau%s__A", format(TAU_PRIMARY, nsmall = 1))
.primary_B_key <- sprintf("soft_tau%s__B", format(TAU_PRIMARY, nsmall = 1))
speech_A    <- sd_list[[.primary_A_key]]
speech_B    <- sd_list[[.primary_B_key]]
manifesto_A <- md_list$A
manifesto_B <- md_list$B

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# H3a — Salience-side ownership heatmap (Aggregation A)
# ============================================================================
message("[H3a] Computing per-cell, per-bucket JSD contributions ...")

# Wide form per cell (mirrors 05_H2.R H2a logic).
speech_A_wide <- speech_A %>%
  filter(bucket %in% BUCKETS_A) %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

manifesto_A_wide <- manifesto_A %>%
  filter(bucket %in% BUCKETS_A) %>%
  inner_join(m1_map, by = "election_date") %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

joined_A <- speech_A_wide %>%
  inner_join(manifesto_A_wide, by = c("party", "lp"), suffix = c("_S", "_M"))

s_cols <- paste0(BUCKETS_A, "_S")
m_cols <- paste0(BUCKETS_A, "_M")

h3a_long <- bind_rows(lapply(seq_len(nrow(joined_A)), function(i) {
  s   <- as.numeric(joined_A[i, s_cols]); names(s)   <- BUCKETS_A
  m_v <- as.numeric(joined_A[i, m_cols]); names(m_v) <- BUCKETS_A
  s[is.na(s)] <- 0; m_v[is.na(m_v)] <- 0
  contrib <- if (sum(s) == 0 || sum(m_v) == 0)
    setNames(rep(NA_real_, length(BUCKETS_A)), BUCKETS_A)
  else
    per_bucket_jsd(s, m_v)
  tibble(party = joined_A$party[i], lp = joined_A$lp[i],
         bucket = names(contrib), jsd_contrib = unname(contrib))
}))

# Average across LPs within each (party, bucket).
h3a_party_bucket <- h3a_long %>%
  group_by(party, bucket) %>%
  summarise(mean_jsd_contrib = mean(jsd_contrib, na.rm = TRUE),
            n_lps_used       = sum(!is.na(jsd_contrib)),
            .groups = "drop") %>%
  left_join(OWNERSHIP_A %>% mutate(is_owned = TRUE),
            by = c("party", "bucket" = "bucket_A")) %>%
  mutate(is_owned = !is.na(is_owned))

write_csv(h3a_party_bucket,
          file.path(PATHS$out_dir, "H3a_per_bucket_table.csv"))

# Ownership test: per-party, mean JSD on owned vs non-owned buckets.
# Wilcoxon paired across LPs (per spec §4.1.1) — for each party with at
# least one ownership claim, compute the per-LP mean JSD on its owned
# buckets vs. non-owned buckets and run a paired test on the LP-level
# differences.
h3a_test_rows <- list()
for (pty in unique(OWNERSHIP_A$party)) {
  owned_buckets <- OWNERSHIP_A$bucket_A[OWNERSHIP_A$party == pty]
  per_lp <- h3a_long %>%
    filter(party == pty, !is.na(jsd_contrib)) %>%
    mutate(is_owned = bucket %in% owned_buckets) %>%
    group_by(lp, is_owned) %>%
    summarise(mean_contrib = mean(jsd_contrib), .groups = "drop") %>%
    pivot_wider(names_from = is_owned, values_from = mean_contrib,
                names_prefix = "mean_") %>%
    rename(owned = mean_TRUE, non_owned = mean_FALSE) %>%
    filter(!is.na(owned), !is.na(non_owned))
  if (nrow(per_lp) >= 2L) {
    wt <- suppressWarnings(wilcox.test(per_lp$owned, per_lp$non_owned, paired = TRUE))
    h3a_test_rows[[pty]] <- tibble(
      party             = pty,
      n_lps             = nrow(per_lp),
      n_owned_buckets   = length(owned_buckets),
      mean_owned        = mean(per_lp$owned),
      mean_non_owned    = mean(per_lp$non_owned),
      diff_mean         = mean(per_lp$owned) - mean(per_lp$non_owned),
      wilcox_V          = unname(wt$statistic),
      wilcox_p_value    = wt$p.value
    )
  }
}
h3a_test <- bind_rows(h3a_test_rows)
write_csv(h3a_test, file.path(PATHS$out_dir, "H3a_ownership_test.csv"))

# ---- H3a heatmap -----------------------------------------------------------
ownership_cells_A <- OWNERSHIP_A %>% rename(bucket = bucket_A)

# Order axes: parties as in PARTIES_KEEP; buckets as in BUCKETS_A.
h3a_plot_dat <- h3a_party_bucket %>%
  mutate(party  = factor(party,  levels = PARTIES_KEEP),
         bucket = factor(bucket, levels = BUCKETS_A))

p_h3a <- ggplot(h3a_plot_dat,
                aes(x = bucket, y = fct_rev(party), fill = mean_jsd_contrib)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_tile(data = ownership_cells_A %>%
              mutate(party  = factor(party,  levels = PARTIES_KEEP),
                     bucket = factor(bucket, levels = BUCKETS_A)),
            aes(x = bucket, y = fct_rev(party)),
            fill = NA, colour = "red", linewidth = 0.9, inherit.aes = FALSE) +
  geom_text(aes(label = sprintf("%.3f", mean_jsd_contrib)),
            size = 2.6, colour = "white") +
  scale_fill_viridis_c(direction = -1, name = "Mean JSD\ncontribution") +
  scale_x_discrete(position = "top") +
  labs(
    title    = "H3a — Salience-side ownership heatmap (Aggregation A)",
    subtitle = "Lighter = lower manifesto-speech divergence on that bucket. Red outlines mark predicted ownership cells.",
    x = NULL, y = NULL,
    caption  = "Per-bucket JSD contribution averaged across LPs 13–20. Source: speech_dists.rds (soft τ = 1.0)."
  ) +
  theme_thesis() +
  theme(axis.text.x.top = element_text(angle = 28, hjust = 0))

ggsave(file.path(PATHS$fig_dir, "H3a_ownership_heatmap.pdf"),
       p_h3a, width = 11, height = 5.5)
ggsave(file.path(PATHS$fig_dir, "H3a_ownership_heatmap.png"),
       p_h3a, width = 11, height = 5.5, dpi = 200, bg = "white")

# ============================================================================
# H3b — Directional-side ownership heatmap (Aggregation B)
# ============================================================================
message("[H3b] Computing per-(party, LP, domain) within-domain JSD ...")

b_lookup <- AGG_B %>% distinct(domain_B, bucket_B)

# Speech and manifesto wide forms with within-domain renormalization.
speech_B_dir <- speech_B %>%
  filter(bucket %in% BUCKETS_B) %>%
  inner_join(b_lookup, by = c("bucket" = "bucket_B")) %>%
  group_by(party, lp, domain_B) %>%
  mutate(share_within = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  select(party, lp, domain = domain_B, sub = bucket, share_within)

manifesto_B_dir <- manifesto_B %>%
  filter(bucket %in% BUCKETS_B) %>%
  inner_join(b_lookup, by = c("bucket" = "bucket_B")) %>%
  group_by(party, election_date, domain_B) %>%
  mutate(share_within = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  inner_join(m1_map, by = "election_date") %>%
  select(party, lp, domain = domain_B, sub = bucket, share_within)

# Pairwise JSD per (party, lp, domain).
joined_B <- inner_join(
  speech_B_dir   %>% rename(speech_share   = share_within),
  manifesto_B_dir %>% rename(manifesto_share = share_within),
  by = c("party", "lp", "domain", "sub")
)

jsd_per_cell <- joined_B %>%
  group_by(party, lp, domain) %>%
  summarise(jsd_within = jsd(speech_share, manifesto_share),
            mass_speech    = sum(speech_share),
            mass_manifesto = sum(manifesto_share),
            .groups = "drop") %>%
  filter(mass_speech > 0, mass_manifesto > 0)

# Average per (party, domain).
h3b_party_domain <- jsd_per_cell %>%
  group_by(party, domain) %>%
  summarise(mean_jsd_within_domain = mean(jsd_within, na.rm = TRUE),
            n_lps_used             = sum(!is.na(jsd_within)),
            .groups = "drop") %>%
  left_join(OWNERSHIP_B %>% mutate(is_owned = TRUE),
            by = c("party", "domain" = "domain_B")) %>%
  mutate(is_owned = !is.na(is_owned))

write_csv(h3b_party_domain,
          file.path(PATHS$out_dir, "H3b_per_domain_table.csv"))

# Ownership test (Wilcoxon paired across LPs).
h3b_test_rows <- list()
for (pty in unique(OWNERSHIP_B$party)) {
  owned_domains <- OWNERSHIP_B$domain_B[OWNERSHIP_B$party == pty]
  per_lp <- jsd_per_cell %>%
    filter(party == pty) %>%
    mutate(is_owned = domain %in% owned_domains) %>%
    group_by(lp, is_owned) %>%
    summarise(mean_jsd = mean(jsd_within, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = is_owned, values_from = mean_jsd,
                names_prefix = "mean_") %>%
    rename(owned = mean_TRUE, non_owned = mean_FALSE) %>%
    filter(!is.na(owned), !is.na(non_owned))
  if (nrow(per_lp) >= 2L) {
    wt <- suppressWarnings(wilcox.test(per_lp$owned, per_lp$non_owned, paired = TRUE))
    h3b_test_rows[[pty]] <- tibble(
      party             = pty,
      n_lps             = nrow(per_lp),
      n_owned_domains   = length(owned_domains),
      mean_owned        = mean(per_lp$owned),
      mean_non_owned    = mean(per_lp$non_owned),
      diff_mean         = mean(per_lp$owned) - mean(per_lp$non_owned),
      wilcox_V          = unname(wt$statistic),
      wilcox_p_value    = wt$p.value
    )
  }
}
h3b_test <- bind_rows(h3b_test_rows)
write_csv(h3b_test, file.path(PATHS$out_dir, "H3b_ownership_test.csv"))

# ---- H3b heatmap -----------------------------------------------------------
ownership_cells_B <- OWNERSHIP_B %>% rename(domain = domain_B)

h3b_plot_dat <- h3b_party_domain %>%
  mutate(party  = factor(party,  levels = PARTIES_KEEP),
         domain = factor(domain, levels = DOMAINS_B))

p_h3b <- ggplot(h3b_plot_dat,
                aes(x = domain, y = fct_rev(party), fill = mean_jsd_within_domain)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_tile(data = ownership_cells_B %>%
              mutate(party  = factor(party,  levels = PARTIES_KEEP),
                     domain = factor(domain, levels = DOMAINS_B)),
            aes(x = domain, y = fct_rev(party)),
            fill = NA, colour = "red", linewidth = 0.9, inherit.aes = FALSE) +
  geom_text(aes(label = sprintf("%.3f", mean_jsd_within_domain)),
            size = 2.8, colour = "white") +
  scale_fill_viridis_c(direction = -1, name = "Mean JSD\nwithin domain") +
  scale_x_discrete(position = "top") +
  labs(
    title    = "H3b — Directional-side ownership heatmap (Aggregation B)",
    subtitle = "Within-domain JSD between manifesto and speech directional distributions, averaged across LPs.\nLighter = closer alignment. Red outlines mark predicted ownership cells (Europe excluded by design).",
    x = NULL, y = NULL,
    caption  = "Within-domain renormalization (Andere excluded). Source: speech_dists.rds (soft τ = 1.0)."
  ) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H3b_ownership_heatmap.pdf"),
       p_h3b, width = 8, height = 5.5)
ggsave(file.path(PATHS$fig_dir, "H3b_ownership_heatmap.png"),
       p_h3b, width = 8, height = 5.5, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H3a OWNERSHIP TEST ====================\n")
print(h3a_test %>% mutate(across(where(is.numeric), ~ signif(., 3))))
cat("\n==================== H3b OWNERSHIP TEST ====================\n")
print(h3b_test %>% mutate(across(where(is.numeric), ~ signif(., 3))))

cat("\n----- H3a per-row owned vs row-median check -----\n")
h3a_diag <- h3a_party_bucket %>%
  group_by(party) %>%
  mutate(row_median = median(mean_jsd_contrib, na.rm = TRUE)) %>%
  filter(is_owned) %>%
  mutate(below_median = mean_jsd_contrib < row_median) %>%
  ungroup()
print(h3a_diag %>%
        select(party, bucket, mean_jsd_contrib, row_median, below_median))

afd_mig <- h3a_party_bucket %>% filter(party == "AfD", bucket == "Migration")
if (nrow(afd_mig) > 0) {
  cat(sprintf("\n  AfD ↔ Migration cell: mean JSD contribution = %.4f (is_owned = TRUE)\n",
              afd_mig$mean_jsd_contrib))
}

message("\n[H3] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H3a_per_bucket_table.csv"))
message("  - ", file.path(PATHS$out_dir, "H3a_ownership_test.csv"))
message("  - ", file.path(PATHS$fig_dir, "H3a_ownership_heatmap.pdf"))
message("  - ", file.path(PATHS$out_dir, "H3b_per_domain_table.csv"))
message("  - ", file.path(PATHS$out_dir, "H3b_ownership_test.csv"))
message("  - ", file.path(PATHS$fig_dir, "H3b_ownership_heatmap.pdf"))
