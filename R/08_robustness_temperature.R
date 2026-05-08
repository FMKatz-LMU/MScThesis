# ============================================================================
# 08_robustness_temperature.R — Temperature sweep robustness on H1a / H1b
# ----------------------------------------------------------------------------
# Reads only from cache. For tau in {0.5, 1.0, 2.0} on the speech-side
# softmax, recomputes per-cell JSD against the M1 manifesto on Aggregation A
# (H1a) and overall directional JSD on Aggregation B with within-domain
# renormalization, Andere excluded (H1b).
#
# MIv2 §3.3 acceptance: cross-cell ordering of JSD values should be preserved
# across tau values (Spearman >= 0.9). Rank correlations are reported.
#
# Outputs:
#   results/empirics/H1a_temperature_sweep.csv
#   results/empirics/H1b_temperature_sweep.csv
#   results/empirics/figures/H1a_temperature_sweep.{pdf,png}
#   results/empirics/figures/H1b_temperature_sweep.{pdf,png}
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))
manifesto_A <- md_list$A
manifesto_B <- md_list$B
m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# H1a temperature sweep — per-cell JSD on Aggregation A
# ============================================================================
message("[08] Computing H1a JSD across τ values ...")

manifesto_A_wide <- manifesto_A %>%
  filter(bucket %in% BUCKETS_A) %>%
  inner_join(m1_map, by = "election_date") %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

m_cols <- BUCKETS_A
h1a_sweep_rows <- list()
for (tn in names(TEMPERATURES)) {
  tau   <- TEMPERATURES[[tn]]
  key   <- sprintf("soft_tau%s__A", format(tau, nsmall = 1))
  sp    <- sd_list[[key]]
  sp_w  <- sp %>%
    filter(bucket %in% BUCKETS_A) %>%
    pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)
  joined <- sp_w %>%
    inner_join(manifesto_A_wide, by = c("party", "lp"), suffix = c("_S", "_M"))
  for (i in seq_len(nrow(joined))) {
    s   <- as.numeric(joined[i, paste0(BUCKETS_A, "_S")])
    m_v <- as.numeric(joined[i, paste0(BUCKETS_A, "_M")])
    s[is.na(s)] <- 0; m_v[is.na(m_v)] <- 0
    if (sum(s) == 0 || sum(m_v) == 0) next
    h1a_sweep_rows[[length(h1a_sweep_rows) + 1L]] <- tibble(
      tau_name = tn, tau = tau,
      party = joined$party[i], lp = joined$lp[i],
      jsd = jsd(s, m_v)
    )
  }
}
h1a_sweep <- bind_rows(h1a_sweep_rows) %>%
  mutate(party = factor(party, levels = PARTIES_KEEP),
         tau_name = factor(tau_name, levels = c("sharp", "native", "flat")))

# Drop the FDP LP18 cell (same exclusion as 04_H1.R).
h1a_sweep <- h1a_sweep %>% filter(!(party == "FDP" & lp == 18L))

# Spearman rank correlations between tau pairs.
h1a_wide <- h1a_sweep %>%
  pivot_wider(id_cols = c(party, lp), names_from = tau_name,
              values_from = jsd) %>%
  filter(!is.na(sharp), !is.na(native), !is.na(flat))

h1a_rho <- tibble(
  pair = c("sharp_vs_native", "sharp_vs_flat", "native_vs_flat"),
  spearman_rho = c(
    cor(h1a_wide$sharp,  h1a_wide$native, method = "spearman"),
    cor(h1a_wide$sharp,  h1a_wide$flat,   method = "spearman"),
    cor(h1a_wide$native, h1a_wide$flat,   method = "spearman")
  )
)

write_csv(h1a_sweep %>% mutate(spearman_rho_sharp_vs_native =
                                  h1a_rho$spearman_rho[h1a_rho$pair == "sharp_vs_native"],
                                spearman_rho_sharp_vs_flat =
                                  h1a_rho$spearman_rho[h1a_rho$pair == "sharp_vs_flat"],
                                spearman_rho_native_vs_flat =
                                  h1a_rho$spearman_rho[h1a_rho$pair == "native_vs_flat"]),
          file.path(PATHS$out_dir, "H1a_temperature_sweep.csv"))

# ---- H1a sweep plot --------------------------------------------------------
TAU_LINE_TYPES <- c("sharp" = "dashed", "native" = "solid", "flat" = "dotted")
TAU_COLOURS    <- c("sharp" = "#A23B22", "native" = "#264653", "flat" = "#1A6F66")

p_h1a_sweep <- h1a_sweep %>%
  ggplot(aes(x = lp, y = jsd, colour = tau_name, linetype = tau_name,
             group = interaction(party, tau_name))) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = TAU_COLOURS,
                      labels = c("τ = 0.5 (sharp)", "τ = 1.0 (native)", "τ = 2.0 (flat)"),
                      name = NULL) +
  scale_linetype_manual(values = TAU_LINE_TYPES,
                        labels = c("τ = 0.5 (sharp)", "τ = 1.0 (native)", "τ = 2.0 (flat)"),
                        name = NULL) +
  facet_wrap(~ party, ncol = 3, drop = FALSE) +
  labs(title    = "H1a — Manifesto–speech JSD across temperature settings",
       subtitle = "Per-cell JSD on Aggregation A under three temperature settings of the speech-side softmax",
       x = "Legislative period",
       y = "Jensen–Shannon Divergence",
       caption  = sprintf(
         "Spearman rank correlations across cells:  sharp ↔ native = %.3f;  sharp ↔ flat = %.3f;  native ↔ flat = %.3f.\nMIv2 §3.3 acceptance threshold: ρ ≥ 0.9.",
         h1a_rho$spearman_rho[h1a_rho$pair == "sharp_vs_native"],
         h1a_rho$spearman_rho[h1a_rho$pair == "sharp_vs_flat"],
         h1a_rho$spearman_rho[h1a_rho$pair == "native_vs_flat"])) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H1a_temperature_sweep.pdf"),
       p_h1a_sweep, width = 10, height = 6.5)
ggsave(file.path(PATHS$fig_dir, "H1a_temperature_sweep.png"),
       p_h1a_sweep, width = 10, height = 6.5, dpi = 200, bg = "white")

# ============================================================================
# H1b temperature sweep — overall within-domain JSD on Aggregation B
# ============================================================================
message("[08] Computing H1b within-domain overall JSD across τ values ...")

# Manifesto B side: within-domain renormalization, Andere excluded.
manifesto_B_renorm <- manifesto_B %>%
  filter(bucket != "Andere") %>%
  inner_join(m1_map, by = "election_date") %>%
  group_by(party, lp) %>%
  mutate(share_renorm = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  select(party, lp, bucket, share_renorm_m = share_renorm)

h1b_sweep_rows <- list()
for (tn in names(TEMPERATURES)) {
  tau <- TEMPERATURES[[tn]]
  key <- sprintf("soft_tau%s__B", format(tau, nsmall = 1))
  sp  <- sd_list[[key]]
  sp_renorm <- sp %>%
    filter(bucket != "Andere") %>%
    group_by(party, lp) %>%
    mutate(share_renorm = if (sum(share) > 0) share / sum(share) else 0) %>%
    ungroup() %>%
    select(party, lp, bucket, share_renorm)
  joined <- sp_renorm %>%
    inner_join(manifesto_B_renorm, by = c("party", "lp", "bucket"))
  per_cell <- joined %>%
    group_by(party, lp) %>%
    summarise(jsd_B = jsd(share_renorm, share_renorm_m), .groups = "drop") %>%
    mutate(tau_name = tn, tau = tau)
  h1b_sweep_rows[[tn]] <- per_cell
}
h1b_sweep <- bind_rows(h1b_sweep_rows) %>%
  mutate(party    = factor(party, levels = PARTIES_KEEP),
         tau_name = factor(tau_name, levels = c("sharp", "native", "flat"))) %>%
  filter(!(party == "FDP" & lp == 18L))

h1b_wide <- h1b_sweep %>%
  pivot_wider(id_cols = c(party, lp), names_from = tau_name, values_from = jsd_B) %>%
  filter(!is.na(sharp), !is.na(native), !is.na(flat))

h1b_rho <- tibble(
  pair = c("sharp_vs_native", "sharp_vs_flat", "native_vs_flat"),
  spearman_rho = c(
    cor(h1b_wide$sharp,  h1b_wide$native, method = "spearman"),
    cor(h1b_wide$sharp,  h1b_wide$flat,   method = "spearman"),
    cor(h1b_wide$native, h1b_wide$flat,   method = "spearman")
  )
)

write_csv(h1b_sweep %>% mutate(spearman_rho_sharp_vs_native =
                                  h1b_rho$spearman_rho[h1b_rho$pair == "sharp_vs_native"],
                                spearman_rho_sharp_vs_flat =
                                  h1b_rho$spearman_rho[h1b_rho$pair == "sharp_vs_flat"],
                                spearman_rho_native_vs_flat =
                                  h1b_rho$spearman_rho[h1b_rho$pair == "native_vs_flat"]),
          file.path(PATHS$out_dir, "H1b_temperature_sweep.csv"))

p_h1b_sweep <- h1b_sweep %>%
  ggplot(aes(x = lp, y = jsd_B, colour = tau_name, linetype = tau_name,
             group = interaction(party, tau_name))) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = TAU_COLOURS,
                      labels = c("τ = 0.5 (sharp)", "τ = 1.0 (native)", "τ = 2.0 (flat)"),
                      name = NULL) +
  scale_linetype_manual(values = TAU_LINE_TYPES,
                        labels = c("τ = 0.5 (sharp)", "τ = 1.0 (native)", "τ = 2.0 (flat)"),
                        name = NULL) +
  facet_wrap(~ party, ncol = 3, drop = FALSE) +
  labs(title    = "H1b — Directional JSD across temperature settings",
       subtitle = "Within-domain renormalized JSD on Aggregation B (Andere excluded), three τ values",
       x = "Legislative period",
       y = "Within-domain JSD (B)",
       caption  = sprintf(
         "Spearman rank correlations across cells:  sharp ↔ native = %.3f;  sharp ↔ flat = %.3f;  native ↔ flat = %.3f.",
         h1b_rho$spearman_rho[h1b_rho$pair == "sharp_vs_native"],
         h1b_rho$spearman_rho[h1b_rho$pair == "sharp_vs_flat"],
         h1b_rho$spearman_rho[h1b_rho$pair == "native_vs_flat"])) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H1b_temperature_sweep.pdf"),
       p_h1b_sweep, width = 10, height = 6.5)
ggsave(file.path(PATHS$fig_dir, "H1b_temperature_sweep.png"),
       p_h1b_sweep, width = 10, height = 6.5, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H1a τ-SWEEP RANK CORRELATIONS ====================\n")
print(h1a_rho %>% mutate(spearman_rho = round(spearman_rho, 3)))

cat("\n==================== H1b τ-SWEEP RANK CORRELATIONS ====================\n")
print(h1b_rho %>% mutate(spearman_rho = round(spearman_rho, 3)))

if (any(c(h1a_rho$spearman_rho, h1b_rho$spearman_rho) < 0.9)) {
  message("[08] ⚠  At least one rank correlation below 0.9 — flag for the writeup.")
} else {
  message("[08] All τ-pair rank correlations ≥ 0.9 (MIv2 §3.3 acceptance OK).")
}

message("\n[08] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H1a_temperature_sweep.csv"))
message("  - ", file.path(PATHS$out_dir, "H1b_temperature_sweep.csv"))
message("  - ", file.path(PATHS$fig_dir, "H1a_temperature_sweep.pdf"))
message("  - ", file.path(PATHS$fig_dir, "H1b_temperature_sweep.pdf"))
