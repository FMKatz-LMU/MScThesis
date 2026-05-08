# ============================================================================
# 07_H4.R — System-level polarization over time
# ----------------------------------------------------------------------------
# H4: Polarization in the German party system has increased over LPs 13–20.
#     Trend is observable on both channels (manifesto, speech) and is most
#     pronounced in Migration and Europe after 2015–2017.
#
# Operationalization: Dalton-style weighted dispersion of polarization scores
# (helper `dalton_polarization` in 02_helpers.R).
#
#   Polarization(LP, domain, channel) = sqrt( Σ_p w_p · (pos_p − μ)² )
#   where μ = Σ_p w_p · pos_p   (weighted mean position)
#
# Weights (from 00_config.R):
#   - Manifesto channel: VOTE_SHARES  (Zweitstimmen at the election)
#   - Speech channel:    SEAT_SHARES  (Bundestag seat share at LP start)
#
# Position scores come from `polarization_score` in 02_helpers.R (same
# canonical implementation used by H2b and H3b).
#
# Outputs:
#   results/empirics/H4_system_polarization.csv
#   results/empirics/H4_mean_positions.csv
#   results/empirics/H4_trend_fits.csv
#   results/empirics/H4_pre_post_2017.csv
#   results/empirics/figures/H4_polarization_lines.{pdf,png}
#   results/empirics/figures/H4_polarization_lines_robust.pdf
#   results/empirics/figures/H4_mean_positions.{pdf,png}
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))

.primary_B_key <- sprintf("soft_tau%s__B", format(TAU_PRIMARY, nsmall = 1))
speech_B    <- if (.primary_B_key %in% names(sd_list)) sd_list[[.primary_B_key]] else sd_list$B
manifesto_B <- md_list$B

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# Step 1 — Per-cell position scores (both channels)
# ============================================================================
message("[H4] Computing per-cell positions (speech) ...")
sp_pos <- distinct(speech_B, party, lp) %>%
  rowwise() %>%
  mutate(pos = list(polarization_score(speech_B, party, lp, "lp"))) %>%
  unnest(pos) %>%
  ungroup() %>%
  pivot_longer(c(Economy, Welfare, Migration, Europe),
               names_to = "domain", values_to = "position")

message("[H4] Computing per-cell positions (manifesto) ...")
mf_pos <- distinct(manifesto_B, party, election_date) %>%
  rowwise() %>%
  mutate(pos = list(polarization_score(manifesto_B, party, election_date, "election_date"))) %>%
  unnest(pos) %>%
  ungroup() %>%
  pivot_longer(c(Economy, Welfare, Migration, Europe),
               names_to = "domain", values_to = "position")

# ============================================================================
# Step 2 — Attach weights from 00_config.R
# ============================================================================
sp_weighted <- sp_pos %>%
  inner_join(SEAT_SHARES, by = c("party", "lp")) %>%
  rename(weight = seat_share) %>%
  inner_join(m1_map, by = "lp") %>%
  mutate(channel = "Speech") %>%
  select(party, lp, election_date, domain, position, weight, channel)

mf_weighted <- mf_pos %>%
  inner_join(VOTE_SHARES, by = c("party", "election_date")) %>%
  rename(weight = vote_share) %>%
  inner_join(m1_map, by = "election_date") %>%
  mutate(channel = "Manifesto") %>%
  select(party, lp, election_date, domain, position, weight, channel)

both <- bind_rows(sp_weighted, mf_weighted)

# ============================================================================
# Step 3 — Dalton-weighted polarization per (LP, domain, channel)
# ============================================================================
message("[H4] Computing Dalton-weighted polarization ...")
h4_main <- both %>%
  group_by(lp, domain, channel) %>%
  summarise(out = list(dalton_polarization(position, weight)), .groups = "drop") %>%
  unnest(out) %>%
  mutate(domain  = factor(domain,  levels = DOMAINS_B),
         channel = factor(channel, levels = c("Manifesto", "Speech")))

# Robustness: same measure excluding AfD
h4_no_afd <- both %>%
  filter(party != "AfD") %>%
  group_by(lp, domain, channel) %>%
  summarise(out = list(dalton_polarization(position, weight)), .groups = "drop") %>%
  unnest(out) %>%
  mutate(domain  = factor(domain,  levels = DOMAINS_B),
         channel = factor(channel, levels = c("Manifesto", "Speech")),
         variant = "Excl. AfD")

h4_all <- bind_rows(
  h4_main %>% mutate(variant = "All parties"),
  h4_no_afd
) %>%
  mutate(variant = factor(variant, levels = c("All parties", "Excl. AfD")))

write_csv(h4_all, file.path(PATHS$out_dir, "H4_system_polarization.csv"))
write_csv(h4_main %>% select(lp, domain, channel, mean_pos),
          file.path(PATHS$out_dir, "H4_mean_positions.csv"))

# ============================================================================
# Step 4 — Trend tests
# ============================================================================
message("[H4] Fitting per-(domain, channel) OLS trends ...")
trend_fits <- h4_main %>%
  filter(!is.na(polarization)) %>%
  group_by(domain, channel) %>%
  summarise(
    n          = n(),
    intercept  = coef(lm(polarization ~ lp))[1],
    slope      = coef(lm(polarization ~ lp))[2],
    slope_se   = summary(lm(polarization ~ lp))$coefficients[2, 2],
    slope_p    = summary(lm(polarization ~ lp))$coefficients[2, 4],
    r2         = summary(lm(polarization ~ lp))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(slope_lo = slope - 1.96 * slope_se,
         slope_hi = slope + 1.96 * slope_se)

write_csv(trend_fits, file.path(PATHS$out_dir, "H4_trend_fits.csv"))

# Pre-2017 vs Post-2017 means (LPs 13–18 vs 19–20)
prepost <- h4_main %>%
  filter(!is.na(polarization)) %>%
  mutate(period = if_else(lp <= 18, "Pre-2017 (LP 13–18)", "Post-2017 (LP 19–20)")) %>%
  group_by(domain, channel, period) %>%
  summarise(mean_pol = mean(polarization), n = n(), .groups = "drop")

prepost_wide <- prepost %>%
  pivot_wider(names_from = period, values_from = mean_pol) %>%
  mutate(delta = `Post-2017 (LP 19–20)` - `Pre-2017 (LP 13–18)`)

write_csv(prepost_wide, file.path(PATHS$out_dir, "H4_pre_post_2017.csv"))

# ============================================================================
# Step 5 — Plots
# ============================================================================
CHANNEL_COLOURS <- c("Manifesto" = "#264653", "Speech" = "#E76F51")

# Main figure: polarization over time, per domain, both channels
p_h4_main <- h4_main %>%
  ggplot(aes(x = lp, y = polarization, colour = channel, group = channel)) +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 18.5, y = Inf, label = "  AfD enters\n  (LP 19, 2017)",
           hjust = 0, vjust = 1.3, colour = "grey40", size = 3) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2) +
  labs(
    title    = "H4 — System-level polarization over time",
    subtitle = paste0(
      "Per-domain Dalton-weighted polarization across LPs 13–20.\n",
      "Manifesto channel weighted by vote share; speech channel weighted by seat share."
    ),
    x        = "Legislative period",
    y        = "Weighted polarization (SD of positions)",
    caption  = paste0(
      "Polarization = sqrt( Σ_p w_p · (pos_p − μ)² ) where μ is the weighted mean position.\n",
      "Vertical reference line at LP 18.5 marks AfD entry into Bundestag (LP 19, 2017).\n",
      "Per-domain decomposition shows post-2017 shifts concentrated in Europe (both channels)\n",
      "and Migration (speech channel)."
    )
  ) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H4_polarization_lines.pdf"),
       p_h4_main, width = 10, height = 7)
ggsave(file.path(PATHS$fig_dir, "H4_polarization_lines.png"),
       p_h4_main, width = 10, height = 7, dpi = 200, bg = "white")

# Same figure with AfD-exclusion overlay as dashed lines
p_h4_robust <- h4_all %>%
  ggplot(aes(x = lp, y = polarization, colour = channel,
             linetype = variant, group = interaction(channel, variant))) +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  scale_linetype_manual(values = c("All parties" = "solid", "Excl. AfD" = "dashed"),
                        name = NULL) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2) +
  labs(title    = "H4 — System polarization, with and without AfD",
       subtitle = "Solid: all six parties. Dashed: excluding AfD.",
       x = "Legislative period",
       y = "Weighted polarization") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H4_polarization_lines_robust.pdf"),
       p_h4_robust, width = 10, height = 7)

# Mean positions figure
p_h4_means <- h4_main %>%
  ggplot(aes(x = lp, y = mean_pos, colour = channel, group = channel)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  facet_wrap(~ domain, ncol = 2) +
  labs(title    = "H4 — Weighted mean position over time",
       subtitle = paste0(
         "Where the centre of mass of the German party system sits, per domain.\n",
         "Positive = market-liberal / retrenchment / restrictive / Eurosceptic."
       ),
       x = "Legislative period",
       y = "Weighted mean position",
       caption  = "Allows distinguishing 'parties moved together' (mean shifts) from 'parties moved apart' (polarization rises).") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H4_mean_positions.pdf"),
       p_h4_means, width = 10, height = 7)
ggsave(file.path(PATHS$fig_dir, "H4_mean_positions.png"),
       p_h4_means, width = 10, height = 7, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H4 TREND FITS ====================\n")
print(trend_fits %>%
        mutate(across(c(intercept, slope, slope_se, slope_lo, slope_hi, r2),
                      ~ round(., 4)),
               slope_p = signif(slope_p, 3)))

cat("\n==================== H4 PRE/POST 2017 ====================\n")
print(prepost_wide %>%
        mutate(across(where(is.numeric), ~ round(., 4))))

cat("\n==================== H4 POLARIZATION SUMMARY ====================\n")
print(h4_main %>%
        group_by(domain, channel) %>%
        summarise(min_pol  = min(polarization, na.rm = TRUE),
                  max_pol  = max(polarization, na.rm = TRUE),
                  mean_pol = mean(polarization, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(across(where(is.numeric), ~ round(., 3))))

message("\n[H4] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H4_system_polarization.csv"))
message("  - ", file.path(PATHS$out_dir, "H4_mean_positions.csv"))
message("  - ", file.path(PATHS$out_dir, "H4_trend_fits.csv"))
message("  - ", file.path(PATHS$out_dir, "H4_pre_post_2017.csv"))
message("  - ", file.path(PATHS$fig_dir, "H4_polarization_lines.pdf"))
message("  - ", file.path(PATHS$fig_dir, "H4_polarization_lines_robust.pdf"))
message("  - ", file.path(PATHS$fig_dir, "H4_mean_positions.pdf"))
