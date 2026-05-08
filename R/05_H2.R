# ============================================================================
# 05_H2.R — Hypotheses 2a, 2b, 2c
# ----------------------------------------------------------------------------
# H2a: per-bucket JSD contributions across parties — sorted bar chart.
# H2b: manifesto-polarization vs. speech-polarization scatter, by domain.
# H2c: inter-party SD of polarization scores by LP, with AfD entry marked.
#
# Outputs:
#   results/empirics/H2a_per_bucket_long.csv
#   results/empirics/H2a_per_bucket_summary.csv
#   results/empirics/figures/H2a_per_bucket_bar.pdf / .png
#   results/empirics/H2b_polarization_table.csv
#   results/empirics/H2b_regression_fits.csv
#   results/empirics/figures/H2b_polarization_scatter.pdf / .png
#   results/empirics/H2c_dispersion_table.csv
#   results/empirics/figures/H2c_dispersion_lines.pdf / .png
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))

.primary_A_key <- sprintf("soft_tau%s__A", format(TAU_PRIMARY, nsmall = 1))
.primary_B_key <- sprintf("soft_tau%s__B", format(TAU_PRIMARY, nsmall = 1))
speech_A    <- if (.primary_A_key %in% names(sd_list)) sd_list[[.primary_A_key]] else sd_list$A
speech_B    <- if (.primary_B_key %in% names(sd_list)) sd_list[[.primary_B_key]] else sd_list$B
manifesto_A <- md_list$A
manifesto_B <- md_list$B

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# H2a — Topic-level salience variation (per-bucket JSD contribution)
# (per_bucket_jsd is now in 02_helpers.R; reused here.)
# ============================================================================

message("[H2a] Computing per-bucket JSD contributions ...")

speech_A_wide <- speech_A %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

manifesto_A_wide <- manifesto_A %>%
  inner_join(m1_map, by = "election_date") %>%
  pivot_wider(id_cols = c(party, lp), names_from = bucket, values_from = share)

joined <- speech_A_wide %>%
  inner_join(manifesto_A_wide, by = c("party", "lp"), suffix = c("_S", "_M"))

# Precompute column selectors in BUCKETS_A order (avoids c_across name issue in dplyr 1.2.x)
s_cols <- paste0(BUCKETS_A, "_S")
m_cols <- paste0(BUCKETS_A, "_M")

h2a_long <- bind_rows(lapply(seq_len(nrow(joined)), function(i) {
  s   <- as.numeric(joined[i, s_cols]); names(s)   <- BUCKETS_A
  m_v <- as.numeric(joined[i, m_cols]); names(m_v) <- BUCKETS_A
  s[is.na(s)] <- 0; m_v[is.na(m_v)] <- 0
  contrib <- if (sum(s) == 0 || sum(m_v) == 0)
    setNames(rep(NA_real_, length(BUCKETS_A)), BUCKETS_A)
  else
    per_bucket_jsd(s, m_v)
  tibble(party = joined$party[i], lp = joined$lp[i],
         bucket = names(contrib), jsd_contrib = unname(contrib))
}))

h2a_summary <- h2a_long %>%
  group_by(bucket) %>%
  summarise(mean_contrib = mean(jsd_contrib, na.rm = TRUE),
            sd_contrib   = sd(jsd_contrib,   na.rm = TRUE),
            n_cells      = sum(!is.na(jsd_contrib)),
            .groups = "drop") %>%
  arrange(desc(mean_contrib))

write_csv(h2a_long,    file.path(PATHS$out_dir, "H2a_per_bucket_long.csv"))
write_csv(h2a_summary, file.path(PATHS$out_dir, "H2a_per_bucket_summary.csv"))

# H2a reframed (MIv2 §6.3): drop Reactive/Programmatic colouring; single
# neutral fill, with the Democracy & Political System bar singled out as a
# procedural-language artefact (parliamentary phrasing absent from manifestos).
DPS_BUCKET   <- "Democracy & Political System"
H2A_NEUTRAL  <- "#264653"
H2A_HIGHLIGHT<- "#E76F51"

h2a_plot_data <- h2a_summary %>%
  mutate(bucket = fct_reorder(bucket, mean_contrib),
         is_dps = bucket == DPS_BUCKET)

bucket_levels <- levels(h2a_plot_data$bucket)

p_h2a <- ggplot(h2a_plot_data,
                aes(x = mean_contrib, y = bucket, fill = is_dps)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_jitter(
    data = h2a_long %>%
      mutate(bucket = factor(bucket, levels = bucket_levels),
             is_dps = bucket == DPS_BUCKET),
    aes(x = jsd_contrib, y = bucket, colour = is_dps),
    inherit.aes = FALSE,
    height = 0.18, width = 0, size = 1.4, alpha = 0.55
  ) +
  geom_text(
    data = h2a_plot_data %>% filter(is_dps),
    aes(x = mean_contrib, y = bucket,
        label = "procedural-language\nartefact"),
    hjust = -0.05, vjust = 0.5, size = 3, colour = "#A23B22",
    inherit.aes = FALSE, lineheight = 0.9
  ) +
  scale_fill_manual(values = c(`FALSE` = H2A_NEUTRAL, `TRUE` = H2A_HIGHLIGHT),
                    guide = "none") +
  scale_colour_manual(values = c(`FALSE` = "#1A3640", `TRUE` = "#A23B22"),
                      guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "H2a — Topic-level salience divergence",
    subtitle = paste0(
      "Mean per-bucket contribution to JSD across cells; dots = individual (party, LP) cells.\n",
      "Procedural and institutional language inflates Democracy & Political System on the\n",
      "speech side relative to manifestos (genre artefact, see MIv2 §6.3)."
    ),
    x        = "Mean JSD contribution",
    y        = NULL,
    caption  = paste0(
      "The Democracy & Political System bar reflects parliamentary procedural-rhetorical\n",
      "phrasing absent from manifestos rather than substantive divergence on democratic-\n",
      "political topics. See MIv2 §6.4 for a tightened pre-filter robustness check."
    )
  ) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H2a_per_bucket_bar.pdf"),  p_h2a, width = 8.5, height = 6)
ggsave(file.path(PATHS$fig_dir, "H2a_per_bucket_bar.png"),  p_h2a, width = 8.5, height = 6,
       dpi = 200, bg = "white")

# ============================================================================
# H2b — Directional moderation: manifesto vs. speech polarization
# ============================================================================
# Polarization score per domain (signed [-1, +1]):
#   Economy:   share(Marktlib) - share(Staatsint)   [Wirtsch. Allg. in denominator]
#   Welfare:   share(Begrenzung) - share(Ausbau)
#   Migration: share(Restriktiv) - share(Liberal)
#   Europe:    share(Contra-EU) - share(Pro-EU)
# All shares renormalized within-domain before scoring.

message("\n[H2b] Computing per-domain polarization scores ...")
# polarization_score is now in 02_helpers.R (single canonical implementation
# shared with 06_H3.R and 07_H4.R).

sp_pol_rows <- vector("list", nrow(distinct(speech_B, party, lp)))
for (i in seq_len(nrow(distinct(speech_B, party, lp)))) {
  cell <- distinct(speech_B, party, lp)[i, ]
  out  <- polarization_score(speech_B, cell$party, cell$lp, "lp")
  sp_pol_rows[[i]] <- bind_cols(cell, out)
}
speech_pol <- bind_rows(sp_pol_rows) %>%
  pivot_longer(c(Economy, Welfare, Migration, Europe),
               names_to = "domain", values_to = "speech_pol")

mf_pol_rows <- vector("list", nrow(distinct(manifesto_B, party, election_date)))
for (i in seq_len(nrow(distinct(manifesto_B, party, election_date)))) {
  cell <- distinct(manifesto_B, party, election_date)[i, ]
  out  <- polarization_score(manifesto_B, cell$party, cell$election_date, "election_date")
  mf_pol_rows[[i]] <- bind_cols(cell, out)
}
manifesto_pol <- bind_rows(mf_pol_rows) %>%
  pivot_longer(c(Economy, Welfare, Migration, Europe),
               names_to = "domain", values_to = "manifesto_pol")

h2b_long <- speech_pol %>%
  inner_join(m1_map, by = "lp") %>%
  inner_join(manifesto_pol, by = c("party", "election_date", "domain")) %>%
  mutate(domain = factor(domain, levels = DOMAINS_B),
         party  = factor(party,  levels = PARTIES_KEEP))

write_csv(h2b_long, file.path(PATHS$out_dir, "H2b_polarization_table.csv"))

h2b_fits <- h2b_long %>%
  filter(!is.na(speech_pol), !is.na(manifesto_pol)) %>%
  group_by(domain) %>%
  summarise(
    n         = n(),
    intercept = coef(lm(speech_pol ~ manifesto_pol))[1],
    slope     = coef(lm(speech_pol ~ manifesto_pol))[2],
    r2        = summary(lm(speech_pol ~ manifesto_pol))$r.squared,
    .groups = "drop"
  )
write_csv(h2b_fits, file.path(PATHS$out_dir, "H2b_regression_fits.csv"))

p_h2b <- h2b_long %>%
  ggplot(aes(x = manifesto_pol, y = speech_pol, colour = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.6) +
  geom_point(size = 2.5, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)),
                            size = 2.6, max.overlaps = 12, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  facet_wrap(~ domain, scales = "free", ncol = 2) +
  labs(
    title    = "H2b — Directional moderation in parliamentary speech",
    subtitle = "Polarization score: signed within-domain balance of directional sub-labels",
    x        = "Manifesto polarization (M1)",
    y        = "Speech polarization",
    caption  = paste0(
      "Dashed line: 45° (perfect consistency). Solid line: per-domain OLS fit.\n",
      "Slope < 1 indicates systematic moderation."
    )
  ) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H2b_polarization_scatter.pdf"),
       p_h2b, width = 10, height = 8)
ggsave(file.path(PATHS$fig_dir, "H2b_polarization_scatter.png"),
       p_h2b, width = 10, height = 8, dpi = 200, bg = "white")

# ============================================================================
# H2c — Inter-party dispersion of polarization scores (Dalton-weighted)
# ----------------------------------------------------------------------------
# MIv2 §6.6: weighted standard deviation of party position scores, Dalton-
# style. Speech channel weighted by SEAT_SHARES (per LP); manifesto channel
# weighted by VOTE_SHARES (per election, projected onto LP via M1 mapping).
# ============================================================================
message("\n[H2c] Computing Dalton-weighted inter-party dispersion ...")

# Speech-side (all 6 parties)
disp_speech_all <- speech_pol %>%
  inner_join(SEAT_SHARES, by = c("party", "lp")) %>%
  group_by(lp, domain) %>%
  summarise(out = list(dalton_polarization(speech_pol, seat_share)),
            .groups = "drop") %>%
  unnest(out) %>%
  transmute(lp, domain, sd_speech_all = polarization)

# Speech-side (excluding AfD)
disp_speech_noafd <- speech_pol %>%
  filter(party != "AfD") %>%
  inner_join(SEAT_SHARES, by = c("party", "lp")) %>%
  group_by(lp, domain) %>%
  summarise(out = list(dalton_polarization(speech_pol, seat_share)),
            .groups = "drop") %>%
  unnest(out) %>%
  transmute(lp, domain, sd_speech_no_afd = polarization)

# Manifesto-side (all 6 parties), projected onto LP via M1 mapping
disp_manifesto <- manifesto_pol %>%
  inner_join(VOTE_SHARES, by = c("party", "election_date")) %>%
  inner_join(m1_map, by = "election_date") %>%
  group_by(lp, domain) %>%
  summarise(out = list(dalton_polarization(manifesto_pol, vote_share)),
            .groups = "drop") %>%
  unnest(out) %>%
  transmute(lp, domain, sd_manifesto = polarization)

# Manifesto-side (excluding AfD)
disp_manifesto_noafd <- manifesto_pol %>%
  filter(party != "AfD") %>%
  inner_join(VOTE_SHARES, by = c("party", "election_date")) %>%
  inner_join(m1_map, by = "election_date") %>%
  group_by(lp, domain) %>%
  summarise(out = list(dalton_polarization(manifesto_pol, vote_share)),
            .groups = "drop") %>%
  unnest(out) %>%
  transmute(lp, domain, sd_manifesto_no_afd = polarization)

h2c_long <- disp_manifesto %>%
  full_join(disp_manifesto_noafd, by = c("lp", "domain")) %>%
  full_join(disp_speech_all,      by = c("lp", "domain")) %>%
  full_join(disp_speech_noafd,    by = c("lp", "domain")) %>%
  pivot_longer(starts_with("sd_"), names_to = "series", values_to = "sd_polarization") %>%
  mutate(
    series = factor(series,
                    levels = c("sd_manifesto", "sd_manifesto_no_afd",
                               "sd_speech_all", "sd_speech_no_afd"),
                    labels = c("Manifesto (all)", "Manifesto (excl. AfD)",
                               "Speech (all)", "Speech (excl. AfD)")),
    domain = factor(domain, levels = DOMAINS_B)
  )

write_csv(h2c_long, file.path(PATHS$out_dir, "H2c_dispersion_table.csv"))

p_h2c <- h2c_long %>%
  ggplot(aes(x = lp, y = sd_polarization,
             colour = series, linetype = series, group = series)) +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 18.5, y = Inf, label = "  AfD enters",
           hjust = 0, vjust = 1.4, colour = "grey40", size = 3) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(
    values = c("Manifesto (all)"        = "#264653",
               "Manifesto (excl. AfD)"  = "#264653",
               "Speech (all)"           = "#E76F51",
               "Speech (excl. AfD)"     = "#E76F51"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Manifesto (all)"        = "dashed",
               "Manifesto (excl. AfD)"  = "dotted",
               "Speech (all)"           = "solid",
               "Speech (excl. AfD)"     = "twodash"),
    name = NULL
  ) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2) +
  labs(
    title    = "H2c — Inter-party dispersion: manifesto vs. speech",
    subtitle = "Dalton-weighted SD of party polarization scores per LP, by domain",
    x        = "Legislative period",
    y        = "Weighted SD of party polarization score",
    caption  = paste0(
      "Manifesto channel weighted by vote share (Dalton standard); speech channel weighted by seat share.\n",
      "AfD entered the Bundestag in LP 19 (2017).\n",
      "Gap between solid and dashed = polarization differential between channels;\n",
      "gap between solid and twodash = AfD's compositional contribution to speech-side dispersion."
    )
  ) +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H2c_dispersion_lines.pdf"),
       p_h2c, width = 10, height = 7)
ggsave(file.path(PATHS$fig_dir, "H2c_dispersion_lines.png"),
       p_h2c, width = 10, height = 7, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H2a TOP 5 / BOTTOM 5 ====================\n")
print(h2a_summary %>% slice_max(mean_contrib, n = 5))
print(h2a_summary %>% slice_min(mean_contrib, n = 5))

cat("\n==================== H2b OLS FITS ====================\n")
print(h2b_fits)

cat("\n==================== H2c BY-DOMAIN MEANS ====================\n")
print(h2c_long %>%
        group_by(domain, series) %>%
        summarise(mean_sd = mean(sd_polarization, na.rm = TRUE), .groups = "drop"))

message("\n[H2] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H2a_per_bucket_long.csv"))
message("  - ", file.path(PATHS$out_dir, "H2a_per_bucket_summary.csv"))
message("  - ", file.path(PATHS$fig_dir, "H2a_per_bucket_bar.pdf"))
message("  - ", file.path(PATHS$out_dir, "H2b_polarization_table.csv"))
message("  - ", file.path(PATHS$out_dir, "H2b_regression_fits.csv"))
message("  - ", file.path(PATHS$fig_dir, "H2b_polarization_scatter.pdf"))
message("  - ", file.path(PATHS$out_dir, "H2c_dispersion_table.csv"))
message("  - ", file.path(PATHS$fig_dir, "H2c_dispersion_lines.pdf"))
