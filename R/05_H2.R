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

speech_A    <- sd_list$A
speech_B    <- sd_list$B
manifesto_A <- md_list$A
manifesto_B <- md_list$B

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# H2a — Topic-level salience variation (per-bucket JSD contribution)
# ============================================================================
per_bucket_jsd <- function(p, q) {
  stopifnot(length(p) == length(q), !is.null(names(p)), all(names(p) == names(q)))
  p <- p / sum(p); q <- q / sum(q)
  m <- 0.5 * (p + q)
  out <- numeric(length(p)); names(out) <- names(p)
  for (i in seq_along(p)) {
    a <- 0
    if (p[i] > 0 && m[i] > 0) a <- a + 0.5 * p[i] * log2(p[i] / m[i])
    if (q[i] > 0 && m[i] > 0) a <- a + 0.5 * q[i] * log2(q[i] / m[i])
    out[i] <- a
  }
  out
}

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

REACTIVE_BUCKETS <- c("Migration", "Foreign Policy & Defence", "European Integration")

h2a_summary <- h2a_long %>%
  group_by(bucket) %>%
  summarise(mean_contrib = mean(jsd_contrib, na.rm = TRUE),
            sd_contrib   = sd(jsd_contrib,   na.rm = TRUE),
            n_cells      = sum(!is.na(jsd_contrib)),
            .groups = "drop") %>%
  mutate(bucket_type = if_else(bucket %in% REACTIVE_BUCKETS, "Reactive", "Programmatic")) %>%
  arrange(desc(mean_contrib))

write_csv(h2a_long,    file.path(PATHS$out_dir, "H2a_per_bucket_long.csv"))
write_csv(h2a_summary, file.path(PATHS$out_dir, "H2a_per_bucket_summary.csv"))

p_h2a <- h2a_summary %>%
  mutate(bucket = fct_reorder(bucket, mean_contrib)) %>%
  ggplot(aes(x = mean_contrib, y = bucket, fill = bucket_type)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_jitter(
    data = h2a_long %>%
      inner_join(h2a_summary %>% select(bucket, bucket_type), by = "bucket") %>%
      mutate(bucket = factor(bucket,
                             levels = levels(fct_reorder(h2a_summary$bucket,
                                                          h2a_summary$mean_contrib)))),
    aes(x = jsd_contrib, y = bucket, colour = bucket_type),
    inherit.aes = FALSE,
    height = 0.18, width = 0, size = 1.4, alpha = 0.55
  ) +
  scale_fill_manual(values = c("Reactive" = "#E76F51", "Programmatic" = "#2A9D8F"),
                    name = NULL) +
  scale_colour_manual(values = c("Reactive" = "#A23B22", "Programmatic" = "#1A6F66"),
                      name = NULL, guide = "none") +
  labs(
    title    = "H2a — Topic-level salience divergence",
    subtitle = "Mean per-bucket contribution to JSD; dots = individual (party, LP) cells",
    x        = "Mean JSD contribution",
    y        = NULL,
    caption  = "Reactive = topics whose parliamentary presence is largely event-driven."
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

polarization_score <- function(df_long, party_val, scope_val, scope_col) {
  full_b <- c(BUCKETS_B, "Andere")
  d <- df_long %>%
    filter(.data$party          == .env$party_val,
           .data[[scope_col]]   == .env$scope_val,
           bucket %in% full_b)

  norm <- function(x, name) {
    s <- sum(x$share)
    if (s <= 0) return(setNames(rep(NA_real_, length(name)), name))
    setNames(x$share[match(name, x$bucket)], name) / s
  }

  econ <- d %>% filter(bucket %in% c("Marktliberalismus", "Staatsintervention", "Wirtschaft Allgemein"))
  welf <- d %>% filter(bucket %in% c("Sozialstaat Ausbau", "Sozialstaat Begrenzung"))
  migr <- d %>% filter(bucket %in% c("Migration restriktiv", "Migration liberal"))
  euro <- d %>% filter(bucket %in% c("Pro-EU", "Contra-EU"))

  econ_n <- norm(econ, c("Marktliberalismus", "Staatsintervention"))
  welf_n <- norm(welf, c("Sozialstaat Begrenzung", "Sozialstaat Ausbau"))
  migr_n <- norm(migr, c("Migration restriktiv", "Migration liberal"))
  euro_n <- norm(euro, c("Contra-EU", "Pro-EU"))

  tibble(
    Economy   = if (anyNA(econ_n)) NA_real_ else unname(econ_n[1] - econ_n[2]),
    Welfare   = if (anyNA(welf_n)) NA_real_ else unname(welf_n[1] - welf_n[2]),
    Migration = if (anyNA(migr_n)) NA_real_ else unname(migr_n[1] - migr_n[2]),
    Europe    = if (anyNA(euro_n)) NA_real_ else unname(euro_n[1] - euro_n[2])
  )
}

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
# H2c — Inter-party dispersion of polarization scores
# ============================================================================
message("\n[H2c] Computing inter-party dispersion ...")

disp_speech_all <- speech_pol %>%
  group_by(lp, domain) %>%
  summarise(sd_speech_all = sd(speech_pol, na.rm = TRUE), .groups = "drop")

disp_speech_noafd <- speech_pol %>%
  filter(party != "AfD") %>%
  group_by(lp, domain) %>%
  summarise(sd_speech_no_afd = sd(speech_pol, na.rm = TRUE), .groups = "drop")

disp_manifesto <- manifesto_pol %>%
  inner_join(m1_map, by = "election_date") %>%
  group_by(lp, domain) %>%
  summarise(sd_manifesto = sd(manifesto_pol, na.rm = TRUE), .groups = "drop")

disp_manifesto_noafd <- manifesto_pol %>%
  filter(party != "AfD") %>%
  inner_join(m1_map, by = "election_date") %>%
  group_by(lp, domain) %>%
  summarise(sd_manifesto_no_afd = sd(manifesto_pol, na.rm = TRUE), .groups = "drop")

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
    subtitle = "SD of party polarization scores per LP, by domain",
    x        = "Legislative period",
    y        = "SD of party polarization score",
    caption  = paste0(
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
