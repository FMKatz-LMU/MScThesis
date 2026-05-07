# ============================================================================
# 06_H6.R — System-level polarization over time
# ----------------------------------------------------------------------------
# H6: Polarization in the German party system has increased over LPs 13–20.
#     Trend is observable on both channels (manifesto, speech) and is most
#     pronounced in Migration and Europe after 2015–2017.
#
# Operationalization: Dalton-style weighted dispersion of polarization scores.
#
#   Polarization(LP, domain, channel) = sqrt( Σ_p w_p · (pos_p − μ)² )
#   where μ = Σ_p w_p · pos_p   (weighted mean position)
#
# Weights:
#   - Manifesto channel: vote share at the election (Dalton standard)
#   - Speech channel:    seat share in the LP (institutional weight)
#
# Position scores (signed, in [-1, +1]) come from polarization_score()
# in 05_H2.R; we recompute here using the same logic for transparency.
#
# Outputs:
#   results/empirics/H6_system_polarization.csv
#   results/empirics/H6_mean_positions.csv
#   results/empirics/H6_trend_fits.csv
#   results/empirics/H6_pre_post_2017.csv
#   results/empirics/figures/H6_polarization_lines.pdf
#   results/empirics/figures/H6_mean_positions.pdf
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))

speech_B    <- sd_list$B
manifesto_B <- md_list$B

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# WEIGHT LOOKUPS — VERIFY THESE BEFORE TRUSTING RESULTS
# ----------------------------------------------------------------------------
# Source: Bundeswahlleiterin (official federal election results) and
# Deutscher Bundestag (official seat distributions per LP, after corrections).
# All numbers as percentages or absolute seat counts at the START of the LP
# (before any defections/changes during the term).
# ============================================================================

# Vote shares (Zweitstimmen) per federal election, per party.
# Parties not on the ballot or below 5%-Hürde get 0; we still keep them in
# the table so the join is total. A party with 0 vote share contributes
# nothing to weighted polarization, which is correct.
VOTE_SHARES <- tribble(
  ~election_date,           ~party,      ~vote_share,
  # 1994 federal election
  as.Date("1994-10-16"),    "CDU/CSU",   41.5,
  as.Date("1994-10-16"),    "SPD",       36.4,
  as.Date("1994-10-16"),    "FDP",       6.9,
  as.Date("1994-10-16"),    "Grüne",     7.3,
  as.Date("1994-10-16"),    "Linke",     4.4,    # PDS
  as.Date("1994-10-16"),    "AfD",       0,
  # 1998
  as.Date("1998-09-27"),    "CDU/CSU",   35.1,
  as.Date("1998-09-27"),    "SPD",       40.9,
  as.Date("1998-09-27"),    "FDP",       6.2,
  as.Date("1998-09-27"),    "Grüne",     6.7,
  as.Date("1998-09-27"),    "Linke",     5.1,    # PDS
  as.Date("1998-09-27"),    "AfD",       0,
  # 2002
  as.Date("2002-09-22"),    "CDU/CSU",   38.5,
  as.Date("2002-09-22"),    "SPD",       38.5,
  as.Date("2002-09-22"),    "FDP",       7.4,
  as.Date("2002-09-22"),    "Grüne",     8.6,
  as.Date("2002-09-22"),    "Linke",     4.0,    # PDS, below 5%
  as.Date("2002-09-22"),    "AfD",       0,
  # 2005
  as.Date("2005-09-18"),    "CDU/CSU",   35.2,
  as.Date("2005-09-18"),    "SPD",       34.2,
  as.Date("2005-09-18"),    "FDP",       9.8,
  as.Date("2005-09-18"),    "Grüne",     8.1,
  as.Date("2005-09-18"),    "Linke",     8.7,    # Linkspartei.PDS
  as.Date("2005-09-18"),    "AfD",       0,
  # 2009
  as.Date("2009-09-27"),    "CDU/CSU",   33.8,
  as.Date("2009-09-27"),    "SPD",       23.0,
  as.Date("2009-09-27"),    "FDP",       14.6,
  as.Date("2009-09-27"),    "Grüne",     10.7,
  as.Date("2009-09-27"),    "Linke",     11.9,
  as.Date("2009-09-27"),    "AfD",       0,
  # 2013
  as.Date("2013-09-22"),    "CDU/CSU",   41.5,
  as.Date("2013-09-22"),    "SPD",       25.7,
  as.Date("2013-09-22"),    "FDP",       4.8,    # below 5%-Hürde
  as.Date("2013-09-22"),    "Grüne",     8.4,
  as.Date("2013-09-22"),    "Linke",     8.6,
  as.Date("2013-09-22"),    "AfD",       4.7,    # below 5%-Hürde, manifesto exists
  # 2017
  as.Date("2017-09-24"),    "CDU/CSU",   32.9,
  as.Date("2017-09-24"),    "SPD",       20.5,
  as.Date("2017-09-24"),    "FDP",       10.7,
  as.Date("2017-09-24"),    "Grüne",     8.9,
  as.Date("2017-09-24"),    "Linke",     9.2,
  as.Date("2017-09-24"),    "AfD",       12.6,
  # 2021
  as.Date("2021-09-26"),    "CDU/CSU",   24.1,
  as.Date("2021-09-26"),    "SPD",       25.7,
  as.Date("2021-09-26"),    "FDP",       11.5,
  as.Date("2021-09-26"),    "Grüne",     14.8,
  as.Date("2021-09-26"),    "Linke",     4.9,    # below 5% but Grundmandate
  as.Date("2021-09-26"),    "AfD",       10.3
)

# Seat shares per LP. These are at LP start (post-Konstituierung), as
# percentage of total Bundestag seats. Rounded to 0.1.
# Source: bundestag.de "Sitzverteilung" historical archive.
SEAT_SHARES <- tribble(
  ~lp, ~party,     ~seat_share,
  # LP 13 (1994–1998): 672 seats total
  13, "CDU/CSU",   43.8,
  13, "SPD",       37.5,
  13, "FDP",       7.0,
  13, "Grüne",     7.3,
  13, "Linke",     4.4,    # PDS, 30 seats
  13, "AfD",       0,
  # LP 14 (1998–2002): 669 seats
  14, "CDU/CSU",   36.6,
  14, "SPD",       44.5,
  14, "FDP",       6.3,
  14, "Grüne",     7.0,
  14, "Linke",     5.4,    # PDS, 36 seats
  14, "AfD",       0,
  # LP 15 (2002–2005): 603 seats — PDS only had 2 direct mandates
  15, "CDU/CSU",   41.1,
  15, "SPD",       41.6,
  15, "FDP",       7.8,
  15, "Grüne",     9.1,
  15, "Linke",     0.3,    # PDS 2 Direktmandate, no fraction status
  15, "AfD",       0,
  # LP 16 (2005–2009): 614 seats
  16, "CDU/CSU",   36.8,
  16, "SPD",       36.2,
  16, "FDP",       9.9,
  16, "Grüne",     8.3,
  16, "Linke",     8.8,
  16, "AfD",       0,
  # LP 17 (2009–2013): 622 seats
  17, "CDU/CSU",   38.4,
  17, "SPD",       23.5,
  17, "FDP",       15.0,
  17, "Grüne",     10.9,
  17, "Linke",     12.2,
  17, "AfD",       0,
  # LP 18 (2013–2017): 631 seats — FDP, AfD out
  18, "CDU/CSU",   49.3,
  18, "SPD",       30.6,
  18, "FDP",       0,
  18, "Grüne",     10.0,
  18, "Linke",     10.1,
  18, "AfD",       0,
  # LP 19 (2017–2021): 709 seats
  19, "CDU/CSU",   34.7,
  19, "SPD",       21.6,
  19, "FDP",       11.3,
  19, "Grüne",     9.4,
  19, "Linke",     9.7,
  19, "AfD",       13.3,
  # LP 20 (2021–2025): 736 seats
  20, "CDU/CSU",   26.6,
  20, "SPD",       28.0,
  20, "FDP",       12.5,
  20, "Grüne",     16.0,
  20, "Linke",     5.3,
  20, "AfD",       11.7
)

# Sanity checks
stopifnot(all(VOTE_SHARES$party  %in% PARTIES_KEEP))
stopifnot(all(SEAT_SHARES$party  %in% PARTIES_KEEP))

# Vote shares per election should be ≤ 100 (smaller parties not in our 6 sum
# to the rest, that's fine — they don't enter the polarization measure).
.vs_check <- VOTE_SHARES %>% group_by(election_date) %>%
  summarise(total = sum(vote_share), .groups = "drop")
message("[H6] Vote share totals per election (our 6 parties):")
print(.vs_check)
stopifnot(all(.vs_check$total <= 100.5))

.ss_check <- SEAT_SHARES %>% group_by(lp) %>%
  summarise(total = sum(seat_share), .groups = "drop")
message("[H6] Seat share totals per LP (our 6 parties):")
print(.ss_check)
stopifnot(all(.ss_check$total <= 100.5))

# ============================================================================
# Step 1 — Recompute per-cell polarization scores for both channels
# (Reuses the logic from H2b. We re-implement here to avoid sourcing 05_H2.R.)
# ============================================================================

position_score <- function(df_long, party_, scope_val, scope_col) {
  d <- df_long %>%
    filter(.data$party == party_, .data[[scope_col]] == scope_val,
           bucket %in% c(BUCKETS_B, "Andere"))

  pull_pair <- function(sub_df, pos_label, neg_label) {
    s <- sub_df$share[sub_df$bucket == pos_label]
    n <- sub_df$share[sub_df$bucket == neg_label]
    s <- if (length(s)) s else 0
    n <- if (length(n)) n else 0
    tot <- s + n
    if (tot <= 0) return(NA_real_)
    (s - n) / tot
  }

  pull_econ <- function(sub_df) {
    ml <- sub_df$share[sub_df$bucket == "Marktliberalismus"]
    si <- sub_df$share[sub_df$bucket == "Staatsintervention"]
    ml <- if (length(ml)) ml else 0
    si <- if (length(si)) si else 0
    tot <- ml + si
    if (tot <= 0) return(NA_real_)
    (ml - si) / tot
  }

  econ <- d %>% filter(bucket %in% c("Marktliberalismus", "Staatsintervention"))
  welf <- d %>% filter(bucket %in% c("Sozialstaat Ausbau", "Sozialstaat Begrenzung"))
  migr <- d %>% filter(bucket %in% c("Migration restriktiv", "Migration liberal"))
  euro <- d %>% filter(bucket %in% c("Pro-EU", "Contra-EU"))

  tibble(
    Economy   = pull_econ(econ),
    Welfare   = pull_pair(welf, "Sozialstaat Begrenzung", "Sozialstaat Ausbau"),
    Migration = pull_pair(migr, "Migration restriktiv",   "Migration liberal"),
    Europe    = pull_pair(euro, "Contra-EU",              "Pro-EU")
  )
}

message("[H6] Computing per-cell positions (speech) ...")
sp_pos <- distinct(speech_B, party, lp) %>%
  rowwise() %>%
  mutate(pos = list(position_score(speech_B, party, lp, "lp"))) %>%
  unnest(pos) %>%
  ungroup() %>%
  pivot_longer(c(Economy, Welfare, Migration, Europe),
               names_to = "domain", values_to = "position")

message("[H6] Computing per-cell positions (manifesto) ...")
mf_pos <- distinct(manifesto_B, party, election_date) %>%
  rowwise() %>%
  mutate(pos = list(position_score(manifesto_B, party, election_date, "election_date"))) %>%
  unnest(pos) %>%
  ungroup() %>%
  pivot_longer(c(Economy, Welfare, Migration, Europe),
               names_to = "domain", values_to = "position")

# ============================================================================
# Step 2 — Attach weights
# ============================================================================

# Speech channel: seat shares per LP
sp_weighted <- sp_pos %>%
  inner_join(SEAT_SHARES, by = c("party", "lp")) %>%
  rename(weight = seat_share) %>%
  mutate(channel = "Speech")

# Manifesto channel: vote shares per election; project onto the LP they produce
mf_weighted <- mf_pos %>%
  inner_join(VOTE_SHARES, by = c("party", "election_date")) %>%
  rename(weight = vote_share) %>%
  inner_join(m1_map, by = "election_date") %>%   # attach LP via M1
  mutate(channel = "Manifesto") %>%
  select(party, lp, election_date, domain, position, weight, channel)

sp_weighted <- sp_weighted %>%
  inner_join(m1_map, by = "lp") %>%
  select(party, lp, election_date, domain, position, weight, channel)

both <- bind_rows(sp_weighted, mf_weighted)

# ============================================================================
# Step 3 — Compute Dalton-style weighted polarization per (LP, domain, channel)
# ============================================================================

dalton_polarization <- function(positions, weights) {
  ok <- !is.na(positions) & !is.na(weights) & weights > 0
  if (sum(ok) < 2) return(tibble(mean_pos = NA_real_, polarization = NA_real_, n_parties = sum(ok)))
  p <- positions[ok]; w <- weights[ok]
  w <- w / sum(w)                  # renormalize after dropping NAs
  mu <- sum(w * p)
  sigma <- sqrt(sum(w * (p - mu)^2))
  tibble(mean_pos = mu, polarization = sigma, n_parties = sum(ok))
}

message("[H6] Computing Dalton-weighted polarization ...")
h6_main <- both %>%
  group_by(lp, domain, channel) %>%
  summarise(out = list(dalton_polarization(position, weight)), .groups = "drop") %>%
  unnest(out) %>%
  mutate(domain  = factor(domain,  levels = DOMAINS_B),
         channel = factor(channel, levels = c("Manifesto", "Speech")))

# Robustness: same measure excluding AfD
h6_no_afd <- both %>%
  filter(party != "AfD") %>%
  group_by(lp, domain, channel) %>%
  summarise(out = list(dalton_polarization(position, weight)), .groups = "drop") %>%
  unnest(out) %>%
  mutate(domain  = factor(domain,  levels = DOMAINS_B),
         channel = factor(channel, levels = c("Manifesto", "Speech")),
         variant = "Excl. AfD")

h6_all <- bind_rows(
  h6_main %>% mutate(variant = "All parties"),
  h6_no_afd
) %>%
  mutate(variant = factor(variant, levels = c("All parties", "Excl. AfD")))

write_csv(h6_all, file.path(PATHS$out_dir, "H6_system_polarization.csv"))
write_csv(h6_main %>% select(lp, domain, channel, mean_pos),
          file.path(PATHS$out_dir, "H6_mean_positions.csv"))

# ============================================================================
# Step 4 — Trend tests
# ============================================================================

# (a) Per (domain, channel) OLS regression: polarization ~ lp
message("[H6] Fitting per-(domain, channel) OLS trends ...")
trend_fits <- h6_main %>%
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

write_csv(trend_fits, file.path(PATHS$out_dir, "H6_trend_fits.csv"))

# (b) Pre-2017 vs Post-2017 means (LPs 13–18 vs 19–20)
prepost <- h6_main %>%
  filter(!is.na(polarization)) %>%
  mutate(period = if_else(lp <= 18, "Pre-2017 (LP 13–18)", "Post-2017 (LP 19–20)")) %>%
  group_by(domain, channel, period) %>%
  summarise(mean_pol = mean(polarization), n = n(), .groups = "drop")

prepost_wide <- prepost %>%
  pivot_wider(names_from = period, values_from = mean_pol) %>%
  mutate(delta = `Post-2017 (LP 19–20)` - `Pre-2017 (LP 13–18)`)

write_csv(prepost_wide, file.path(PATHS$out_dir, "H6_pre_post_2017.csv"))

# ============================================================================
# Step 5 — Plots
# ============================================================================

CHANNEL_COLOURS <- c("Manifesto" = "#264653", "Speech" = "#E76F51")

# Main figure: polarization over time, per domain, both channels
p_h6_main <- h6_main %>%
  ggplot(aes(x = lp, y = polarization, colour = channel, group = channel)) +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 18.5, y = Inf, label = "  AfD enters\n  (2017)",
           hjust = 0, vjust = 1.3, colour = "grey40", size = 3) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2) +
  labs(title    = "H6 — System-level polarization over time",
       subtitle = "Dalton-weighted dispersion of party position scores per (LP, domain).\nManifesto channel weighted by vote share; speech channel weighted by seat share.",
       x        = "Legislative period",
       y        = "Weighted polarization (SD of positions)",
       caption  = "Polarization = sqrt( Σ_p w_p · (pos_p − μ)² ) where μ is the weighted mean position.\nDotted line: AfD enters the Bundestag in LP 19.") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H6_polarization_lines.pdf"),
       p_h6_main, width = 10, height = 7)
ggsave(file.path(PATHS$fig_dir, "H6_polarization_lines.png"),
       p_h6_main, width = 10, height = 7, dpi = 200, bg = "white")

# Same figure, with AfD-exclusion overlay as faint lines
p_h6_robust <- h6_all %>%
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
  labs(title    = "H6 — System polarization, with and without AfD",
       subtitle = "Solid: all six parties. Dashed: excluding AfD.",
       x = "Legislative period",
       y = "Weighted polarization") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H6_polarization_lines_robust.pdf"),
       p_h6_robust, width = 10, height = 7)

# Mean positions figure
p_h6_means <- h6_main %>%
  ggplot(aes(x = lp, y = mean_pos, colour = channel, group = channel)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  facet_wrap(~ domain, ncol = 2) +
  labs(title    = "H6 — Weighted mean position over time",
       subtitle = "Where the centre of mass of the German party system sits, per domain.\nPositive = market-liberal / retrenchment / restrictive / Eurosceptic.",
       x = "Legislative period",
       y = "Weighted mean position",
       caption  = "Allows distinguishing 'parties moved together' (mean shifts) from 'parties moved apart' (polarization rises).") +
  theme_thesis()

ggsave(file.path(PATHS$fig_dir, "H6_mean_positions.pdf"),
       p_h6_means, width = 10, height = 7)
ggsave(file.path(PATHS$fig_dir, "H6_mean_positions.png"),
       p_h6_means, width = 10, height = 7, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H6 TREND FITS ====================\n")
print(trend_fits %>%
        mutate(across(c(intercept, slope, slope_se, slope_lo, slope_hi, r2),
                      ~ round(., 4)),
               slope_p = signif(slope_p, 3)))

cat("\n==================== H6 PRE/POST 2017 ====================\n")
print(prepost_wide %>%
        mutate(across(where(is.numeric), ~ round(., 4))))

cat("\n==================== H6 POLARIZATION SUMMARY ====================\n")
print(h6_main %>%
        group_by(domain, channel) %>%
        summarise(min_pol  = min(polarization, na.rm = TRUE),
                  max_pol  = max(polarization, na.rm = TRUE),
                  mean_pol = mean(polarization, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(across(where(is.numeric), ~ round(., 3))))

message("\n[H6] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H6_system_polarization.csv"))
message("  - ", file.path(PATHS$out_dir, "H6_mean_positions.csv"))
message("  - ", file.path(PATHS$out_dir, "H6_trend_fits.csv"))
message("  - ", file.path(PATHS$out_dir, "H6_pre_post_2017.csv"))
message("  - ", file.path(PATHS$fig_dir, "H6_polarization_lines.pdf"))
message("  - ", file.path(PATHS$fig_dir, "H6_polarization_lines_robust.pdf"))
message("  - ", file.path(PATHS$fig_dir, "H6_mean_positions.pdf"))
