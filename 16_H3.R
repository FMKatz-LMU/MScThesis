# ============================================================================
# 16_H3.R — System-level polarization over time   (formerly H4; H3-ownership cut)
# ----------------------------------------------------------------------------
# DESCRIPTIVE: how the spread of party positions evolves across LPs 13–20, per
# domain and channel. Reframed as descriptive (not a validity-dependent claim);
# the directional (B) layer is EXPLORATORY (gold κ(B) = 0.456). Emphasis on RELATIVE
# change over time (more robust than absolute position levels).
#
#   Polarization(LP, domain, channel) = Dalton-weighted SD of position scores
#     = sqrt( Σ_p w_p · (pos_p − μ)² ),  μ = Σ_p w_p · pos_p
#   Manifesto channel weighted by VOTE_SHARES, speech channel by SEAT_SHARES.
#   Position scores: polarization_score() (same as H2b).
#
# SCOPE / DIFFERENTIATION FROM H2c: H2c owns the cross-CHANNEL dispersion
# comparison and the AfD compositional decomposition (all vs excl-AfD) at the
# LEVEL. This script (H3) owns the cross-TIME story: the TREND (slope,
# pre/post-2017) and the mean positions, and — as a cross-time robustness — the
# all-vs-excl-AfD TREND-SLOPE comparison (NOT the level/compositional
# decomposition, which stays in H2c).
#
# ----------------------------------------------------------------------------
# ANALYSIS-BASE FIX (31.07.2026) — see the block "1b. Analysis base" below.
#   Before this change the party set of every H3 series came from the weight-table
#   joins alone, which do not enforce the analysis base: the PDS (labelled "Linke"
#   by 05_pull_marpor.R, which pools MARPOR ids 41221/41222/41223 onto one label)
#   carries positive vote and seat shares in LP 13-15 and therefore entered the
#   series in BOTH channels — five parties where the declared base has four.
#   The restriction is now applied once, right after the position scores.
#   Canonical twin of this rule: 12_aggregate.R, is_excluded().
#   Deliberately NOT restricted: the LP-18 manifesto channel keeps the FDP and AfD
#   2013 programmes although neither party holds a speech cell. That asymmetry is
#   a stated design feature, not an oversight.
#   NOTE: the all-vs-excl-AfD slope DIFFERENCE is mathematically unaffected by this
#   change — the two series differ only in LP 18-20 and OLS is linear, so anything
#   done to LP 13-15 cancels in the difference.
# ============================================================================
#
# Outputs: H3_system_polarization.csv, H3_mean_positions.csv,
#   H3_trend_fits.csv, H3_trend_fits_exafd.csv, H3_trend_exafd_compare.csv,
#   H3_pre_post_2017.csv, figures/H3_polarization_lines.{pdf,png},
#   figures/H3_mean_positions.{pdf,png}, figures/H3_trend_exafd.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

TAU_NATIVE   <- "native"
C305_DEFAULT <- CODE305_PRIMARY_DEFAULT   # "incl" (B-layer; 305 not in the 4 domains)
M1           <- "M1_entering"
AFD_ENTRY_X  <- 18.5                       # AfD entered Bundestag at LP19 (2017)

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[16][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[16][FLAG] ", msg)

# ---- caches + slices -------------------------------------------------------
cache <- function(f) { p <- file.path(PATHS$cache_dir, f); chk(file.exists(p), paste0("missing cache: ", p)); as.data.table(readRDS(p)) }
ss <- cache("speech_soft.rds")
md <- cache("manifesto_dists.rds")
le <- as.data.table(LP_ELECTION)[, .(lp = as.integer(lp), election_date)]
m1_map <- as_tibble(le)

spkB <- as_tibble(ss[scheme=="B" & filter==FILTER_PRIMARY & tau_name==TAU_NATIVE & code305==C305_DEFAULT, .(party, lp, bucket, share)])
manB <- as_tibble(merge(md[scheme=="B" & code305==C305_DEFAULT & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date"))
chk(nrow(spkB) > 0 && nrow(manB) > 0, "B incl slices empty (spec mismatch with 03?)")

# ---- per-cell position scores ----------------------------------------------
message("[H3] Computing per-cell position scores (scheme B) ...")
pol_long <- function(df_tb, scope_col) {
  cells <- distinct(df_tb, party, !!sym(scope_col))
  bind_rows(lapply(seq_len(nrow(cells)), function(i)
    bind_cols(cells[i, ], polarization_score(df_tb, cells$party[i], cells[[scope_col]][i], scope_col)))) %>%
    pivot_longer(c(Economy, Welfare, Migration, Europe), names_to = "domain", values_to = "position")
}
sp_pos <- pol_long(spkB, "lp")
mf_pos <- pol_long(manB, "election_date")

# ============================================================================
# 1b. Analysis base — the party-continuity exclusion, applied ONCE
# ----------------------------------------------------------------------------
# The PDS years of the Linke are not part of the analysis base: coded PDS
# manifestos exist for 1994-2002, but the PDS and Die Linke are not treated as
# the same programmatic actor. 05_pull_marpor.R pools MARPOR ids 41221 (PDS),
# 41222 (L-PDS) and 41223 (DIE LINKE) onto a single party_label, so the PDS is
# indistinguishable from Die Linke downstream and has to be removed here by
# (party, period). Election dates are derived from LP_ELECTION rather than
# hard-coded, so a change to the LP grid cannot silently desynchronise them.
# ============================================================================
PDS_LPS            <- 13:15
PDS_ELECTION_DATES <- LP_ELECTION$election_date[LP_ELECTION$lp %in% PDS_LPS]
chk(length(PDS_ELECTION_DATES) == length(PDS_LPS),
    "LP_ELECTION does not cover the PDS periods — check the LP grid")

.n_before <- c(speech = nrow(sp_pos), manifesto = nrow(mf_pos))
sp_pos <- sp_pos %>% filter(!(party == "Linke" & lp %in% PDS_LPS))
mf_pos <- mf_pos %>% filter(!(party == "Linke" & election_date %in% PDS_ELECTION_DATES))
message(sprintf("[16][base] PDS rows removed: speech %d, manifesto %d.",
                .n_before[["speech"]] - nrow(sp_pos),
                .n_before[["manifesto"]] - nrow(mf_pos)))
chk(nrow(filter(sp_pos, party == "Linke", lp %in% PDS_LPS)) == 0L,
    "PDS speech rows survived the analysis-base filter")
chk(nrow(filter(mf_pos, party == "Linke", election_date %in% PDS_ELECTION_DATES)) == 0L,
    "PDS manifesto rows survived the analysis-base filter")

flag(all(is.na(sp_pos$position) | abs(sp_pos$position) <= 1 + 1e-9), "speech positions outside [-1,1]")
flag(all(is.na(mf_pos$position) | abs(mf_pos$position) <= 1 + 1e-9), "manifesto positions outside [-1,1]")

# ---- attach weights, combine channels (weight joins restrict to in-Bundestag / running parties)
# NOTE: the weight joins alone do NOT enforce the analysis base — the PDS carries a
# positive vote and seat share in LP 13-15. The base is enforced in block 1b above.
sp_w <- sp_pos %>% inner_join(as_tibble(SEAT_SHARES), by = c("party","lp")) %>%
  rename(weight = seat_share) %>% inner_join(m1_map, by = "lp") %>%
  mutate(channel = "Speech") %>% select(party, lp, election_date, domain, position, weight, channel)
mf_w <- mf_pos %>% inner_join(as_tibble(VOTE_SHARES), by = c("party","election_date")) %>%
  rename(weight = vote_share) %>% inner_join(m1_map, by = "election_date") %>%
  mutate(channel = "Manifesto") %>% select(party, lp, election_date, domain, position, weight, channel)
both <- bind_rows(sp_w, mf_w)

# ---- Dalton-weighted polarization per (LP, domain, channel) — ALL parties ----
message("[H3] Dalton-weighted polarization per (LP, domain, channel) ...")
h3_main <- both %>%
  group_by(lp, domain, channel) %>%
  summarise(out = list(dalton_polarization(position, weight)), .groups = "drop") %>%
  unnest(out) %>%
  mutate(domain = factor(domain, levels = DOMAINS_B), channel = factor(channel, levels = c("Manifesto","Speech")))
flag(all(is.na(h3_main$polarization) | h3_main$polarization >= -1e-12), "negative polarization encountered")
flag(all(is.na(h3_main$n_parties) | h3_main$n_parties >= 2 | is.na(h3_main$polarization)),
     "a non-NA polarization was computed from < 2 parties")

# ---- BASE AUDIT: party counts per LP and channel ---------------------------
# Expected after the fix: 4 in LP 13-15, 5 in LP 16-17, 6 manifesto / 4 speech in
# LP 18, 6 in LP 19-20. A 5 in LP 13-15 means the PDS is back in.
cat("\n==================== ANALYSIS BASE: parties per LP ====================\n")
print(as.data.table(h3_main %>%
  group_by(lp, channel) %>%
  summarise(n_parties = paste(sort(unique(n_parties)), collapse = "/"), .groups = "drop") %>%
  pivot_wider(names_from = channel, values_from = n_parties) %>% arrange(lp)), row.names = FALSE)
.pds_ok <- h3_main %>% filter(lp %in% PDS_LPS) %>% pull(n_parties) %>% max(na.rm = TRUE)
cat(sprintf("  max parties in LP %s: %d  -> %s\n", paste(range(PDS_LPS), collapse = "-"),
            .pds_ok, if (.pds_ok <= 4L) "OK, analysis base as declared" else "PROBLEM: PDS still counted"))

fwrite(as.data.table(h3_main), file.path(PATHS$out_dir, "H3_system_polarization.csv"))
fwrite(as.data.table(h3_main %>% select(lp, domain, channel, mean_pos)),
       file.path(PATHS$out_dir, "H3_mean_positions.csv"))

# ---- Trend fits (descriptive): polarization ~ LP, per (domain, channel) ------
message("[H3] Fitting per-(domain, channel) OLS trends (descriptive) ...")
safe_lm <- function(d) {
  d <- d %>% filter(!is.na(polarization))
  if (nrow(d) < 3) return(tibble(n = nrow(d), intercept = NA, slope = NA, slope_se = NA, slope_p = NA, r2 = NA))
  m <- lm(polarization ~ lp, data = d); sm <- summary(m)
  tibble(n = nrow(d), intercept = coef(m)[1], slope = coef(m)[2],
         slope_se = sm$coefficients[2,2], slope_p = sm$coefficients[2,4], r2 = sm$r.squared)
}
trend_fits <- h3_main %>% group_by(domain, channel) %>% group_modify(~ safe_lm(.x)) %>% ungroup() %>%
  mutate(slope_lo = slope - 1.96 * slope_se, slope_hi = slope + 1.96 * slope_se)
fwrite(as.data.table(trend_fits), file.path(PATHS$out_dir, "H3_trend_fits.csv"))

# ---- excl-AfD trend slope (cross-time robustness) ----------------------------
# AfD enters the SPEECH channel only from LP19 (the seat-share join drops it
# earlier); in the MANIFESTO channel its 2013 manifesto already enters at LP18
# (vote share 4.7 > 0, kept by the vote-share join). dalton_polarization() renormalises
# the remaining weights, so filtering party != "AfD" cleanly re-weights the system
# to the pre-AfD party set; the slope is refit over the same LP span and only the
# LP19–20 (speech) and LP18–20 (manifesto) polarization points move. This
# isolates the COMPOSITIONAL contribution of the AfD's own position and weight —
# whatever its entry set in motion among the established parties stays in the refit.
# The LEVEL / compositional all-vs-excl-AfD split stays in H2c; this is the
# cross-TIME (slope) counterpart only.
message("[H3] excl-AfD trend-slope robustness ...")
h3_exafd <- both %>%
  filter(party != "AfD") %>%
  group_by(lp, domain, channel) %>%
  summarise(out = list(dalton_polarization(position, weight)), .groups = "drop") %>%
  unnest(out) %>%
  mutate(domain  = factor(domain,  levels = DOMAINS_B),
         channel = factor(channel, levels = c("Manifesto","Speech")))
flag(all(is.na(h3_exafd$n_parties) | h3_exafd$n_parties >= 2 | is.na(h3_exafd$polarization)),
     "excl-AfD: a non-NA polarization was computed from < 2 parties")
trend_fits_exafd <- h3_exafd %>% group_by(domain, channel) %>% group_modify(~ safe_lm(.x)) %>% ungroup() %>%
  mutate(slope_lo = slope - 1.96 * slope_se, slope_hi = slope + 1.96 * slope_se)
fwrite(as.data.table(trend_fits_exafd), file.path(PATHS$out_dir, "H3_trend_fits_exafd.csv"))

trend_compare <- trend_fits %>%
  transmute(domain = as.character(domain), channel = as.character(channel),
            slope_all = slope, slope_p_all = slope_p) %>%
  left_join(trend_fits_exafd %>%
              transmute(domain = as.character(domain), channel = as.character(channel),
                        slope_exafd = slope, slope_p_exafd = slope_p),
            by = c("domain", "channel")) %>%
  mutate(slope_delta = slope_all - slope_exafd)
fwrite(as.data.table(trend_compare), file.path(PATHS$out_dir, "H3_trend_exafd_compare.csv"))

# ---- Pre-2017 (LP 13–18) vs Post-2017 (LP 19–20) -----------------------------
prepost <- h3_main %>% filter(!is.na(polarization)) %>%
  mutate(period = if_else(lp <= 18, "Pre-2017 (LP 13–18)", "Post-2017 (LP 19–20)")) %>%
  group_by(domain, channel, period) %>% summarise(mean_pol = mean(polarization), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(mean_pol, n)) %>%
  mutate(delta = `mean_pol_Post-2017 (LP 19–20)` - `mean_pol_Pre-2017 (LP 13–18)`)
fwrite(as.data.table(prepost), file.path(PATHS$out_dir, "H3_pre_post_2017.csv"))

# ============================================================================
# Figures
# ============================================================================
CHANNEL_COLOURS <- c("Manifesto" = "#264653", "Speech" = "#E76F51")

p_main <- h3_main %>%
  ggplot(aes(lp, polarization, colour = channel, group = channel)) +
  geom_vline(xintercept = AFD_ENTRY_X, linetype = "dotted", colour = "grey50") +
  annotate("text", x = AFD_ENTRY_X, y = Inf, label = "  AfD enters the\n  Bundestag (LP 19)", hjust = 0, vjust = 1.3, colour = "grey40", size = 3) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2) +
  labs(title = "H3: System-level polarisation over time",
       subtitle = "Per-domain Dalton-weighted polarisation across LP 13–20 · manifesto weighted by vote share, speech by seat share",
       x = "Legislative period", y = "Weighted polarisation (SD of positions)",
       caption = "Descriptive trends on the directional layer, validated at κ(B) = 0.456. Dotted line = AfD Bundestag entry; the manifesto channel includes the AfD's 2013 manifesto from LP 18. The PDS periods (LP 13-15) are outside the analysis base. The AfD compositional decomposition is in H2c.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "H3_polarization_lines.pdf"), p_main, width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "H3_polarization_lines.png"), p_main, width = 10, height = 7, dpi = 200, bg = "white")

p_means <- h3_main %>%
  ggplot(aes(lp, mean_pos, colour = channel, group = channel)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = AFD_ENTRY_X, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = CHANNEL_COLOURS, name = NULL) +
  facet_wrap(~ domain, ncol = 2) +
  labs(title = "H3: Weighted mean position over time",
       subtitle = "Centre of mass of the party system per domain · positive = market-liberal / retrenchment / restrictive / Eurosceptic",
       x = "Legislative period", y = "Weighted mean position",
       caption = "Distinguishes 'parties moved together' (mean shifts) from 'parties moved apart' (polarisation rises). Directional layer validated at κ(B) = 0.456.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "H3_mean_positions.pdf"), p_means, width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "H3_mean_positions.png"), p_means, width = 10, height = 7, dpi = 200, bg = "white")

# excl-AfD trend-slope comparison (dumbbell: all parties vs excl-AfD, per domain × channel)
slope_long <- trend_compare %>%
  select(domain, channel, slope_all, slope_exafd) %>%
  pivot_longer(c(slope_all, slope_exafd), names_to = "set", values_to = "slope") %>%
  mutate(set     = factor(if_else(set == "slope_all", "All parties", "Excl. AfD"),
                          levels = c("All parties", "Excl. AfD")),
         domain  = factor(domain,  levels = DOMAINS_B),
         channel = factor(channel, levels = c("Manifesto","Speech")))
p_exafd <- slope_long %>%
  ggplot(aes(slope, domain)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_line(aes(group = domain), colour = "grey60", linewidth = 0.8) +
  geom_point(aes(colour = set), size = 3) +
  scale_colour_manual(values = c("All parties" = "#E76F51", "Excl. AfD" = "#264653"), name = NULL) +
  facet_wrap(~ channel) +
  labs(title = "H3 robustness: polarisation trend slope, all parties vs excl-AfD",
       subtitle = "OLS slope of Dalton-weighted polarisation on LP, per domain and channel. AfD: speech channel from LP 19; manifesto channel from LP 18 (2013 manifesto).",
       x = "Trend slope (delta weighted polarisation per LP)", y = NULL,
       caption = "Coinciding points = the time trend is not carried by the AfD's own position. The LEVEL/compositional decomposition is in H2c. Directional layer validated at κ(B) = 0.456.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "H3_trend_exafd.pdf"), p_exafd, width = 9, height = 6, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "H3_trend_exafd.png"), p_exafd, width = 9, height = 6, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H3 TREND FITS (descriptive) ====================\n")
print(as.data.table(trend_fits)[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)], row.names = FALSE)
cat("\n==================== H3 PRE/POST 2017 (delta) ====================\n")
print(as.data.table(prepost)[, .(domain, channel, delta = round(delta, 4))], row.names = FALSE)
cat("\n==================== H3 excl-AfD TREND SLOPE (robustness) ====================\n")
print(as.data.table(trend_compare)[, .(domain, channel,
        slope_all   = round(slope_all, 4),
        slope_exafd = round(slope_exafd, 4),
        slope_delta = round(slope_delta, 4))], row.names = FALSE)
cat("  (AfD: speech channel LP 19–20 only, manifesto channel from LP 18; slope_delta localises the contribution of its own position.)\n")
message("\n[H3] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
