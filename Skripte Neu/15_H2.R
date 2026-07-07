# ============================================================================
# 15_H2.R — Hypotheses H2a, H2b, H2c
# ----------------------------------------------------------------------------
#   H2a  Per-bucket JSD attribution (Aggregation A) — which buckets drive the
#        manifesto-speech divergence. MAIN spec: filtered_000, native, code 305
#        EXCLUDED. The null-class (filtered_000) filter removes most of the 305
#        artefact at source; the surgical 305-exclusion removes the residual, so the
#        per-bucket attribution reflects substantive drivers. The artefact magnitude
#        (incl-vs-excl before/after, DPS code decomposition) lives in 10 and 11.
#
#   H2b  Directional moderation: per-party manifesto vs speech polarization
#        score, OLS fit per domain (slope < 1 => moderation). [EXPLORATORY]
#
#   H2c  Inter-party dispersion: Dalton-weighted SD of polarization scores per
#        LP and domain, speech vs manifesto, all vs excl-AfD. [EXPLORATORY]
#
#   H2b/H2c sit on the directional (B) layer (human κ≈0.27) -> exploratory.
#   The B-side polarization scores are computed WITHIN the 4 domains, which do
#   not contain code 305, so incl/excl is irrelevant there -> default incl.
#
# Outputs: H2a_per_bucket_long.csv, H2a_per_bucket_summary.csv,
#   figures/H2a_per_bucket_bar.{pdf,png}, H2b_polarization_table.csv,
#   H2b_regression_fits.csv, figures/H2b_polarization_scatter.{pdf,png},
#   H2c_dispersion_table.csv, figures/H2c_dispersion_lines.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

TAU_NATIVE   <- "native"
C305_H1A     <- CODE305_PRIMARY_H1A_H2A   # "excl"  (H2a; PRIMARY)
C305_DEFAULT <- CODE305_PRIMARY_DEFAULT   # "incl"  (H2b/H2c B-layer)
M1           <- "M1_entering"

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[05][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[05][FLAG] ", msg)
vec_of <- function(dt, buckets) { v <- setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)] <- 0; as.numeric(v) }

# ---- load caches + slices --------------------------------------------------
cache <- function(f) { p <- file.path(PATHS$cache_dir, f); chk(file.exists(p), paste0("missing cache: ", p)); as.data.table(readRDS(p)) }
ci <- cache("bootstrap_ci.rds")
ss <- cache("speech_soft.rds")
md <- cache("manifesto_dists.rds")
le <- as.data.table(LP_ELECTION)[, .(lp = as.integer(lp), election_date)]

spkA <- ss[scheme=="A" & filter==FILTER_PRIMARY & tau_name==TAU_NATIVE & code305==C305_H1A,     .(party, lp, bucket, share)]
spkB <- ss[scheme=="B" & filter==FILTER_PRIMARY & tau_name==TAU_NATIVE & code305==C305_DEFAULT, .(party, lp, bucket, share)]
manA <- merge(md[scheme=="A" & code305==C305_H1A     & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
manB <- merge(md[scheme=="B" & code305==C305_DEFAULT & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
chk(nrow(spkA)>0 && nrow(manA)>0, "A excl slices empty (spec mismatch with 03?)")
chk(nrow(spkB)>0 && nrow(manB)>0, "B incl slices empty")

valid <- ci[is_primary==TRUE, .(party, lp)]                 # the analysis cells (is_primary band; 38 after exclusions)
chk(nrow(valid)>0, "bootstrap_ci has no is_primary band")
spkA_v <- merge(spkA, valid, by=c("party","lp"))
manA_v <- merge(manA, valid, by=c("party","lp"))

# ============================================================================
# H2a — per-bucket JSD attribution on the filtered_000 primary, excl (clean drivers)
# ============================================================================
message("[H2a] Per-bucket JSD attribution (filtered_000, native, EXCL) ...")
boot_pt <- ci[is_primary==TRUE, .(party, lp, jsd_boot = jsd_point)]   # for the consistency check

h2a_rows <- list(); sumchk <- numeric(0); bootchk <- numeric(0)
signed_rows <- list()   # signed speech-vs-manifesto shares per (party, lp, bucket) on the PRIMARY (excl)
for (k in seq_len(nrow(valid))) {
  p <- valid$party[k]; L <- valid$lp[k]
  s   <- vec_of(spkA_v[party==p & lp==L], BUCKETS_A); names(s)   <- BUCKETS_A
  m_v <- vec_of(manA_v[party==p & lp==L], BUCKETS_A); names(m_v) <- BUCKETS_A
  if (sum(s)<=0 || sum(m_v)<=0) next
  contrib <- per_bucket_jsd(s, m_v)
  h2a_rows[[length(h2a_rows)+1L]] <- data.table(party=p, lp=L, bucket=names(contrib), jsd_contrib=unname(contrib))
  # signed divergence: reuse the SAME 0-filled excl vectors (speech s, manifesto m_v)
  signed_rows[[length(signed_rows)+1L]] <- data.table(party=p, lp=L, bucket=BUCKETS_A,
                                                      share_speech=as.numeric(s), share_manifesto=as.numeric(m_v))
  # CHECK A: per-bucket contributions sum to the scalar JSD
  sumchk <- c(sumchk, abs(sum(contrib) - jsd(s, m_v)))
  # CHECK B: that scalar JSD equals the official bootstrap point for this cell
  bp <- boot_pt[party==p & lp==L, jsd_boot]
  if (length(bp)) bootchk <- c(bootchk, abs(sum(contrib) - bp))
}
h2a_long <- rbindlist(h2a_rows)
flag(max(sumchk, na.rm=TRUE) < 1e-9,
     sprintf("per-bucket contributions do NOT sum to scalar JSD (max dev %.2e)", max(sumchk, na.rm=TRUE)))
message(sprintf("[H2a][check] max |Σ contrib - jsd| = %.2e ; max |Σ contrib - bootstrap point| = %.2e",
                max(sumchk, na.rm=TRUE), if (length(bootchk)) max(bootchk, na.rm=TRUE) else NA_real_))
flag(!length(bootchk) || max(bootchk, na.rm=TRUE) < 1e-6,
     "H2a cell JSD disagrees with bootstrap_ci primary point — speech_soft/bootstrap inconsistent.")

h2a_summary <- h2a_long[, .(mean_contrib = mean(jsd_contrib, na.rm=TRUE),
                            sd_contrib   = sd(jsd_contrib,   na.rm=TRUE),
                            n_cells      = sum(!is.na(jsd_contrib))), by=bucket][order(-mean_contrib)]
fwrite(h2a_long,    file.path(PATHS$out_dir, "H2a_per_bucket_long.csv"))
fwrite(h2a_summary, file.path(PATHS$out_dir, "H2a_per_bucket_summary.csv"))

# signed divergence per (party, bucket), averaged over the party's valid LPs, on the
# PRIMARY spec (filtered_000, native, excl). diff>0 = over-emphasised in plenary vs manifesto.
# Regenerated by the pipeline -> replaces the stale standalone quick_signed.R.
signed_long <- rbindlist(signed_rows)
signed_A <- signed_long[, .(mean_speech    = round(mean(share_speech), 4),
                            mean_manifesto = round(mean(share_manifesto), 4),
                            mean_diff      = round(mean(share_speech - share_manifesto), 4),
                            n_lps          = .N), by=.(party, bucket)][order(party, -mean_diff)]
fwrite(signed_A, file.path(PATHS$out_dir, "signed_divergence_A.csv"))
message(sprintf("[H2a] signed_divergence_A.csv written (excl, %d party×bucket rows).", nrow(signed_A)))

# H2a figure — neutral single-fill bars (NO artefact highlight: on filtered_000 the
# null-class filter already removes the 305 artefact source; clean post-correction attribution)
h2a_plot <- h2a_summary %>% as_tibble() %>% mutate(bucket = fct_reorder(bucket, mean_contrib))
blevels  <- levels(h2a_plot$bucket)
p_h2a <- ggplot(h2a_plot, aes(mean_contrib, bucket)) +
  geom_col(width = 0.7, fill = "#264653", alpha = 0.9) +
  geom_jitter(data = h2a_long %>% as_tibble() %>% mutate(bucket = factor(bucket, levels = blevels)),
              aes(jsd_contrib, bucket), height = 0.18, width = 0, size = 1.4, alpha = 0.5, colour = "#1A3640") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "H2a — Topic-level salience divergence (clean attribution)",
       subtitle = "Mean per-bucket contribution to JSD; dots = individual (party, LP) cells · spec: filtered_000, native tau, code 305 EXCLUDED",
       x = "Mean JSD contribution", y = NULL,
       caption = "Drivers of divergence AFTER removing the code-305 procedural artefact. The artefact's magnitude (incl-vs-excl) and its code-level source are shown in scripts 10 and 11.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "H2a_per_bucket_bar.pdf"), p_h2a, width = 8.5, height = 6)
ggsave(file.path(PATHS$fig_dir, "H2a_per_bucket_bar.png"), p_h2a, width = 8.5, height = 6, dpi = 200, bg = "white")

# ============================================================================
# H2b / H2c — directional polarization (scheme B, incl) [EXPLORATORY]
# ============================================================================
message("\n[H2b/c] Computing per-cell polarization scores (scheme B) ...")
spkB_tb <- as_tibble(spkB); manB_tb <- as_tibble(manB)
m1_map  <- as_tibble(le)

pol_long <- function(df_tb, scope_col, value_name) {
  cells <- distinct(df_tb, party, !!sym(scope_col))
  bind_rows(lapply(seq_len(nrow(cells)), function(i)
    bind_cols(cells[i, ], polarization_score(df_tb, cells$party[i], cells[[scope_col]][i], scope_col)))) %>%
    pivot_longer(c(Economy, Welfare, Migration, Europe), names_to = "domain", values_to = value_name)
}
speech_pol    <- pol_long(spkB_tb, "lp",            "speech_pol")
manifesto_pol <- pol_long(manB_tb, "election_date", "manifesto_pol")

# polarization score sanity: signed, in [-1, 1]
flag(all(is.na(speech_pol$speech_pol)       | abs(speech_pol$speech_pol)       <= 1 + 1e-9), "speech_pol outside [-1,1]")
flag(all(is.na(manifesto_pol$manifesto_pol) | abs(manifesto_pol$manifesto_pol) <= 1 + 1e-9), "manifesto_pol outside [-1,1]")

# ---- H2b: manifesto vs speech polarization, restricted to valid joint cells
h2b_long <- speech_pol %>%
  semi_join(as_tibble(valid), by = c("party", "lp")) %>%      # exclude FDP LP18 etc.
  inner_join(m1_map, by = "lp") %>%
  inner_join(manifesto_pol, by = c("party", "election_date", "domain")) %>%
  mutate(domain = factor(domain, levels = DOMAINS_B),
         party  = factor(party,  levels = PARTIES_KEEP))
fwrite(as.data.table(h2b_long), file.path(PATHS$out_dir, "H2b_polarization_table.csv"))

h2b_fits <- h2b_long %>% filter(!is.na(speech_pol), !is.na(manifesto_pol)) %>%
  group_by(domain) %>%
  summarise(n = n(),
            intercept = if (n() >= 3) coef(lm(speech_pol ~ manifesto_pol))[1] else NA_real_,
            slope     = if (n() >= 3) coef(lm(speech_pol ~ manifesto_pol))[2] else NA_real_,
            r2        = if (n() >= 3) summary(lm(speech_pol ~ manifesto_pol))$r.squared else NA_real_,
            .groups = "drop")
fwrite(as.data.table(h2b_fits), file.path(PATHS$out_dir, "H2b_regression_fits.csv"))

p_h2b <- h2b_long %>%
  ggplot(aes(manifesto_pol, speech_pol, colour = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, colour = "black", fill = "grey80", linewidth = 0.6) +
  geom_point(size = 2.5, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)), size = 2.6, max.overlaps = 12, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  facet_wrap(~ domain, scales = "free", ncol = 2) +
  labs(title = "H2b — Directional moderation in parliamentary speech  [EXPLORATORY]",
       subtitle = "Polarization score: signed within-domain balance of directional sub-labels (scheme B)",
       x = "Manifesto polarization (M1)", y = "Speech polarization",
       caption = "Dashed: 45° (perfect consistency). Solid: per-domain OLS. Slope < 1 = moderation. Directional layer exploratory (kappa~0.27).") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "H2b_polarization_scatter.pdf"), p_h2b, width = 10, height = 8)
ggsave(file.path(PATHS$fig_dir, "H2b_polarization_scatter.png"), p_h2b, width = 10, height = 8, dpi = 200, bg = "white")

# ---- H2c: Dalton-weighted inter-party dispersion (weight joins restrict to in-Bundestag / running parties)
message("\n[H2c] Dalton-weighted inter-party dispersion ...")
disp <- function(pol_df, weight_df, by_cols, val_col, weight_col, project_lp = FALSE, out_name) {
  d <- pol_df %>% inner_join(weight_df, by = by_cols)
  if (project_lp) d <- d %>% inner_join(m1_map, by = "election_date")
  d %>% group_by(lp, domain) %>%
    summarise(out = list(dalton_polarization(.data[[val_col]], .data[[weight_col]])), .groups = "drop") %>%
    unnest(out) %>% transmute(lp, domain, !!out_name := polarization)
}
SEAT <- as_tibble(SEAT_SHARES); VOTE <- as_tibble(VOTE_SHARES)
d_sp_all   <- disp(speech_pol,                            SEAT, c("party","lp"),            "speech_pol",    "seat_share", FALSE, "sd_speech_all")
d_sp_noafd <- disp(speech_pol    %>% filter(party!="AfD"), SEAT, c("party","lp"),            "speech_pol",    "seat_share", FALSE, "sd_speech_no_afd")
d_mf_all   <- disp(manifesto_pol,                         VOTE, c("party","election_date"), "manifesto_pol", "vote_share", TRUE,  "sd_manifesto")
d_mf_noafd <- disp(manifesto_pol %>% filter(party!="AfD"), VOTE, c("party","election_date"), "manifesto_pol", "vote_share", TRUE,  "sd_manifesto_no_afd")

h2c_long <- d_mf_all %>%
  full_join(d_mf_noafd, by = c("lp","domain")) %>%
  full_join(d_sp_all,   by = c("lp","domain")) %>%
  full_join(d_sp_noafd, by = c("lp","domain")) %>%
  pivot_longer(starts_with("sd_"), names_to = "series", values_to = "sd_polarization") %>%
  mutate(series = factor(series,
            levels = c("sd_manifesto","sd_manifesto_no_afd","sd_speech_all","sd_speech_no_afd"),
            labels = c("Manifesto (all)","Manifesto (excl. AfD)","Speech (all)","Speech (excl. AfD)")),
         domain = factor(domain, levels = DOMAINS_B))
flag(all(is.na(h2c_long$sd_polarization) | h2c_long$sd_polarization >= -1e-12), "negative dispersion encountered")
fwrite(as.data.table(h2c_long), file.path(PATHS$out_dir, "H2c_dispersion_table.csv"))

p_h2c <- h2c_long %>%
  ggplot(aes(lp, sd_polarization, colour = series, linetype = series, group = series)) +
  geom_vline(xintercept = 18.5, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 18.5, y = Inf, label = "  AfD enters", hjust = 0, vjust = 1.4, colour = "grey40", size = 3) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = c("Manifesto (all)"="#264653","Manifesto (excl. AfD)"="#264653",
                                 "Speech (all)"="#E76F51","Speech (excl. AfD)"="#E76F51"), name = NULL) +
  scale_linetype_manual(values = c("Manifesto (all)"="dashed","Manifesto (excl. AfD)"="dotted",
                                   "Speech (all)"="solid","Speech (excl. AfD)"="twodash"), name = NULL) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2) +
  labs(title = "H2c — Inter-party dispersion: manifesto vs. speech  [EXPLORATORY]",
       subtitle = "Dalton-weighted SD of party polarization scores per LP, by domain",
       x = "Legislative period", y = "Weighted SD of polarization score",
       caption = "Manifesto weighted by vote share, speech by seat share. AfD entered LP19 (2017). Directional layer exploratory (kappa~0.27).") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "H2c_dispersion_lines.pdf"), p_h2c, width = 10, height = 7)
ggsave(file.path(PATHS$fig_dir, "H2c_dispersion_lines.png"), p_h2c, width = 10, height = 7, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H2a per-bucket (sorted) ====================\n")
print(h2a_summary, row.names = FALSE)
cat("\n==================== H2b OLS fits (slope < 1 = moderation) ====================\n")
print(as.data.table(h2b_fits), row.names = FALSE)
cat("\n==================== H2c by-domain means ====================\n")
print(as.data.table(h2c_long)[, .(mean_sd = round(mean(sd_polarization, na.rm=TRUE), 4)), by = .(domain, series)], row.names = FALSE)
message("\n[H2] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
