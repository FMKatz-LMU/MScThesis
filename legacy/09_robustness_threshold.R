# ============================================================================
# 09_robustness_threshold.R — Argmax-with-threshold robustness on H1a / H1b
# ----------------------------------------------------------------------------
# Reads from cache. Compares the SOFT (native τ) primary aggregation against
# HARD argmax-with-threshold aggregation at thresholds 0.4 and 0.5.
#   Soft baseline: speech_soft  [filtered_000, native, INCL]
#   Hard variants: speech_method[method = hard_thr0.4 / hard_thr0.5]   (filtered_000)
# Both sides are compared INCL (the hard methods carry no 305-mass axis; the
# 305 / filter interventions are studied separately in 10 and 11).
#
# Hard aggregation assigns each sentence to its argmax bucket only if the argmax
# probability exceeds the threshold, else to "Andere" (below-threshold mass).
#   H1a (A is a 9-bucket partition, no manifesto Andere): drop the speech-side
#       Andere mass and renormalize over BUCKETS_A before JSD ("keep only
#       above-threshold sentences").
#   H1b (B): renormalize both sides within the 4 domains (Andere excluded).
#
# Acceptance (MIv2 §3.5): cross-cell JSD ordering preserved (Spearman ρ ≥ 0.85);
# absolute magnitudes expected to shift upward, concentrated in the DPS bucket
# on A (per-bucket attribution reported).
#
# Outputs: H1a_threshold_robustness.csv, H1a_threshold_attribution.csv,
#   H1b_threshold_robustness.csv, figures/H1{a,b}_threshold_scatter.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
source(here::here("02_helpers.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

C305_DEFAULT <- CODE305_PRIMARY_DEFAULT   # incl
M1           <- "M1_entering"
DPS_BUCKET   <- BUCKET_OF_305_A   # "Democracy & Political System" (single source of truth)
RHO_MIN      <- 0.85

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[09][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[09][FLAG] ", msg)
vec_of <- function(dt, buckets) { v <- setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)] <- 0; as.numeric(v) }

# ---- caches ----------------------------------------------------------------
cache <- function(f) { p <- file.path(PATHS$cache_dir, f); chk(file.exists(p), paste0("missing cache: ", p)); as.data.table(readRDS(p)) }
ss  <- cache("speech_soft.rds")
sm  <- cache("speech_method.rds")
md  <- cache("manifesto_dists.rds")
ci  <- cache("bootstrap_ci.rds")
le  <- as.data.table(LP_ELECTION)[, .(lp = as.integer(lp), election_date)]
valid <- ci[is_primary == TRUE, .(party, lp)]
chk(nrow(valid) > 0, "no is_primary cells")
chk(all(c("hard_thr0.4","hard_thr0.5") %in% unique(sm$method)), "speech_method missing hard_thr0.4/0.5")
flag(DPS_BUCKET %in% BUCKETS_A, "DPS_BUCKET name not found in BUCKETS_A — attribution will be NA")

# manifesto refs (incl)
manA <- merge(md[scheme=="A" & code305==C305_DEFAULT & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
manA <- merge(manA, valid, by=c("party","lp"))
manB <- merge(md[scheme=="B" & code305==C305_DEFAULT & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
manB <- merge(manB, valid, by=c("party","lp"))
manB <- manB[bucket != "Andere"][, share := if (sum(share) > 0) share/sum(share) else 0, by=.(party, lp)]

# ---- speech slices ---------------------------------------------------------
soft_A <- ss[scheme=="A" & filter==FILTER_PRIMARY & tau_name=="native" & code305==C305_DEFAULT, .(party, lp, bucket, share)]
soft_B <- ss[scheme=="B" & filter==FILTER_PRIMARY & tau_name=="native" & code305==C305_DEFAULT, .(party, lp, bucket, share)]
hardA  <- function(m) sm[scheme=="A" & method==m, .(party, lp, bucket, share)]
hardB  <- function(m) sm[scheme=="B" & method==m, .(party, lp, bucket, share)]

# ============================================================================
# H1a — per-cell JSD (+ DPS attribution), A renormalized over BUCKETS_A
# ============================================================================
jsd_A <- function(sp_long, spec) {
  sp <- sp_long[bucket %in% BUCKETS_A]
  sp[, share := if (sum(share) > 0) share/sum(share) else 0, by=.(party, lp)]   # drop below-threshold Andere
  sp <- merge(sp, valid, by=c("party","lp"))
  cells <- unique(sp[, .(party, lp)]); rows <- list()
  for (k in seq_len(nrow(cells))) {
    p <- cells$party[k]; L <- cells$lp[k]
    s <- vec_of(sp[party==p & lp==L], BUCKETS_A); names(s) <- BUCKETS_A
    m_v <- vec_of(manA[party==p & lp==L], BUCKETS_A); names(m_v) <- BUCKETS_A
    if (sum(s) <= 0 || sum(m_v) <= 0) next
    cc <- per_bucket_jsd(s, m_v)
    dps <- if (DPS_BUCKET %in% names(cc)) unname(cc[DPS_BUCKET]) else NA_real_
    rows[[length(rows)+1L]] <- data.table(party=p, lp=L, spec=spec,
      jsd = jsd(s, m_v), dps_contrib = dps, total_contrib = sum(cc))
  }
  rbindlist(rows)
}
h1a <- rbindlist(list(jsd_A(soft_A, "soft_tau1.0"),
                      jsd_A(hardA("hard_thr0.4"), "hard_thr0.4"),
                      jsd_A(hardA("hard_thr0.5"), "hard_thr0.5")))
h1a[, spec := factor(spec, levels = c("soft_tau1.0","hard_thr0.4","hard_thr0.5"))]
flag(all(h1a$jsd >= 0 & h1a$jsd <= 1), "H1a JSD outside [0,1]")
flag(max(abs(h1a$total_contrib - h1a$jsd), na.rm=TRUE) < 1e-9, "per-bucket contributions != total JSD (A)")

attribution_A <- h1a[, .(party, lp, spec, dps_contrib, total_contrib,
                         dps_share_of_jsd = fifelse(total_contrib > 0, dps_contrib/total_contrib, NA_real_))]
fwrite(h1a[, .(party, lp, spec, jsd, dps_contrib, total_contrib)], file.path(PATHS$out_dir, "H1a_threshold_robustness.csv"))
fwrite(attribution_A, file.path(PATHS$out_dir, "H1a_threshold_attribution.csv"))

# ============================================================================
# H1b — per-cell within-domain JSD (Andere excluded both sides)
# ============================================================================
jsd_B <- function(sp_long, spec) {
  sp <- sp_long[bucket != "Andere"]
  sp[, share := if (sum(share) > 0) share/sum(share) else 0, by=.(party, lp)]
  sp <- merge(sp, valid, by=c("party","lp"))
  cells <- unique(sp[, .(party, lp)]); rows <- list()
  for (k in seq_len(nrow(cells))) {
    p <- cells$party[k]; L <- cells$lp[k]
    s <- vec_of(sp[party==p & lp==L], BUCKETS_B); m_v <- vec_of(manB[party==p & lp==L], BUCKETS_B)
    if (sum(s) <= 0 || sum(m_v) <= 0) next
    rows[[length(rows)+1L]] <- data.table(party=p, lp=L, spec=spec, jsd = jsd(s, m_v))
  }
  rbindlist(rows)
}
h1b <- rbindlist(list(jsd_B(soft_B, "soft_tau1.0"),
                      jsd_B(hardB("hard_thr0.4"), "hard_thr0.4"),
                      jsd_B(hardB("hard_thr0.5"), "hard_thr0.5")))
h1b[, spec := factor(spec, levels = c("soft_tau1.0","hard_thr0.4","hard_thr0.5"))]
flag(all(h1b$jsd >= 0 & h1b$jsd <= 1), "H1b JSD outside [0,1]")
fwrite(h1b, file.path(PATHS$out_dir, "H1b_threshold_robustness.csv"))

# ============================================================================
# Spearman rank correlations + per-party shifts
# ============================================================================
rho_table <- function(h) {
  w <- dcast(h, party + lp ~ spec, value.var = "jsd")
  w <- w[complete.cases(w[, .(soft_tau1.0, hard_thr0.4, hard_thr0.5)])]
  flag(nrow(w) >= 3, "fewer than 3 complete cells for Spearman ρ")
  list(rho = data.table(
        pair = c("soft_vs_hard0.4","soft_vs_hard0.5","hard0.4_vs_hard0.5"),
        spearman_rho = c(cor(w$soft_tau1.0, w$hard_thr0.4, method="spearman"),
                         cor(w$soft_tau1.0, w$hard_thr0.5, method="spearman"),
                         cor(w$hard_thr0.4, w$hard_thr0.5, method="spearman"))),
       wide = w)
}
ra <- rho_table(h1a); h1a_rho <- ra$rho
rb <- rho_table(h1b); h1b_rho <- rb$rho
party_shift <- ra$wide[, .(mean_delta_h04 = mean(hard_thr0.4 - soft_tau1.0, na.rm=TRUE),
                           mean_delta_h05 = mean(hard_thr0.5 - soft_tau1.0, na.rm=TRUE)), by = party]

# ============================================================================
# Figures
# ============================================================================
scatter <- function(h, title, ylab) {
  d <- dcast(h, party + lp ~ spec, value.var = "jsd") %>% as_tibble() %>%
    pivot_longer(c(hard_thr0.4, hard_thr0.5), names_to = "threshold_spec", values_to = "jsd_hard") %>%
    rename(jsd_soft = soft_tau1.0) %>%
    mutate(party = factor(party, levels = PARTIES_KEEP),
           threshold_spec = factor(threshold_spec, levels = c("hard_thr0.4","hard_thr0.5"),
                                   labels = c("threshold 0.4","threshold 0.5")))
  ggplot(d, aes(jsd_soft, jsd_hard, colour = party)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 2.4, alpha = 0.85) +
    ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)), size = 2.5, max.overlaps = 10, show.legend = FALSE) +
    scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
    facet_wrap(~ threshold_spec, ncol = 2) +
    labs(title = title, subtitle = "Each point is a (party, LP) cell. Dashed line: 45° reference.",
         x = "Soft JSD (native tau)", y = ylab, caption = "Rank correlations & shifts in the threshold CSVs.") +
    theme_thesis()
}
p_a <- scatter(h1a, "H1a — Soft vs hard-threshold JSD (Aggregation A)", "Hard JSD (argmax + threshold; Andere dropped & renormalized)")
p_b <- scatter(h1b, "H1b — Soft vs hard-threshold within-domain JSD (Aggregation B)", "Hard JSD (argmax + threshold)")
ggsave(file.path(PATHS$fig_dir, "H1a_threshold_scatter.pdf"), p_a, width = 11, height = 6)
ggsave(file.path(PATHS$fig_dir, "H1a_threshold_scatter.png"), p_a, width = 11, height = 6, dpi = 200, bg = "white")
ggsave(file.path(PATHS$fig_dir, "H1b_threshold_scatter.pdf"), p_b, width = 11, height = 6)
ggsave(file.path(PATHS$fig_dir, "H1b_threshold_scatter.png"), p_b, width = 11, height = 6, dpi = 200, bg = "white")

# ============================================================================
# Console summary + acceptance
# ============================================================================
cat("\n==================== H1a SOFT vs HARD RANK CORRELATIONS ====================\n")
print(h1a_rho[, .(pair, spearman_rho = round(spearman_rho, 3))], row.names = FALSE)
cat("\nPer-party mean delta-JSD (hard - soft):\n")
print(party_shift[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)], row.names = FALSE)
cat("\n==================== H1b SOFT vs HARD RANK CORRELATIONS ====================\n")
print(h1b_rho[, .(pair, spearman_rho = round(spearman_rho, 3))], row.names = FALSE)
cat("\n----- H1a DPS share of JSD by spec -----\n")
print(attribution_A[, .(mean_dps_share = round(mean(dps_share_of_jsd, na.rm=TRUE), 3),
                        median_dps_share = round(median(dps_share_of_jsd, na.rm=TRUE), 3)), by = spec], row.names = FALSE)
if (any(c(h1a_rho$spearman_rho, h1b_rho$spearman_rho) < RHO_MIN)) {
  message(sprintf("[09] WARNING: at least one rank correlation below %.2f — flag for the writeup.", RHO_MIN))
} else {
  message(sprintf("[09] All soft-hard rank correlations >= %.2f (acceptance OK).", RHO_MIN))
}
message("\n[09] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
