# ============================================================================
# 17_robustness.R — Robustness arms for H1a / H1b (drei PARTs)
# ----------------------------------------------------------------------------
# MERGE (pipeline reorg 2026-06): vereint die drei bisherigen Robustness-Skripte
#   PART 1  Temperatur-Sweep    (formerly 08_robustness_temperature.R)
#   PART 2  Threshold (argmax)  (formerly 09_robustness_threshold.R)
#   PART 3  Intervention/Filter (formerly 10_robustness_prefilter.R)
#
# Alle drei lesen NUR aus den Aggregations-Caches (results/empirics/cache/) und
# schreiben CSVs + Figuren; keiner haengt an den In-Memory-Objekten eines anderen.
# Jeder PART laeuft in seinem EIGENEN local({})-Scope, weil die drei Skripte
# gleichnamige, aber NICHT identische Helfer/Konstanten definieren — v.a.:
#   * chk/flag/vec_of/cache : pro PART neu (chk/flag tragen [08]/[09]/[10]-Praefix)
#   * rho_table             : PART 1 -> data.table, PART 2 -> list(rho, wide)
#   * RHO_MIN               : PART 1 = 0.90, PART 2 = 0.85  (UNTERSCHIEDLICH!)
# Die Kapselung haelt jede Zahl exakt wie in den Einzelskripten.
#
# source(here::here("00_config.R")) zieht jetzt auch die Helfer (ex 02_helpers.R).
# PREREQUISITE: aggregate (ex 03) hat speech_soft.rds / speech_method.rds /
#   manifesto_dists.rds / bootstrap_ci.rds nach results/empirics/cache/ geschrieben.
# ============================================================================

source(here::here("00_config.R"))   # PATHS, AGG/BUCKETS, TEMPERATURES, theme_thesis + Helfer
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
# (ggrepel wird in PART 2 nur ueber ggrepel:: qualifiziert genutzt -> kein attach noetig)


# ############################################################################
# ##  PART 1 — TEMPERATUR-SWEEP   (formerly 08_robustness_temperature.R)      ##
# ############################################################################
local({
C305_H1A     <- CODE305_PRIMARY_H1A_H2A   # excl (H1a panel; PRIMARY)
C305_DEFAULT <- CODE305_PRIMARY_DEFAULT   # incl (H1b panel)
M1           <- "M1_entering"
TAU_ORDER    <- c("sharp", "native", "flat")
RHO_MIN      <- 0.9

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[08][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[08][FLAG] ", msg)
vec_of <- function(dt, buckets) { v <- setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)] <- 0; as.numeric(v) }

# ---- caches + fixed references ---------------------------------------------
cache <- function(f) { p <- file.path(PATHS$cache_dir, f); chk(file.exists(p), paste0("missing cache: ", p)); as.data.table(readRDS(p)) }
ss <- cache("speech_soft.rds")
md <- cache("manifesto_dists.rds")
ci <- cache("bootstrap_ci.rds")
le <- as.data.table(LP_ELECTION)[, .(lp = as.integer(lp), election_date)]
valid <- ci[is_primary == TRUE, .(party, lp)]
chk(nrow(valid) > 0, "no is_primary cells in bootstrap_ci")
chk(all(TAU_ORDER %in% unique(ss$tau_name)), "speech_soft is missing one of the τ levels")

# manifesto A (excl), fixed across τ
manA <- merge(md[scheme=="A" & code305==C305_H1A & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
manA <- merge(manA, valid, by=c("party","lp"))
# manifesto B (incl), Andere excluded + within-cell renorm, fixed across τ
manB <- merge(md[scheme=="B" & code305==C305_DEFAULT & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
manB <- merge(manB, valid, by=c("party","lp"))
manB <- manB[bucket != "Andere"][, share := if (sum(share) > 0) share/sum(share) else 0, by=.(party, lp)]

# ============================================================================
# Per-τ per-cell JSD
# ============================================================================
sweep_one <- function(sch, c305v, buckets, man_ref, andere_excl) {
  out <- list()
  for (tn in TAU_ORDER) {
    sp <- ss[scheme==sch & filter==FILTER_PRIMARY & tau_name==tn & code305==c305v, .(party, lp, bucket, share)]
    sp <- merge(sp, valid, by=c("party","lp"))
    if (andere_excl) sp <- sp[bucket != "Andere"][, share := if (sum(share) > 0) share/sum(share) else 0, by=.(party, lp)]
    cells <- unique(sp[, .(party, lp)])
    for (k in seq_len(nrow(cells))) {
      p <- cells$party[k]; L <- cells$lp[k]
      sv <- vec_of(sp[party==p & lp==L], buckets)
      mv <- vec_of(man_ref[party==p & lp==L], buckets)
      if (sum(sv) <= 0 || sum(mv) <= 0) next
      out[[length(out)+1L]] <- data.table(tau_name=tn, tau=unname(TEMPERATURES[[tn]]), party=p, lp=L, jsd=jsd(sv, mv))
    }
  }
  rbindlist(out)
}
h1a_sweep <- sweep_one("A", C305_H1A,     BUCKETS_A, manA, andere_excl = FALSE)
h1b_sweep <- sweep_one("B", C305_DEFAULT, BUCKETS_B, manB, andere_excl = TRUE)

chk(nrow(h1a_sweep) > 0 && nrow(h1b_sweep) > 0, "a temperature sweep produced no rows (spec mismatch?)")
flag(all(h1a_sweep$jsd >= 0 & h1a_sweep$jsd <= 1), "H1a sweep JSD outside [0,1]")
flag(all(h1b_sweep$jsd >= 0 & h1b_sweep$jsd <= 1), "H1b sweep JSD outside [0,1]")

# ============================================================================
# Spearman rank correlations across cells, per τ pair
# ============================================================================
rho_table <- function(sw) {
  w <- dcast(sw, party + lp ~ tau_name, value.var = "jsd")
  w <- w[complete.cases(w[, ..TAU_ORDER])]
  flag(nrow(w) >= 3, "fewer than 3 complete cells for Spearman ρ — correlation unstable")
  data.table(
    pair = c("sharp_vs_native", "sharp_vs_flat", "native_vs_flat"),
    spearman_rho = c(cor(w$sharp, w$native, method="spearman"),
                     cor(w$sharp, w$flat,   method="spearman"),
                     cor(w$native, w$flat,  method="spearman")))
}
h1a_rho <- rho_table(h1a_sweep)
h1b_rho <- rho_table(h1b_sweep)

# attach rho columns and write (matches prior output schema)
attach_rho <- function(sw, rho) {
  sw[, `:=`(spearman_rho_sharp_vs_native = rho[pair=="sharp_vs_native", spearman_rho],
            spearman_rho_sharp_vs_flat   = rho[pair=="sharp_vs_flat",   spearman_rho],
            spearman_rho_native_vs_flat  = rho[pair=="native_vs_flat",  spearman_rho])]
  sw[]
}
fwrite(attach_rho(copy(h1a_sweep), h1a_rho), file.path(PATHS$out_dir, "H1a_temperature_sweep.csv"))
fwrite(attach_rho(copy(h1b_sweep), h1b_rho), file.path(PATHS$out_dir, "H1b_temperature_sweep.csv"))

# ============================================================================
# Figures
# ============================================================================
TAU_LINETYPE <- c(sharp="dashed", native="solid", flat="dotted")
TAU_COLOUR   <- c(sharp="#A23B22", native="#264653", flat="#1A6F66")
TAU_LABELS   <- c("tau = 0.5 (sharp)", "tau = 1.0 (native)", "tau = 2.0 (flat)")

sweep_plot <- function(sw, rho, yvar, title, ylab) {
  sw <- sw %>% as_tibble() %>%
    mutate(party = factor(party, levels = PARTIES_KEEP),
           tau_name = factor(tau_name, levels = TAU_ORDER))
  ggplot(sw, aes(lp, .data[[yvar]], colour = tau_name, linetype = tau_name,
                 group = interaction(party, tau_name))) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.6) +
    scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
    scale_colour_manual(values = TAU_COLOUR, labels = TAU_LABELS, name = NULL) +
    scale_linetype_manual(values = TAU_LINETYPE, labels = TAU_LABELS, name = NULL) +
    facet_wrap(~ party, ncol = 3, drop = FALSE) +
    labs(title = title, x = "Legislative period", y = ylab,
         caption = sprintf("Spearman rho across cells: sharp vs native = %.3f; sharp vs flat = %.3f; native vs flat = %.3f.  Acceptance rho >= %.1f.",
                           rho[pair=="sharp_vs_native", spearman_rho], rho[pair=="sharp_vs_flat", spearman_rho],
                           rho[pair=="native_vs_flat", spearman_rho], RHO_MIN)) +
    theme_thesis()
}
p_h1a <- sweep_plot(h1a_sweep, h1a_rho, "jsd",
                    "H1a — Manifesto–speech JSD across temperature settings (A, excl)", "Jensen–Shannon Divergence")
p_h1b <- sweep_plot(h1b_sweep, h1b_rho, "jsd",
                    "H1b — Directional JSD across temperature settings (B, incl, Andere excl)", "Within-domain JSD (B)")
ggsave(file.path(PATHS$fig_dir, "H1a_temperature_sweep.pdf"), p_h1a, width = 10, height = 6.5)
ggsave(file.path(PATHS$fig_dir, "H1a_temperature_sweep.png"), p_h1a, width = 10, height = 6.5, dpi = 200, bg = "white")
ggsave(file.path(PATHS$fig_dir, "H1b_temperature_sweep.pdf"), p_h1b, width = 10, height = 6.5)
ggsave(file.path(PATHS$fig_dir, "H1b_temperature_sweep.png"), p_h1b, width = 10, height = 6.5, dpi = 200, bg = "white")

# ============================================================================
# Console summary + acceptance
# ============================================================================
cat("\n==================== H1a τ-SWEEP RANK CORRELATIONS ====================\n")
print(h1a_rho[, .(pair, spearman_rho = round(spearman_rho, 3))], row.names = FALSE)
cat("\n==================== H1b τ-SWEEP RANK CORRELATIONS ====================\n")
print(h1b_rho[, .(pair, spearman_rho = round(spearman_rho, 3))], row.names = FALSE)
if (any(c(h1a_rho$spearman_rho, h1b_rho$spearman_rho) < RHO_MIN)) {
  message(sprintf("[08] WARNING: at least one rank correlation below %.1f — flag for the writeup.", RHO_MIN))
} else {
  message(sprintf("[08] All tau-pair rank correlations >= %.1f (acceptance OK).", RHO_MIN))
}
message("\n[08] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
})


# ############################################################################
# ##  PART 2 — THRESHOLD (ARGMAX)   (formerly 09_robustness_threshold.R)      ##
# ############################################################################
local({
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
})


# ############################################################################
# ##  PART 3 — INTERVENTION/FILTER   (formerly 10_robustness_prefilter.R)     ##
# ############################################################################
local({
M1         <- "M1_entering"
DPS_BUCKET <- BUCKET_OF_305_A   # "Democracy & Political System" (single source of truth)
COMBOS     <- CJ(filter = c("unfiltered","filtered","filtered_000"), code305 = c("incl","excl"), sorted = FALSE)

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[10][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[10][FLAG] ", msg)
vec_of <- function(dt, buckets) { v <- setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)] <- 0; as.numeric(v) }

# ---- caches ----------------------------------------------------------------
cache <- function(f) { p <- file.path(PATHS$cache_dir, f); chk(file.exists(p), paste0("missing cache: ", p)); as.data.table(readRDS(p)) }
ss <- cache("speech_soft.rds")
md <- cache("manifesto_dists.rds")
ci <- cache("bootstrap_ci.rds")
le <- as.data.table(LP_ELECTION)[, .(lp = as.integer(lp), election_date)]
valid <- ci[is_primary == TRUE, .(party, lp)]
chk(nrow(valid) > 0, "no is_primary cells")
chk(all(c("unfiltered","filtered","filtered_000") %in% unique(ss$filter)),
    "speech_soft missing a filter level (need unfiltered + filtered + filtered_000)")
flag(DPS_BUCKET %in% BUCKETS_A, "DPS_BUCKET not in BUCKETS_A — DPS summary will be NA")

man_ref <- function(c305) {
  m <- merge(md[scheme=="A" & code305==c305 & mapping==M1, .(party, election_date, bucket, share)], le, by="election_date")
  merge(m, valid, by=c("party","lp"))
}
manI <- man_ref("incl"); manE <- man_ref("excl")

# ============================================================================
# Per-combo per-bucket JSD contribution + total, with bootstrap cross-check
# ============================================================================
message("[10] Decomposing per-bucket JSD across the 3×2 intervention grid ...")
long_rows <- list(); total_rows <- list(); xcheck <- numeric(0)
for (j in seq_len(nrow(COMBOS))) {
  f <- COMBOS$filter[j]; c305 <- COMBOS$code305[j]
  sp  <- ss[scheme=="A" & filter==f & tau_name=="native" & code305==c305, .(party, lp, bucket, share)]
  sp  <- merge(sp, valid, by=c("party","lp"))
  man <- if (c305 == "incl") manI else manE
  cells <- unique(sp[, .(party, lp)])
  for (k in seq_len(nrow(cells))) {
    p <- cells$party[k]; L <- cells$lp[k]
    s   <- vec_of(sp[party==p & lp==L], BUCKETS_A); names(s)   <- BUCKETS_A
    m_v <- vec_of(man[party==p & lp==L], BUCKETS_A); names(m_v) <- BUCKETS_A
    if (sum(s) <= 0 || sum(m_v) <= 0) next
    cc <- per_bucket_jsd(s, m_v); tot <- sum(cc); jj <- jsd(s, m_v)
    chk(abs(tot - jj) < 1e-9, sprintf("Σ contrib != jsd at %s/%s %s LP%d", f, c305, p, L))
    long_rows[[length(long_rows)+1L]]  <- data.table(filter=f, code305=c305, party=p, lp=L, bucket=names(cc), contrib=unname(cc))
    total_rows[[length(total_rows)+1L]] <- data.table(filter=f, code305=c305, party=p, lp=L, total_jsd=tot)
    # cross-check vs bootstrap band
    bp <- ci[filter==f & code305==c305 & party==p & lp==L, jsd_point]
    if (length(bp) == 1L) xcheck <- c(xcheck, abs(tot - bp))
  }
}
inter_long  <- rbindlist(long_rows)
inter_total <- rbindlist(total_rows)
flag(length(xcheck) > 0 && max(xcheck) < 1e-6,
     sprintf("combo total JSD disagrees with bootstrap_ci band (max dev %.2e) — speech_soft/bootstrap inconsistent", if (length(xcheck)) max(xcheck) else NA_real_))
message(sprintf("[10][check] max |combo total - bootstrap band point| = %.2e over %d matched cells",
                if (length(xcheck)) max(xcheck) else NA_real_, length(xcheck)))

# ---- per-bucket means per combo --------------------------------------------
per_bucket <- inter_long[, .(mean_contrib = mean(contrib), sd_contrib = sd(contrib), n_cells = .N),
                         by = .(filter, code305, bucket)]
fwrite(per_bucket, file.path(PATHS$out_dir, "intervention_per_bucket_long.csv"))

# ---- DPS-focused 3×2 summary + intervention deltas --------------------------
dps <- inter_long[bucket == DPS_BUCKET, .(dps_contrib = mean(contrib)), by = .(filter, code305)]
tot <- inter_total[, .(total_jsd = mean(total_jsd)), by = .(filter, code305)]
dps_summary <- merge(dps, tot, by = c("filter","code305"))
dps_summary[, dps_share := fifelse(total_jsd > 0, dps_contrib/total_jsd, NA_real_)]
setorder(dps_summary, filter, code305)

# intervention effects on the DPS contribution:
wide_dps <- dcast(dps_summary, filter ~ code305, value.var = "dps_contrib")
wide_dps[, delta_incl_to_excl := excl - incl]                    # 305-mass removal effect (per filter)
across_filter <- dcast(dps_summary, code305 ~ filter, value.var = "dps_contrib")
across_filter[, delta_unfilt_to_filt := filtered - unfiltered]      # procedural-filter effect (per code305)
across_filter[, delta_filt_to_000    := filtered_000 - filtered]    # null-class-filter effect ON TOP of procedural
fwrite(dps_summary, file.path(PATHS$out_dir, "intervention_dps_summary.csv"))

# ============================================================================
# 000-filter vs 305-exclusion: redundancy / double-correction check
# ----------------------------------------------------------------------------
# The filtered_000 null-class filter and the surgical 305-excl both target the
# Democracy-bucket (DPS) inflation. This quantifies, on the DPS JSD contribution:
#   (1) does 305-excl still remove anything once the corpus is 000-filtered?
#       A_new (filtered_000×incl) vs C_dbl (filtered_000×excl)  -> excl redundant?
#   (2) does applying BOTH over-correct (double-count)?  C_dbl pushed below A_new.
#   (3) does the NEW primary land where the OLD corrected primary did?
#       A_new (filtered_000×incl) vs B_old (filtered×excl)  -> like-for-like switch.
# ============================================================================
get_dps <- function(flt, c305) {
  v <- dps_summary[filter == flt & code305 == c305, dps_contrib]
  if (length(v) == 1L) v else NA_real_
}
D_raw <- get_dps("unfiltered",   "incl")   # raw / inflated baseline
B_old <- get_dps("filtered",     "excl")   # OLD primary (procedural filter × 305-excl)
A_new <- get_dps("filtered_000", "incl")   # filtered_000 × incl (null-class filter only, 305 kept)
C_dbl <- get_dps("filtered_000", "excl")   # filtered_000 × excl (null-class filter + surgical 305-excl; declared PRIMARY)

redundancy <- data.table(
  quantity = c("D_raw  unfiltered × incl  (inflated)",
               "B_old  filtered × excl    (old primary)",
               "A_new  filtered_000 × incl (filter-only)",
               "C_dbl  filtered_000 × excl (filter + 305-excl; declared PRIMARY)"),
  dps_contrib = round(c(D_raw, B_old, A_new, C_dbl), 4))
fwrite(redundancy, file.path(PATHS$out_dir, "intervention_000_vs_305_redundancy.csv"))

raw_reduction_000 <- D_raw - A_new                       # DPS divergence the 000-filter already removes
excl_adds_on_000  <- A_new - C_dbl                       # what excl removes ON TOP of the 000-filter
excl_extra_share  <- if (is.finite(raw_reduction_000) && abs(raw_reduction_000) > 1e-9)
                        excl_adds_on_000 / raw_reduction_000 else NA_real_
conv_share        <- if (is.finite(D_raw) && is.finite(B_old) && abs(D_raw - B_old) > 1e-9)
                        abs(A_new - B_old) / abs(D_raw - B_old) else NA_real_
# Pre-stated thresholds (declared before seeing v2 numbers): excl is "redundant"
# if it removes < 15% on top of the 000-filter AND the new primary agrees with
# the old corrected primary to within 15% of the correction magnitude.
verdict_redundant <- is.finite(excl_extra_share) && abs(excl_extra_share) < 0.15 &&
                     is.finite(conv_share)       && conv_share          < 0.15

# ============================================================================
# Figures
# ============================================================================
combo_lab <- function(dt) dt[, combo := factor(paste0(filter, " / ", code305),
                              levels = c("unfiltered / incl","filtered / incl","filtered_000 / incl",
                                         "unfiltered / excl","filtered / excl","filtered_000 / excl"))][]

p_dps <- dps_summary %>% as.data.table() %>% combo_lab() %>% as_tibble() %>%
  ggplot(aes(combo, dps_contrib, fill = code305)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.3f\n(%.0f%% of JSD)", dps_contrib, 100*dps_share)),
            vjust = -0.25, size = 3, lineheight = 0.9) +
  scale_fill_manual(values = c(incl = "#E76F51", excl = "#2A9D8F"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Intervention effect on the DPS-bucket JSD contribution",
       subtitle = "Mean per-cell contribution of Democracy & Political System to the manifesto–speech JSD · Aggregation A, native tau",
       x = NULL, y = "Mean DPS JSD contribution",
       caption = "Left-to-right within each colour: procedural then null-class filter shrink the DPS artefact. (filtered_000 / excl) = primary; (filtered_000 / incl) = filter-only arm; (filtered / excl) = old primary, shown for convergence.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "intervention_dps_bars.pdf"), p_dps, width = 11, height = 6)
ggsave(file.path(PATHS$fig_dir, "intervention_dps_bars.png"), p_dps, width = 11, height = 6, dpi = 200, bg = "white")

p_all <- per_bucket %>% as.data.table() %>% combo_lab() %>% as_tibble() %>%
  mutate(bucket = fct_reorder(bucket, mean_contrib),
         is_dps = bucket == DPS_BUCKET) %>%
  ggplot(aes(mean_contrib, bucket, fill = is_dps)) +
  geom_col(width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c(`FALSE` = "#264653", `TRUE` = "#E76F51"), guide = "none") +
  facet_wrap(~ combo, ncol = 3) +
  labs(title = "Per-bucket JSD contribution across the intervention grid",
       subtitle = "DPS (highlighted) absorbs almost all of the intervention effect; substantive buckets are ~stable",
       x = "Mean JSD contribution", y = NULL,
       caption = "If the interventions were distorting substance, non-DPS bars would move across panels. They do not.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "intervention_all_buckets.pdf"), p_all, width = 13, height = 7)
ggsave(file.path(PATHS$fig_dir, "intervention_all_buckets.png"), p_all, width = 13, height = 7, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== DPS 3×2 SUMMARY ====================\n")
print(dps_summary[, .(filter, code305, dps_contrib = round(dps_contrib,4),
                      total_jsd = round(total_jsd,4), dps_share = round(dps_share,3))], row.names = FALSE)
cat("\n----- 305-mass removal effect on DPS contribution (excl - incl), per filter -----\n")
print(wide_dps[, .(filter, incl = round(incl,4), excl = round(excl,4), delta_incl_to_excl = round(delta_incl_to_excl,4))], row.names = FALSE)
cat("\n----- filter-ladder effect on DPS contribution, per code305 -----\n")
print(across_filter[, .(code305, unfiltered = round(unfiltered,4), filtered = round(filtered,4),
                        filtered_000 = round(filtered_000,4),
                        delta_unfilt_to_filt = round(delta_unfilt_to_filt,4),
                        delta_filt_to_000 = round(delta_filt_to_000,4))], row.names = FALSE)

cat("\n==================== 000-FILTER vs 305-EXCLUSION (redundancy) ====================\n")
print(redundancy, row.names = FALSE)
cat(sprintf("\n  DPS divergence removed by the 000-filter alone (incl)       : %.4f\n", raw_reduction_000))
cat(sprintf("  305-excl removes ADDITIONALLY on top of the 000-filter      : %+.4f  (%.0f%% of the above)\n",
            excl_adds_on_000, 100*excl_extra_share))
cat(sprintf("  filter-only (filtered_000×incl) vs old (filtered×excl)      : %+.4f  (%.0f%% of the correction)\n",
            A_new - B_old, 100*conv_share))
cat("\n  --- pre-stated verdict (is the surgical 305-excl still needed?) ---\n")
if (isTRUE(verdict_redundant)) {
  cat("  REDUNDANT: once the corpus is 000-filtered, 305-excl removes a negligible extra\n")
  cat("  slice of the DPS divergence, and the new primary lands at the old corrected one.\n")
  cat("  -> incl is the honest primary (keeps the matched 305 sockel); excl-on-top would\n")
  cat("     double-correct. Report excl only as a robustness arm. (Corroborate with 11 PART 2.)\n")
} else {
  cat("  NOT REDUNDANT on this evidence: 305-excl still removes a non-trivial slice beyond the\n")
  cat("  000-filter, or the two primaries diverge. -> keep excl as a primary candidate and\n")
  cat("  adjudicate via the 11 PART 2 speech-vs-manifesto 305 asymmetry.\n")
}
message("\n[10] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
})
