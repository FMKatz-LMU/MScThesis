# ============================================================================
# 10_robustness_prefilter.R — Intervention-effect decomposition (filter × 305)
# ----------------------------------------------------------------------------
# Shows WHERE in the bucket space the two corpus interventions act, by
# decomposing the per-cell JSD into per-bucket contributions across the full
# 3×2 grid of interventions:
#
#     filter   ∈ {unfiltered, filtered, filtered_000}  (procedural + null-class)
#     code305  ∈ {incl, excl}               (residual 305-mass removal)
#
# Aggregation A, native τ. The manifesto reference is matched on code305
# (incl-speech vs incl-manifesto, excl-speech vs excl-manifesto) so JSD always
# has a common support. Restricted to the 41 valid cells.
#
# This HOSTS the H2a before/after AND the 000-vs-305 redundancy check. The
# declared PRIMARY (00_config) is filtered_000 × excl; whether the surgical
# 305-excl earns its place on top of the 000-filter is adjudicated by the
# pre-committed verdict below. The artefact-laden "before" is (unfiltered, incl);
# (filtered, excl) is the OLD primary, kept for the convergence comparison. All
# three interventions (procedural, null-class,
# 305-mass) act almost entirely on the Democracy & Political System (DPS) bucket,
# leaving the substantive buckets ~unchanged.
#
# NOTE: the earlier tight-LENGTH-filter proxy (old 10) is removed entirely;
# it is superseded by the real procedural pre-filter implemented upstream.
#
# Cross-check: each combo's per-cell total JSD is verified against the matching
# bootstrap_ci band (filter × code305), linking this script to the headline CIs.
#
# Outputs: intervention_per_bucket_long.csv, intervention_dps_summary.csv,
#   intervention_000_vs_305_redundancy.csv,
#   figures/intervention_dps_bars.{pdf,png}, figures/intervention_all_buckets.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
source(here::here("02_helpers.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

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
