# ============================================================================
# 08_robustness_temperature.R — Temperature sweep robustness on H1a / H1b
# ----------------------------------------------------------------------------
# Reads only from cache. For τ ∈ {sharp 0.5, native 1.0, flat 2.0} on the
# SPEECH-side softmax, recomputes per-cell JSD vs the (τ-invariant) M1
# manifesto: H1a on Aggregation A (filtered_000, EXCL) and H1b overall directional
# JSD on Aggregation B (filtered_000, INCL, within-domain renorm, Andere excluded).
#
# The manifesto side carries no softmax, so it does NOT vary with τ.
# Acceptance (MIv2 §3.3): cross-cell ordering of JSD preserved across τ
# (Spearman ρ ≥ 0.9). Rank correlations are reported.
#
# Outputs: H1a_temperature_sweep.csv, H1b_temperature_sweep.csv,
#          figures/H1a_temperature_sweep.{pdf,png}, figures/H1b_temperature_sweep.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
source(here::here("02_helpers.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

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
