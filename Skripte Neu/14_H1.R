# ============================================================================
# 14_H1.R — Hypotheses H1a, H1b, and the between-party benchmarks
# ----------------------------------------------------------------------------
# Reads the NEW long-format caches written by 03 and selects the primary spec
# by column (never hard-codes a spec value).
#
#   H1a  Per (party, LP) manifesto-vs-speech JSD, Aggregation A, M1 mapping,
#        with 95% bootstrap intervals. Source: bootstrap_ci[is_primary] —
#        the filtered_000 × excl × native band (primary; null-class removed at source,
#        305 procedural artefact surgically excluded).
#
#   H1b  Directional composition within the four B domains (manifesto vs speech)
#        + per-cell overall JSD on B. Spec: scheme B, filtered_000, native, incl.
#        (Directional layer — reported as EXPLORATORY; κ≈0.27.)
#
#   Benchmarks (Justyna's H1a feedback: absolute JSD is uninterpretable alone)
#        On the SAME footing as H1a (scheme A, filtered_000, native, excl):
#        (1) Reference scale — all unordered pairwise between-party JSDs per LP,
#            speech-side and manifesto-side.
#        (2) Nearest-manifesto test — for each (party, LP), is its speech closest
#            to its OWN manifesto among all parties' manifestos?
#        incl versions are deferred to robustness (not computed here).
#
# Outputs (results/empirics/):
#   H1a_jsd_table.csv, excluded_cells.csv, figures/H1a_jsd_dotplot.{pdf,png}
#   H1b_directional_table.csv, H1b_overall_jsd.csv, figures/H1b_directional_bars.{pdf,png}
#   benchmark_pairwise_jsd.csv, benchmark_nearest_manifesto.csv,
#   figures/benchmark_reference_scale.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

# ---- spec selectors (read from config; never hard-code a value) ------------
TAU_NATIVE     <- "native"
C305_H1A       <- CODE305_PRIMARY_H1A_H2A   # "excl"  (H1a + benchmarks; PRIMARY)
C305_DEFAULT   <- CODE305_PRIMARY_DEFAULT   # "incl"  (H1b)
M1             <- "M1_entering"

# ---- tiny check helpers ----------------------------------------------------
chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[04][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[04][FLAG] ", msg)

# named bucket vector from a long (bucket, share) slice, ordered & zero-filled
vec_of <- function(dt, buckets) {
  v <- setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)] <- 0; as.numeric(v)
}

# ============================================================================
# 0. Load caches + select primary specs
# ============================================================================
cache <- function(f) {
  p <- file.path(PATHS$cache_dir, f)
  chk(file.exists(p), paste0("missing cache: ", p, " — run 03 first."))
  as.data.table(readRDS(p))
}
ci <- cache("bootstrap_ci.rds")
ss <- cache("speech_soft.rds")
md <- cache("manifesto_dists.rds")

# schema guards (flag missing columns early, with a clear message)
chk(all(c("party","lp","filter","code305","jsd_point","lo95","hi95","n_sent","is_primary") %in% names(ci)),
    "bootstrap_ci is missing expected columns")
chk(all(c("party","lp","scheme","filter","tau_name","code305","bucket","share","n_sent") %in% names(ss)),
    "speech_soft is missing expected columns")
chk(all(c("party","election_date","mapping","scheme","code305","bucket","share") %in% names(md)),
    "manifesto_dists is missing expected columns")

# LP <-> entering-election date (M1)
le <- as.data.table(LP_ELECTION)[, .(lp = as.integer(lp), election_date)]

# primary slices ------------------------------------------------------------
spkA <- ss[scheme == "A" & filter == FILTER_PRIMARY & tau_name == TAU_NATIVE & code305 == C305_H1A,
           .(party, lp, bucket, share, n_sent)]                         # speech A, excl
spkB <- ss[scheme == "B" & filter == FILTER_PRIMARY & tau_name == TAU_NATIVE & code305 == C305_DEFAULT,
           .(party, lp, bucket, share, n_sent)]                         # speech B, incl
manA <- md[scheme == "A" & code305 == C305_H1A     & mapping == M1, .(party, election_date, bucket, share)]
manB <- md[scheme == "B" & code305 == C305_DEFAULT & mapping == M1, .(party, election_date, bucket, share)]
manA <- merge(manA, le, by = "election_date")                          # add lp
manB <- merge(manB, le, by = "election_date")

# non-empty guards: catch a missing/mis-named spec immediately
chk(nrow(spkA) > 0, "speech A/filtered/native/excl slice is EMPTY (spec mismatch with 03?)")
chk(nrow(spkB) > 0, "speech B/filtered/native/incl slice is EMPTY")
chk(nrow(manA) > 0, "manifesto A/excl/M1 slice is EMPTY")
chk(nrow(manB) > 0, "manifesto B/incl/M1 slice is EMPTY")

# bucket-set guards (A is a 9-bucket partition; B has the 9 directional + Andere)
flag(setequal(unique(spkA$bucket), BUCKETS_A), "speech A buckets != BUCKETS_A")
flag(all(BUCKETS_B %in% unique(spkB$bucket)),   "speech B is missing some BUCKETS_B")

# ============================================================================
# 1. H1a — salience JSD with bootstrap intervals (from bootstrap_ci primary)
# ============================================================================
message("[H1a] Reading primary bootstrap band (filtered_000 × excl × native) ...")
h1a <- ci[is_primary == TRUE]
chk(nrow(h1a) > 0, "bootstrap_ci has no is_primary band")
flag(all(h1a$filter == BOOTSTRAP_PRIMARY_FILTER & h1a$code305 == BOOTSTRAP_PRIMARY_CODE305),
     "is_primary band is not exactly filtered_000×excl — check 03's BOOTSTRAP_PRIMARY_*")

# integrity of the band
flag(all(h1a$jsd_point >= 0 & h1a$jsd_point <= 1), "some H1a JSD outside [0,1]")
flag(all(h1a$lo95 <= h1a$jsd_point + 1e-9 & h1a$jsd_point <= h1a$hi95 + 1e-9),
     "some H1a CI does not bracket its point estimate")
flag(!any(h1a$party == "FDP" & h1a$lp == 18L), "FDP LP18 present in H1a band (should be excluded)")
flag(all(h1a$n_sent >= N_SENT_MIN), paste0("some H1a cell has n_sent < ", N_SENT_MIN))

# assemble the H1a table (add election_date; presentation columns)
h1a_tbl <- merge(h1a[, .(party, lp, jsd = jsd_point, lo95, hi95, n_sent)], le, by = "lp")[
  , .(party, lp, election_date, jsd, lo95, hi95, n_sent, manifesto_present = TRUE)]
setorder(h1a_tbl, party, lp)

# valid analysis cells = the set H1a uses (the 4 exclusions + the n_sent gate are
# already baked into the bootstrap is_primary band). Reused by H1b and the
# benchmarks so all three report on the SAME cells — this is what stops e.g. the
# FDP-LP18 mis-attributions (or any sub-1000-sentence cell) leaking into H1b.
valid     <- unique(h1a_tbl[, .(party, lp)])
valid_key <- paste(valid$party, valid$lp, sep = "|")

# --- document exclusions (defensive; bootstrap already applied them) --------
all_speech_cells <- unique(spkA[, .(party, lp)])
excluded_cells <- merge(all_speech_cells, le, by = "lp")[
  (party == "FDP" & lp == 18L)][
  , exclusion_reason := "FDP absent from Bundestag LP18 (missed 5% threshold 2013); n_sent are mis-attributions"]
if (nrow(excluded_cells) > 0)
  fwrite(excluded_cells, file.path(PATHS$out_dir, "excluded_cells.csv"))

fwrite(h1a_tbl, file.path(PATHS$out_dir, "H1a_jsd_table.csv"))
message(sprintf("[H1a] %d valid cells written.", nrow(h1a_tbl)))

# ============================================================================
# 2. CROSS-CHECK: speech_soft cell-JSD must equal the bootstrap point
#    (independent cache paths; if they disagree, a real inconsistency exists)
# ============================================================================
xcheck <- h1a_tbl[, .(party, lp, jsd_boot = jsd)]
xcheck[, jsd_ss := NA_real_]
for (k in seq_len(nrow(xcheck))) {
  p <- xcheck$party[k]; L <- xcheck$lp[k]
  sv <- vec_of(spkA[party == p & lp == L], BUCKETS_A)
  mv <- vec_of(manA[party == p & lp == L], BUCKETS_A)
  if (sum(sv) > 0 && sum(mv) > 0) xcheck$jsd_ss[k] <- jsd(sv, mv)
}
xcheck[, d := abs(jsd_ss - jsd_boot)]
maxd <- max(xcheck$d, na.rm = TRUE)
message(sprintf("[H1a][cross-check] max |speech_soft JSD - bootstrap point| = %.3e", maxd))
flag(is.finite(maxd) && maxd < 1e-6,
     sprintf("speech_soft and bootstrap_ci DISAGREE on the cell JSD (max %.3e) — investigate 03.", maxd))

# ============================================================================
# 3. H1a PLOT
# ============================================================================
p_h1a <- h1a_tbl %>% as_tibble() %>%
  mutate(party = factor(party, levels = PARTIES_KEEP)) %>%
  ggplot(aes(lp, jsd, colour = party, group = party)) +
  geom_line(linewidth = 0.6, alpha = 0.5) +
  geom_errorbar(aes(ymin = lo95, ymax = hi95), width = 0.18, linewidth = 0.4) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = PARTY_COLOURS, drop = FALSE) +
  facet_wrap(~ party, ncol = 3, drop = FALSE) +
  labs(title = "H1a — Manifesto–speech salience divergence (Aggregation A)",
       subtitle = "Per-cell JSD with 95% sentence-bootstrap intervals · primary spec: filtered_000 (procedural + null-class removed), native tau, code 305 excluded",
       x = "Legislative period", y = "Jensen–Shannon Divergence",
       caption = "M1 mapping: speeches in LP X vs. the manifesto of the election that produced LP X.") +
  theme_thesis() + theme(legend.position = "none")
ggsave(file.path(PATHS$fig_dir, "H1a_jsd_dotplot.pdf"), p_h1a, width = 9, height = 6.5, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "H1a_jsd_dotplot.png"), p_h1a, width = 9, height = 6.5, dpi = 200, bg = "white")

# ============================================================================
# 4. H1b — directional composition + overall JSD on B (EXPLORATORY)
# ============================================================================
message("\n[H1b] Building directional composition (scheme B, incl) ...")
b_lookup <- as.data.table(AGG_B)[, .(domain_B, bucket_B)] |> unique()

within_domain <- function(dt, group_cols) {
  d <- merge(dt[bucket %in% BUCKETS_B], b_lookup, by.x = "bucket", by.y = "bucket_B")
  setnames(d, c("domain_B", "bucket"), c("domain", "sub_bucket"))
  d[, share_within_domain := if (sum(share) > 0) share / sum(share) else 0,
    by = c(group_cols, "domain")]
  d
}
spkB_dir <- within_domain(spkB, c("party", "lp"))[, .(party, lp, domain, sub_bucket, share_within_domain)][, source := "Speech"]
manB_dir <- within_domain(manB, c("party", "election_date"))[
  , .(party, lp, domain, sub_bucket, share_within_domain)][, source := "Manifesto"]

h1b_long <- rbindlist(list(spkB_dir, manB_dir), use.names = TRUE)
h1b_long <- h1b_long[paste(party, lp, sep = "|") %in% valid_key]   # restrict to H1a's valid cells (drops FDP LP18 + sub-1000 cells)
flag(all(abs(h1b_long[, .(s = sum(share_within_domain)), by = .(party, lp, domain, source)]$s - 1) < 1e-6 |
         h1b_long[, .(s = sum(share_within_domain)), by = .(party, lp, domain, source)]$s == 0),
     "some within-domain composition does not sum to 1")
fwrite(h1b_long, file.path(PATHS$out_dir, "H1b_directional_table.csv"))

# overall per-cell JSD on B (directional buckets only, Andere excluded)
b_renorm <- function(dt, key) {
  d <- dt[bucket != "Andere"]
  d[, share_r := if (sum(share) > 0) share / sum(share) else 0, by = key]
  d
}
sB <- b_renorm(spkB, c("party", "lp"))[, .(party, lp, bucket, s = share_r)]
mB <- b_renorm(manB, c("party", "lp"))[, .(party, lp, bucket, m = share_r)]
h1b_jsd <- merge(sB, mB, by = c("party", "lp", "bucket"))[
  , .(jsd_B = jsd(s, m)), by = .(party, lp)]
h1b_jsd <- h1b_jsd[paste(party, lp, sep = "|") %in% valid_key]     # restrict to H1a's valid cells
fwrite(h1b_jsd, file.path(PATHS$out_dir, "H1b_overall_jsd.csv"))

# H1b plot (paired stacked bars, pooled across LP)
SUB_COLOURS <- c("Marktliberalismus"="#F4A261","Staatsintervention"="#264653",
  "Wirtschaft Allgemein"="#A8C0CC","Sozialstaat Ausbau"="#2A9D8F","Sozialstaat Begrenzung"="#E76F51",
  "Migration restriktiv"="#6D597A","Migration liberal"="#B5838D","Pro-EU"="#003399","Contra-EU"="#FFCC00")
h1b_pooled <- h1b_long %>% as_tibble() %>%
  group_by(party, source, domain, sub_bucket) %>%
  summarise(share_within_domain = mean(share_within_domain), .groups = "drop") %>%
  mutate(sub_bucket = factor(sub_bucket, levels = names(SUB_COLOURS)),
         source = factor(source, levels = c("Manifesto", "Speech")),
         party = factor(party, levels = PARTIES_KEEP),
         domain = factor(domain, levels = DOMAINS_B))
strip_labels <- h1b_jsd %>% as_tibble() %>% group_by(party) %>%
  summarise(m = mean(jsd_B, na.rm = TRUE), .groups = "drop") %>%
  transmute(party = as.character(party), lab = sprintf("%s  (mean JSD = %.3f)", party, m)) %>%
  { setNames(.$lab, .$party) }
p_h1b <- h1b_pooled %>%
  ggplot(aes(source, share_within_domain, fill = sub_bucket)) +
  geom_col(width = 0.7) + geom_vline(xintercept = 1.5, colour = "grey85", linewidth = 0.3) +
  facet_grid(rows = vars(party), cols = vars(domain), switch = "y",
             labeller = labeller(party = strip_labels)) +
  scale_fill_manual(values = SUB_COLOURS, name = NULL, guide = guide_legend(nrow = 3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "H1b — Directional composition: manifesto (M) vs. speech (S)  [EXPLORATORY]",
       subtitle = "Within-domain share of directional sub-labels, averaged across LP 13–20 (scheme B, code 305 incl)",
       x = NULL, y = NULL,
       caption = "Directional layer reported as exploratory (human kappa~0.27). Row strips show each party's mean overall JSD on B. M1 mapping.") +
  theme_thesis() +
  theme(panel.spacing.x = unit(0.7, "lines"),
        strip.text.y.left = element_text(angle = 0, face = "bold"),
        axis.text.x = element_text(size = 9))
ggsave(file.path(PATHS$fig_dir, "H1b_directional_bars.pdf"), p_h1b, width = 11, height = 9, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "H1b_directional_bars.png"), p_h1b, width = 11, height = 9, dpi = 200, bg = "white")

# ============================================================================
# 5. BENCHMARKS — restrict to the valid cells (same footing as H1a)
# ============================================================================
message("\n[Benchmark] Computing between-party reference scale + nearest-manifesto (scheme A, excl) ...")
# `valid` is the analysis-cell set defined once after the H1a table (above) — reused here.
spkA_v <- merge(spkA, valid, by = c("party", "lp"))
manA_v <- merge(manA, valid, by = c("party", "lp"))               # manifesto restricted to valid cells

# (5a) Reference scale: all unordered pairwise between-party JSDs per LP
pairwise <- function(dist_dt, side) {
  out <- list()
  for (L in sort(unique(dist_dt$lp))) {
    sub <- dist_dt[lp == L]; ps <- sort(unique(sub$party))
    if (length(ps) < 2L) next
    vl <- lapply(ps, function(p) vec_of(sub[party == p], BUCKETS_A)); names(vl) <- ps
    for (a in 1:(length(ps) - 1L)) for (b in (a + 1L):length(ps)) {
      out[[length(out) + 1L]] <- data.table(lp = L, side = side,
        party_i = ps[a], party_j = ps[b], jsd = jsd(vl[[a]], vl[[b]]))
    }
  }
  rbindlist(out)
}
ref <- rbindlist(list(pairwise(spkA_v, "speech"), pairwise(manA_v, "manifesto")))
flag(all(ref$jsd >= 0 & ref$jsd <= 1), "some between-party JSD outside [0,1]")
fwrite(ref, file.path(PATHS$out_dir, "benchmark_pairwise_jsd.csv"))

# (5b) Nearest-manifesto: each party's speech vs every party's manifesto at that LP
near <- list()
for (k in seq_len(nrow(valid))) {
  p <- valid$party[k]; L <- valid$lp[k]
  sv <- vec_of(spkA_v[party == p & lp == L], BUCKETS_A)
  qs <- sort(unique(manA_v[lp == L, party]))
  if (!length(qs) || sum(sv) <= 0) next
  d <- vapply(qs, function(q) jsd(sv, vec_of(manA_v[party == q & lp == L], BUCKETS_A)), numeric(1))
  own_in <- p %in% qs
  near[[length(near) + 1L]] <- data.table(
    party = p, lp = L,
    jsd_own       = if (own_in) unname(d[match(p, qs)]) else NA_real_,
    jsd_min       = min(d),
    nearest_party = qs[which.min(d)],
    rank_own      = if (own_in) as.integer(rank(d, ties.method = "min")[match(p, qs)]) else NA_integer_,
    n_manifestos  = length(qs),
    own_is_nearest = own_in && (qs[which.min(d)] == p))
}
near <- rbindlist(near)
fwrite(near, file.path(PATHS$out_dir, "benchmark_nearest_manifesto.csv"))

# CROSS-CHECK 2: nearest-manifesto own-JSD must equal the H1a point
nn <- merge(near[, .(party, lp, jsd_own)], h1a_tbl[, .(party, lp, jsd)], by = c("party", "lp"))
maxd2 <- max(abs(nn$jsd_own - nn$jsd), na.rm = TRUE)
message(sprintf("[Benchmark][cross-check] max |nearest own-JSD - H1a point| = %.3e", maxd2))
flag(is.finite(maxd2) && maxd2 < 1e-6,
     sprintf("nearest-manifesto own-JSD != H1a point (max %.3e) — speech_A/manifesto_A inconsistent with bootstrap.", maxd2))

# (5c) Benchmark figure: per-LP between-party speech JSD range, with each party's own JSD overlaid
ref_speech <- ref %>% as_tibble() %>% filter(side == "speech")
own_pts    <- h1a_tbl %>% as_tibble() %>% transmute(lp, party = factor(party, levels = PARTIES_KEEP), jsd)
p_bench <- ggplot() +
  geom_boxplot(data = ref_speech, aes(x = factor(lp), y = jsd),
               width = 0.5, fill = "grey90", colour = "grey55", outlier.shape = NA) +
  geom_jitter(data = own_pts, aes(x = factor(lp), y = jsd, colour = party),
              width = 0.12, size = 2, alpha = 0.9) +
  scale_colour_manual(values = PARTY_COLOURS, drop = FALSE, name = NULL) +
  labs(title = "Benchmark — own manifesto–speech divergence vs. the between-party reference",
       subtitle = "Grey box: spread of pairwise between-party speech JSDs per LP. Points: each party's own H1a JSD (scheme A, excl).",
       x = "Legislative period", y = "Jensen–Shannon Divergence",
       caption = "Points below the box = the party's speeches are closer to its own manifesto than parties typically are to each other.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "benchmark_reference_scale.pdf"), p_bench, width = 9, height = 6, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "benchmark_reference_scale.png"), p_bench, width = 9, height = 6, dpi = 200, bg = "white")

# ============================================================================
# 6. Console summary
# ============================================================================
cat("\n==================== H1a SUMMARY (filtered_000 × excl × native) ====================\n")
print(h1a_tbl[, .(mean_jsd = round(mean(jsd), 4), median_jsd = round(median(jsd), 4),
                  n_cells = .N), by = party][order(party)], row.names = FALSE)
cat("\n==================== Nearest-manifesto: own = closest? ====================\n")
print(near[, .(cells = .N, own_is_nearest = sum(own_is_nearest),
               pct = round(100 * mean(own_is_nearest), 1)), by = party][order(-pct)], row.names = FALSE)
cat(sprintf("\nOverall: own manifesto is the nearest in %d / %d cells (%.1f%%).\n",
            sum(near$own_is_nearest), nrow(near), 100 * mean(near$own_is_nearest)))
cat("\n==================== H1b OVERALL JSD ON B (EXPLORATORY) ====================\n")
print(h1b_jsd[, .(mean_jsd_B = round(mean(jsd_B, na.rm = TRUE), 4), n_cells = .N), by = party][order(party)], row.names = FALSE)

message("\n[H1] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
