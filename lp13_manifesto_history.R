# ============================================================================
# lp13_manifesto_history.R — Diagnostic for the LP-13 (1994) manifesto anomaly
# ----------------------------------------------------------------------------
# Question: are the 1994 manifestos (which drive the LP-13 JSD spike) an outlier
# in each party's LONG manifesto history, or were manifesto-to-manifesto jumps
# of that size common before 1994? Manifesto side only, at the aggregate
# 9-bucket (Aggregation A) level, JSD between consecutive manifestos.
#
# Runs from the project ROOT; reuses 00_config.R (AGG_A, jsd, PATHS, theme) and
# the same MARPOR access as 05_pull_marpor.R, but pulls the FULL German history
# (all elections, not just LP 13-20), so it can look back before 1994.
#
# Outputs (results/empirics/):
#   manifesto_history_jsd.csv
#   figures/manifesto_change_over_time.{png,pdf}      (JSD to previous manifesto)
#   figures/manifesto_composition_over_time.{png,pdf} (9-bucket stacked area)
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table); library(manifestoR) })
mp_setapikey(here::here("manifesto_apikey.txt"))

# --- MARPOR party ids -> display names (as in 05; 41112 added = pre-1990 West Greens)
party_lookup <- data.table(
  marpor_party = c(41320L, 41521L, 41420L, 41113L, 41112L, 41221L, 41222L, 41223L, 41953L),
  party        = c("SPD", "CDU/CSU", "FDP", "Grüne", "Grüne", "Linke", "Linke", "Linke", "AfD"))

# --- pull full main dataset, keep our parties across ALL elections ----------
mpds     <- as.data.table(mp_maindataset())
per_cols <- grep("^per[0-9]{3}$", names(mpds), value = TRUE); stopifnot(length(per_cols) == 56)

d <- mpds[party %in% party_lookup$marpor_party, c("party", "date", per_cols), with = FALSE]
for (col in per_cols) set(d, which(is.na(d[[col]])), col, 0)          # NA -> 0 (CMP practice)
rs <- rowSums(d[, ..per_cols]); d <- d[rs > 0]; rs <- rs[rs > 0]
d[, (per_cols) := lapply(.SD, function(x) x / rs), .SDcols = per_cols] # unit-sum per manifesto
d[, party := party_lookup$party[match(party, party_lookup$marpor_party)]]
d[, year  := as.integer(substr(sprintf("%06d", date), 1, 4))]

# --- aggregate the 56 codes -> 9 A-buckets ----------------------------------
code_to_A <- setNames(AGG_A$bucket_A, as.character(AGG_A$code))
ind <- matrix(0, length(per_cols), length(BUCKETS_A), dimnames = list(NULL, BUCKETS_A))
for (i in seq_along(per_cols)) ind[i, code_to_A[[sub("per", "", per_cols[i])]]] <- 1
B  <- as.matrix(d[, ..per_cols]) %*% ind                              # rows x 9, sum to 1
bd <- cbind(d[, .(party, year)], as.data.table(B))
bd <- bd[, lapply(.SD, mean), by = .(party, year), .SDcols = BUCKETS_A]  # collapse any dup (party, year)
setorder(bd, party, year)

# --- per party: JSD to previous manifesto + JSD to the party's centroid -----
res <- list()
for (p in unique(bd$party)) {
  x <- bd[party == p]; M <- as.matrix(x[, ..BUCKETS_A])
  if (nrow(x) < 2) next
  centroid <- colMeans(M)
  jp <- c(NA_real_, vapply(2:nrow(x), function(i) jsd(M[i, ], M[i - 1, ]), numeric(1)))
  jc <- vapply(seq_len(nrow(x)), function(i) jsd(M[i, ], centroid), numeric(1))
  res[[p]] <- data.table(party = p, year = x$year, jsd_prev = jp, jsd_centroid = jc)
}
hist <- rbindlist(res)
fwrite(hist, here::here("results", "empirics", "manifesto_history_jsd.csv"))

# --- verdict: is 1994 exceptional vs the SAME party's pre-1994 changes? ------
cat("\n============= 1994 vs pre-1994 manifesto change (JSD to previous manifesto) =============\n")
for (p in c("CDU/CSU", "SPD", "FDP", "Grüne")) {
  h <- hist[party == p & !is.na(jsd_prev)]
  if (!nrow(h[year == 1994])) next
  pre <- h[year < 1994]; v94 <- h[year == 1994, jsd_prev]
  rk  <- rank(-h$jsd_prev, ties.method = "min")[h$year == 1994]
  cat(sprintf("%-8s  1994 = %.4f  |  pre-1994 mean = %.4f, max = %.4f (%s)  |  rank of 1994 among %d changes: %d (1 = largest)\n",
              p, v94,
              if (nrow(pre)) mean(pre$jsd_prev) else NA_real_,
              if (nrow(pre)) max(pre$jsd_prev)  else NA_real_,
              if (nrow(pre)) pre[which.max(jsd_prev), year] else "-",
              nrow(h), rk))
}
cat("\nReading: if 1994 dwarfs the pre-1994 mean/max and ranks 1, the 1994 manifesto is a genuine\n",
    "historical outlier; if earlier changes were of similar size, large manifesto shifts were normal.\n", sep = "")

# --- figure 1: manifesto change over time (JSD to previous), by party -------
p1 <- hist %>% as_tibble() %>% filter(!is.na(jsd_prev)) %>%
  mutate(party = factor(party, levels = PARTIES_KEEP), is94 = year == 1994) %>%
  ggplot(aes(year, jsd_prev)) +
  geom_vline(xintercept = 1994, linetype = "dotted", colour = "grey60") +
  geom_line(aes(colour = party), linewidth = 0.6) +
  geom_point(aes(colour = party, size = is94)) +
  scale_size_manual(values = c(`FALSE` = 1.4, `TRUE` = 3.2), guide = "none") +
  scale_colour_manual(values = PARTY_COLOURS, drop = FALSE, guide = "none") +
  facet_wrap(~ party, ncol = 2, scales = "free_x") +
  labs(title = "Manifesto change between consecutive elections (Aggregation A)",
       subtitle = "JSD between each manifesto and the party's previous manifesto; dotted line = 1994 (LP 13)",
       x = "Election year", y = "JSD to previous manifesto",
       caption = "A 1994 point standing out from earlier changes indicates the 1994 manifesto is a genuine historical outlier.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "manifesto_change_over_time.png"), p1, width = 9, height = 6, dpi = 200, bg = "white")
ggsave(file.path(PATHS$fig_dir, "manifesto_change_over_time.pdf"), p1, width = 9, height = 6)

# --- figure 2: manifesto topic composition over time, by party --------------
p2 <- bd %>% pivot_longer(all_of(BUCKETS_A), names_to = "bucket", values_to = "share") %>%
  mutate(party = factor(party, levels = PARTIES_KEEP)) %>%
  ggplot(aes(year, share, fill = bucket)) +
  geom_area(colour = "white", linewidth = 0.1) +
  geom_vline(xintercept = 1994, linetype = "dotted", colour = "grey30") +
  facet_wrap(~ party, ncol = 2, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Manifesto topic composition over time (Aggregation A)",
       subtitle = "Nine-bucket manifesto share per election; dotted line = 1994 (LP 13)",
       x = "Election year", y = "Share of manifesto", fill = NULL) +
  theme_thesis() + theme(legend.position = "bottom")
ggsave(file.path(PATHS$fig_dir, "manifesto_composition_over_time.png"), p2, width = 11, height = 7.5, dpi = 200, bg = "white")
ggsave(file.path(PATHS$fig_dir, "manifesto_composition_over_time.pdf"), p2, width = 11, height = 7.5)

cat("\nDone. Figures in ", PATHS$fig_dir, "\nTable: results/empirics/manifesto_history_jsd.csv\n", sep = "")
