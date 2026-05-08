# ============================================================================
# 04_H1.R — Hypotheses 1a and 1b
# ----------------------------------------------------------------------------
# H1a: Per (party, LP) JSD between manifesto and speech under Aggregation A,
#      M1 mapping, with 95% bootstrap intervals.
#      Bootstrap source (in order of preference):
#        1. speech_prob_matrix.rds  (per-sentence bucket matrix built by 03)
#        2. jsd_permutation.rds     (pre-computed CIs from compute_jsd.R)
#        3. Point estimates only (no CI)
#
# H1b: Paired stacked bars of directional sub-label composition
#      (manifesto vs. speech) within the four B domains.
#
# Outputs:
#   results/empirics/H1a_jsd_table.csv
#   results/empirics/figures/H1a_jsd_dotplot.pdf / .png
#   results/empirics/H1b_directional_table.csv
#   results/empirics/H1b_overall_jsd.csv
#   results/empirics/figures/H1b_directional_bars.pdf / .png
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list  <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list  <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))
mat_list <- readRDS(file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))

# Read primary specification by explicit key (spec §3.4).
# Falls back to legacy $A / $B shims for backward compatibility.
.primary_A_key <- sprintf("soft_tau%s__A", format(TAU_PRIMARY, nsmall = 1))
.primary_B_key <- sprintf("soft_tau%s__B", format(TAU_PRIMARY, nsmall = 1))
speech_A    <- if (.primary_A_key %in% names(sd_list)) sd_list[[.primary_A_key]] else sd_list$A
speech_B    <- if (.primary_B_key %in% names(sd_list)) sd_list[[.primary_B_key]] else sd_list$B
manifesto_A <- md_list$A
manifesto_B <- md_list$B

bucket_mat <- mat_list$bucket_mat   # n x 9 matrix, or NULL
cell_meta  <- mat_list$cell_meta    # tibble(party, lp, .row), or NULL
has_matrix <- !is.null(bucket_mat)

# ---- Fallback: pre-computed bootstrap CIs ----------------------------------
perm_data <- NULL
if (!has_matrix && file.exists(PATHS$jsd_permutation)) {
  message("[H1a] No bucket matrix — loading pre-computed CIs from jsd_permutation.rds")
  perm_raw <- readRDS(PATHS$jsd_permutation)
  perm_data <- perm_raw %>%
    mutate(party = normalize_party(party)) %>%
    filter(!is.na(party), party %in% PARTIES_KEEP) %>%
    rename(lp = legislative_period) %>%
    # Pool CDU + CSU: weight-average CIs by sentence count
    group_by(party, lp) %>%
    summarise(
      jsd_lower_95 = weighted.mean(jsd_lower_95, n_speech_sentences, na.rm = TRUE),
      jsd_upper_95 = weighted.mean(jsd_upper_95, n_speech_sentences, na.rm = TRUE),
      n_sent_perm  = sum(n_speech_sentences),
      .groups = "drop"
    )
} else if (!has_matrix) {
  message("[H1a] No bucket matrix and no jsd_permutation.rds — H1a will show point estimates only.")
}

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# H1a — Salience JSD with bootstrap intervals
# ============================================================================
message("[H1a] Computing per-cell JSD ...")

cell_keys <- speech_A %>% distinct(party, lp)
h1a_rows  <- vector("list", nrow(cell_keys))

for (i in seq_len(nrow(cell_keys))) {
  pty  <- cell_keys$party[i]
  lp_i <- cell_keys$lp[i]
  ed   <- m1_map$election_date[m1_map$lp == lp_i]
  if (length(ed) == 0L) next

  # Manifesto distribution (M1, Aggregation A)
  mv_long <- manifesto_A %>% filter(party == pty, election_date == ed)
  if (nrow(mv_long) == 0L) {
    h1a_rows[[i]] <- tibble(party = pty, lp = lp_i, election_date = ed,
                            jsd = NA_real_, lo95 = NA_real_, hi95 = NA_real_,
                            n_sent = NA_integer_, manifesto_present = FALSE)
    next
  }
  mv <- setNames(mv_long$share, mv_long$bucket)[BUCKETS_A]
  mv[is.na(mv)] <- 0
  if (sum(mv) > 0) mv <- mv / sum(mv)

  # Speech distribution (point estimate)
  sv_long <- speech_A %>% filter(party == pty, lp == lp_i)
  sv <- setNames(sv_long$share, sv_long$bucket)[BUCKETS_A]
  sv[is.na(sv)] <- 0
  n_sent_cell <- if (nrow(sv_long) > 0) sv_long$n_sent[1] else NA_integer_
  jsd_point   <- jsd(sv, mv)

  # Bootstrap CIs
  if (has_matrix) {
    rows <- cell_meta %>% filter(party == pty, lp == lp_i) %>% pull(.row)
    bm <- bucket_mat[rows, BUCKETS_A, drop = FALSE]
    bs <- bootstrap_jsd_buckets(bm, mv, BUCKETS_A,
                                n_boot = BOOTSTRAP_DRAWS,
                                seed   = BOOTSTRAP_SEED + i)
    lo95   <- bs$lo95
    hi95   <- bs$hi95
    n_sent_cell <- bs$n_sent
    jsd_point   <- bs$point
  } else if (!is.null(perm_data)) {
    pm_row <- perm_data %>% filter(party == pty, lp == lp_i)
    lo95 <- if (nrow(pm_row) > 0) pm_row$jsd_lower_95[1] else NA_real_
    hi95 <- if (nrow(pm_row) > 0) pm_row$jsd_upper_95[1] else NA_real_
    if (!is.na(n_sent_cell) && n_sent_cell == 0 && nrow(pm_row) > 0)
      n_sent_cell <- pm_row$n_sent_perm[1]
  } else {
    lo95 <- NA_real_
    hi95 <- NA_real_
  }

  # Multinomial bootstrap fallback: large cells with no CI source
  # (e.g. Linke LP 13-15 — absent from jsd_permutation.rds because
  # compute_jsd.R excluded PDS before the bootstrap step).
  if (is.na(lo95) && !is.na(n_sent_cell) && n_sent_cell >= N_SENT_MIN && sum(sv) > 0) {
    sv_prob <- sv / sum(sv)
    set.seed(BOOTSTRAP_SEED + i)
    reps <- numeric(BOOTSTRAP_DRAWS)
    for (b in seq_len(BOOTSTRAP_DRAWS)) {
      boot_sv <- as.numeric(rmultinom(1L, n_sent_cell, prob = sv_prob)) / n_sent_cell
      reps[b] <- jsd(boot_sv, mv)
    }
    ci    <- quantile(reps, c(0.025, 0.975), na.rm = TRUE)
    lo95  <- unname(ci[1])
    hi95  <- unname(ci[2])
  }

  h1a_rows[[i]] <- tibble(party = pty, lp = lp_i, election_date = ed,
                          jsd = jsd_point, lo95 = lo95, hi95 = hi95,
                          n_sent = n_sent_cell, manifesto_present = TRUE)
  message(sprintf("  %s | LP %d  JSD = %.4f  [%s, %s]  (n = %s)",
                  pty, lp_i, jsd_point,
                  if (!is.na(lo95)) sprintf("%.4f", lo95) else "NA",
                  if (!is.na(hi95)) sprintf("%.4f", hi95) else "NA",
                  format(n_sent_cell, big.mark = ",")))
}

h1a_tbl <- bind_rows(h1a_rows) %>%
  mutate(party = factor(party, levels = PARTIES_KEEP))

# ---- Exclusion rules (consistent with compute_jsd.R) -----------------------
excluded_cells <- h1a_tbl %>%
  filter(
    (party == "FDP" & lp == 18L) |
    (!is.na(n_sent) & n_sent < N_SENT_MIN)
  ) %>%
  mutate(exclusion_reason = case_when(
    party == "FDP" & lp == 18L ~
      "FDP absent from Bundestag LP18 (missed 5% threshold 2013); n_sent are mis-attributions",
    !is.na(n_sent) & n_sent < N_SENT_MIN ~
      paste0("n_sent (", n_sent, ") < N_SENT_MIN (", N_SENT_MIN, ")"),
    TRUE ~ NA_character_
  ))

if (nrow(excluded_cells) > 0) {
  message(sprintf("[H1a] Excluding %d cell(s): %s",
                  nrow(excluded_cells),
                  paste(paste0(excluded_cells$party, " LP", excluded_cells$lp), collapse = ", ")))
  write_csv(excluded_cells, file.path(PATHS$out_dir, "excluded_cells.csv"))
}

h1a_tbl <- h1a_tbl %>%
  filter(!(party == "FDP" & lp == 18L)) %>%
  filter(is.na(n_sent) | n_sent >= N_SENT_MIN)

write_csv(h1a_tbl, file.path(PATHS$out_dir, "H1a_jsd_table.csv"))

# ---- H1a PLOT --------------------------------------------------------------
ci_available <- any(!is.na(h1a_tbl$lo95))

p_h1a <- h1a_tbl %>%
  filter(!is.na(jsd)) %>%
  ggplot(aes(x = lp, y = jsd, colour = party, group = party)) +
  geom_line(linewidth = 0.6, alpha = 0.5) +
  { if (ci_available)
      geom_errorbar(aes(ymin = lo95, ymax = hi95), width = 0.18, linewidth = 0.4)
    else
      list() } +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = PARTY_COLOURS, drop = FALSE) +
  facet_wrap(~ party, ncol = 3, drop = FALSE) +
  labs(
    title    = "H1a — Manifesto–speech salience divergence (Aggregation A)",
    subtitle = if (ci_available)
      "Per-cell JSD with 95% bootstrap intervals (1000 draws)"
    else
      "Per-cell JSD (point estimates; run with BUILD_BUCKET_MATRIX = TRUE for bootstrap CIs)",
    x        = "Legislative period",
    y        = "Jensen–Shannon Divergence",
    caption  = paste0(
      "M1 mapping: speeches in LP X vs. manifesto from the election that produced LP X.\n",
      if (!ci_available && !is.null(perm_data)) "CIs from jsd_permutation.rds (pre-computed).\n"
      else if (!ci_available) "Set BUILD_BUCKET_MATRIX <- TRUE in 03_load_and_aggregate.R for bootstrap CIs.\n"
      else "Native temperature τ = 1."
    )
  ) +
  theme_thesis() +
  theme(legend.position = "none")

ggsave(file.path(PATHS$fig_dir, "H1a_jsd_dotplot.pdf"),
       p_h1a, width = 9, height = 6.5)
ggsave(file.path(PATHS$fig_dir, "H1a_jsd_dotplot.png"),
       p_h1a, width = 9, height = 6.5, dpi = 200, bg = "white")

# ============================================================================
# H1b — Directional composition within B domains
# ============================================================================
message("\n[H1b] Building directional composition tables ...")

b_lookup <- AGG_B %>% distinct(domain_B, bucket_B)

# Speech side
speech_B_dir <- speech_B %>%
  filter(bucket %in% BUCKETS_B) %>%
  inner_join(b_lookup, by = c("bucket" = "bucket_B")) %>%
  rename(domain = domain_B, sub_bucket = bucket) %>%
  group_by(party, lp, domain) %>%
  mutate(share_within_domain = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  mutate(source = "Speech")

# Manifesto side — attach LP via M1 election date
manifesto_B_dir <- manifesto_B %>%
  filter(bucket %in% BUCKETS_B) %>%
  inner_join(b_lookup, by = c("bucket" = "bucket_B")) %>%
  rename(domain = domain_B, sub_bucket = bucket) %>%
  group_by(party, election_date, domain) %>%
  mutate(share_within_domain = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  inner_join(m1_map, by = "election_date") %>%
  mutate(source = "Manifesto") %>%
  select(party, lp, domain, sub_bucket, share_within_domain, source)

h1b_long <- bind_rows(
  speech_B_dir %>% select(party, lp, domain, sub_bucket, share_within_domain, source),
  manifesto_B_dir
) %>%
  mutate(party  = factor(party,  levels = PARTIES_KEEP),
         domain = factor(domain, levels = DOMAINS_B))

write_csv(h1b_long, file.path(PATHS$out_dir, "H1b_directional_table.csv"))

# ---- H1b summary: per-party per-LP JSD on B (excluding Andere) -------------
h1b_overall_jsd <- speech_B %>%
  filter(bucket != "Andere") %>%
  group_by(party, lp) %>%
  mutate(share_renorm = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  select(party, lp, bucket, share_renorm) %>%
  inner_join(
    manifesto_B %>%
      filter(bucket != "Andere") %>%
      group_by(party, election_date) %>%
      mutate(share_renorm_m = if (sum(share) > 0) share / sum(share) else 0) %>%
      ungroup() %>%
      inner_join(m1_map, by = "election_date") %>%
      select(party, lp, bucket, share_renorm_m),
    by = c("party", "lp", "bucket")
  ) %>%
  group_by(party, lp) %>%
  summarise(jsd_B = jsd(share_renorm, share_renorm_m), .groups = "drop") %>%
  mutate(party = factor(party, levels = PARTIES_KEEP))

write_csv(h1b_overall_jsd, file.path(PATHS$out_dir, "H1b_overall_jsd.csv"))

# ---- H1b PLOT --------------------------------------------------------------
SUB_COLOURS <- c(
  "Marktliberalismus"      = "#F4A261",
  "Staatsintervention"     = "#264653",
  "Wirtschaft Allgemein"   = "#A8C0CC",
  "Sozialstaat Ausbau"     = "#2A9D8F",
  "Sozialstaat Begrenzung" = "#E76F51",
  "Migration restriktiv"   = "#6D597A",
  "Migration liberal"      = "#B5838D",
  "Pro-EU"                 = "#003399",
  "Contra-EU"              = "#FFCC00"
)

h1b_pooled <- h1b_long %>%
  group_by(party, source, domain, sub_bucket) %>%
  summarise(share_within_domain = mean(share_within_domain), .groups = "drop") %>%
  mutate(sub_bucket = factor(sub_bucket, levels = names(SUB_COLOURS)),
         source     = factor(source, levels = c("Manifesto", "Speech")))

# Per-party mean overall JSD on B → strip labels (spec §3.4 / MIv2 §6.2).
party_jsd_strip <- h1b_overall_jsd %>%
  group_by(party) %>%
  summarise(mean_jsd_B = mean(jsd_B, na.rm = TRUE), .groups = "drop")
strip_labels <- setNames(
  sprintf("%s  (mean JSD = %.3f)",
          as.character(party_jsd_strip$party), party_jsd_strip$mean_jsd_B),
  as.character(party_jsd_strip$party)
)

p_h1b <- h1b_pooled %>%
  ggplot(aes(x = source, y = share_within_domain, fill = sub_bucket)) +
  geom_col(width = 0.7) +
  # Thin vertical separator between Manifesto (x=1) and Speech (x=2) bars
  # — the only between-bar position in a 2-bar panel; spec §3.4 describes
  # separators "dividing topic groups within each panel".
  geom_vline(xintercept = 1.5, colour = "grey85", linewidth = 0.3) +
  facet_grid(rows = vars(party), cols = vars(domain), switch = "y",
             labeller = labeller(party = strip_labels)) +
  scale_fill_manual(values = SUB_COLOURS, name = NULL,
                    guide = guide_legend(nrow = 3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "H1b — Directional composition: manifesto (M) vs. speech (S)",
    subtitle = "Within-domain share of directional sub-labels, averaged across LP 13–20",
    x = NULL, y = NULL,
    caption  = paste0(
      "Within each panel cell, manifesto and speech bars sum to 100% of mass within the domain (Andere excluded).\n",
      "Row strip text shows each party's mean overall JSD on Aggregation B across LPs (M1 mapping).\n",
      "M1 mapping. Colours grouped by domain. Thin vertical separators divide manifesto/speech bars."
    )
  ) +
  theme_thesis() +
  theme(panel.spacing.x      = unit(0.7, "lines"),
        strip.text.y.left    = element_text(angle = 0, face = "bold"),
        axis.text.x          = element_text(size = 9))

ggsave(file.path(PATHS$fig_dir, "H1b_directional_bars.pdf"),
       p_h1b, width = 11, height = 9)
ggsave(file.path(PATHS$fig_dir, "H1b_directional_bars.png"),
       p_h1b, width = 11, height = 9, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n==================== H1a SUMMARY ====================\n")
print(h1a_tbl %>%
        filter(!is.na(jsd)) %>%
        group_by(party) %>%
        summarise(mean_jsd   = mean(jsd),
                  median_jsd = median(jsd),
                  n_cells    = n(), .groups = "drop"))

cat("\n==================== H1b OVERALL JSD ON B ====================\n")
print(h1b_overall_jsd %>%
        group_by(party) %>%
        summarise(mean_jsd_B = mean(jsd_B, na.rm = TRUE),
                  n_cells    = n(), .groups = "drop"))

message("\n[H1] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H1a_jsd_table.csv"))
message("  - ", file.path(PATHS$fig_dir, "H1a_jsd_dotplot.pdf"))
message("  - ", file.path(PATHS$out_dir, "H1b_directional_table.csv"))
message("  - ", file.path(PATHS$out_dir, "H1b_overall_jsd.csv"))
message("  - ", file.path(PATHS$fig_dir, "H1b_directional_bars.pdf"))
