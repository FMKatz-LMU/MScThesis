# ============================================================================
# 04_H1.R — Hypotheses 1a (salience baseline) and 1b (directional baseline)
# ----------------------------------------------------------------------------
# H1a: Per (party, LP) JSD between manifesto and speech under Aggregation A,
#      M1 mapping, native temperature τ = 1, with 95% bootstrap intervals
#      (1000 draws, sentence-level resampling on speech side).
#
# H1b: Within the four B domains, paired stacked bar charts of directional
#      sub-label composition (manifesto vs. speech) per party, all LPs pooled
#      into the panel grid.
#
# Output:
#   results/empirics/H1a_jsd_table.csv
#   results/empirics/figures/H1a_jsd_dotplot.pdf
#   results/empirics/H1b_directional_table.csv
#   results/empirics/figures/H1b_directional_bars.pdf
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

# ---- Load caches -----------------------------------------------------------
sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))
mat_list <- readRDS(file.path(PATHS$cache_dir, "speech_prob_matrix.rds"))

speech_A    <- sd_list$A
speech_B    <- sd_list$B
manifesto_A <- md_list$A
manifesto_B <- md_list$B
prob_mat    <- mat_list$prob_mat
cells       <- mat_list$cells

# ---- Build M1 mapping: each (party, LP) -> (party, election_date) ----------
m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# ============================================================================
# H1a — Salience JSD with bootstrap intervals
# ============================================================================
message("[H1a] Computing per-cell JSD with bootstrap …")

cell_keys <- cells %>% distinct(party, lp)

h1a_rows <- vector("list", nrow(cell_keys))

for (i in seq_len(nrow(cell_keys))) {
  pty  <- cell_keys$party[i]
  lp_i <- cell_keys$lp[i]
  ed   <- m1_map$election_date[m1_map$lp == lp_i]
  if (length(ed) == 0) next

  # Speech matrix for this cell
  rows <- cells %>% filter(party == pty, lp == lp_i) %>% pull(.row)
  pm <- prob_mat[rows, , drop = FALSE]

  # Manifesto vector for the M1 election (Aggregation A)
  mv_long <- manifesto_A %>%
    filter(party == pty, election_date == ed)
  if (nrow(mv_long) == 0L) {
    h1a_rows[[i]] <- tibble(party = pty, lp = lp_i, election_date = ed,
                            jsd = NA_real_, lo95 = NA_real_, hi95 = NA_real_,
                            n_sent = nrow(pm), manifesto_present = FALSE)
    next
  }
  mv <- setNames(mv_long$share, mv_long$bucket)[BUCKETS_A]
  mv[is.na(mv)] <- 0
  if (sum(mv) > 0) mv <- mv / sum(mv)  # normalize

  bs <- bootstrap_jsd(pm, mv, AGG_A, "bucket_A", BUCKETS_A,
                      n_boot = BOOTSTRAP_DRAWS, seed = BOOTSTRAP_SEED + i)

  h1a_rows[[i]] <- tibble(party = pty, lp = lp_i, election_date = ed,
                          jsd = bs$point, lo95 = bs$lo95, hi95 = bs$hi95,
                          n_sent = bs$n_sent, manifesto_present = TRUE)
  message(sprintf("  %s | LP %d  →  JSD = %.4f  [%.4f, %.4f]  (n=%s)",
                  pty, lp_i, bs$point, bs$lo95, bs$hi95,
                  format(bs$n_sent, big.mark = ",")))
}

h1a_tbl <- bind_rows(h1a_rows) %>%
  mutate(party = factor(party, levels = PARTIES_KEEP))

write_csv(h1a_tbl, file.path(PATHS$out_dir, "H1a_jsd_table.csv"))

# ---- H1a PLOT --------------------------------------------------------------
p_h1a <- h1a_tbl %>%
  filter(!is.na(jsd)) %>%
  ggplot(aes(x = lp, y = jsd, colour = party, group = party)) +
  geom_line(linewidth = 0.6, alpha = 0.5) +
  geom_errorbar(aes(ymin = lo95, ymax = hi95), width = 0.18, linewidth = 0.4) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  scale_colour_manual(values = PARTY_COLOURS, drop = FALSE) +
  facet_wrap(~ party, ncol = 3, drop = FALSE) +
  labs(title    = "H1a — Manifesto–speech salience divergence (Aggregation A)",
       subtitle = "Per-cell JSD with 95% bootstrap intervals (1000 draws on sentence-level resamples)",
       x        = "Legislative period",
       y        = "Jensen–Shannon Divergence",
       caption  = "M1 mapping: speeches in LP X compared to the manifesto from the election that produced LP X.\nNative temperature τ = 1.") +
  theme_thesis() +
  theme(legend.position = "none")

ggsave(file.path(PATHS$fig_dir, "H1a_jsd_dotplot.pdf"),
       p_h1a, width = 9, height = 6.5)
ggsave(file.path(PATHS$fig_dir, "H1a_jsd_dotplot.png"),
       p_h1a, width = 9, height = 6.5, dpi = 200, bg = "white")

# ============================================================================
# H1b — Directional baseline within the four B domains
# ============================================================================
message("\n[H1b] Building directional composition tables …")

# Long table: per (party, lp, source, domain, sub_bucket, share)
# where share is the within-domain share among the directional sub-buckets
# (renormalized to 1 within each domain).

# 1) Speech side: take speech_B, drop "Andere", keep B buckets, attach domain
b_lookup <- AGG_B %>% distinct(domain_B, bucket_B)

speech_B_dir <- speech_B %>%
  filter(bucket %in% BUCKETS_B) %>%
  inner_join(b_lookup, by = c("bucket" = "bucket_B")) %>%
  rename(domain = domain_B, sub_bucket = bucket) %>%
  group_by(party, lp, domain) %>%
  mutate(share_within_domain = if (sum(share) > 0) share / sum(share) else 0) %>%
  ungroup() %>%
  mutate(source = "Speech")

# 2) Manifesto side: same operation on manifesto_B, then attach LP via M1
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
  mutate(party = factor(party, levels = PARTIES_KEEP),
         domain = factor(domain, levels = DOMAINS_B))

write_csv(h1b_long, file.path(PATHS$out_dir, "H1b_directional_table.csv"))

# ---- H1b SUMMARY: per-party, per-LP overall JSD on B (excluding Andere) ----
# This goes in the panel header.
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
# Colours for the 9 sub-buckets — domain-coherent so each domain reads as a
# small palette of related shades.
SUB_COLOURS <- c(
  "Marktliberalismus"     = "#F4A261",
  "Staatsintervention"    = "#264653",
  "Wirtschaft Allgemein"  = "#A8C0CC",
  "Sozialstaat Ausbau"    = "#2A9D8F",
  "Sozialstaat Begrenzung"= "#E76F51",
  "Migration restriktiv"  = "#6D597A",
  "Migration liberal"     = "#B5838D",
  "Pro-EU"                = "#003399",
  "Contra-EU"             = "#FFCC00"
)

# Pool LPs by averaging within-domain shares per (party, source, domain, sub_bucket).
# This matches the "all LPs pooled" framing in H1b. A per-LP version is also
# useful for the appendix; we save that too.
h1b_pooled <- h1b_long %>%
  group_by(party, source, domain, sub_bucket) %>%
  summarise(share_within_domain = mean(share_within_domain), .groups = "drop") %>%
  mutate(sub_bucket = factor(sub_bucket, levels = names(SUB_COLOURS)),
         source = factor(source, levels = c("Manifesto", "Speech")))

p_h1b <- h1b_pooled %>%
  ggplot(aes(x = source, y = share_within_domain, fill = sub_bucket)) +
  geom_col(width = 0.7) +
  facet_grid(rows = vars(party), cols = vars(domain), switch = "y") +
  scale_fill_manual(values = SUB_COLOURS, name = NULL,
                    guide = guide_legend(nrow = 3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title    = "H1b — Directional composition: manifesto (M) vs. speech (S)",
       subtitle = "Within-domain share of directional sub-labels, averaged across LP 13–20",
       x = NULL, y = NULL,
       caption  = "Within each panel cell, manifesto and speech bars sum to 100% of mass within the domain (Andere excluded).\nM1 mapping. Colours grouped by domain.") +
  theme_thesis() +
  theme(panel.spacing.x = unit(0.7, "lines"),
        strip.text.y.left = element_text(angle = 0, face = "bold"),
        axis.text.x = element_text(size = 9))

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
        summarise(mean_jsd = mean(jsd),
                  median_jsd = median(jsd),
                  n_cells = n(), .groups = "drop"))

cat("\n==================== H1b OVERALL JSD ON B ====================\n")
print(h1b_overall_jsd %>%
        group_by(party) %>%
        summarise(mean_jsd_B = mean(jsd_B, na.rm = TRUE),
                  n_cells = n(), .groups = "drop"))

message("\n[H1] Outputs:")
message("  - ", file.path(PATHS$out_dir, "H1a_jsd_table.csv"))
message("  - ", file.path(PATHS$fig_dir, "H1a_jsd_dotplot.pdf"))
message("  - ", file.path(PATHS$out_dir, "H1b_directional_table.csv"))
message("  - ", file.path(PATHS$out_dir, "H1b_overall_jsd.csv"))
message("  - ", file.path(PATHS$fig_dir, "H1b_directional_bars.pdf"))
