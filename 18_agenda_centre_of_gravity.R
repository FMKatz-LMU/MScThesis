# ============================================================================
# 18_agenda_centre_of_gravity.R — Where does parliamentary speech's agenda sit?
# ----------------------------------------------------------------------------
# Exploratory extension of the nearest-manifesto benchmark (3.8 / 4.2 P3).
# The nearest-manifesto test asks whether a party's OWN manifesto is nearest to
# its speech agenda. This script asks the complementary question: across ALL
# party programmes, which one does the chamber's speech agenda collectively sit
# closest to? It computes the full speech x manifesto JSD matrix on the PRIMARY
# salience specification and summarises it two ways:
#
#   Panel A  Centre of gravity: mean JSD from every one of the 38 speech cells
#            to each party's manifesto in the same period (candidates restricted
#            to parties actually in parliament that period, as in the benchmark).
#            Lower = the programme whose topic profile the chamber most resembles.
#   Panel B  The mechanism: the grand-mean speech agenda (9 salience buckets)
#            against each party's mean manifesto agenda, showing WHY the FDP is
#            the centre of gravity (governance/economy-heavy, welfare/migration-
#            light) and WHY the Linke and AfD sit farthest (welfare/migration).
#
# NOTE (interpretation): this is a SALIENCE result (what is talked about), not a
# positional one. See 4.2 P3 / Chapter 5. Runs from the project ROOT; reuses
# 00_config.R (jsd, PARTY_COLOURS, theme_thesis, BUCKETS_A, PATHS, LP_ELECTION).
#
# Output (results/empirics/):
#   agenda_centre_of_gravity.csv
#   figures/agenda_centre_of_gravity.{png,pdf}
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })

# ---- primary distributions from the cached spec grid -----------------------
ss <- as_tibble(readRDS(file.path(PATHS$cache_dir, "speech_soft.rds")))
mm <- as_tibble(readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds")))

# PRIMARY speech: Aggregation A, full filter, native temperature, code 305 excl
speech <- ss %>%
  filter(scheme == "A", filter == FILTER_PRIMARY,
         tau_name == "native", code305 == CODE305_PRIMARY_H1A_H2A) %>%
  transmute(party = normalize_party(party), lp, bucket, share)

# PRIMARY manifesto: Aggregation A, M1_entering, code 305 excl; date -> LP
manif <- mm %>%
  filter(scheme == "A", mapping == "M1_entering",
         code305 == CODE305_PRIMARY_H1A_H2A) %>%
  mutate(election_date = as.Date(election_date)) %>%
  left_join(LP_ELECTION, by = "election_date") %>%
  transmute(party = normalize_party(party), lp, bucket, share)

# ---- helper: (party, lp) -> named 9-bucket vector in BUCKETS_A order --------
to_vec <- function(df) {
  df %>% group_by(party, lp) %>%
    summarise(vec = list({
      v <- setNames(share[match(BUCKETS_A, bucket)], BUCKETS_A)
      v[is.na(v)] <- 0; v
    }), .groups = "drop")
}
sp_v <- to_vec(speech) %>% rename(sp_party = party, sp_vec = vec)
mf_v <- to_vec(manif)  %>% rename(mf_party = party, mf_vec = vec)

# candidate manifestos = parties actually IN PARLIAMENT that LP (= have a speech
# cell), matching the nearest-manifesto benchmark's candidate rule.
present <- speech %>% distinct(party, lp)
cands   <- mf_v %>% semi_join(present, by = c("mf_party" = "party", "lp" = "lp"))

# ---- full within-LP speech x manifesto JSD matrix --------------------------
M <- sp_v %>%
  inner_join(cands, by = "lp", relationship = "many-to-many") %>%
  mutate(jsd = map2_dbl(sp_vec, mf_vec, jsd)) %>%
  select(sp_party, lp, mf_party, jsd)

# ---- Panel A data: centre of gravity ---------------------------------------
cg <- M %>% group_by(mf_party) %>%
  summarise(mean_jsd = mean(jsd), n = n(), .groups = "drop") %>%
  arrange(mean_jsd)
write_csv(cg, file.path(PATHS$out_dir, "agenda_centre_of_gravity.csv"))

# ---- Panel B data: grand speech agenda vs party manifesto agendas ----------
# grand speech agenda = mean bucket share over the 38 speech cells (equal weight)
speech_prof <- speech %>% group_by(bucket) %>%
  summarise(share = mean(share), .groups = "drop")
# party mean manifesto agenda over its in-parliament LPs
manif_prof <- manif %>% semi_join(present, by = c("party", "lp")) %>%
  group_by(party, bucket) %>% summarise(share = mean(share), .groups = "drop")

bk_order <- speech_prof %>% arrange(share) %>% pull(bucket)   # highest on top
show_parties <- c("FDP", "Linke", "AfD")   # closest + the two farthest; set to
                                            # PARTIES_KEEP to show all six.

# ---- plot ------------------------------------------------------------------
pA <- cg %>% mutate(mf_party = factor(mf_party, levels = rev(mf_party))) %>%
  ggplot(aes(mean_jsd, mf_party, colour = mf_party)) +
  geom_segment(aes(x = 0, xend = mean_jsd, yend = mf_party), linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.4f", mean_jsd)), hjust = -0.30,
            size = 3.1, colour = "grey20") +
  scale_colour_manual(values = PARTY_COLOURS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title = "A  Agenda centre of gravity",
       subtitle = "Mean JSD from all speech cells to each party's manifesto (lower = closer)",
       x = "mean JSD to manifesto", y = NULL) +
  theme_thesis()

pB <- ggplot() +
  geom_col(data = speech_prof %>% mutate(bucket = factor(bucket, levels = bk_order)),
           aes(100 * share, bucket), fill = "grey80", width = 0.66) +
  geom_point(data = manif_prof %>% filter(party %in% show_parties) %>%
               mutate(bucket = factor(bucket, levels = bk_order)),
             aes(100 * share, bucket, colour = party, shape = party),
             size = 2.7, stroke = 0.7) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  scale_shape_manual(values = c(FDP = 16, Linke = 15, AfD = 18), name = NULL) +
  labs(title = "B  Why: the shared agenda profile",
       subtitle = "Bars = parliamentary speech (all cells); points = party manifesto agendas",
       x = "share of agenda (%)", y = NULL) +
  theme_thesis()

p <- pA + pB + plot_layout(widths = c(1, 1.25)) +
  plot_annotation(
    title = "The parliamentary agenda leans toward the FDP's programmatic topic profile",
    subtitle = "Salience layer (Aggregation A); primary specification: full filter, code-305 mass subtracted, native temperature",
    caption = "Centre of gravity = mean JSD from each of the 38 speech cells to a party's manifesto in the same period; manifesto candidates restricted to parties in parliament that period. Salience, not position.",
    theme = theme_thesis())

ggsave(file.path(PATHS$fig_dir, "agenda_centre_of_gravity.png"),
       p, width = 11, height = 5.4, dpi = 200, bg = "white")
ggsave(file.path(PATHS$fig_dir, "agenda_centre_of_gravity.pdf"),
       p, width = 11, height = 5.4, device = cairo_pdf)

cat("\nCentre of gravity (mean JSD from all speeches to each manifesto):\n")
print(as.data.frame(cg), row.names = FALSE, digits = 4)
cat("\nFigures written to", PATHS$fig_dir, "\n")
