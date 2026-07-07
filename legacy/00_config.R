# ============================================================================
# 00_config.R — Project configuration, paths, lookup tables
# ----------------------------------------------------------------------------
# Sourced by every other script. PROJECT_ROOT is the only thing to edit.
# Everything below the PATHS block is methodology — change only if your
# aggregation scheme changes.
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(glue)
  library(fs)
})

# ---- PATHS -----------------------------------------------------------------
# This machine's project root. All data + output paths derive from it.
# Portable alternative (survives drive/machine moves automatically, as long as
# Data/ and results/ sit inside the .Rproj folder):  PROJECT_ROOT <- here::here()
PROJECT_ROOT <- "C:/RProj_MSc/MScThesis"

PATHS <- list(
  # LEGACY (retired side-scripts apply_aggregation_AB.R / compute_jsd.R) —
  # no longer read by the v2 pipeline; kept so older scripts don't error.
  agg_A_speech    = file.path(PROJECT_ROOT, "Data/agg_A_speech.rds"),
  agg_B_speech    = file.path(PROJECT_ROOT, "Data/agg_B_speech.rds"),
  agg_A_manifesto = file.path(PROJECT_ROOT, "Data/agg_A_manifesto.rds"),
  agg_B_manifesto = file.path(PROJECT_ROOT, "Data/agg_B_manifesto.rds"),
  jsd_permutation = file.path(PROJECT_ROOT, "Data/jsd_permutation.rds"),
  # v2 classified parquet (SoMaJo re-segmented + procedural-flagged) — the
  # PRIMARY corpus streamed by 03_load_and_aggregate.R.
  parquet_dir     = file.path(PROJECT_ROOT, "Data/sentences_classified_v2"),
  # Output directories
  out_dir         = file.path(PROJECT_ROOT, "results/empirics"),
  fig_dir         = file.path(PROJECT_ROOT, "results/empirics/figures"),
  cache_dir       = file.path(PROJECT_ROOT, "results/empirics/cache")
)
invisible(lapply(PATHS[c("out_dir", "fig_dir", "cache_dir")], dir_create))

# ---- 000-Korpus-Override (exact-only Prozedural-Reflag, 2026-06) ------------
# reflag_procedural_exact_only.py hat das exact-only is_procedural in einen
# SCHWESTER-Ordner geschrieben (nicht in-place). 03 und 11 lesen
# .PARQUET_DIR_000_OVERRIDE falls gesetzt, sonst paste0(PATHS$parquet_dir, "_000").
# Diese Zeile auskommentieren = zurück zum Zwei-Layer-Korpus (exact+rule).
.PARQUET_DIR_000_OVERRIDE <- file.path(PROJECT_ROOT, "Data/sentences_classified_v2_000_exactonly")

# ---- ANALYTICAL SCOPE ------------------------------------------------------
LEGISLATIVE_PERIODS <- 13:20
PARTIES_KEEP <- c("CDU/CSU", "SPD", "FDP", "Grüne", "Linke", "AfD")
MANIFESTO_MODE_PRIMARY <- "M1"

# Bootstrap settings for H1a intervals
BOOTSTRAP_DRAWS <- 1000L
BOOTSTRAP_SEED  <- 20260507L

# Minimum speech sentences to include a (party, LP) cell (consistent with compute_jsd.R)
N_SENT_MIN <- 1000L

# ============================================================================
# SPEECH-SIDE SPECIFICATION AXES  (clean-pipeline sprint, 1d)
# ----------------------------------------------------------------------------
# Every speech (party, LP) cell is computed under the FULL CROSS of three axes:
#
#   filter  ∈ {unfiltered, filtered, filtered_000}   sentence-removal filter
#   tau     ∈ {sharp(0.5), native(1.0), flat(2.0)}   softmax temperature
#   code305 ∈ {incl, excl}                     code-305-only exclusion
#
# = 3 × 3 × 2 = 18 specs per cell, per scheme (A, B). The MANIFESTO side carries
# only code305 (no procedural sentences, no softmax temperature) = 2 specs.
#
# PRIMARY specifications are declared ONCE here. Every downstream script MUST
# read these constants and never hard-code a spec string.
#
#   filter  : "filtered_000"  — PRIMARY. Procedural floor language AND non-
#             codeable (null-class) sentences removed. Restores symmetry with
#             the manifesto side, which is already 000-free. Comparison arms:
#             "filtered" (procedural only) and "unfiltered" (nothing removed);
#             the ladder unfiltered → filtered → filtered_000 isolates the
#             marginal effect of each filter.
#   tau     : "native"    (= TAU_PRIMARY, defined further below).
#   code305 : "excl" for the salience hypotheses H1a/H2a — PRIMARY. The
#             filtered_000 null-class filter removes MOST but not ALL of the 305
#             inflation: the text-based 000-classifier (19/20) has recall < 1, so
#             residual non-codeables still leak into 305, and genuine procedural
#             floor language (Political Authority) remains. The surgical 305-only
#             exclusion is therefore the PRIMARY correction ON TOP of filtered_000,
#             not a robustness arm. Corroborated by 11 PART 2 (residual 305 excess
#             persists on filtered_000) and PART 3 (low gold-305 precision). "incl"
#             on filtered_000 = the filter-only robustness arm. For the directional
#             layer (scheme B: H1b/H2b/H2c/H3) code 305 is residual ("Andere") and
#             is dropped in within-domain renorm, so incl/excl is immaterial there
#             -> CODE305_PRIMARY_DEFAULT stays "incl".
# ----------------------------------------------------------------------------
FILTERS          <- c("unfiltered", "filtered", "filtered_000")
FILTER_PRIMARY   <- "filtered_000"

# 000-filter (null-class removal). ManifestoBERTa has no MARPOR 000 class, so it
# forces every non-codeable speech sentence into one of the 56 policy codes
# (disproportionately code 305). The manifesto side is already 000-free (MARPOR
# drops uncoded quasi-sentences, then renormalises). filtered_000 restores that
# symmetry: a sentence is dropped when is_procedural OR is_000 is TRUE (union).
# p_000/is_000 are written per sentence by 20_apply_000_text.py. 03 re-derives
# the boolean from the stored p_000 at this threshold, so the cutoff is an
# R-side knob (no GPU re-run needed to change it). 0.50 = validated operating
# point (CV/holdout: deflation ≈ oracle, flag rate ≈ true null-class rate).
FILTER000_THRESHOLD <- 0.50          # is_000 := p_000 >= FILTER000_THRESHOLD
COL_P000            <- "p_000"
COL_IS000           <- "is_000"

CODE305_VARIANTS <- c("incl", "excl")
CODE305_PRIMARY_H1A_H2A <- "excl"   # PRIMARY. filtered_000 removes the 305
                                    # inflation at source but not fully (000-
                                    # classifier recall < 1 + genuine procedural
                                    # 305 remains), so the surgical 305-only
                                    # exclusion sits ON TOP of filtered_000.
                                    # Pre-committed via 10 (excl removes a non-
                                    # trivial residual DPS share) + 11 PART 2/3.
CODE305_PRIMARY_DEFAULT <- "incl"   # all other hypotheses

# The MARPOR code the 305-only exclusion removes, and the A/B bucket carrying it
# (03 subtracts its mass before renormalising the cell).
CODE_305        <- 305L
BUCKET_OF_305_A <- "Democracy & Political System"   # 305 lives here under A
BUCKET_OF_305_B <- "Andere"                         # 305 is residual under B

# Bootstrap CI bands: filter × code305 at native tau, scheme A only.
# PRIMARY band reported in the thesis:
BOOTSTRAP_PRIMARY_FILTER  <- FILTER_PRIMARY          # "filtered_000"
BOOTSTRAP_PRIMARY_CODE305 <- CODE305_PRIMARY_H1A_H2A # "excl"
# 6 bands total now (3 filters × 2 code305 at native τ, scheme A). The other
# five are appendix-only: they show neither the procedural/null-class filtering
# nor the 305 exclusion inflate the interval.

# ---- THE 56 MARPOR HANDBOOK 4 CODES ----------------------------------------
MARPOR_CODES_56 <- c(
  101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
  201, 202, 203, 204,
  301, 302, 303, 304, 305,
  401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416,
  501, 502, 503, 504, 505, 506, 507,
  601, 602, 603, 604, 605, 606, 607, 608,
  701, 702, 703, 704, 705, 706
)
stopifnot(length(MARPOR_CODES_56) == 56)

# ============================================================================
# AGGREGATION A — 9 buckets, exact partition of 56 codes
# ============================================================================
AGG_A <- tribble(
  ~code, ~bucket_A,
  # Foreign Policy & Defence
  101, "Foreign Policy & Defence", 102, "Foreign Policy & Defence",
  103, "Foreign Policy & Defence", 104, "Foreign Policy & Defence",
  105, "Foreign Policy & Defence", 106, "Foreign Policy & Defence",
  107, "Foreign Policy & Defence", 109, "Foreign Policy & Defence",
  # European Integration
  108, "European Integration", 110, "European Integration",
  # Democracy & Political System
  201, "Democracy & Political System", 202, "Democracy & Political System",
  203, "Democracy & Political System", 204, "Democracy & Political System",
  301, "Democracy & Political System", 302, "Democracy & Political System",
  303, "Democracy & Political System", 304, "Democracy & Political System",
  305, "Democracy & Political System",
  # Economy
  401, "Economy", 402, "Economy", 403, "Economy", 404, "Economy",
  405, "Economy", 406, "Economy", 407, "Economy", 408, "Economy",
  409, "Economy", 410, "Economy", 411, "Economy", 412, "Economy",
  413, "Economy", 414, "Economy", 415, "Economy",
  # Environment
  416, "Environment", 501, "Environment",
  # Welfare & Social Policy
  502, "Welfare & Social Policy", 503, "Welfare & Social Policy",
  504, "Welfare & Social Policy", 505, "Welfare & Social Policy",
  506, "Welfare & Social Policy", 507, "Welfare & Social Policy",
  # Law & Order and National Identity
  603, "Law & Order and National Identity", 604, "Law & Order and National Identity",
  605, "Law & Order and National Identity", 606, "Law & Order and National Identity",
  # Migration
  601, "Migration", 602, "Migration", 607, "Migration", 608, "Migration",
  # Social Groups
  701, "Social Groups", 702, "Social Groups", 703, "Social Groups",
  704, "Social Groups", 705, "Social Groups", 706, "Social Groups"
)

BUCKETS_A <- c(
  "Foreign Policy & Defence", "European Integration",
  "Democracy & Political System", "Economy", "Welfare & Social Policy",
  "Environment", "Law & Order and National Identity", "Migration", "Social Groups"
)

# ============================================================================
# AGGREGATION B — directional within four key domains
# ============================================================================
AGG_B <- tribble(
  ~code, ~domain_B,   ~bucket_B,
  401, "Economy",     "Marktliberalismus",
  402, "Economy",     "Marktliberalismus",
  407, "Economy",     "Marktliberalismus",
  414, "Economy",     "Marktliberalismus",
  403, "Economy",     "Staatsintervention",
  404, "Economy",     "Staatsintervention",
  405, "Economy",     "Staatsintervention",
  409, "Economy",     "Staatsintervention",
  412, "Economy",     "Staatsintervention",
  413, "Economy",     "Staatsintervention",
  408, "Economy",     "Wirtschaft Allgemein",
  410, "Economy",     "Wirtschaft Allgemein",
  411, "Economy",     "Wirtschaft Allgemein",
  503, "Welfare",     "Sozialstaat Ausbau",
  504, "Welfare",     "Sozialstaat Ausbau",
  506, "Welfare",     "Sozialstaat Ausbau",
  505, "Welfare",     "Sozialstaat Begrenzung",
  507, "Welfare",     "Sozialstaat Begrenzung",
  601, "Migration",   "Migration restriktiv",
  608, "Migration",   "Migration restriktiv",
  602, "Migration",   "Migration liberal",
  607, "Migration",   "Migration liberal",
  108, "Europe",      "Pro-EU",
  110, "Europe",      "Contra-EU"
)

DOMAINS_B <- c("Economy", "Welfare", "Migration", "Europe")
BUCKETS_B <- c(
  "Marktliberalismus", "Staatsintervention", "Wirtschaft Allgemein",
  "Sozialstaat Ausbau", "Sozialstaat Begrenzung",
  "Migration restriktiv", "Migration liberal", "Pro-EU", "Contra-EU"
)

# ---- PARTY COLOURS ---------------------------------------------------------
PARTY_COLOURS <- c(
  "CDU/CSU" = "#000000", "SPD"   = "#E3000F", "FDP"   = "#FFCC00",
  "Grüne"   = "#1AA037", "Linke" = "#BE3075", "AfD"   = "#0489DB"
)

# ---- DEFAULT GGPLOT THEME --------------------------------------------------
theme_thesis <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(colour = "grey30"),
      plot.caption     = element_text(colour = "grey40", size = base_size - 2, hjust = 0),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom"
    )
}

# ============================================================================
# ROBUSTNESS-CHECK CONSTANTS  (MIv2 §3.3, §3.5, §6.4)
# ============================================================================

# Temperature sweep for the speech-side softmax (MIv2 §3.3).
# tau < 1 sharpens, tau > 1 flattens. tau = 1 is the model's native output.
TEMPERATURES <- c(sharp = 0.5, native = 1.0, flat = 2.0)
TAU_PRIMARY  <- 1.0

# Argmax-with-threshold robustness (MIv2 §3.5). Sentences whose top-1
# probability is below CONF_THRESHOLD are routed to a residual 'Andere'
# bucket; otherwise the argmax bucket gets the unit mass.
CONF_THRESHOLDS <- c(low = 0.4, high = 0.5)

# Tightened pre-filter robustness for H2a (MIv2 §6.4).
# Note on data-side limitations (decided 2026-05-08, see project memory):
#   * speaker_role is NOT a column in the parquet — Script_Speechrefinement.R
#     already filters speaker_role ∈ {mp, government} upstream during corpus
#     extraction. Presidium/president speech is therefore already excluded
#     before the parquet is written. drop_speakers_role is kept here for
#     documentation but is a no-op.
#   * n_words is NOT a column either; sentence length is computed from the
#     `sentence` text via lengths(strsplit(sentence, "\\s+")).
#   * Speeches are already filtered to >= 100 words upstream, so the
#     per-speech word filter would only marginally trim further. We leave
#     min_speech_words documented but disable it via drop_short_speeches=FALSE
#     to keep the filter strictly sentence-level (spec §3.1.3 fallback).
TIGHT_FILTER <- list(
  min_words_per_sentence = 8L,
  drop_speakers_role     = c("presidium", "vice-president", "president"),  # no-op (column missing)
  drop_short_speeches    = FALSE,                                          # disabled, see comment
  min_speech_words       = 50L
)


# ============================================================================
# OWNERSHIP PRIORS — REMOVED. The issue-ownership hypothesis (old H3a/H3b) was
# cut from the design; OWNERSHIP_A/OWNERSHIP_B and the old 06 script that used
# them are gone. Nothing in 02/03 or 04-11 references them.
# ============================================================================
# WEIGHT LOOKUPS  (used by H2c in 05 and by H3 in 06_H3.R)
# ----------------------------------------------------------------------------
# Source: Bundeswahlleiterin (official federal election results) and
# Deutscher Bundestag (official seat distributions per LP, after corrections).
# Numbers as percentages at the START of the LP (before any defections).
# ============================================================================

VOTE_SHARES <- tribble(
  ~election_date,           ~party,      ~vote_share,
  # 1994 federal election
  as.Date("1994-10-16"),    "CDU/CSU",   41.5,
  as.Date("1994-10-16"),    "SPD",       36.4,
  as.Date("1994-10-16"),    "FDP",       6.9,
  as.Date("1994-10-16"),    "Grüne",     7.3,
  as.Date("1994-10-16"),    "Linke",     4.4,    # PDS
  as.Date("1994-10-16"),    "AfD",       0,
  # 1998
  as.Date("1998-09-27"),    "CDU/CSU",   35.1,
  as.Date("1998-09-27"),    "SPD",       40.9,
  as.Date("1998-09-27"),    "FDP",       6.2,
  as.Date("1998-09-27"),    "Grüne",     6.7,
  as.Date("1998-09-27"),    "Linke",     5.1,    # PDS
  as.Date("1998-09-27"),    "AfD",       0,
  # 2002
  as.Date("2002-09-22"),    "CDU/CSU",   38.5,
  as.Date("2002-09-22"),    "SPD",       38.5,
  as.Date("2002-09-22"),    "FDP",       7.4,
  as.Date("2002-09-22"),    "Grüne",     8.6,
  as.Date("2002-09-22"),    "Linke",     4.0,    # PDS, below 5%
  as.Date("2002-09-22"),    "AfD",       0,
  # 2005
  as.Date("2005-09-18"),    "CDU/CSU",   35.2,
  as.Date("2005-09-18"),    "SPD",       34.2,
  as.Date("2005-09-18"),    "FDP",       9.8,
  as.Date("2005-09-18"),    "Grüne",     8.1,
  as.Date("2005-09-18"),    "Linke",     8.7,    # Linkspartei.PDS
  as.Date("2005-09-18"),    "AfD",       0,
  # 2009
  as.Date("2009-09-27"),    "CDU/CSU",   33.8,
  as.Date("2009-09-27"),    "SPD",       23.0,
  as.Date("2009-09-27"),    "FDP",       14.6,
  as.Date("2009-09-27"),    "Grüne",     10.7,
  as.Date("2009-09-27"),    "Linke",     11.9,
  as.Date("2009-09-27"),    "AfD",       0,
  # 2013
  as.Date("2013-09-22"),    "CDU/CSU",   41.5,
  as.Date("2013-09-22"),    "SPD",       25.7,
  as.Date("2013-09-22"),    "FDP",       4.8,    # below 5%-Hürde
  as.Date("2013-09-22"),    "Grüne",     8.4,
  as.Date("2013-09-22"),    "Linke",     8.6,
  as.Date("2013-09-22"),    "AfD",       4.7,    # below 5%-Hürde, manifesto exists
  # 2017
  as.Date("2017-09-24"),    "CDU/CSU",   32.9,
  as.Date("2017-09-24"),    "SPD",       20.5,
  as.Date("2017-09-24"),    "FDP",       10.7,
  as.Date("2017-09-24"),    "Grüne",     8.9,
  as.Date("2017-09-24"),    "Linke",     9.2,
  as.Date("2017-09-24"),    "AfD",       12.6,
  # 2021
  as.Date("2021-09-26"),    "CDU/CSU",   24.1,
  as.Date("2021-09-26"),    "SPD",       25.7,
  as.Date("2021-09-26"),    "FDP",       11.5,
  as.Date("2021-09-26"),    "Grüne",     14.8,
  as.Date("2021-09-26"),    "Linke",     4.9,    # below 5% but Grundmandate
  as.Date("2021-09-26"),    "AfD",       10.3
)

SEAT_SHARES <- tribble(
  ~lp, ~party,     ~seat_share,
  # LP 13 (1994–1998): 672 seats total
  13, "CDU/CSU",   43.8,
  13, "SPD",       37.5,
  13, "FDP",       7.0,
  13, "Grüne",     7.3,
  13, "Linke",     4.4,    # PDS, 30 seats
  13, "AfD",       0,
  # LP 14 (1998–2002): 669 seats
  14, "CDU/CSU",   36.6,
  14, "SPD",       44.5,
  14, "FDP",       6.3,
  14, "Grüne",     7.0,
  14, "Linke",     5.4,    # PDS, 36 seats
  14, "AfD",       0,
  # LP 15 (2002–2005): 603 seats — PDS only had 2 direct mandates
  15, "CDU/CSU",   41.1,
  15, "SPD",       41.6,
  15, "FDP",       7.8,
  15, "Grüne",     9.1,
  15, "Linke",     0.3,    # PDS 2 Direktmandate, no fraction status
  15, "AfD",       0,
  # LP 16 (2005–2009): 614 seats
  16, "CDU/CSU",   36.8,
  16, "SPD",       36.2,
  16, "FDP",       9.9,
  16, "Grüne",     8.3,
  16, "Linke",     8.8,
  16, "AfD",       0,
  # LP 17 (2009–2013): 622 seats
  17, "CDU/CSU",   38.4,
  17, "SPD",       23.5,
  17, "FDP",       15.0,
  17, "Grüne",     10.9,
  17, "Linke",     12.2,
  17, "AfD",       0,
  # LP 18 (2013–2017): 631 seats — FDP, AfD out
  18, "CDU/CSU",   49.3,
  18, "SPD",       30.6,
  18, "FDP",       0,
  18, "Grüne",     10.0,
  18, "Linke",     10.1,
  18, "AfD",       0,
  # LP 19 (2017–2021): 709 seats
  19, "CDU/CSU",   34.7,
  19, "SPD",       21.6,
  19, "FDP",       11.3,
  19, "Grüne",     9.4,
  19, "Linke",     9.7,
  19, "AfD",       13.3,
  # LP 20 (2021–2025): 736 seats
  20, "CDU/CSU",   26.6,
  20, "SPD",       28.0,
  20, "FDP",       12.5,
  20, "Grüne",     16.0,
  20, "Linke",     5.3,
  20, "AfD",       11.7
)

stopifnot(all(VOTE_SHARES$party %in% PARTIES_KEEP))
stopifnot(all(SEAT_SHARES$party %in% PARTIES_KEEP))
.vs_check <- VOTE_SHARES %>% group_by(election_date) %>%
  summarise(total = sum(vote_share), .groups = "drop")
stopifnot(all(.vs_check$total <= 100.5))
.ss_check <- SEAT_SHARES %>% group_by(lp) %>%
  summarise(total = sum(seat_share), .groups = "drop")
stopifnot(all(.ss_check$total <= 100.5))
rm(.vs_check, .ss_check)

message("[00_config] Configuration loaded. ",
        length(BUCKETS_A), " A-buckets; ",
        length(BUCKETS_B), " B-buckets across ",
        length(DOMAINS_B), " domains; ",
        length(TEMPERATURES), " τ values.")
