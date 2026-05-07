# ============================================================================
# 00_config.R — Project configuration, paths, lookup tables
# ----------------------------------------------------------------------------
# Sourced by every other script. Edit paths in the PATHS block to match your
# local layout. Everything below the PATHS block is methodology — change only
# if your aggregation scheme changes.
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)         # parquet, if you ever switch from RDS
  library(glue)
  library(fs)
})

# ---- PATHS -----------------------------------------------------------------
# Adjust these to your machine.
PROJECT_ROOT <- "~/masterarbeit"   # <-- edit if needed

PATHS <- list(
  speech_rds        = file.path(PROJECT_ROOT, "data/processed/speech_predictions.rds"),
  manifesto_rds     = file.path(PROJECT_ROOT, "data/processed/manifesto_data.rds"),
  out_dir           = file.path(PROJECT_ROOT, "results/empirics"),
  fig_dir           = file.path(PROJECT_ROOT, "results/empirics/figures"),
  cache_dir         = file.path(PROJECT_ROOT, "results/empirics/cache")
)
invisible(lapply(PATHS[c("out_dir", "fig_dir", "cache_dir")], dir_create))

# ---- ANALYTICAL SCOPE ------------------------------------------------------
LEGISLATIVE_PERIODS <- 13:20
PARTIES_KEEP <- c("CDU/CSU", "SPD", "FDP", "Grüne", "Linke", "AfD")

# Manifesto mapping mode for H1/H2: "M1" (preceding-election manifesto) is
# primary; M2 (time-weighted blend) is reported as robustness.
MANIFESTO_MODE_PRIMARY <- "M1"

# Bootstrap settings for H1a intervals
BOOTSTRAP_DRAWS <- 1000L
BOOTSTRAP_SEED  <- 20260507L

# ---- THE 56 MARPOR HANDBOOK 4 CODES ----------------------------------------
# Used to verify completeness of bucket lookups and to generate column names.
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

# Probability column name convention in the speech RDS: "p_101", "p_102", ...
PROB_COLS <- paste0("p_", MARPOR_CODES_56)

# Manifesto per-category column convention in MARPOR Main Dataset: "per101", "per102", ...
MARPOR_PER_COLS <- paste0("per", MARPOR_CODES_56)

# ============================================================================
# AGGREGATION A — Broad salience (9 buckets, exact partition of 56 codes)
# ============================================================================
AGG_A <- tribble(
  ~code, ~bucket_A,
  # Foreign Policy & Defence
  101, "Foreign Policy & Defence",
  102, "Foreign Policy & Defence",
  103, "Foreign Policy & Defence",
  104, "Foreign Policy & Defence",
  105, "Foreign Policy & Defence",
  106, "Foreign Policy & Defence",
  107, "Foreign Policy & Defence",
  109, "Foreign Policy & Defence",
  # European Integration
  108, "European Integration",
  110, "European Integration",
  # Democracy & Political System
  201, "Democracy & Political System",
  202, "Democracy & Political System",
  203, "Democracy & Political System",
  204, "Democracy & Political System",
  301, "Democracy & Political System",
  302, "Democracy & Political System",
  303, "Democracy & Political System",
  304, "Democracy & Political System",
  305, "Democracy & Political System",
  # Economy
  401, "Economy", 402, "Economy", 403, "Economy", 404, "Economy",
  405, "Economy", 406, "Economy", 407, "Economy", 408, "Economy",
  409, "Economy", 410, "Economy", 411, "Economy", 412, "Economy",
  413, "Economy", 414, "Economy", 415, "Economy",
  # Environment
  416, "Environment",
  501, "Environment",
  # Welfare & Social Policy
  502, "Welfare & Social Policy",
  503, "Welfare & Social Policy",
  504, "Welfare & Social Policy",
  505, "Welfare & Social Policy",
  506, "Welfare & Social Policy",
  507, "Welfare & Social Policy",
  # Law & Order and National Identity
  603, "Law & Order and National Identity",
  604, "Law & Order and National Identity",
  605, "Law & Order and National Identity",
  606, "Law & Order and National Identity",
  # Migration  (601, 602, 607, 608 — see Methodische Strategie §5.1)
  601, "Migration",
  602, "Migration",
  607, "Migration",
  608, "Migration",
  # Social Groups
  701, "Social Groups", 702, "Social Groups", 703, "Social Groups",
  704, "Social Groups", 705, "Social Groups", 706, "Social Groups"
)

# Sanity check: every code mapped exactly once
.check_A <- AGG_A %>% count(code) %>% filter(n != 1)
stopifnot(nrow(.check_A) == 0)
.missing_A <- setdiff(MARPOR_CODES_56, AGG_A$code)
.extra_A   <- setdiff(AGG_A$code, MARPOR_CODES_56)
if (length(.missing_A) > 0) stop("AGG_A missing codes: ", paste(.missing_A, collapse = ", "))
if (length(.extra_A)   > 0) stop("AGG_A extra codes: ",   paste(.extra_A,   collapse = ", "))

BUCKETS_A <- c(
  "Foreign Policy & Defence",
  "European Integration",
  "Democracy & Political System",
  "Economy",
  "Welfare & Social Policy",
  "Environment",
  "Law & Order and National Identity",
  "Migration",
  "Social Groups"
)

# ============================================================================
# AGGREGATION B — Directional within key domains
# (Andere = everything not listed; used only as filter, see methodology §3.2)
# ============================================================================
AGG_B <- tribble(
  ~code, ~domain_B,           ~bucket_B,
  # Economy
  401, "Economy",             "Marktliberalismus",
  402, "Economy",             "Marktliberalismus",
  407, "Economy",             "Marktliberalismus",
  414, "Economy",             "Marktliberalismus",
  403, "Economy",             "Staatsintervention",
  404, "Economy",             "Staatsintervention",
  405, "Economy",             "Staatsintervention",
  409, "Economy",             "Staatsintervention",
  412, "Economy",             "Staatsintervention",
  413, "Economy",             "Staatsintervention",
  408, "Economy",             "Wirtschaft Allgemein",
  410, "Economy",             "Wirtschaft Allgemein",
  411, "Economy",             "Wirtschaft Allgemein",
  # Welfare
  503, "Welfare",             "Sozialstaat Ausbau",
  504, "Welfare",             "Sozialstaat Ausbau",
  506, "Welfare",             "Sozialstaat Ausbau",
  505, "Welfare",             "Sozialstaat Begrenzung",
  507, "Welfare",             "Sozialstaat Begrenzung",
  # Migration  (Handbook 4 three-digit codes; see methodology §5.2)
  601, "Migration",           "Migration restriktiv",
  608, "Migration",           "Migration restriktiv",
  602, "Migration",           "Migration liberal",
  607, "Migration",           "Migration liberal",
  # Europe
  108, "Europe",              "Pro-EU",
  110, "Europe",              "Contra-EU"
)

DOMAINS_B <- c("Economy", "Welfare", "Migration", "Europe")

BUCKETS_B <- c(
  "Marktliberalismus", "Staatsintervention", "Wirtschaft Allgemein",
  "Sozialstaat Ausbau", "Sozialstaat Begrenzung",
  "Migration restriktiv", "Migration liberal",
  "Pro-EU", "Contra-EU"
)

# ---- PARTY COLOURS (consistent across all figures) -------------------------
PARTY_COLOURS <- c(
  "CDU/CSU" = "#000000",
  "SPD"     = "#E3000F",
  "FDP"     = "#FFCC00",
  "Grüne"   = "#1AA037",
  "Linke"   = "#BE3075",
  "AfD"     = "#0489DB"
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

message("[00_config] Configuration loaded. ",
        length(BUCKETS_A), " A-buckets; ",
        length(BUCKETS_B), " B-buckets across ",
        length(DOMAINS_B), " domains.")
