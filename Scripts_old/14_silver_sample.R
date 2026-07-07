# ============================================================================
# 14_silver_sample.R — Draw the silver-standard sample + build the LLM input
# ----------------------------------------------------------------------------
# Implements the silver-standard sampling step (MIv2 §3.4.2): ~N_PER_PARTY
# sentences per party, stratified by PREDICTED Aggregation-B domain, so the
# downstream LLM-vs-ManifestoBERTa comparison has power in every
# (party × domain) cell — that is the design that lets §3.4.3 detect
# systematic classifier asymmetries across parties and domains.
#
# STRATIFICATION
#   A 6 × 5 grid: six parties × five predicted domains
#   {Economy, Welfare, Migration, Europe, Andere}, where the domain is
#   AGG_B's domain for the 24 directional codes and "Andere" otherwise.
#   Allocation is BALANCED (target TARGET_PER_DOMAIN per cell) rather than
#   proportional, because asymmetry detection needs adequate power in the
#   small key domains (Europe, Migration), not a representative mirror.
#   Cells are capped at their available population and shortfalls are
#   water-filled into the party's other domains, so each party still reaches
#   ~N_PER_PARTY where the corpus allows.
#
# OUTPUTS (in results/empirics/silver/)
#   silver_for_classification.parquet  — BLIND input for the Python classifier
#   silver_for_classification.csv      — human-readable copy
#       columns: item_id, set, context_before, sentence, context_after
#       (NO party, NO ManifestoBERTa prediction → the LLM codes uncontaminated)
#   silver_key.rds / silver_key_slim.csv
#       sv_id ↔ ManifestoBERTa prediction, party, LP, domain + 56 probs,
#       for the asymmetry analysis later.
#
# GOLD SENTENCES (for the κ gate)
#   If the gold key exists, its 200 hand-coded sentences are appended to the
#   classification input (set = "gold") so ONE Python run codes both. The
#   gold rows give you the human↔LLM Cohen's κ (§3.4.2 gate, κ > 0.6); the
#   silver rows give the asymmetry analysis. Codes link back by item_id.
#
# Run AFTER the classified parquet exists (and, ideally, after 12 so the gold
# sentences can be appended). Edit PARAMETERS, then source the whole file.
# ============================================================================

source(here::here("R/00_config.R"))     # PROJECT_ROOT, PATHS, AGG_A, AGG_B, DOMAINS_B, ...
source(here::here("R/02_helpers.R"))    # normalize_party()

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

# Defensive fallback (see 12_goldstandard_sample.R for rationale)
if (!exists("normalize_party")) {
  normalize_party <- function(x) {
    x <- toupper(trimws(x))
    dplyr::case_when(
      x %in% c("CDU", "CSU", "CDU/CSU")                         ~ "CDU/CSU",
      x == "SPD"                                                ~ "SPD",
      x == "FDP"                                                ~ "FDP",
      x %in% c("GRUENE", "GRÜNE", "B90/GRUENE",
               "BÜNDNIS 90/DIE GRÜNEN")                         ~ "Grüne",
      x %in% c("LINKE", "DIE LINKE", "PDS", "LINKSPARTEI")      ~ "Linke",
      x == "AFD"                                                ~ "AfD",
      TRUE                                                      ~ NA_character_
    )
  }
}

# ---- PARAMETERS (edit these) ----------------------------------------------
N_PER_PARTY       <- 500L          # target sentences per party (~3000 total over 6 parties)
TARGET_PER_DOMAIN <- 100L          # balanced target per (party × domain) cell (N_PER_PARTY / 5)
SV_SEED           <- 20260603L
INCLUDE_GOLD      <- TRUE          # append the 200 gold sentences for the human↔LLM κ gate
RESTRICT_MIN_CELL <- FALSE         # TRUE = sample only from (party,LP) cells with n_sent >= N_SENT_MIN

OUT_DIR      <- file.path(PATHS$out_dir, "silver")
OUT_DIR_GOLD <- file.path(PATHS$out_dir, "goldstandard")
fs::dir_create(OUT_DIR)

DOMAINS5 <- c(DOMAINS_B, "Andere")   # Economy, Welfare, Migration, Europe, Andere

# ============================================================================
# Balanced allocation: aim TARGET_PER_DOMAIN per stratum, cap at population,
# water-fill the remainder into strata with headroom (keeping balance), until
# the total reaches min(N, sum(pops)).
# ============================================================================
allocate_balanced <- function(pops, N, target_each) {
  pops <- pmax(as.integer(pops), 0L)
  N    <- min(N, sum(pops))
  a    <- pmin(target_each, pops)
  guard <- 0L
  while (sum(a) < N) {
    head <- pops - a
    cand <- which(head > 0L)
    if (!length(cand)) break
    # add one to the currently-smallest cell with headroom (ties: most headroom)
    j <- cand[order(a[cand], -head[cand])][1]
    a[j] <- a[j] + 1L
    guard <- guard + 1L
    if (guard > 1e6L) stop("allocate_balanced() failed to converge.")
  }
  a
}

# ============================================================================
# 1. Scan each parquet chunk for stratification columns + a position-based uid
# ----------------------------------------------------------------------------
# (speech_id, sentence_nr) is NOT unique in this corpus, so we key on
# uid = "<chunk_index>_<row_in_chunk>"; read_parquet preserves physical row
# order, so the same uid re-reads the same sentence in step 4.
# ============================================================================
chunk_files <- sort(list.files(PATHS$parquet_dir,
                               pattern = "^chunk_\\d+\\.parquet$",
                               full.names = TRUE))
if (length(chunk_files) == 0L)
  stop("No parquet chunks found in: ", PATHS$parquet_dir)

schema_names <- arrow::open_dataset(chunk_files[1], format = "parquet")$schema$names
has_ctx      <- all(c("context_before", "context_after") %in% schema_names)
prob_cols    <- grep("^[0-9]{3} - ", schema_names, value = TRUE)
stopifnot(length(prob_cols) == 56L)

light_cols <- c("speech_id", "legislative_period", "party",
                "sentence_nr", "pred_label", "pred_score")

message(sprintf("[14] Scanning %d parquet chunks for stratification columns ...",
                length(chunk_files)))
light_list <- vector("list", length(chunk_files))
for (i in seq_along(chunk_files)) {
  ch <- arrow::read_parquet(chunk_files[i], col_select = all_of(light_cols))
  ch$chunk_i      <- i
  ch$row_in_chunk <- seq_len(nrow(ch))
  light_list[[i]] <- ch
  if (i %% 50L == 0L || i == length(chunk_files))
    message(sprintf("[14]   chunk %d / %d", i, length(chunk_files)))
}
light <- dplyr::bind_rows(light_list)
rm(light_list); invisible(gc(verbose = FALSE))

light <- light %>%
  mutate(uid   = sprintf("%d_%d", chunk_i, row_in_chunk),
         party = normalize_party(party)) %>%
  filter(!is.na(party), party %in% PARTIES_KEEP,
         legislative_period %in% LEGISLATIVE_PERIODS) %>%
  mutate(pred_code = as.integer(str_extract(pred_label, "^\\d{3}"))) %>%
  left_join(AGG_A, by = c("pred_code" = "code")) %>%                 # -> bucket_A
  left_join(AGG_B, by = c("pred_code" = "code")) %>%                 # -> domain_B, bucket_B
  mutate(domain_stratum = dplyr::coalesce(domain_B, "Andere"))
stopifnot(all(light$pred_code %in% MARPOR_CODES_56))
stopifnot(all(light$domain_stratum %in% DOMAINS5))
message(sprintf("[14]   %s in-scope sentences across %d (party,LP) cells.",
                format(nrow(light), big.mark = ","),
                nrow(distinct(light, party, legislative_period))))

if (RESTRICT_MIN_CELL) {
  keep_cells <- light %>%
    count(party, legislative_period, name = "n_sent") %>%
    filter(n_sent >= N_SENT_MIN) %>%
    select(party, legislative_period)
  light <- semi_join(light, keep_cells, by = c("party", "legislative_period"))
  message(sprintf("[14]   RESTRICT_MIN_CELL = TRUE: %s sentences remain.",
                  format(nrow(light), big.mark = ",")))
}

# ============================================================================
# 2. Allocation grid: balanced TARGET_PER_DOMAIN per (party × domain), capped
# ============================================================================
pop_grid <- expand_grid(party = PARTIES_KEEP, domain_stratum = DOMAINS5) %>%
  left_join(count(light, party, domain_stratum, name = "pop"),
            by = c("party", "domain_stratum")) %>%
  mutate(pop = dplyr::coalesce(pop, 0L))

alloc_grid <- pop_grid %>%
  group_by(party) %>%
  group_modify(~{
    a <- allocate_balanced(setNames(.x$pop, .x$domain_stratum),
                           N_PER_PARTY, TARGET_PER_DOMAIN)
    tibble(domain_stratum = names(a), n_alloc = as.integer(a))
  }) %>%
  ungroup() %>%
  left_join(pop_grid, by = c("party", "domain_stratum"))

# ============================================================================
# 3. Draw the sample (per party × domain cell)
# ============================================================================
set.seed(SV_SEED)
sampled <- light %>%
  group_by(party, domain_stratum) %>%
  group_modify(function(df, key) {
    n_k <- alloc_grid$n_alloc[alloc_grid$party == key$party &
                              alloc_grid$domain_stratum == key$domain_stratum]
    if (!length(n_k) || n_k == 0L) return(df[0, ])
    slice_sample(df, n = min(n_k, nrow(df)))
  }) %>%
  ungroup()

# Shuffle and assign blind IDs
set.seed(SV_SEED + 1L)
sampled <- sampled %>%
  slice_sample(prop = 1) %>%
  mutate(sv_id = sprintf("SV%04d", row_number()))

# ============================================================================
# 4. Fetch text / context / probabilities BY POSITION (1-to-1 on uid)
# ============================================================================
message("[14] Fetching sentence text / context / probabilities for the sample ...")
ctx_cols   <- if (has_ctx) c("context_before", "context_after") else character(0)
fetch_cols <- c("date", "sentence", ctx_cols, prob_cols)

needed <- sampled %>% distinct(chunk_i, row_in_chunk, uid)
ci_vec <- sort(unique(needed$chunk_i))
fetch_list <- vector("list", length(ci_vec))
for (k in seq_along(ci_vec)) {
  ci  <- ci_vec[k]
  sub <- needed[needed$chunk_i == ci, ]
  ch  <- arrow::read_parquet(chunk_files[ci],
                             col_select = all_of(c("speech_id", "sentence_nr", fetch_cols)))
  ch  <- ch[sub$row_in_chunk, , drop = FALSE]
  ch$uid <- sub$uid
  fetch_list[[k]] <- ch
}
fetched <- dplyr::bind_rows(fetch_list) %>% select(uid, all_of(fetch_cols))

sampled_full <- sampled %>%
  select(sv_id, uid, speech_id, sentence_nr, party, lp = legislative_period,
         pred_label, pred_code, pred_score, bucket_A, domain_B, bucket_B,
         domain_stratum) %>%
  left_join(fetched, by = "uid")
stopifnot(nrow(sampled_full) == nrow(sampled))   # guard against any fan-out

if (!has_ctx) {
  sampled_full <- sampled_full %>%
    mutate(context_before = NA_character_, context_after = NA_character_)
  warning("Parquet carries no context columns — context left blank for the classifier.")
}

# ============================================================================
# 5. Build the BLIND classification input (silver rows + optional gold rows)
# ============================================================================
nz <- function(x) ifelse(is.na(x), "", x)

class_silver <- sampled_full %>%
  transmute(item_id = sv_id, set = "silver",
            context_before = nz(context_before),
            sentence,
            context_after  = nz(context_after))

n_gold <- 0L
gold_key_path <- file.path(OUT_DIR_GOLD, "goldstandard_key.rds")
if (INCLUDE_GOLD && file.exists(gold_key_path)) {
  gold <- readRDS(gold_key_path) %>% as_tibble()
  class_gold <- gold %>%
    transmute(item_id = gs_id, set = "gold",
              context_before = nz(context_before),
              sentence,
              context_after  = nz(context_after))
  n_gold <- nrow(class_gold)
  class_input <- bind_rows(class_gold, class_silver)
} else {
  if (INCLUDE_GOLD)
    warning("Gold key not found at ", gold_key_path,
            " — classification input has SILVER only. Code the gold sentences ",
            "separately (or re-run 12 first) to get the human↔LLM κ gate.")
  class_input <- class_silver
}

# ============================================================================
# 6. Write outputs
# ============================================================================
class_parquet <- file.path(OUT_DIR, "silver_for_classification.parquet")
class_csv     <- file.path(OUT_DIR, "silver_for_classification.csv")
key_rds       <- file.path(OUT_DIR, "silver_key.rds")
key_csv       <- file.path(OUT_DIR, "silver_key_slim.csv")

arrow::write_parquet(class_input, class_parquet)
readr::write_excel_csv(class_input, class_csv)

saveRDS(sampled_full, key_rds)   # full silver key incl. 56 prob cols
readr::write_excel_csv(
  select(sampled_full, sv_id, uid, speech_id, sentence_nr, party, lp, date,
         pred_label, pred_code, bucket_A, domain_B, bucket_B, domain_stratum),
  key_csv)

# ============================================================================
# 7. Sampling report
# ============================================================================
realised_n   <- nrow(sampled_full)
grid_realised <- sampled_full %>%
  count(party, domain_stratum) %>%
  pivot_wider(names_from = domain_stratum, values_from = n, values_fill = 0) %>%
  select(party, any_of(DOMAINS5)) %>%
  mutate(Total = rowSums(across(any_of(DOMAINS5))))
shortfall <- alloc_grid %>%
  filter(n_alloc < TARGET_PER_DOMAIN) %>%
  arrange(party, domain_stratum) %>%
  select(party, domain_stratum, available = pop, allocated = n_alloc)

cat("\n==================== SILVER-STANDARD SAMPLE ====================\n")
cat(sprintf("Target/party: %d   |   target/cell: %d   |   realised silver n: %d\n",
            N_PER_PARTY, TARGET_PER_DOMAIN, realised_n))
cat(sprintf("Gold sentences appended for the κ gate: %d\n", n_gold))
cat(sprintf("Total rows in classification input: %d\n", nrow(class_input)))
cat(sprintf("Seed: %d   |   RESTRICT_MIN_CELL: %s\n", SV_SEED, RESTRICT_MIN_CELL))
cat("\n-- realised counts: party × predicted domain -------------------\n")
print(grid_realised, n = Inf)
if (nrow(shortfall) > 0) {
  cat("\n-- cells below target (capped by available population) ---------\n")
  print(shortfall, n = Inf)
} else {
  cat("\n-- every cell reached its target; no capping needed ------------\n")
}
cat("\nFiles written:\n")
cat("  Classifier input (parquet): ", class_parquet, "\n", sep = "")
cat("  Classifier input (csv):     ", class_csv, "\n", sep = "")
cat("  Silver key (rds, + probs):  ", key_rds, "\n", sep = "")
cat("  Silver key (csv, slim):     ", key_csv, "\n", sep = "")
cat("===============================================================\n")
