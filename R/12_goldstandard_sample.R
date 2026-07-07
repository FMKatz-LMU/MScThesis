# ============================================================================
# 12_goldstandard_sample.R — Draw & format the gold-standard validation sample
# ----------------------------------------------------------------------------
# Implements the manual gold-standard sampling step (MIv2 §3.4.1).
#
# WHAT IT DOES
#   1. Loads the post-filter, classified sentence corpus (the same parquet
#      that feeds every hypothesis) and restricts it to the six analysis
#      parties across LP 13–20.
#   2. Builds ONE stratification variable that simultaneously covers
#        (a) all nine directional Aggregation-B sub-buckets, AND
#        (b) all nine Aggregation-A salience buckets,
#      by using the B sub-bucket where a code has one, and the A bucket
#      otherwise. Coverage of both schemes is therefore guaranteed by
#      construction (MIv2 §3.4.1: "...coverage of all directional buckets in
#      Aggregation B and supplementary coverage of the salience buckets in
#      Aggregation A").
#   3. Draws ~N_TARGET sentences, proportional to predicted-stratum mass with
#      a per-stratum FLOOR so rare directional buckets (e.g. Contra-EU) are
#      still represented, plus a safety-net floor on code-305 sentences (the
#      DPS / Political-Authority artefact diagnostic that feeds the planned
#      H1a code-305 exclusion).
#   4. Writes TWO artefacts:
#        * a BLIND coding sheet (xlsx) with a 56+1 code dropdown and the
#          sentence context — it contains NO model prediction and NO party
#          label, so your coding is not anchored by either;
#        * a SEPARATE key file (rds + slim csv) linking gs_id -> model
#          prediction, party, LP, buckets and the full 56-d probability
#          vector, for the evaluation step later.
#
# CODING GRANULARITY
#   The three-digit MARPOR Handbook-4 category — the exact label space
#   ManifestoBERTa predicts. 000 ("no other category applies") is available
#   in the dropdown for genuinely uncodable / purely procedural sentences;
#   its share is itself a finding (it quantifies the cross-domain force-fit).
#
# PREREQUISITE
#   Data/sentences_classified/chunk_*.parquet must exist (output of the
#   Python ManifestoBERTa inference). Edit the PARAMETERS block, then source
#   the whole file. Final n may exceed N_TARGET slightly if the 305 floor
#   binds; the realised n is printed at the end — re-run with a tuned
#   N_TARGET if you want it exactly on 200.
# ============================================================================

source(here::here("R/00_config.R"))     # PROJECT_ROOT, PATHS, AGG_A, AGG_B, ...
source(here::here("R/02_helpers.R"))    # provides normalize_party()

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(stringr)
  library(tibble)
})
if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("Package 'openxlsx' is required. Run install.packages('openxlsx').")
library(openxlsx)

# Defensive fallback: 03_load_and_aggregate.R relies on normalize_party()
# existing in 02_helpers.R. If it isn't found, define a minimal version so
# the script still runs standalone.
if (!exists("normalize_party")) {
  normalize_party <- function(x) {
    x <- toupper(trimws(x))
    dplyr::case_when(
      x %in% c("CDU", "CSU", "CDU/CSU") ~ "CDU/CSU",
      x == "SPD"                         ~ "SPD",
      x == "FDP"                         ~ "FDP",
      x %in% c("GRUENE", "GRÜNE", "B90/GRUENE",
               "BÜNDNIS 90/DIE GRÜNEN")  ~ "Grüne",
      x %in% c("LINKE", "DIE LINKE", "PDS",
               "LINKSPARTEI")            ~ "Linke",
      x == "AFD"                         ~ "AfD",
      TRUE                               ~ NA_character_
    )
  }
}

# ---- PARAMETERS (edit these) ----------------------------------------------
N_TARGET          <- 200L          # target sample size
FLOOR_STRAT       <- 10L            # min sentences per stratum (capped at stratum population)
N_305_MIN         <- 15L           # safety-net floor on code-305 sentences
GS_SEED           <- 20260602L     # reproducibility
RESTRICT_MIN_CELL <- FALSE         # TRUE = sample only from (party,LP) cells with n_sent >= N_SENT_MIN

OUT_DIR <- file.path(PATHS$out_dir, "goldstandard")
fs::dir_create(OUT_DIR)

# ============================================================================
# Official Handbook-4 category titles (source: MARPOR codebook_categories
# MPDS2020a). Used only to label the dropdown / reference sheet; matching at
# evaluation time uses the three-digit code, so exact title wording is moot.
# ============================================================================
TITLE_LOOKUP <- c(
  "000" = "No other category applies (uncodable / rein prozedural)",
  "101" = "Foreign Special Relationships: Positive",
  "102" = "Foreign Special Relationships: Negative",
  "103" = "Anti-Imperialism",
  "104" = "Military: Positive",
  "105" = "Military: Negative",
  "106" = "Peace",
  "107" = "Internationalism: Positive",
  "108" = "European Community/Union: Positive",
  "109" = "Internationalism: Negative",
  "110" = "European Community/Union: Negative",
  "201" = "Freedom and Human Rights",
  "202" = "Democracy",
  "203" = "Constitutionalism: Positive",
  "204" = "Constitutionalism: Negative",
  "301" = "Decentralization",
  "302" = "Centralisation",
  "303" = "Governmental and Administrative Efficiency",
  "304" = "Political Corruption",
  "305" = "Political Authority",
  "401" = "Free Market Economy",
  "402" = "Incentives: Positive",
  "403" = "Market Regulation",
  "404" = "Economic Planning",
  "405" = "Corporatism/Mixed Economy",
  "406" = "Protectionism: Positive",
  "407" = "Protectionism: Negative",
  "408" = "Economic Goals",
  "409" = "Keynesian Demand Management",
  "410" = "Economic Growth: Positive",
  "411" = "Technology and Infrastructure: Positive",
  "412" = "Controlled Economy",
  "413" = "Nationalisation",
  "414" = "Economic Orthodoxy",
  "415" = "Marxist Analysis",
  "416" = "Anti-Growth Economy: Positive",
  "501" = "Environmental Protection",
  "502" = "Culture: Positive",
  "503" = "Equality: Positive",
  "504" = "Welfare State Expansion",
  "505" = "Welfare State Limitation",
  "506" = "Education Expansion",
  "507" = "Education Limitation",
  "601" = "National Way of Life: Positive",
  "602" = "National Way of Life: Negative",
  "603" = "Traditional Morality: Positive",
  "604" = "Traditional Morality: Negative",
  "605" = "Law and Order: Positive",
  "606" = "Civic Mindedness: Positive",
  "607" = "Multiculturalism: Positive",
  "608" = "Multiculturalism: Negative",
  "701" = "Labour Groups: Positive",
  "702" = "Labour Groups: Negative",
  "703" = "Agriculture and Farmers: Positive",
  "704" = "Middle Class and Professional Groups",
  "705" = "Underprivileged Minority Groups",
  "706" = "Non-economic Demographic Groups"
)
TITLES <- tibble(code = as.integer(names(TITLE_LOOKUP)),
                 title = unname(TITLE_LOOKUP))
stopifnot(setequal(setdiff(TITLES$code, 0L), MARPOR_CODES_56))

# ============================================================================
# Allocation helper: split N across strata proportionally to population, with
# a per-stratum floor (capped at the stratum's population), summing exactly
# to N.
# ============================================================================
allocate_counts <- function(pop, N, floor_min) {
  pop    <- pop[pop > 0]
  mins   <- pmin(floor_min, pop)
  if (sum(mins) > N)
    stop(sprintf("Infeasible: floors require %d sentences but N_TARGET = %d. Lower FLOOR_STRAT or raise N_TARGET.",
                 sum(mins), N))
  if (sum(pop) < N)
    stop(sprintf("In-scope population (%d) is smaller than N_TARGET (%d).", sum(pop), N))
  target <- N * pop / sum(pop)
  a      <- pmin(pmax(round(target), mins), pop)
  guard  <- 0L
  while (sum(a) != N) {
    guard <- guard + 1L
    if (guard > 1e6L) stop("allocate_counts() failed to converge.")
    if (sum(a) > N) {
      cand <- which(a > mins)
      j <- cand[which.max((a - target)[cand])]
      a[j] <- a[j] - 1L
    } else {
      cand <- which(a < pop)
      j <- cand[which.max((target - a)[cand])]
      a[j] <- a[j] + 1L
    }
  }
  a
}

# ============================================================================
# 1. Scan each parquet chunk for stratification columns + a position-based uid
# ----------------------------------------------------------------------------
# IMPORTANT: (speech_id, sentence_nr) is NOT a unique key in this corpus — a
# single speech_id spans several speaker turns and sentence_nr restarts each
# turn, so joining back on that pair fans out massively. We therefore build a
# bulletproof uid = "<chunk_index>_<row_in_chunk>". read_parquet preserves a
# file's physical row order (independent of which columns are selected), so
# the same uid re-reads exactly the same sentence in step 4.
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

message(sprintf("[12] Scanning %d parquet chunks for stratification columns ...",
                length(chunk_files)))
light_list <- vector("list", length(chunk_files))
for (i in seq_along(chunk_files)) {
  ch <- arrow::read_parquet(chunk_files[i], col_select = all_of(light_cols))
  ch$chunk_i      <- i
  ch$row_in_chunk <- seq_len(nrow(ch))
  light_list[[i]] <- ch
  if (i %% 50L == 0L || i == length(chunk_files))
    message(sprintf("[12]   chunk %d / %d", i, length(chunk_files)))
}
light <- dplyr::bind_rows(light_list)
rm(light_list); invisible(gc(verbose = FALSE))

light <- light %>%
  mutate(uid   = sprintf("%d_%d", chunk_i, row_in_chunk),
         party = normalize_party(party)) %>%
  filter(!is.na(party), party %in% PARTIES_KEEP,
         legislative_period %in% LEGISLATIVE_PERIODS) %>%
  mutate(pred_code = as.integer(str_extract(pred_label, "^\\d{3}")))
stopifnot(all(light$pred_code %in% MARPOR_CODES_56))
message(sprintf("[12]   %s in-scope sentences across %d (party,LP) cells.",
                format(nrow(light), big.mark = ","),
                nrow(distinct(light, party, legislative_period))))

# Optional: restrict to cells that survive the n_sent >= N_SENT_MIN inclusion
# rule used in the JSD analysis (validate exactly what is analysed).
if (RESTRICT_MIN_CELL) {
  keep_cells <- light %>%
    count(party, legislative_period, name = "n_sent") %>%
    filter(n_sent >= N_SENT_MIN) %>%
    select(party, legislative_period)
  light <- semi_join(light, keep_cells, by = c("party", "legislative_period"))
  message(sprintf("[12]   RESTRICT_MIN_CELL = TRUE: %s sentences remain in %d cells.",
                  format(nrow(light), big.mark = ","), nrow(keep_cells)))
}

# ============================================================================
# 2. Derive the combined stratum (covers Aggregation A and B at once)
# ============================================================================
key_domain_A <- c("Economy", "Welfare & Social Policy", "Migration", "European Integration")

light <- light %>%
  left_join(AGG_A, by = c("pred_code" = "code")) %>%                 # -> bucket_A
  left_join(AGG_B, by = c("pred_code" = "code")) %>%                 # -> domain_B, bucket_B
  mutate(stratum = dplyr::if_else(
    !is.na(bucket_B), bucket_B,
    dplyr::if_else(bucket_A %in% key_domain_A,
                   paste0(bucket_A, " (sonstige)"), bucket_A)))

# ============================================================================
# 3. Stratified draw
# ============================================================================
pop   <- light %>% count(stratum, name = "pop")
alloc <- allocate_counts(setNames(pop$pop, pop$stratum), N_TARGET, FLOOR_STRAT)

set.seed(GS_SEED)
sampled <- light %>%
  group_by(stratum) %>%
  group_modify(~ slice_sample(.x, n = alloc[[.y$stratum]])) %>%
  ungroup()

# 305 safety net (usually non-binding given 305 inflation)
n305 <- sum(sampled$pred_code == 305L)
if (n305 < N_305_MIN) {
  need  <- N_305_MIN - n305
  extra <- light %>%
    filter(pred_code == 305L) %>%
    anti_join(sampled, by = "uid") %>%
    slice_sample(n = need)
  sampled <- bind_rows(sampled, extra)
  message(sprintf("[12]   305 floor bound: added %d extra code-305 sentences.", nrow(extra)))
}

# Shuffle so strata are not clustered, then assign blind IDs
set.seed(GS_SEED + 1L)
sampled <- sampled %>%
  slice_sample(prop = 1) %>%
  mutate(gs_id = sprintf("GS%04d", row_number()))

# ============================================================================
# 4. Fetch text / context / probabilities for the sampled rows BY POSITION
# ----------------------------------------------------------------------------
# Re-read only the chunks that contain sampled rows, subset by row index, and
# attach via the unique uid. The join below is strictly 1-to-1 on uid, so no
# (speech_id, sentence_nr) fan-out is possible.
# ============================================================================
message("[12] Fetching sentence text / context / probabilities for the sample ...")
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
  ch  <- ch[sub$row_in_chunk, , drop = FALSE]   # same physical order as step 1
  ch$uid <- sub$uid
  fetch_list[[k]] <- ch
}
fetched <- dplyr::bind_rows(fetch_list) %>% select(uid, all_of(fetch_cols))

sampled_full <- sampled %>%
  select(gs_id, uid, speech_id, sentence_nr, party, lp = legislative_period,
         pred_label, pred_code, pred_score, bucket_A, domain_B, bucket_B, stratum) %>%
  left_join(fetched, by = "uid")
stopifnot(nrow(sampled_full) == nrow(sampled))   # guard against any fan-out

if (!has_ctx) {
  sampled_full <- sampled_full %>%
    mutate(context_before = NA_character_, context_after = NA_character_)
  warning("Parquet carries no context columns — context left blank in the coding sheet.")
}

# ============================================================================
# 5. Build the blind coding sheet and the key
# ============================================================================
nz <- function(x) ifelse(is.na(x), "", x)

coding <- sampled_full %>%
  transmute(gs_id,
            context_before = nz(context_before),
            sentence,
            context_after  = nz(context_after),
            code        = "",   # <- you fill (dropdown: 000 + 56 codes)
            code_alt    = "",   # <- optional 2nd-best code for ambiguous sentences
            flag_unsure = "",   # <- optional: "unsicher" / "strittig"
            note        = "")   # <- optional free-text rationale

key <- sampled_full %>%
  select(gs_id, speech_id, sentence_nr, party, lp, date,
         pred_label, pred_code, pred_score,
         bucket_A, domain_B, bucket_B, stratum,
         sentence, context_before, context_after,
         all_of(prob_cols))

# Reference sheet for the workbook (codes + titles + bucket mapping)
codes_ref <- TITLES %>%
  left_join(AGG_A, by = "code") %>%
  left_join(select(AGG_B, code, domain_B, bucket_B), by = "code") %>%
  mutate(code3 = sprintf("%03d", code),
         label = paste0(code3, " - ", title),
         `Aggregation A` = ifelse(is.na(bucket_A), "—", bucket_A),
         `Aggregation B` = ifelse(is.na(bucket_B), "—", bucket_B)) %>%
  arrange(code) %>%
  select(`Auswahl (für Dropdown)` = label, Code = code3, Titel = title,
         `Aggregation A`, `Aggregation B`)
n_codes <- nrow(codes_ref)   # 57 (000 + 56)

# ---- Write the xlsx --------------------------------------------------------
wb <- createWorkbook()

# Sheet: Anleitung
addWorksheet(wb, "Anleitung")
anleitung <- c(
  "GOLD-STANDARD-KODIERUNG — Anleitung",
  "",
  "Aufgabe: Ordne jedem Satz GENAU EINE dreistellige MARPOR-Kategorie (Handbook 4) zu.",
  "Verwende die Spalte 'code' (Dropdown). Lies dazu 'context_before' und 'context_after' mit —",
  "ManifestoBERTa nutzt die Kontext-Variante, also kodierst du fair mit derselben Information.",
  "",
  "Sonderfall 000: Wenn ein Satz in KEINE der 56 inhaltlichen Kategorien passt (rein",
  "prozedural, formelhaft, leer), waehle 000. Der 000-Anteil ist ein eigenes Ergebnis",
  "(er misst, wie oft das Modell prozedurale Saetze in eine Sachkategorie zwingt).",
  "",
  "Optional: 'code_alt' = zweitbeste Kategorie bei Mehrdeutigkeit; 'flag_unsure' = unsicher/strittig;",
  "'note' = kurze Begruendung bei kniffligen Faellen. Diese drei sind freiwillig.",
  "",
  "Blind by design: Diese Tabelle enthaelt WEDER die Modellvorhersage NOCH die Partei,",
  "damit deine Kodierung nicht voreingenommen wird. Die Verknuepfung steht im separaten key-File.",
  "",
  "Kodierregeln (festhalten!): Notiere Entscheidungen fuer Grenzfaelle, damit die Kodierung",
  "dokumentiert und (per Test-Retest nach einigen Tagen) reproduzierbar ist.",
  "",
  "Referenzen:",
  "  Handbook v4 (PDF): https://manifesto-project.wzb.eu/down/papers/handbook_2011_version_4.pdf",
  "  Kategorienschema v4: https://manifesto-project.wzb.eu/coding_schemes/mp_v4",
  "  Kategorien-CSV: https://manifesto-project.wzb.eu/down/data/2020a/codebooks/codebook_categories_MPDS2020a.csv",
  "  Das Tabellenblatt 'Codes' listet alle 56 Kategorien + 000 mit Titel und Bucket-Zuordnung."
)
writeData(wb, "Anleitung", anleitung, colNames = FALSE)
setColWidths(wb, "Anleitung", cols = 1, widths = 110)

# Sheet: Codes (reference + dropdown source in column A)
addWorksheet(wb, "Codes")
writeData(wb, "Codes", codes_ref)
setColWidths(wb, "Codes", cols = 1:5, widths = c(46, 8, 42, 30, 24))
freezePane(wb, "Codes", firstRow = TRUE)
addStyle(wb, "Codes", createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:5, gridExpand = TRUE)

# Sheet: Coding (what you fill in)
addWorksheet(wb, "Coding")
writeData(wb, "Coding", coding)
n_rows <- nrow(coding)
setColWidths(wb, "Coding",
             cols = 1:8, widths = c(9, 46, 62, 46, 30, 30, 13, 34))
freezePane(wb, "Coding", firstRow = TRUE, firstCol = TRUE)
addStyle(wb, "Coding", createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:8, gridExpand = TRUE)
addStyle(wb, "Coding", createStyle(wrapText = TRUE, valign = "top"),
         rows = 2:(n_rows + 1), cols = 2:4, gridExpand = TRUE)
addStyle(wb, "Coding", createStyle(valign = "top"),
         rows = 2:(n_rows + 1), cols = c(1, 5:8), gridExpand = TRUE)

# Dropdowns: code + code_alt reference the 'Codes' label column (A2:A{n+1})
code_range <- sprintf("'Codes'!$A$2:$A$%d", n_codes + 1L)
dataValidation(wb, "Coding", cols = 5, rows = 2:(n_rows + 1),
               type = "list", value = code_range, allowBlank = TRUE)
dataValidation(wb, "Coding", cols = 6, rows = 2:(n_rows + 1),
               type = "list", value = code_range, allowBlank = TRUE)
dataValidation(wb, "Coding", cols = 7, rows = 2:(n_rows + 1),
               type = "list", value = '"unsicher,strittig"', allowBlank = TRUE)

xlsx_path <- file.path(OUT_DIR, "goldstandard_codingsheet.xlsx")
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# ---- Plain-CSV fallbacks + the key ----------------------------------------
csv_coding_path <- file.path(OUT_DIR, "goldstandard_codingsheet.csv")
key_rds_path    <- file.path(OUT_DIR, "goldstandard_key.rds")
key_csv_path    <- file.path(OUT_DIR, "goldstandard_key_slim.csv")

readr::write_excel_csv(coding, csv_coding_path)
saveRDS(key, key_rds_path)                         # full key incl. 56 prob cols (for soft metrics later)
readr::write_excel_csv(
  select(key, gs_id, speech_id, sentence_nr, party, lp, date,
         pred_label, pred_code, pred_score, bucket_A, domain_B, bucket_B, stratum),
  key_csv_path)

# ============================================================================
# 6. Sampling report
# ============================================================================
realised_n <- nrow(sampled_full)
report_stratum <- sampled_full %>% count(stratum, name = "n") %>% arrange(desc(n))
report_party   <- sampled_full %>% count(party,   name = "n") %>% arrange(desc(n))
report_A       <- sampled_full %>% count(bucket_A, name = "n") %>% arrange(desc(n))
report_B       <- sampled_full %>%
  mutate(b = ifelse(is.na(bucket_B), "Andere (kein Richtungsbucket)", bucket_B)) %>%
  count(b, name = "n") %>% arrange(desc(n))

cat("\n==================== GOLD-STANDARD SAMPLE ====================\n")
cat(sprintf("Target n: %d   |   Realised n: %d   |   code-305 in sample: %d\n",
            N_TARGET, realised_n, sum(sampled_full$pred_code == 305L)))
cat(sprintf("Seed: %d   |   FLOOR_STRAT: %d   |   N_305_MIN: %d   |   RESTRICT_MIN_CELL: %s\n",
            GS_SEED, FLOOR_STRAT, N_305_MIN, RESTRICT_MIN_CELL))
cat("\n-- per stratum --------------------------------------------------\n");        print(report_stratum, n = Inf)
cat("\n-- per Aggregation-A bucket -------------------------------------\n");        print(report_A,       n = Inf)
cat("\n-- per Aggregation-B bucket -------------------------------------\n");        print(report_B,       n = Inf)
cat("\n-- per party ----------------------------------------------------\n");        print(report_party,   n = Inf)
cat("\nFiles written:\n")
cat("  Coding sheet (edit this): ", xlsx_path, "\n", sep = "")
cat("  Coding sheet (csv backup):", csv_coding_path, "\n", sep = "")
cat("  Key (rds, full + probs):  ", key_rds_path, "\n", sep = "")
cat("  Key (csv, slim):          ", key_csv_path, "\n", sep = "")
cat("==============================================================\n")
