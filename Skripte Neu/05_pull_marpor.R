# 05_pull_marpor.R  (v4; ex pull_marpor_align.R)
# -------------------------
# Builds 56-class manifesto distributions per (legislative_period, party_label,
# mapping) for the JSD comparison against speech-side distributions.
#
# Changes from v3:
#   - Removed CDU/CSU pooling code: MARPOR codes CDU and CSU jointly under
#     a single id (41521, partyname = "CDU/CSU"). There is no separate CSU
#     id to pool with. The v2/v3 attempts at "joint" handling were either
#     misnamed (v2: 41521 alone, called joint; happened to be correct since
#     it IS the joint) or over-engineered (v3: tried to pool a non-existent
#     41522 with 41521).
#   - Added party id 41222 (L-PDS / Die Linkspartei.PDS, 2005 transitional
#     entity) to the DIE LINKE lookup. Without this, LP16 (2005-09 election)
#     has no manifesto on the left because PDS (41221) ends in 2002 and
#     DIE LINKE (41223) starts in 2009.
#   - Kept the v3 fixes that mattered: SPD = 41320 (was wrongly 41221 in v2);
#     setnames() now uses per-prefixed names; coverage assertion at end.
#
# Approach: uses mp_maindataset() rather than mp_corpus_df(). The corpus only
# has annotated quasi-sentence text for a subset of (party, year) cells; the
# main dataset has aggregated per-category percentages for essentially every
# (party, year) cell we need. Since JSD only requires the aggregate
# distribution, the main dataset is both more complete and more direct.
#
# Mappings produced:
#   M1: preceding-election only (one manifesto per LP)
#   M2: bracketing average (entering + next-election manifesto)
#
# Output: Data/manifesto_distributions.rds  -- a data.table keyed by
# (legislative_period, party_label, mapping) with 56 prob columns matching
# the speech-side aggregate column names exactly.

# ---- 0. Setup -----------------------------------------------------------

library(manifestoR)
library(data.table)

setwd("S:/RProj_MSc/MScThesis")
mp_setapikey("manifesto_apikey.txt")


# ---- 1. Reference tables ------------------------------------------------

# Speech-side prob columns (canonical 56-class column names: "NNN - <Label>").
speech_agg <- readRDS("Data/speech_distributions_party_lp.rds")
prob_cols  <- grep("^[0-9]{3} - ", names(speech_agg), value = TRUE)
stopifnot(length(prob_cols) == 56)

# Bare 3-digit code -> speech-side column name lookup.
code_to_col <- setNames(prob_cols,
                        sub("^([0-9]{3}).*", "\\1", prob_cols))

# Federal election dates (entering each LP).
elections <- data.table(
  legislative_period = 13:20,
  election_date      = as.Date(c("1994-10-16", "1998-09-27", "2002-09-22",
                                 "2005-09-18", "2009-09-27", "2013-09-22",
                                 "2017-09-24", "2021-09-26"))
)
elections[, date_yyyymm := as.integer(format(election_date, "%Y%m"))]

# 2025-02-23 election as the "next election" anchor for LP20's M2 bracketing.
next_anchor_yyyymm <- 202502L

# Verified MARPOR party IDs (Manifesto Project Main Dataset 2025-1).
# Confirmed by inspecting the German subset of mp_maindataset():
#   41320 = SPD
#   41521 = CDU/CSU joint (MARPOR codes the Union as a single entity;
#                          there is no separate CSU id at the federal level)
#   41420 = FDP
#   41113 = Bündnis 90/Die Grünen (post-1993 merger)
#   41221 = PDS (1990-2002 era, partyname "Party of Democratic Socialism")
#   41222 = L-PDS / Die Linkspartei.PDS (2005 transitional)
#   41223 = DIE LINKE (post-2007 merger of Linkspartei.PDS and WASG)
#   41953 = AfD
party_lookup <- data.table(
  marpor_party = c(41320L, 41521L, 41420L, 41113L,
                   41221L, 41222L, 41223L,
                   41953L),
  party_label  = c("SPD", "CDU_CSU_joint", "FDP", "GRUENE",
                   "DIE LINKE", "DIE LINKE", "DIE LINKE",
                   "AfD")
)


# ---- 2. Pull main dataset and filter to our cells -----------------------

mpds <- as.data.table(mp_maindataset())
cat("Main dataset rows:", format(nrow(mpds), big.mark = ","), "\n")

# Identify the 56 per-category columns (per101..per706).
per_cols <- grep("^per[0-9]{3}$", names(mpds), value = TRUE)
stopifnot(length(per_cols) == 56)

# Sanity: every per-column maps to exactly one speech-side column.
per_codes <- sub("^per", "", per_cols)
stopifnot(all(per_codes %in% names(code_to_col)))

target_dates <- c(elections$date_yyyymm, next_anchor_yyyymm)
mpds_target <- mpds[party %in% party_lookup$marpor_party &
                      date %in% target_dates]
cat("Filtered main dataset:", nrow(mpds_target),
    "rows for our (party, election) grid.\n")

# Diagnostic: rows per party id (catches missing parties immediately).
cat("\nRows per party id (after filter):\n")
print(mpds_target[, .(n_rows = .N,
                      first_date = min(date),
                      last_date  = max(date)),
                  by = party][order(party)])


# ---- 3. Renormalize per-category vectors to unit sum -------------------

# Each per-category column holds the percentage of coded quasi-sentences
# in that category. They sum to ~100 (less the H/uncoded share -- e.g. SPD
# 1998 sums to 98.936). Replace NA with 0 (treat unscored cells as zero
# count, standard CMP practice), then renormalize to a unit-sum probability
# vector.

for (c in per_cols) {
  set(mpds_target, which(is.na(mpds_target[[c]])), c, 0)
}

row_sum <- rowSums(mpds_target[, ..per_cols])
cat("\nPre-normalization row sum range:", round(range(row_sum), 2), "\n")
zero_rows <- which(row_sum == 0)
if (length(zero_rows) > 0) {
  cat("Dropping", length(zero_rows), "rows with zero per-category total.\n")
  mpds_target <- mpds_target[-zero_rows]
  row_sum <- row_sum[-zero_rows]
}
mpds_target[, (per_cols) := lapply(.SD, function(x) x / row_sum),
            .SDcols = per_cols]
stopifnot(all(abs(rowSums(mpds_target[, ..per_cols]) - 1) < 1e-9))


# ---- 4. Rename per-cols to canonical speech-side names -----------------

# Build the rename vector: per101 -> "101 - Foreign Special Relationships..."
rename_old <- per_cols                  # e.g. "per101", "per102"
rename_new <- code_to_col[per_codes]    # e.g. "101 - ...", "102 - ..."
stopifnot(all(rename_old %in% names(mpds_target)))
stopifnot(!any(is.na(rename_new)))

setnames(mpds_target, old = rename_old, new = unname(rename_new))


# ---- 5. Attach party labels --------------------------------------------

mpds_target[, party_label := party_lookup$party_label[
  match(party, party_lookup$marpor_party)]]
stopifnot(!any(is.na(mpds_target$party_label)))

# Within DIE LINKE: 41221, 41222, 41223 each contribute a single manifesto
# per election year, and these years don't overlap (41221 covers 1994-2002,
# 41222 covers 2005, 41223 covers 2009+). So no further pooling is needed --
# rbinding them under the same label gives one row per election year.

manifesto_per_election <- mpds_target[, c("party", "party_label",
                                          "date",
                                          prob_cols), with = FALSE]

cat("\nManifesto-by-election table:", nrow(manifesto_per_election), "rows.\n")
cat("Coverage by party_label x date:\n")
print(dcast(manifesto_per_election[, .N, by = .(party_label, date)],
            party_label ~ date, value.var = "N", fill = 0))

# Sanity: within a party_label, no date should have more than one row.
dup_check <- manifesto_per_election[, .N, by = .(party_label, date)][N > 1]
if (nrow(dup_check) > 0) {
  cat("\n!!! Duplicate (party_label, date) cells:\n")
  print(dup_check)
  stop("Multiple manifestos found for the same (party_label, date). ",
       "If pooling is intended, add explicit pooling logic.")
}


# ---- 6. Map manifestos to legislative periods --------------------------

# M1: each LP gets the manifesto from the entering election.
m1 <- merge(
  elections[, .(legislative_period, date_yyyymm)],
  manifesto_per_election,
  by.x = "date_yyyymm", by.y = "date",
  allow.cartesian = TRUE
)
m1[, mapping := "M1_entering"]
m1[, n_manifestos := 1L]

# M2: bracketing average. For each LP, average the prob vectors of the
# entering manifesto and the next-election manifesto.
elections_m2 <- copy(elections)
elections_m2[, next_yyyymm := shift(date_yyyymm, type = "lead")]
elections_m2[is.na(next_yyyymm), next_yyyymm := next_anchor_yyyymm]

entering <- merge(elections_m2[, .(legislative_period, date_yyyymm)],
                  manifesto_per_election,
                  by.x = "date_yyyymm", by.y = "date",
                  allow.cartesian = TRUE)
trailing <- merge(elections_m2[, .(legislative_period,
                                   date_yyyymm = next_yyyymm)],
                  manifesto_per_election,
                  by.x = "date_yyyymm", by.y = "date",
                  allow.cartesian = TRUE)

m2_long <- rbindlist(list(entering, trailing), use.names = TRUE)

# Average the 56 prob columns across the (up to two) manifestos per (LP, party).
# If a party only has one of the two bracketing manifestos, M2 falls back to
# that single manifesto -- documented in the limitations section of the thesis.
m2 <- m2_long[, c(
  list(n_manifestos = .N),
  lapply(.SD, mean)
), by = .(legislative_period, party_label),
  .SDcols = prob_cols]
m2[, mapping := "M2_bracketing"]

# Sanity: M2 vectors still sum to 1 (mean of unit-sum vectors).
stopifnot(all(abs(rowSums(m2[, ..prob_cols]) - 1) < 1e-9))


# ---- 7. Combine and save -----------------------------------------------

keep_cols <- c("legislative_period", "party_label", "mapping",
               "n_manifestos", prob_cols)
m1_out <- m1[, ..keep_cols]
m2_out <- m2[, ..keep_cols]

manifesto_distributions <- rbindlist(list(m1_out, m2_out), use.names = TRUE)
setkey(manifesto_distributions, legislative_period, party_label, mapping)

cat("\n=== Final manifesto_distributions table ===\n")
cat("Rows:", nrow(manifesto_distributions),
    " Columns:", ncol(manifesto_distributions), "\n\n")
print(manifesto_distributions[, .N, by = .(party_label, mapping)][
                                order(party_label, mapping)])

cat("\n=== Coverage matrix: rows present per (LP, party_label, mapping) ===\n")
cov_mat <- dcast(manifesto_distributions[, .(legislative_period,
                                             party_label, mapping)],
                 legislative_period + party_label ~ mapping,
                 fun.aggregate = length)
print(cov_mat)


# ---- 8. Coverage assertion --------------------------------------------

# These (party_label, LP) cells MUST exist on the manifesto side under M1.
# Failure here means the MARPOR pull/labelling broke -- do NOT proceed with
# downstream JSD calculations until coverage is restored.
required_m1 <- list(
  SPD              = 13:20,
  CDU_CSU_joint    = 13:20,
  FDP              = 13:20,    # speech-side FDP excluded for LP18 in JSD
  GRUENE           = 13:20,
  AfD              = 19:20,    # entered Bundestag in 2017
  `DIE LINKE`      = 16:20     # 41222 (2005 -> LP16), 41223 (2009 -> LP17+)
)

missing_cells <- list()
for (lab in names(required_m1)) {
  lps <- required_m1[[lab]]
  have <- manifesto_distributions[party_label == lab & mapping == "M1_entering",
                                  legislative_period]
  miss <- setdiff(lps, have)
  if (length(miss) > 0) missing_cells[[lab]] <- miss
}
if (length(missing_cells) > 0) {
  cat("\n!!! MISSING M1 CELLS:\n")
  for (lab in names(missing_cells))
    cat("  ", lab, ": LP(s) ",
        paste(missing_cells[[lab]], collapse = ", "), "\n", sep = "")
  stop("Coverage check failed -- see missing cells above.")
} else {
  cat("\nCoverage check: all required (party_label, LP) cells present under M1.\n")
}

saveRDS(manifesto_distributions, "Data/manifesto_distributions.rds")
cat("\nSaved: Data/manifesto_distributions.rds\n")


# ---- 9. Sanity preview -------------------------------------------------

# SPD 1998 manifesto top codes (LP14, M1).
spd_lp14 <- manifesto_distributions[party_label == "SPD" &
                                      legislative_period == 14 &
                                      mapping == "M1_entering"]
if (nrow(spd_lp14) == 1) {
  spd_long <- melt(spd_lp14, id.vars = "party_label",
                   measure.vars = prob_cols,
                   variable.name = "code", value.name = "prob")
  cat("\nPreview: SPD 1998 manifesto top 8 codes (LP14, M1):\n")
  print(spd_long[order(-prob)][1:8, .(code, prob = round(prob, 4))])
}

# DIE LINKE 2005 manifesto top codes (LP16, M1) -- spot-check the 41222 row.
linke_lp16 <- manifesto_distributions[party_label == "DIE LINKE" &
                                        legislative_period == 16 &
                                        mapping == "M1_entering"]
if (nrow(linke_lp16) == 1) {
  l_long <- melt(linke_lp16, id.vars = "party_label",
                 measure.vars = prob_cols,
                 variable.name = "code", value.name = "prob")
  cat("\nPreview: L-PDS 2005 manifesto top 8 codes (LP16, M1):\n")
  print(l_long[order(-prob)][1:8, .(code, prob = round(prob, 4))])
}
