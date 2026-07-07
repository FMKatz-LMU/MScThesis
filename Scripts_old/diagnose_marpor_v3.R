# diagnose_marpor_v3.R
# --------------------
# Full coverage check across ALL plausible German party codes and ALL
# annotation sources (German-coded, English-translated, main-dataset-only).

library(manifestoR)
library(data.table)

setwd("C:/masterarbeit")
mp_setapikey("manifesto_apikey.txt")
mp_load_cache(file = "Data/manifesto_cache.RData")

target_years <- c(1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)

# Expanded party code list, including PDS as a separate code (41222),
# possible SPD variants, and "the usual six".
party_codes_extended <- data.table(
  code  = c(41221, 41222, 41223, 41521, 41420, 41113, 41953, 41320),
  label = c("SPD", "PDS", "DIE LINKE", "CDU_CSU", "FDP", "GRUENE",
            "AfD", "BP_or_other")
)
# 41320 is just included as a sanity placeholder; we'll see if it appears.


# ---- 1. Full availability for all candidate codes ----------------------
avail_full <- as.data.table(mp_availability(countryname == "Germany"))
avail_full[, year := substr(as.character(date), 1, 4)]
target <- avail_full[year %in% as.character(target_years) &
                     party %in% party_codes_extended$code]
target <- merge(target, party_codes_extended,
                by.x = "party", by.y = "code", all.x = TRUE)

cat("=== Full availability matrix for target years ===\n")
cat("Cols: manifestos = PDF/text exists; annotations = CMP-coded;\n")
cat("      translation_en = English-translated coding exists\n\n")
print(target[, .(label, party, date, manifestos, annotations,
                 translation_en, language)][order(date, label)])


# ---- 2. Cross-tab: years where each party has annotations ---------------
cat("\n=== Annotation coverage by (party, year) ===\n")
coverage <- dcast(target, label + party ~ year,
                  value.var = "annotations", fun.aggregate = any)
print(coverage)


# ---- 3. Check the MAIN DATASET (mpds) — different from corpus ----------
# The main dataset has aggregated category percentages even when the
# annotated text isn't in the corpus. This is our fallback for SPD.
cat("\n=== Main dataset (mpds) coverage for target parties/years ===\n")
mpds <- as.data.table(mp_maindataset())
mpds[, year := substr(as.character(date), 1, 4)]
mpds_target <- mpds[year %in% as.character(target_years) &
                    party %in% party_codes_extended$code]
mpds_target <- merge(mpds_target, party_codes_extended,
                     by.x = "party", by.y = "code", all.x = TRUE)

cat("\nRows in main dataset for target grid:", nrow(mpds_target), "\n")
print(mpds_target[, .(label, party, date)][order(date, label)])

# How many of our (party, date) cells does mpds cover that the corpus
# doesn't have annotated?
corpus_cells <- target[annotations == TRUE, .(party, date)]
mpds_cells <- mpds_target[, .(party, date)]
only_in_mpds <- fsetdiff(mpds_cells, corpus_cells)
cat("\n(party, date) cells with main-dataset entry but NO annotated corpus:\n")
only_in_mpds <- merge(only_in_mpds, party_codes_extended,
                      by.x = "party", by.y = "code", all.x = TRUE)
print(only_in_mpds[order(date, label)])


# ---- 4. Sanity: peek at the per-category columns in mpds ---------------
# Main dataset columns "per101", "per102" ... "per706" ARE the 56-class
# percentages (per-thousand or per-hundred depending on version).
per_cols <- grep("^per[0-9]{3}$", names(mpds), value = TRUE)
cat("\nMain dataset has", length(per_cols),
    "per-category columns (sample:",
    paste(head(per_cols), collapse = ", "), "...)\n")

# Verify they sum to ~100 (percentages) for a known SPD entry:
spd_98 <- mpds[party == 41221 & date == 199809]
if (nrow(spd_98) == 1) {
  cat("\nSPD 1998 main-dataset row sum across per-categories:",
      sum(spd_98[, ..per_cols], na.rm = TRUE), "\n")
}
