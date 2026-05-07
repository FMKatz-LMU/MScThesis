# diagnose_marpor.R  (v2)
# -----------------------
# Inspects the actual schema of mp_availability() and mp_corpus_df() output,
# then reports per-(party, date) coverage.

library(manifestoR)
library(data.table)

setwd("C:/masterarbeit")
mp_setapikey("manifesto_apikey.txt")
mp_load_cache(file = "Data/manifesto_cache.RData")

target_years <- c(1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)
target_parties <- c(41221, 41521, 41420, 41113, 41223, 41953)

# ---- 1. Inspect availability schema -----------------------------------
avail_full <- as.data.table(mp_availability(countryname == "Germany"))
cat("=== mp_availability() columns ===\n")
print(names(avail_full))
cat("\n=== first 3 rows ===\n")
print(head(avail_full, 3))

# Pick whatever date column actually exists.
date_col <- intersect(c("date", "edate", "election_date"), names(avail_full))[1]
cat("\nUsing date column:", date_col, "\n")

avail_full[, year := substr(as.character(get(date_col)), 1, 4)]
target <- avail_full[year %in% as.character(target_years) &
                     party %in% target_parties]

cat("\n=== Target (party, date) cells: ===\n")
print_cols <- intersect(c("party", "partyname", "party_name",
                          date_col, "annotations", "manifesto_id"),
                        names(target))
print(target[, ..print_cols][order(get(date_col), party)])

cat("\nAnnotated rows in target grid:",
    sum(target$annotations, na.rm = TRUE),
    "of", nrow(target), "\n")


# ---- 2. What's actually in the corpus pull ----------------------------
in_cache <- as.data.table(mp_corpus_df(
  countryname == "Germany" & party %in% target_parties
))

cat("\n=== mp_corpus_df() columns ===\n")
print(names(in_cache))

in_cache_summary <- unique(in_cache[, .(party, date)])
in_cache_summary[, year := substr(as.character(date), 1, 4)]

cat("\n=== Unique (party, date) in cache for target years ===\n")
print(in_cache_summary[year %in% as.character(target_years)][order(date, party)])

cat("\nNumber of (party, date) manifestos in cache for target years:",
    nrow(in_cache_summary[year %in% as.character(target_years)]), "\n")


# ---- 3. SPD-specific drilldown ----------------------------------------
cat("\n=== SPD (41221) availability across all dates ===\n")
spd_avail <- avail_full[party == 41221]
print(spd_avail[, intersect(names(spd_avail),
                            c("party", date_col, "annotations",
                              "manifesto_id", "language")), with = FALSE][
                            order(get(date_col))])
