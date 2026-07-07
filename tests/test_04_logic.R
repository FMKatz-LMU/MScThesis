# ============================================================================
# test_04_logic.R — base-R/data.table logic test for 04_H1.R core algorithms
# Runs WITHOUT tidyverse/ggplot/arrow: builds synthetic caches and replicates
# the exact computations from 04 (vec_of, pairwise, nearest, within_domain,
# cross-checks), asserting expected results. Includes a deliberately corrupted
# cross-check case to confirm the discrepancy is detected.
# ============================================================================
suppressPackageStartupMessages(library(data.table))

PASS <- 0L; FAIL <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  PASS  %s\n", label)) }
  else              { FAIL <<- FAIL + 1L; cat(sprintf("  FAIL  %s\n", label)) }
}

# ---- exact jsd (copied verbatim from 02_helpers) ---------------------------
jsd <- function(p, q) {
  stopifnot(length(p) == length(q))
  p <- as.numeric(p); q <- as.numeric(q)
  sp <- sum(p); sq <- sum(q)
  if (sp <= 0 || sq <= 0) return(NA_real_)
  p <- p / sp; q <- q / sq
  m <- 0.5 * (p + q)
  kl <- function(a, b) { keep <- a > 0 & b > 0; sum(a[keep] * log2(a[keep] / b[keep])) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}

# ---- constants (self-contained mock; algorithm-agnostic to specific names) --
BUCKETS_A <- paste0("A", 1:9)
DOMAINS_B <- c("Economy", "Welfare", "Migration", "Europe")
BUCKETS_B <- c("Marktliberalismus","Staatsintervention","Wirtschaft Allgemein",
               "Sozialstaat Ausbau","Sozialstaat Begrenzung",
               "Migration restriktiv","Migration liberal","Pro-EU","Contra-EU")
AGG_B <- data.table(bucket_B = BUCKETS_B, domain_B = c(
  "Economy","Economy","Economy","Welfare","Welfare","Migration","Migration","Europe","Europe"))
LP_ELECTION <- data.table(lp = c(19L, 20L), election_date = c("2017-09-24", "2021-09-26"))
N_SENT_MIN <- 1000L

# ---- functions copied verbatim from 04 -------------------------------------
vec_of <- function(dt, buckets) { v <- setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)] <- 0; as.numeric(v) }

pairwise <- function(dist_dt, side) {
  out <- list()
  for (L in sort(unique(dist_dt$lp))) {
    sub <- dist_dt[lp == L]; ps <- sort(unique(sub$party))
    if (length(ps) < 2L) next
    vl <- lapply(ps, function(p) vec_of(sub[party == p], BUCKETS_A)); names(vl) <- ps
    for (a in 1:(length(ps) - 1L)) for (b in (a + 1L):length(ps))
      out[[length(out) + 1L]] <- data.table(lp = L, side = side,
        party_i = ps[a], party_j = ps[b], jsd = jsd(vl[[a]], vl[[b]]))
  }
  rbindlist(out)
}

b_lookup <- AGG_B[, .(domain_B, bucket_B)] |> unique()
within_domain <- function(dt, group_cols) {
  d <- merge(dt[bucket %in% BUCKETS_B], b_lookup, by.x = "bucket", by.y = "bucket_B")
  setnames(d, c("domain_B", "bucket"), c("domain", "sub_bucket"))
  d[, share_within_domain := if (sum(share) > 0) share / sum(share) else 0, by = c(group_cols, "domain")]
  d
}
b_renorm <- function(dt, key) {
  d <- dt[bucket != "Andere"]
  d[, share_r := if (sum(share) > 0) share / sum(share) else 0, by = key]
  d
}

# ============================================================================
# Build synthetic caches
# ============================================================================
make_A <- function(peak, hi = 0.6) { v <- rep((1 - hi) / 8, 9); names(v) <- BUCKETS_A; v[peak] <- hi; v }
A_to_long <- function(party, lp, vec) data.table(party = party, lp = lp, bucket = names(vec), share = as.numeric(vec))

# speech A excl: P2@19 peaks A3 (matches P3's manifesto, NOT its own) -> own not nearest
spk_peaks <- list("P1|19"="A1","P2|19"="A3","P3|19"="A3","P1|20"="A1","P2|20"="A2","P3|20"="A3")
# manifesto A excl (M1): each party peaks its own slot
man_peaks <- list("P1|2017-09-24"="A1","P2|2017-09-24"="A2","P3|2017-09-24"="A3",
                  "P1|2021-09-26"="A1","P2|2021-09-26"="A2","P3|2021-09-26"="A3")

spkA <- rbindlist(lapply(names(spk_peaks), function(k){
  pr <- strsplit(k,"|",fixed=TRUE)[[1]]; A_to_long(pr[1], as.integer(pr[2]), make_A(spk_peaks[[k]])) }))
spkA[, n_sent := 5000L]
manA <- rbindlist(lapply(names(man_peaks), function(k){
  pr <- strsplit(k,"|",fixed=TRUE)[[1]]
  v <- make_A(man_peaks[[k]]); data.table(party=pr[1], election_date=pr[2], bucket=names(v), share=as.numeric(v)) }))
manA <- merge(manA, LP_ELECTION, by = "election_date")

# bootstrap_ci is_primary: jsd_point = jsd(speech cell, own manifesto) BY CONSTRUCTION
cells <- unique(spkA[, .(party, lp)])
ci <- copy(cells)
ci[, `:=`(filter="filtered", code305="excl", is_primary=TRUE, n_sent=5000L)]
ci$jsd_point <- vapply(seq_len(nrow(ci)), function(i){
  p <- ci$party[i]; L <- ci$lp[i]
  jsd(vec_of(spkA[party==p & lp==L], BUCKETS_A),
      vec_of(manA[party==p & lp==L], BUCKETS_A)) }, numeric(1))
ci[, `:=`(lo95 = pmax(0, jsd_point - 0.01), hi95 = jsd_point + 0.01)]

# ============================================================================
# TEST 1 — H1a table assembly + integrity
# ============================================================================
cat("\n[TEST 1] H1a assembly\n")
le <- LP_ELECTION
h1a <- ci[is_primary == TRUE]
h1a_tbl <- merge(h1a[, .(party, lp, jsd = jsd_point, lo95, hi95, n_sent)], le, by = "lp")[
  , .(party, lp, election_date, jsd, lo95, hi95, n_sent, manifesto_present = TRUE)]
setorder(h1a_tbl, party, lp)
ok(nrow(h1a_tbl) == 6L, "6 valid cells")
ok(all(h1a_tbl$lo95 <= h1a_tbl$jsd + 1e-9 & h1a_tbl$jsd <= h1a_tbl$hi95 + 1e-9), "CIs bracket point")
ok(all(h1a_tbl$jsd >= 0 & h1a_tbl$jsd <= 1), "JSD in [0,1]")

# ============================================================================
# TEST 2 — cross-check (correct: should agree) + corrupted (should be detected)
# ============================================================================
cat("\n[TEST 2] speech_soft vs bootstrap cross-check\n")
recompute_xcheck <- function(boot_tbl) {
  x <- boot_tbl[, .(party, lp, jsd_boot = jsd)]
  x[, jsd_ss := NA_real_]
  for (k in seq_len(nrow(x))) {
    sv <- vec_of(spkA[party == x$party[k] & lp == x$lp[k]], BUCKETS_A)
    mv <- vec_of(manA[party == x$party[k] & lp == x$lp[k]], BUCKETS_A)
    if (sum(sv) > 0 && sum(mv) > 0) x$jsd_ss[k] <- jsd(sv, mv)
  }
  max(abs(x$jsd_ss - x$jsd_boot), na.rm = TRUE)
}
maxd_good <- recompute_xcheck(h1a_tbl)
ok(maxd_good < 1e-6, sprintf("correct case agrees (maxd = %.2e)", maxd_good))

h1a_bad <- copy(h1a_tbl); h1a_bad$jsd[2] <- h1a_bad$jsd[2] + 0.1   # corrupt one cell
maxd_bad <- recompute_xcheck(h1a_bad)
ok(maxd_bad > 1e-6, sprintf("corrupted case is detected (maxd = %.2e > 1e-6)", maxd_bad))

# ============================================================================
# TEST 3 — benchmark pairwise (counts, bounds, symmetry)
# ============================================================================
cat("\n[TEST 3] benchmark pairwise reference scale\n")
valid <- h1a_tbl[, .(party, lp)]
spkA_v <- merge(spkA, valid, by = c("party","lp"))
manA_v <- merge(manA, valid, by = c("party","lp"))
ref <- rbindlist(list(pairwise(spkA_v, "speech"), pairwise(manA_v, "manifesto")))
ok(nrow(ref) == 12L, "3 parties x 2 LPs x 2 sides = 12 pairwise JSDs (3 pairs/LP)")
ok(all(ref$jsd >= 0 & ref$jsd <= 1), "all between-party JSDs in [0,1]")
# symmetry spot-check
v1 <- vec_of(spkA_v[party=="P1" & lp==19], BUCKETS_A); v2 <- vec_of(spkA_v[party=="P2" & lp==19], BUCKETS_A)
ok(abs(jsd(v1,v2) - jsd(v2,v1)) < 1e-12, "JSD symmetric")

# ============================================================================
# TEST 4 — nearest-manifesto (own-nearest logic, rank, cross-check 2)
# ============================================================================
cat("\n[TEST 4] nearest-manifesto\n")
near <- list()
for (k in seq_len(nrow(valid))) {
  p <- valid$party[k]; L <- valid$lp[k]
  sv <- vec_of(spkA_v[party == p & lp == L], BUCKETS_A)
  qs <- sort(unique(manA_v[lp == L, party]))
  if (!length(qs) || sum(sv) <= 0) next
  d <- vapply(qs, function(q) jsd(sv, vec_of(manA_v[party == q & lp == L], BUCKETS_A)), numeric(1))
  own_in <- p %in% qs
  near[[length(near)+1L]] <- data.table(party=p, lp=L,
    jsd_own = if (own_in) unname(d[match(p, qs)]) else NA_real_,
    jsd_min = min(d), nearest_party = qs[which.min(d)],
    rank_own = if (own_in) as.integer(rank(d, ties.method="min")[match(p, qs)]) else NA_integer_,
    n_manifestos = length(qs), own_is_nearest = own_in && (qs[which.min(d)] == p))
}
near <- rbindlist(near)
ok(near[party=="P1" & lp==19]$own_is_nearest == TRUE,  "P1@19 own IS nearest")
ok(near[party=="P2" & lp==19]$own_is_nearest == FALSE, "P2@19 own is NOT nearest (designed)")
ok(near[party=="P2" & lp==19]$nearest_party == "P3",   "P2@19 nearest manifesto = P3 (designed)")
ok(near[party=="P2" & lp==19]$rank_own >= 2L,          "P2@19 own rank >= 2")
ok(near[party=="P3" & lp==19]$own_is_nearest == TRUE,  "P3@19 own IS nearest")
# cross-check 2: nearest own-JSD == H1a point
nn <- merge(near[, .(party, lp, jsd_own)], h1a_tbl[, .(party, lp, jsd)], by = c("party","lp"))
ok(max(abs(nn$jsd_own - nn$jsd), na.rm=TRUE) < 1e-9, "nearest own-JSD == H1a point")

# ============================================================================
# TEST 5 — within-domain composition (party in grouping => sums to 1 per cell)
# ============================================================================
cat("\n[TEST 5] within-domain composition\n")
# synthetic B distributions: 9 directional + Andere, summing to 1 per (party,lp)
mkB <- function(party, lp, ed=NA) {
  base <- setNames(c(0.10,0.05,0.05, 0.15,0.05, 0.10,0.05, 0.05,0.05), BUCKETS_B)
  dt <- data.table(party=party, lp=lp, bucket=c(BUCKETS_B,"Andere"),
                   share=c(as.numeric(base), 1 - sum(base)))
  if (!is.na(ed)) dt[, election_date := ed]
  dt
}
spkB <- rbindlist(lapply(1:nrow(cells), function(i) mkB(cells$party[i], cells$lp[i])))
manB <- rbindlist(lapply(1:nrow(cells), function(i) {
  ed <- LP_ELECTION[lp==cells$lp[i], election_date]; mkB(cells$party[i], cells$lp[i], ed) }))

dir <- within_domain(spkB, c("party","lp"))
sums <- dir[, .(s = sum(share_within_domain)), by = .(party, lp, domain)]
ok(all(abs(sums$s - 1) < 1e-9), "every (party,lp,domain) composition sums to 1")
# sanity: with party dropped, the buggy grouping would NOT sum to 1 per party
dir_bug <- copy(spkB[bucket %in% BUCKETS_B])
dir_bug <- merge(dir_bug, b_lookup, by.x="bucket", by.y="bucket_B"); setnames(dir_bug, "domain_B", "domain")
dir_bug[, swd := if (sum(share) > 0) share / sum(share) else 0, by = c("lp","domain")]   # buggy: no party
bug_sums <- dir_bug[, .(s = sum(swd)), by = .(party, lp, domain)]
ok(any(abs(bug_sums$s - 1) > 1e-6), "buggy (party-less) grouping would NOT sum to 1 (confirms the fix matters)")

# overall B-JSD
sB <- b_renorm(spkB, c("party","lp"))[, .(party, lp, bucket, s = share_r)]
mB <- b_renorm(manB, c("party","lp"))[, .(party, lp, bucket, m = share_r)]
h1b_jsd <- merge(sB, mB, by=c("party","lp","bucket"))[, .(jsd_B = jsd(s, m)), by = .(party, lp)]
ok(nrow(h1b_jsd) == 6L && all(h1b_jsd$jsd_B >= -1e-12), "overall B-JSD computed for all 6 cells (>=0)")

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
