# ============================================================================
# test_05_logic.R — base-R/data.table logic test for 05_H2.R core algorithms
# (tidyverse helpers can't run in the sandbox, so the polarization-score and
# Dalton formulas are re-implemented in base R to verify the math 05 relies on.)
# ============================================================================
suppressPackageStartupMessages(library(data.table))
PASS <- 0L; FAIL <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { PASS<<-PASS+1L; cat(sprintf("  PASS  %s\n", label)) }
                              else { FAIL<<-FAIL+1L; cat(sprintf("  FAIL  %s\n", label)) } }

# ---- exact jsd + per_bucket_jsd (copied from 02_helpers) --------------------
jsd <- function(p, q) {
  p<-as.numeric(p); q<-as.numeric(q); sp<-sum(p); sq<-sum(q)
  if (sp<=0||sq<=0) return(NA_real_); p<-p/sp; q<-q/sq; m<-0.5*(p+q)
  kl<-function(a,b){keep<-a>0&b>0; sum(a[keep]*log2(a[keep]/b[keep]))}
  0.5*kl(p,m)+0.5*kl(q,m)
}
per_bucket_jsd <- function(p, q) {
  p<-p/sum(p); q<-q/sum(q); m<-0.5*(p+q); out<-numeric(length(p)); names(out)<-names(p)
  for (i in seq_along(p)) { a<-0
    if (p[i]>0 && m[i]>0) a<-a+0.5*p[i]*log2(p[i]/m[i])
    if (q[i]>0 && m[i]>0) a<-a+0.5*q[i]*log2(q[i]/m[i]); out[i]<-a }
  out
}
BUCKETS_A <- paste0("A", 1:9)
vec_of <- function(dt, buckets){ v<-setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)]<-0; as.numeric(v) }

# ============================================================================
# TEST 1 — per-bucket contributions sum to scalar JSD (H2a integrity) + loop
# ============================================================================
cat("\n[TEST 1] H2a per-bucket decomposition sums to JSD\n")
set.seed(1)
s <- runif(9); s <- s/sum(s); names(s) <- BUCKETS_A
m <- runif(9); m <- m/sum(m); names(m) <- BUCKETS_A
ctr <- per_bucket_jsd(s, m)
ok(abs(sum(ctr) - jsd(s, m)) < 1e-12, "single cell: Σ contrib == jsd")
# degenerate: identical distributions => all contributions 0
ok(all(abs(per_bucket_jsd(s, s)) < 1e-12), "identical p==q => all contributions 0")

# H2a loop assembly on synthetic valid cells
mk_long <- function(party, lp, vec) data.table(party=party, lp=lp, bucket=names(vec), share=as.numeric(vec))
mkv <- function(peak){ v<-rep(0.05,9); names(v)<-BUCKETS_A; v[peak]<-0.6; v/sum(v) }
spkA_v <- rbindlist(list(mk_long("P1",19,mkv("A1")), mk_long("P2",19,mkv("A2")), mk_long("P1",20,mkv("A3"))))
manA_v <- rbindlist(list(mk_long("P1",19,mkv("A2")), mk_long("P2",19,mkv("A2")), mk_long("P1",20,mkv("A1"))))
valid  <- unique(spkA_v[, .(party, lp)])
sumchk <- numeric(0); rows <- list()
for (k in seq_len(nrow(valid))) {
  p<-valid$party[k]; L<-valid$lp[k]
  sv<-vec_of(spkA_v[party==p & lp==L], BUCKETS_A); names(sv)<-BUCKETS_A
  mv<-vec_of(manA_v[party==p & lp==L], BUCKETS_A); names(mv)<-BUCKETS_A
  if (sum(sv)<=0||sum(mv)<=0) next
  cc<-per_bucket_jsd(sv,mv)
  rows[[length(rows)+1L]]<-data.table(party=p, lp=L, bucket=names(cc), jsd_contrib=unname(cc))
  sumchk<-c(sumchk, abs(sum(cc)-jsd(sv,mv)))
}
h2a_long <- rbindlist(rows)
ok(max(sumchk) < 1e-12, sprintf("loop: every cell Σ contrib == jsd (max dev %.1e)", max(sumchk)))
ok(nrow(h2a_long) == 3L*9L, "h2a_long has 9 buckets x 3 cells = 27 rows")
ok(all(h2a_long$bucket %in% BUCKETS_A), "all buckets valid")

# ============================================================================
# TEST 2 — polarization score formula (signed within-domain balance, [-1,1])
# ============================================================================
cat("\n[TEST 2] polarization score formula\n")
pol <- function(pos, neg){ tot<-pos+neg; if (tot<=0) NA_real_ else (pos-neg)/tot }
ok(abs(pol(0.3, 0.1) - 0.5) < 1e-12, "(0.3-0.1)/0.4 = 0.5")
ok(pol(0.4, 0) == 1,   "all on positive pole => +1")
ok(pol(0, 0.4) == -1,  "all on negative pole => -1")
ok(is.na(pol(0, 0)),   "zero in-domain mass => NA")
ok(abs(pol(0.25, 0.25)) < 1e-12, "balanced => 0")

# ============================================================================
# TEST 3 — Dalton-weighted dispersion + all-vs-excl-AfD logic (H2c)
# ============================================================================
cat("\n[TEST 3] Dalton dispersion + AfD-exclusion\n")
dalton <- function(positions, weights){
  okp <- !is.na(positions) & !is.na(weights) & weights > 0
  if (sum(okp) < 2L) return(c(mean_pos=NA, polarization=NA, n=sum(okp)))
  p<-positions[okp]; w<-weights[okp]; w<-w/sum(w); mu<-sum(w*p); s<-sqrt(sum(w*(p-mu)^2))
  c(mean_pos=mu, polarization=unname(s), n=sum(okp))
}
# two parties at +1 / -1, equal weight => mu=0, sigma=1
ok(abs(dalton(c(1,-1), c(0.5,0.5))["polarization"] - 1) < 1e-12, "two poles, equal weight => SD=1")
# weighted mean shifts with weights
d2 <- dalton(c(1,-1), c(0.9,0.1)); ok(abs(d2["mean_pos"] - 0.8) < 1e-12, "weighted mean = 0.8")
# <2 valid => NA
ok(is.na(dalton(c(1, NA), c(0.5,0.5))["polarization"]), "<2 valid => NA")

# AfD-exclusion: synthetic (lp, domain, party, pol, weight) with AfD as outlier
dt <- data.table(
  lp = 19L, domain = "Migration",
  party = c("CDU/CSU","SPD","Grüne","AfD"),
  pol   = c(0.10, -0.05, -0.20, 0.95),   # AfD far out
  w     = c(0.30, 0.25, 0.20, 0.25))
disp_all   <- dt[, dalton(pol, w)["polarization"], by=.(lp,domain)]$V1
disp_noafd <- dt[party!="AfD", dalton(pol, w)["polarization"], by=.(lp,domain)]$V1
ok(is.finite(disp_all) && is.finite(disp_noafd), "both dispersions finite")
ok(disp_noafd < disp_all, sprintf("excl-AfD dispersion < all (%.3f < %.3f) — AfD inflates spread", disp_noafd, disp_all))

# ============================================================================
# TEST 4 — valid-cell restriction excludes FDP LP18 (H2b semi_join logic)
# ============================================================================
cat("\n[TEST 4] valid-cell restriction (FDP LP18 excluded from H2b)\n")
speech_cells <- data.table(party=c("FDP","SPD","FDP"), lp=c(18L,19L,19L))
valid_cells  <- data.table(party=c("SPD","FDP"),       lp=c(19L,19L))   # no FDP LP18
restricted   <- speech_cells[valid_cells, on=.(party,lp), nomatch=0]
ok(nrow(restricted[party=="FDP" & lp==18L]) == 0L, "FDP LP18 removed by valid-cell restriction")
ok(nrow(restricted) == 2L, "only the 2 valid cells remain")

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
