# ============================================================================
# test_10_logic.R — base-R/data.table logic test for 10 core algorithms
# ============================================================================
suppressPackageStartupMessages(library(data.table))
PASS <- 0L; FAIL <- 0L
ok <- function(cond, label){ if (isTRUE(cond)){PASS<<-PASS+1L; cat(sprintf("  PASS  %s\n",label))}
                             else {FAIL<<-FAIL+1L; cat(sprintf("  FAIL  %s\n",label))} }
jsd <- function(p, q){ p<-as.numeric(p); q<-as.numeric(q); sp<-sum(p); sq<-sum(q)
  if (sp<=0||sq<=0) return(NA_real_); p<-p/sp; q<-q/sq; m<-0.5*(p+q)
  kl<-function(a,b){k<-a>0&b>0; sum(a[k]*log2(a[k]/b[k]))}; 0.5*kl(p,m)+0.5*kl(q,m) }
per_bucket_jsd <- function(p, q){ p<-p/sum(p); q<-q/sum(q); m<-0.5*(p+q); out<-numeric(length(p)); names(out)<-names(p)
  for (i in seq_along(p)){ a<-0; if (p[i]>0&&m[i]>0) a<-a+0.5*p[i]*log2(p[i]/m[i])
    if (q[i]>0&&m[i]>0) a<-a+0.5*q[i]*log2(q[i]/m[i]); out[i]<-a }; out }
vec_of <- function(dt, buckets){ v<-setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)]<-0; as.numeric(v) }
BUCKETS_A <- paste0("A", 1:9); DPS <- "A9"
mklong <- function(f, c305, peakval){ # build a speech cell with DPS(=A9) mass = peakval
  rest <- (1 - peakval)/8; v <- c(rep(rest,8), peakval); names(v) <- BUCKETS_A
  data.table(filter=f, code305=c305, party="P1", lp=19L, bucket=BUCKETS_A, share=as.numeric(v)) }

# synthetic speech across the 2x2 grid: DPS mass high in incl/unfiltered, low in excl/filtered
ss <- rbindlist(list(
  mklong("unfiltered","incl", 0.40),
  mklong("filtered","incl",   0.25),
  mklong("unfiltered","excl", 0.12),
  mklong("filtered","excl",   0.08)))
manI <- data.table(party="P1", lp=19L, bucket=BUCKETS_A, share=c(rep(0.9/8,8), 0.10)) # genuine 305 sockel 0.10
manE <- data.table(party="P1", lp=19L, bucket=BUCKETS_A, share=c(rep(1/8,8), 0.0))     # 305 removed
valid <- data.table(party="P1", lp=19L)
COMBOS <- CJ(filter=c("unfiltered","filtered"), code305=c("incl","excl"), sorted=FALSE)

# ============================================================================
# TEST 1 — combo loop: Σ contrib == total == jsd; build totals + DPS table
# ============================================================================
cat("\n[TEST 1] 2x2 combo decomposition\n")
long_rows<-list(); total_rows<-list(); sumdev<-numeric(0)
for (j in seq_len(nrow(COMBOS))) {
  f<-COMBOS$filter[j]; c305<-COMBOS$code305[j]
  sp <- merge(ss[filter==f & code305==c305, .(party,lp,bucket,share)], valid, by=c("party","lp"))
  man <- if (c305=="incl") manI else manE
  s <- vec_of(sp, BUCKETS_A); names(s)<-BUCKETS_A
  m <- vec_of(man, BUCKETS_A); names(m)<-BUCKETS_A
  cc <- per_bucket_jsd(s,m); tot<-sum(cc); jj<-jsd(s,m)
  sumdev <- c(sumdev, abs(tot-jj))
  long_rows[[j]]  <- data.table(filter=f, code305=c305, bucket=names(cc), contrib=unname(cc))
  total_rows[[j]] <- data.table(filter=f, code305=c305, party="P1", lp=19L, total_jsd=tot)
}
inter_long  <- rbindlist(long_rows); inter_total <- rbindlist(total_rows)
ok(max(sumdev) < 1e-12, sprintf("every combo: Σ contrib == jsd (max dev %.1e)", max(sumdev)))
ok(nrow(inter_long) == 4L*9L, "4 combos x 9 buckets = 36 long rows")

# ============================================================================
# TEST 2 — bootstrap band cross-check matching (right band picked)
# ============================================================================
cat("\n[TEST 2] bootstrap band matching\n")
# synthetic ci: jsd_point set EXACTLY to each combo total -> cross-check dev ~ 0
ci <- merge(inter_total, COMBOS, by=c("filter","code305"))
setnames(ci, "total_jsd", "jsd_point")
xcheck <- numeric(0)
for (j in seq_len(nrow(inter_total))) {
  r <- inter_total[j]
  bp <- ci[filter==r$filter & code305==r$code305 & party==r$party & lp==r$lp, jsd_point]
  if (length(bp)==1L) xcheck <- c(xcheck, abs(r$total_jsd - bp))
}
ok(length(xcheck)==4L, "all 4 combos matched to a band")
ok(max(xcheck) < 1e-12, "matched band jsd_point == combo total")
# wrong-key guard: a band with mismatched filter must NOT match
bad <- ci[filter=="filtered" & code305=="incl" & party=="P1" & lp==20L, jsd_point]  # lp 20 absent
ok(length(bad)==0L, "non-existent (filter,code305,party,lp) yields no match")

# ============================================================================
# TEST 3 — DPS delta ARITHMETIC (dcast + subtraction), share bounds
# (the empirical direction "excl reduces DPS" is a property of the real,
#  symmetrically de-305'd cache data verified at runtime, not of this code;
#  here we verify the script computes the deltas correctly given contributions.)
# ============================================================================
cat("\n[TEST 3] DPS delta arithmetic + share bounds\n")
dps_summary <- data.table(
  filter      = c("unfiltered","unfiltered","filtered","filtered"),
  code305     = c("incl","excl","incl","excl"),
  dps_contrib = c(0.070, 0.030, 0.040, 0.015),   # known (illustrate incl>excl, unfilt>filt)
  total_jsd   = c(0.200, 0.120, 0.150, 0.100))
dps_summary[, dps_share := fifelse(total_jsd > 0, dps_contrib/total_jsd, NA_real_)]

wide_dps <- dcast(dps_summary, filter ~ code305, value.var = "dps_contrib")
wide_dps[, delta_incl_to_excl := excl - incl]
ok(abs(wide_dps[filter=="unfiltered", delta_incl_to_excl] - (-0.040)) < 1e-12, "delta_incl_to_excl (unfiltered) = excl-incl = -0.040")
ok(abs(wide_dps[filter=="filtered",   delta_incl_to_excl] - (-0.025)) < 1e-12, "delta_incl_to_excl (filtered) = excl-incl = -0.025")

across_filter <- dcast(dps_summary, code305 ~ filter, value.var = "dps_contrib")
across_filter[, delta_unfilt_to_filt := filtered - unfiltered]
ok(abs(across_filter[code305=="incl", delta_unfilt_to_filt] - (-0.030)) < 1e-12, "delta_unfilt_to_filt (incl) = filtered-unfiltered = -0.030")
ok(abs(across_filter[code305=="excl", delta_unfilt_to_filt] - (-0.015)) < 1e-12, "delta_unfilt_to_filt (excl) = filtered-unfiltered = -0.015")

ok(all(dps_summary$dps_share >= 0 & dps_summary$dps_share <= 1), "DPS share in [0,1] for all combos")
ok(abs(dps_summary[filter=="unfiltered" & code305=="incl", dps_share] - 0.35) < 1e-12, "dps_share = 0.070/0.200 = 0.35")

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
