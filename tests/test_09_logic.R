# ============================================================================
# test_09_logic.R — base-R/data.table logic test for 09 core algorithms
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
BUCKETS_A <- paste0("A", 1:9)
DPS <- "A5"   # stand-in for "Democracy & Political System"

# ============================================================================
# TEST 1 — hard-A threshold: drop Andere (below-threshold) mass + renorm
# ============================================================================
cat("\n[TEST 1] hard-A Andere-drop + renorm\n")
hard_cell <- data.table(party="P1", lp=19L, bucket=c(BUCKETS_A, "Andere"),
                        share=c(0.30, rep(0.05, 8), 0.30))  # 0.30+0.40+0.30 = 1.0
sp <- hard_cell[bucket %in% BUCKETS_A]
sp[, share := if (sum(share) > 0) share/sum(share) else 0, by=.(party,lp)]
ok(!("Andere" %in% sp$bucket), "Andere (below-threshold) dropped")
ok(abs(sum(sp$share) - 1) < 1e-12, "remaining 9 A-buckets renormalize to 1")
ok(nrow(sp) == 9L, "9 A-buckets remain")
# JSD vs a manifesto distribution
man <- data.table(bucket=BUCKETS_A, share=c(0.05,0.05,0.10,0.10,0.40,0.10,0.05,0.10,0.05))
s <- vec_of(sp, BUCKETS_A); m <- vec_of(man, BUCKETS_A)
ok(is.finite(jsd(s, m)) && jsd(s,m) >= 0 && jsd(s,m) <= 1, "JSD finite and in [0,1]")

# ============================================================================
# TEST 2 — DPS attribution: total == jsd, dps_share in [0,1]
# ============================================================================
cat("\n[TEST 2] DPS attribution\n")
sv <- setNames(s, BUCKETS_A); mv <- setNames(m, BUCKETS_A)
cc <- per_bucket_jsd(sv, mv)
total <- sum(cc); dps_contrib <- unname(cc[DPS])
ok(abs(total - jsd(sv, mv)) < 1e-12, "Σ per-bucket contrib == jsd")
ok(all(cc >= -1e-15), "all per-bucket contributions non-negative")
dps_share <- dps_contrib / total
ok(dps_share >= 0 && dps_share <= 1, sprintf("DPS share in [0,1] (got %.3f)", dps_share))

# ============================================================================
# TEST 3 — soft vs hard per-cell JSD, shift, and Spearman ordering
# ============================================================================
cat("\n[TEST 3] soft-vs-hard shift + Spearman\n")
# synthetic per-cell JSDs for 4 cells under 3 specs (hard >= soft, order preserved)
h <- data.table(
  party = rep(c("P1","P1","P2","P2"), 3),
  lp    = rep(c(19L,20L,19L,20L), 3),
  spec  = rep(c("soft_tau1.0","hard_thr0.4","hard_thr0.5"), each = 4),
  jsd   = c(0.10,0.20,0.30,0.40,   # soft
            0.14,0.24,0.34,0.44,   # hard0.4 (soft + 0.04, order preserved)
            0.17,0.27,0.37,0.47))  # hard0.5 (soft + 0.07)
w <- dcast(h, party + lp ~ spec, value.var = "jsd")
ok(abs(cor(w$soft_tau1.0, w$hard_thr0.4, method="spearman") - 1) < 1e-12, "soft~hard0.4 ordering preserved (rho=1)")
ok(abs(cor(w$soft_tau1.0, w$hard_thr0.5, method="spearman") - 1) < 1e-12, "soft~hard0.5 ordering preserved (rho=1)")
# per-party shift
ps <- w[, .(mean_delta_h04 = mean(hard_thr0.4 - soft_tau1.0),
            mean_delta_h05 = mean(hard_thr0.5 - soft_tau1.0)), by=party]
ok(all(abs(ps$mean_delta_h04 - 0.04) < 1e-12), "per-party mean delta (hard0.4 - soft) = 0.04")
ok(all(abs(ps$mean_delta_h05 - 0.07) < 1e-12), "per-party mean delta (hard0.5 - soft) = 0.07")
ok(all(ps$mean_delta_h05 > ps$mean_delta_h04), "higher threshold shifts JSD further from soft")

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
