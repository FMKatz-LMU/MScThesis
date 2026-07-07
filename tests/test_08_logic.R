# ============================================================================
# test_08_logic.R — base-R/data.table logic test for 08 core algorithms
# ============================================================================
suppressPackageStartupMessages(library(data.table))
PASS <- 0L; FAIL <- 0L
ok <- function(cond, label){ if (isTRUE(cond)){PASS<<-PASS+1L; cat(sprintf("  PASS  %s\n",label))}
                             else {FAIL<<-FAIL+1L; cat(sprintf("  FAIL  %s\n",label))} }
jsd <- function(p, q){ p<-as.numeric(p); q<-as.numeric(q); sp<-sum(p); sq<-sum(q)
  if (sp<=0||sq<=0) return(NA_real_); p<-p/sp; q<-q/sq; m<-0.5*(p+q)
  kl<-function(a,b){k<-a>0&b>0; sum(a[k]*log2(a[k]/b[k]))}; 0.5*kl(p,m)+0.5*kl(q,m) }
vec_of <- function(dt, buckets){ v<-setNames(dt$share, dt$bucket)[buckets]; v[is.na(v)]<-0; as.numeric(v) }
BUCKETS_A <- paste0("A", 1:9)
TAU_ORDER <- c("sharp","native","flat")

# ============================================================================
# TEST 1 — per-tau sweep loop produces one JSD per (tau, cell)
# ============================================================================
cat("\n[TEST 1] per-tau sweep loop\n")
mk <- function(party, lp, tn, peak, hi){ v<-rep((1-hi)/8,9); names(v)<-BUCKETS_A; v[peak]<-hi
  data.table(party=party, lp=lp, tau_name=tn, bucket=BUCKETS_A, share=as.numeric(v)) }
# same cells, slightly different sharpness per tau (sharper hi for 'sharp')
ss <- rbindlist(list(
  mk("P1",19,"sharp","A1",0.75), mk("P2",19,"sharp","A2",0.75),
  mk("P1",19,"native","A1",0.60), mk("P2",19,"native","A2",0.60),
  mk("P1",19,"flat","A1",0.45), mk("P2",19,"flat","A2",0.45)))
manA <- rbindlist(list(mk("P1",19,"_","A2",0.60)[, .(party,lp,bucket,share)],
                       mk("P2",19,"_","A2",0.60)[, .(party,lp,bucket,share)]))
valid <- data.table(party=c("P1","P2"), lp=c(19L,19L))
out <- list()
for (tn in TAU_ORDER) {
  sp <- merge(ss[tau_name==tn, .(party,lp,bucket,share)], valid, by=c("party","lp"))
  cells <- unique(sp[, .(party,lp)])
  for (k in seq_len(nrow(cells))) { p<-cells$party[k]; L<-cells$lp[k]
    sv<-vec_of(sp[party==p&lp==L], BUCKETS_A); mv<-vec_of(manA[party==p&lp==L], BUCKETS_A)
    out[[length(out)+1L]]<-data.table(tau_name=tn, party=p, lp=L, jsd=jsd(sv,mv)) }
}
sweep <- rbindlist(out)
ok(nrow(sweep) == 6L, "3 taus x 2 cells = 6 JSD rows")
ok(all(sweep$jsd >= 0 & sweep$jsd <= 1), "all JSD in [0,1]")
# P1: speech peaks A1, manifesto peaks A2 -> JSD should decrease as speech flattens toward uniform? 
# At least: sharp (most peaked, furthest from A2-peaked manifesto) >= flat for P1
ok(sweep[tau_name=="sharp" & party=="P1", jsd] >= sweep[tau_name=="flat" & party=="P1", jsd] - 1e-9,
   "P1: sharper speech is at least as far from the (A2-peaked) manifesto as flatter")

# ============================================================================
# TEST 2 — Spearman rank correlation logic (order-preserving=1, reversed=-1)
# ============================================================================
cat("\n[TEST 2] Spearman rho\n")
sw <- data.table(
  party = rep(c("P1","P2","P3","P4"), 3),
  lp    = rep(19L, 12),
  tau_name = rep(TAU_ORDER, each = 4),
  jsd = c(0.10,0.20,0.30,0.40,    # sharp
          0.11,0.22,0.33,0.44,    # native (same order as sharp)
          0.40,0.30,0.20,0.10))   # flat (reversed)
w <- dcast(sw, party + lp ~ tau_name, value.var = "jsd")
w <- w[complete.cases(w[, ..TAU_ORDER])]
rho_sn <- cor(w$sharp, w$native, method="spearman")
rho_sf <- cor(w$sharp, w$flat,   method="spearman")
rho_nf <- cor(w$native, w$flat,  method="spearman")
ok(abs(rho_sn - 1)  < 1e-12, sprintf("sharp~native order-preserving => rho=1 (got %.3f)", rho_sn))
ok(abs(rho_sf + 1)  < 1e-12, sprintf("sharp~flat reversed => rho=-1 (got %.3f)", rho_sf))
ok(abs(rho_nf + 1)  < 1e-12, sprintf("native~flat reversed => rho=-1 (got %.3f)", rho_nf))
ok(nrow(w) == 4L, "complete.cases keeps all 4 cells")

# ============================================================================
# TEST 3 — Andere exclusion + within-cell renorm (H1b panel)
# ============================================================================
cat("\n[TEST 3] Andere exclusion + renorm\n")
BUCKETS_B <- paste0("B", 1:9)
cell <- data.table(party="P1", lp=19L, bucket=c(BUCKETS_B,"Andere"),
                   share=c(rep(0.7/9, 9), 0.3))
ren <- cell[bucket != "Andere"][, share := if (sum(share)>0) share/sum(share) else 0, by=.(party,lp)]
ok(!("Andere" %in% ren$bucket), "Andere removed")
ok(abs(sum(ren$share) - 1) < 1e-12, "remaining 9 directional buckets renormalize to 1")
ok(nrow(ren) == 9L, "exactly 9 directional buckets remain")

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
