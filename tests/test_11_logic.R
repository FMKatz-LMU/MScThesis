# ============================================================================
# test_11_logic.R — base-R/data.table logic test for 11 core math
# (the parquet streaming runs on the real machine; here we test the
#  decomposition / asymmetry / verdict logic on synthetic per-code data)
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

# ---- synthetic 13-code layout: 9 DPS codes (incl 305) + 4 others in 2 buckets
CODES <- c(201,202,203,204,301,302,303,304,305, 601,602, 701,702)
code_to_A <- c(rep("DPS",9), "Migration","Migration", "Welfare","Welfare")
names(code_to_A) <- as.character(CODES)
CODE_305 <- 305L
DPS_BUCKET <- "DPS"; BUCKETS_A <- c("DPS","Migration","Welfare")
DPS_codes <- CODES[code_to_A == DPS_BUCKET]                 # 9
other_buckets <- setdiff(BUCKETS_A, DPS_BUCKET)             # 2
labels17 <- c(paste0("code_", DPS_codes), other_buckets)    # 11 here
dps_idx  <- match(DPS_codes, CODES)
other_idx <- lapply(other_buckets, function(b) which(code_to_A == b)); names(other_idx) <- other_buckets
to17 <- function(v) setNames(c(v[dps_idx], vapply(other_idx, function(ix) sum(v[ix]), 0.0)), labels17)

# ============================================================================
# TEST 1 — to17 partition: mass preservation + correct structure
# ============================================================================
cat("\n[TEST 1] to17 partition\n")
set.seed(1); v <- runif(13); v <- v/sum(v)
e <- to17(v)
ok(length(e) == 11L, "partition has 9 DPS codes + 2 other buckets = 11 elements")
ok(abs(sum(e) - 1) < 1e-12, "to17 preserves total mass (sum = 1)")
ok(abs(unname(e["code_305"]) - v[which(CODES==305)]) < 1e-12, "code_305 element == raw 305 share")
ok(abs(unname(e["Migration"]) - (v[which(CODES==601)] + v[which(CODES==602)])) < 1e-12, "Migration element == sum of its codes")
ok(abs(unname(e["Welfare"])   - (v[which(CODES==701)] + v[which(CODES==702)])) < 1e-12, "Welfare element == sum of its codes")

# ============================================================================
# TEST 2 — per-code DPS decomposition: Σ==jsd, 305 share, mass cross-check
# ============================================================================
cat("\n[TEST 2] per-code decomposition\n")
# speech: huge 305 mass (procedural artefact); manifesto: small 305
s56 <- c(0.02,0.02,0.02,0.02, 0.03,0.03,0.03,0.03, 0.50,  0.10,0.05, 0.06,0.09)  # 305 = 0.50
m56 <- c(0.04,0.04,0.04,0.04, 0.05,0.05,0.05,0.05, 0.05,  0.20,0.10, 0.12,0.08)  # 305 = 0.05
s56 <- s56/sum(s56); m56 <- m56/sum(m56)
s17 <- to17(s56); m17 <- to17(m56)
cc <- per_bucket_jsd(s17, m17)
ok(abs(sum(cc) - jsd(s17, m17)) < 1e-12, "Σ 17-element contrib == jsd(s17,m17)")
dps_lbls <- paste0("code_", DPS_codes)
dps_div <- sum(cc[dps_lbls]); c305 <- unname(cc["code_305"])
share_jsd <- c305 / dps_div
ok(share_jsd >= 0 && share_jsd <= 1, sprintf("305 share of DPS JSD in [0,1] (got %.3f)", share_jsd))
ok(share_jsd > 0.5, sprintf("305 dominates DPS divergence when speech 305 >> manifesto 305 (got %.3f)", share_jsd))
# mass-based |Δ| cross-check
d_abs <- abs(s56[dps_idx] - m56[dps_idx]); names(d_abs) <- as.character(DPS_codes)
share_mass <- unname(d_abs["305"] / sum(d_abs))
ok(share_mass >= 0 && share_mass <= 1, sprintf("305 share of DPS |Δ| in [0,1] (got %.3f)", share_mass))

# ============================================================================
# TEST 3 — 305 asymmetry + pre-stated verdict branching
# ============================================================================
cat("\n[TEST 3] asymmetry verdict\n")
verdict <- function(asym){ me <- mean(asym$excess); fp <- mean(asym$excess > 0)
  if (is.finite(me) && me > 0 && fp >= 0.8) "SUPPORTS" else "DOES_NOT_SUPPORT" }
# case A: speech systematically over-emphasizes 305 -> SUPPORTS
asymA <- data.table(speech_305=c(0.40,0.35,0.50,0.30,0.45), manifesto_305=c(0.05,0.06,0.04,0.10,0.05))
asymA[, excess := speech_305 - manifesto_305]
ok(verdict(asymA) == "SUPPORTS", "systematic speech-305 excess => SUPPORTS excl")
ok(abs(mean(asymA$excess) - mean(c(0.35,0.29,0.46,0.20,0.40))) < 1e-12, "mean excess computed correctly")
ok(mean(asymA$excess > 0) == 1.0, "100% of cells speech > manifesto")
# case B: no systematic asymmetry -> DOES NOT SUPPORT
asymB <- data.table(speech_305=c(0.10,0.05,0.08,0.04,0.06), manifesto_305=c(0.09,0.07,0.05,0.06,0.05))
asymB[, excess := speech_305 - manifesto_305]
ok(verdict(asymB) == "DOES_NOT_SUPPORT", "no systematic excess => DOES NOT SUPPORT excl")

# ============================================================================
# TEST 4 — gold-precision verdict branching
# ============================================================================
cat("\n[TEST 4] gold-precision branching\n")
gold_verdict <- function(p) if (!is.finite(p)) "PENDING" else if (p < 0.30) "CORROBORATES" else "CAUTIOUS"
ok(gold_verdict(0.16) == "CORROBORATES", "16% precision corroborates artefact (excl admissible)")
ok(gold_verdict(0.50) == "CAUTIOUS", "50% precision => treat excl cautiously")
ok(gold_verdict(NA_real_) == "PENDING", "absent gold => pending")

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
