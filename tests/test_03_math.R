# test_03_math.R — verifies the numeric core of 03 in base R (no data.table/arrow).
# Focus: the code305="excl" identity and the aggregation/bootstrap math.

set.seed(42)

MARPOR_CODES_56 <- c(101,102,103,104,105,106,107,108,109,110,
                     201,202,203,204, 301,302,303,304,305,
                     401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,
                     501,502,503,504,505,506,507,
                     601,602,603,604,605,606,607,608,
                     701,702,703,704,705,706)
stopifnot(length(MARPOR_CODES_56) == 56)
I305 <- which(MARPOR_CODES_56 == 305)

# --- A mapping (9 buckets, full partition) ---
A_map <- list(
  "Foreign Policy & Defence"=c(101,102,103,104,105,106,107,109),
  "European Integration"=c(108,110),
  "Democracy & Political System"=c(201,202,203,204,301,302,303,304,305),
  "Economy"=c(401,402,403,404,405,406,407,408,409,410,411,412,413,414,415),
  "Environment"=c(416,501),
  "Welfare & Social Policy"=c(502,503,504,505,506,507),
  "Law & Order and National Identity"=c(603,604,605,606),
  "Migration"=c(601,602,607,608),
  "Social Groups"=c(701,702,703,704,705,706))
BUCKETS_A <- names(A_map)
code_to_A <- setNames(rep(NA_character_,56), as.character(MARPOR_CODES_56))
for (b in names(A_map)) for (cd in A_map[[b]]) code_to_A[as.character(cd)] <- b
stopifnot(!any(is.na(code_to_A)))   # A partitions all 56

# --- B mapping (directional + Andere) ---
B_map <- list(
  "Marktliberalismus"=c(401,402,407,414),"Staatsintervention"=c(403,404,405,409,412,413),
  "Wirtschaft Allgemein"=c(408,410,411),"Sozialstaat Ausbau"=c(503,504,506),
  "Sozialstaat Begrenzung"=c(505,507),"Migration restriktiv"=c(601,608),
  "Migration liberal"=c(602,607),"Pro-EU"=c(108),"Contra-EU"=c(110))
BUCKETS_B <- names(B_map)
code_to_B <- setNames(rep(NA_character_,56), as.character(MARPOR_CODES_56))
for (b in names(B_map)) for (cd in B_map[[b]]) code_to_B[as.character(cd)] <- b

build_indicator <- function(c2b, levels) {
  M <- matrix(0,56,length(levels),dimnames=list(as.character(MARPOR_CODES_56),levels))
  for (i in 1:56) { bk <- c2b[as.character(MARPOR_CODES_56[i])]; if (is.na(bk)) bk<-"Andere"; M[i,bk]<-1 }
  M
}
indA <- build_indicator(code_to_A, BUCKETS_A)
indB <- build_indicator(code_to_B, c(BUCKETS_B,"Andere"))
stopifnot(all(rowSums(indA)==1), all(rowSums(indB)==1))
COL305_A <- "Democracy & Political System"; COL305_B <- "Andere"
stopifnot(indA[I305,COL305_A]==1, indB[I305,COL305_B]==1)

# --- functions copied verbatim-equivalent from 03/02 ---
temper <- function(P, tau) {
  if (isTRUE(all.equal(tau,1))) return(P)
  lp <- log(pmax(P,.Machine$double.eps))/tau
  e <- exp(lp - apply(lp,1,max)); e/rowSums(e)
}
bucket_shares <- function(P_t, ind, col305, mode) {
  BS <- P_t %*% ind
  if (mode=="excl") BS[,col305] <- BS[,col305] - P_t[,I305]
  BS
}
jsd <- function(p,q){ p<-as.numeric(p);q<-as.numeric(q);sp<-sum(p);sq<-sum(q)
  if(sp<=0||sq<=0)return(NA_real_); p<-p/sp;q<-q/sq;m<-.5*(p+q)
  kl<-function(a,b){k<-a>0&b>0;sum(a[k]*log2(a[k]/b[k]))};.5*kl(p,m)+.5*kl(q,m)}

# ============ synthetic cell: 25 sentences × 56, each row sums to 1 ============
n <- 25
P <- matrix(rexp(n*56), n, 56); P <- P / rowSums(P)
stopifnot(max(abs(rowSums(P)-1)) < 1e-12)

cat("==== TEST 1: code305 excl identity (THE critical one) ====\n")
# Method 1 (what 03 does): per-row bucket shares (excl=subtract 305 from DPS), mean, renorm.
BS_excl <- bucket_shares(P, indA, COL305_A, "excl")   # n × 9
m9_M1 <- colMeans(BS_excl); m9_M1 <- m9_M1 / sum(m9_M1)

# Method 2 (reference): drop code 305 from the cell-MEAN 56-vector, renorm over 55, bucket.
M56 <- colMeans(P)                                    # 56-vector, sums to 1
M55 <- M56; M55[I305] <- 0; M55 <- M55 / sum(M55)     # 305 removed, renormalized
m9_M2 <- as.numeric(M55 %*% indA); names(m9_M2) <- BUCKETS_A

# Method 3: zero 305 per sentence (no per-row renorm), bucket, mean, renorm.
P0 <- P; P0[,I305] <- 0
BS0 <- P0 %*% indA; m9_M3 <- colMeans(BS0); m9_M3 <- m9_M3/sum(m9_M3)

d12 <- max(abs(m9_M1 - m9_M2)); d13 <- max(abs(m9_M1 - m9_M3))
cat(sprintf("  max|M1-M2| = %.3e   max|M1-M3| = %.3e\n", d12, d13))
stopifnot(d12 < 1e-12, d13 < 1e-12)
cat("  PASS: all three excl formulations identical.\n")

cat("\n==== TEST 2: incl bucket shares sum to 1; partition correct ====\n")
BS_incl <- bucket_shares(P, indA, COL305_A, "incl")
cat(sprintf("  max|rowSums(BS_incl)-1| = %.3e\n", max(abs(rowSums(BS_incl)-1))))
stopifnot(max(abs(rowSums(BS_incl)-1)) < 1e-12)
# DPS incl vs excl difference must equal exactly the mean 305 mass
dps_diff <- mean(BS_incl[,COL305_A]) - mean(BS_excl[,COL305_A])
cat(sprintf("  mean DPS(incl)-DPS(excl) = %.6f ; mean p305 = %.6f\n",
            dps_diff, mean(P[,I305])))
stopifnot(abs(dps_diff - mean(P[,I305])) < 1e-12)
cat("  PASS.\n")

cat("\n==== TEST 3: B scheme — 305 lives in Andere, excl shrinks Andere ====\n")
BSb_incl <- bucket_shares(P, indB, COL305_B, "incl")
BSb_excl <- bucket_shares(P, indB, COL305_B, "excl")
stopifnot(max(abs(rowSums(BSb_incl)-1)) < 1e-12)
diffB <- mean(BSb_incl[,"Andere"]) - mean(BSb_excl[,"Andere"])
stopifnot(abs(diffB - mean(P[,I305])) < 1e-12)
cat(sprintf("  PASS: Andere(incl)-Andere(excl)=%.6f == mean p305.\n", diffB))

cat("\n==== TEST 4: temper native = identity; tau!=1 stays a valid distribution ====\n")
stopifnot(identical(temper(P,1.0), P))
Pt <- temper(P,0.5); stopifnot(max(abs(rowSums(Pt)-1))<1e-12)
Pf <- temper(P,2.0); stopifnot(max(abs(rowSums(Pf)-1))<1e-12)
# order check: temper THEN excl yields valid renormalizable cell
BS_tau_excl <- bucket_shares(temper(P,0.5), indA, COL305_A, "excl")
m <- colMeans(BS_tau_excl); m <- m/sum(m)
stopifnot(abs(sum(m)-1) < 1e-12)
cat("  PASS.\n")

cat("\n==== TEST 5: jsd properties ====\n")
stopifnot(abs(jsd(m9_M1, m9_M1)) < 1e-12)             # identical -> 0
a <- c(1,0,0,0,0,0,0,0,0); b <- c(0,1,0,0,0,0,0,0,0)
stopifnot(abs(jsd(a,b) - 1) < 1e-9)                   # disjoint -> 1 bit
stopifnot(abs(jsd(m9_M1, m9_M2)) < 1e-9)   # M1≈M2 -> jsd≈0 (fp noise can be ±1e-16)
cat("  PASS: jsd(x,x)=0, disjoint=1, near-identical≈0.\n")

cat("\n==== TEST 6: bootstrap on excl matrix (mean = colMeans, jsd renorms) ====\n")
bootstrap_jsd_buckets <- function(bucket_mat, mv, all_buckets, n_boot=200, seed=1){
  bm <- bucket_mat[,all_buckets,drop=FALSE]; nn<-nrow(bm)
  pt <- jsd(colMeans(bm), mv); set.seed(seed); reps<-numeric(n_boot)
  for (b in 1:n_boot){ idx<-sample.int(nn,nn,replace=TRUE); reps[b]<-jsd(colMeans(bm[idx,,drop=FALSE]),mv) }
  ci<-quantile(reps,c(.025,.975),na.rm=TRUE); list(point=pt,lo95=unname(ci[1]),hi95=unname(ci[2]),n_sent=nn)
}
mv <- runif(9); mv <- mv/sum(mv); names(mv) <- BUCKETS_A
BM <- BS_incl; colnames(BM) <- BUCKETS_A
BM[,COL305_A] <- BM[,COL305_A] - P[,I305]   # excl subtraction on the matrix
bs <- bootstrap_jsd_buckets(BM, mv, BUCKETS_A, n_boot=500, seed=7)
# point must equal jsd of the renormalized excl cell mean vs mv
ref <- jsd(m9_M1, mv)
cat(sprintf("  point=%.5f  ref=%.5f  CI=[%.5f, %.5f]  n=%d\n",
            bs$point, ref, bs$lo95, bs$hi95, bs$n_sent))
stopifnot(abs(bs$point - ref) < 1e-12, bs$lo95 <= bs$point, bs$point <= bs$hi95)
cat("  PASS: bootstrap point matches cell-level excl JSD; CI brackets point.\n")

cat("\n==== TEST 7: per_bucket_rel_asym ====\n")
per_bucket_rel_asym <- function(p,q){ p<-as.numeric(p);q<-as.numeric(q)
  p<-p/sum(p);q<-q/sum(q);m<-.5*(p+q); ifelse(m>0, abs(p-q)/m, NA_real_) }
ra <- per_bucket_rel_asym(m9_M1, mv)
stopifnot(all(ra>=0 & ra<=2, na.rm=TRUE))
# identical inputs -> all zero
stopifnot(all(abs(per_bucket_rel_asym(mv,mv)) < 1e-12))
cat(sprintf("  PASS: bounded [0,2], identical->0. (range %.3f..%.3f)\n", min(ra), max(ra)))

cat("\n==== TEST 8: manifesto excl identity (same as speech) ====\n")
v <- rexp(56); v <- v/sum(v)                          # one manifesto row, 56 fractions
# Method A: bucket then subtract 305 then renorm
ba <- as.numeric(v %*% indA); names(ba)<-BUCKETS_A; ba[COL305_A]<-ba[COL305_A]-v[I305]; ba<-ba/sum(ba)
# Method B: drop 305 from 56-vec, renorm, bucket
v55 <- v; v55[I305]<-0; v55<-v55/sum(v55); bb <- as.numeric(v55 %*% indA)
stopifnot(max(abs(ba-bb)) < 1e-12)
cat("  PASS: manifesto excl matches drop-then-bucket.\n")

cat("\n================ ALL CORE-MATH TESTS PASSED ================\n")
