# test_03_boot_lpmatch.R — proves the bootstrap now matches each speech cell to
# the CORRECT LP's manifesto (the bug: party-only match picked the first LP for
# every cell). Also confirms the cell-index split == the old which()-scan.
suppressPackageStartupMessages(library(data.table))
set.seed(3)
BUCKETS_A <- paste0("b", 1:9); COL305_A <- "b3"
jsd <- function(p,q){p<-as.numeric(p);q<-as.numeric(q);sp<-sum(p);sq<-sum(q)
  if(sp<=0||sq<=0)return(NA_real_);p<-p/sp;q<-q/sq;m<-.5*(p+q)
  kl<-function(a,b){k<-a>0&b>0;sum(a[k]*log2(a[k]/b[k]))};.5*kl(p,m)+.5*kl(q,m)}
bootstrap_jsd_buckets <- function(bucket_mat,mv,all_buckets,n_boot=100,seed=1){
  bm<-bucket_mat[,all_buckets,drop=FALSE];nn<-nrow(bm);pt<-jsd(colMeans(bm),mv)
  set.seed(seed);reps<-numeric(n_boot)
  for(b in 1:n_boot){idx<-sample.int(nn,nn,replace=TRUE);reps[b]<-jsd(colMeans(bm[idx,,drop=FALSE]),mv)}
  ci<-quantile(reps,c(.025,.975),na.rm=TRUE);list(point=pt,lo95=unname(ci[1]),hi95=unname(ci[2]),n_sent=nn)}
LP_ELECTION <- data.table(lp=c(19L,20L), election_date=as.Date(c("2017-09-24","2021-09-26")))

# --- speech: SPD has TWO cells (LP19, LP20), each its own sentences ---
mk <- function(n){m<-matrix(rexp(n*9),n,9);m/rowSums(m)}
BS19 <- mk(1500); BS20 <- mk(1500)
BS_all <- rbind(BS19, BS20); colnames(BS_all) <- BUCKETS_A
p305_all <- runif(3000)*BS_all[,"b3"]
proc_all <- rep(FALSE, 3000)
lp_all   <- c(rep(19L,1500), rep(20L,1500))
party_all<- rep("SPD", 3000)
group_all<- paste(lp_all, party_all, sep="|")
cell_idx_all <- split(seq_along(group_all), group_all)

# cell-index == old which()-scan?
stopifnot(identical(sort(cell_idx_all[["19|SPD"]]), which(group_all=="19|SPD")))
stopifnot(identical(sort(cell_idx_all[["20|SPD"]]), which(group_all=="20|SPD")))
cat("PASS: cell-index split matches which()-scan.\n")

# --- manifesto: SPD with TWO DISTINCT distributions, one per election ---
mfA <- runif(9); mfA <- mfA/sum(mfA)          # 2017 manifesto (LP19)
mfB <- runif(9); mfB <- mfB/sum(mfB)          # 2021 manifesto (LP20)
m1 <- rbindlist(list(
  data.table(party="SPD", election_date=as.Date("2017-09-24"), bucket=BUCKETS_A, share=mfA, code305="incl"),
  data.table(party="SPD", election_date=as.Date("2021-09-26"), bucket=BUCKETS_A, share=mfB, code305="incl")
))
mref <- m1[code305=="incl"]
cells <- data.table(lp=c(19L,20L), party=c("SPD","SPD"))

# --- FIXED boot_band logic (per-LP match) ---
fixed_point <- function(lp_j, pt_j){
  idx <- cell_idx_all[[paste(lp_j,pt_j,sep="|")]]
  BM  <- BS_all[idx,,drop=FALSE]
  ed_j <- LP_ELECTION$election_date[match(lp_j, LP_ELECTION$lp)]
  mv_long <- mref[party==pt_j & election_date==ed_j]
  mv <- setNames(mv_long$share, mv_long$bucket)[BUCKETS_A]; mv[is.na(mv)]<-0
  bootstrap_jsd_buckets(BM, mv, BUCKETS_A, n_boot=50, seed=1)$point
}
# --- BUGGY logic (party-only match, first election wins) ---
buggy_point <- function(lp_j, pt_j){
  idx <- cell_idx_all[[paste(lp_j,pt_j,sep="|")]]
  BM  <- BS_all[idx,,drop=FALSE]
  mv_long <- mref[party==pt_j]                          # NO election filter
  mv <- setNames(mv_long$share, mv_long$bucket)[BUCKETS_A]; mv[is.na(mv)]<-0  # first LP wins
  bootstrap_jsd_buckets(BM, mv, BUCKETS_A, n_boot=50, seed=1)$point
}

# Independent references: cell mean vs the CORRECT manifesto for that LP.
ref19 <- jsd(colMeans(BS19), mfA)   # LP19 -> 2017 -> mfA
ref20 <- jsd(colMeans(BS20), mfB)   # LP20 -> 2021 -> mfB

f19 <- fixed_point(19L,"SPD"); f20 <- fixed_point(20L,"SPD")
cat(sprintf("FIXED:  LP19 point=%.6f (ref %.6f)   LP20 point=%.6f (ref %.6f)\n", f19, ref19, f20, ref20))
stopifnot(abs(f19-ref19)<1e-12, abs(f20-ref20)<1e-12)
cat("PASS: fixed matching uses each cell's own-LP manifesto.\n")

# The buggy version uses mfA (first election) for BOTH cells -> LP20 wrong.
b20 <- buggy_point(20L,"SPD")
ref20_wrong <- jsd(colMeans(BS20), mfA)    # what the bug computes
cat(sprintf("BUGGY:  LP20 point=%.6f (== wrong-manifesto ref %.6f; correct is %.6f)\n",
            b20, ref20_wrong, ref20))
stopifnot(abs(b20-ref20_wrong)<1e-12)       # bug reproduced
stopifnot(abs(b20-ref20) > 1e-6)            # and it really differs from correct
cat("PASS: confirmed the old party-only match was wrong (LP20 used the 2017 manifesto).\n")

cat("\n================ BOOTSTRAP LP-MATCH FIX VERIFIED ================\n")
