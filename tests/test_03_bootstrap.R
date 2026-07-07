# test_03_bootstrap.R — exercises 03's bootstrap assembly + boot_band wiring
# on synthetic in-memory chunk pieces (no arrow). Verifies the 4 bands compose,
# CIs bracket points, and excl differs from incl.
suppressPackageStartupMessages(library(data.table))
set.seed(7)
BUCKETS_A <- paste0("b",1:9); COL305_A <- "b3"   # pretend DPS = b3
FILTERS <- c("filtered","unfiltered"); CODE305_VARIANTS <- c("incl","excl")
PARTIES_KEEP <- c("CDU/CSU","SPD","FDP","Grüne","Linke","AfD")
N_SENT_MIN <- 1000L; BOOTSTRAP_SEED <- 123L
BOOTSTRAP_PRIMARY_FILTER<-"filtered"; BOOTSTRAP_PRIMARY_CODE305<-"excl"
jsd <- function(p,q){p<-as.numeric(p);q<-as.numeric(q);sp<-sum(p);sq<-sum(q)
  if(sp<=0||sq<=0)return(NA_real_);p<-p/sp;q<-q/sq;m<-.5*(p+q)
  kl<-function(a,b){k<-a>0&b>0;sum(a[k]*log2(a[k]/b[k]))};.5*kl(p,m)+.5*kl(q,m)}
bootstrap_jsd_buckets <- function(bucket_mat,mv,all_buckets,n_boot=200,seed=1){
  bm<-bucket_mat[,all_buckets,drop=FALSE];nn<-nrow(bm)
  if(nn==0||all(is.na(mv)))return(list(point=NA,lo95=NA,hi95=NA,n_sent=nn))
  pt<-jsd(colMeans(bm),mv);set.seed(seed);reps<-numeric(n_boot)
  for(b in 1:n_boot){idx<-sample.int(nn,nn,replace=TRUE);reps[b]<-jsd(colMeans(bm[idx,,drop=FALSE]),mv)}
  ci<-quantile(reps,c(.025,.975),na.rm=TRUE);list(point=pt,lo95=unname(ci[1]),hi95=unname(ci[2]),n_sent=nn)}

# ---- synthetic per-chunk bootstrap pieces: 2 chunks ----
mk_piece <- function(parties,lps,n_){
  BS<-matrix(rexp(n_*9),n_,9);BS<-BS/rowSums(BS)         # 9 incl bucket shares (sum 1)
  p305<-pmin(BS[,3]*runif(n_),BS[,3])                    # 305 mass <= its bucket (b3)
  list(BS=BS,p305=p305,proc=sample(c(TRUE,FALSE),n_,TRUE,prob=c(.15,.85)),
       lp=lps[sample.int(length(lps),n_,TRUE)],          # avoid sample() length-1 gotcha
       pty=match(parties[sample.int(length(parties),n_,TRUE)],PARTIES_KEEP))
}
pc1<-mk_piece(c("SPD","AfD"),c(19L),1500); pc2<-mk_piece(c("SPD","AfD"),c(19L),1400)
boot_BS_chunks<-list(pc1$BS,pc2$BS); boot_p305_chunks<-list(pc1$p305,pc2$p305)
boot_proc_chunks<-list(pc1$proc,pc2$proc); boot_lp_chunks<-list(pc1$lp,pc2$lp)
boot_pty_chunks<-list(pc1$pty,pc2$pty)

# synthetic cell_diag + manifesto (scheme A, M1)
cell_diag <- data.table(party=c("SPD","AfD"),lp=c(19L,19L),
                        n_sent_filtered=c(2500L,2400L),n_sent_unfiltered=c(2900L,2800L))
m1 <- rbindlist(lapply(CODE305_VARIANTS,function(c305){
  rbindlist(lapply(c("SPD","AfD"),function(p){mv<-runif(9);mv<-mv/sum(mv)
    data.table(party=p,bucket=BUCKETS_A,share=mv,code305=c305,scheme="A",mapping="M1_entering")}))}))

# ================= ASSEMBLY + boot_band (copied from 03) =================
BS_all<-do.call(rbind,boot_BS_chunks); p305_all<-unlist(boot_p305_chunks)
proc_all<-unlist(boot_proc_chunks); lp_all<-unlist(boot_lp_chunks); pty_all<-unlist(boot_pty_chunks)
colnames(BS_all)<-BUCKETS_A; party_all<-PARTIES_KEEP[pty_all]; group_all<-paste(lp_all,party_all,sep="|")
is_excluded<-function(party,lp) party%in%c("parteilos","NA",NA_character_)|(party=="Linke"&lp%in%13:15)|(party=="FDP"&lp==18L)
cells<-unique(data.table(lp=lp_all,party=party_all)); cells<-cells[!is_excluded(party,lp)]
cells<-merge(cells,cell_diag[,.(party,lp,n_sent_filtered)],by=c("party","lp"),all.x=TRUE)
cells<-cells[n_sent_filtered>=N_SENT_MIN]
N_BOOT<-80L
boot_band<-function(flt,c305){
  mref<-m1[code305==c305]; out<-vector("list",nrow(cells))
  for(j in seq_len(nrow(cells))){lp_j<-cells$lp[j];pt_j<-cells$party[j]
    sel<-group_all==paste(lp_j,pt_j,sep="|"); if(flt=="filtered") sel<-sel&!proc_all
    idx<-which(sel); if(!length(idx))next
    BM<-BS_all[idx,,drop=FALSE]; if(c305=="excl")BM[,COL305_A]<-BM[,COL305_A]-p305_all[idx]
    mv_long<-mref[party==pt_j]; if(!nrow(mv_long))next
    mv<-setNames(mv_long$share,mv_long$bucket)[BUCKETS_A];mv[is.na(mv)]<-0
    bs<-bootstrap_jsd_buckets(BM,mv,BUCKETS_A,n_boot=N_BOOT,seed=BOOTSTRAP_SEED+j)
    out[[j]]<-data.table(party=pt_j,lp=lp_j,filter=flt,code305=c305,jsd_point=bs$point,
                         lo95=bs$lo95,hi95=bs$hi95,n_sent=bs$n_sent)}
  rbindlist(out)}
bands<-list(); for(flt in FILTERS) for(c305 in CODE305_VARIANTS) bands[[length(bands)+1L]]<-boot_band(flt,c305)
bootstrap_ci<-rbindlist(bands,use.names=TRUE)
bootstrap_ci[,is_primary:=(filter==BOOTSTRAP_PRIMARY_FILTER&code305==BOOTSTRAP_PRIMARY_CODE305)]

cat("==== BOOTSTRAP ASSEMBLY TEST ====\n")
print(bootstrap_ci[order(party,lp,filter,code305)])
# checks
stopifnot(nrow(bootstrap_ci)==2*length(FILTERS)*length(CODE305_VARIANTS))   # 2 cells × 4 bands = 8
stopifnot(sum(bootstrap_ci$is_primary)==2)                                  # 2 cells in primary band
stopifnot(all(bootstrap_ci$lo95<=bootstrap_ci$jsd_point+1e-9))
stopifnot(all(bootstrap_ci$jsd_point<=bootstrap_ci$hi95+1e-9))
# filtered n < unfiltered n per cell/code305
w<-dcast(bootstrap_ci,party+code305~filter,value.var="n_sent")
stopifnot(all(w$filtered<w$unfiltered))
# excl point != incl point (305 subtraction changes the distribution)
w2<-dcast(bootstrap_ci[filter=="filtered"],party~code305,value.var="jsd_point")
stopifnot(all(abs(w2$incl-w2$excl)>0))
cat("\n  PASS: 4 bands × cells assembled; CIs bracket points; filtered<unfiltered;\n")
cat("        excl differs from incl; is_primary marks exactly the filtered×excl band.\n")
cat("\n================ BOOTSTRAP ASSEMBLY TEST PASSED ================\n")
