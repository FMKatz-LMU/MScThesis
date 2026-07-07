# test_03_manifesto_hard.R — runs 03's manifesto block and hard-threshold block
# verbatim-equivalent on synthetic in-memory data (no arrow needed).
suppressPackageStartupMessages(library(data.table))
set.seed(99)

MARPOR_CODES_56 <- c(101,102,103,104,105,106,107,108,109,110,201,202,203,204,
  301,302,303,304,305,401,402,403,404,405,406,407,408,409,410,411,412,413,414,
  415,416,501,502,503,504,505,506,507,601,602,603,604,605,606,607,608,701,702,
  703,704,705,706)
I305 <- which(MARPOR_CODES_56==305)
A_map <- list("FPD"=c(101,102,103,104,105,106,107,109),"EU"=c(108,110),
  "DPS"=c(201,202,203,204,301,302,303,304,305),
  "Econ"=c(401,402,403,404,405,406,407,408,409,410,411,412,413,414,415),
  "Env"=c(416,501),"Welf"=c(502,503,504,505,506,507),"Law"=c(603,604,605,606),
  "Migr"=c(601,602,607,608),"SocG"=c(701,702,703,704,705,706))
BUCKETS_A <- names(A_map); BUCKETS_A_OUT <- BUCKETS_A
code_to_A <- setNames(rep(NA_character_,56),as.character(MARPOR_CODES_56))
for(b in names(A_map)) for(cd in A_map[[b]]) code_to_A[as.character(cd)]<-b
build_ind<-function(c2b,lev){M<-matrix(0,56,length(lev),dimnames=list(as.character(MARPOR_CODES_56),lev))
  for(i in 1:56){bk<-c2b[as.character(MARPOR_CODES_56[i])];if(is.na(bk))bk<-"Andere";M[i,bk]<-1};M}
indA<-build_ind(code_to_A,BUCKETS_A); COL305_A<-"DPS"; BUCKET_OF_305_A<-"DPS"
CODE305_VARIANTS<-c("incl","excl"); PARTIES_KEEP<-c("CDU/CSU","SPD","FDP","Grüne","Linke","AfD")
LP_ELECTION <- data.table(lp=c(19L,20L), election_date=as.Date(c("2017-09-24","2021-09-26")))

# =================== MANIFESTO BLOCK (copied from 03) ===================
cat("==== MANIFESTO BLOCK TEST ====\n")
# synthetic manifesto: 3 rows, "NNN - Title" cols summing to 1
mk_mf <- function(){
  P <- matrix(rexp(3*56),3,56); P<-P/rowSums(P)
  cn <- sprintf("%03d - Title", MARPOR_CODES_56)
  dt <- as.data.table(P); setnames(dt,cn)
  dt[, party_label := c("CDU_CSU_joint","SPD","AfD")]
  dt[, mapping := "M1_entering"]; dt[, legislative_period := c(19L,19L,19L)]
  dt
}
mf <- mk_mf()
mf_prob <- grep("^[0-9]{3} - ",names(mf),value=TRUE)
mf_codes <- as.integer(sub("^([0-9]{3}).*","\\1",mf_prob)); mf_prob<-mf_prob[match(MARPOR_CODES_56,mf_codes)]
Mmf <- as.matrix(mf[,..mf_prob]); storage.mode(Mmf)<-"double"
rs<-rowSums(Mmf); if(any(abs(rs-1)>1e-6)) Mmf<-Mmf/rs

manifesto_long <- rbindlist(lapply(c("A"), function(scheme){
  ind<-indA; col305<-COL305_A; bk<-BUCKETS_A_OUT
  rbindlist(lapply(CODE305_VARIANTS, function(c305){
    BS<-Mmf%*%ind; if(c305=="excl") BS[,col305]<-BS[,col305]-Mmf[,I305]
    BS<-BS/rowSums(BS)
    dt<-data.table(party_label=mf$party_label,mapping=mf$mapping,lp=mf$legislative_period)
    long<-dt[rep(seq_len(nrow(dt)),each=length(bk))]
    long[,bucket:=rep(bk,nrow(dt))]; long[,share:=as.numeric(t(BS))]
    long[,scheme:=scheme][,code305:=c305]; long
  }))
}),use.names=TRUE)
MANIFESTO_PARTY_MAP<-c("CDU_CSU_joint"="CDU/CSU","SPD"="SPD","FDP"="FDP","GRUENE"="Grüne","DIE LINKE"="Linke","AfD"="AfD")
manifesto_long[,party:=MANIFESTO_PARTY_MAP[party_label]]
manifesto_long<-manifesto_long[!is.na(party)&party%in%PARTIES_KEEP]
manifesto_long<-merge(manifesto_long,LP_ELECTION,by="lp",all.x=TRUE)
manifesto_dists<-manifesto_long[,.(party,election_date,mapping,scheme,code305,bucket,share)]

# cross-check: CDU_CSU_joint row -> party "CDU/CSU"; incl bucket vec == Mmf row %*% indA
stopifnot("CDU/CSU" %in% manifesto_dists$party)
cdu_incl <- manifesto_dists[party=="CDU/CSU"&code305=="incl"][order(match(bucket,BUCKETS_A))]$share
ref_incl <- as.numeric(Mmf[1,]%*%indA)
stopifnot(max(abs(cdu_incl-ref_incl))<1e-12)
# excl: drop 305 from 56-vec, renorm, bucket
v55 <- Mmf[1,]; v55[I305]<-0; v55<-v55/sum(v55); ref_excl<-as.numeric(v55%*%indA)
cdu_excl <- manifesto_dists[party=="CDU/CSU"&code305=="excl"][order(match(bucket,BUCKETS_A))]$share
stopifnot(max(abs(cdu_excl-ref_excl))<1e-12)
# every cell sums to 1, election_date attached
chk<-manifesto_dists[,.(s=sum(share)),by=.(party,code305)]; stopifnot(max(abs(chk$s-1))<1e-12)
stopifnot(all(!is.na(manifesto_dists$election_date)))
cat("  PASS: party map, t(BS) alignment, incl/excl values, renorm, election_date merge.\n")

# =================== HARD-THRESHOLD BLOCK (copied from 03) ===================
cat("\n==== HARD-THRESHOLD BLOCK TEST ====\n")
# 4 sentences with KNOWN argmax + max-prob, scheme A.
# rows: argmax code, max prob
#  s1: code 305 (DPS), 0.6  -> thr0.5 accept -> DPS ; thr0.4 accept -> DPS
#  s2: code 401 (Econ), 0.45 -> thr0.5 REJECT -> Andere ; thr0.4 accept -> Econ
#  s3: code 503 (Welf), 0.9 -> accept both -> Welf
#  s4: code 108 (EU),  0.3  -> reject both -> Andere
P <- matrix(0.001, 4, 56)
setp <- function(r, code, p){ j<-which(MARPOR_CODES_56==code); P[r,]<<-(1-p)/55; P[r,j]<<-p }
setp(1,305,0.6); setp(2,401,0.45); setp(3,503,0.9); setp(4,108,0.3)
amax_idx <- max.col(P,ties.method="first"); mpf<-P[cbind(1:4,amax_idx)]
gf <- rep("19|SPD",4)
hard_one <- function(thr){
  scheme<-"A"; c2b<-code_to_A
  assigned<-c2b[as.character(MARPOR_CODES_56[amax_idx])]
  assigned[is.na(assigned)|mpf<thr]<-"Andere"
  lev<-c(BUCKETS_A,"Andere")
  M<-matrix(0,length(assigned),length(lev),dimnames=list(NULL,lev))
  M[cbind(seq_along(assigned),match(assigned,lev))]<-1
  sm<-rowsum(M,gf,reorder=FALSE); as.numeric(sm)/sum(sm[1,]) -> sh; names(sh)<-lev; sh
}
h5 <- hard_one(0.5); h4 <- hard_one(0.4)
cat("  thr0.5:", paste(names(h5)[h5>0], round(h5[h5>0],3), collapse="  "), "\n")
cat("  thr0.4:", paste(names(h4)[h4>0], round(h4[h4>0],3), collapse="  "), "\n")
# thr0.5: DPS .25 (s1), Welf .25 (s3), Andere .5 (s2,s4)
stopifnot(abs(h5["DPS"]-0.25)<1e-12, abs(h5["Welf"]-0.25)<1e-12, abs(h5["Andere"]-0.5)<1e-12)
# thr0.4: DPS .25, Econ .25 (s2 now accepted), Welf .25, Andere .25 (s4)
stopifnot(abs(h4["DPS"]-0.25)<1e-12, abs(h4["Econ"]-0.25)<1e-12,
          abs(h4["Welf"]-0.25)<1e-12, abs(h4["Andere"]-0.25)<1e-12)
stopifnot(abs(sum(h5)-1)<1e-12, abs(sum(h4)-1)<1e-12)
cat("  PASS: argmax routing + threshold + Andere fallback exact; shares sum to 1.\n")

cat("\n================ MANIFESTO + HARD BLOCK TESTS PASSED ================\n")
