# test_03_stream.R — exercises 03's streaming accumulation across MULTIPLE chunks
# with synthetic in-memory data.tables, cross-checked against an independent
# colMeans computation over the concatenated corpus.
suppressPackageStartupMessages(library(data.table))
set.seed(2025)

# ---- minimal config (mirrors 00_config) ----
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
BUCKETS_A <- names(A_map)
code_to_A <- setNames(rep(NA_character_,56),as.character(MARPOR_CODES_56))
for(b in names(A_map)) for(cd in A_map[[b]]) code_to_A[as.character(cd)]<-b
build_ind <- function(c2b,lev){M<-matrix(0,56,length(lev),dimnames=list(as.character(MARPOR_CODES_56),lev))
  for(i in 1:56){bk<-c2b[as.character(MARPOR_CODES_56[i])];if(is.na(bk))bk<-"Andere";M[i,bk]<-1};M}
indA <- build_ind(code_to_A,BUCKETS_A); COL305_A<-"DPS"
TEMPERATURES <- c(sharp=0.5,native=1.0,flat=2.0)
PARTIES_KEEP <- c("CDU/CSU","SPD","FDP","Grüne","Linke","AfD")
LEGISLATIVE_PERIODS <- 13:20; CODE305_VARIANTS <- c("incl","excl"); FILTERS<-c("filtered","unfiltered")

normalize_party <- function(x){x<-tolower(as.character(x));o<-rep(NA_character_,length(x))
  o[grepl("cdu|csu|union",x)&!grepl("verband",x)]<-"CDU/CSU";o[grepl("\\bspd\\b",x)]<-"SPD"
  o[grepl("\\bfdp\\b",x)]<-"FDP";o[grepl("gr[uü]n",x)]<-"Grüne";o[grepl("\\blinke\\b|pds",x)]<-"Linke"
  o[grepl("\\bafd\\b",x)]<-"AfD";o}
temper <- function(P,tau){if(isTRUE(all.equal(tau,1)))return(P);lp<-log(pmax(P,.Machine$double.eps))/tau
  e<-exp(lp-apply(lp,1,max));e/rowSums(e)}
bucket_shares <- function(P_t,ind,col305,mode){BS<-P_t%*%ind;if(mode=="excl")BS[,col305]<-BS[,col305]-P_t[,I305];BS}

# ---- build 2 synthetic chunks; party raw "CDU"/"CSU"/"SPD"/"AfD", 2 LPs ----
mk_chunk <- function(parties, lps, nrow_) {
  P <- matrix(rexp(nrow_*56),nrow_,56); P<-P/rowSums(P)
  cn <- sprintf("%03d - X", MARPOR_CODES_56)
  dt <- as.data.table(P); setnames(dt, cn)
  dt[, party := sample(parties, nrow_, TRUE)]
  dt[, legislative_period := sample(lps, nrow_, TRUE)]
  dt[, is_procedural := sample(c(TRUE,FALSE), nrow_, TRUE, prob=c(0.2,0.8))]
  dt[, sentence := paste("wort", seq_len(nrow_), "noch ein paar woerter hier")]  # >=8 words
  dt
}
ch1 <- mk_chunk(c("CDU","CSU","SPD","AfD"), c(19,20), 400)
ch2 <- mk_chunk(c("CDU","CSU","SPD","AfD"), c(19,20), 350)
chunks <- list(ch1, ch2)
prob_cols <- sprintf("%03d - X", MARPOR_CODES_56)   # already MARPOR order

# ---- accumulators + loop body COPIED from 03 (soft cube only, scheme A) ----
new_env_ <- function() new.env(hash=TRUE)
soft_env <- list()
for(flt in FILTERS) for(tn in names(TEMPERATURES)) for(c305 in CODE305_VARIANTS)
  soft_env[[paste("soft","A",flt,tn,c305,sep="|")]] <- new_env_()
bump <- function(env,key,vec,n){cur<-env[[key]];env[[key]]<-if(is.null(cur))list(sum_mass=vec,n=n) else list(sum_mass=cur$sum_mass+vec,n=cur$n+n);invisible()}

for (i in seq_along(chunks)) {
  chunk <- chunks[[i]]
  party_norm <- normalize_party(chunk$party)
  keep <- !is.na(party_norm)&party_norm%in%PARTIES_KEEP&chunk$legislative_period%in%LEGISLATIVE_PERIODS
  chunk <- chunk[keep]; party_norm<-party_norm[keep]
  P <- as.matrix(chunk[,..prob_cols]); storage.mode(P)<-"double"
  is_proc <- as.logical(chunk$is_procedural); keep_flt <- !is_proc
  group <- paste(chunk$legislative_period, party_norm, sep="|")
  for (tn in names(TEMPERATURES)) {
    P_t <- temper(P, TEMPERATURES[[tn]])
    ind<-indA; col305<-COL305_A
    for (c305 in CODE305_VARIANTS) {
      BS <- bucket_shares(P_t,ind,col305,c305)
      su <- rowsum(BS,group,reorder=FALSE); nu<-as.integer(table(group)[rownames(su)])
      env <- soft_env[[paste("soft","A","unfiltered",tn,c305,sep="|")]]
      for(k in seq_len(nrow(su))) bump(env,rownames(su)[k],su[k,],nu[k])
      if(any(keep_flt)){BSf<-BS[keep_flt,,drop=FALSE];gf<-group[keep_flt]
        sf<-rowsum(BSf,gf,reorder=FALSE);nf<-as.integer(table(gf)[rownames(sf)])
        envf<-soft_env[[paste("soft","A","filtered",tn,c305,sep="|")]]
        for(k in seq_len(nrow(sf))) bump(envf,rownames(sf)[k],sf[k,],nf[k])}
    }
  }
}

# ---- INDEPENDENT cross-check over the concatenated corpus ----
allP <- rbind(as.matrix(ch1[,..prob_cols]), as.matrix(ch2[,..prob_cols])); storage.mode(allP)<-"double"
allparty <- normalize_party(c(ch1$party, ch2$party))
alllp    <- c(ch1$legislative_period, ch2$legislative_period)
allproc  <- c(ch1$is_procedural, ch2$is_procedural)
allgroup <- paste(alllp, allparty, sep="|")

cat("==== STREAM TEST: cross-check accumulator vs independent colMeans ====\n")
test_cell <- function(lp_, pty_, flt, c305, tn) {
  env <- soft_env[[paste("soft","A",flt,tn,c305,sep="|")]]
  cur <- env[[paste(lp_,pty_,sep="|")]]
  acc <- cur$sum_mass/cur$n; acc <- acc/sum(acc)              # finalize renorm
  # independent:
  sel <- allgroup==paste(lp_,pty_,sep="|")
  if(flt=="filtered") sel <- sel & !allproc
  Psel <- temper(allP[sel,,drop=FALSE], TEMPERATURES[[tn]])
  BS <- Psel%*%indA; if(c305=="excl") BS[,COL305_A]<-BS[,COL305_A]-Psel[,I305]
  ref <- colMeans(BS); ref<-ref/sum(ref)
  list(maxdiff=max(abs(acc-ref)), n_acc=cur$n, n_ref=sum(sel))
}
ok <- TRUE
for (pty in c("CDU/CSU","SPD","AfD"))
  for (flt in FILTERS)
    for (c305 in CODE305_VARIANTS)
      for (tn in c("native","sharp","flat")) {
        r <- test_cell(19L, pty, flt, c305, tn)
        if (r$maxdiff > 1e-12 || r$n_acc != r$n_ref) {
          cat(sprintf("  FAIL %s|19 %s/%s/%s diff=%.2e n_acc=%d n_ref=%d\n",
                      pty,flt,c305,tn,r$maxdiff,r$n_acc,r$n_ref)); ok<-FALSE
        }
      }
stopifnot(ok)
cat("  PASS: all (party × filter × code305 × tau) cells match independent colMeans,\n")
cat("        and accumulated n equals corpus n (multi-chunk bump correct).\n")

# filtered n < unfiltered n (since ~20% procedural)
e_u <- soft_env[["soft|A|unfiltered|native|incl"]][["19|SPD"]]$n
e_f <- soft_env[["soft|A|filtered|native|incl"]][["19|SPD"]]$n
cat(sprintf("  SPD|19: n_unfiltered=%d  n_filtered=%d  (filtered<unfiltered: %s)\n",
            e_u, e_f, e_f < e_u))
stopifnot(e_f < e_u)
# CDU/CSU pooling: pooled n equals raw CDU + CSU sentences at LP19
n_pool <- e_pool <- soft_env[["soft|A|unfiltered|native|incl"]][["19|CDU/CSU"]]$n
raw_cdu_csu <- sum(normalize_party(c(ch1$party,ch2$party))=="CDU/CSU" & c(ch1$legislative_period,ch2$legislative_period)==19)
stopifnot(n_pool == raw_cdu_csu)
cat(sprintf("  CDU/CSU|19 pooled n=%d == raw CDU+CSU sentences=%d (sentence-level pooling OK)\n",
            n_pool, raw_cdu_csu))

cat("\n================ STREAM INTEGRATION TEST PASSED ================\n")
