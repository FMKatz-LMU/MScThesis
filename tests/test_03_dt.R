# test_03_dt.R — verifies the data.table idioms in 03 that base R can't.
suppressPackageStartupMessages(library(data.table))
set.seed(1)
BUCKETS_A <- paste0("b", 1:9)

cat("==== DT TEST A: finalize() idiom (key split, := axes, cell renorm) ====\n")
finalize <- function(env, buckets, extra) {
  keys <- ls(env)
  if (!length(keys)) return(NULL)
  rbindlist(lapply(keys, function(k) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    cur <- env[[k]]
    sh <- cur$sum_mass / cur$n
    s <- sum(sh); if (s > 0) sh <- sh / s
    data.table(lp = as.integer(parts[1]), party = parts[2],
               bucket = buckets, share = as.numeric(sh), n_sent = cur$n)[,
      (names(extra)) := extra]
  }))
}
e <- new.env()
# cell "19|SPD": sum_mass that sums to 0.8 (mimics an excl cell <1) over 9 buckets, n=10
sm <- c(0.30,0.10,0.05,0.05,0.10,0.05,0.05,0.05,0.05); stopifnot(abs(sum(sm)-0.8)<1e-12)
e[["19|SPD"]] <- list(sum_mass = sm, n = 10L)
e[["20|CDU/CSU"]] <- list(sum_mass = rep(1/9,9)*0.5, n = 4L)   # party with slash in key
out <- finalize(e, BUCKETS_A, list(scheme="A", filter="filtered", tau_name="native", code305="excl"))
print(out[order(party,lp)])
# checks: renormalized to 1 per cell; party with "/" preserved; axes attached
chk <- out[, .(s=sum(share)), by=.(party,lp,scheme,filter,tau_name,code305)]
stopifnot(max(abs(chk$s-1)) < 1e-12)
stopifnot("CDU/CSU" %in% out$party, "SPD" %in% out$party)
stopifnot(all(c("scheme","filter","tau_name","code305") %in% names(out)))
# SPD bucket1 share must be 0.30/0.80 = 0.375 after renorm
stopifnot(abs(out[party=="SPD" & bucket=="b1", share] - 0.375) < 1e-12)
stopifnot(all(out[party=="SPD", n_sent] == 10L))
cat("  PASS: renorm correct, slash-party intact, axes attached, n_sent carried.\n")

cat("\n==== DT TEST B: rowsum + table count alignment (first-appearance order) ====\n")
# groups deliberately NOT sorted; rownames(rowsum) follow first appearance.
group <- c("19|SPD","20|CDU/CSU","19|SPD","19|SPD","20|CDU/CSU")
M <- matrix(1, nrow=5, ncol=3)
su <- rowsum(M, group, reorder=FALSE)
nn <- as.integer(table(group)[rownames(su)])
# rownames(su) first-appearance = c("19|SPD","20|CDU/CSU"); counts 3 and 2
stopifnot(identical(rownames(su), c("19|SPD","20|CDU/CSU")))
stopifnot(identical(nn, c(3L,2L)))
# row sums equal counts (since M all ones, 3 cols) -> each cell row = count
stopifnot(all(su[,1] == nn))
cat("  PASS: rowsum rows align with table() counts via rownames indexing.\n")

cat("\n==== DT TEST C: manifesto t(BS) flattening keeps bucket↔value alignment ====\n")
# 2 manifesto rows, 9 buckets. Build BS with DISTINCT, identifiable values so a
# transpose/flatten bug would scramble them visibly.
bk <- BUCKETS_A
BS <- rbind(row1 = 1:9, row2 = 101:109) / 1   # row1 = 1..9, row2 = 101..109
mf <- data.table(party_label=c("SPD","AfD"), mapping=c("M1_entering","M1_entering"),
                 lp=c(19L,19L))
long <- mf[rep(seq_len(nrow(mf)), each=length(bk))]
long[, bucket := rep(bk, nrow(mf))]
long[, share  := as.numeric(t(BS))]
print(long)
# row1 (SPD) must carry 1..9 in bucket order; row2 (AfD) 101..109
stopifnot(identical(long[party_label=="SPD", share], as.numeric(1:9)))
stopifnot(identical(long[party_label=="AfD", share], as.numeric(101:109)))
stopifnot(identical(long[party_label=="SPD", bucket], bk))
cat("  PASS: each manifesto row's bucket values stay correctly aligned.\n")

cat("\n==== DT TEST D: merge for election_date + cells gate (left join, no row loss) ====\n")
cells <- data.table(party=c("SPD","AfD","FDP"), lp=c(19L,19L,18L))
diag  <- data.table(party=c("SPD","AfD","FDP"), lp=c(19L,19L,18L),
                    n_sent_filtered=c(50000L, 8000L, 300L))
cells2 <- merge(cells, diag[, .(party,lp,n_sent_filtered)], by=c("party","lp"), all.x=TRUE)
gated <- cells2[n_sent_filtered >= 1000L]
stopifnot(nrow(gated)==2, !("FDP" %in% gated$party))   # FDP(300) dropped
cat("  PASS: left-join keeps cells, n_sent gate drops the sub-1000 cell.\n")

cat("\n================ ALL DATA.TABLE IDIOM TESTS PASSED ================\n")
