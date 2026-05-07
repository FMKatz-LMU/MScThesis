# diagnose_rowsum.R
# -----------------
# After apply_aggregation_AB.R fails on the sanity check, run this to see
# how bad the deviation actually is and where it's coming from.

library(data.table)

# The accumulators are not saved on failure, but the function-local
# variable agg_A_speech / agg_B_speech should still exist in the global
# env if the script ran in an interactive session.

if (!exists("agg_A_speech")) {
  stop("agg_A_speech not found. Re-source apply_aggregation_AB.R up to and ",
       "including the finalize_acc() calls, then run this script.")
}

cat("=== Row-sum deviation, Aggregation A ===\n")
sumA <- agg_A_speech[, .(s = sum(share)),
                      by = .(legislative_period, party, tau_name)]
sumA[, dev := abs(s - 1)]
cat("max abs deviation:", max(sumA$dev), "\n")
cat("mean abs deviation:", mean(sumA$dev), "\n")
cat("worst offenders:\n")
print(sumA[order(-dev)][1:10])

cat("\n=== Row-sum deviation, Aggregation B ===\n")
sumB <- agg_B_speech[, .(s = sum(share)),
                      by = .(legislative_period, party, tau_name)]
sumB[, dev := abs(s - 1)]
cat("max abs deviation:", max(sumB$dev), "\n")
cat("mean abs deviation:", mean(sumB$dev), "\n")
print(sumB[order(-dev)][1:10])

# Are bad rows concentrated in any tau?
cat("\n=== Deviation by temperature (Aggregation A) ===\n")
print(sumA[, .(max_dev = max(dev), mean_dev = mean(dev)), by = tau_name])
