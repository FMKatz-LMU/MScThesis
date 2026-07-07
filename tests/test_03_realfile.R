# test_03_realfile.R — END-TO-END test of the REAL 03 on synthetic parquet.
# Runs on the PC (needs arrow + data.table). NOT pre-verified in Claude's sandbox
# (no R-arrow there) — its first run doubles as the arrow-I/O + schema check.
#
# It writes a small synthetic v2 corpus + synthetic manifesto into a temp dir,
# points the REAL 03 at them via the override globals, sources 03, and checks
# the caches. Cleans up after itself (incl. the override globals).
suppressPackageStartupMessages({ library(arrow); library(data.table) })
source(here::here("00_config.R"))   # MARPOR_CODES_56, AGG_A/AGG_B, BUCKETS_*, etc.

.tmp   <- file.path(tempdir(), paste0("t03_real_", as.integer(Sys.time())))
.pq    <- file.path(.tmp, "parquet")
.cache <- file.path(.tmp, "cache")
dir.create(.pq, recursive = TRUE, showWarnings = FALSE)
dir.create(.cache, recursive = TRUE, showWarnings = FALSE)

set.seed(20260613)
.codes <- MARPOR_CODES_56
.pnames <- sprintf("%03d - Title", .codes)             # "NNN - Title" like the real v2 schema

.mk_chunk <- function(n) {
  P <- matrix(rexp(n * 56), n, 56); P <- P / rowSums(P)
  dt <- as.data.table(P); setnames(dt, .pnames)
  dt[, party := sample(c("CDU", "CSU", "SPD", "AfD"), n, TRUE)]
  dt[, legislative_period := 20L]                      # single LP -> cells exceed n_min
  dt[, is_procedural := sample(c(TRUE, FALSE), n, TRUE, prob = c(0.15, 0.85))]
  dt[, sentence := paste("hier", seq_len(n), "stehen ein paar weitere woerter im satz heute")]  # >=8 words (exercises tight)
  dt[, proc_class  := ifelse(is_procedural, "anrede", NA_character_)]   # v2 passthrough (03 ignores)
  dt[, proc_source := ifelse(is_procedural, "exact", NA_character_)]
  dt[, pred_label  := .pnames[max.col(P, ties.method = "first")]]
  dt[, pred_score  := apply(P, 1, max)]
  dt
}
for (k in 1:3)
  write_parquet(.mk_chunk(2500), file.path(.pq, sprintf("chunk_%03d.parquet", k)))

# synthetic manifesto: CDU_CSU_joint, SPD, AfD at LP20; "NNN - Title" fractions.
.mkmf <- function() {
  P <- matrix(rexp(3 * 56), 3, 56); P <- P / rowSums(P)
  dt <- as.data.table(P); setnames(dt, .pnames)
  dt[, party_label := c("CDU_CSU_joint", "SPD", "AfD")]
  dt[, mapping := "M1_entering"]; dt[, legislative_period := 20L]
  dt
}
.mfpath <- file.path(.tmp, "manifesto_distributions.rds")
saveRDS(.mkmf(), .mfpath)

# ---- point the REAL 03 at the synthetic data, tiny bootstrap ----
.PARQUET_DIR_OVERRIDE   <- .pq
.MANIFESTO_RDS_OVERRIDE <- .mfpath
.CACHE_DIR_OVERRIDE     <- .cache
.N_BOOT_OVERRIDE_EXT    <- 40L

source(here::here("03_load_and_aggregate.R"))

# ============================ CHECKS ============================
.ss <- readRDS(file.path(.cache, "speech_soft.rds"))
.sm <- readRDS(file.path(.cache, "speech_method.rds"))
.md <- readRDS(file.path(.cache, "manifesto_dists.rds"))
.bc <- readRDS(file.path(.cache, "bootstrap_ci.rds"))
.cd <- readRDS(file.path(.cache, "cell_diag.rds"))

stopifnot(nrow(.ss) > 0, nrow(.sm) > 0, nrow(.md) > 0, nrow(.bc) > 0, nrow(.cd) > 0)
# 24 soft specs = scheme(2) × filter(2) × tau(3) × code305(2)
stopifnot(nrow(unique(.ss[, .(scheme, filter, tau_name, code305)])) == 24L)
# row sums = 1 per cell-spec (A and B)
.rs <- .ss[, .(s = sum(share)), by = .(party, lp, scheme, filter, tau_name, code305)]
stopifnot(max(abs(.rs$s - 1)) < 1e-6)
# manifesto: incl + excl present, sums to 1
stopifnot(setequal(unique(.md$code305), c("incl", "excl")))
.rsm <- .md[, .(s = sum(share)), by = .(party, mapping, scheme, code305)]
stopifnot(max(abs(.rsm$s - 1)) < 1e-6)
# bootstrap: 4 bands, primary flag set, CIs bracket points
stopifnot(nrow(unique(.bc[, .(filter, code305)])) == 4L, any(.bc$is_primary))
stopifnot(all(.bc$lo95 <= .bc$jsd_point + 1e-9 & .bc$jsd_point <= .bc$hi95 + 1e-9, na.rm = TRUE))
# CDU/CSU pooled into one cell; filtered <= unfiltered
stopifnot("CDU/CSU" %in% .ss$party)
stopifnot(all(.cd$n_sent_filtered <= .cd$n_sent_unfiltered))
# excl differs from incl on the speech side (305 removed) for a populated cell
.cmp <- dcast(.ss[scheme=="A" & filter=="filtered" & tau_name=="native" & bucket==BUCKET_OF_305_A],
              party ~ code305, value.var = "share")
stopifnot(all(abs(.cmp$incl - .cmp$excl) > 0))

cat("\n>>> REAL-FILE END-TO-END TEST PASSED:\n")
cat("    arrow read + schema discovery OK; 24 soft specs; A/B row-sums = 1;\n")
cat("    manifesto incl/excl; 4 bootstrap bands bracket points; CDU/CSU pooled.\n")

# ---- cleanup (temp dir + override globals so the real run uses real paths) ----
unlink(.tmp, recursive = TRUE)
suppressWarnings(rm(list = c(".PARQUET_DIR_OVERRIDE", ".MANIFESTO_RDS_OVERRIDE",
                             ".CACHE_DIR_OVERRIDE", ".N_BOOT_OVERRIDE_EXT"),
                    envir = environment()))
