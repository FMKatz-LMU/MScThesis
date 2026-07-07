# ============================================================================
# test_06_logic.R — base-R/data.table logic test for 06_H3.R core algorithms
# ============================================================================
suppressPackageStartupMessages(library(data.table))
PASS <- 0L; FAIL <- 0L
ok <- function(cond, label){ if (isTRUE(cond)){PASS<<-PASS+1L; cat(sprintf("  PASS  %s\n",label))}
                             else {FAIL<<-FAIL+1L; cat(sprintf("  FAIL  %s\n",label))} }

# ---- safe_lm core (lm is base R) -------------------------------------------
safe_lm <- function(d) {
  d <- d[!is.na(d$polarization), ]
  if (nrow(d) < 3) return(list(n = nrow(d), slope = NA_real_, r2 = NA_real_))
  m <- lm(polarization ~ lp, data = d); sm <- summary(m)
  list(n = nrow(d), slope = unname(coef(m)[2]), r2 = sm$r.squared)
}
dalton <- function(positions, weights){
  okp <- !is.na(positions) & !is.na(weights) & weights > 0
  if (sum(okp) < 2L) return(c(mean_pos=NA, polarization=NA, n=sum(okp)))
  p<-positions[okp]; w<-weights[okp]; w<-w/sum(w); mu<-sum(w*p)
  c(mean_pos=mu, polarization=unname(sqrt(sum(w*(p-mu)^2))), n=sum(okp))
}

# ============================================================================
# TEST 1 — trend fit (known linear slope) + n<3 guard
# ============================================================================
cat("\n[TEST 1] OLS trend fit\n")
d_lin <- data.frame(lp = 13:20, polarization = 0.1 + 0.02 * (13:20))
f <- safe_lm(d_lin)
ok(abs(f$slope - 0.02) < 1e-12, sprintf("recovers slope 0.02 (got %.4f)", f$slope))
ok(abs(f$r2 - 1) < 1e-9, "perfect linear fit => r2 = 1")
ok(is.na(safe_lm(data.frame(lp = c(19,20), polarization = c(0.3,0.4)))$slope), "n<3 => slope NA")
# negative trend recovered too
d_neg <- data.frame(lp = 13:20, polarization = 0.5 - 0.01 * (13:20))
ok(abs(safe_lm(d_neg)$slope + 0.01) < 1e-12, "recovers negative slope")

# ============================================================================
# TEST 2 — pre/post-2017 split + delta
# ============================================================================
cat("\n[TEST 2] pre/post-2017 split\n")
dt <- data.table(lp = 13:20, polarization = c(0.10,0.12,0.11,0.13,0.12,0.14, 0.30,0.34))  # last two = post
dt[, period := ifelse(lp <= 18, "pre", "post")]
pre_mean  <- dt[period=="pre",  mean(polarization)]
post_mean <- dt[period=="post", mean(polarization)]
ok(dt[lp==18, period] == "pre"  && dt[lp==19, period] == "post", "LP18=pre, LP19=post boundary")
ok(abs(pre_mean  - mean(c(0.10,0.12,0.11,0.13,0.12,0.14))) < 1e-12, "pre mean = LP13-18 mean")
ok(abs(post_mean - mean(c(0.30,0.34))) < 1e-12, "post mean = LP19-20 mean")
ok(abs((post_mean - pre_mean) - (0.32 - 0.12)) < 1e-12, "delta = post - pre")

# ============================================================================
# TEST 3 — Dalton polarization per (lp, domain, channel) grouping
# ============================================================================
cat("\n[TEST 3] Dalton per (lp, domain, channel)\n")
g <- data.table(
  lp = rep(19L, 6), domain = "Migration",
  channel = rep(c("Speech","Manifesto"), each = 3),
  party = rep(c("CDU/CSU","SPD","AfD"), 2),
  position = c(0.1, -0.1, 0.9,   0.0, -0.2, 0.8),
  weight   = c(0.4, 0.4, 0.2,    0.4, 0.4, 0.2))
res <- g[, as.list(dalton(position, weight)), by = .(lp, domain, channel)]
ok(nrow(res) == 2L, "one row per (lp, domain, channel)")
ok(all(res$n == 3), "n_parties = 3 per channel")
ok(all(is.finite(res$polarization) & res$polarization > 0), "positive finite polarization both channels")
# manual check speech channel: mu = .4*.1 + .4*(-.1) + .2*.9 = .18
mu_sp <- 0.4*0.1 + 0.4*(-0.1) + 0.2*0.9
ok(abs(res[channel=="Speech", mean_pos] - mu_sp) < 1e-12, sprintf("speech weighted mean = %.3f", mu_sp))

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
