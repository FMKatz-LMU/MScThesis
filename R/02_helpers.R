# ============================================================================
# 02_helpers.R — Shared analytical functions
# ============================================================================

source(here::here("R/00_config.R"))

# ----------------------------------------------------------------------------
# Party name normalization — handles GermaParl / MARPOR naming variants.
# Returns one of PARTIES_KEEP or NA.
# ----------------------------------------------------------------------------
normalize_party <- function(x) {
  x_chr <- as.character(x)
  x_low <- tolower(x_chr)
  out <- rep(NA_character_, length(x_chr))

  out[grepl("cdu|csu|union", x_low) & !grepl("verband", x_low)] <- "CDU/CSU"
  out[grepl("\\bspd\\b|sozialdemokrat", x_low)]                 <- "SPD"
  out[grepl("\\bfdp\\b|freie demokrat", x_low)]                 <- "FDP"
  out[grepl("gr[uü]ne|b[uü]ndnis ?90", x_low)]                  <- "Grüne"
  out[grepl("\\blinke\\b|pds|linkspartei|die linke", x_low)]    <- "Linke"
  out[grepl("\\bafd\\b|alternative.*deutschland", x_low)]       <- "AfD"

  out
}

# ----------------------------------------------------------------------------
# LP -> election date table (federal elections, LPs 13–20)
# ----------------------------------------------------------------------------
LP_ELECTION <- tibble::tribble(
  ~lp, ~election_date,
  13L, as.Date("1994-10-16"),
  14L, as.Date("1998-09-27"),
  15L, as.Date("2002-09-22"),
  16L, as.Date("2005-09-18"),
  17L, as.Date("2009-09-27"),
  18L, as.Date("2013-09-22"),
  19L, as.Date("2017-09-24"),
  20L, as.Date("2021-09-26")
)

# ----------------------------------------------------------------------------
# Soft aggregation on a (n x 56) sentence probability matrix.
# Returns named numeric vector of bucket shares (sums to <=1).
# ----------------------------------------------------------------------------
soft_aggregate <- function(prob_mat, lookup, bucket_col, all_buckets) {
  stopifnot(ncol(prob_mat) == 56L)
  if (nrow(prob_mat) == 0L) {
    out <- rep(0, length(all_buckets)); names(out) <- all_buckets; return(out)
  }
  bcol <- lookup[[bucket_col]]
  ind <- matrix(0, nrow = 56L, ncol = length(all_buckets),
                dimnames = list(MARPOR_CODES_56, all_buckets))
  for (k in seq_len(nrow(lookup))) {
    code <- as.integer(lookup$code[k])
    bk   <- bcol[k]
    if (code %in% MARPOR_CODES_56 && bk %in% all_buckets)
      ind[as.character(code), bk] <- 1
  }
  colMeans(prob_mat %*% ind)
}

# ----------------------------------------------------------------------------
# Manifesto-side aggregation from raw per<code> columns.
# ----------------------------------------------------------------------------
aggregate_manifesto_row <- function(row, lookup, bucket_col, all_buckets) {
  pct_vec <- numeric(length(MARPOR_CODES_56)); names(pct_vec) <- MARPOR_CODES_56
  for (cd in MARPOR_CODES_56) {
    col <- paste0("per", cd)
    if (col %in% names(row)) {
      v <- as.numeric(row[[col]])
      if (!is.na(v)) pct_vec[as.character(cd)] <- v
    }
  }
  frac_vec <- pct_vec / 100
  out <- rep(0, length(all_buckets)); names(out) <- all_buckets
  for (k in seq_len(nrow(lookup))) {
    cd <- as.character(lookup$code[k])
    bk <- lookup[[bucket_col]][k]
    if (bk %in% all_buckets) out[bk] <- out[bk] + frac_vec[cd]
  }
  out
}

# ----------------------------------------------------------------------------
# Jensen-Shannon Divergence, base-2 (bounded in [0,1]).
# Both inputs are renormalized to sum to 1 before computation.
# ----------------------------------------------------------------------------
jsd <- function(p, q) {
  stopifnot(length(p) == length(q))
  p <- as.numeric(p); q <- as.numeric(q)
  sp <- sum(p); sq <- sum(q)
  if (sp <= 0 || sq <= 0) return(NA_real_)
  p <- p / sp; q <- q / sq
  m <- 0.5 * (p + q)
  kl <- function(a, b) {
    keep <- a > 0 & b > 0
    sum(a[keep] * log2(a[keep] / b[keep]))
  }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}

# ----------------------------------------------------------------------------
# Bootstrap JSD for a (n x 56) raw probability matrix.
# ----------------------------------------------------------------------------
bootstrap_jsd <- function(prob_mat, manifesto_vec, lookup, bucket_col, all_buckets,
                          n_boot = BOOTSTRAP_DRAWS, seed = BOOTSTRAP_SEED) {
  n <- nrow(prob_mat)
  if (n == 0L || all(is.na(manifesto_vec))) {
    return(list(point = NA_real_, lo95 = NA_real_, hi95 = NA_real_,
                n_boot = 0L, n_sent = n))
  }
  speech_vec <- soft_aggregate(prob_mat, lookup, bucket_col, all_buckets)
  point <- jsd(speech_vec, manifesto_vec)

  set.seed(seed)
  reps <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    sv  <- soft_aggregate(prob_mat[idx, , drop = FALSE], lookup, bucket_col, all_buckets)
    reps[b] <- jsd(sv, manifesto_vec)
  }
  ci <- quantile(reps, c(0.025, 0.975), na.rm = TRUE)
  list(point = point, lo95 = unname(ci[1]), hi95 = unname(ci[2]),
       n_boot = n_boot, n_sent = n)
}

# ----------------------------------------------------------------------------
# Bootstrap JSD for a pre-aggregated (n x n_buckets) bucket-share matrix.
# Faster alternative when per-sentence 56-d probabilities are unavailable:
# columns must be named by all_buckets.
# ----------------------------------------------------------------------------
bootstrap_jsd_buckets <- function(bucket_mat, manifesto_vec, all_buckets,
                                   n_boot = BOOTSTRAP_DRAWS, seed = BOOTSTRAP_SEED) {
  n <- nrow(bucket_mat)
  if (n == 0L || all(is.na(manifesto_vec))) {
    return(list(point = NA_real_, lo95 = NA_real_, hi95 = NA_real_,
                n_boot = 0L, n_sent = n))
  }
  bm <- bucket_mat[, all_buckets, drop = FALSE]
  speech_vec <- colMeans(bm)
  point <- jsd(speech_vec, manifesto_vec)

  set.seed(seed)
  reps <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    sv  <- colMeans(bm[idx, , drop = FALSE])
    reps[b] <- jsd(sv, manifesto_vec)
  }
  ci <- quantile(reps, c(0.025, 0.975), na.rm = TRUE)
  list(point = point, lo95 = unname(ci[1]), hi95 = unname(ci[2]),
       n_boot = n_boot, n_sent = n)
}

# ----------------------------------------------------------------------------
# Softmax temperature scaling (row-wise). tau < 1 sharpens, tau > 1 flattens.
# ----------------------------------------------------------------------------
temper <- function(prob_mat, tau) {
  if (isTRUE(all.equal(tau, 1))) return(prob_mat)
  log_p <- log(pmax(prob_mat, .Machine$double.eps)) / tau
  e <- exp(log_p - apply(log_p, 1, max))
  e / rowSums(e)
}

message("[02_helpers] Helpers loaded.")
