# ============================================================================
# 02_helpers.R — Shared analytical functions
# ----------------------------------------------------------------------------
# Pure functions used by the hypothesis scripts. No side effects, no plotting.
# ============================================================================

source(here::here("R/00_config.R"))

# ----------------------------------------------------------------------------
# Party name normalization
# Handles common variants in MARPOR / GermaParl: "GRUENE" / "B'90/Gruene" /
# "Bündnis 90/Die Grünen" / "DIE LINKE" / "CDU"+"CSU" merged etc.
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
# Map a (party, LP) cell on the speech side to its M1 manifesto.
# M1 = the manifesto from the election that brought this Bundestag into office.
# German LP -> election date table (federal elections only).
# ----------------------------------------------------------------------------
LP_ELECTION <- tibble::tribble(
  ~lp, ~election_date,
  13, as.Date("1994-10-16"),
  14, as.Date("1998-09-27"),
  15, as.Date("2002-09-22"),
  16, as.Date("2005-09-18"),
  17, as.Date("2009-09-27"),
  18, as.Date("2013-09-22"),
  19, as.Date("2017-09-24"),
  20, as.Date("2021-09-26")
)

# ----------------------------------------------------------------------------
# Soft aggregation on the speech side
# Input:  prob_mat = matrix [n_sentences x 56] of softmax probabilities,
#                    columns in MARPOR_CODES_56 order
#         lookup   = data frame with `code` and bucket column
#         bucket_col = name of the bucket column in `lookup`
#         all_buckets = full ordered vector of bucket names (so empty buckets
#                       still appear with 0)
# Returns: named numeric vector summing to <=1 (sums to 1 if every code is
#          mapped to some bucket; otherwise mass on unmapped codes is dropped).
# Steps: per-sentence bucket sums, then mean over sentences.
# ----------------------------------------------------------------------------
soft_aggregate <- function(prob_mat, lookup, bucket_col, all_buckets) {
  stopifnot(ncol(prob_mat) == 56L)
  if (nrow(prob_mat) == 0L) {
    out <- rep(0, length(all_buckets)); names(out) <- all_buckets; return(out)
  }
  # Build a 56 x n_buckets indicator matrix
  bcol <- lookup[[bucket_col]]
  ind <- matrix(0, nrow = 56L, ncol = length(all_buckets),
                dimnames = list(MARPOR_CODES_56, all_buckets))
  # Only codes mentioned in `lookup` get assigned; rest contribute zero
  for (k in seq_len(nrow(lookup))) {
    code <- as.integer(lookup$code[k])
    bk   <- bcol[k]
    if (code %in% MARPOR_CODES_56 && bk %in% all_buckets) {
      ind[as.character(code), bk] <- 1
    }
  }
  # Per-sentence bucket shares = prob_mat %*% ind  (n_sent x n_buckets)
  per_sent <- prob_mat %*% ind
  # Cell distribution = column means
  out <- colMeans(per_sent)
  out
}

# ----------------------------------------------------------------------------
# Manifesto-side aggregation from raw `per<code>` columns.
# Input:  one row of MARPOR Main Dataset (or a tibble of one row)
#         lookup, bucket_col, all_buckets as above
# Returns: named numeric vector summing to 1 over all_buckets (if every MARPOR
#          code is mapped) — for Aggregation B we additionally renormalize
#          after dropping the Andere mass; this function does NOT renormalize.
# Note: `per<code>` columns are percentages (0-100); we scale to fractions.
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
  frac_vec <- pct_vec / 100  # MARPOR percentages already sum to ~100
  out <- rep(0, length(all_buckets)); names(out) <- all_buckets
  for (k in seq_len(nrow(lookup))) {
    cd <- as.character(lookup$code[k])
    bk <- lookup[[bucket_col]][k]
    if (bk %in% all_buckets) out[bk] <- out[bk] + frac_vec[cd]
  }
  out
}

# ----------------------------------------------------------------------------
# Jensen-Shannon Divergence between two probability vectors (base-2, in [0,1]).
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
# Bootstrap interval for speech-side JSD against a fixed manifesto vector.
# Input:  prob_mat (n x 56) for a single (party, LP) cell
#         manifesto_vec (named over all_buckets, summing to 1)
#         lookup, bucket_col, all_buckets
# Returns: list(point, lo95, hi95, n_boot, n_sent)
# ----------------------------------------------------------------------------
bootstrap_jsd <- function(prob_mat, manifesto_vec, lookup, bucket_col, all_buckets,
                          n_boot = BOOTSTRAP_DRAWS, seed = BOOTSTRAP_SEED) {
  n <- nrow(prob_mat)
  if (n == 0L || all(is.na(manifesto_vec))) {
    return(list(point = NA_real_, lo95 = NA_real_, hi95 = NA_real_,
                n_boot = 0L, n_sent = n))
  }
  # Point estimate
  speech_vec <- soft_aggregate(prob_mat, lookup, bucket_col, all_buckets)
  point <- jsd(speech_vec, manifesto_vec)

  # Bootstrap
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
# Apply softmax temperature scaling row-wise.
# T < 1 sharpens, T > 1 flattens. Input must already be probabilities.
# ----------------------------------------------------------------------------
temper <- function(prob_mat, tau) {
  if (isTRUE(all.equal(tau, 1))) return(prob_mat)
  log_p <- log(pmax(prob_mat, .Machine$double.eps)) / tau
  e <- exp(log_p - apply(log_p, 1, max))      # numerical stability
  e / rowSums(e)
}

message("[02_helpers] Helpers loaded.")
