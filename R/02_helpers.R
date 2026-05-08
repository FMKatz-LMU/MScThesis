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
  out[grepl("gr[uü]n|gruen|b[uü]ndnis", x_low)]                  <- "Grüne"
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

# ----------------------------------------------------------------------------
# Hard aggregation on the speech side (argmax + threshold rule).
# Each sentence's full unit mass is routed to its argmax bucket if max-prob
# >= threshold, otherwise to a residual 'Andere' bucket.
#
# Inputs:
#   prob_mat    — n_sentences x 56 softmax probability matrix
#                 (columns in MARPOR_CODES_56 order)
#   lookup      — data frame with `code` and a bucket column
#   bucket_col  — name of the bucket column in lookup (e.g. "bucket_A")
#   all_buckets — full ordered vector of bucket names (without Andere)
#   threshold   — minimum top-1 probability to accept the argmax label
#
# Edge cases:
#   * Rows where the argmax probability equals threshold are accepted (>=).
#   * Rows whose argmax code is in MARPOR_CODES_56 but is not mapped to any
#     all_buckets entry (relevant for Aggregation B) go to 'Andere'.
#
# Returns a named numeric vector summing to 1 over c(all_buckets, 'Andere').
# ----------------------------------------------------------------------------
hard_aggregate <- function(prob_mat, lookup, bucket_col, all_buckets, threshold) {
  stopifnot(ncol(prob_mat) == 56L)
  if (nrow(prob_mat) == 0L) {
    out <- rep(0, length(all_buckets) + 1L)
    names(out) <- c(all_buckets, "Andere")
    return(out)
  }
  # code -> bucket lookup as named char vector (NA for unmapped codes).
  code_to_bucket <- setNames(rep(NA_character_, 56L), as.character(MARPOR_CODES_56))
  for (k in seq_len(nrow(lookup))) {
    cd <- as.character(lookup$code[k])
    bk <- lookup[[bucket_col]][k]
    if (cd %in% names(code_to_bucket) && bk %in% all_buckets)
      code_to_bucket[cd] <- bk
  }
  # Argmax + max-prob.
  argmax_idx  <- max.col(prob_mat, ties.method = "first")
  max_prob    <- prob_mat[cbind(seq_len(nrow(prob_mat)), argmax_idx)]
  argmax_code <- as.character(MARPOR_CODES_56[argmax_idx])
  assigned    <- code_to_bucket[argmax_code]
  # Route to Andere if below threshold OR argmax code is unmapped.
  go_andere   <- is.na(assigned) | max_prob < threshold
  assigned[go_andere] <- "Andere"
  out_levels  <- c(all_buckets, "Andere")
  tab         <- tabulate(match(assigned, out_levels), nbins = length(out_levels))
  out         <- tab / nrow(prob_mat)
  names(out)  <- out_levels
  out
}

# ----------------------------------------------------------------------------
# Per-bucket JSD contribution.
# Decomposes the scalar JSD(p, q) into per-coordinate contributions.
#   contrib_i = 0.5 * (p_i log(p_i / m_i) + q_i log(q_i / m_i))   with m = (p+q)/2
# Sum of contributions equals jsd(p, q). Both inputs are renormalized to 1.
# ----------------------------------------------------------------------------
per_bucket_jsd <- function(p, q) {
  stopifnot(length(p) == length(q), !is.null(names(p)), all(names(p) == names(q)))
  p <- p / sum(p); q <- q / sum(q)
  m <- 0.5 * (p + q)
  out <- numeric(length(p)); names(out) <- names(p)
  for (i in seq_along(p)) {
    a <- 0
    if (p[i] > 0 && m[i] > 0) a <- a + 0.5 * p[i] * log2(p[i] / m[i])
    if (q[i] > 0 && m[i] > 0) a <- a + 0.5 * q[i] * log2(q[i] / m[i])
    out[i] <- a
  }
  out
}

# ----------------------------------------------------------------------------
# Polarization score per (party, scope).
# Promoted from 05_H2.R / 06_H6.R. Single canonical signature.
#
# Sign convention (positive = right-coded, market-liberal / restrictive /
# Eurosceptic / retrenchment):
#   Economy   = (Marktlib   - Staatsint)  / (Marktlib + Staatsint)
#               (Wirtschaft Allgemein excluded from the denominator)
#   Welfare   = (Begrenzung - Ausbau)     / (Begrenzung + Ausbau)
#   Migration = (restriktiv - liberal)    / (restriktiv + liberal)
#   Europe    = (Contra-EU  - Pro-EU)     / (Contra-EU  + Pro-EU)
#
# Returns NA for a domain when the in-domain mass is zero.
# ----------------------------------------------------------------------------
polarization_score <- function(df_long, party_, scope_val, scope_col) {
  d <- df_long %>%
    filter(.data$party == party_, .data[[scope_col]] == scope_val,
           bucket %in% c(BUCKETS_B, "Andere"))

  pull_pair <- function(sub_df, pos_label, neg_label) {
    s <- sub_df$share[sub_df$bucket == pos_label]
    n <- sub_df$share[sub_df$bucket == neg_label]
    s <- if (length(s)) s else 0
    n <- if (length(n)) n else 0
    tot <- s + n
    if (tot <= 0) return(NA_real_)
    (s - n) / tot
  }

  econ <- d %>% filter(bucket %in% c("Marktliberalismus", "Staatsintervention"))
  welf <- d %>% filter(bucket %in% c("Sozialstaat Ausbau", "Sozialstaat Begrenzung"))
  migr <- d %>% filter(bucket %in% c("Migration restriktiv", "Migration liberal"))
  euro <- d %>% filter(bucket %in% c("Pro-EU", "Contra-EU"))

  tibble(
    Economy   = pull_pair(econ, "Marktliberalismus",      "Staatsintervention"),
    Welfare   = pull_pair(welf, "Sozialstaat Begrenzung", "Sozialstaat Ausbau"),
    Migration = pull_pair(migr, "Migration restriktiv",   "Migration liberal"),
    Europe    = pull_pair(euro, "Contra-EU",              "Pro-EU")
  )
}

# ----------------------------------------------------------------------------
# Dalton-style weighted dispersion of position scores.
# Promoted from 06_H6.R.
#
# Inputs: positions (numeric vector), weights (numeric vector, same length).
# weights are renormalized to sum to 1 after dropping NAs/zeros.
#   polarization = sqrt( sum_i w_i * (p_i - mu)^2 )  where mu = sum_i w_i * p_i
# Returns NA polarization if fewer than 2 valid (positions, weights).
# ----------------------------------------------------------------------------
dalton_polarization <- function(positions, weights) {
  ok <- !is.na(positions) & !is.na(weights) & weights > 0
  if (sum(ok) < 2L)
    return(tibble(mean_pos = NA_real_, polarization = NA_real_, n_parties = sum(ok)))
  p <- positions[ok]; w <- weights[ok]
  w  <- w / sum(w)
  mu <- sum(w * p)
  sigma <- sqrt(sum(w * (p - mu)^2))
  tibble(mean_pos = mu, polarization = sigma, n_parties = sum(ok))
}

message("[02_helpers] Helpers loaded.")
