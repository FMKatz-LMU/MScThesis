# ============================================================================
# 12_aggregate.R  —  SINGLE SOURCE OF TRUTH for every number from the
#                             raw v2 corpus (clean-pipeline rebuild, sprint 1d)
# ----------------------------------------------------------------------------
# Replaces the retired side-scripts apply_aggregation_AB.R and compute_jsd.R.
# ONE streaming pass over the v2 parquet produces:
#
#   speech_soft.rds     long: party, lp, scheme, filter, tau_name, code305,
#                             bucket, share, n_sent
#                       The 18-spec cross (filter × tau × code305) × {A, B}.
#   speech_method.rds   long: party, lp, scheme, method, bucket, share, n_sent
#                             method ∈ {hard_thr0.4, hard_thr0.5, tight},
#                             all on filtered / code305=incl / native τ (for 09/10).
#   manifesto_dists.rds long: party, election_date, mapping, scheme, code305,
#                             bucket, share
#   bootstrap_ci.rds    party, lp, filter, code305, jsd_point, lo95, hi95, n_sent
#                             6 bands: filter × code305 at native τ, scheme A only.
#   cell_diag.rds       party, lp, n_sent_unfiltered, n_sent_filtered,
#                             n_sent_filtered_000, mean_argmax_prob
#
# Axis semantics, primary-spec declarations, code305 proof: see 00_config.R + CLAUDE.md.
# ----------------------------------------------------------------------------
# Expected runtime (RTX-class PC): ~6–10 min stream + ~10–25 min bootstrap
# (6 bands × ~40 cells × 1000 draws). Set N_BOOT_OVERRIDE for a fast smoke test.
# ============================================================================

source(here::here("00_config.R"))

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
})

# ---- Flags -----------------------------------------------------------------
BUILD_BOOTSTRAP <- TRUE      # build per-sentence A-bucket matrix + 4 CI bands.
N_BOOT_OVERRIDE <- NULL      # set e.g. 50L for a quick smoke test; NULL = BOOTSTRAP_DRAWS.
N_BOOT <- if (is.null(N_BOOT_OVERRIDE)) BOOTSTRAP_DRAWS else N_BOOT_OVERRIDE

# Raw manifesto distributions: 56 "NNN - Title" FRACTION columns (sum to 1),
# plus legislative_period, party_label, mapping. Path hoisted for visibility.
MANIFESTO_RDS <- file.path(PROJECT_ROOT, "Data/manifesto_distributions.rds")

stopifnot(TAU_PRIMARY == 1.0)            # native τ is the bootstrap τ
NATIVE_TAU_NAME <- names(TEMPERATURES)[abs(TEMPERATURES - 1) < 1e-12]
stopifnot(length(NATIVE_TAU_NAME) == 1L)

# ---- Optional overrides for isolated end-to-end testing --------------------
# A test harness may set these globals BEFORE sourcing 12 to point it at a
# synthetic corpus. They are applied AFTER 00_config (which sets PATHS) so the
# config source does not clobber them. Undefined in normal runs -> no effect.
if (exists(".PARQUET_DIR_OVERRIDE"))   PATHS$parquet_dir <- .PARQUET_DIR_OVERRIDE
if (exists(".MANIFESTO_RDS_OVERRIDE")) MANIFESTO_RDS     <- .MANIFESTO_RDS_OVERRIDE
if (exists(".CACHE_DIR_OVERRIDE"))     PATHS$cache_dir   <- .CACHE_DIR_OVERRIDE
if (exists(".N_BOOT_OVERRIDE_EXT"))    N_BOOT            <- .N_BOOT_OVERRIDE_EXT

# 000-augmented corpus (written by 11_apply_000.py): the v2 chunks PLUS the
# per-sentence null-class columns p_000 / is_000. The whole pipeline reads THIS
# directory now; it is a strict superset of v2 (all original columns retained).
# Derived from parquet_dir so a test-harness override of parquet_dir flows
# through; an explicit .PARQUET_DIR_000_OVERRIDE can point tests straight at a
# synthetic 000 corpus.
PARQUET_DIR_000 <- if (exists(".PARQUET_DIR_000_OVERRIDE")) .PARQUET_DIR_000_OVERRIDE else
                   paste0(PATHS$parquet_dir, "_000")

# ============================================================================
# 1. Discover schema, build code→bucket indicators
# ============================================================================
chunk_files <- sort(list.files(PARQUET_DIR_000,
                               pattern = "^chunk_\\d+\\.parquet$",
                               full.names = TRUE))
if (length(chunk_files) == 0L)
  stop("No 000-augmented parquet chunks in: ", PARQUET_DIR_000,
       " — run 11_apply_000.py first (it writes p_000/is_000 to a sibling _000 dir).")
message(sprintf("[12] %d 000-augmented v2 parquet chunks found in %s.",
                length(chunk_files), PARQUET_DIR_000))

.first <- arrow::read_parquet(chunk_files[1])
prob_cols <- grep("^[0-9]{3} - ", names(.first), value = TRUE)
stopifnot(length(prob_cols) == 56L)
prob_codes_int <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
stopifnot(setequal(prob_codes_int, MARPOR_CODES_56))
# Reorder prob_cols to MARPOR_CODES_56 order so column index == code position.
prob_cols <- prob_cols[match(MARPOR_CODES_56, prob_codes_int)]

# Verify the v2 passthrough columns are present (guards against the wrong parquet).
required_cols <- c("party", "legislative_period", "sentence", "is_procedural",
                    COL_P000, COL_IS000)
miss <- setdiff(required_cols, names(.first))
if (length(miss) > 0)
  stop("[12] Missing columns in parquet — wrong/old corpus or 11_apply_000.py ",
       "not run? Missing: ", paste(miss, collapse = ", "))
rm(.first)

# Column index of code 305 in the 56-wide prob matrix (MARPOR order).
I305 <- which(MARPOR_CODES_56 == CODE_305)
stopifnot(length(I305) == 1L)

# Indicator matrices: 56 × n_bucket. A partitions all 56 (no Andere needed);
# B sends out-of-domain codes (incl. 305) to Andere.
build_indicator <- function(code_to_bucket_named, bucket_levels) {
  M <- matrix(0, nrow = 56L, ncol = length(bucket_levels),
              dimnames = list(as.character(MARPOR_CODES_56), bucket_levels))
  for (i in seq_len(56L)) {
    bk <- code_to_bucket_named[as.character(MARPOR_CODES_56[i])]
    if (is.na(bk)) bk <- "Andere"
    M[i, bk] <- 1
  }
  M
}
code_to_A <- setNames(AGG_A$bucket_A[match(MARPOR_CODES_56, AGG_A$code)],
                      as.character(MARPOR_CODES_56))
code_to_B <- setNames(AGG_B$bucket_B[match(MARPOR_CODES_56, AGG_B$code)],
                      as.character(MARPOR_CODES_56))   # NA for out-of-domain
indA <- build_indicator(code_to_A, BUCKETS_A)                 # 56 × 9
indB <- build_indicator(code_to_B, c(BUCKETS_B, "Andere"))    # 56 × 10
stopifnot(all(rowSums(indA) == 1), all(rowSums(indB) == 1))   # every code placed once

# Bucket column that holds 305 (for the excl subtraction).
COL305_A <- BUCKET_OF_305_A
COL305_B <- BUCKET_OF_305_B
stopifnot(indA[I305, COL305_A] == 1, indB[I305, COL305_B] == 1)

BUCKETS_A_OUT <- BUCKETS_A
BUCKETS_B_OUT <- c(BUCKETS_B, "Andere")

# ============================================================================
# 2. Accumulators
# ----------------------------------------------------------------------------
# One env per spec, keyed by cell "lp|party" -> list(sum_mass = vec, n = int).
# ============================================================================
new_env_ <- function() new.env(hash = TRUE)

# Soft cube spec table: scheme × filter × tau × code305.
soft_specs <- list()
for (scheme in c("A", "B"))
  for (flt in FILTERS)
    for (tn in names(TEMPERATURES))
      for (c305 in CODE305_VARIANTS)
        soft_specs[[length(soft_specs) + 1L]] <- list(
          scheme = scheme, filter = flt, tau_name = tn, code305 = c305,
          buckets = if (scheme == "A") BUCKETS_A_OUT else BUCKETS_B_OUT,
          key = paste("soft", scheme, flt, tn, c305, sep = "|"))
soft_env <- setNames(lapply(soft_specs, function(.) new_env_()),
                     vapply(soft_specs, `[[`, "", "key"))

# Method-robustness specs (filtered / incl / native only).
method_specs <- list(
  list(scheme = "A", method = "hard_thr0.4", buckets = BUCKETS_A_OUT),
  list(scheme = "A", method = "hard_thr0.5", buckets = BUCKETS_A_OUT),
  list(scheme = "B", method = "hard_thr0.4", buckets = BUCKETS_B_OUT),
  list(scheme = "B", method = "hard_thr0.5", buckets = BUCKETS_B_OUT),
  list(scheme = "A", method = "tight",       buckets = BUCKETS_A_OUT),
  list(scheme = "B", method = "tight",       buckets = BUCKETS_B_OUT))
method_env <- setNames(
  lapply(method_specs, function(.) new_env_()),
  vapply(method_specs, function(s) paste(s$method, s$scheme, sep = "|"), ""))
HARD_THRS <- c(hard_thr0.4 = 0.4, hard_thr0.5 = 0.5)

# Diagnostics.
n_sent_unf_env <- new_env_()   # unfiltered sentence count per cell
n_sent_flt_env <- new_env_()   # filtered sentence count per cell
n_sent_000_env <- new_env_()   # filtered_000 sentence count per cell (PRIMARY)
argmax_sum_env <- new_env_()   # sum of pred top-1 prob per cell (unfiltered)
argmax_n_env   <- new_env_()

bump <- function(env, key, vec, n) {
  cur <- env[[key]]
  env[[key]] <- if (is.null(cur)) list(sum_mass = vec, n = n)
                else list(sum_mass = cur$sum_mass + vec, n = cur$n + n)
  invisible(NULL)
}
bump_s <- function(env, key, x) {
  cur <- env[[key]]; env[[key]] <- if (is.null(cur)) x else cur + x; invisible(NULL)
}

# Per-sentence storage for the bootstrap (scheme A, native τ):
# 9 incl-bucket shares + p305 + is_procedural + lp + party-index.
if (BUILD_BOOTSTRAP) {
  boot_BS_chunks  <- vector("list", length(chunk_files))  # n × 9 (incl)
  boot_p305_chunks<- vector("list", length(chunk_files))  # n
  boot_proc_chunks<- vector("list", length(chunk_files))  # n logical
  boot_is000_chunks<- vector("list", length(chunk_files)) # n logical (null-class)
  boot_lp_chunks  <- vector("list", length(chunk_files))  # n int
  boot_pty_chunks <- vector("list", length(chunk_files))  # n int (PARTIES_KEEP idx)
}

# ---- per-row bucket shares: BS = P_t %*% ind, excl subtracts 305 mass --------
bucket_shares <- function(P_t, ind, col305, mode) {
  BS <- P_t %*% ind
  if (mode == "excl") BS[, col305] <- BS[, col305] - P_t[, I305]
  BS
}

# ============================================================================
# 3. Stream
# ============================================================================
t0 <- Sys.time()
for (i in seq_along(chunk_files)) {
  chunk <- as.data.table(arrow::read_parquet(chunk_files[i]))
  party_norm <- normalize_party(chunk$party)
  keep <- !is.na(party_norm) & party_norm %in% PARTIES_KEEP &
          chunk$legislative_period %in% LEGISLATIVE_PERIODS
  if (!any(keep)) next
  chunk <- chunk[keep]; party_norm <- party_norm[keep]

  P <- as.matrix(chunk[, ..prob_cols])         # n × 56, MARPOR order
  storage.mode(P) <- "double"
  is_proc <- as.logical(chunk$is_procedural)   # TRUE = procedural (drop when filtered)
  keep_flt <- !is_proc                         # filtered = non-procedural rows
  p000     <- as.numeric(chunk[[COL_P000]])    # null-class probability (from 20)
  is_000   <- p000 >= FILTER000_THRESHOLD      # re-thresholded HERE (R-side knob)
  keep_000 <- keep_flt & !is_000               # filtered_000 = !is_proc & !is_000 (PRIMARY)
  lp_vec  <- chunk$legislative_period
  group   <- paste(lp_vec, party_norm, sep = "|")

  # Diagnostics: argmax prob (unfiltered) and counts.
  amax_idx <- max.col(P, ties.method = "first")
  max_prob <- P[cbind(seq_len(nrow(P)), amax_idx)]

  # ---- SOFT cube: per τ temper once, then per scheme×code305 accumulate ----
  for (tn in names(TEMPERATURES)) {
    P_t <- temper(P, TEMPERATURES[[tn]])       # native τ=1 -> P unchanged
    for (scheme in c("A", "B")) {
      ind <- if (scheme == "A") indA else indB
      col305 <- if (scheme == "A") COL305_A else COL305_B
      for (c305 in CODE305_VARIANTS) {
        BS <- bucket_shares(P_t, ind, col305, c305)      # n × n_bucket
        # unfiltered
        su <- rowsum(BS, group, reorder = FALSE)
        nu <- as.integer(table(group)[rownames(su)])
        env <- soft_env[[paste("soft", scheme, "unfiltered", tn, c305, sep = "|")]]
        for (k in seq_len(nrow(su))) bump(env, rownames(su)[k], su[k, ], nu[k])
        # filtered (non-procedural rows only)
        if (any(keep_flt)) {
          BSf <- BS[keep_flt, , drop = FALSE]; gf <- group[keep_flt]
          sf <- rowsum(BSf, gf, reorder = FALSE)
          nf <- as.integer(table(gf)[rownames(sf)])
          envf <- soft_env[[paste("soft", scheme, "filtered", tn, c305, sep = "|")]]
          for (k in seq_len(nrow(sf))) bump(envf, rownames(sf)[k], sf[k, ], nf[k])
        }
        # filtered_000 (non-procedural AND non-null-class rows; PRIMARY spec)
        if (any(keep_000)) {
          BS0 <- BS[keep_000, , drop = FALSE]; g0 <- group[keep_000]
          s0 <- rowsum(BS0, g0, reorder = FALSE)
          n0 <- as.integer(table(g0)[rownames(s0)])
          env0 <- soft_env[[paste("soft", scheme, "filtered_000", tn, c305, sep = "|")]]
          for (k in seq_len(nrow(s0))) bump(env0, rownames(s0)[k], s0[k, ], n0[k])
        }
      }
    }
  }

  # ---- METHOD robustness on the PRIMARY filter (filtered_000) / incl / native
  # (decision (b): hard-threshold + tight methods sit on the SAME corpus as the
  # primary soft baseline, so 09's soft-vs-hard contrast varies only the method.)
  if (any(keep_000)) {
    Pf  <- P[keep_000, , drop = FALSE]
    gf  <- group[keep_000]
    amf <- amax_idx[keep_000]; mpf <- max_prob[keep_000]
    # Hard threshold (argmax routed to its bucket if max_prob >= thr, else Andere).
    for (mname in names(HARD_THRS)) {
      thr <- HARD_THRS[[mname]]
      for (scheme in c("A", "B")) {
        blevels <- if (scheme == "A") BUCKETS_A_OUT else BUCKETS_B_OUT
        c2b <- if (scheme == "A") code_to_A else code_to_B
        assigned <- c2b[as.character(MARPOR_CODES_56[amf])]
        assigned[is.na(assigned) | mpf < thr] <- "Andere"
        # A has no Andere column; route sub-threshold A mass to Andere only if
        # it exists — for A we instead keep an Andere column to hold it so shares
        # stay interpretable (matches the legacy hard spec).
        lev <- if (scheme == "A") c(BUCKETS_A, "Andere") else BUCKETS_B_OUT
        M <- matrix(0, nrow = length(assigned), ncol = length(lev),
                    dimnames = list(NULL, lev))
        M[cbind(seq_along(assigned), match(assigned, lev))] <- 1
        sm <- rowsum(M, gf, reorder = FALSE)
        nn <- as.integer(table(gf)[rownames(sm)])
        env <- method_env[[paste(mname, scheme, sep = "|")]]
        for (k in seq_len(nrow(sm))) bump(env, rownames(sm)[k], sm[k, ], nn[k])
      }
    }
    # Tight pre-filter (soft, native τ, sentence-length filter on top of the
    # PRIMARY filter). NOTE: now layered on filtered_000, so it is largely
    # subsumed by the null-class filter — kept only as a documented robustness.
    n_words <- lengths(strsplit(chunk$sentence[keep_000], "\\s+"))
    tmask <- n_words >= TIGHT_FILTER$min_words_per_sentence
    if (any(tmask)) {
      Pt2 <- Pf[tmask, , drop = FALSE]; gt <- gf[tmask]
      for (scheme in c("A", "B")) {
        ind <- if (scheme == "A") indA else indB
        BS <- Pt2 %*% ind
        sm <- rowsum(BS, gt, reorder = FALSE)
        nn <- as.integer(table(gt)[rownames(sm)])
        env <- method_env[[paste("tight", scheme, sep = "|")]]
        for (k in seq_len(nrow(sm))) bump(env, rownames(sm)[k], sm[k, ], nn[k])
      }
    }
  }

  # ---- Diagnostics ---------------------------------------------------------
  tu <- table(group)
  for (k in seq_along(tu)) bump_s(n_sent_unf_env, names(tu)[k], as.integer(tu[k]))
  if (any(keep_flt)) {
    tf <- table(group[keep_flt])
    for (k in seq_along(tf)) bump_s(n_sent_flt_env, names(tf)[k], as.integer(tf[k]))
  }
  if (any(keep_000)) {
    tf0 <- table(group[keep_000])
    for (k in seq_along(tf0)) bump_s(n_sent_000_env, names(tf0)[k], as.integer(tf0[k]))
  }
  for (k in seq_along(max_prob)) {
    bump_s(argmax_sum_env, group[k], max_prob[k]); bump_s(argmax_n_env, group[k], 1L)
  }

  # ---- Bootstrap storage (scheme A, native τ) ------------------------------
  if (BUILD_BOOTSTRAP) {
    BS_A <- P %*% indA                       # n × 9 (incl), native τ
    boot_BS_chunks[[i]]   <- BS_A
    boot_p305_chunks[[i]] <- P[, I305]
    boot_proc_chunks[[i]] <- is_proc
    boot_is000_chunks[[i]]<- is_000
    boot_lp_chunks[[i]]   <- lp_vec
    boot_pty_chunks[[i]]  <- match(party_norm, PARTIES_KEEP)
  }

  if (i %% 50L == 0L || i == length(chunk_files))
    message(sprintf("[12]   chunk %d / %d  (%.0fs)", i, length(chunk_files),
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
rm(chunk, P); invisible(gc(verbose = FALSE))

# ============================================================================
# 4. Finalize accumulators -> long tibbles (renormalise each cell to sum 1)
# ============================================================================
finalize <- function(env, buckets, extra) {
  keys <- ls(env)
  if (!length(keys)) return(NULL)
  rbindlist(lapply(keys, function(k) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    cur <- env[[k]]
    sh <- cur$sum_mass / cur$n
    s <- sum(sh); if (s > 0) sh <- sh / s          # cell renorm (real for excl)
    data.table(lp = as.integer(parts[1]), party = parts[2],
               bucket = buckets, share = as.numeric(sh), n_sent = cur$n)[,
      (names(extra)) := extra]
  }))
}

speech_soft <- rbindlist(lapply(soft_specs, function(s)
  finalize(soft_env[[s$key]], s$buckets,
           list(scheme = s$scheme, filter = s$filter,
                tau_name = s$tau_name, code305 = s$code305))),
  use.names = TRUE)
setcolorder(speech_soft, c("party","lp","scheme","filter","tau_name","code305",
                           "bucket","share","n_sent"))

speech_method <- rbindlist(lapply(method_specs, function(s) {
  bk <- if (s$scheme == "A") c(BUCKETS_A, "Andere") else BUCKETS_B_OUT  # hard A has Andere
  if (s$method == "tight") bk <- s$buckets                              # tight = soft buckets
  finalize(method_env[[paste(s$method, s$scheme, sep = "|")]], bk,
           list(scheme = s$scheme, method = s$method))
}), use.names = TRUE)
setcolorder(speech_method, c("party","lp","scheme","method","bucket","share","n_sent"))

message(sprintf("[12] speech_soft: %d rows (%d cells × specs); speech_method: %d rows.",
                nrow(speech_soft), nrow(unique(speech_soft[, .(party, lp)])),
                nrow(speech_method)))

# ============================================================================
# 5. Manifesto side (both code305 variants, both schemes)
# ============================================================================
message("[12] Aggregating manifesto side (code305 incl + excl) ...")
mf <- as.data.table(readRDS(MANIFESTO_RDS))
mf_prob <- grep("^[0-9]{3} - ", names(mf), value = TRUE)
stopifnot(length(mf_prob) == 56L)
mf_codes <- as.integer(sub("^([0-9]{3}).*", "\\1", mf_prob))
stopifnot(setequal(mf_codes, MARPOR_CODES_56))
mf_prob <- mf_prob[match(MARPOR_CODES_56, mf_codes)]   # MARPOR order
stopifnot(all(c("legislative_period","party_label","mapping") %in% names(mf)))

Mmf <- as.matrix(mf[, ..mf_prob]); storage.mode(Mmf) <- "double"
# Rows should already sum to 1 (fractions). Guard.
rs <- rowSums(Mmf)
if (any(abs(rs - 1) > 1e-6)) Mmf <- Mmf / rs        # defensive renorm

manifesto_long <- rbindlist(lapply(c("A", "B"), function(scheme) {
  ind <- if (scheme == "A") indA else indB
  col305 <- if (scheme == "A") COL305_A else COL305_B
  bk <- if (scheme == "A") BUCKETS_A_OUT else BUCKETS_B_OUT
  rbindlist(lapply(CODE305_VARIANTS, function(c305) {
    BS <- Mmf %*% ind                                  # nrow(mf) × n_bucket
    if (c305 == "excl") BS[, col305] <- BS[, col305] - Mmf[, I305]
    BS <- BS / rowSums(BS)                              # cell renorm
    dt <- data.table(
      party_label = mf$party_label, mapping = mf$mapping,
      lp = mf$legislative_period)
    long <- dt[rep(seq_len(nrow(dt)), each = length(bk))]
    long[, bucket := rep(bk, nrow(dt))]
    long[, share  := as.numeric(t(BS))]
    long[, scheme := scheme][, code305 := c305]
    long
  }))
}), use.names = TRUE)

# Map party_label -> PARTIES_KEEP; attach election_date by lp.
MANIFESTO_PARTY_MAP <- c("CDU_CSU_joint"="CDU/CSU","SPD"="SPD","FDP"="FDP",
                         "GRUENE"="Grüne","DIE LINKE"="Linke","AfD"="AfD")
manifesto_long[, party := MANIFESTO_PARTY_MAP[party_label]]
manifesto_long <- manifesto_long[!is.na(party) & party %in% PARTIES_KEEP]
le <- as.data.table(LP_ELECTION)
manifesto_long <- merge(manifesto_long, le, by = "lp", all.x = TRUE)
manifesto_dists <- manifesto_long[, .(party, election_date, mapping, scheme,
                                       code305, bucket, share)]
message(sprintf("[12] manifesto_dists: %d rows (%d party×mapping×code305 cells).",
                nrow(manifesto_dists),
                nrow(unique(manifesto_dists[, .(party, mapping, code305)]))))

# ============================================================================
# 6. Cell diagnostics
# ============================================================================
cell_keys <- ls(n_sent_unf_env)
cell_diag <- rbindlist(lapply(cell_keys, function(k) {
  parts <- strsplit(k, "|", fixed = TRUE)[[1]]
  data.table(lp = as.integer(parts[1]), party = parts[2],
             n_sent_unfiltered = n_sent_unf_env[[k]],
             n_sent_filtered   = if (!is.null(n_sent_flt_env[[k]])) n_sent_flt_env[[k]] else 0L,
             n_sent_filtered_000 = if (!is.null(n_sent_000_env[[k]])) n_sent_000_env[[k]] else 0L,
             mean_argmax_prob  = argmax_sum_env[[k]] / argmax_n_env[[k]])
}))[order(party, lp)]

# ============================================================================
# 7. Bootstrap — 4 bands (filter × code305) at native τ, scheme A
# ============================================================================
bootstrap_ci <- NULL
if (BUILD_BOOTSTRAP) {
  message(sprintf("[12] Bootstrap: assembling per-sentence A-matrix (%d draws/band) ...",
                  N_BOOT))
  BS_all  <- do.call(rbind, boot_BS_chunks);   rm(boot_BS_chunks)
  p305_all<- unlist(boot_p305_chunks);         rm(boot_p305_chunks)
  proc_all<- unlist(boot_proc_chunks);         rm(boot_proc_chunks)
  is000_all<- unlist(boot_is000_chunks);       rm(boot_is000_chunks)
  lp_all  <- unlist(boot_lp_chunks);           rm(boot_lp_chunks)
  pty_all <- unlist(boot_pty_chunks);          rm(boot_pty_chunks)
  invisible(gc(verbose = FALSE))
  colnames(BS_all) <- BUCKETS_A
  party_all <- PARTIES_KEEP[pty_all]
  group_all <- paste(lp_all, party_all, sep = "|")
  # Per-cell row index, computed ONCE (avoids re-scanning the 5.75M-row group
  # vector for every band × cell).
  cell_idx_all <- split(seq_along(group_all), group_all)

  # Exclusion rule (same as the thesis): no manifesto / PDS LP13-15 / FDP LP18 /
  # filtered n_sent < N_SENT_MIN. Bootstrap only cells that survive AND have a
  # manifesto match for the given code305.
  is_excluded <- function(party, lp)
    party %in% c("parteilos","NA",NA_character_) |
    (party == "Linke" & lp %in% 13:15) |      # PDS years, no MARPOR manifesto
    (party == "FDP" & lp == 18L)

  m1 <- manifesto_dists[mapping == "M1_entering" & scheme == "A"]

  cells <- unique(data.table(lp = lp_all, party = party_all))
  cells <- cells[!is_excluded(party, lp)]
  # filtered n_sent gate
  cells <- merge(cells, cell_diag[, .(party, lp, n_sent_filtered)],
                 by = c("party","lp"), all.x = TRUE)
  cells <- cells[n_sent_filtered >= N_SENT_MIN]

  boot_band <- function(flt, c305) {
    mref <- m1[code305 == c305]
    out <- vector("list", nrow(cells))
    for (j in seq_len(nrow(cells))) {
      lp_j <- cells$lp[j]; pt_j <- cells$party[j]
      idx <- cell_idx_all[[paste(lp_j, pt_j, sep = "|")]]
      if (is.null(idx)) next
      if (flt == "filtered")     idx <- idx[!proc_all[idx]]
      if (flt == "filtered_000") idx <- idx[!proc_all[idx] & !is000_all[idx]]
      if (!length(idx)) next
      BM <- BS_all[idx, , drop = FALSE]
      if (c305 == "excl") BM[, COL305_A] <- BM[, COL305_A] - p305_all[idx]
      # manifesto vector for THIS cell: match party AND the cell's LP (via the
      # entering-election date). Matching party only would pick the wrong LP's
      # manifesto (duplicate bucket names collapse to the first election).
      ed_j <- LP_ELECTION$election_date[match(lp_j, LP_ELECTION$lp)]
      mv_long <- mref[party == pt_j & election_date == ed_j]
      if (!nrow(mv_long)) next
      mv <- setNames(mv_long$share, mv_long$bucket)[BUCKETS_A]; mv[is.na(mv)] <- 0
      bs <- bootstrap_jsd_buckets(BM, mv, BUCKETS_A,
                                  n_boot = N_BOOT, seed = BOOTSTRAP_SEED + j)
      out[[j]] <- data.table(party = pt_j, lp = lp_j, filter = flt, code305 = c305,
                             jsd_point = bs$point, lo95 = bs$lo95, hi95 = bs$hi95,
                             n_sent = bs$n_sent)
    }
    rbindlist(out)
  }

  bands <- list()
  for (flt in FILTERS) for (c305 in CODE305_VARIANTS) {
    message(sprintf("[12]   bootstrap band: %s × %s", flt, c305))
    bands[[length(bands)+1L]] <- boot_band(flt, c305)
  }
  bootstrap_ci <- rbindlist(bands, use.names = TRUE)
  # Mark the primary band explicitly.
  bootstrap_ci[, is_primary := (filter == BOOTSTRAP_PRIMARY_FILTER &
                                code305 == BOOTSTRAP_PRIMARY_CODE305)]
  rm(BS_all, p305_all, proc_all, is000_all); invisible(gc(verbose = FALSE))
}

# ============================================================================
# 8. Sanity checks
# ============================================================================
check_sums <- function(dt, by_cols, label) {
  rs <- dt[, .(s = sum(share)), by = by_cols]
  bad <- rs[abs(s - 1) > 1e-6]
  if (nrow(bad) > 0)
    warning(sprintf("[12] row-sum check FAILED for %s: %d groups (max dev %.2e)",
                    label, nrow(bad), max(abs(bad$s - 1))))
  else message(sprintf("[12] row-sum OK: %s", label))
}
check_sums(speech_soft, c("party","lp","scheme","filter","tau_name","code305"), "speech_soft")
check_sums(speech_method, c("party","lp","scheme","method"), "speech_method")
check_sums(manifesto_dists, c("party","election_date","mapping","scheme","code305"), "manifesto_dists")

low_n <- cell_diag[n_sent_filtered < N_SENT_MIN]
if (nrow(low_n) > 0) {
  message("[12] cells with filtered n_sent < ", N_SENT_MIN, " (review for exclusion):")
  print(low_n[, .(party, lp, n_sent_filtered, n_sent_unfiltered)])
}
low_n0 <- cell_diag[n_sent_filtered_000 < N_SENT_MIN]
if (nrow(low_n0) > 0) {
  message("[12] cells with filtered_000 n_sent < ", N_SENT_MIN,
          " (PRIMARY spec — review; bootstrap gate is on n_sent_filtered):")
  print(low_n0[, .(party, lp, n_sent_filtered_000, n_sent_filtered, n_sent_unfiltered)])
}

# ============================================================================
# 9. Save
# ============================================================================
dir_create(PATHS$cache_dir)
saveRDS(speech_soft,    file.path(PATHS$cache_dir, "speech_soft.rds"))
saveRDS(speech_method,  file.path(PATHS$cache_dir, "speech_method.rds"))
saveRDS(manifesto_dists,file.path(PATHS$cache_dir, "manifesto_dists.rds"))
saveRDS(cell_diag,      file.path(PATHS$cache_dir, "cell_diag.rds"))
if (BUILD_BOOTSTRAP)
  saveRDS(bootstrap_ci, file.path(PATHS$cache_dir, "bootstrap_ci.rds"))

message(sprintf("[12] DONE (%.0fs). Caches in %s",
                as.numeric(difftime(Sys.time(), t0, units = "secs")), PATHS$cache_dir))
if (BUILD_BOOTSTRAP && !is.null(bootstrap_ci)) {
  message("[12] Primary bootstrap band (filtered_000 × excl), first rows:")
  print(bootstrap_ci[is_primary == TRUE][order(party, lp)][1:min(6, .N)])
}
