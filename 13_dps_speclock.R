# ============================================================================
# 13_dps_speclock.R — Per-code decomposition of the DPS bucket
#                               + admissibility analysis of the 305 exclusion
# ----------------------------------------------------------------------------
# Spec: filtered_000 (the PRIMARY null-class-filtered corpus), native τ, code305
# = INCL, Aggregation A. (The 305 artefact must be PRESENT to decompose it, hence
# incl; the null-class filter removes the SENTENCES that inflate 305, not 305
# itself.) speech_soft is already bucket-
# aggregated, so this script runs its OWN light single parquet pass to obtain
# per-cell 56-dim mean softmax vectors, then resolves the DPS bucket into its
# 9 constituent codes.
#
# PART 1 — Per-code DPS decomposition.
#   17-element partition = {9 DPS codes} ∪ {8 other A-buckets}. Per-cell
#   per_bucket_jsd over this partition. 305's contribution / (Σ DPS-code
#   contributions) = 305's share of the DPS divergence (v2: 67.6% mean across
#   the 38 cells; the old v1 value was ~79.6%). A mass-based |Δ|-share is reported as a
#   cross-check.
#
# PART 2 — Does the null-class filter remove the 305 artefact? (central test, on filtered_000).
#   The discriminator is NOT confidence (institutional/procedural language is
#   genuinely high-confidence 305). It is the speech-vs-manifesto 305 ASYMMETRY:
#     manifesto-305  ≈ a genuine common SOCKEL, matched on both sides, hence
#                      divergence-NEUTRAL (it cancels in JSD);
#     speech-305 EXCESS (speech_305 − manifesto_305) = the genre artefact, which
#                      is what actually DRIVES the 305 divergence.
#   On the PRIMARY filtered_000 corpus the artefactual excess should COLLAPSE,
#   because the null-class filter has already removed the sentences that inflate
#   305. A collapsed excess => the residual 305 is the matched (divergence-neutral)
#   sockel => incl is the honest primary and the surgical excl is redundant. A
#   surviving excess => the null-class filter under-corrected, and excl still
#   removes real artefact. A pre-stated verdict with a disconfirmation path is printed.
#
# PART 3 — Gold-standard 305 precision on the filtered_000 corpus (decisive human
#   check). Reads the v2 gold result when available; otherwise reports pending.
#
# PART 4 — H1b Migration directional sensitivity (601-asymmetry). Re-splits the B
#   Migration domain to the clean Multiculturalism pair only (restriktiv = 608,
#   liberal = 607), dropping the national-way-of-life codes 601/602, and compares
#   the within-Migration speech-vs-manifesto JSD against the 601+608 / 602+607
#   baseline. Lives here because it needs the per-code parquet pass above; reported
#   as an EXPLORATORY H1b robustness arm (gold κ(B) = 0.456). Salience-A Migration untouched.
#
# Outputs: dps_percode_decomposition.csv, dps_percode_summary.csv,
#   dps_305_asymmetry.csv, h1b_migration_601608_sensitivity.csv,
#   h1b_migration_601608_summary.csv, figures/dps_percode_bars.{pdf,png},
#   figures/dps_305_asymmetry.{pdf,png}, figures/h1b_migration_601608_sensitivity.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table); library(arrow) })

DPS_BUCKET <- BUCKET_OF_305_A
M1         <- "M1_entering"
MANIFESTO_RDS <- file.path(PROJECT_ROOT, "Data/manifesto_distributions.rds")
MANIFESTO_PARTY_MAP <- c("CDU_CSU_joint"="CDU/CSU","SPD"="SPD","FDP"="FDP",
                         "GRUENE"="Grüne","DIE LINKE"="Linke","AfD"="AfD")
GOLD_305_FILE <- file.path(PATHS$out_dir, "goldstandard", "gold_305_precision_v2.csv")  # written by 13 into the goldstandard/ subdir

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[13][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[13][FLAG] ", msg)

# ---- 56-code layout + 17-element DPS partition -----------------------------
code_to_A <- setNames(AGG_A$bucket_A[match(MARPOR_CODES_56, AGG_A$code)], as.character(MARPOR_CODES_56))
chk(!any(is.na(code_to_A)), "AGG_A does not cover all 56 codes")
I305      <- which(MARPOR_CODES_56 == CODE_305); chk(length(I305) == 1L, "code 305 not found in MARPOR_CODES_56")
DPS_codes <- MARPOR_CODES_56[code_to_A == DPS_BUCKET]            # 9 codes incl. 305
chk(CODE_305 %in% DPS_codes, "code 305 not in the DPS bucket")
other_buckets <- setdiff(BUCKETS_A, DPS_BUCKET)                  # 8 buckets
labels17  <- c(paste0("code_", DPS_codes), other_buckets)
dps_idx   <- match(DPS_codes, MARPOR_CODES_56)
other_idx <- lapply(other_buckets, function(b) which(code_to_A == b))
names(other_idx) <- other_buckets
to17 <- function(v56) setNames(c(v56[dps_idx], vapply(other_idx, function(ix) sum(v56[ix]), 0.0)), labels17)
# self-test the partition on a uniform vector
.u <- to17(rep(1/56, 56)); chk(abs(sum(.u) - 1) < 1e-12, "to17 does not preserve total mass")

# ---- valid cells -----------------------------------------------------------
cache <- function(f) { p <- file.path(PATHS$cache_dir, f); chk(file.exists(p), paste0("missing cache: ", p)); as.data.table(readRDS(p)) }
valid <- cache("bootstrap_ci.rds")[is_primary == TRUE, .(party, lp = as.integer(lp))]
chk(nrow(valid) > 0, "no is_primary cells")

# ============================================================================
# Speech side — own light parquet pass: per-cell mean 56-dim (filtered_000, native)
# ============================================================================
# Read the 000-augmented corpus (same derivation as 03; P2: derived, not in config).
PARQUET_DIR_000 <- if (exists(".PARQUET_DIR_000_OVERRIDE")) .PARQUET_DIR_000_OVERRIDE else
                   paste0(PATHS$parquet_dir, "_000")
chunk_files <- sort(list.files(PARQUET_DIR_000, pattern = "^chunk_\\d+\\.parquet$", full.names = TRUE))
chk(length(chunk_files) > 0, paste0("no 000-augmented parquet chunks in ", PARQUET_DIR_000,
                                    " — run 11_apply_000.py first"))
.first <- arrow::read_parquet(chunk_files[1])
prob_cols <- grep("^[0-9]{3} - ", names(.first), value = TRUE)
chk(length(prob_cols) == 56L, "expected 56 prob columns")
prob_codes_int <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
prob_cols <- prob_cols[match(MARPOR_CODES_56, prob_codes_int)]   # MARPOR order => col index == code position
chk(all(c("party","legislative_period","is_procedural", COL_P000, COL_IS000) %in% names(.first)),
    "parquet missing passthrough/000 columns — wrong corpus or 11_apply_000.py not run")
rm(.first)

acc <- new.env(hash = TRUE)   # key "lp|party" -> list(sum56, n)
bump56 <- function(key, v, n) { cur <- acc[[key]]
  acc[[key]] <- if (is.null(cur)) list(s = v, n = n) else list(s = cur$s + v, n = cur$n + n) }

message(sprintf("[13] Streaming %d parquet chunks (filtered_000, native, 56-dim cell means) ...", length(chunk_files)))
for (i in seq_along(chunk_files)) {
  ch <- as.data.table(arrow::read_parquet(chunk_files[i]))
  pnorm <- normalize_party(ch$party)
  keep <- !is.na(pnorm) & pnorm %in% PARTIES_KEEP & ch$legislative_period %in% LEGISLATIVE_PERIODS &
          !as.logical(ch$is_procedural) & !(as.numeric(ch[[COL_P000]]) >= FILTER000_THRESHOLD)   # filtered_000 = !proc & !is_000
  if (!any(keep)) next
  P <- as.matrix(ch[keep, ..prob_cols]); storage.mode(P) <- "double"   # native => raw fractions
  grp <- paste(ch$legislative_period[keep], pnorm[keep], sep = "|")
  su  <- rowsum(P, grp, reorder = FALSE)
  nn  <- as.integer(table(grp)[rownames(su)])
  for (k in seq_len(nrow(su))) bump56(rownames(su)[k], su[k, ], nn[k])
}
speech56 <- rbindlist(lapply(ls(acc), function(k) {
  pr <- strsplit(k, "|", fixed = TRUE)[[1]]
  data.table(lp = as.integer(pr[1]), party = pr[2], n_sent = acc[[k]]$n,
             t(acc[[k]]$s / acc[[k]]$n))                      # 56-dim cell mean
}))
setnames(speech56, c("lp","party","n_sent", as.character(MARPOR_CODES_56)))
speech56 <- merge(speech56, valid, by = c("party","lp"))
chk(nrow(speech56) > 0, "no valid speech cells after streaming/merge")
flag(all(abs(rowSums(as.matrix(speech56[, as.character(MARPOR_CODES_56), with = FALSE])) - 1) < 1e-6),
     "speech 56-dim cell means do not sum to 1")

# ============================================================================
# Manifesto side — per-cell 56-dim (M1)
# ============================================================================
mf <- as.data.table(readRDS(MANIFESTO_RDS))
mf_prob <- grep("^[0-9]{3} - ", names(mf), value = TRUE)
mf_codes <- as.integer(sub("^([0-9]{3}).*", "\\1", mf_prob))
mf_prob <- mf_prob[match(MARPOR_CODES_56, mf_codes)]
Mmf <- as.matrix(mf[, ..mf_prob]); storage.mode(Mmf) <- "double"
rs <- rowSums(Mmf); if (any(abs(rs - 1) > 1e-6)) Mmf <- Mmf / rs
man56 <- data.table(party = MANIFESTO_PARTY_MAP[mf$party_label], mapping = mf$mapping,
                    lp = as.integer(mf$legislative_period))
man56 <- cbind(man56, as.data.table(Mmf))
setnames(man56, c("party","mapping","lp", as.character(MARPOR_CODES_56)))
man56 <- man56[mapping == M1 & !is.na(party) & party %in% PARTIES_KEEP]
man56 <- merge(man56, valid, by = c("party","lp"))

# ============================================================================
# PART 1 — per-code DPS decomposition
# ============================================================================
message("[13] PART 1 — per-code DPS decomposition (17-element partition) ...")
code_cols <- as.character(MARPOR_CODES_56)
rows <- list(); s305 <- numeric(0)
for (k in seq_len(nrow(valid))) {
  p <- valid$party[k]; L <- valid$lp[k]
  sv <- speech56[party==p & lp==L]; mv <- man56[party==p & lp==L]
  if (nrow(sv) != 1L || nrow(mv) != 1L) next
  s56 <- as.numeric(sv[, ..code_cols]); m56 <- as.numeric(mv[, ..code_cols])
  s17 <- to17(s56); m17 <- to17(m56)
  cc  <- per_bucket_jsd(s17, m17)                       # 17 contributions, sum == jsd(s17,m17)
  chk(abs(sum(cc) - jsd(s17, m17)) < 1e-9, sprintf("17-element Σ contrib != jsd at %s LP%d", p, L))
  dps_lbls   <- paste0("code_", DPS_codes)
  dps_div    <- sum(cc[dps_lbls])                       # DPS divergence (code-resolved)
  c305       <- unname(cc[paste0("code_", CODE_305)])
  share_jsd  <- if (dps_div > 0) c305 / dps_div else NA_real_
  # mass-based |Δ| cross-check within DPS codes
  d_abs      <- abs(s56[dps_idx] - m56[dps_idx]); names(d_abs) <- as.character(DPS_codes)
  share_mass <- if (sum(d_abs) > 0) d_abs[as.character(CODE_305)] / sum(d_abs) else NA_real_
  rows[[length(rows)+1L]] <- data.table(party=p, lp=L,
    dps_jsd_codes = dps_div, code305_contrib = c305,
    share_305_of_dps_jsd = share_jsd, share_305_of_dps_absdelta = unname(share_mass))
  s305 <- c(s305, share_jsd)
}
percode <- rbindlist(rows)
flag(all(percode$share_305_of_dps_jsd >= -1e-12 & percode$share_305_of_dps_jsd <= 1 + 1e-12, na.rm = TRUE),
     "305 share of DPS JSD outside [0,1]")
fwrite(percode, file.path(PATHS$out_dir, "dps_percode_decomposition.csv"))

# mean per-code contribution across cells (for the bar figure)
percode_long <- rbindlist(lapply(seq_len(nrow(valid)), function(k) {
  p <- valid$party[k]; L <- valid$lp[k]
  sv <- speech56[party==p & lp==L]; mv <- man56[party==p & lp==L]
  if (nrow(sv) != 1L || nrow(mv) != 1L) return(NULL)
  cc <- per_bucket_jsd(to17(as.numeric(sv[, ..code_cols])), to17(as.numeric(mv[, ..code_cols])))
  data.table(party=p, lp=L, element=names(cc), contrib=unname(cc))
}))
dps_lbls <- paste0("code_", DPS_codes)
percode_summary <- percode_long[element %in% dps_lbls,
  .(mean_contrib = mean(contrib), sd_contrib = sd(contrib), n_cells = .N), by = element][order(-mean_contrib)]
fwrite(percode_summary, file.path(PATHS$out_dir, "dps_percode_summary.csv"))

# ============================================================================
# PART 2 — speech-vs-manifesto 305 asymmetry (admissibility of excl)
# ============================================================================
message("[13] PART 2 — 305 asymmetry / admissibility ...")
c305col <- as.character(CODE_305)   # "305" (non-syntactic column name)
sp_305 <- speech56[, .(party, lp, speech_305 = .SD[[1]]), .SDcols = c305col]
mf_305 <- man56[,    .(party, lp, manifesto_305 = .SD[[1]]), .SDcols = c305col]
asym <- merge(sp_305, mf_305, by = c("party","lp"))
asym[, excess := speech_305 - manifesto_305]
asym[, ratio  := fifelse(manifesto_305 > 0, speech_305 / manifesto_305, NA_real_)]
setorder(asym, -excess)
fwrite(asym, file.path(PATHS$out_dir, "dps_305_asymmetry.csv"))

mean_speech_305 <- mean(asym$speech_305); mean_man_305 <- mean(asym$manifesto_305)
mean_excess     <- mean(asym$excess);     frac_excess_pos <- mean(asym$excess > 0)

# ============================================================================
# PART 3 — gold-standard 305 precision on the filtered_000 corpus (if available)
# ============================================================================
gold_precision <- NA_real_; gold_note <- "PENDING (gold_305_precision_v2.csv not found)"
if (file.exists(GOLD_305_FILE)) {
  g <- as.data.table(fread(GOLD_305_FILE))
  cand <- intersect(c("precision_305","precision","share_confirmed","prec_305","precision305",
                      "p305_precision","share_305_confirmed","confirmed_share","gold_precision"),
                    names(g))
  if (length(cand) == 0L) {
    # fall back ONLY if exactly one numeric column is a proportion in [0,1] (unambiguous)
    num_cols <- names(g)[vapply(g, function(x) is.numeric(x) && length(x) >= 1L &&
                                 is.finite(x[1]) && x[1] >= 0 && x[1] <= 1, logical(1))]
    if (length(num_cols) == 1L) cand <- num_cols
  }
  if (length(cand) >= 1L) {
    gold_precision <- as.numeric(g[[cand[1]]][1])
    gold_note <- sprintf("v2 gold: %.1f%% of filtered_000-corpus AI-305 predictions confirmed by human coder (column '%s')",
                         100*gold_precision, cand[1])
  } else {
    gold_note <- sprintf("gold_305_precision_v2.csv found but no recognised precision column. Columns present: %s",
                         paste(names(g), collapse = ", "))
  }
}

# ============================================================================
# PART 4 — H1b Migration directional sensitivity (601-asymmetry robustness)
#   ManifestoBERTa predicts only HB4 three-digit codes, so 601 (National Way of
#   Life: positive) conflates national-sovereignty with immigration-restrictive
#   content, and 602 is its mirror. The B-baseline folds 601 into "Migration
#   restriktiv" (601+608) and 602 into "Migration liberal" (602+607). This arm
#   drops the national-way-of-life pair and keeps only the clean Multiculturalism
#   pair — restriktiv = 608, liberal = 607 — then recomputes the within-Migration
#   directional split (speech vs manifesto) and its 2-bin JSD per cell. Reuses the
#   per-code cell means from the parquet pass above (no extra I/O). H1b/B layer is
#   EXPLORATORY (gold κ(B) = 0.456); the salience-A Migration bucket is untouched.
# ============================================================================
message("[13] PART 4 — H1b Migration 601/608 directional sensitivity ...")

# guard the code partition against AGG_B drift (codes are read, not assumed)
.agg_b <- as.data.table(AGG_B)
chk(setequal(.agg_b[bucket_B == "Migration restriktiv", code], c(601L, 608L)),
    "AGG_B 'Migration restriktiv' != {601,608} — Migration sensitivity codes are stale")
chk(setequal(.agg_b[bucket_B == "Migration liberal", code], c(602L, 607L)),
    "AGG_B 'Migration liberal' != {602,607} — Migration sensitivity codes are stale")
chk(all(c("601","602","607","608") %in% code_cols), "Migration codes missing from the 56-code layout")

# within-Migration restriktiv share = restr / (restr + lib); NA if a side has no
# Migration mass. mig_mass = the (full or MC) domain mass, kept to flag thin cells.
mig_split <- function(dt, restr_codes, lib_codes) {
  R   <- rowSums(as.matrix(dt[, ..restr_codes]))
  L   <- rowSums(as.matrix(dt[, ..lib_codes]))
  tot <- R + L
  data.table(party = dt$party, lp = dt$lp,
             restr_share = fifelse(tot > 0, R / tot, NA_real_), mig_mass = tot)
}
sp_base <- mig_split(speech56, c("601","608"), c("602","607"))   # baseline split
mf_base <- mig_split(man56,    c("601","608"), c("602","607"))
sp_mc   <- mig_split(speech56, "608", "607")                      # Multiculturalism-only
mf_mc   <- mig_split(man56,    "608", "607")
setnames(sp_base, c("restr_share","mig_mass"), c("restr_sp_base","mass_sp"))
setnames(mf_base, c("restr_share","mig_mass"), c("restr_mf_base","mass_mf"))
setnames(sp_mc,   c("restr_share","mig_mass"), c("restr_sp_mc","mass_sp_mc"))
setnames(mf_mc,   c("restr_share","mig_mass"), c("restr_mf_mc","mass_mf_mc"))

mig <- Reduce(function(a, b) merge(a, b, by = c("party","lp")),
              list(sp_base, mf_base, sp_mc, mf_mc))
jsd2 <- function(a, b) if (is.na(a) || is.na(b)) NA_real_ else jsd(c(a, 1 - a), c(b, 1 - b))
mig[, jsd_mig_baseline := mapply(jsd2, restr_sp_base, restr_mf_base)]
mig[, jsd_mig_mc       := mapply(jsd2, restr_sp_mc,   restr_mf_mc)]
mig[, delta_jsd        := jsd_mig_mc - jsd_mig_baseline]
mig <- mig[, .(party, lp, restr_sp_base, restr_mf_base, jsd_mig_baseline,
               restr_sp_mc, restr_mf_mc, jsd_mig_mc, delta_jsd, mass_sp, mass_mf)]
setorder(mig, -delta_jsd)
fwrite(mig, file.path(PATHS$out_dir, "h1b_migration_601608_sensitivity.csv"))

# independent-path cross-check: the baseline restriktiv share from the parquet
# pass must equal speech_soft's scheme-B "Migration restriktiv" within-domain
# share (same filtered_000 × native × incl spec, derived two different ways).
ss_b <- as.data.table(readRDS(file.path(PATHS$cache_dir, "speech_soft.rds")))[
  scheme == "B" & filter == FILTER_PRIMARY & tau_name == "native" &
  code305 == CODE305_PRIMARY_DEFAULT & bucket %in% c("Migration restriktiv","Migration liberal"),
  .(party, lp = as.integer(lp), bucket, share)]
ss_b <- dcast(ss_b, party + lp ~ bucket, value.var = "share", fill = 0)
ss_b[, restr_ss := fifelse((`Migration restriktiv` + `Migration liberal`) > 0,
                           `Migration restriktiv` / (`Migration restriktiv` + `Migration liberal`), NA_real_)]
xchk_mig <- merge(mig[, .(party, lp, restr_sp_base)], ss_b[, .(party, lp, restr_ss)], by = c("party","lp"))
maxd_mig <- max(abs(xchk_mig$restr_sp_base - xchk_mig$restr_ss), na.rm = TRUE)
message(sprintf("[13][PART4][cross-check] max |Migration baseline split (parquet) - speech_soft share| = %.2e", maxd_mig))
flag(is.finite(maxd_mig) && maxd_mig < 1e-6,
     sprintf("Migration baseline split != speech_soft scheme-B share (max %.2e) — 03/11 corpus-mask mismatch?", maxd_mig))

mig_summary <- mig[, .(
  n_cells            = sum(!is.na(jsd_mig_baseline)),
  mean_jsd_baseline  = mean(jsd_mig_baseline, na.rm = TRUE),
  mean_jsd_mc        = mean(jsd_mig_mc, na.rm = TRUE),
  mean_delta_jsd     = mean(delta_jsd, na.rm = TRUE),
  mean_restr_sp_base = mean(restr_sp_base, na.rm = TRUE),
  mean_restr_sp_mc   = mean(restr_sp_mc, na.rm = TRUE))]
fwrite(mig_summary, file.path(PATHS$out_dir, "h1b_migration_601608_summary.csv"))

# ============================================================================
# Figures
# ============================================================================
p_codes <- percode_summary %>% as_tibble() %>%
  mutate(element = fct_reorder(element, mean_contrib), is305 = element == paste0("code_", CODE_305)) %>%
  ggplot(aes(mean_contrib, element, fill = is305)) +
  geom_col(width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c(`FALSE` = "#264653", `TRUE` = "#E76F51"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Per-code decomposition of the DPS-bucket divergence",
       subtitle = sprintf("Mean per-cell JSD contribution of each DPS code (full filter, native tau, code 305 incl). Code 305 (highlighted) carries %.0f%% of the DPS divergence on average.",
                          100*mean(percode$share_305_of_dps_jsd, na.rm = TRUE)),
       x = "Mean JSD contribution (17-element partition)", y = NULL,
       caption = "If 305 dominates, the DPS-bucket divergence is a single-code (procedural-authority) phenomenon, not a substantive democracy-topic disagreement.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "dps_percode_bars.pdf"), p_codes, width = 9, height = 6, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "dps_percode_bars.png"), p_codes, width = 9, height = 6, dpi = 200, bg = "white")

lim <- max(asym$speech_305, asym$manifesto_305) * 1.05
p_asym <- asym %>% as_tibble() %>% mutate(party = factor(party, levels = PARTIES_KEEP)) %>%
  ggplot(aes(manifesto_305, speech_305, colour = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)), size = 2.5, max.overlaps = 12, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  coord_equal(xlim = c(0, lim), ylim = c(0, lim)) +
  labs(title = "Speech-vs-manifesto code-305 asymmetry",
       subtitle = "Each point is a (party, LP) cell. Points above the 45° line = speech over-emphasizes procedural-authority (305) language.",
       x = "Manifesto 305 share (genuine common sockel)", y = "Speech 305 share",
       caption = "The vertical gap above 45° is the genre artefact that excl removes; the on-line component is the matched, divergence-neutral sockel.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "dps_305_asymmetry.pdf"), p_asym, width = 8, height = 7, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "dps_305_asymmetry.png"), p_asym, width = 8, height = 7, dpi = 200, bg = "white")

# PART 4 figure — Migration directional JSD: baseline (601+608) vs Multiculturalism-only (608)
mig_fig <- mig[!is.na(jsd_mig_baseline) & !is.na(jsd_mig_mc)]
lim_m   <- max(c(mig_fig$jsd_mig_baseline, mig_fig$jsd_mig_mc, 1e-6), na.rm = TRUE) * 1.05
p_mig <- mig_fig %>% as_tibble() %>% mutate(party = factor(party, levels = PARTIES_KEEP)) %>%
  ggplot(aes(jsd_mig_baseline, jsd_mig_mc, colour = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = paste0(party, " ", lp)), size = 2.5, max.overlaps = 12, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, name = NULL) +
  coord_equal(xlim = c(0, lim_m), ylim = c(0, lim_m)) +
  labs(title = "Migration directional sensitivity: dropping the 601/602 national-identity codes",
       subtitle = "Per-(party, LP) within-Migration JSD (restrictive vs liberal). x: baseline 601+608 / 602+607. y: Multiculturalism-only 608 / 607.",
       x = "Migration JSD — baseline (incl. 601/602)", y = "Migration JSD — 607/608 only",
       caption = "Points off the 45° line = the national-way-of-life codes (601/602) materially move the directional Migration measure. H1b is validated at κ(B) = 0.456.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "h1b_migration_601608_sensitivity.pdf"), p_mig, width = 8, height = 7, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "h1b_migration_601608_sensitivity.png"), p_mig, width = 8, height = 7, dpi = 200, bg = "white")

# ============================================================================
# Console summary + pre-stated verdict
# ============================================================================
cat("\n==================== PART 1: 305 SHARE OF DPS DIVERGENCE ====================\n")
cat(sprintf("  mean 305 share of DPS JSD (code-resolved): %.1f%%\n", 100*mean(percode$share_305_of_dps_jsd, na.rm=TRUE)))
cat(sprintf("  mean 305 share of DPS |Δ| (mass cross-chk): %.1f%%\n", 100*mean(percode$share_305_of_dps_absdelta, na.rm=TRUE)))
cat("\n==================== PART 2: 305 ASYMMETRY ====================\n")
cat(sprintf("  mean speech-305 share   : %.4f\n", mean_speech_305))
cat(sprintf("  mean manifesto-305 share: %.4f  (genuine common sockel)\n", mean_man_305))
cat(sprintf("  mean excess (speech-mf) : %+.4f\n", mean_excess))
cat(sprintf("  cells with speech > mf  : %.0f%%\n", 100*frac_excess_pos))
cat("\n  --- pre-stated verdict (does the null-class filter remove the 305 artefact?) ---\n")
if (is.finite(mean_excess) && mean_excess > 0 && frac_excess_pos >= 0.8) {
  cat("  RESIDUAL ARTEFACT on filtered_000: even after the null-class filter, speech-305 is\n")
  cat("  still systematically inflated over the matched manifesto sockel. The 000-filter\n")
  cat("  UNDER-corrected -> a genuine residual speech-305 over-emphasis remains, so 305-excl\n")
  cat("  still removes real artefact. Treat excl as more than a robustness arm (reconsider it\n")
  cat("  closer to primary, or tighten FILTER000_THRESHOLD).\n")
} else {
  cat("  ARTEFACT REMOVED AT SOURCE: on filtered_000 the speech-305 excess over the matched\n")
  cat("  manifesto sockel has collapsed. The null-class filter removed the SENTENCES that\n")
  cat("  inflated 305, so the residual 305 is the genuine, divergence-neutral sockel.\n")
  cat("  -> incl is the honest primary (keeps the matched sockel); the surgical 305-excl is\n")
  cat("     redundant, reported as a robustness arm only. (Confirms the 10 redundancy check.)\n")
}
cat("\n==================== PART 3: GOLD 305 PRECISION (filtered_000 corpus) ====================\n")
cat("  ", gold_note, "\n", sep = "")
if (is.finite(gold_precision)) {
  if (gold_precision < 0.30)
    cat(sprintf("  Low precision (%.0f%%) corroborates the artefact reading: most AI-305 on the filtered_000\n  corpus is not human-confirmed substantive 305 -> excl admissible.\n", 100*gold_precision))
  else
    cat(sprintf("  Precision %.0f%% is non-trivial: a material share of AI-305 is human-confirmed -> treat\n  excl cautiously and lean on the asymmetry bound.\n", 100*gold_precision))
}
cat("\n==================== PART 4: MIGRATION 601/608 DIRECTIONAL SENSITIVITY ====================\n")
cat(sprintf("  cells with Migration mass on both sides : %d\n", mig_summary$n_cells))
cat(sprintf("  mean within-Migration JSD  baseline (601+608 / 602+607): %.4f\n", mig_summary$mean_jsd_baseline))
cat(sprintf("  mean within-Migration JSD  607/608 only               : %.4f\n", mig_summary$mean_jsd_mc))
cat(sprintf("  mean Δ (607/608-only − baseline)                      : %+.4f\n", mig_summary$mean_delta_jsd))
cat(sprintf("  mean speech restriktiv-share  baseline %.3f -> 607/608-only %.3f\n",
            mig_summary$mean_restr_sp_base, mig_summary$mean_restr_sp_mc))
if (is.finite(mig_summary$mean_delta_jsd)) {
  if (abs(mig_summary$mean_delta_jsd) < 0.02)
    cat("  -> dropping 601/602 barely moves the directional Migration measure: the 601-asymmetry\n     does not materially distort H1b's Migration domain (report baseline; note the check).\n")
  else
    cat("  -> dropping 601/602 shifts the directional Migration measure: the 601-asymmetry matters;\n     report the 607/608-only arm alongside the baseline.\n")
}
message("\n[13] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
