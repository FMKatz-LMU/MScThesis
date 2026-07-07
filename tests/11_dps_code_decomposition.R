# ============================================================================
# 11_dps_code_decomposition.R — Per-code decomposition of the DPS bucket
#                               + admissibility analysis of the 305 exclusion
# ----------------------------------------------------------------------------
# Spec: filtered, native τ, code305 = INCL, Aggregation A. (The 305 artefact
# must be PRESENT to decompose it, hence incl.) speech_soft is already bucket-
# aggregated, so this script runs its OWN light single parquet pass to obtain
# per-cell 56-dim mean softmax vectors, then resolves the DPS bucket into its
# 9 constituent codes.
#
# PART 1 — Per-code DPS decomposition.
#   17-element partition = {9 DPS codes} ∪ {8 other A-buckets}. Per-cell
#   per_bucket_jsd over this partition. 305's contribution / (Σ DPS-code
#   contributions) = 305's share of the DPS divergence (the v2 analog of the
#   old ~79.6%; it WILL shift on v2). A mass-based |Δ|-share is reported as a
#   cross-check.
#
# PART 2 — Admissibility of the 305 exclusion (the central methodological test).
#   The discriminator is NOT confidence (institutional/procedural language is
#   genuinely high-confidence 305). It is the speech-vs-manifesto 305 ASYMMETRY:
#     manifesto-305  ≈ a genuine common SOCKEL, matched on both sides, hence
#                      divergence-NEUTRAL (it cancels in JSD);
#     speech-305 EXCESS (speech_305 − manifesto_305) = the genre artefact, which
#                      is what actually DRIVES the 305 divergence.
#   excl removes the whole 305 dimension symmetrically: mostly the artefactual
#   excess (good), plus the matched sockel (costless, since divergence-neutral).
#   The only honest residual cost is any GENUINE speech-vs-manifesto authority-
#   emphasis difference, bounded by the asymmetry and (Part 3) the gold check.
#   A pre-stated verdict with an explicit disconfirmation path is printed.
#
# PART 3 — Gold-standard 305 precision on the FILTERED corpus (decisive human
#   check). Reads the v2 gold result when available; otherwise reports pending.
#
# Outputs: dps_percode_decomposition.csv, dps_percode_summary.csv,
#   dps_305_asymmetry.csv, figures/dps_percode_bars.{pdf,png},
#   figures/dps_305_asymmetry.{pdf,png}
# ============================================================================

source(here::here("00_config.R"))
source(here::here("02_helpers.R"))
suppressPackageStartupMessages({ library(tidyverse); library(data.table); library(arrow) })

DPS_BUCKET <- BUCKET_OF_305_A
M1         <- "M1_entering"
MANIFESTO_RDS <- file.path(PROJECT_ROOT, "Data/manifesto_distributions.rds")
MANIFESTO_PARTY_MAP <- c("CDU_CSU_joint"="CDU/CSU","SPD"="SPD","FDP"="FDP",
                         "GRUENE"="Grüne","DIE LINKE"="Linke","AfD"="AfD")
GOLD_305_FILE <- file.path(PATHS$out_dir, "gold_305_precision_v2.csv")  # written by the gold workstream

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[11][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[11][FLAG] ", msg)

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
# Speech side — own light parquet pass: per-cell mean 56-dim (filtered, native)
# ============================================================================
chunk_files <- sort(list.files(PATHS$parquet_dir, pattern = "^chunk_\\d+\\.parquet$", full.names = TRUE))
chk(length(chunk_files) > 0, paste0("no parquet chunks in ", PATHS$parquet_dir))
.first <- arrow::read_parquet(chunk_files[1])
prob_cols <- grep("^[0-9]{3} - ", names(.first), value = TRUE)
chk(length(prob_cols) == 56L, "expected 56 prob columns")
prob_codes_int <- as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols))
prob_cols <- prob_cols[match(MARPOR_CODES_56, prob_codes_int)]   # MARPOR order => col index == code position
chk(all(c("party","legislative_period","is_procedural") %in% names(.first)), "parquet missing passthrough columns")
rm(.first)

acc <- new.env(hash = TRUE)   # key "lp|party" -> list(sum56, n)
bump56 <- function(key, v, n) { cur <- acc[[key]]
  acc[[key]] <- if (is.null(cur)) list(s = v, n = n) else list(s = cur$s + v, n = cur$n + n) }

message(sprintf("[11] Streaming %d parquet chunks (filtered, native, 56-dim cell means) ...", length(chunk_files)))
for (i in seq_along(chunk_files)) {
  ch <- as.data.table(arrow::read_parquet(chunk_files[i]))
  pnorm <- normalize_party(ch$party)
  keep <- !is.na(pnorm) & pnorm %in% PARTIES_KEEP & ch$legislative_period %in% LEGISLATIVE_PERIODS & !as.logical(ch$is_procedural)
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
message("[11] PART 1 — per-code DPS decomposition (17-element partition) ...")
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
message("[11] PART 2 — 305 asymmetry / admissibility ...")
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
# PART 3 — gold-standard 305 precision on the filtered corpus (if available)
# ============================================================================
gold_precision <- NA_real_; gold_note <- "PENDING (v2 gold re-validation not yet run)"
if (file.exists(GOLD_305_FILE)) {
  g <- as.data.table(fread(GOLD_305_FILE))
  cand <- intersect(c("precision_305","precision","share_confirmed"), names(g))
  if (length(cand) >= 1L) { gold_precision <- as.numeric(g[[cand[1]]][1])
    gold_note <- sprintf("v2 gold: %.1f%% of filtered-corpus AI-305 predictions confirmed by human coder", 100*gold_precision) }
}

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
       subtitle = sprintf("Mean per-cell JSD contribution of each DPS code (filtered, native, incl). Code 305 (highlighted) carries %.0f%% of the DPS divergence on average.",
                          100*mean(percode$share_305_of_dps_jsd, na.rm = TRUE)),
       x = "Mean JSD contribution (17-element partition)", y = NULL,
       caption = "If 305 dominates, the DPS-bucket divergence is a single-code (procedural-authority) phenomenon, not a substantive democracy-topic disagreement.") +
  theme_thesis()
ggsave(file.path(PATHS$fig_dir, "dps_percode_bars.pdf"), p_codes, width = 9, height = 6)
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
ggsave(file.path(PATHS$fig_dir, "dps_305_asymmetry.pdf"), p_asym, width = 8, height = 7)
ggsave(file.path(PATHS$fig_dir, "dps_305_asymmetry.png"), p_asym, width = 8, height = 7, dpi = 200, bg = "white")

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
cat("\n  --- pre-stated verdict (admissibility of the 305 exclusion) ---\n")
if (is.finite(mean_excess) && mean_excess > 0 && frac_excess_pos >= 0.8) {
  cat("  SUPPORTS excl: 305 mass is systematically speech-inflated relative to the matched\n")
  cat("  manifesto sockel, i.e. a one-sided genre artefact that drives the DPS divergence.\n")
  cat("  excl removes mostly this excess; the matched sockel it also removes is divergence-neutral.\n")
} else {
  cat("  DOES NOT SUPPORT excl on this evidence: 305 mass is NOT systematically speech-inflated,\n")
  cat("  so excl would risk removing a genuine speech-vs-manifesto authority-emphasis signal.\n")
  cat("  -> reconsider excl as primary; report incl-vs-excl as a sensitivity instead.\n")
}
cat("\n==================== PART 3: GOLD 305 PRECISION (filtered corpus) ====================\n")
cat("  ", gold_note, "\n", sep = "")
if (is.finite(gold_precision)) {
  if (gold_precision < 0.30)
    cat(sprintf("  Low precision (%.0f%%) corroborates the artefact reading: most AI-305 on the filtered\n  corpus is not human-confirmed substantive 305 -> excl admissible.\n", 100*gold_precision))
  else
    cat(sprintf("  Precision %.0f%% is non-trivial: a material share of AI-305 is human-confirmed -> treat\n  excl cautiously and lean on the asymmetry bound.\n", 100*gold_precision))
}
message("\n[11] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
