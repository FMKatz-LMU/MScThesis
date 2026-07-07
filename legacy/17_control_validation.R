# ============================================================================
# 17_control_validation.R — Sonnet (Silber, Referenz) vs. ManifestoBERTa (Prüfling)
#                           auf der korpus-repräsentativen 10k-Kontrollstichprobe
# ----------------------------------------------------------------------------
# Misst BERT gegen die breite, blind kodierte Sonnet-Kontrollinstanz. Anders als
# 14 (kleines, bucket-stratifiziertes Gold) ist dieses Sample KORPUS-GEWICHTET
# (einfache Zufallsziehung) -> die κ sind die korpus-repräsentative Reliabilität.
# Sonnet ist an den 259 Gold-Sätzen validiert (κ ~0.68 vs. Mensch); hier ist es
# die Referenz, BERT der Prüfling. KEINE menschliche Wahrheit im Spiel.
#
# Spiegelt die Maße aus 13/14:
#   A Domäne (10, inkl. 000) · B Richtung (streng) · F Feincode (56)
#   + codeable-only-κ (fair für BERT) · 305≡000-Sensitivität · Null-Klassen-Analyse
#   + Konfidenz-Stratifizierung (BERT pred_score UND Sonnet-confidence)
#   + Soft Agreement (falls Probs vorhanden) · Per-Party-κ
#   + Korpus-Verteilungsvergleich inkl. JSD zwischen den zwei Kodierern
#
# Aufruf:  source(here::here("17_control_validation.R"))
# ============================================================================

source(here::here("00_config.R"))     # PATHS, AGG_A, AGG_B
source(here::here("02_helpers.R"))

suppressPackageStartupMessages({ library(data.table); library(arrow) })

GS_DIR  <- file.path(PATHS$out_dir, "goldstandard")
SONNET  <- file.path(GS_DIR, "control_sample_10k_sonnet.csv")
PARQUET <- file.path(GS_DIR, "control_sample_10k.parquet")
NULL000 <- "Nicht zuordenbar (000)"

# ---- Helfer: Kappa, Per-Klassen-PRF, JSD (selbst implementiert, wie in 14) --
cohen_kappa <- function(a, b) {
  ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
  if (!length(a)) return(list(n = 0L, po = NA, kappa = NA))
  lv <- union(unique(a), unique(b)); a <- factor(a, lv); b <- factor(b, lv); n <- length(a)
  po <- mean(a == b)
  pa <- as.numeric(table(a)) / n; pb <- as.numeric(table(b)) / n
  pe <- sum(pa * pb)
  list(n = n, po = po, kappa = if (pe < 1) (po - pe) / (1 - pe) else NA)
}
per_class <- function(ref, pred, classes = NULL) {
  ok <- !is.na(ref) & !is.na(pred); ref <- ref[ok]; pred <- pred[ok]
  if (is.null(classes)) classes <- sort(union(unique(ref), unique(pred)))
  rbindlist(lapply(classes, function(k) {
    tp <- sum(pred == k & ref == k); fp <- sum(pred == k & ref != k); fn <- sum(pred != k & ref == k)
    prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA_real_
    data.table(class = k, support = sum(ref == k), precision = prec, recall = rec, f1 = f1)
  }))
}
wF1 <- function(prf) sum(prf$support * prf$f1, na.rm = TRUE) / sum(prf$support[!is.na(prf$f1)])
jsd <- function(p, q) {
  lv <- union(names(p), names(q))
  p <- setNames(as.numeric(p[lv]), lv); q <- setNames(as.numeric(q[lv]), lv)
  p[is.na(p)] <- 0; q[is.na(q)] <- 0; p <- p / sum(p); q <- q / sum(q); m <- 0.5 * (p + q)
  kl <- function(a, b) { i <- a > 0; sum(a[i] * log2(a[i] / b[i])) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}
fix_code <- function(x) {
  x <- trimws(as.character(x)); num <- suppressWarnings(as.integer(x))
  ifelse(!is.na(num), sprintf("%03d", num), x)
}

# ---- Daten laden + Codes -> Buckets (identischer Crosswalk für beide) -------
dat <- fread(SONNET, colClasses = list(character = c("marpor")))
dat <- dat[status == "ok"]
dat[, bert_code   := fix_code(pred_code)]
dat[, sonnet_code := fix_code(marpor)]

a_lk  <- setNames(AGG_A$bucket_A, sprintf("%03d", AGG_A$code))
bd_lk <- setNames(AGG_B$domain_B, sprintf("%03d", AGG_B$code))
bb_lk <- setNames(AGG_B$bucket_B, sprintf("%03d", AGG_B$code))
a_lk["000"] <- NULL000   # 000-Code -> Restkategorie-Bucket

dat[, bert_A   := a_lk[bert_code]]
dat[, sonnet_A := a_lk[sonnet_code]]

# Richtungs-Domänen datengetrieben: bucket_A mit > 1 bucket_B
cw <- data.table(code = names(a_lk), A = a_lk[names(a_lk)], B = bb_lk[names(a_lk)])
dir_doms <- cw[!is.na(B), .(nB = uniqueN(B)), by = A][nB > 1, A]
norm_B <- function(A, B) { B <- as.character(B); B[is.na(B) & A %in% dir_doms] <- "keine klare Richtung"; B }
dat[, bert_B   := norm_B(bert_A,   bb_lk[bert_code])]
dat[, sonnet_B := norm_B(sonnet_A, bb_lk[sonnet_code])]

N <- nrow(dat)
A_CLASSES <- union(unique(dat$sonnet_A), unique(dat$bert_A))
out <- list()  # für CSV-Export
cat(sprintf("\n[17] Kontrollstichprobe: %d kodierte Sätze (Sonnet=Referenz, BERT=Prüfling)\n", N))

# ============================================================================
# 1) ÜBERBLICK: A / B / F  (BERT vs. Sonnet)
# ============================================================================
kA <- cohen_kappa(dat$bert_A, dat$sonnet_A)
prfA <- per_class(dat$sonnet_A, dat$bert_A, A_CLASSES)

mB <- dat[sonnet_A %in% dir_doms]
kB <- cohen_kappa(mB$bert_B, mB$sonnet_B)
prfB <- per_class(mB$sonnet_B, mB$bert_B)

kF <- cohen_kappa(dat$bert_code, dat$sonnet_code)

overview <- data.table(
  Ebene    = c("A – Domäne (10, inkl. 000)", "B – Richtung (streng)", "F – Feincode (56)"),
  n        = c(kA$n, kB$n, kF$n),
  Accuracy = round(c(kA$po, kB$po, kF$po), 3),
  Kappa    = round(c(kA$kappa, kB$kappa, kF$kappa), 3),
  gew_F1   = round(c(wF1(prfA), wF1(prfB), NA), 3)
)
out[["control_agreement_overview.csv"]] <- overview
cat("\n== 1) Überblick (BERT vs. Sonnet) ==\n"); print(overview, row.names = FALSE)

# ============================================================================
# 2) CODEABLE-ONLY: fairer Vergleich für BERT (Sonnet-000 ausschließen)
# ============================================================================
cod <- dat[sonnet_A != NULL000]
kA_cod <- cohen_kappa(cod$bert_A, cod$sonnet_A)
flavors <- data.table(
  Teilmenge = c("roh (inkl. 000)", "codeable-only (ohne Sonnet-000)"),
  n         = c(kA$n, kA_cod$n),
  Accuracy  = round(c(kA$po, kA_cod$po), 3),
  Kappa     = round(c(kA$kappa, kA_cod$kappa), 3)
)
flavors[, Delta_Kappa := round(Kappa - Kappa[1], 3)]
out[["control_agreement_kappa_flavors.csv"]] <- flavors
cat("\n== 2) Kappa: roh vs. codeable-only (Domäne A) ==\n"); print(flavors, row.names = FALSE)

# ============================================================================
# 3) PER-KLASSEN-PRF auf Domänenebene (Sonnet als Referenz)
# ============================================================================
prfA_out <- prfA[order(-support)]
prfA_out[, `:=`(precision = round(precision, 3), recall = round(recall, 3), f1 = round(f1, 3))]
out[["control_prf_A_domaene.csv"]] <- prfA_out
cat("\n== 3) Per-Domäne P/R/F1 (Referenz = Sonnet) ==\n"); print(prfA_out, row.names = FALSE)

prfB_out <- prfB[order(-support)]
prfB_out[, `:=`(precision = round(precision, 3), recall = round(recall, 3), f1 = round(f1, 3))]
out[["control_prf_B_richtung.csv"]] <- prfB_out

# ============================================================================
# 4) KONFUSION Domäne (Sonnet-Zeile x BERT-Spalte)
# ============================================================================
confA <- dcast(dat[, .N, by = .(sonnet_A, bert_A)], sonnet_A ~ bert_A, value.var = "N", fill = 0)
out[["control_confusion_A.csv"]] <- confA

# ============================================================================
# 5) NULL-KLASSEN-ANALYSE (Befund 2, korpus-gewichtet)
# ============================================================================
s000 <- mean(dat$sonnet_code == "000")
b000 <- mean(dat$bert_code   == "000")
proc_rate <- if ("is_procedural" %in% names(dat))
  dat[, .(sonnet_000 = round(mean(sonnet_code == "000"), 3)), by = is_procedural] else NULL
# wohin schiebt BERT die Sonnet-000?
where <- dat[sonnet_code == "000", .N, by = bert_A][order(-N)]
where[, anteil := round(N / sum(N), 3)]
n_bert305_of_s000 <- dat[sonnet_code == "000" & bert_code == "305", .N]
nullinfo <- data.table(
  Maß = c("Sonnet sagt 000 (Anteil)", "BERT sagt 000 (Anteil)",
          "Sonnet-000, die BERT als 305 kodiert", "Sonnet-000 gesamt"),
  Wert = c(round(s000, 3), round(b000, 3), n_bert305_of_s000, sum(dat$sonnet_code == "000"))
)
out[["control_nullclass_summary.csv"]] <- nullinfo
out[["control_nullclass_where_bert_puts_them.csv"]] <- where
cat("\n== 5) Null-Klasse ==\n"); print(nullinfo, row.names = FALSE)
cat("Wohin BERT die Sonnet-000-Sätze legt:\n"); print(where, row.names = FALSE)
if (!is.null(proc_rate)) { cat("Sonnet-000-Rate nach is_procedural:\n"); print(proc_rate, row.names = FALSE) }

# ============================================================================
# 6) 305 ≡ 000 SENSITIVITÄT
# ============================================================================
dat[, bert_code_r := fifelse(bert_code == "305", "000", bert_code)]
dat[, bert_A_r := a_lk[bert_code_r]]
kA_r  <- cohen_kappa(dat$bert_A_r, dat$sonnet_A)
kF_r  <- cohen_kappa(dat$bert_code_r, dat$sonnet_code)
prfA_r <- per_class(dat$sonnet_A, dat$bert_A_r, A_CLASSES)
get_prf <- function(tab, cls, m) { v <- tab[class == cls][[m]]; if (length(v)) v else NA_real_ }
dem_label <- a_lk["305"]   # exakt der Bucket, in dem 305 liegt — robust ggü. Benennung
eff <- data.table(
  Klasse = c(rep(NULL000, 2), rep(dem_label, 3)),
  Maß    = c("Recall", "F1", "Precision", "Recall", "F1"),
  original = round(c(get_prf(prfA, NULL000, "recall"), get_prf(prfA, NULL000, "f1"),
                     get_prf(prfA, dem_label, "precision"), get_prf(prfA, dem_label, "recall"),
                     get_prf(prfA, dem_label, "f1")), 3),
  remap_305_000 = round(c(get_prf(prfA_r, NULL000, "recall"), get_prf(prfA_r, NULL000, "f1"),
                          get_prf(prfA_r, dem_label, "precision"), get_prf(prfA_r, dem_label, "recall"),
                          get_prf(prfA_r, dem_label, "f1")), 3)
)
eff[, Delta := round(remap_305_000 - original, 3)]
sens <- data.table(
  Maß = c("Accuracy A", "Kappa A", "Kappa Feincode"),
  original = round(c(kA$po, kA$kappa, kF$kappa), 3),
  remap_305_000 = round(c(kA_r$po, kA_r$kappa, kF_r$kappa), 3)
)
out[["control_bert305_equiv000_overall.csv"]] <- sens
out[["control_bert305_equiv000_affected.csv"]] <- eff
cat("\n== 6) 305 ≡ 000 ==\n"); print(sens, row.names = FALSE); print(eff, row.names = FALSE)

# ============================================================================
# 7) KONFIDENZ-STRATIFIZIERUNG
# ============================================================================
# (a) nach BERT pred_score (3 Bins) — Agreement + codeable-κ
dat[, conf_bin := cut(as.numeric(pred_score), breaks = c(-Inf, 0.5, 0.8, Inf),
                      labels = c("<0.5", "0.5-0.8", ">=0.8"))]
conf_bert <- dat[, {
  cc <- .SD[sonnet_A != NULL000]
  .(n = .N, agreement = round(mean(bert_A == sonnet_A), 3),
    kappa_codeable = round(cohen_kappa(cc$bert_A, cc$sonnet_A)$kappa, 3))
}, by = conf_bin][order(conf_bin)]
out[["control_confidence_by_bert_predscore.csv"]] <- conf_bert
cat("\n== 7a) Agreement nach BERT pred_score ==\n"); print(conf_bert, row.names = FALSE)

# (b) nach Sonnet-confidence (high/medium/low)
if ("confidence" %in% names(dat)) {
  conf_sonnet <- dat[, .(n = .N, agreement_A = round(mean(bert_A == sonnet_A), 3),
                         anteil_flag_unsure = round(mean(flag_unsure == TRUE), 3)),
                     by = confidence]
  ord <- c("high", "medium", "low"); conf_sonnet <- conf_sonnet[order(match(confidence, ord))]
  out[["control_confidence_by_sonnet.csv"]] <- conf_sonnet
  cat("\n== 7b) Agreement nach Sonnet-confidence ==\n"); print(conf_sonnet, row.names = FALSE)
}

# ============================================================================
# 8) SOFT AGREEMENT (nur falls die 56 Prob-Spalten im Parquet liegen)
# ============================================================================
soft_done <- FALSE
if (file.exists(PARQUET)) {
  pnames <- arrow::read_parquet(PARQUET, as_data_frame = FALSE)$schema$names
  prob_cols <- grep("^[0-9]{3} - ", pnames, value = TRUE)
  if (length(prob_cols) >= 56) {
    pq <- as.data.table(arrow::read_parquet(PARQUET, col_select = all_of(c("cs_id", prob_cols))))
    m  <- merge(dat[, .(cs_id, sonnet_A, conf_bin)], pq, by = "cs_id")
    prob_codes <- sprintf("%03d", as.integer(sub("^([0-9]{3}).*", "\\1", prob_cols)))
    code_to_A  <- a_lk[prob_codes]
    P <- as.matrix(m[, ..prob_cols])
    bucketsA <- unique(code_to_A)
    massByA <- sapply(bucketsA, function(bk) rowSums(P[, code_to_A == bk, drop = FALSE]))
    colnames(massByA) <- bucketsA
    m[, soft := massByA[cbind(seq_len(.N), match(sonnet_A, bucketsA))]]
    soft_overall <- round(mean(m$soft, na.rm = TRUE), 3)
    soft_bin <- m[, .(n = .N, soft_masse = round(mean(soft, na.rm = TRUE), 3)), by = conf_bin][order(conf_bin)]
    out[["control_soft_agreement_by_bin.csv"]] <- soft_bin
    cat(sprintf("\n== 8) Soft Agreement (BERT-Masse auf Sonnet-Bucket): gesamt %.3f ==\n", soft_overall))
    print(soft_bin, row.names = FALSE); soft_done <- TRUE
  }
}
if (!soft_done) cat("\n== 8) Soft Agreement übersprungen (keine Prob-Spalten im Parquet; 15 mit KEEP_PROBS=TRUE neu ziehen, um es zu aktivieren) ==\n")

# ============================================================================
# 9) PER-PARTY-κ (codeable, korpus-gewichtet)
# ============================================================================
if ("party" %in% names(dat)) {
  party_k <- cod[, .(n = .N,
                     uebereinstimmung = round(mean(bert_A == sonnet_A), 3),
                     kappa_A = round(cohen_kappa(bert_A, sonnet_A)$kappa, 3)),
                 by = party][order(kappa_A)]
  out[["control_kappa_by_party.csv"]] <- party_k
  cat("\n== 9) Per-Party-κ (codeable) ==\n"); print(party_k, row.names = FALSE)
}

# ============================================================================
# 10) KORPUS-VERTEILUNG: BERT- vs. Sonnet-Agenda + JSD zwischen den Kodierern
# ============================================================================
ps <- prop.table(table(dat$sonnet_A)); pb <- prop.table(table(dat$bert_A))
dist <- merge(data.table(Domäne = names(ps), Sonnet = round(as.numeric(ps), 3)),
              data.table(Domäne = names(pb), BERT = round(as.numeric(pb), 3)),
              by = "Domäne", all = TRUE)
dist[is.na(Sonnet), Sonnet := 0]
dist[is.na(BERT), BERT := 0]
dist[, Differenz := round(BERT - Sonnet, 3)]
dist <- dist[order(-Sonnet)]
jsd_full <- jsd(ps, pb)
cod_s <- prop.table(table(cod$sonnet_A)); cod_b <- prop.table(table(cod$bert_A))
jsd_cod <- jsd(cod_s, cod_b)
out[["control_corpus_distribution.csv"]] <- dist
cat("\n== 10) Korpus-Agenda: Sonnet vs. BERT (Domänen-Anteile) ==\n"); print(dist, row.names = FALSE)
cat(sprintf("JSD(Sonnet || BERT) Domänen: voll (inkl. 000) = %.4f | codeable = %.4f bit\n", jsd_full, jsd_cod))

# ============================================================================
# 11) Export
# ============================================================================
for (fn in names(out)) fwrite(out[[fn]], file.path(GS_DIR, fn))
cat(sprintf("\n[17] %d Tabellen geschrieben nach %s\n", length(out), GS_DIR))
cat("Headline:  Domäne κ =", round(kA$kappa, 3),
    "| codeable κ =", round(kA_cod$kappa, 3),
    "| Sonnet-000-Rate =", round(s000, 3),
    "| JSD(Agenda) =", round(jsd_full, 4), "\n")
