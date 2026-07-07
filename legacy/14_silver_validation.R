# =============================================================================
#  14_silver_validation.R
# -----------------------------------------------------------------------------
#  Vergleicht drei Kodierungen derselben Bundestags-Sätze (Batch 2, GS0001–GS0425):
#
#    (1) HUMAN   – manueller Goldstandard            -> goldstandard_codingsheet_FINAL_merged.xlsx (Sheet "Coding")
#    (2) BERT    – ManifestoBERTa-Vorhersagen        -> goldstandard_key_FINAL.rds
#    (3) CLAUDE  – LLM-Zweitkodierung (Feincodes)    -> <claude_json_file> (JSON)
#
#  HUMAN ist die Referenz ("Wahrheit"). Verglichen wird auf drei Ebenen:
#    A) Domäne      (Aggregation A, 10 Klassen inkl. "Nicht zuordenbar (000)")
#    B) Richtung    (Aggregation B, STRENG/roh: 'Wirtschaft Allgemein' != 'keine klare Richtung')
#    F) Feincode    (56 MARPOR-Kategorien; nur BERT vs. CLAUDE, da HUMAN keine Feincodes hat)
#
#  Metriken: Accuracy + Cohen's Kappa (paarweise), Per-Klasse Precision/Recall/F1,
#            Confusion-Matrizen, BERT-Confidence-Analyse (Agreement vs. pred_score),
#            305=000-Sensitivität (8b), und — abgeglichen mit Skript 13 — codeable-only
#            Kappa, Soft Agreement und konfidenz-stratifizierter Kappa (8c).
#
#  Pfade:   alle Ein- und Ausgaben in  results/empirics/goldstandard/
#           (relativ zur RStudio-Projektwurzel via here::here(); Outputs flach im
#            selben Ordner wie die Inputs, konsistent mit Skript 13).
#  Aufruf:  source(here::here("14_silver_validation.R"))
#
#  Pakete:  tidyverse, readxl, jsonlite, here  (+ ggplot2 optional für Heatmaps)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(jsonlite)
})

# ----------------------------------------------------------------------------
# 0) Konfiguration  -- bei Bedarf hier anpassen
# ----------------------------------------------------------------------------
GS_DIR <- here::here("results", "empirics", "goldstandard")          # Projektwurzel/results/empirics/goldstandard

cfg <- list(
  base_dir         = GS_DIR,                                      # Ordner mit den Input-Dateien
  human_xlsx       = "goldstandard_codingsheet_FINAL_merged.xlsx",
  human_sheet      = "Coding",
  bert_rds         = "goldstandard_key_FINAL.rds",
  claude_json      = "marpor_coding_GS0001-GS0425.json",          # <- ggf. an deinen Dateinamen anpassen
  out_dir          = GS_DIR,                                      # Ausgaben flach in denselben Ordner (wie Skript 13)
  make_plots       = TRUE,                                        # ggplot2-Heatmaps + Confidence-Plot
  # Domänen, in denen agg_B (Richtung) überhaupt definiert ist:
  directional_domains = c("Economy", "Welfare & Social Policy",
                          "Migration", "European Integration"),
  none_label       = "(keine Richtung)"                           # Platzhalter für NA auf B-Ebene
)

p   <- function(...)  file.path(cfg$base_dir, ...)   # Pfad zu Input-Dateien
out <- function(file) file.path(cfg$out_dir,  file)  # Pfad zu Output-Dateien
dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------------
# 1) Feincode -> Bucket-Crosswalk (kanonisch, an die Pipeline angelehnt)
# ----------------------------------------------------------------------------
#  bucket_A  = Domäne (Aggregation A)
#  bucket_B  = Richtung (Aggregation B); NA = nicht-gerichtet bzw. Domäne ohne Richtung
#  Die 46 von BERT tatsächlich vergebenen Codes stammen 1:1 aus goldstandard_key_FINAL.rds
#  (siehe Validierung unten); die übrigen Codes (000,101,103,203,304,401,404,405,409,412,704)
#  sind ergänzt nach Buckets-Sheet + Standard-Markt/Staat-Logik (MARPOR).
crosswalk <- tibble::tribble(
  ~code, ~bucket_A,                          ~bucket_B,
  "000", "Nicht zuordenbar (000)",           NA,
  "101", "Foreign Policy & Defence",         NA,
  "102", "Foreign Policy & Defence",         NA,
  "103", "Foreign Policy & Defence",         NA,
  "104", "Foreign Policy & Defence",         NA,
  "105", "Foreign Policy & Defence",         NA,
  "106", "Foreign Policy & Defence",         NA,
  "107", "Foreign Policy & Defence",         NA,
  "108", "European Integration",             "Pro-EU",
  "109", "Foreign Policy & Defence",         NA,
  "110", "European Integration",             "Contra-EU",
  "201", "Democracy & Political System",     NA,
  "202", "Democracy & Political System",     NA,
  "203", "Democracy & Political System",     NA,
  "204", "Democracy & Political System",     NA,
  "301", "Democracy & Political System",     NA,
  "302", "Democracy & Political System",     NA,
  "303", "Democracy & Political System",     NA,
  "304", "Democracy & Political System",     NA,
  "305", "Democracy & Political System",     NA,
  "401", "Economy",                          "Marktliberalismus",
  "402", "Economy",                          "Marktliberalismus",
  "403", "Economy",                          "Staatsintervention",
  "404", "Economy",                          "Staatsintervention",
  "405", "Economy",                          "Staatsintervention",
  "406", "Economy",                          "keine klare Richtung",  # 406/415 = "Economy (sonstige)" -> auf Human-Label gemappt
  "407", "Economy",                          "Marktliberalismus",
  "408", "Economy",                          "Wirtschaft Allgemein",
  "409", "Economy",                          "Staatsintervention",
  "410", "Economy",                          "Wirtschaft Allgemein",
  "411", "Economy",                          "Wirtschaft Allgemein",
  "412", "Economy",                          "Staatsintervention",
  "413", "Economy",                          "Staatsintervention",
  "414", "Economy",                          "Marktliberalismus",
  "415", "Economy",                          "keine klare Richtung",
  "416", "Environment",                      NA,
  "501", "Environment",                      NA,
  "502", "Welfare & Social Policy",          "keine klare Richtung",  # 502 = "Welfare (sonstige)" -> auf Human-Label gemappt
  "503", "Welfare & Social Policy",          "Sozialstaat Ausbau",
  "504", "Welfare & Social Policy",          "Sozialstaat Ausbau",
  "505", "Welfare & Social Policy",          "Sozialstaat Begrenzung",
  "506", "Welfare & Social Policy",          "Sozialstaat Ausbau",
  "507", "Welfare & Social Policy",          "Sozialstaat Begrenzung",
  "601", "Migration",                        "Migration restriktiv",
  "602", "Migration",                        "Migration liberal",
  "603", "Law & Order and National Identity", NA,
  "604", "Law & Order and National Identity", NA,
  "605", "Law & Order and National Identity", NA,
  "606", "Law & Order and National Identity", NA,
  "607", "Migration",                        "Migration liberal",
  "608", "Migration",                        "Migration restriktiv",
  "701", "Social Groups",                    NA,
  "702", "Social Groups",                    NA,
  "703", "Social Groups",                    NA,
  "704", "Social Groups",                    NA,
  "705", "Social Groups",                    NA,
  "706", "Social Groups",                    NA
)

# ----------------------------------------------------------------------------
# 2) Hilfsfunktionen (Metriken; bewusst ohne Zusatzpakete)
# ----------------------------------------------------------------------------

## Richtung normalisieren: gemeinsame Label-Vokabular über alle Kodierer.
## - "Andere"/"" -> NA  (nicht-gerichtete Domänen)
## - in Economy/Welfare: fehlende Richtung -> "keine klare Richtung"
##   (damit Human "keine klare Richtung" und Pipeline-NaN dasselbe Label tragen)
normalize_direction <- function(domain, direction) {
  d <- as.character(direction)
  d[d %in% c("Andere", "", "NA")] <- NA
  econ_welf <- domain %in% c("Economy", "Welfare & Social Policy")
  d[econ_welf & is.na(d)] <- "keine klare Richtung"
  d
}

## Cohen's Kappa (zwei Vektoren, ungeordnet kategorial; paarweise vollständige Fälle)
cohen_kappa <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) == 0) return(NA_real_)
  lev <- sort(union(unique(as.character(a)), unique(as.character(b))))
  a <- factor(as.character(a), levels = lev)
  b <- factor(as.character(b), levels = lev)
  tab <- table(a, b)
  n   <- sum(tab)
  po  <- sum(diag(tab)) / n
  pe  <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (abs(1 - pe) < .Machine$double.eps) return(NA_real_)
  (po - pe) / (1 - pe)
}

## Accuracy (Anteil exakter Übereinstimmungen über vollständige Fälle)
accuracy <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) == 0) return(NA_real_)
  mean(as.character(a[ok]) == as.character(b[ok]))
}

## Per-Klasse Precision/Recall/F1 (truth = Referenz, pred = Vorhersage)
class_prf <- function(truth, pred) {
  ok <- !is.na(truth) & !is.na(pred)
  truth <- as.character(truth[ok]); pred <- as.character(pred[ok])
  lev <- sort(union(unique(truth), unique(pred)))
  truth <- factor(truth, lev); pred <- factor(pred, lev)
  cm <- table(truth, pred)
  tp        <- diag(cm)
  support   <- rowSums(cm)     # echte Fälle je Klasse
  pred_pos  <- colSums(cm)     # vorhergesagte Fälle je Klasse
  precision <- ifelse(pred_pos > 0, tp / pred_pos, NA_real_)
  recall    <- ifelse(support  > 0, tp / support,  NA_real_)
  f1        <- ifelse((precision + recall) > 0,
                      2 * precision * recall / (precision + recall), 0)
  tibble(class = lev,
         support   = as.integer(support),
         precision = as.numeric(precision),
         recall    = as.numeric(recall),
         f1        = as.numeric(f1))
}

## Headline-Kennzahlen aus einem PRF-Frame (Macro- und Support-gewichtetes F1)
prf_headline <- function(truth, pred) {
  prf <- class_prf(truth, pred)
  tibble(
    n          = sum(!is.na(truth) & !is.na(pred)),
    accuracy   = accuracy(truth, pred),
    kappa      = cohen_kappa(truth, pred),
    macro_f1   = mean(prf$f1, na.rm = TRUE),
    weighted_f1= sum(prf$f1 * prf$support, na.rm = TRUE) / sum(prf$support)
  )
}

## Confusion-Matrix als langes Tibble (truth-Zeilen x pred-Spalten)
confusion_long <- function(truth, pred, truth_name = "truth", pred_name = "pred") {
  ok <- !is.na(truth) & !is.na(pred)
  truth <- as.character(truth[ok]); pred <- as.character(pred[ok])
  lev <- sort(union(unique(truth), unique(pred)))
  tab <- table(factor(truth, lev), factor(pred, lev))
  as.data.frame(tab, stringsAsFactors = FALSE) |>
    rlang::set_names(c(truth_name, pred_name, "n")) |>
    tibble::as_tibble()
}

write_out <- function(x, file) {
  readr::write_csv(x, out(file))
  invisible(x)
}

# ----------------------------------------------------------------------------
# 3) Daten laden
# ----------------------------------------------------------------------------

## (1) HUMAN
human <- read_excel(p(cfg$human_xlsx), sheet = cfg$human_sheet) |>
  transmute(
    gs_id,
    human_A     = as.character(agg_A),
    human_B_raw = as.character(agg_B),
    human_dps   = as.character(dps_fine),
    human_flag  = suppressWarnings(as.logical(flag_unsure)),
    human_note  = as.character(note)
  )

## (2) BERT  (rds enthält Feincode + Buckets + volle Wahrscheinlichkeiten + pred_score)
bert_raw <- readRDS(p(cfg$bert_rds))
stopifnot(all(c("gs_id","pred_code","pred_score","bucket_A","bucket_B") %in% names(bert_raw)))
bert <- bert_raw |>
  transmute(
    gs_id,
    party      = as.character(party),
    lp         = suppressWarnings(as.integer(lp)),
    bert_code  = sprintf("%03d", as.integer(round(as.numeric(pred_code)))),
    bert_score = as.numeric(pred_score),
    bert_A     = as.character(bucket_A),
    bert_B     = as.character(bucket_B)
  )

## (3) CLAUDE  (JSON; Feincode in Feld "marpor")
if (!file.exists(p(cfg$claude_json))) {
  cand <- list.files(cfg$base_dir, pattern = "\\.json$", full.names = FALSE)
  stop("Claude-JSON '", cfg$claude_json, "' nicht gefunden. Gefundene .json im Ordner: ",
       if (length(cand)) paste(cand, collapse = ", ") else "(keine)",
       "\n-> cfg$claude_json entsprechend setzen.")
}
claude <- jsonlite::fromJSON(p(cfg$claude_json)) |>
  as_tibble() |>
  transmute(
    gs_id,
    claude_code  = stringr::str_pad(as.character(marpor), 3, pad = "0"),
    claude_fehlt = as.logical(fehltrennung),
    claude_flag  = as.logical(flag_unsure),
    claude_conf  = as.character(confidence),
    claude_note  = as.character(note)
  )

# ----------------------------------------------------------------------------
# 4) Crosswalk gegen BERT-Key validieren (keine widersprüchlichen Mappings)
# ----------------------------------------------------------------------------
bert_cw <- bert_raw |>
  transmute(code = sprintf("%03d", as.integer(round(as.numeric(pred_code)))),
            bucket_A_emp = as.character(bucket_A),
            bucket_B_emp = as.character(bucket_B)) |>
  distinct()

check <- bert_cw |>
  left_join(crosswalk, by = "code") |>
  mutate(
    # in Economy/Welfare hat die Pipeline NaN, wo der Crosswalk "keine klare Richtung" setzt -> ok
    b_norm_emp = normalize_direction(bucket_A_emp, bucket_B_emp),
    b_norm_cw  = normalize_direction(bucket_A,     bucket_B),
    A_ok = bucket_A_emp == bucket_A,
    B_ok = (is.na(b_norm_emp) & is.na(b_norm_cw)) | (b_norm_emp == b_norm_cw)
  )
if (!all(check$A_ok & check$B_ok, na.rm = TRUE)) {
  warning("Crosswalk weicht vom BERT-Key ab — bitte prüfen:")
  print(filter(check, !(A_ok & B_ok)))
} else {
  message("Crosswalk-Validierung ok: stimmt fuer alle ", nrow(bert_cw),
          " von BERT vergebenen Codes mit dem Key ueberein.")
}

# ----------------------------------------------------------------------------
# 5) Zusammenführen + abgeleitete Spalten
# ----------------------------------------------------------------------------
cw_claude <- crosswalk |> rename(claude_code = code, claude_A = bucket_A, claude_B = bucket_B)

dat <- human |>
  full_join(bert,   by = "gs_id") |>
  full_join(claude, by = "gs_id") |>
  left_join(cw_claude, by = "claude_code") |>
  mutate(
    # Richtung (B) für alle drei Kodierer ins gemeinsame Vokabular bringen
    human_B  = normalize_direction(human_A,  human_B_raw),
    bert_B   = normalize_direction(bert_A,   bert_B),
    claude_B = normalize_direction(claude_A, claude_B),
    # Übereinstimmungs-Flags (Domäne A)
    agree_A_bert_human   = human_A == bert_A,
    agree_A_claude_human = human_A == claude_A,
    agree_A_bert_claude  = bert_A  == claude_A
  ) |>
  arrange(gs_id)

# Vollständigkeit prüfen
n_total <- nrow(dat)
missing <- dat |> filter(is.na(human_A) | is.na(bert_A) | is.na(claude_A))
if (nrow(missing) > 0) {
  warning(nrow(missing), " Satz/Sätze ohne vollständiges Tripel (Join-Lücke) — siehe join_gaps.csv")
  write_out(missing |> select(gs_id, human_A, bert_A, claude_A, claude_code), "join_gaps.csv")
}
message("Zeilen gesamt: ", n_total,
        " | vollständige Tripel: ", sum(complete.cases(dat[c("human_A","bert_A","claude_A")])))

# Lange Vergleichstabelle als Hauptartefakt
comparison_long <- dat |>
  select(gs_id, party, lp,
         human_A, bert_A, claude_A,
         human_B_raw, human_B, bert_B, claude_B,
         bert_code, claude_code, bert_score,
         claude_conf, claude_fehlt, claude_flag, human_flag,
         agree_A_bert_human, agree_A_claude_human, agree_A_bert_claude)
write_out(comparison_long, "comparison_long.csv")

# ----------------------------------------------------------------------------
# 6) EBENE A — Domäne (10 Klassen)
# ----------------------------------------------------------------------------
pairs_A <- tibble::tribble(
  ~vergleich,        ~truth,     ~pred,
  "BERT_vs_HUMAN",   "human_A",  "bert_A",
  "CLAUDE_vs_HUMAN", "human_A",  "claude_A",
  "BERT_vs_CLAUDE",  "claude_A", "bert_A"     # hier ist "truth" nur Referenzachse, kein Goldstandard
)

metrics_A <- pairs_A |>
  mutate(res = purrr::map2(truth, pred, ~prf_headline(dat[[.x]], dat[[.y]]))) |>
  tidyr::unnest(res) |>
  mutate(ebene = "A_Domaene", .before = 1) |>
  select(ebene, vergleich, n, accuracy, kappa, macro_f1, weighted_f1)

# Per-Klasse PRF (nur Vergleiche gegen HUMAN sind als Validierung sinnvoll)
prf_A_bert   <- class_prf(dat$human_A, dat$bert_A)   |> mutate(vergleich="BERT_vs_HUMAN",   .before=1)
prf_A_claude <- class_prf(dat$human_A, dat$claude_A) |> mutate(vergleich="CLAUDE_vs_HUMAN", .before=1)
write_out(bind_rows(prf_A_bert, prf_A_claude), "prf_A_domaene.csv")

# Confusion-Matrizen (Domäne)
write_out(confusion_long(dat$human_A, dat$bert_A,   "human","bert"),   "confusion_A_bert_vs_human.csv")
write_out(confusion_long(dat$human_A, dat$claude_A, "human","claude"), "confusion_A_claude_vs_human.csv")
write_out(confusion_long(dat$claude_A, dat$bert_A,  "claude","bert"),  "confusion_A_bert_vs_claude.csv")

# ----------------------------------------------------------------------------
# 7) EBENE B — Richtung (STRENG/roh; nur auf gerichteten Domänen laut HUMAN)
# ----------------------------------------------------------------------------
# Teilmenge: HUMAN ordnet den Satz einer gerichteten Domäne zu.
# NA-Richtung wird als eigenes Label (none_label) behandelt -> strenger Vergleich.
datB <- dat |>
  filter(human_A %in% cfg$directional_domains) |>
  mutate(
    human_Bx  = tidyr::replace_na(human_B,  cfg$none_label),
    bert_Bx   = tidyr::replace_na(bert_B,   cfg$none_label),
    claude_Bx = tidyr::replace_na(claude_B, cfg$none_label)
  )

pairs_B <- tibble::tribble(
  ~vergleich,        ~truth,      ~pred,
  "BERT_vs_HUMAN",   "human_Bx",  "bert_Bx",
  "CLAUDE_vs_HUMAN", "human_Bx",  "claude_Bx",
  "BERT_vs_CLAUDE",  "claude_Bx", "bert_Bx"
)
metrics_B <- pairs_B |>
  mutate(res = purrr::map2(truth, pred, ~prf_headline(datB[[.x]], datB[[.y]]))) |>
  tidyr::unnest(res) |>
  mutate(ebene = "B_Richtung_streng", .before = 1) |>
  select(ebene, vergleich, n, accuracy, kappa, macro_f1, weighted_f1)

prf_B_bert   <- class_prf(datB$human_Bx, datB$bert_Bx)   |> mutate(vergleich="BERT_vs_HUMAN",   .before=1)
prf_B_claude <- class_prf(datB$human_Bx, datB$claude_Bx) |> mutate(vergleich="CLAUDE_vs_HUMAN", .before=1)
write_out(bind_rows(prf_B_bert, prf_B_claude), "prf_B_richtung.csv")

write_out(confusion_long(datB$human_Bx, datB$bert_Bx,   "human","bert"),   "confusion_B_bert_vs_human.csv")
write_out(confusion_long(datB$human_Bx, datB$claude_Bx, "human","claude"), "confusion_B_claude_vs_human.csv")

# ----------------------------------------------------------------------------
# 8) EBENE F — Feincode (56 Klassen; nur BERT vs. CLAUDE)
# ----------------------------------------------------------------------------
metrics_F <- prf_headline(dat$claude_code, dat$bert_code) |>
  mutate(ebene = "F_Feincode", vergleich = "BERT_vs_CLAUDE", .before = 1)

# Top-Diskrepanzen Feincode (wo BERT und Claude am häufigsten auseinanderliegen)
fine_disagree <- dat |>
  filter(bert_code != claude_code) |>
  count(claude_code, bert_code, name = "n") |>
  arrange(desc(n))
write_out(fine_disagree, "fine_disagreement_bert_vs_claude.csv")

# ----------------------------------------------------------------------------
# 8b) SENSITIVITÄT — BERT-Code 305 ("Political Authority") als Äquivalent zu 000
# ----------------------------------------------------------------------------
# Motivation (Befund 2): ManifestoBERTa hat keine Restkategorie und leitet
# prozedurale 000-Sätze überwiegend auf 305 um. Diese Analyse wertet BERTs 305
# generell als 000 und prüft, was das für die Übereinstimmung bedeutet.
#
# WICHTIG: Das Remapping ist ein TRADE-OFF, kein Gratis-Gewinn. Ein Teil der
# 305-Sätze sind echte Demokratie-Aussagen, die durch die Regel fälschlich zu
# 000 werden. Die Diagnostik unten macht beide Seiten explizit (Recovery der
# 000-Fälle vs. Kollateralschaden an der Demokratie-Domäne).

remap_code <- "305"   # bei Bedarf anpassbar

# (a) BERT mit remappten Feincodes (305 -> 000), erneut über DENSELBEN Crosswalk
#     auf Domäne/Richtung abgebildet (konsistent mit der Hauptanalyse).
cw_bert_rm <- crosswalk |>
  rename(bert_code_rm = code, bert_A_rm = bucket_A, bert_B_rm_raw = bucket_B)
dat <- dat |>
  mutate(bert_code_rm = if_else(bert_code == remap_code, "000", bert_code)) |>
  left_join(cw_bert_rm, by = "bert_code_rm") |>
  mutate(bert_B_rm = normalize_direction(bert_A_rm, bert_B_rm_raw)) |>
  select(-bert_B_rm_raw)

# (b) Diagnostik 1 — wohin gehen die BERT-305-Sätze laut HUMAN bzw. CLAUDE?
b_rm <- dat |> filter(bert_code == remap_code)
write_out(b_rm |> count(human_A, name = "n") |> arrange(desc(n)) |>
            mutate(anteil = round(n / sum(n), 3)),
          sprintf("diag_bert%s_to_human.csv", remap_code))
write_out(b_rm |> count(claude_code, name = "n") |> arrange(desc(n)) |>
            mutate(anteil = round(n / sum(n), 3)),
          sprintf("diag_bert%s_to_claude.csv", remap_code))

# (c) Diagnostik 2 — Recovery (Treffer) vs. Kollateralschaden der 305=000-Regel
n_rm           <- nrow(b_rm)
n_human000     <- sum(dat$human_A     == "Nicht zuordenbar (000)",        na.rm = TRUE)
n_claude000    <- sum(dat$claude_code == "000",                           na.rm = TRUE)
hit_human000   <- sum(b_rm$human_A     == "Nicht zuordenbar (000)",       na.rm = TRUE)
hit_claude000  <- sum(b_rm$claude_code == "000",                          na.rm = TRUE)
collateral_dem <- sum(b_rm$human_A     == "Democracy & Political System", na.rm = TRUE)

diag_summary <- tibble::tibble(
  kennzahl = c(
    sprintf("BERT-%s gesamt", remap_code),
    "  davon HUMAN-000 (Recovery)",
    "  davon CLAUDE-000",
    "  davon HUMAN-Democracy (Kollateral -> faelschlich 000)",
    "HUMAN-000 gesamt",
    sprintf("  davon von BERT als %s kodiert (-> Recovery)", remap_code),
    "CLAUDE-000 gesamt",
    sprintf("  davon von BERT als %s kodiert", remap_code)
  ),
  wert = c(n_rm, hit_human000, hit_claude000, collateral_dem,
           n_human000, hit_human000, n_claude000, hit_claude000),
  anteil = c(NA_real_,
             round(hit_human000 / n_rm, 3), round(hit_claude000 / n_rm, 3),
             round(collateral_dem / n_rm, 3),
             NA_real_, round(hit_human000 / n_human000, 3),
             NA_real_, round(hit_claude000 / n_claude000, 3))
)
write_out(diag_summary, sprintf("diag_bert%s_equiv000_summary.csv", remap_code))

# (d) Neuberechnung der Übereinstimmung MIT der Regel (Domäne A + Feincode),
#     je 'original' vs. 'remap_305eq000', gegen HUMAN und CLAUDE.
mk <- function(truth, pred, ebene, vergleich, variante) {
  prf_headline(truth, pred) |>
    mutate(ebene = ebene, vergleich = vergleich, variante = variante, .before = 1)
}
metrics_305 <- bind_rows(
  mk(dat$human_A,     dat$bert_A,        "A_Domaene",  "BERT_vs_HUMAN",  "original"),
  mk(dat$human_A,     dat$bert_A_rm,     "A_Domaene",  "BERT_vs_HUMAN",  "remap_305eq000"),
  mk(dat$claude_A,    dat$bert_A,        "A_Domaene",  "BERT_vs_CLAUDE", "original"),
  mk(dat$claude_A,    dat$bert_A_rm,     "A_Domaene",  "BERT_vs_CLAUDE", "remap_305eq000"),
  mk(dat$claude_code, dat$bert_code,     "F_Feincode", "BERT_vs_CLAUDE", "original"),
  mk(dat$claude_code, dat$bert_code_rm,  "F_Feincode", "BERT_vs_CLAUDE", "remap_305eq000")
) |>
  select(ebene, vergleich, variante, n, accuracy, kappa, macro_f1, weighted_f1)
write_out(metrics_305, sprintf("metrics_bert%s_equiv000.csv", remap_code))

# Delta-Tabelle (remap minus original) je Vergleich/Ebene/Metrik
delta_305 <- metrics_305 |>
  tidyr::pivot_longer(c(accuracy, kappa, macro_f1, weighted_f1),
                      names_to = "metrik", values_to = "wert") |>
  tidyr::pivot_wider(names_from = variante, values_from = wert) |>
  mutate(delta = remap_305eq000 - original) |>
  select(ebene, vergleich, metrik, original, remap_305eq000, delta)
write_out(delta_305, sprintf("metrics_bert%s_equiv000_delta.csv", remap_code))

# (e) Per-Klasse-Effekt auf die zwei betroffenen Domänen (Referenz HUMAN):
#     000 (gewinnt Recall) und Democracy & Political System (verliert Recall).
prf_affected <- bind_rows(
  class_prf(dat$human_A, dat$bert_A)    |> mutate(variante = "original",       .before = 1),
  class_prf(dat$human_A, dat$bert_A_rm) |> mutate(variante = "remap_305eq000", .before = 1)
) |>
  filter(class %in% c("Nicht zuordenbar (000)", "Democracy & Political System")) |>
  arrange(class, variante)
write_out(prf_affected, sprintf("prf_bert%s_equiv000_affected.csv", remap_code))

# ----------------------------------------------------------------------------
# 8c) ABGLEICH MIT SKRIPT 13 — sophistizierte Maße
#     (codeable-only Kappa, konfidenz-stratifizierter Kappa, Soft Agreement)
# ----------------------------------------------------------------------------
# WARUM dieser Block (Erklärung der Kappa-Differenz zu Skript 13):
#   Skript 14 berichtet als Headline den ROH-Kappa über ALLE Sätze inkl. der
#   menschlichen 000-Fälle ("Nicht zuordenbar"). ManifestoBERTa kann 000
#   strukturell NIE vorhersagen (000 ist nicht unter den 56 Codes) — die
#   Human-000-Sätze sind damit ein asymmetrischer Strafterm, der den Kappa
#   nach unten zieht. Skript 13 stellt eine andere Frage und berichtet:
#     (i)  kappa_A_codeable_only  — nur die 9 echten Buckets, Human-000 raus,
#     (ii) soft_agreement         — Modell-Posterior-Masse auf dem Human-Bucket
#          über die volle 56-dim Verteilung (passt zur weichen Aggregation),
#     (iii) konfidenz-stratifizierter Kappa (3 Bins) statt Accuracy-Bins.
#   Hier werden alle drei ergänzt, damit Skript 14 dieselbe Frage beantwortet.
#
#   RESIDUUM: Skript 13 nutzt zusätzlich FRISCHE v2-Vorhersagen (argmax auf dem
#   re-segmentierten v2-Korpus) und das konsolidierte FINAL-Gold abzüglich
#   Carry-Gaps; deshalb wird der codeable-only-Kappa hier (auf den rds-Daten)
#   die Skript-13-Zahl nicht auf die dritte Stelle treffen. Der definitorische
#   Haupthebel — Human-000 ausschließen — ist aber genau dieser.
#
#   CAVEAT (wie in 13): Das Gold-Sample ist bucket-STRATIFIZIERT (DPS über-
#   gewichtet). Jeder Kappa hier ist eine Per-Bucket-Reliabilität, KEINE
#   korpus-frequenz-gewichtete; nicht als Zufallsziehung lesen.

# (a) Evaluable / codeable Subsets (Seg-Faults & Platzhalter raus, analog 13)
dat <- dat |>
  mutate(
    is_segfault    = grepl("Feh", human_A, ignore.case = TRUE),        # Fehl-/Fehltrennung im HUMAN-Code
    is_placeholder = tolower(trimws(human_A)) %in% c("none", "na", ""),
    is_notassign   = grepl("zuordenbar|000", human_A)                  # HUMAN '000'
  )
eval_set <- dat |> filter(!is_segfault, !is_placeholder,
                          !is.na(human_A), nzchar(trimws(human_A)))
code_set <- eval_set |> filter(!is_notassign)                          # nur die 9 echten Buckets

# zentrale Kappa-Skalare (wiederverwendet in Tabelle + Konsole)
ka_raw_bert  <- cohen_kappa(eval_set$human_A, eval_set$bert_A)
ka_code_bert <- cohen_kappa(code_set$human_A, code_set$bert_A)

# (b) Kappa_A: roh (inkl. 000) vs. codeable-only — BERT UND CLAUDE gegen HUMAN
kappa_flavors <- tibble::tibble(
  vergleich = c("BERT_vs_HUMAN","BERT_vs_HUMAN","CLAUDE_vs_HUMAN","CLAUDE_vs_HUMAN"),
  subset    = c("raw_incl_000","codeable_only","raw_incl_000","codeable_only"),
  n         = c(nrow(eval_set), nrow(code_set), nrow(eval_set), nrow(code_set)),
  accuracy  = c(accuracy(eval_set$human_A, eval_set$bert_A),
                accuracy(code_set$human_A, code_set$bert_A),
                accuracy(eval_set$human_A, eval_set$claude_A),
                accuracy(code_set$human_A, code_set$claude_A)),
  kappa     = c(ka_raw_bert, ka_code_bert,
                cohen_kappa(eval_set$human_A, eval_set$claude_A),
                cohen_kappa(code_set$human_A, code_set$claude_A))
)
write_out(kappa_flavors, "agreement_kappa_flavors.csv")

# (c) Per-Party Kappa_A (codeable; BERT vs. HUMAN), wie in 13
perparty_A <- code_set |>
  group_by(party) |>
  summarise(n     = sum(!is.na(human_A) & !is.na(bert_A)),
            agree = round(accuracy(human_A, bert_A), 3),
            kappa = round(cohen_kappa(human_A, bert_A), 3),
            .groups = "drop") |>
  arrange(kappa)
write_out(perparty_A, "agreement_kappaA_by_party.csv")

# (d) Konfidenz-stratifizierter Kappa_A (3 Bins wie in 13: <0.5 / 0.5-0.8 / >=0.8)
code_set <- code_set |>
  mutate(conf_bin3 = cut(bert_score, breaks = c(-Inf, 0.5, 0.8, Inf),
                         labels = c("<0.5", "0.5-0.8", ">=0.8")))
kappa_by_conf <- code_set |>
  group_by(conf_bin3) |>
  summarise(n     = sum(!is.na(human_A) & !is.na(bert_A)),
            agree = round(accuracy(human_A, bert_A), 3),
            kappa = round(cohen_kappa(human_A, bert_A), 3),
            .groups = "drop") |>
  arrange(conf_bin3)
write_out(kappa_by_conf, "agreement_kappaA_by_confidence_3bin.csv")

# (e) SOFT AGREEMENT — Modell-Posterior-Masse auf dem HUMAN-Bucket (codeable).
#     Nutzt die 56 Wahrscheinlichkeits-Spalten ("NNN - Titel") aus dem rds.
prob_cols <- grep("^[0-9]{3} - ", names(bert_raw), value = TRUE)
if (length(prob_cols) == 0L) {
  message("[14] Keine 56 Prob-Spalten im rds gefunden -> Soft Agreement übersprungen.")
  soft_overall <- NA_real_; soft_by_bin <- tibble::tibble()
} else {
  prob_code   <- sprintf("%03d", as.integer(substr(prob_cols, 1, 3)))
  prob_bucket <- crosswalk$bucket_A[match(prob_code, crosswalk$code)]   # Bucket je Prob-Spalte
  bert_probs  <- bert_raw |> select(gs_id, dplyr::all_of(prob_cols))

  soft <- code_set |>
    select(gs_id, human_A, conf_bin3) |>
    left_join(bert_probs, by = "gs_id")
  Pmat <- as.matrix(soft[, prob_cols])
  soft$soft_mass_human <- vapply(seq_len(nrow(soft)), function(i)
    sum(Pmat[i, prob_bucket == soft$human_A[i]], na.rm = TRUE), numeric(1))
  soft_overall <- mean(soft$soft_mass_human)
  soft_by_bin  <- soft |>
    group_by(conf_bin3) |>
    summarise(n = n(),
              mean_mass_on_human_bucket = round(mean(soft_mass_human), 3),
              .groups = "drop") |>
    arrange(conf_bin3)
  write_out(soft_by_bin, "agreement_soft_by_confidence.csv")
}

# (f) Sammel-Summary im Stil von Skript 13 (goldstandard_v2_validation_summary)
agreement_sophisticated <- tibble::tibble(
  metric = c("kappa_A_raw_incl_notassignable", "kappa_A_codeable_only",
             "soft_agreement_mass_on_human_bucket"),
  n      = c(nrow(eval_set), nrow(code_set),
             if (exists("soft")) nrow(soft) else 0L),
  value  = round(c(ka_raw_bert, ka_code_bert, soft_overall), 3)
)
write_out(agreement_sophisticated, "agreement_sophisticated_summary.csv")

# ----------------------------------------------------------------------------
# 9) BERT-Confidence-Analyse (Agreement vs. pred_score, Referenz HUMAN, Ebene A)
# ----------------------------------------------------------------------------
# Hinweis: Dies ist die 5-Bin-ACCURACY-Sicht über ALLE Sätze (inkl. 000) und
# dient als ergänzende Farbe. Die Skript-13-konforme Sicht (konfidenz-
# stratifizierter KAPPA auf dem codeable Set, 3 Bins) steht in Sektion 8c.
conf <- dat |>
  filter(!is.na(bert_score)) |>
  mutate(score_bin = cut(bert_score,
                         breaks = c(0, .2, .4, .6, .8, 1.0),
                         include.lowest = TRUE,
                         labels = c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1.0")))

agreement_by_confidence <- conf |>
  group_by(score_bin) |>
  summarise(
    n              = n(),
    mean_score     = mean(bert_score),
    acc_bert_human = mean(agree_A_bert_human, na.rm = TRUE),
    .groups = "drop"
  )
write_out(agreement_by_confidence, "agreement_by_confidence.csv")

# Mittlerer Score bei Treffer vs. Fehler (BERT vs. HUMAN, Domäne)
score_by_hit <- conf |>
  group_by(treffer_bert_human = agree_A_bert_human) |>
  summarise(n = n(), mean_score = mean(bert_score), median_score = median(bert_score),
            .groups = "drop")
write_out(score_by_hit, "score_by_hit.csv")

# ----------------------------------------------------------------------------
# 10) Headline-Tabelle + Konsolen-Report
# ----------------------------------------------------------------------------
metrics_summary <- bind_rows(metrics_A, metrics_B, metrics_F)
write_out(metrics_summary, "metrics_summary.csv")

cat("\n=========================================================\n")
cat(" GOLDSTANDARD-VERGLEICH  (N =", n_total, "Sätze)\n")
cat("=========================================================\n\n")
cat("— Ebene A: Domäne (Aggregation A) —\n")
print(metrics_A   |> mutate(across(c(accuracy,kappa,macro_f1,weighted_f1), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Ebene B: Richtung (streng, nur gerichtete Domänen; N =", nrow(datB), ") —\n")
print(metrics_B   |> mutate(across(c(accuracy,kappa,macro_f1,weighted_f1), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Ebene F: Feincode (BERT vs. Claude) —\n")
print(metrics_F   |> select(vergleich,n,accuracy,kappa,macro_f1,weighted_f1) |>
        mutate(across(c(accuracy,kappa,macro_f1,weighted_f1), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Sensitivität: BERT-", remap_code, " \u2261 000 —\n", sep = "")
cat(sprintf("  BERT-%s gesamt: %d | Recovery (HUMAN-000): %d (%.0f%%) | Kollateral (echte Democracy): %d (%.0f%%)\n",
            remap_code, n_rm, hit_human000, 100*hit_human000/n_rm,
            collateral_dem, 100*collateral_dem/n_rm))
cat(sprintf("  000-Recall (BERT vs. HUMAN) steigt 0.00 -> %.2f (von %d HUMAN-000)\n",
            hit_human000/n_human000, n_human000))
print(delta_305 |> mutate(across(c(original, remap_305eq000, delta), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\n— Abgleich mit Skript 13: codeable-only Kappa + Soft Agreement (BERT vs. HUMAN) —\n")
cat(sprintf("  kappa_A roh (inkl. 000, n=%d)         : %.3f   <- Skript-14-Headline\n",
            nrow(eval_set), ka_raw_bert))
cat(sprintf("  kappa_A codeable-only (ohne 000, n=%d): %.3f   <- die Frage, die Skript 13 stellt\n",
            nrow(code_set), ka_code_bert))
cat(sprintf("  soft agreement (Posterior-Masse, n=%d): %.3f\n",
            if (exists("soft")) nrow(soft) else 0L, soft_overall))
cat("\n  konfidenz-stratifizierter kappa_A (codeable, 3 Bins):\n")
print(kappa_by_conf |> as.data.frame(), row.names = FALSE)
if (nrow(soft_by_bin)) {
  cat("\n  soft agreement nach Konfidenz:\n")
  print(soft_by_bin |> as.data.frame(), row.names = FALSE)
}
cat("\n— Agreement BERT vs. HUMAN (Domäne) nach BERT-Confidence —\n")
print(agreement_by_confidence |> mutate(across(c(mean_score,acc_bert_human), ~round(.,3))) |> as.data.frame(), row.names = FALSE)
cat("\nAlle Tabellen geschrieben nach: ", normalizePath(cfg$out_dir, mustWork = FALSE), "\n\n")

# ----------------------------------------------------------------------------
# 11) Plots (optional, ggplot2)
# ----------------------------------------------------------------------------
if (cfg$make_plots && requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  heat <- function(cm_long, truth_col, pred_col, title) {
    ggplot(cm_long, aes(.data[[pred_col]], .data[[truth_col]], fill = n)) +
      geom_tile() +
      geom_text(aes(label = ifelse(n > 0, n, "")), size = 3) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = title, x = paste(pred_col, "(Vorhersage)"),
           y = paste(truth_col, "(Referenz)")) +
      theme_minimal(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank())
  }

  ggsave(out("plot_confusion_A_bert_vs_human.png"),
         heat(confusion_long(dat$human_A, dat$bert_A, "human","bert"),
              "human","bert","Domäne (A): BERT vs. HUMAN"),
         width = 8, height = 6, dpi = 150)
  ggsave(out("plot_confusion_A_claude_vs_human.png"),
         heat(confusion_long(dat$human_A, dat$claude_A, "human","claude"),
              "human","claude","Domäne (A): CLAUDE vs. HUMAN"),
         width = 8, height = 6, dpi = 150)

  pc <- ggplot(agreement_by_confidence, aes(score_bin, acc_bert_human)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "BERT-Confidence vs. Übereinstimmung mit HUMAN (Domäne)",
         x = "pred_score (Bin)", y = "Accuracy BERT vs. HUMAN") +
    theme_minimal(base_size = 10)
  ggsave(out("plot_agreement_by_confidence.png"), pc,
         width = 7, height = 5, dpi = 150)

  message("Plots geschrieben (PNG) in ", cfg$out_dir, "/")
} else if (cfg$make_plots) {
  message("ggplot2 nicht installiert — Plots übersprungen (Tabellen sind vollständig).")
}

invisible(list(data = dat, metrics = metrics_summary))
# =============================================================================
#  Ende
# =============================================================================
