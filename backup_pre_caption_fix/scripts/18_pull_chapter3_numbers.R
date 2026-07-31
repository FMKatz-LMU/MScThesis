# ============================================================================
# 18_pull_chapter3_numbers.R — Kanonische Zahlen fuer Kapitel 3 (Schreibplan)
# ----------------------------------------------------------------------------
# Zieht JEDE im Schreibplan_Kapitel3 referenzierte Zahl direkt aus den
# Output-Dateien der Pipeline (CSV / RDS / JSON / Parquet) und schreibt:
#
#   results/empirics/chapter3_numbers.csv      <- Querschnittsregel 5:
#                                                  Abschnitt | Platzhalter | Wert |
#                                                  Quelle | Hinweis | geprueft_am
#   results/empirics/chapter3_numbers.md       <- lesbarer Report, gruppiert
#                                                  nach Schreibplan-Abschnitt
#   results/empirics/chapter3_T1_cell_matrix.csv <- Tabelle T1 (Partei x LP,
#                                                  n_sent, valid/excluded)
#
# Prinzipien:
#   * NIE aus dem Gedaechtnis: jede Zahl kommt aus einer Datei; fehlt die
#     Datei/Spalte, wird der Eintrag als [MISSING] bzw. [MANUAL] registriert
#     (mit Grund), das Skript bricht NICHT ab.
#   * Werte, die nur in Konsolen-Logs existieren (z.B. Regex-vs-SoMaJo-
#     Vergleichslauf, MARPOR-Version), werden als [MANUAL] mit exakter
#     Anweisung registriert.
#   * Laeuft gegen den PROJECT_ROOT aus 00_config.R (aktuell ggf. der
#     Test-Klon!) — vor dem Verifikationspass pruefen, dass der Root auf
#     das Projekt zeigt, dessen Zahlen in die Arbeit gehen.
#
# Aufruf:  Rscript 18_pull_chapter3_numbers.R
# ============================================================================

source(here::here("00_config.R"))

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})
HAVE_ARROW <- requireNamespace("arrow", quietly = TRUE) &&
              requireNamespace("dplyr", quietly = TRUE)
if (!HAVE_ARROW)
  cat("!! arrow/dplyr nicht verfuegbar — Parquet-Korpuszahlen werden als [MISSING] registriert.\n")

TODAY <- format(Sys.Date(), "%Y-%m-%d")
GS_DIR <- file.path(PATHS$out_dir, "goldstandard")

cat("============================================================\n")
cat("[18] Kapitel-3-Zahlen — PROJECT_ROOT:", PROJECT_ROOT, "\n")
cat("============================================================\n\n")

# ---- Registry ----------------------------------------------------------------
REG <- data.table(abschnitt = character(), platzhalter = character(),
                  wert = character(), quelle = character(),
                  hinweis = character(), geprueft_am = character())

fmtv <- function(x, digits = 4) {
  if (is.null(x) || length(x) == 0) return("[MISSING]")
  if (is.numeric(x)) {
    if (length(x) == 1 && is.na(x)) return("[NA]")
    return(paste(format(round(x, digits), big.mark = ".", decimal.mark = ",",
                        scientific = FALSE, trim = TRUE), collapse = "; "))
  }
  paste(as.character(x), collapse = "; ")
}

reg <- function(abschnitt, platzhalter, wert, quelle, hinweis = "") {
  REG <<- rbind(REG, data.table(abschnitt = abschnitt, platzhalter = platzhalter,
                                wert = fmtv(wert), quelle = quelle,
                                hinweis = hinweis, geprueft_am = TODAY))
  cat(sprintf("  [%s] %-52s = %s\n", abschnitt, platzhalter, fmtv(wert)))
  invisible(NULL)
}

reg_manual <- function(abschnitt, platzhalter, quelle, hinweis) {
  reg(abschnitt, platzhalter, "[MANUAL]", quelle, hinweis)
}

reg_missing <- function(abschnitt, platzhalter, quelle, hinweis = "Datei nicht gefunden") {
  reg(abschnitt, platzhalter, "[MISSING]", quelle, hinweis)
}

# defensive Reader: NULL statt Abbruch
read_csv_if <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(fread(path, encoding = "UTF-8"), error = function(e) {
    cat("  !! Lesefehler:", path, "->", conditionMessage(e), "\n"); NULL })
}
read_rds_if <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) {
    cat("  !! Lesefehler:", path, "->", conditionMessage(e), "\n"); NULL })
}
read_json_if <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path), error = function(e) {
    cat("  !! Lesefehler:", path, "->", conditionMessage(e), "\n"); NULL })
}
pick_col <- function(dt, candidates) {
  if (is.null(dt)) return(NULL)
  hit <- intersect(candidates, names(dt))
  if (length(hit)) hit[1] else NULL
}
block <- function(title, expr) {
  cat("\n----", title, "----\n")
  tryCatch(expr, error = function(e)
    cat("  !! Abschnitt uebersprungen:", conditionMessage(e), "\n"))
}

# fuer den MD-Report: kleine Tabellen 1:1 einbetten
MD_TABLES <- list()
md_table <- function(key, dt, caption) {
  if (is.null(dt) || !nrow(dt)) return(invisible(NULL))
  MD_TABLES[[key]] <<- list(dt = as.data.table(dt), caption = caption)
  invisible(NULL)
}

# ============================================================================
# KONSTANTEN aus 00_config.R  (Offener Punkt 14.5; 3.1.3 / 3.6.1 / 3.7 / 3.9)
# ============================================================================
block("Konstanten (00_config.R)", {
  reg("3.1.3", "N_SENT_MIN (Mindest-Satzzahl je Zelle)", N_SENT_MIN, "00_config.R")
  reg("3.7.4", "BOOTSTRAP_DRAWS", BOOTSTRAP_DRAWS, "00_config.R")
  reg("3.7.4", "BOOTSTRAP_SEED", BOOTSTRAP_SEED, "00_config.R")
  reg("3.6.1", "FILTER000_THRESHOLD (is_000 := p_000 >= x)", FILTER000_THRESHOLD, "00_config.R")
  reg("3.7.2", "Primaerspezifikation (Filter x 305 x tau)",
      paste(FILTER_PRIMARY, CODE305_PRIMARY_H1A_H2A, sprintf("tau=%.1f", TAU_PRIMARY), sep = " x "),
      "00_config.R", "H1a/H2a; B-Ebene: code305 = incl (CODE305_PRIMARY_DEFAULT)")
  reg("3.7.2", "Temperatur-Achse tau", paste(sprintf("%s=%.1f", names(TEMPERATURES), TEMPERATURES), collapse = ", "),
      "00_config.R")
  reg("3.9",   "Argmax-Schwellen (Arm 2)", paste(sprintf("%s=%.1f", names(CONF_THRESHOLDS), CONF_THRESHOLDS), collapse = ", "),
      "00_config.R")
  reg("3.7.1", "Anzahl Buckets Aggregation A", length(BUCKETS_A), "00_config.R",
      paste(BUCKETS_A, collapse = " | "))
  reg("3.7.1", "Anzahl Buckets Aggregation B / Domaenen",
      sprintf("%d Buckets in %d Domaenen", length(BUCKETS_B), length(DOMAINS_B)), "00_config.R",
      paste(BUCKETS_B, collapse = " | "))
  reg("3.1.3", "Parteien x LPs (moegliche Zellen)",
      sprintf("%d x %d = %d", length(PARTIES_KEEP), length(LEGISLATIVE_PERIODS),
              length(PARTIES_KEEP) * length(LEGISLATIVE_PERIODS)),
      "00_config.R", paste(PARTIES_KEEP, collapse = ", "))
  reg("3.9",   "Akzeptanzkriterium tau-Sweep (Arm 1)", "Spearman rho >= 0,90",
      "17_robustness.R (RHO_MIN PART 1)")
  reg("3.9",   "Akzeptanzkriterium Threshold (Arm 2)", "Spearman rho >= 0,85",
      "17_robustness.R (RHO_MIN PART 2)")
  reg("3.8",   "Quelle VOTE_SHARES/SEAT_SHARES",
      "Bundeswahlleiterin (amtl. Endergebnisse) + Deutscher Bundestag (Sitzverteilung, Start der LP)",
      "00_config.R (Kommentar WEIGHT LOOKUPS)", "Offener Punkt 14.6 — im Text zitierbar machen")
})

# ============================================================================
# 3.1.1 — GermaParl2: Redenzahl
# ============================================================================
block("3.1.1 Reden (01_build_speech_corpus.R)", {
  f_csv <- file.path(PROJECT_ROOT, "Data", "all_speeches.csv")
  f_rds <- file.path(PROJECT_ROOT, "Data", "speeches_by_lp.rds")
  n_speeches <- NA_integer_; per_lp <- NULL
  if (file.exists(f_rds)) {
    sp <- read_rds_if(f_rds)
    if (!is.null(sp)) {
      if (is.list(sp) && !is.data.frame(sp)) {
        per_lp <- data.table(lp = names(sp), n_speeches = vapply(sp, NROW, integer(1)))
        n_speeches <- sum(per_lp$n_speeches)
      } else {
        sp <- as.data.table(sp); n_speeches <- nrow(sp)
        if ("lp" %in% names(sp)) per_lp <- sp[, .(n_speeches = .N), by = lp][order(lp)]
      }
    }
  } else if (file.exists(f_csv)) {
    hdr <- names(fread(f_csv, nrows = 0))
    sel <- if ("lp" %in% hdr) "lp" else hdr[1]
    sp <- fread(f_csv, select = sel, encoding = "UTF-8")
    n_speeches <- nrow(sp)
    if ("lp" %in% names(sp)) per_lp <- sp[, .(n_speeches = .N), by = lp][order(lp)]
  }
  if (is.na(n_speeches)) {
    reg_missing("3.1.1", "Anzahl Reden LP 13-20 (Arbeitswert 123.883)",
                "Data/speeches_by_lp.rds | Data/all_speeches.csv")
  } else {
    reg("3.1.1", "Anzahl Reden LP 13-20", n_speeches,
        if (file.exists(f_rds)) "Data/speeches_by_lp.rds" else "Data/all_speeches.csv",
        "Extraktion: speaker_role in {mp, government}, >=100 Woerter (01/00_config)")
    if (!is.null(per_lp)) md_table("reden_lp", per_lp, "3.1.1 — Reden je LP")
  }
  reg_manual("3.1.1", "GermaParl2-Korpusversion + Zenodo-DOI [LOCAL]",
             "01-Konsolenlog / CWB-Registry",
             "v2.3.0-rc1 laut letzter Session; exakte Version + DOI aus dem 01-Log bzw. registry ablesen")
})

# ============================================================================
# 3.1.2 — MARPOR (Version = [LOCAL], Offener Punkt 14.3)
# ============================================================================
block("3.1.2 MARPOR", {
  md <- read_rds_if(file.path(PROJECT_ROOT, "Data", "manifesto_distributions.rds"))
  if (!is.null(md)) {
    mdt <- as.data.table(md)
    keyc <- intersect(c("party", "party_label", "lp", "election_date"), names(mdt))
    if (length(keyc) >= 2) {
      n_man <- nrow(unique(mdt[, ..keyc]))
      reg("3.1.2", "Manifest-(Partei,Wahl)-Kombinationen im Sample", n_man,
          "Data/manifesto_distributions.rds")
    }
  } else reg_missing("3.1.2", "manifesto_distributions.rds vorhanden", "Data/manifesto_distributions.rds")
  reg_manual("3.1.2", "MARPOR Main Dataset Version + Zugriffsdatum [LOCAL]",
             "05_pull_marpor.R Konsolenlog / manifestoR",
             "manifestoR haelt die Version nicht auf Platte: einmal interaktiv mp_which_dataset_versions() bzw. beim 05-Lauf die gemeldete Version notieren (Offener Punkt 14.3)")
})

# ============================================================================
# 3.1.3 — Zellmatrix: valide Zellen, Ausschluesse, T1
# ============================================================================
block("3.1.3 Zellmatrix (bootstrap_ci.rds / cell_diag.rds / excluded_cells.csv)", {
  ci <- read_rds_if(file.path(PATHS$cache_dir, "bootstrap_ci.rds"))
  if (!is.null(ci)) {
    ci <- as.data.table(ci)
    if ("is_primary" %in% names(ci)) {
      prim <- ci[is_primary == TRUE]
      reg("3.1.3", "Valide Zellen (is_primary == TRUE)", nrow(prim),
          "cache/bootstrap_ci.rds", "Erwartung laut Plan: 38")
      if (all(c("party", "lp", "n_sent") %in% names(prim)))
        reg("3.1.3", "n_sent ueber valide Zellen (min / median / max)",
            sprintf("%s / %s / %s", fmtv(min(prim$n_sent), 0),
                    fmtv(stats::median(prim$n_sent), 0), fmtv(max(prim$n_sent), 0)),
            "cache/bootstrap_ci.rds")
    } else reg("3.1.3", "Valide Zellen", "[MISSING]", "cache/bootstrap_ci.rds",
               "Spalte is_primary fehlt")
  } else reg_missing("3.1.3", "Valide Zellen (38)", "cache/bootstrap_ci.rds",
                     "12_aggregate.R noch nicht gelaufen?")

  exc <- read_csv_if(file.path(PATHS$out_dir, "excluded_cells.csv"))
  if (!is.null(exc)) {
    reg("3.1.3", "Dokumentierte Ausschluss-Zellen (excluded_cells.csv)", nrow(exc),
        "excluded_cells.csv",
        paste(unique(exc$exclusion_reason), collapse = " | "))
    md_table("excluded", exc, "3.1.3 — excluded_cells.csv (vollstaendig)")
  } else reg("3.1.3", "excluded_cells.csv", "[nicht vorhanden]",
             "excluded_cells.csv", "wird von 14 nur geschrieben, wenn Ausschluesse anfallen")

  # T1: Partei x LP mit n_sent + Status (valid / gate / strukturell leer)
  cd <- read_rds_if(file.path(PATHS$cache_dir, "cell_diag.rds"))
  if (!is.null(cd) && !is.null(ci) && "is_primary" %in% names(ci)) {
    cd <- as.data.table(cd)
    nsc <- pick_col(cd, c("n_sent", "n_sentences", "n"))
    if (!is.null(nsc) && all(c("party", "lp") %in% names(cd))) {
      base <- CJ(party = PARTIES_KEEP, lp = LEGISLATIVE_PERIODS)
      t1 <- merge(base, unique(cd[, c("party", "lp", nsc), with = FALSE]),
                  by = c("party", "lp"), all.x = TRUE)
      setnames(t1, nsc, "n_sent")
      vkey <- ci[is_primary == TRUE, paste(party, lp)]
      t1[, status := fifelse(paste(party, lp) %in% vkey, "valid",
                     fifelse(is.na(n_sent) | n_sent == 0, "empty",
                     fifelse(n_sent < N_SENT_MIN, "below_N_SENT_MIN", "excluded_other")))]
      t1[party == "AfD" & lp <= 18 & status != "valid", status := "structural (AfD ab LP19)"]
      t1[party == "FDP" & lp == 18 & status != "valid", status := "excluded (FDP LP18 5%-Huerde)"]
      fwrite(t1[order(party, lp)], file.path(PATHS$out_dir, "chapter3_T1_cell_matrix.csv"))
      reg("3.1.3", "Tabelle T1 geschrieben", "chapter3_T1_cell_matrix.csv",
          "cache/cell_diag.rds + bootstrap_ci.rds",
          sprintf("Status-Verteilung: %s",
                  paste(sprintf("%s=%d", names(table(t1$status)), as.integer(table(t1$status))), collapse = ", ")))
      md_table("t1", dcast(t1, party ~ lp, value.var = "n_sent"),
               "3.1.3 — T1: n_sent je (Partei, LP) [Status siehe chapter3_T1_cell_matrix.csv]")
    } else reg("3.1.3", "Tabelle T1", "[MISSING]", "cache/cell_diag.rds",
               sprintf("erwartete Spalten party/lp/n_sent; vorhanden: %s", paste(names(cd), collapse = ", ")))
  } else reg_missing("3.1.3", "Tabelle T1 (Partei x LP-Matrix)", "cache/cell_diag.rds")
})

# ============================================================================
# 3.3.1 / 3.3.2 — Korpus: Satzzahlen, Prozedural-Flag, Fehltrennung (Silber)
# ============================================================================
block("3.3 Korpuszahlen (Parquet / procedural_flag_stats / Sonnet-CSV)", {
  pq_dir <- if (exists(".PARQUET_DIR_000_OVERRIDE") && dir.exists(.PARQUET_DIR_000_OVERRIDE))
    .PARQUET_DIR_000_OVERRIDE else PATHS$parquet_dir
  if (HAVE_ARROW && dir.exists(pq_dir) && length(list.files(pq_dir, pattern = "\\.parquet$"))) {
    ds <- arrow::open_dataset(pq_dir)
    cols <- names(ds$schema)
    n_total <- nrow(ds)
    reg("3.3.1", "Saetze im klassifizierten Korpus (gesamt)", n_total,
        basename(pq_dir), "post-SoMaJo, post-Verwurf reiner Nicht-Alpha-Fragmente")

    if ("is_procedural" %in% cols) {
      agg <- ds |> dplyr::summarise(n = dplyr::n(),
                                    n_proc = sum(as.integer(is_procedural), na.rm = TRUE)) |>
        dplyr::collect() |> as.data.table()
      reg("3.3.2", "Prozedural geflaggt (exact-only): Anzahl", agg$n_proc, basename(pq_dir))
      reg("3.3.2", "Prozedural geflaggt (exact-only): Anteil",
          sprintf("%.3f%%", 100 * agg$n_proc / agg$n), basename(pq_dir),
          "Plan-Arbeitswert 5,758% stammt aus v1 (exact+rule) — dieser Wert ist der neue exact-only-Wert")
    } else reg("3.3.2", "Prozedural-Anteil", "[MISSING]", basename(pq_dir), "Spalte is_procedural fehlt")

    # 3.6.1: is_000-Rate bei der Config-Schwelle (12 rederiviert aus p_000)
    if ("p_000" %in% cols) {
      agg0 <- ds |> dplyr::summarise(
        n = dplyr::n(),
        n_000  = sum(as.integer(p_000 >= FILTER000_THRESHOLD), na.rm = TRUE),
        n_p0na = sum(as.integer(is.na(p_000)))) |>
        dplyr::collect() |> as.data.table()
      reg("3.6.1", sprintf("is_000-Rate im Korpus (p_000 >= %.2f)", FILTER000_THRESHOLD),
          sprintf("%.2f%% (%s Saetze)", 100 * agg0$n_000 / agg0$n, fmtv(agg0$n_000, 0)),
          basename(pq_dir),
          if (agg0$n_p0na > 0) sprintf("ACHTUNG: %d Saetze ohne p_000 (11 unvollstaendig?)", agg0$n_p0na) else "")
      if ("is_procedural" %in% cols) {
        aggu <- ds |> dplyr::summarise(
          n = dplyr::n(),
          n_drop = sum(as.integer(is_procedural | (p_000 >= FILTER000_THRESHOLD)), na.rm = TRUE)) |>
          dplyr::collect() |> as.data.table()
        reg("3.6.1", "filtered_000-Drop gesamt (is_procedural ODER is_000)",
            sprintf("%.2f%% (%s Saetze; verbleibend %s)", 100 * aggu$n_drop / aggu$n,
                    fmtv(aggu$n_drop, 0), fmtv(aggu$n - aggu$n_drop, 0)),
            basename(pq_dir), "Union-Definition aus 00_config.R")
      }
    } else reg("3.6.1", "is_000-Rate im Korpus", "[MISSING]", basename(pq_dir),
               "Spalte p_000 fehlt — 11_apply_000.py noch nicht gelaufen (Offener Punkt 14.2)")
  } else reg_missing("3.3.1", "Klassifizierter Parquet-Korpus", pq_dir)

  # Post-SoMaJo Rohzahl (vor Klassifikation) aus sentences_somajo.csv, falls da
  f_som <- file.path(PROJECT_ROOT, "Data", "sentences_somajo.csv")
  if (file.exists(f_som)) {
    hdr <- names(fread(f_som, nrows = 0))
    n_som <- nrow(fread(f_som, select = hdr[1], encoding = "UTF-8"))
    reg("3.3.1", "Saetze nach SoMaJo-Resegmentierung (Rohdatei)", n_som, "Data/sentences_somajo.csv")
  }
  reg_manual("3.3.1", "Regex-vs-SoMaJo-Vergleich (5.977.336 -> 5.751.659; ~3,8%) + Tabelle T2",
             "Diagnostik-Output des Vergleichslaufs (02-Session)",
             "stammt aus dem separaten Vergleichslauf, nicht aus der Pipeline — Log/Diagnostik-CSV der Session heranziehen")

  # Fehltrennungsrate im Silber-Sample
  son <- read_csv_if(file.path(GS_DIR, "control_sample_10k_sonnet.csv"))
  if (!is.null(son)) {
    if ("status" %in% names(son)) son <- son[status == "ok"]
    if ("fehltrennung" %in% names(son)) {
      fr <- mean(son$fehltrennung %in% c(TRUE, "True", "TRUE", "true", 1))
      reg("3.3.1", "Fehltrennungsrate Silber (Sonnet-Flag)",
          sprintf("%.2f%% (n=%s)", 100 * fr, fmtv(nrow(son), 0)),
          "goldstandard/control_sample_10k_sonnet.csv", "Plan-Arbeitswert 0,91%")
    } else reg("3.3.1", "Fehltrennungsrate Silber", "[MISSING]",
               "control_sample_10k_sonnet.csv", "Spalte fehltrennung fehlt")
  } else reg_missing("3.3.1", "Fehltrennungsrate Silber (0,91%)",
                     "goldstandard/control_sample_10k_sonnet.csv")

  # 03-Statistikdatei, falls vorhanden (v2 exact-only)
  pf <- read_csv_if(file.path(PROJECT_ROOT, "results", "procedural_flag_stats.csv"))
  if (!is.null(pf)) md_table("procstats", pf, "3.3.2 — procedural_flag_stats.csv (03, exact-only)")
})

# ============================================================================
# 3.4 — Klassifikator (statische Werkzeug-Angaben aus 04)
# ============================================================================
block("3.4 ManifestoBERTa (statisch aus 04_classify_manifestoberta.py)", {
  reg("3.4", "Modell [LOCAL]",
      "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1",
      "04_classify_manifestoberta.py (MODEL_NAME)")
  reg("3.4", "Kontextfenster-Definition",
      "context = context_before + sentence + context_after (greedy bis 200 Tokens; Fallback: Satz als eigener Kontext); sentence ~100 Tokens; max_length=300",
      "04_classify_manifestoberta.py (Header + Konstanten)")
  reg("3.4", "Tokenizer", "xlm-roberta-large (separat geladen)", "04_classify_manifestoberta.py")
})

# ============================================================================
# 3.5.1 — Goldstandard (06-Outputs)
# ============================================================================
block("3.5.1 Gold (goldstandard/*.csv)", {
  car <- read_csv_if(file.path(GS_DIR, "goldstandard_v2_carried.csv"))
  if (!is.null(car)) reg("3.5.1", "Gold-Saetze (carried, v2)", nrow(car),
                         "goldstandard/goldstandard_v2_carried.csv", "Plan-Arbeitswert 259")
  else reg_missing("3.5.1", "Gold-Saetze (259)", "goldstandard/goldstandard_v2_carried.csv")

  vs <- read_csv_if(file.path(GS_DIR, "goldstandard_v2_validation_summary.csv"))
  if (!is.null(vs)) {
    for (i in seq_len(nrow(vs)))
      reg("3.5.1", sprintf("Gold %s (n=%s)", vs$metric[i], vs$n[i]), vs$value[i],
          "goldstandard/goldstandard_v2_validation_summary.csv")
    md_table("gold_vs", vs, "3.5.1 — goldstandard_v2_validation_summary.csv")
  } else reg_missing("3.5.1", "kappa_A raw / codeable / kappa_B (0,570 / 0,635 / ~0,27)",
                     "goldstandard/goldstandard_v2_validation_summary.csv")

  g305 <- read_csv_if(file.path(GS_DIR, "gold_305_precision_v2.csv"))
  if (!is.null(g305)) {
    reg("3.5.1/3.6.2", "305-Praezision Gold (share_confirmed)",
        sprintf("%.1f%% (n_ai305_filtered=%s)", 100 * g305$precision_305[1], g305$n_ai305_filtered[1]),
        "goldstandard/gold_305_precision_v2.csv",
        "Fehlerliste (A): 37%, NICHT 44%")
    if ("share_not_assignable" %in% names(g305))
      reg("3.6.2", "AI-305 human als nicht-zuordenbar (000)",
          sprintf("%.1f%%", 100 * g305$share_not_assignable[1]),
          "goldstandard/gold_305_precision_v2.csv")
  } else reg_missing("3.5.1/3.6.2", "305-Praezision Gold (ca. 0,37)",
                     "goldstandard/gold_305_precision_v2.csv")

  kf <- read_csv_if(file.path(GS_DIR, "agreement_kappa_flavors.csv"))
  if (!is.null(kf)) md_table("gold_kf", kf, "3.5.1 — agreement_kappa_flavors.csv (06 Teil 2: Human/BERT/Claude-Tripel)")

  bb <- read_csv_if(file.path(GS_DIR, "goldstandard_v2_bucket_balance.csv"))
  if (!is.null(bb)) md_table("gold_bb", bb, "3.5.1/3.5.3 — goldstandard_v2_bucket_balance.csv (305-Unterrepraesentation im Gold)")
})

# ============================================================================
# 3.5.2 — Silber / Kontrollsample (09-Outputs)
# ============================================================================
block("3.5.2 Silber (goldstandard/control_*.csv)", {
  ov <- read_csv_if(file.path(GS_DIR, "control_agreement_overview.csv"))
  if (!is.null(ov)) {
    for (i in seq_len(nrow(ov)))
      reg("3.5.2", sprintf("Silber kappa %s", ov[[1]][i]), ov$Kappa[i],
          "goldstandard/control_agreement_overview.csv",
          "Fehlerliste (D): Feincode-kappa ist SILBER, nicht Gold")
    md_table("silver_ov", ov, "3.5.2 — control_agreement_overview.csv (BERT vs. Sonnet)")
  } else reg_missing("3.5.2", "Silber-Kappas (Domaene ~0,58 / direktional ~0,70 / Feincode ~0,36)",
                     "goldstandard/control_agreement_overview.csv")

  fl <- read_csv_if(file.path(GS_DIR, "control_agreement_kappa_flavors.csv"))
  if (!is.null(fl)) md_table("silver_fl", fl, "3.5.2 — control_agreement_kappa_flavors.csv (roh vs. codeable-only)")

  reg_manual("3.5.2", "Sample B + Welfare-Booster in kappa eingeflossen?",
             "09-Konsolenlog / 10_train_000.py LABEL_FILES",
             "Offener Punkt 14.7 — 09 liest control_sample_10k_sonnet.csv (Sample A); 10 trainiert auf A+B. Gegen 09-Output pruefen, welche Basis die berichteten kappas haben")
})

# ============================================================================
# 3.5.3 — Diagnose: zwei Fehlermodi (09-Outputs + Roh-Silber)
# ============================================================================
block("3.5.3 Fehlermodi", {
  ns <- read_csv_if(file.path(GS_DIR, "control_nullclass_summary.csv"))
  if (!is.null(ns)) {
    for (i in seq_len(nrow(ns)))
      reg("3.5.3", sprintf("Null-Klasse: %s", ns[[1]][i]), ns[[2]][i],
          "goldstandard/control_nullclass_summary.csv",
          "Fehlerliste (C): korpusweite Non-codeable-Rate ~22,9%, nicht ~9%")
    md_table("nullsum", ns, "3.5.3 — control_nullclass_summary.csv")
  } else reg_missing("3.5.3", "Sonnet-000-Rate (~22,9%)", "goldstandard/control_nullclass_summary.csv")

  wh <- read_csv_if(file.path(GS_DIR, "control_nullclass_where_bert_puts_them.csv"))
  if (!is.null(wh)) {
    dpsrow <- wh[grepl("Democracy", wh[[1]])]
    if (nrow(dpsrow))
      reg("3.5.3", "Anteil Sonnet-000, die BERT in den DPS-Bucket legt",
          sprintf("%.1f%%", 100 * dpsrow$anteil[1]),
          "goldstandard/control_nullclass_where_bert_puts_them.csv",
          "Bucket-Ebene; die 53,8% des Plans sind CODE-Ebene (BERT-Code 305) — siehe naechster Eintrag")
    md_table("where", wh, "3.5.3 — control_nullclass_where_bert_puts_them.csv (Bucket-Ebene)")
  }

  # Code-Ebene direkt aus dem Roh-Silber (BERT-Code vs. Sonnet-Code)
  son <- read_csv_if(file.path(GS_DIR, "control_sample_10k_sonnet.csv"))
  if (!is.null(son)) {
    if ("status" %in% names(son)) son <- son[status == "ok"]
    bcol <- pick_col(son, c("pred_code", "bert_code", "bert_pred", "pred"))
    scol <- pick_col(son, c("marpor", "sonnet_code", "sonnet_marpor"))
    ccol <- pick_col(son, c("pred_score", "bert_score", "pred_prob", "score"))
    if (!is.null(bcol) && !is.null(scol)) {
      b <- as.character(son[[bcol]]); s <- as.character(son[[scol]])
      reg("3.5.3", "argmax-305-Rate BERT (Silber-Saetze)",
          sprintf("%.1f%%", 100 * mean(b == "305", na.rm = TRUE)),
          "goldstandard/control_sample_10k_sonnet.csv", "Plan-Arbeitswert 24,9%")
      reg("3.5.3", "305-Rate Sonnet (dieselben Saetze)",
          sprintf("%.1f%%", 100 * mean(s == "305", na.rm = TRUE)),
          "goldstandard/control_sample_10k_sonnet.csv", "Plan-Arbeitswert 4,1%")
      is000 <- s %in% c("0", "00", "000")
      reg("3.5.3", "Sonnet-000 -> BERT-305 (Code-Ebene)",
          sprintf("%.1f%%", 100 * mean(b[is000] == "305", na.rm = TRUE)),
          "goldstandard/control_sample_10k_sonnet.csv", "Plan-Arbeitswert ~53,8%")
      if (!is.null(ccol)) {
        cs <- suppressWarnings(as.numeric(son[[ccol]]))
        reg("3.5.3", "Median-Konfidenz BERT auf Sonnet-000->305",
            stats::median(cs[is000 & b == "305"], na.rm = TRUE),
            "goldstandard/control_sample_10k_sonnet.csv", "Plan-Arbeitswert 0,74 ('konfidenter Irrtum')")
        reg("3.5.3", "Sonnet-000 mit Nicht-305-Code und Konfidenz > 0,7",
            sprintf("%.1f%%", 100 * mean(b[is000] != "305" & cs[is000] > 0.7, na.rm = TRUE)),
            "goldstandard/control_sample_10k_sonnet.csv", "Plan-Arbeitswert ~9,5%")
      }
    } else reg("3.5.3", "Code-Ebene 305/000 (Silber)", "[MISSING]",
               "control_sample_10k_sonnet.csv",
               sprintf("BERT-/Sonnet-Code-Spalten nicht erkannt; vorhanden: %s", paste(names(son), collapse = ", ")))
  }

  cdist <- read_csv_if(file.path(GS_DIR, "control_corpus_distribution.csv"))
  if (!is.null(cdist)) md_table("cdist", cdist, "3.5.3 — control_corpus_distribution.csv (Domaenen-Agenda Sonnet vs. BERT)")
})

# ============================================================================
# 3.6.1 — gBERT-000-Filter (10/11-Outputs)
# ============================================================================
block("3.6.1 gBERT-000 (model_000_text_metrics.json / filter_config.json)", {
  mm <- read_json_if(file.path(PROJECT_ROOT, "model_000_text_metrics.json"))
  if (is.null(mm)) mm <- read_json_if(file.path(PROJECT_ROOT, "model_000_gbert", "model_000_text_metrics.json"))
  if (!is.null(mm)) {
    grab <- function(k) if (!is.null(mm[[k]])) round(as.numeric(mm[[k]]), 3) else NA_real_
    reg("3.6.1", "gBERT PR-AUC (000)", grab("pr_auc"), "model_000_text_metrics.json", "Plan-Arbeitswert 0,852")
    reg("3.6.1", "gBERT ROC-AUC", grab("roc_auc"), "model_000_text_metrics.json", "Plan-Arbeitswert 0,939")
    reg("3.6.1", "gBERT F1@0,5", grab("f1_at_0.5"), "model_000_text_metrics.json", "Plan-Arbeitswert 0,745")
    reg("3.6.1", "gBERT Precision/Recall @0,5",
        sprintf("%s / %s", fmtv(grab("precision_at_0.5"), 3), fmtv(grab("recall_at_0.5"), 3)),
        "model_000_text_metrics.json")
    for (k in intersect(c("n", "n_train", "n_total", "n_pos", "pos_rate", "class_balance", "cv_folds"), names(mm)))
      reg("3.6.1", sprintf("gBERT Trainingsdesign: %s", k), mm[[k]], "model_000_text_metrics.json")
  } else reg_missing("3.6.1", "gBERT-Holdout (PR-AUC 0,852 / ROC-AUC 0,939 / F1 0,745)",
                     "model_000_text_metrics.json", "liegt im Arbeitsverzeichnis des 10-Laufs")

  fc <- read_json_if(file.path(PROJECT_ROOT, "model_000_gbert", "filter_config.json"))
  if (!is.null(fc)) {
    reg("3.6.1", "filter_config.json (thr_default / thr_recall90)",
        sprintf("%s / %s", fmtv(fc$thr_default, 3), fmtv(fc$thr_recall90, 3)),
        "model_000_gbert/filter_config.json",
        sprintf("operativ genutzt wird FILTER000_THRESHOLD=%.2f aus 00_config.R (R-seitiger Knopf)", FILTER000_THRESHOLD))
  } else reg_missing("3.6.1", "filter_config.json", "model_000_gbert/filter_config.json")

  reg("3.6.1", "Trainingsmodell", "deepset/gbert-large, Labels: Sonnet-Silber (Sample A+B)",
      "10_train_000.py (statisch)")
})

# ============================================================================
# 3.6.2 — 305-Exklusion (13 / 17 PART 3)
# ============================================================================
block("3.6.2 305-Exklusion", {
  asym <- read_csv_if(file.path(PATHS$out_dir, "dps_305_asymmetry.csv"))
  if (!is.null(asym)) {
    reg("3.6.2", "305-Asymmetrie: mittlerer Excess (Rede-305 minus Manifest-305)",
        round(mean(asym$excess), 3), "dps_305_asymmetry.csv", "Plan-Arbeitswert +0,132")
    reg("3.6.2", "Zellen mit Excess > 0",
        sprintf("%d / %d (%.0f%%)", sum(asym$excess > 0), nrow(asym), 100 * mean(asym$excess > 0)),
        "dps_305_asymmetry.csv", "Plan-Arbeitswert 100%")
    reg("3.6.2", "305-Excess min / median / max",
        sprintf("%s / %s / %s", fmtv(min(asym$excess), 3),
                fmtv(stats::median(asym$excess), 3), fmtv(max(asym$excess), 3)),
        "dps_305_asymmetry.csv")
  } else reg_missing("3.6.2", "305-Asymmetrie (+0,132 in 100% der Zellen)",
                     "dps_305_asymmetry.csv")

  pcs <- read_csv_if(file.path(PATHS$out_dir, "dps_percode_summary.csv"))
  if (!is.null(pcs)) md_table("percode", pcs, "3.6.2 — dps_percode_summary.csv (per-Code-Beitraege im DPS-Bucket)")

  red <- read_csv_if(file.path(PATHS$out_dir, "intervention_000_vs_305_redundancy.csv"))
  if (!is.null(red) && nrow(red) == 4) {
    v <- red$dps_contrib
    names(v) <- c("D_raw", "B_old", "A_new", "C_dbl")
    for (k in names(v)) reg("3.6.2", sprintf("Redundanzobjekt %s (DPS-Beitrag)", k), v[[k]],
                            "intervention_000_vs_305_redundancy.csv")
    excl_marg   <- v[["A_new"]] - v[["C_dbl"]]   # was excl ZUSAETZLICH zu filtered_000 entfernt
    filt_marg   <- v[["D_raw"]] - v[["A_new"]]   # was der 000-Filter (inkl. Prozedurfilter) allein entfernt
    reg("3.6.2", "excl-Marginaleffekt (A_new - C_dbl)", round(excl_marg, 4),
        "intervention_000_vs_305_redundancy.csv (abgeleitet)")
    reg("3.6.2", "Filter-Marginaleffekt (D_raw - A_new)", round(filt_marg, 4),
        "intervention_000_vs_305_redundancy.csv (abgeleitet)")
    reg("3.6.2", "Kandidat 'excl entfernt X% dessen, was der Filter entfernt' ((A_new-C_dbl)/(D_raw-A_new))",
        sprintf("%.0f%%", 100 * excl_marg / filt_marg),
        "intervention_000_vs_305_redundancy.csv (abgeleitet)",
        "Plan-Arbeitswert ~113% — Definition gegen das 17-PART-3-Konsolenverdikt gegenpruefen, bevor der Satz in den Text geht")
    reg("3.6.2", "Nicht-Redundanz-Schwelle (pre-stated)",
        "redundant falls excl < 15% zusaetzlich entfernt UND Konvergenzkriterium erfuellt",
        "17_robustness.R PART 3 / 13 (pre-committed)", "Verdikt-Logik stand vor den v2-Zahlen")
    md_table("redundancy", red, "3.6.2 — intervention_000_vs_305_redundancy.csv (D/B/A/C)")
  } else reg_missing("3.6.2", "Redundanzobjekte D_raw/B_old/A_new/C_dbl (~113%)",
                     "intervention_000_vs_305_redundancy.csv")

  dsum <- read_csv_if(file.path(PATHS$out_dir, "intervention_dps_summary.csv"))
  if (!is.null(dsum)) md_table("dpsmatrix", dsum, "3.6.2/3.9 — intervention_dps_summary.csv (3x2-Interventionsleiter)")

  reg_manual("3.6.2", "Genuiner DPS-Anteil der AI-305 (~14-20%)",
             "13-Konsolenlog PART 1-3 / Silber-Session 23.06.",
             "kombinierter Befund (Gold-Bestaetigung + Silber-Zerlegung); exakte Herleitung aus dem 13-Summary-Log uebernehmen, nicht rekonstruieren")
})

# ============================================================================
# 3.8 — Empirische Strategie: H1a-Objekte, Benchmarks, H3
# ============================================================================
block("3.8 H1a / Benchmarks / H3", {
  h1a <- read_csv_if(file.path(PATHS$out_dir, "H1a_jsd_table.csv"))
  if (!is.null(h1a)) {
    reg("3.8", "H1a: Zellen in der Primaertabelle", nrow(h1a), "H1a_jsd_table.csv")
    reg("3.8", "H1a: JSD mean / median / min / max",
        sprintf("%s / %s / %s / %s", fmtv(mean(h1a$jsd), 4), fmtv(stats::median(h1a$jsd), 4),
                fmtv(min(h1a$jsd), 4), fmtv(max(h1a$jsd), 4)),
        "H1a_jsd_table.csv")
  } else reg_missing("3.8", "H1a-Tabelle", "H1a_jsd_table.csv")

  near <- read_csv_if(file.path(PATHS$out_dir, "benchmark_nearest_manifesto.csv"))
  if (!is.null(near)) {
    hit <- mean(near$own_is_nearest %in% c(TRUE, "TRUE", "True", 1))
    reg("3.8", "Nearest-Manifesto-Trefferquote",
        sprintf("%.1f%% (%d / %d Zellen)", 100 * hit, sum(near$own_is_nearest %in% c(TRUE, "TRUE", "True", 1)), nrow(near)),
        "benchmark_nearest_manifesto.csv")
    reg("3.8", "Zufallsbaseline Nearest-Manifesto (mean 1/n_manifestos)",
        sprintf("%.1f%%", 100 * mean(1 / near$n_manifestos)),
        "benchmark_nearest_manifesto.csv (abgeleitet)",
        sprintf("n_manifestos je LP: %s", paste(sort(unique(near$n_manifestos)), collapse = ", ")))
    if ("rank_own" %in% names(near))
      reg("3.8", "Nearest-Manifesto: mittlerer Rang des eigenen Programms",
          round(mean(near$rank_own, na.rm = TRUE), 2), "benchmark_nearest_manifesto.csv")
  } else reg_missing("3.8", "Nearest-Manifesto-Benchmark", "benchmark_nearest_manifesto.csv")

  ref <- read_csv_if(file.path(PATHS$out_dir, "benchmark_pairwise_jsd.csv"))
  if (!is.null(ref) && "jsd" %in% names(ref)) {
    sp <- ref[side == "speech"]
    reg("3.8", "Between-Party-Referenz (Rede): mean / median / q25 / q75",
        sprintf("%s / %s / %s / %s", fmtv(mean(sp$jsd), 4), fmtv(stats::median(sp$jsd), 4),
                fmtv(stats::quantile(sp$jsd, .25), 4), fmtv(stats::quantile(sp$jsd, .75), 4)),
        "benchmark_pairwise_jsd.csv", sprintf("n Paare = %d", nrow(sp)))
  } else reg_missing("3.8", "Between-Party-Referenzverteilung", "benchmark_pairwise_jsd.csv")

  h2a <- read_csv_if(file.path(PATHS$out_dir, "H2a_per_bucket_summary.csv"))
  if (!is.null(h2a)) {
    md_table("h2a", h2a, "3.8 — H2a_per_bucket_summary.csv (Beitragszerlegung)")
    bcol <- pick_col(h2a, c("bucket", "bucket_A"))
    if (!is.null(bcol)) {
      dps <- h2a[get(bcol) == BUCKET_OF_305_A]
      if (nrow(dps)) {
        numc <- names(dps)[vapply(dps, is.numeric, logical(1))]
        reg("3.8", "H2a: DPS-Bucket-Zeile",
            paste(sprintf("%s=%s", numc, vapply(dps[1, ..numc], fmtv, character(1))), collapse = ", "),
            "H2a_per_bucket_summary.csv")
      }
    }
  }

  for (f in c("H3_trend_fits.csv", "H3_trend_fits_exafd.csv", "H3_trend_exafd_compare.csv",
              "H3_pre_post_2017.csv")) {
    x <- read_csv_if(file.path(PATHS$out_dir, f))
    if (!is.null(x)) {
      md_table(f, x, sprintf("3.8/H3 — %s", f))
      dcol <- pick_col(x, c("domain", "domain_B", "domaene"))
      if (!is.null(dcol) && f %in% c("H3_trend_fits.csv", "H3_trend_fits_exafd.csv")) {
        eu <- as.data.table(x)[get(dcol) == "Europe"]
        scol <- pick_col(eu, c("slope", "estimate", "beta", "coef"))
        if (nrow(eu) && !is.null(scol)) {
          chn <- pick_col(eu, c("channel", "kanal"))
          for (i in seq_len(nrow(eu))) {
            lab <- if (!is.null(chn)) sprintf(" (%s)", eu[[chn]][i]) else ""
            reg("3.8", sprintf("H3 Europa-Slope%s [%s]", lab, sub("\\.csv$", "", f)),
                round(eu[[scol]][i], 4), f,
                "Sign-Flip bei AfD-Ausschluss ist der H3-Kernbefund (Bericht in Kap. 4)")
          }
        }
      }
    } else reg_missing("3.8", sub("\\.csv$", "", f), f)
  }
})

# ============================================================================
# 3.9 — Robustheit: Spearman-rhos der Arme 1 und 2
# ============================================================================
block("3.9 Robustheitsarme", {
  for (f in c("H1a_temperature_sweep.csv", "H1b_temperature_sweep.csv")) {
    sw <- read_csv_if(file.path(PATHS$out_dir, f))
    if (!is.null(sw)) {
      rc <- grep("^spearman_rho_", names(sw), value = TRUE)
      if (length(rc)) {
        vals <- vapply(rc, function(c) round(unique(sw[[c]])[1], 3), numeric(1))
        reg("3.9", sprintf("Arm 1 tau-Sweep rho [%s]", sub("_temperature_sweep\\.csv$", "", f)),
            paste(sprintf("%s=%s", sub("spearman_rho_", "", rc), vals), collapse = ", "),
            f, "Akzeptanz: rho >= 0,90")
      }
    } else reg_missing("3.9", sprintf("tau-Sweep (%s)", f), f)
  }

  # Arm 2: rho aus den Threshold-Tabellen rekonstruieren (17 schreibt sie nicht als Spalte)
  thr_rho <- function(f, abschn_lab) {
    h <- read_csv_if(file.path(PATHS$out_dir, f))
    if (is.null(h) || !all(c("party", "lp", "spec", "jsd") %in% names(h))) {
      reg_missing("3.9", abschn_lab, f); return(invisible(NULL))
    }
    w <- dcast(as.data.table(h), party + lp ~ spec, value.var = "jsd")
    specs <- setdiff(names(w), c("party", "lp"))
    w <- w[stats::complete.cases(w[, ..specs])]
    if (nrow(w) < 3 || length(specs) < 2) { reg("3.9", abschn_lab, "[zu wenige Zellen]", f); return(invisible(NULL)) }
    prs <- utils::combn(specs, 2, simplify = FALSE)
    vals <- vapply(prs, function(p) round(stats::cor(w[[p[1]]], w[[p[2]]], method = "spearman"), 3), numeric(1))
    reg("3.9", abschn_lab,
        paste(sprintf("%s~%s=%s", vapply(prs, `[`, "", 1), vapply(prs, `[`, "", 2), vals), collapse = ", "),
        paste0(f, " (rho rekonstruiert wie 17 PART 2)"), "Akzeptanz: rho >= 0,85")
  }
  thr_rho("H1a_threshold_robustness.csv", "Arm 2 Threshold rho [H1a]")
  thr_rho("H1b_threshold_robustness.csv", "Arm 2 Threshold rho [H1b]")

  mig <- read_csv_if(file.path(PATHS$out_dir, "h1b_migration_601608_summary.csv"))
  if (!is.null(mig)) md_table("mig", mig, "3.9/Arm 6 — h1b_migration_601608_summary.csv")
})

# ============================================================================
# EXPORT: CSV (Regel-5-Tabelle) + MD-Report
# ============================================================================
block("Export", {
  out_csv <- file.path(PATHS$out_dir, "chapter3_numbers.csv")
  fwrite(REG, out_csv, bom = TRUE)
  cat("\n[18] Regel-5-Tabelle:", out_csv, sprintf("(%d Eintraege)\n", nrow(REG)))

  out_md <- file.path(PATHS$out_dir, "chapter3_numbers.md")
  con <- file(out_md, open = "wb")   # Bytes schreiben: locale-fest (Windows cp1252!)
  w <- function(...) writeLines(enc2utf8(paste0(...)), con, useBytes = TRUE)

  # Abschnitte in Schreibplan-Reihenfolge
  sec_order <- c("3.1.1", "3.1.2", "3.1.3", "3.3.1", "3.3.2", "3.4",
                 "3.5.1", "3.5.1/3.6.2", "3.5.2", "3.5.3", "3.6.1", "3.6.2",
                 "3.7.1", "3.7.2", "3.7.4", "3.8", "3.9")
  secs <- unique(REG$abschnitt)
  secs <- c(intersect(sec_order, secs), setdiff(secs, sec_order))
  w("# Kapitel 3 — Kanonische Zahlen (", TODAY, ")")
  w("")
  w("PROJECT_ROOT: `", PROJECT_ROOT, "`  ")
  w("Regel 1 (Zahlendisziplin): Jeder Wert hier ist direkt aus der genannten ",
    "Quelldatei gezogen. `[MANUAL]` = nur im Konsolen-Log verfuegbar, ",
    "`[MISSING]` = Datei/Spalte fehlt (Pipeline-Status pruefen, Offener Punkt 14.2).")
  w("")
  for (ab in secs) {
    w("## Abschnitt ", ab)
    w("")
    sub <- REG[abschnitt == ab]
    w("| Platzhalter | Wert | Quelle | Hinweis |")
    w("|---|---|---|---|")
    for (i in seq_len(nrow(sub)))
      w("| ", gsub("\\|", "/", sub$platzhalter[i]), " | **", gsub("\\|", "/", sub$wert[i]),
        "** | `", gsub("\\|", "/", sub$quelle[i]), "` | ", gsub("\\|", "/", sub$hinweis[i]), " |")
    w("")
  }
  if (length(MD_TABLES)) {
    w("---")
    w("")
    w("# Eingebettete Kleintabellen (1:1 aus den Quelldateien)")
    w("")
    for (key in names(MD_TABLES)) {
      tb <- MD_TABLES[[key]]
      w("## ", tb$caption)
      w("")
      dt <- tb$dt
      if (nrow(dt) > 60) { dt <- dt[1:60]; w("_(gekuerzt auf 60 Zeilen)_"); w("") }
      w("| ", paste(names(dt), collapse = " | "), " |")
      w("|", paste(rep("---", ncol(dt)), collapse = "|"), "|")
      for (i in seq_len(nrow(dt)))
        w("| ", paste(gsub("\\|", "/", vapply(dt[i], function(x) fmtv(x), character(1))), collapse = " | "), " |")
      w("")
    }
  }
  close(con)
  cat("[18] MD-Report:", out_md, "\n")

  n_missing <- sum(REG$wert == "[MISSING]")
  n_manual  <- sum(REG$wert == "[MANUAL]")
  cat(sprintf("\n[18] FERTIG. %d Zahlen registriert | %d [MISSING] | %d [MANUAL].\n",
              nrow(REG), n_missing, n_manual))
  if (n_missing > 0)
    cat("[18] HINWEIS: [MISSING]-Eintraege deuten auf noch nicht gelaufene Pipeline-Schritte\n",
        "     (z.B. 11-Apply / 12-17 nach dem GPU-Lauf) — erst danach Verifikationspass starten.\n")
})
