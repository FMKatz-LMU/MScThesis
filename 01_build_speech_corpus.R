# ============================================================================
# 01_build_speech_corpus.R — Reden aus GERMAPARL2 ziehen und flach exportieren
# ----------------------------------------------------------------------------
# Stufe 1 (Korpusbau), Sprecher-Zweig. Vereint die beiden bisherigen Skripte:
#   * Script_Speechrefinement.R  (Extraktion aus dem CWB/GERMAPARL2-Korpus)
#   * Rerun_Speechsplit_Refined.R (Flatten der LP-Liste zu all_speeches.csv)
#
# ENTFERNT gegenüber Script_Speechrefinement.R: die naive Regex-Satztrennung
# (ehem. Abschnitt 6–7, strsplit(text, "(?<=[.!?])\\s+")). Diese wird durch
# 02_resegment_somajo.py ersetzt (SoMaJo respektiert Abkürzungen/Ordinalzahlen).
# Dieses Skript endet daher bei EINER ZEILE PRO REDE.
#
# INPUT  : GERMAPARL2 (CWB-Registry unter <project>/cwb/registry)
# OUTPUT : Data/speeches_by_lp.rds   Checkpoint (Liste, eine data.table je LP)
#          Data/all_speeches.csv     EINE Zeile pro Rede -> Input für 02
#          Spalten: speech_id, legislative_period, speaker, party, date, text
#
# Aufruf:  source(here::here("01_build_speech_corpus.R"))
# ============================================================================
install.packages(c("polmineR", "data.table", "here", "fs"))
suppressPackageStartupMessages({
  library(polmineR)
  library(data.table)
})

n_cores <- parallel::detectCores()
data.table::setDTthreads(max(1L, n_cores - 1L))

# CWB-Registry liegt im Projekt (here::here statt getwd(), konsistent mit der Pipeline)
RcppCWB::cqp_initialize(registry = here::here("cwb", "registry"))

DATA_DIR <- here::here("Data")
fs::dir_create(DATA_DIR)

# ---- 1. Reden je Legislaturperiode extrahieren -----------------------------
stopifnot("GERMAPARL2" %in% polmineR::corpus()$corpus)

target_lps     <- c(13, 14, 15, 16, 17, 18, 19, 20)
speeches_by_lp <- list()

for (current_lp in target_lps) {
  cat("\n========================================\n")
  cat("Processing Legislative Period:", current_lp, "\n")

  # Step A: strukturelles Rauschen herausfiltern
  gparl_filtered <- "GERMAPARL2" %>%
    partition(protocol_lp   = as.character(current_lp)) %>%
    partition(p_type        = "speech") %>%
    partition(speaker_role  = c("mp", "government"))

  # Step B: in einzelne Reden segmentieren
  individual_speeches <- as.speeches(
    gparl_filtered,
    s_attribute_name = "speaker_name",
    gap              = 500
  )

  # Step C: Text + Metadaten je Partition ziehen (robust gegen struc-Fehler)
  cat("Extracting text and metadata...\n")
  results <- lapply(seq_along(individual_speeches@objects), function(i) {
    p <- individual_speeches@objects[[i]]
    tryCatch({
      list(
        speech_id = names(individual_speeches)[i],
        speaker   = s_attributes(p, "speaker_name")[1],
        party     = s_attributes(p, "speaker_party")[1],
        date      = s_attributes(p, "protocol_date")[1],
        text      = get_token_stream(p, p_attribute = "word", collapse = " ")
      )
    }, error = function(e) {
      cat("  Skipping speech", i, "due to error:", conditionMessage(e), "\n")
      NULL
    })
  })

  results <- Filter(Negate(is.null), results)
  cat("  Successfully extracted", length(results), "of",
      length(individual_speeches), "speeches.\n")

  final_dt <- rbindlist(lapply(results, function(r) {
    data.table(
      speech_id          = r$speech_id,
      legislative_period = current_lp,
      speaker            = r$speaker,
      party              = r$party,
      date               = r$date,
      text               = r$text
    )
  }))

  # Step D: Klammerannotationen entfernen (Beifall, Zwischenrufe, ...)
  final_dt[, text := trimws(gsub("\\s+", " ", gsub("\\([^)]*\\)", "", text)))]

  # Step E: Wörter auf bereinigtem Text neu zählen, >= 100 Wörter behalten
  final_dt[, word_count := lengths(strsplit(text, "\\s+"))]
  n_before <- nrow(final_dt)
  final_dt <- final_dt[word_count >= 100]
  cat("  Removed parenthetical annotations. Kept", nrow(final_dt),
      "of", n_before, "speeches with >= 100 words after cleaning.\n")

  speeches_by_lp[[as.character(current_lp)]] <- final_dt
}

cat("\nSUCCESS! All legislative periods processed.\n")

# ---- 2. Checkpoint speichern -----------------------------------------------
saveRDS(speeches_by_lp, file.path(DATA_DIR, "speeches_by_lp.rds"))
cat("Saved checkpoint:", file.path(DATA_DIR, "speeches_by_lp.rds"), "\n")

# ---- 3. Flach exportieren (eine Zeile pro Rede) -> Input für SoMaJo (02) ----
# (ehem. Rerun_Speechsplit_Refined.R)
all_speeches <- rbindlist(speeches_by_lp)
all_speeches[, word_count := NULL]   # rein internes Filterfeld, downstream unnötig
fwrite(all_speeches, file.path(DATA_DIR, "all_speeches.csv"))
cat("Saved", nrow(all_speeches), "speeches ->",
    file.path(DATA_DIR, "all_speeches.csv"), "\n")
cat("Next: python 02_resegment_somajo.py  (Satztrennung mit SoMaJo)\n")
