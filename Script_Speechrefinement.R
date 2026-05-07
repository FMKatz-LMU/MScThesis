# TOTAL SCRIPT ALL STEPS FOR THESIS BY FLORIAN KATZMANN
directory <- getwd()

#### 1. Setup & Corpus Initialization ####
# (Assuming packages are already installed and loaded as per your previous script)
library(polmineR)
library(data.table)

n_cores <- parallel::detectCores()
data.table::setDTthreads(n_cores - 1)

RcppCWB::cqp_initialize(registry = file.path(directory, "cwb", "registry"))

# ==========================================
# RUNNING THE LOOP (SYNTAX FIXED)
# ==========================================
# Only proceed if the corpus is actually loaded
if ("GERMAPARL2" %in% polmineR::corpus()$corpus) {
  
  target_lps <- c(13, 14, 15, 16, 17, 18, 19, 20)
  speeches_by_lp <- list()
  
  for (current_lp in target_lps) {
    cat("\n========================================\n")
    cat("Processing Legislative Period:", current_lp, "\n")
    
    # Step A: Filter out structural noise
    gparl_filtered <- "GERMAPARL2" %>% 
      partition(protocol_lp = as.character(current_lp)) %>% 
      partition(p_type = "speech") %>%                             
      partition(speaker_role = c("mp", "government"))              
    
    # Step B: Segment into individual speeches
    individual_speeches <- as.speeches(
      gparl_filtered,
      s_attribute_name = "speaker_name", 
      gap = 500 
    )
    
    # Step C: Extract text and metadata (per-partition to avoid struc errors)
    cat("Extracting text and metadata...\n")
    results <- lapply(seq_along(individual_speeches@objects), function(i) {
      p <- individual_speeches@objects[[i]]
      tryCatch({
        list(
          speech_id = names(individual_speeches)[i],
          speaker  = s_attributes(p, "speaker_name")[1],
          party    = s_attributes(p, "speaker_party")[1],
          date     = s_attributes(p, "protocol_date")[1],
          text     = get_token_stream(p, p_attribute = "word", collapse = " ")
        )
      }, error = function(e) {
        cat("  Skipping speech", i, "due to error:", conditionMessage(e), "\n")
        NULL
      })
    })

    results <- Filter(Negate(is.null), results)
    cat("  Successfully extracted", length(results), "of", length(individual_speeches), "speeches.\n")

    final_dt <- rbindlist(lapply(results, function(r) {
      data.table(
        speech_id = r$speech_id,
        legislative_period = current_lp,
        speaker = r$speaker,
        party = r$party,
        date = r$date,
        text = r$text
      )
    }))

    # Step D: Remove parenthetical annotations (e.g. Beifall, Zwischenrufe)
    final_dt[, text := trimws(gsub("\\s+", " ", gsub("\\([^)]*\\)", "", text)))]

    # Step E: Recount words on cleaned text and enforce >= 100 word threshold
    final_dt[, word_count := lengths(strsplit(text, "\\s+"))]
    n_before <- nrow(final_dt)
    final_dt <- final_dt[word_count >= 100]
    cat("  Removed parenthetical annotations. Kept", nrow(final_dt),
        "of", n_before, "speeches with >= 100 words after cleaning.\n")
    
    speeches_by_lp[[as.character(current_lp)]] <- final_dt
  }
  
  cat("\nSUCCESS! All legislative periods processed.\n")
  
} else {
  cat("\nERROR: GERMAPARL2 is not loaded in the current session.\n")
}


#### 4. Save results ####
saveRDS(speeches_by_lp, file.path(directory, "Data", "speeches_by_lp.rds"))
cat("Saved to", file.path(directory, "Data", "speeches_by_lp.rds"), "\n")

#### 5. Load saved results (use this on restart instead of re-running everything) ####
# speeches_by_lp <- readRDS(file.path(getwd(), "Data", "speeches_by_lp.rds"))

#### 6. Sentence splitting with context ####
all_speeches <- rbindlist(speeches_by_lp)

# Split each speech into sentences
sentences_list <- lapply(seq_len(nrow(all_speeches)), function(i) {
  row <- all_speeches[i]
  sents <- unlist(strsplit(row$text, "(?<=[.!?])\\s+", perl = TRUE))
  sents <- trimws(sents)
  sents <- sents[nchar(sents) > 0]
  if (length(sents) == 0) return(NULL)

  # Build context: previous and next sentence
  prev_sent <- c(NA_character_, sents[-length(sents)])
  next_sent <- c(sents[-1], NA_character_)

  data.table(
    speech_id          = row$speech_id,
    legislative_period = row$legislative_period,
    speaker            = row$speaker,
    party              = row$party,
    date               = row$date,
    sentence_nr        = seq_along(sents),
    sentence           = sents,
    context_before     = prev_sent,
    context_after      = next_sent
  )
})

sentences_dt <- rbindlist(sentences_list)
cat("Total sentences:", nrow(sentences_dt), "\n")

#### 7. Save sentence-level data ####
fwrite(sentences_dt, file.path(directory, "Data", "sentences_for_classification.csv"))
saveRDS(sentences_dt, file.path(directory, "Data", "sentences_dt.rds"))
cat("Saved to Data/sentences_for_classification.csv and Data/sentences_dt.rds\n")

# To reload later:
# sentences_dt <- readRDS(file.path(getwd(), "Data", "sentences_dt.rds"))