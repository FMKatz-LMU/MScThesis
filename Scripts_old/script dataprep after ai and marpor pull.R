if (!requireNamespace("manifestoR", quietly = TRUE)) install.packages("manifestoR")
packageVersion("manifestoR")  # should be 1.5.0 or newer for mp_corpus_df
install.packages("openxlsx")

#Reload R Session
directory <- getwd()
setwd("S:/RProjMSc/MScThesis")
source("reload_session.R")


source("diagnose_marpor.R")
source("diagnose_marpor_v3.R")


source("pull_marpor_align.R")        # rebuild manifesto_distributions.rds
source("apply_aggregation_AB.R")     # rebuild agg_A/B_manifesto.rds (no edits needed)
source("compute_jsd.R")              # recompute jsd_results.rds and jsd_permutation.rds



source("run_all.R")
source("quick_signed.R")
source("R/11_dps_code_decomposition.R")

source("R/12_goldstandard_sample.R")


library(data.table)
agg_A_speech <- readRDS("Data/agg_A_speech.rds")
agg_A_manifesto <- readRDS("Data/agg_A_manifesto.rds")

# Speech-side cells per party (across LPs)
speech_cells <- unique(agg_A_speech[tau_name == "native", .(legislative_period, party)])
print(speech_cells[, .N, by = party][order(-N)])

# Manifesto-side cells per party (under M1)
manifesto_cells <- unique(agg_A_manifesto[mapping == "M1_entering", 
                                          .(legislative_period, party_label)])
print(manifesto_cells[, .N, by = party_label][order(-N)])

# Where's the disconnect? Show SPD specifically
cat("\nSPD on speech side (LPs):\n")
print(speech_cells[party == "SPD"])
cat("\nSPD on manifesto side (under M1):\n")
print(manifesto_cells[party_label == "SPD"])













