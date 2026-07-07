# ============================================================================
# Quick diagnostic — signed divergence per (party, bucket) on Aggregation A
# Paste into the R console after run_all.R has populated the cache.
# ============================================================================

source(here::here("R/00_config.R"))
source(here::here("R/02_helpers.R"))

sd_list <- readRDS(file.path(PATHS$cache_dir, "speech_dists.rds"))
md_list <- readRDS(file.path(PATHS$cache_dir, "manifesto_dists.rds"))

m1_map <- LP_ELECTION %>% filter(lp %in% LEGISLATIVE_PERIODS)

# Join speech and manifesto shares on (party, lp, bucket)
signed <- sd_list$A %>%
  rename(share_speech = share) %>%
  inner_join(
    md_list$A %>%
      inner_join(m1_map, by = "election_date") %>%
      select(party, lp, bucket, share_manifesto = share),
    by = c("party", "lp", "bucket")
  ) %>%
  filter(n_sent >= 5000) %>%   # exclude FDP LP 18 etc.
  mutate(diff = share_speech - share_manifesto)
# diff > 0  => party talks about this MORE in plenary than its manifesto allocates
# diff < 0  => party talks about this LESS in plenary than its manifesto allocates

# ---- 1. Per-party, per-bucket means ---------------------------------------
mean_signed <- signed %>%
  group_by(party, bucket) %>%
  summarise(mean_speech     = mean(share_speech),
            mean_manifesto  = mean(share_manifesto),
            mean_diff       = mean(diff),
            n_lps           = n(),
            .groups = "drop") %>%
  mutate(across(c(mean_speech, mean_manifesto, mean_diff), \(x) round(x, 4)))

# Print for any party/bucket pair you care about, e.g.:
mean_signed %>% filter(party == "CDU/CSU", bucket == "Environment")
mean_signed %>% filter(party == "Grüne",   bucket == "Environment")
mean_signed %>% filter(bucket == "Environment") %>% arrange(mean_diff)

# Or all of CDU/CSU at once, sorted:
mean_signed %>% filter(party == "CDU/CSU") %>% arrange(mean_diff)

# ---- 2. Per-LP look at a single (party, bucket), to see drift over time ---
signed %>%
  filter(party == "CDU/CSU", bucket == "Environment") %>%
  arrange(lp) %>%
  select(party, lp, share_manifesto, share_speech, diff)

# ---- 3. Save the full signed matrix for later use -------------------------
write_csv(mean_signed, file.path(PATHS$out_dir, "signed_divergence_A.csv"))
