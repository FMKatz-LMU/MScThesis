# ============================================================================
# Gold-Standard — zwei einfache Übersichtsgrafiken
#   (1) n je Aggregation-A-Bucket, in Key-Domänen nach Richtung (agg_B) aufgeschlüsselt
#   (2) n je Partei
# Liest aus  <PROJECT_ROOT>/results/empirics/goldstandard
# ============================================================================

library(dplyr)
library(ggplot2)

# --- Pfade -------------------------------------------------------------------
if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- "S:/RProj_MSc/MScThesis"
GS_DIR <- file.path(PROJECT_ROOT, "results", "empirics", "goldstandard")

CODING_XLSX <- file.path(GS_DIR, "goldstandard_codingsheet_FINAL_merged.xlsx")
KEY_CSV     <- file.path(GS_DIR, "goldstandard_key_slim_FINAL.csv")

LABEL_SOURCE <- "model"   # "model" = BERT-Labels (bucket_A/bucket_B) -> prüft die ZIEHUNG; "human" = deine Codes (agg_A/agg_B)

# --- Einlesen ----------------------------------------------------------------
coding <- readxl::read_excel(CODING_XLSX, sheet = "Coding") %>%
  mutate(across(c(agg_A, agg_B), ~ trimws(as.character(.))))
key <- readr::read_csv(KEY_CSV, show_col_types = FALSE) %>%
  mutate(gs_id = as.character(gs_id))

df <- coding %>%
  select(gs_id, agg_A, agg_B) %>%
  left_join(select(key, gs_id, party, bucket_A, bucket_B), by = "gs_id")

# --- Ziehungs-Check: realisierte n je Stratum (BERT-Stratum, Soll: Floor 10) --
cat("\n== n per drawing stratum (key column 'stratum') ==\n")
strat_n <- key %>% count(stratum, name = "n") %>% arrange(n)
print(strat_n, n = Inf)
cat(sprintf("MIN per stratum = %d   |   strata < 10: %d   |   #strata: %d\n",
            min(strat_n$n), sum(strat_n$n < 10), nrow(strat_n)))

# --- A-Bucket + Richtung je nach Quelle --------------------------------------
if (LABEL_SOURCE == "human") {
  df <- df %>% mutate(A = agg_A, dir = agg_B)
} else {
  df <- df %>% mutate(A = bucket_A, dir = bucket_B)
}
# leere / "Andere" Richtungen (Nicht-Key-Domänen) zu einem neutralen Segment
df <- df %>% mutate(dir = ifelse(is.na(dir) | dir %in% c("", "Andere"),
                                 "(no direction)", dir))

# --- englische Richtungs-Labels (bei Bedarf an Thesis-Wording anpassen) -------
dir_en <- c("Marktliberalismus"      = "Market liberalism",
            "Staatsintervention"     = "State intervention",
            "Wirtschaft Allgemein"   = "General economy",
            "Sozialstaat Ausbau"     = "Welfare expansion",
            "Sozialstaat Begrenzung" = "Welfare limitation",
            "Migration liberal"      = "Migration: liberal",
            "Migration restriktiv"   = "Migration: restrictive",
            "Pro-EU"                 = "Pro-EU",
            "Contra-EU"              = "Anti-EU",
            "keine klare Richtung"   = "no clear direction")
df$dir <- ifelse(df$dir %in% names(dir_en), dir_en[df$dir], df$dir)

# --- Reihenfolgen ------------------------------------------------------------
dir_levels <- c("Market liberalism","State intervention","General economy",
                "Welfare expansion","Welfare limitation",
                "Migration: liberal","Migration: restrictive",
                "Pro-EU","Anti-EU","no clear direction","(no direction)")
df$dir <- factor(df$dir, levels = dir_levels[dir_levels %in% unique(df$dir)])

A_order <- df %>% count(A) %>% arrange(n) %>% pull(A)   # aufsteigend -> coord_flip = größte oben
df$A <- factor(df$A, levels = A_order)
tot   <- df %>% count(A)

# === Grafik 1: n je A-Bucket, nach Richtung aufgeschlüsselt ==================
g1 <- ggplot(df, aes(x = A, fill = dir)) +
  geom_bar(width = 0.8) +
  geom_text(data = tot, aes(x = A, y = n, label = n),
            inherit.aes = FALSE, hjust = -0.3, size = 3.1, colour = "grey25") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3", na.translate = FALSE) +
  expand_limits(y = max(tot$n) * 1.08) +
  labs(title = "Gold standard: n per Aggregation-A bucket (broken down by direction)",
       x = NULL, y = "Number of sentences", fill = "Direction (agg_B)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

# === Grafik 2: n je Partei ===================================================
pc <- df %>% count(party) %>% arrange(n)
pc$party <- factor(pc$party, levels = pc$party)

g2 <- ggplot(pc, aes(x = party, y = n)) +
  geom_col(width = 0.7, fill = "#4C72B0") +
  geom_text(aes(label = n), hjust = -0.35, size = 3.5, colour = "grey25") +
  coord_flip() +
  expand_limits(y = max(pc$n) * 1.08) +
  labs(title = "Gold standard: n per party", x = NULL, y = "Number of sentences") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

# --- Anzeigen + speichern ----------------------------------------------------
print(g1); print(g2)
ggsave(file.path(GS_DIR, "gold_n_by_aggA_direction.png"), g1, width = 9, height = 5, dpi = 150)
ggsave(file.path(GS_DIR, "gold_n_by_party.png"),          g2, width = 7, height = 4, dpi = 150)
message("Saved to: ", GS_DIR)
