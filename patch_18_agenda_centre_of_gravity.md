# Patch für `18_agenda_centre_of_gravity.R` (E2, Variante A)

**Warum:** Das Skript rechnet die Centre-of-Gravity-Mittel über **42** Redezellen, der Fließtext in 5.2.1, Tabelle F.4.2 und die Conclusion über **38**. Deshalb widersprachen sich Text und Abbildung 5.3, und die Rangfolge von Linke und CDU/CSU kippte (K5-A3). Nach dem Patch laufen beide auf den 38 validen Analysezellen, die Nenner sind für alle etablierten Parteien gleich, und beide Bildunterschriften („from each of the 38 speech cells") werden wahr.

Die Kandidatenmenge (`present`) bleibt bewusst **unverändert**: Kandidat ist jede Partei, die in der Periode im Parlament sitzt — genau das, was die Unterschrift behauptet.

---

## Änderung 1 — Redezellen auf die Analysebasis einschränken

Nach der Zeile

```r
sp_v <- to_vec(speech) %>% rename(sp_party = party, sp_vec = vec)
mf_v <- to_vec(manif)  %>% rename(mf_party = party, mf_vec = vec)
```

einfügen:

```r
# --- restrict the speech side to the 38 valid analysis cells (Section 4.8) ---
# Without this the centre of gravity runs on 42 cells (incl. FDP LP18 and the
# PDS cells LP13-15) while the text, Table F.4.2 and the Conclusion run on 38.
valid_cells <- readRDS(file.path(PATHS$cache_dir, "bootstrap_ci.rds")) %>%
  as_tibble() %>%
  filter(is_primary) %>%
  distinct(party, lp) %>%
  mutate(party = normalize_party(party))
stopifnot(nrow(valid_cells) == 38L)

sp_v <- sp_v %>% semi_join(valid_cells, by = c("sp_party" = "party", "lp" = "lp"))
stopifnot(nrow(sp_v) == 38L)
```

## Änderung 2 — Panel B auf dieselbe Basis

```r
# alt
speech_prof <- speech %>% group_by(bucket) %>%
  summarise(share = mean(share), .groups = "drop")

# neu
speech_prof <- speech %>%
  semi_join(valid_cells, by = c("party", "lp")) %>%
  group_by(bucket) %>%
  summarise(share = mean(share), .groups = "drop")
```

## Änderung 3 — Untertitel nicht abschneiden (K5-B21)

Im Aufruf für `pB`:

```r
subtitle = str_wrap("Bars = parliamentary speech (all 38 cells); points = party manifesto agendas", 100)
```

und in `plot_annotation()` die `caption` ebenfalls durch `str_wrap(..., 150)` schicken, sonst bleibt „points = party manifesto age" abgeschnitten.

---

## Erwartetes Ergebnis

```
Centre of gravity (mean JSD from all speeches to each manifesto):
 mf_party mean_jsd  n
      FDP   0.0257 38
      SPD   0.0379 38
    Grüne   0.0396 38
  CDU/CSU   0.0463 38
    Linke   0.0524 38
      AfD   0.0638 12
```

Diese sechs Werte stehen so im überarbeiteten Fließtext von 5.2.1, in Tabelle F.4.2 und in der Conclusion (0.0257). Sie wurden unabhängig nachgerechnet: die JSD-Matrix wurde aus `speech_soft.rds` (Schema A, `filtered_000`, `native`, `excl`) und `manifesto_dists.rds` (`M1_entering`, Schema A, `excl`) zur Basis 2 neu aufgebaut, einmal auf 42 und einmal auf 38 Redezellen. Die 42-Variante reproduziert die heutige Abbildung exakt (0.0335 / 0.0482 / 0.0487 / 0.0584 / 0.0604 / 0.0638), die 38-Variante die obigen Werte.

Zur Kontrolle die zweite Spalte von Tabelle F.4.2 (mean JSD ohne die eigene Rede): FDP 0.0267 · SPD 0.0400 · Grüne 0.0422 · CDU/CSU 0.0484 · Linke 0.0564 · AfD 0.0705.

**Nicht mit ändern:** die Spalte *Times nearest* in Tabelle F.4.2 (18 / 6 / 7 / 5 / 0 / 2). Sie stammt aus `benchmark_nearest_manifesto.csv` und damit aus `14_H1.R`, das die Kandidaten auf Parteien mit gültiger Zelle einschränkt. Das ist eine andere Größe und bleibt korrekt.

Nach dem Lauf: `results/empirics/figures/agenda_centre_of_gravity.png` neu in Abbildung 5.3 einsetzen.
