# Empirics — H1 and H2

Cross-domain agenda consistency analysis between MARPOR manifestos and GermaParl speeches.

## File layout

```
empirics/
├── run_all.R                      ← driver
└── R/
    ├── 00_config.R                ← PATHS, lookup tables (AGG_A, AGG_B), party colours
    ├── 01_probe_data.R            ← prints structure of speech + manifesto RDS
    ├── 02_helpers.R               ← jsd(), soft_aggregate(), bootstrap_jsd(), normalize_party()
    ├── 03_load_and_aggregate.R    ← builds cell-level distributions, caches RDS
    ├── 04_H1.R                    ← H1a (JSD + bootstrap) + H1b (directional bars)
    └── 05_H2.R                    ← H2a (per-bucket) + H2b (moderation) + H2c (dispersion)
```

## Workflow

1. **Edit `PATHS` in `R/00_config.R`** — point them at your speech and manifesto RDS files.
2. **Run `source("R/01_probe_data.R")`** and read `results/empirics/data_probe.txt`.
   The probe diagnoses your manifesto-side format. There are four likely cases:
   - `RAW_WIDE` — MARPOR Main Dataset with `per101..per706` columns. **The script supports this out of the box.**
   - `WIDE_AGGREGATED` — already aggregated to bucket columns. Edit `load_manifesto_wide()` in `03_load_and_aggregate.R` to skip the per-code aggregation.
   - `LONG_AGGREGATED` — already in long form. Same — adapt the loader.
   - `QUASI_SENTENCE_LEVEL` — one row per quasi-sentence with `cmp_code`. You'll need to count codes per (party, election) and divide; let me know if this is the case and I'll add the helper.
3. **Run `source("run_all.R")`**.

## Outputs

In `results/empirics/`:

| File                              | Hypothesis | What it is                                                                              |
| --------------------------------- | ---------- | --------------------------------------------------------------------------------------- |
| `H1a_jsd_table.csv`               | H1a        | Per (party, LP) JSD with bootstrap 95% CI                                               |
| `figures/H1a_jsd_dotplot.pdf`     | H1a        | The H1a figure — six panels, one per party, JSD vs. LP with intervals                   |
| `H1b_directional_table.csv`       | H1b        | Long: party × LP × domain × sub_bucket × source × within-domain share                   |
| `H1b_overall_jsd.csv`             | H1b        | Per (party, LP) JSD on Aggregation B excluding Andere (panel-header numbers)            |
| `figures/H1b_directional_bars.pdf`| H1b        | 6 × 4 panel of paired stacked bars, manifesto vs. speech                                |
| `H2a_per_bucket_summary.csv`      | H2a        | Mean JSD contribution per Aggregation A bucket, with reactive/programmatic flag         |
| `figures/H2a_per_bucket_bar.pdf`  | H2a        | Sorted horizontal bar chart with cell-level dot overlay                                 |
| `H2b_polarization_table.csv`      | H2b        | Per (party, LP, domain): manifesto vs. speech polarization scores                       |
| `H2b_regression_fits.csv`         | H2b        | Per-domain OLS slope, intercept, R² (slope < 1 = systematic moderation)                 |
| `figures/H2b_polarization_scatter.pdf` | H2b   | 2 × 2 scatter, 45° + OLS, party labels                                                  |
| `H2c_dispersion_table.csv`        | H2c        | Per (LP, domain) SD across parties — manifesto, manifesto-excl-AfD, speech, speech-excl-AfD |
| `figures/H2c_dispersion_lines.pdf`| H2c        | Four-line panel per domain                                                              |

Cached intermediates in `results/empirics/cache/`:

- `speech_dists.rds`, `manifesto_dists.rds` — long tibbles of cell distributions (A and B)
- `cell_diag.rds` — per-cell mean argmax probability and predictive entropy (for §3.3 of the methodology — descriptive only, not entering the aggregation)
- `speech_prob_matrix.rds` — the n × 56 matrix plus row index, kept around so the H1a bootstrap can resample without re-loading the original RDS

## Performance notes

- **Bootstrap cost.** H1a does 1000 bootstrap draws × 48 cells (6 parties × 8 LPs) = ~48,000 soft-aggregations. Each draw resamples sentence indices and recomputes a (n × 56) %*% (56 × 9) product. With your largest cell at ~280k sentences this is the bottleneck. Expect 5–20 minutes total depending on RAM and matrix size; if that's too slow, drop `BOOTSTRAP_DRAWS` to 500 in `00_config.R` (intervals tighten by sqrt(2), still defensible) or subsample large cells to a fixed cap before bootstrapping.
- **Memory.** Loading 8 LPs of sentence-level predictions as RDS into a single matrix can easily hit 5–10 GB. If you run out of RAM, switch to chunked Parquet on the speech side and rewrite `03_load_and_aggregate.R` to stream cells in turn — happy to help with that if/when it bites.

## Methodology cross-references

- **Soft aggregation, no confidence threshold:** Methodische Strategie §3.2.
- **Aggregation A — 9 buckets, exact partition of 56 codes:** Methodological Implementation §5.1, with 601/602/607/608 → Migration justified by the Handbook 4 / Handbook 5 limitation.
- **Aggregation B — directional within four domains, Andere = filter:** §5.2.
- **M1 mapping (preceding-election manifesto):** H1a graph spec.
- **Bootstrap: 1000 draws, sentence-level resampling, fixed manifesto:** H1a graph spec.
- **Polarization score (signed within-domain balance):** H2b graph spec.

## Robustness still to be added (later phases)

- Temperature scaling sweep τ ∈ {0.5, 1.0, 2.0} — apply `temper()` from `02_helpers.R` to the speech matrix in `03_load_and_aggregate.R` and re-run the H1/H2 scripts. Three runs, three sets of figures.
- M2 (time-weighted manifesto blend) — alternative to M1.
- Silver-standard validation outputs feed into a separate `06_validation.R`.
