Empirics — H1, H2, H6
Cross-domain agenda consistency analysis between MARPOR manifestos and GermaParl speeches.
File layout
empirics/
├── run_all.R                      ← driver
└── R/
    ├── 00_config.R                ← PATHS, lookup tables (AGG_A, AGG_B), party colours, MIN_N_SENT
    ├── 01_probe_data.R            ← prints structure of speech + manifesto RDS
    ├── 02_helpers.R               ← jsd(), soft_aggregate(), bootstrap_jsd(), normalize_party()
    ├── 03_load_and_aggregate.R    ← builds cell-level distributions, caches RDS
    ├── 04_H1.R                    ← H1a (JSD + bootstrap) + H1b (directional bars)
    ├── 05_H2.R                    ← H2a (per-bucket) + H2b (moderation) + H2c (dispersion)
    └── 06_H6.R                    ← H6 (system polarization over time, both channels)
    
Workflow

Edit PATHS in R/00_config.R — point them at your speech and manifesto RDS files.
Run source("R/01_probe_data.R") and read results/empirics/data_probe.txt.
The probe diagnoses your manifesto-side format. There are four likely cases:

RAW_WIDE — MARPOR Main Dataset with per101..per706 columns. Supported out of the box.
WIDE_AGGREGATED — already aggregated to bucket columns. Edit load_manifesto_wide() in 03_load_and_aggregate.R.
LONG_AGGREGATED — already in long form. Same — adapt the loader.
QUASI_SENTENCE_LEVEL — one row per quasi-sentence with cmp_code. Needs an extra helper.


Verify the lookup tables in 06_H6.R before first run — VOTE_SHARES and SEAT_SHARES are hardcoded for the 1994–2021 federal elections and LPs 13–20. Errors propagate into every H6 number, so eyeball them once.
Run source("run_all.R").

Outputs
In results/empirics/:
FileHypothesisWhat it isH1a_jsd_table.csvH1aPer (party, LP) JSD with bootstrap 95% CIfigures/H1a_jsd_dotplot.pdfH1aSix-panel figure, JSD vs. LP with intervalsH1b_directional_table.csvH1bLong: party × LP × domain × sub_bucket × source × within-domain shareH1b_overall_jsd.csvH1bPer (party, LP) JSD on Aggregation B excluding Anderefigures/H1b_directional_bars.pdfH1b6 × 4 panel of paired stacked bars, manifesto vs. speechexcluded_cells.csvH1/H2/H6Cells dropped because n_sent < MIN_N_SENT (default 5000)H2a_per_bucket_summary.csvH2aMean JSD contribution per Aggregation A bucket, with reactive/programmatic flagfigures/H2a_per_bucket_bar.pdfH2aSorted horizontal bar chart with cell-level dot overlayH2b_polarization_table.csvH2bPer (party, LP, domain): manifesto vs. speech polarization scoresH2b_regression_fits.csvH2bPer-domain OLS slope, intercept, R² (slope < 1 = systematic moderation)figures/H2b_polarization_scatter.pdfH2b2 × 2 scatter, 45° + OLS, party labelsH2c_dispersion_table.csvH2cPer (LP, domain) SD across parties — manifesto, manifesto-excl-AfD, speech, speech-excl-AfDfigures/H2c_dispersion_lines.pdfH2cFour-line panel per domainH6_system_polarization.csvH6Per (LP, domain, channel, variant): Dalton-weighted polarization + weighted mean positionH6_mean_positions.csvH6Per (LP, domain, channel): weighted mean position only (compact)H6_trend_fits.csvH6Per (domain, channel) OLS trend polarization ~ lp with slope, SE, p, 95% CI, R²H6_pre_post_2017.csvH6Mean polarization in LPs 13–18 vs LPs 19–20, plus deltafigures/H6_polarization_lines.pdfH6Main figure — 4 domains × 2 lines (Manifesto / Speech), with AfD-entry markerfigures/H6_polarization_lines_robust.pdfH6Same with AfD-excluded overlayfigures/H6_mean_positions.pdfH6Centre-of-mass figure — distinguishes "parties moved together" from "parties moved apart"
Cached intermediates in results/empirics/cache/:

speech_dists.rds, manifesto_dists.rds — long tibbles of cell distributions (A and B)
cell_diag.rds — per-cell mean argmax probability and predictive entropy (for §3.3 of the methodology — descriptive only)
speech_prob_matrix.rds — the n × 56 matrix plus row index, kept around so the H1a bootstrap can resample without re-loading the original RDS

Performance notes

Bootstrap cost. H1a does 1000 bootstrap draws × ~42 cells. Each draw resamples sentence indices and recomputes a (n × 56) %*% (56 × 9) product. With the largest cell at ~280k sentences this is the bottleneck. Expect 5–20 minutes total depending on RAM and matrix size; if too slow, drop BOOTSTRAP_DRAWS to 500 in 00_config.R (intervals tighten by sqrt(2), still defensible) or subsample large cells to a fixed cap before bootstrapping.
Memory. Loading 8 LPs of sentence-level predictions as RDS into a single matrix can easily hit 5–10 GB. If you run out of RAM, switch to chunked Parquet on the speech side and rewrite 03_load_and_aggregate.R to stream cells in turn.
H6 is cheap. It reuses the cached distributions — no re-aggregation, runs in seconds.

Methodology cross-references

Soft aggregation, no confidence threshold: Methodische Strategie §3.2.
Aggregation A — 9 buckets, exact partition of 56 codes: Methodological Implementation §5.1, with 601/602/607/608 → Migration justified by the Handbook 4 / Handbook 5 limitation.
Aggregation B — directional within four domains, Andere = filter: §5.2.
M1 mapping (preceding-election manifesto): H1a graph spec.
Bootstrap: 1000 draws, sentence-level resampling, fixed manifesto: H1a graph spec.
Polarization score (signed within-domain balance): H2b graph spec.
Dalton-weighted system polarization: H6. Manifesto channel weighted by vote share at the corresponding election; speech channel weighted by seat share in the LP. Channel-appropriate weights rather than identical weighting — choice documented in 06_H6.R header.

Sample restrictions

MIN_N_SENT = 5000 in 00_config.R. Cells below this threshold are excluded from H1/H2/H6 figures and summary statistics, and listed in excluded_cells.csv. Currently affects FDP LP 18 (320 sentences, FDP not in Bundestag 2013–2017).
AfD has manifesto data from 2013 onward but only enters the speech analysis from LP 19. Pre-2017 H6 manifesto polarization includes AfD's 2013 and 2017 manifestos; pre-2017 speech polarization cannot. Documented as a known compositional asymmetry.

Robustness still to be added (later phases)

Temperature scaling sweep τ ∈ {0.5, 1.0, 2.0} — apply temper() from 02_helpers.R to the speech matrix in 03_load_and_aggregate.R and re-run the H1/H2/H6 scripts. Three runs, three sets of figures.
M2 (time-weighted manifesto blend) — alternative to M1.
Silver-standard validation outputs in a separate 07_validation.R.
Seat-weighted manifesto polarization as a robustness check on H6 (currently vote-weighted).
