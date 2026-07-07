# CLAUDE.md — MSc Thesis: Corpus-Revision & Clean-Pipeline Sprint

> Drop this into the chat/project so you (Claude) have full context. Technical work in
> **German** (Flo's preference); English only for supervisor-facing text. Flo is direct,
> blunt, evidence-oriented — **verify every claim against the actual scripts/CSVs, never
> reason from first principles alone.** Plain prose, no structured widgets. Double-check
> every step; surface weak points rather than papering over them.

---

## Who / What

- **Author:** Florian Katzmann ("Flo"), MSc Computational Political Science, LMU München.
- **Thesis:** *Manifesto–Speech Agenda Consistency in the German Bundestag, LP 13–20.*
- **Supervisor:** Justyna (ifo Institute; Prof. Cantoni referenced). Correspondence in English.
- **Timeline:** submission end of July 2026; grading by end of August.
- **Design in one line:** compare each party's **manifesto** (MARPOR Main Dataset) against its
  **plenary speeches** (GermaParl2) classified by **ManifestoBERTa**, via **Jensen–Shannon
  Divergence (JSD)**, across LP 13–20, six parties (CDU/CSU, SPD, FDP, Grüne, Linke, AfD),
  41 valid (party, LP) cells. Soft aggregation, native temperature, **filtered corpus** is the
  primary spec. M1 mapping links each LP to its preceding-election manifesto.

---

## WHERE WE ARE (state as of this sprint)

The corpus has been rebuilt from scratch to fix two upstream problems found in gold-standard
batch 1, and the analysis pipeline is being **consolidated into one clean numbered chain**.

### DONE — Corpus revision (steps 1a–1c)

- **1a SoMaJo re-segmentation — DONE & validated.** Replaced the naive regex splitter
  (`strsplit(text, "(?<=[.!?])\\s+")`, which broke at "1. Januar", "z. B.", "Art. 5 Abs. 1",
  "F.D.P.") with **SoMaJo** (`de_CMC`, deterministic, no LLM). Script: `resegment_somajo.py`
  (reads `Data/all_speeches.csv` → writes `Data/sentences_somajo.csv`, same schema, multiprocessing).
  - 123,883 speeches → **5,751,659 sentences** (old regex: 5,977,336 → ~226k fragment rows removed).
  - Validation (`compare_segmentation.py`, regex reproduced on identical input for like-for-like):
    ordinal-break rate 1.118%→0.001%, abbreviation-end 0.898%→0.041%, month-start 0.794%→0.001%,
    lowercase-start 0.615%→0.000%. Mean sentence length 16.4→17.0 words. **Residuals are legitimate**
    (e.g. "50:50.", sentence-final "usw."), true fragment rate ≈ 0.
  - For the methods chapter: ~3.8% of the old corpus were segmentation artefacts.

- **1b Procedural-sentence filter — DONE & validated.** Strips content-free floor language
  (address / thanks-closing / staging / interjections / list-enumerators) that ManifestoBERTa
  over-codes as 305. Built **content-blind, form-first, pre-committed** (typology decided BEFORE
  looking at any JSD/305 effect).
  - Files: `procedural_rules.py` (versioned rule layer v1), `procedural_filter_decisions_v1.csv`
    (Flo's hand-typology of the Top-500 normalised forms — also the appendix doc),
    `flag_procedural.py` (applies the filter; **flag, don't drop** → adds `is_procedural`,
    `proc_class`, `proc_source`).
  - Candidate generation: `build_procedural_candidates.py` (Top-N normalised short-sentence forms,
    name-masking after address words, cumulative-coverage curve). Cumulative coverage justified
    **N = 500** empirically: Top-50 = 5.15%, Top-200 = 5.78%, Top-500 = 6.16%, Top-1000 = 6.46%
    of the corpus — marginal gain flattens hard after ~200.
  - Result: **331,184 sentences flagged = 5.758%** (anrede 206,634; dank_schluss 84,093;
    gliederung 26,377; interjektion 8,265; prozedural 5,815). Source: exact 323,021 / rule 8,163.
  - **Five removal categories** (Flo added `gliederung` to the original four, decided pre-effect):
    anrede / dank_schluss / prozedural / interjektion / gliederung = remove; everything else KEEP.
    In doubt → KEEP (target FP-rate ≈ 0; residual noise backstopped by the 305-only exclusion).
  - **Validation:** 0 rule-layer hits on Flo's 272 KEEP forms; in the unreviewed tail (rank
    501–1000) 75 hits, all clearly procedural; against gold batch 1 a **false-positive rate of
    1/165** substantive sentences (GS0092 — a thanks-formula that inherited a context code; likely
    a gold error → FP rate ≈ 0 once logged).
  - **Per-(party, LP) removal share — checked, no party confound.** The gradient is **temporal,
    not partisan**: ~4.3–5.5% in LP 13–17, ~6–6.5% in LP 18, ~7.3–9.5% in LP 19–20, parallel
    across all parties (mechanism: shorter speech slots in later LPs ⇒ more address/closing per
    sentence; LP19/20 add audience-greetings). PDS LP13–15 sits ~1.5–2 pts high (small-fraction,
    same mechanism). This is an **argument FOR the filter** (it removes time-correlated 305 noise
    that would have biased H4). Document the gradient transparently.

- **1c ManifestoBERTa re-run — DONE & verified complete.** Classified ALL 5,751,659 re-segmented
  sentences (incl. `is_procedural`). Script: `classify_manifestoberta_v2.py` (minimal patch of the
  **verified production script** — only paths, the three passthrough filter columns, and an input
  guard changed). Output: **576 parquet chunks** in `sentences_classified_v2/`, exactly 5,751,659
  rows (chunk count and row count verified on the laptop).
  - **Model facts corrected against the production script** (update the thesis text):
    model = `manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-**2024-1-1**`
    (NOT 2023-1-1); tokenizer = `xlm-roberta-large` (separate, confirmed); `max_length=300`;
    **dynamic padding** (NOT padding="max_length" — that was the model-card recommendation, not
    the actual run; numerically equivalent under attention masking); **fp16 AMP** with float32
    softmax (NOT fp32); context = sentence-pair `tokenizer(sentence, context)` with
    `context = context_before + sentence + context_after`. Only the model version is citation-relevant.
  - Laptop env: the **`.venv` at `C:/masterarbeit/.venv`** holds the cu128 torch (Blackwell/RTX 5070);
    the system Python 3.14 has a CPU-only torch. Always `.\.venv\Scripts\activate` first.

### 1d — clean pipeline rebuild

**Core DONE & verified (00/02/03 + tests + driver).** Analysis consolidated into one **numbered
chain**; the two hidden side-scripts (`apply_aggregation_AB.R`, `compute_jsd.R`) are retired (soft
aggregation existed in two places, the bootstrap in three). `00_config.R` (v2 path + axis constants),
`02_helpers.R` (+ `per_bucket_rel_asym`, `drop_305_from_buckets`) and `03_load_and_aggregate.R`
(ONE parquet stream → all caches) are written and gated by **6 logic tests + an end-to-end
synthetic-parquet test** (`run_all_v2.R`). The **full v2 run has completed** (~50 min: speech_soft
9,576 rows, speech_method 2,478, manifesto_dists 3,306; FDP LP18 = 298 filtered → excluded). Caches in
`results/empirics/cache`.

**Two bugs found & fixed this sprint (both verified by tests):**
1. **Bootstrap LP-match (MATERIAL).** `boot_band` matched each cell's manifesto by *party only*; since
   each party has 8 LP-manifestos under M1, duplicate bucket names collapsed to the FIRST election, so
   every LP-cell used the wrong (first-LP) manifesto. Fixed: match by `(party, election_date)` via
   `LP_ELECTION` (`test_03_boot_lpmatch.R`). **Proof in the rerun:** CDU/CSU LP13 stayed bit-identical
   (the per-party-first cell the bug happened to get right) while LP14–16 dropped ~0.12 → ~0.02–0.04
   (the corrected per-LP numbers). Affects **only `bootstrap_ci`** — speech_soft/method/manifesto/
   cell_diag were always correct. ⇒ H1a's CDU/CSU picture is now a real temporal pattern, not a flat ~0.12.
2. **Manifesto row-sum check (cosmetic).** The check grouped without `election_date`, summing all 8
   LPs (sum 8, dev 7) — false alarm, data was fine. Fixed by adding `election_date` to the check.

A precomputed cell-row index (`split` vs a per-cell string-scan over 5.75M rows) nearly halved the run
(5,693s → 3,021s).

**REMAINING — migrate the H-scripts 04–13** to the long-format caches (select the primary spec by
columns; open decisions below).

---

## THE THREE SPEECH-SIDE SPECIFICATION AXES (decided this sprint — full cross)

Every speech (party, LP) cell is computed under the **full cross** of three axes:

| axis      | values                          | meaning |
|-----------|---------------------------------|---------|
| `filter`  | `filtered`, `unfiltered`        | procedural-sentence filter on/off (`is_procedural==0` vs all) |
| `tau`     | `sharp`(0.5), `native`(1.0), `flat`(2.0) | softmax temperature (existing robustness) |
| `code305` | `incl`, `excl`                  | code-305-only exclusion on/off |

= 2 × 3 × 2 = **12 specs per cell**, per scheme A and B. **Manifesto side carries only `code305`**
(no procedural sentences, no softmax temperature) = 2 specs.

### Primary specifications (declared ONCE in `00_config.R`; every downstream script reads the constants — never hard-code a spec)

- `filter = "filtered"` — procedural floor language removed (manifesto comparability).
  `"unfiltered"` is the **built-in robustness arm**.
- `tau = "native"` (= `TAU_PRIMARY = 1.0`).
- `code305`: **hypothesis-dependent** —
  - **H1a, H2a → `"excl"`** (305 procedural artefact removed). This is the **surgical 305-only**
    exclusion, NOT the 8-bucket DPS drop (that stays as conservative robustness in `11_dps_*`).
  - **all other hypotheses (H1b, H2b, H2c, H3a, H3b, H4) → `"incl"`** (305 carries substantive
    political-authority signal there).

### code305 = "excl" — exact semantics (decided & proven this sprint)

Remove code 305's probability mass and **renormalise at the CELL level** over the remaining 55
codes. Implemented as: per sentence, subtract 305's mass from its bucket (DPS for A, Andere for B);
accumulate; divide by n; renormalise the cell to sum 1. **Proven identical** to "drop 305 from the
cell-mean distribution and renormalise", and the **manifesto side does the same operation** → JSD
stays apples-to-apples. The rejected alternative (per-sentence renormalise before averaging) would
amplify residual-305 noise from high-305 sentences. Order with tau: **temper first (on the 56-dim
softmax), then remove 305** (tempering a distribution the model never produced is wrong).

### Bootstrap CIs (Variante 2 — all in script 03)

Sentence-level bootstrap (1,000 draws) produces **four CI bands per cell**: `filter` × `code305`
at **native τ, scheme A only** (B stays exploratory, no CIs — consistent with prior setup).

- **Primary band (the one in the thesis): `filtered × excl × native`.**
- The other three (`filtered×incl`, `unfiltered×excl`, `unfiltered×incl`) are **appendix
  belt-and-braces**: they show the 305 exclusion does not inflate the CI (incl vs excl) and the
  filtering does not inflate it either (filtered vs unfiltered). **Discipline:** declare the one
  primary band up front; the other three exist only to prove the spec choice changes nothing.

---

## Pipeline (target — fully numbered, no side-scripts)

`run_all.R` sources, in order:

| script | role |
|--------|------|
| `00_config.R` | paths (→ **v2 parquet**), mappings, **the axis constants & primary-spec declarations** |
| `01_probe_data.R` | optional data probe (unchanged) |
| `02_helpers.R` | JSD, bootstrap, temper, soft/hard aggregation, per-bucket JSD, polarization, Dalton — all distance/aggregation primitives (base-R math) |
| `03_load_and_aggregate.R` | **ONE parquet stream** → all speech specs (soft 12-cross + hard + tight) + manifesto (code305) + **the 4 bootstrap CI bands** + cell diagnostics. Single source of truth for every number from the raw data. |
| `04_H1.R` … `07_H4.R` | read caches, compute hypotheses, plot |
| `08_robustness_temperature.R`, `09_robustness_threshold.R`, `10_robustness_prefilter.R` | robustness — **to be re-examined** against the new filter axis (see below) |
| `11_dps_code_decomposition.R`, `12_goldstandard_sample.R` | DPS/305 decomposition; gold sampling |

**RETIRED:** `apply_aggregation_AB.R`, `compute_jsd.R` (folded into 02/03).

### New cache contract (clean long-format, keyed by named axis columns)

Downstream selects by columns, e.g. `dt[filter==FILTER_PRIMARY & tau_name=="native" & code305=="incl" & scheme=="A"]`.

- `speech_soft.rds` — long: `party, lp, scheme, filter, tau_name, code305, bucket, share, n_sent`
  (the 24-cube: 12 specs × {A,B}).
- `speech_method.rds` — long: `party, lp, scheme, method, bucket, share, n_sent`
  (`method ∈ {hard_thr0.4, hard_thr0.5, tight}`, all on filtered/incl/native — for 09/10).
- `manifesto_dists.rds` — long: `party, election_date, mapping, scheme, code305, bucket, share`.
- `bootstrap_ci.rds` — `party, lp, filter, code305, jsd_point, lo95, hi95, n_sent` (native, scheme A).
- `cell_diag.rds` — `party, lp, n_sent_unfiltered, n_sent_filtered, mean_argmax_prob`.

### Open decisions before building 04–13 (Flo to confirm)
1. **H2a** — filtered/native/**incl** (DPS still mildly elevated → point to 10/11) **or**
   filtered/**excl** + a before/after panel (artefact already handled upstream by filter + 305-excl)?
2. **H3a** — `per_bucket_rel_asym` (|p−q|/m) as the **primary** measure (keep per_bucket_jsd as transparency)?
3. **10** — repurpose to **filtered-vs-unfiltered + incl-vs-excl** DPS decomposition (the length-filter
   proxy is subsumed by the real filter axis)?
4. **11** — create `11_dps_code_decomposition.R` fresh (the "79.6% of DPS from code 305" finding has no script yet)?
5. **Between-party benchmarks** — in `04` or a separate **`04b_between_party.R`** (proposed)?

Otherwise the migration is mechanical: H1a reads `bootstrap_ci[is_primary]` directly; H1b/H2b/H2c/H3b/H4
swap to `speech_soft`/`manifesto_dists` with the per-hypothesis `code305`; 08 reads the τ axis; 09 reads
`speech_method` (hard) vs `speech_soft` (soft baseline), all incl.

---

## PART B — analysis refinements (after 03 is verified; alongside gold/silver)

- **Between-party JSD benchmarks** (Justyna's core H1a feedback) — all unordered pairwise JSDs
  across parties per LP for manifesto-side and speech-side, to make absolute JSD interpretable.
  Highest selling value. (Easy from `speech_soft`/`manifesto_dists`.)
- **H1b directional sharpening** — Migration sensitivity excluding **601 & 602** (national-way-of-life
  pos/neg), keeping only the clean Multiculturalism pair 607(liberal)/608(restrictive). Addresses
  the 601-asymmetry. Run as sensitivity vs. baseline (601+608 / 602+607). Salience Migration bucket
  in A stays full; this is for the directional B measure only.
- **H3a mass-confound fix** — per-bucket JSD contribution scales with bucket mass; apply the
  mass-invariant correction **|p−q|/m** (`per_bucket_rel_asym()` in `02_helpers.R`); report
  corrected (valid) vs uncorrected (transparency).
- **Reframe directional (B) claims as exploratory** (κ≈0.27) throughout.
- **Update the supervisor memo** with clean-corpus κ. (Segmentation/filter note already delivered:
  `Corpus_Revision_Segmentation_Note.docx`.)

## Robustness scripts 08–10 — re-examine (Flo wants them adapted)

The **procedural filter now IS the principled corpus**, and `unfiltered` is the built-in robustness
arm — so the **old Tight-Pre-Filter (10)** may be partly subsumed. Decide per script what unique
function survives: `08` temperature (still valid, reads the τ axis from `speech_soft`), `09`
argmax+threshold (still valid, `speech_method`), `10` tight pre-filter (**reassess** — overlaps the
filter axis; previously showed −20.8%, below the pre-committed 25% threshold, so it was never
promoted to primary anyway). Do this AFTER 03 is locked.

---

## Remaining sprint steps (after 1d pipeline verified)

2. **Re-align the gold sample to v2** — `13_goldstandard_realign.R` (**written & logic-tested**; run
   after 03). Matches each coded batch-1 sentence to v2 **within its speech_id** (whitespace-insensitive;
   speech_id is invariant under re-segmentation, so the same text in another speech can't false-match),
   keeps the human code, attaches the **FRESH v2 prediction** (argmax → bucket_A/domain_B/bucket_B) +
   is_procedural + v2 context, and lists non-matches as **GAPS** to redraw/recode. Outputs
   `goldstandard_v2_carried.{csv,xlsx}`, `_gaps.csv`, `_carried_probs.rds`. Logic verified
   (`/tmp test`): within-speech ws-insensitive match, dup→`ambiguous`(first hit), wrong-speech &
   vanished→gap, argmax→bucket. **Batch 2: only ~35 sentences coded so far** — fold in later (add its
   sheet to `CODED_XLSX`, its key to `KEY_CSV`; both are vectors). Caveat to disclose: carried sentences
   were drawn under v1 strata / coded with v1 context (text identical, context only an aid).
3. **Re-validate gold vs ManifestoBERTa on the clean corpus** — **blind re-review** (re-read
   sentence+context, decide independently, THEN compare; log every change with a reason; do NOT
   align gold to the model) → clean-corpus κ (A, B, codeable-only, per-party, 305 precision).
4. **Silver standard** — blind Batch-API second rater, **Claude Sonnet `claude-sonnet-4-6`, temp 0**,
   AGG_A/AGG_B scheme, input = sentence+context only (no party, no BERTa pred). Three-way κ matrix
   (human↔BERTa, human↔Claude, BERTa↔Claude). Human stays the validity anchor; silver buys stability,
   not validity.

## PART C — extension (STRETCH; scope first, mind July)

- **Causal effect of AfD on polarization.** Federal = one treated unit, one event (LP19/2017) → no
  clean RDD/synthetic control. Real synthetic control needs a **Länder donor pool** (staggered AfD
  entry 2014–2018) with comparable speech data (large lift, may not exist GermaParl-comparable).
  **Honest fallback:** federal interrupted-time-series / event study around 2017 = largely what
  **H4 pre/post-2017** already does, framed descriptive. ⚠ OPEN (ask Flo): federal event-study vs.
  Länder synthetic control, and which polarization outcome.

---

## The aggregation scheme (verified against 00_config.R — use verbatim)

**Aggregation A** (complete 56-code partition, 9 buckets):
- Foreign Policy & Defence: 101,102,103,104,105,106,107,109
- European Integration: 108,110
- Democracy & Political System (**DPS**): 201,202,203,204,301,302,303,304,**305**
- Economy: 401–415
- Environment: 416,501
- Welfare & Social Policy: 502,503,504,505,506,507
- Law & Order and National Identity: 603,604,605,606
- Migration: 601,602,607,608
- Social Groups: 701,702,703,704,705,706

**Aggregation B** (directional; domain_B ∈ {Economy, Welfare, Migration, Europe}, + Andere residual):
- Marktliberalismus 401,402,407,414 · Staatsintervention 403,404,405,409,412,413 ·
  Wirtschaft Allgemein 408,410,411
- Sozialstaat Ausbau 503,504,506 · Sozialstaat Begrenzung 505,507
- Migration restriktiv 601,608 · Migration liberal 602,607
- Pro-EU 108 · Contra-EU 110
- Everything else (incl. **305**) → "Andere".

Coding-sheet cascade: pick `agg_A` (9 buckets + "Nicht zuordenbar (000)"), then dependent `agg_B`.
AGG-B sheet is BLIND (predictions live only in the key file).

---

## Pipeline, paths, data

- **PROJECT_ROOT** = `S:/RProj_MSc/MScThesis` (`00_config.R`, ~line 17).
- **v2 classified parquet:** `sentences_classified_v2/` (576 chunks). **Confirm the exact path on the
  PC and set `PATHS$parquet_dir` to it.** Columns: 56 prob cols `"NNN - Title"`, `pred_label`,
  `pred_score`, context_before/after, sentence, party, legislative_period, speech_id, sentence_nr,
  date, **is_procedural, proc_class, proc_source**.
- Data inputs: GermaParl2 parquet (v2 above), `manifesto_distributions.rds` (56 `"NNN - Title"` cols
  summing to 1, + legislative_period, party_label `CDU_CSU_joint`, mapping `M1_entering`),
  `speeches_by_lp.rds` (full speech text), MARPOR via `manifestoR`.
- **CDU/CSU pooling:** `normalize_party()` maps both CDU and CSU to "CDU/CSU"; sentence-level pooling
  = the old distribution-level weighted.mean pooling (proven identical). Manifesto `CDU_CSU_joint`
  → "CDU/CSU".
- **Exclusions (4):** parteilos/NA (no manifesto); PDS LP13–15 (no MARPOR PDS manifesto); FDP LP18
  (absent from Bundestag, the ~302 sentences are mis-attributions); cells with filtered n_sent < 1000.
- Hardware: RTX 5070 (Blackwell → CUDA 12.8+, torch cu128). Doc/sheet tooling: Node `docx`,
  `openpyxl`/`openxlsx`.

---

## Known artefacts / commitments (don't re-derive wrongly)

- **305 is single-code**: DPS-bucket JSD inflation traces ~79.6% to code 305 alone → surgical
  **305-only** exclusion (the `code305` axis), NOT the 8-bucket DPS drop.
- **601-asymmetry**: ManifestoBERTa predicts HB4 three-digit codes only; 601 conflates national
  sovereignty with immigration-restrictive content → document as an H1b limitation; motivates the
  607/608-only Migration sensitivity check.
- **H3a mass confounder**: per-bucket JSD contribution scales with bucket mass → needs |p−q|/m.
- **Europe bucket** is thin (108, 110 only) → disclose, don't drop post-hoc.
- **Pre-commitment architecture**: never promote a robustness spec to primary post-hoc.
- **Silver buys stability, not validity**: human gold standard is the validity anchor.
- **Directional B layer**: κ≈0.27 → exploratory. Salience A: κ≈0.41 (codeable-only 0.46) →
  defensible with relative/comparative framing + between-party benchmarks + bootstrap CIs.

---

## Validation state (gold batch 1, n = 184 — pre-revision; will be recomputed on v2)

- Aggregation A: raw 48.4%, **κ = 0.41**; codeable-only 53.9%, **κ = 0.46**.
- Aggregation B (direction, n = 86): 32.6%, **κ = 0.27** → exploratory.
- Per-party κ (A): AfD 0.20 (n=6) · Linke 0.25 · CDU/CSU 0.39 · SPD 0.44 · Grüne 0.46 · FDP 0.54.
- **305 artefact:** of 24 BERTa-305 sentences, human coded ~16% as DPS, 63% "not codeable" →
  externally validates the procedural artefact. In-chat silver preview: Claude assigned DPS to 1/24
  (vs human 3/24, BERTa 24/24) → 305 inflation is ManifestoBERTa-specific.

---

## Files from this/prior sessions

`resegment_somajo.py` · `compare_segmentation.py` · `build_procedural_candidates.py` ·
`procedural_rules.py` · `procedural_filter_decisions_v1.csv` · `flag_procedural.py` ·
`classify_manifestoberta_v2.py` · `Corpus_Revision_Segmentation_Note.docx` (supervisor memo) ·
`goldstandard_codingsheet_AB_2.xlsx` · `Aggregation_A_Kodier-Guide.md`.

**1d clean pipeline (this sprint):** `00_config.R` · `02_helpers.R` · `03_load_and_aggregate.R` ·
`run_all_v2.R` · tests `test_03_{math,dt,stream,manifesto_hard,bootstrap,boot_lpmatch,realfile}.R` ·
`13_goldstandard_realign.R`.
