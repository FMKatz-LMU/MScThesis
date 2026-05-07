# MScThesis — Project Context for Claude

## Project overview
MSc thesis by Florian Katzmann. The goal is to classify German parliamentary speeches
(Bundestag, legislative periods 13–20) into Manifesto Project policy domains using
ManifestoBERTa, then link the results to IMF conditionality data.

---

## Repository layout

```
MScThesis/
├── Data/
│   ├── IMFMonitor_Conditionality_Main.dta   # IMF conditionality panel (Stata format)
│   ├── speeches_by_lp.rds                   # R list: one data.table per legislative period
│   ├── sentences_dt.rds                     # R data.table of all sentences (all periods)
│   ├── sentences_for_classification.csv     # Input to ManifestoBERTa (≈6M rows)
│   └── sentences_classified.csv             # Output from ManifestoBERTa (created by Python script)
├── Scripts/
│   ├── Data.R                               # Loads IMF Stata file
│   ├── classify_manifestoberta.py           # ManifestoBERTa classification script
│   └── Script_Speechrefinement.R            # Main R pipeline (see below)
├── cwb/registry/                            # CWB registry for polmineR / GERMAPARL2
└── CLAUDE.md                                # This file
```

---

## R pipeline — Script_Speechrefinement.R

### What it does (in order)
1. **Corpus extraction** — uses `polmineR` + the local CWB corpus `GERMAPARL2` to pull
   speeches for legislative periods 13–20. Filters to `p_type = "speech"` and
   `speaker_role` ∈ {mp, government}.
2. **Per-speech cleaning** — removes parenthetical stage directions (`(Beifall)`,
   `(Zwischenruf)`, etc.) with a regex, then drops speeches with fewer than 100 words.
3. **Sentence splitting** — splits each speech on sentence-ending punctuation
   (`(?<=[.!?])\s+`). Adds `context_before` (previous sentence) and `context_after`
   (next sentence) as separate columns. First/last sentences get `NA` for the missing
   context field.
4. **Saves** two artefacts:
   - `Data/sentences_for_classification.csv` — flat CSV for Python
   - `Data/sentences_dt.rds` — same data as an R `data.table` for downstream R work

### Output schema — sentences_for_classification.csv
| Column             | Type    | Notes                                         |
|--------------------|---------|-----------------------------------------------|
| speech_id          | string  | `"<speaker_name>__<n>"` (unique per speech)   |
| legislative_period | integer | 13–20                                         |
| speaker            | string  | Speaker's full name                           |
| party              | string  | Party abbreviation (CDU, SPD, …)              |
| date               | string  | ISO date of the plenary session               |
| sentence_nr        | integer | 1-based position within the speech            |
| sentence           | string  | The sentence to classify                      |
| context_before     | string  | Previous sentence; empty/NA for first sentence|
| context_after      | string  | Next sentence; empty/NA for last sentence     |

---

## Python classification — Scripts/classify_manifestoberta.py

### Model
- **HuggingFace ID**: `manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2023-1-1`
- Runs on local GPU (CUDA). Falls back to CPU automatically if no GPU is available.
- 56-class sequence classifier (Manifesto Project policy domain codes).

### Input format
ManifestoBERTa's context variant expects the three fields joined with ` </s></s> `:
```
[context_before] </s></s> [sentence] </s></s> [context_after]
```
Missing context (NA / empty string) fields are omitted from the join — the sentence
alone is passed for first/last sentences in a speech.

### Output schema — sentences_classified.csv
Same columns as `sentences_for_classification.csv`, plus:

| Column | Type   | Notes                                              |
|--------|--------|----------------------------------------------------|
| label  | string | Manifesto policy code (e.g. `"per401"`)            |
| score  | float  | Softmax confidence for the predicted label (0–1)   |

### Runtime settings (top of script)
| Variable    | Default | Meaning                                             |
|-------------|---------|-----------------------------------------------------|
| BATCH_SIZE  | 32      | Sentences per GPU forward pass — raise if GPU allows|
| CHUNK_SIZE  | 10_000  | Rows read from CSV at once                          |

### Resume behaviour
If `sentences_classified.csv` already exists, the script counts its rows and skips
that many input rows, appending only new results. Safe to interrupt and restart.

### How to run
```bash
cd "S:/RProj_MSc/MScThesis"
python Scripts/classify_manifestoberta.py
```

---

## IMF data — Data/IMFMonitor_Conditionality_Main.dta
Loaded in `Scripts/Data.R` with `haven::read_dta()`. Contains IMF programme
conditionality panel data. The eventual goal is to merge classified speech data
with this dataset (likely on country + year or legislative period).

---

## Key dependencies
- **R**: `polmineR`, `data.table`, `RcppCWB`, `haven`
- **Python**: `transformers`, `torch`, `pandas`
- **CWB corpus**: GERMAPARL2 (registry at `cwb/registry/`)
