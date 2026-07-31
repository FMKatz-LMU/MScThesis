#!/usr/bin/env python3
# =============================================================================
#  gold_000_check.py — Post-hoc-Konsistenzcheck: gBERT-000-Filter am Goldstandard
# -----------------------------------------------------------------------------
#  ZWECK (3.6.1, Zirkularitaets-Absatz): Der Filter ist gegen Sonnet-Labels
#  kreuzvalidiert, aber nie direkt gegen den menschlichen Kodierer geprueft.
#  Dieses Skript scored die 249 carried Gold-Saetze mit dem produktiven Modell
#  (model_000_gbert/) und kreuzt is_000 (Schwelle 0,5) gegen das Human-Label.
#
#  STATUS: post-hoc consistency check, NICHT Teil des pre-committed Designs.
#  Das Ergebnis wird berichtet, wie es ausfaellt.
#
#  Eingabe-Konstruktion identisch zu 10/11: Satz = Segment A,
#  context_before + " " + context_after = Segment B, max_len 256.
#
#  AUFRUF (im .venv, im Ordner mit model_000_gbert/ und der Gold-CSV):
#    python gold_000_check.py
#  OUTPUT: gold_000_check.csv (satzweise) + gold_000_check_summary.csv + Konsole
# =============================================================================

import os, sys, json
import numpy as np
import pandas as pd

# ----------------------------------------------------------------------------
# KONFIGURATION
# ----------------------------------------------------------------------------
MODEL_DIR  = r"C:\RProj_MSc\MScThesis\results\empirics\goldstandard\model_000_gbert"
GOLD_CSV   = r"C:\RProj_MSc\MScThesis\results\empirics\goldstandard\goldstandard_v2_carried.csv"
THRESHOLD  = 0.5                              # = FILTER000_THRESHOLD (00_config.R)
OUT_ROWS   = "gold_000_check.csv"
OUT_SUM    = "gold_000_check_summary.csv"
LABEL_000  = "Nicht zuordenbar"               # Prefix-Match auf agg_A (robust ggü. Suffix "(000)")

SENT_COL, BEFORE_COL, AFTER_COL, HUMAN_COL, ID_COL = (
    "sentence", "context_before", "context_after", "agg_A", "gs_id")

# ----------------------------------------------------------------------------
# 1) Daten + Modell laden
# ----------------------------------------------------------------------------
if not os.path.isdir(MODEL_DIR):
    sys.exit(f"FEHLER: Modellordner '{MODEL_DIR}' nicht gefunden.")
if not os.path.exists(GOLD_CSV):
    sys.exit(f"FEHLER: '{GOLD_CSV}' nicht gefunden.")

df = pd.read_csv(GOLD_CSV)
for c in (SENT_COL, HUMAN_COL, ID_COL):
    if c not in df.columns:
        sys.exit(f"FEHLER: Spalte '{c}' fehlt. Vorhanden: {list(df.columns)}")
n = len(df)
print(f"[gold_000_check] {n} Gold-Saetze geladen (erwartet: 249).")

cfg_path = os.path.join(MODEL_DIR, "train_config.json")
max_len, use_context = 256, True
if os.path.exists(cfg_path):
    with open(cfg_path, encoding="utf-8") as f:
        cfg = json.load(f)
    max_len     = int(cfg.get("max_len", max_len))
    use_context = bool(cfg.get("use_context", use_context))

import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification
device = "cuda" if torch.cuda.is_available() else "cpu"
tok    = AutoTokenizer.from_pretrained(MODEL_DIR)
model  = AutoModelForSequenceClassification.from_pretrained(MODEL_DIR).to(device).eval()
print(f"  Modell geladen | device={device} | max_len={max_len} | use_context={use_context} | thr={THRESHOLD}")

# ----------------------------------------------------------------------------
# 2) Scoren (Konstruktion wie 10/11: A=Satz, B=Kontext)
# ----------------------------------------------------------------------------
ta = df[SENT_COL].fillna("").astype(str).tolist()
if use_context:
    cb = df.get(BEFORE_COL, pd.Series([""] * n)).fillna("").astype(str)
    ca = df.get(AFTER_COL,  pd.Series([""] * n)).fillna("").astype(str)
    tb = (cb + " " + ca).str.strip().tolist()
else:
    tb = None

p000 = np.empty(n, dtype=np.float32)
BATCH = 32
with torch.inference_mode():
    for s in range(0, n, BATCH):
        a = ta[s:s+BATCH]
        b = tb[s:s+BATCH] if tb is not None else None
        enc = tok(a, b, truncation=True, max_length=max_len,
                  padding=True, return_tensors="pt").to(device)
        logits = model(**enc).logits
        p000[s:s+BATCH] = torch.softmax(logits, dim=-1)[:, 1].float().cpu().numpy()

df["p000"]      = p000
df["is_000"]    = df["p000"] >= THRESHOLD
df["human_000"] = df[HUMAN_COL].astype(str).str.startswith(LABEL_000)

# ----------------------------------------------------------------------------
# 3) Kreuztabelle + Kennzahlen
# ----------------------------------------------------------------------------
h1 = df["human_000"]; m1 = df["is_000"]
tp = int((h1 & m1).sum());  fn = int((h1 & ~m1).sum())
fp = int((~h1 & m1).sum()); tn = int((~h1 & ~m1).sum())
n_h000 = tp + fn

rows = [
    ("n_gold", n), ("n_human_000", n_h000), ("n_human_codable", fp + tn),
    ("threshold", THRESHOLD),
    ("flagged_of_human_000 (TP)", tp),
    ("missed_human_000 (FN)", fn),
    ("flagged_of_codable (FP)", fp),
    ("kept_codable (TN)", tn),
    ("recall_on_human_000", round(tp / n_h000, 4) if n_h000 else float("nan")),
    ("fp_rate_on_codable",  round(fp / (fp + tn), 4) if (fp + tn) else float("nan")),
    ("precision_of_flag",   round(tp / (tp + fp), 4) if (tp + fp) else float("nan")),
    ("median_p000_human000", round(float(df.loc[h1, "p000"].median()), 4) if n_h000 else float("nan")),
    ("median_p000_codable",  round(float(df.loc[~h1, "p000"].median()), 4)),
]
pd.DataFrame(rows, columns=["metric", "value"]).to_csv(OUT_SUM, index=False)
df[[ID_COL, HUMAN_COL, "human_000", "p000", "is_000"]].to_csv(OUT_ROWS, index=False)

print("\n================ GOLD-000-CHECK (post hoc) ================")
print(f"Human-000: {n_h000} | davon geflaggt (TP): {tp} | verpasst (FN): {fn}")
print(f"Codable:   {fp + tn} | faelschlich geflaggt (FP): {fp} | behalten (TN): {tn}")
if n_h000:
    print(f"Recall auf Human-000     : {tp / n_h000:.3f}")
print(f"FP-Rate auf Codable      : {fp / (fp + tn):.3f}")
if tp + fp:
    print(f"Precision der Flags      : {tp / (tp + fp):.3f}")
print(f"\ngeschrieben: {OUT_ROWS} | {OUT_SUM}")
print("===========================================================")
