#!/usr/bin/env python3
# =============================================================================
#  18_train_000_classifier.py — 000-Filter auf BERTs Wahrscheinlichkeitsvektor
# -----------------------------------------------------------------------------
#  Trainiert einen schlanken Klassifikator "codierbar / 000 (nicht zuordenbar)"
#  auf BERTs 56-dim Probs (+ ein paar abgeleitete Features + is_procedural).
#  Label = Sonnets marpor == "000". Trainiert NUR auf sample_type=="random"
#  (der Booster ist ein gezielter Oversample und würde die Klassenbalance
#  verzerren).
#
#  Evaluiert ehrlich per 5-facher Kreuzvalidierung:
#    - Precision/Recall/F1 für die 000-Klasse, PR-AUC, ROC-AUC
#    - Threshold-Sweep (Operating Point wählen: viel Recall vs. Precision)
#    - Demokratie-Deflation: dreht der Filter die +25-Punkte-Inflation zurück?
#  Speichert das finale Modell (auf allen random-Daten) für 19_apply_000_filter.py.
#
#  SETUP:  pip install scikit-learn pandas pyarrow numpy
#  RUN:    python 18_train_000_classifier.py
# =============================================================================

import sys, json
import numpy as np
import pandas as pd

try:
    from sklearn.ensemble import HistGradientBoostingClassifier
    from sklearn.model_selection import StratifiedKFold, cross_val_predict
    from sklearn.metrics import (precision_recall_fscore_support, average_precision_score,
                                 roc_auc_score, precision_recall_curve, confusion_matrix)
    import joblib
except ImportError:
    sys.exit("FEHLER: scikit-learn fehlt -> pip install scikit-learn")

# ---- Konfiguration ----------------------------------------------------------
SONNET_CSV = "control_sample_10k_b_sonnet.csv"     # Labels (+ sample_type)
PARQUET    = "control_sample_10k_b.parquet"        # 56 Probs + is_procedural
MODEL_OUT  = "model_000_histgb.joblib"
OOF_OUT    = "oof_000_predictions.csv"
METRICS_OUT = "model_000_metrics.json"
DEM_LABEL  = "Democracy & Political System"
N_SPLITS   = 5
SEED       = 20260616

# ---- 1) Laden + Join + auf random beschränken -------------------------------
lab = pd.read_csv(SONNET_CSV)
lab = lab[lab["status"] == "ok"].copy()
lab["cs_id"] = lab["cs_id"].astype(str)

pq = pd.read_parquet(PARQUET)
pq["cs_id"] = pq["cs_id"].astype(str)
prob_cols = [c for c in pq.columns if len(c) > 5 and c[:3].isdigit() and c[3:6] == " - "]
if len(prob_cols) < 56:
    sys.exit(f"FEHLER: nur {len(prob_cols)} Prob-Spalten im Parquet gefunden (brauche 56).")

df = lab.merge(pq[["cs_id", *prob_cols]], on="cs_id", how="inner")
df = df[df["sample_type"] == "random"].copy()         # Booster NICHT fürs Training
print(f"Trainingsdaten (random): {len(df)} Sätze")

y = (df["marpor"].astype(str).str.zfill(3) == "000").astype(int).values
print(f"  000-Anteil (Label): {y.mean():.3f}  ({y.sum()} positiv)")

# ---- 2) Features: 56 Probs + abgeleitete + is_procedural --------------------
P = df[prob_cols].to_numpy(dtype=float)
P = P / np.clip(P.sum(axis=1, keepdims=True), 1e-9, None)   # sicherheitshalber normieren
eps = 1e-12
entropy = -(P * np.log(P + eps)).sum(axis=1)
maxp    = P.max(axis=1)
sort2   = np.sort(P, axis=1)[:, ::-1]
top2gap = sort2[:, 0] - sort2[:, 1]
top5sum = sort2[:, :5].sum(axis=1)
p305_col = next((c for c in prob_cols if c.startswith("305 ")), None)
p305 = df[p305_col].to_numpy(dtype=float) if p305_col else np.zeros(len(df))
is_proc = df["is_procedural"].fillna(0).astype(float).to_numpy() if "is_procedural" in df else np.zeros(len(df))

X = np.column_stack([P, entropy, maxp, top2gap, top5sum, p305, is_proc])
feat_names = prob_cols + ["entropy", "max_prob", "top2_gap", "top5_sum", "p305", "is_procedural"]
print(f"  Features: {X.shape[1]} ({len(prob_cols)} Probs + 6 abgeleitete)")

# ---- 3) Kreuzvalidierte Out-of-Fold-Wahrscheinlichkeiten --------------------
def make_model():
    return HistGradientBoostingClassifier(
        max_iter=400, learning_rate=0.06, max_leaf_nodes=31,
        l2_regularization=1.0, early_stopping=True, random_state=SEED)

cv = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=SEED)
print(f"\n{N_SPLITS}-fache Kreuzvalidierung ...")
oof = cross_val_predict(make_model(), X, y, cv=cv, method="predict_proba", n_jobs=-1)[:, 1]

# ---- 4) Metriken ------------------------------------------------------------
def metrics_at(thr):
    pred = (oof >= thr).astype(int)
    p, r, f, _ = precision_recall_fscore_support(y, pred, average="binary", zero_division=0)
    return p, r, f, int(pred.sum())

pr_auc  = average_precision_score(y, oof)
roc_auc = roc_auc_score(y, oof)
p50, r50, f50, npred50 = metrics_at(0.5)

print("\n================ EVALUATION (out-of-fold) ================")
print(f"PR-AUC (000): {pr_auc:.3f}   ROC-AUC: {roc_auc:.3f}")
print(f"\n@ Threshold 0.50:  Precision {p50:.3f} | Recall {r50:.3f} | F1 {f50:.3f}  (als 000 markiert: {npred50})")

# Threshold-Sweep + Operating Point für ~90% Recall
print("\nThreshold-Sweep:")
print(f"  {'thr':>5} {'Prec':>6} {'Recall':>7} {'F1':>6} {'#filtered':>10}")
for thr in [0.30, 0.40, 0.50, 0.60, 0.70, 0.80]:
    p, r, f, n = metrics_at(thr)
    print(f"  {thr:>5.2f} {p:>6.3f} {r:>7.3f} {f:>6.3f} {n:>10}")

prec, rec, thrs = precision_recall_curve(y, oof)
idx = np.where(rec[:-1] >= 0.90)[0]
thr90 = thrs[idx[-1]] if len(idx) else 0.0
p90, r90, f90, n90 = metrics_at(thr90)
print(f"\nOperating Point für ~90% Recall: thr={thr90:.3f} -> Precision {p90:.3f}, Recall {r90:.3f} (filtert {n90})")

# ---- 5) Demokratie-Deflation: dreht der Filter die Inflation zurück? --------
def dem_label_match(s):
    return s.astype(str).str.contains("Democracy", case=False, na=False)

bert_dem = dem_label_match(df["pred_bucket_A"]).to_numpy()
true_000 = (y == 1)
def dem_share(mask_keep):
    return bert_dem[mask_keep].mean()

unfiltered = dem_share(np.ones(len(df), bool))
oracle     = dem_share(~true_000)                       # echte 000 entfernt (Ideal)
model50    = dem_share(oof < 0.5)                        # vom Modell als codierbar behalten
model90    = dem_share(oof < thr90)
print("\n-- Demokratie-Anteil in BERTs Kodierung (Random-Sample) --")
print(f"  ungefiltert            : {unfiltered:.3f}")
print(f"  Oracle (echte 000 raus): {oracle:.3f}   <- Zielmarke")
print(f"  Modell-Filter @0.50    : {model50:.3f}")
print(f"  Modell-Filter @{thr90:.2f} (90%R): {model90:.3f}")

# ---- 6) Feature-Wichtigkeit (Permutation, schnell auf Subsample) -----------
from sklearn.inspection import permutation_importance
m_tmp = make_model().fit(X, y)
sub = np.random.RandomState(SEED).choice(len(X), size=min(3000, len(X)), replace=False)
pi = permutation_importance(m_tmp, X[sub], y[sub], n_repeats=5, random_state=SEED, n_jobs=-1)
order = np.argsort(pi.importances_mean)[::-1][:12]
print("\n-- Top-12 Features (Permutation Importance) --")
for k in order:
    print(f"  {feat_names[k]:<16} {pi.importances_mean[k]:.4f}")

# ---- 7) Finales Modell auf ALLEN random-Daten + speichern ------------------
final = make_model().fit(X, y)
joblib.dump({"model": final, "prob_cols": prob_cols, "feat_order": feat_names,
            "p305_col": p305_col, "thr_default": 0.5, "thr_recall90": float(thr90)}, MODEL_OUT)

pd.DataFrame({"cs_id": df["cs_id"].values, "y_true_000": y,
             "pred_prob_000": oof}).to_csv(OOF_OUT, index=False)
json.dump({"pr_auc": float(pr_auc), "roc_auc": float(roc_auc),
           "f1_at_0.5": float(f50), "precision_at_0.5": float(p50), "recall_at_0.5": float(r50),
           "thr_recall90": float(thr90), "dem_unfiltered": float(unfiltered),
           "dem_oracle": float(oracle), "dem_model_0.5": float(model50)},
          open(METRICS_OUT, "w"), indent=2)

print(f"\nGespeichert: {MODEL_OUT} (Modell) | {OOF_OUT} (OOF-Preds) | {METRICS_OUT}")
print("==========================================================")
print("VERDIKT: F1 ≥ ~0.85 UND Modell-Deflation nahe Oracle -> prob-basiert reicht,")
print("         weiter mit 19_apply_000_filter.py. Sonst -> Text-Modell.")
