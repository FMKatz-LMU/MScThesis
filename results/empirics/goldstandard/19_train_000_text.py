#!/usr/bin/env python3
# =============================================================================
#  19_train_000_text.py — 000-Filter, TEXT-basiert (gbert-large)
# -----------------------------------------------------------------------------
#  Fine-Tunt einen deutschen Encoder (Default deepset/gbert-large) mit binaerem
#  Kopf "codierbar (101-706)  vs.  000 (inhaltsleer, nicht zuordenbar)".
#  Liest den SATZ + KONTEXT — also genau das Signal, das den BERT-Probs fehlt
#  und das die Decke von 18_train_000_classifier.py setzt (Inhalts-000 sind aus
#  den Probs nicht von echtem Inhalt trennbar; aus dem Text sehr wohl).
#
#  EINGABE (Join ueber cs_id):
#    Labels : control_sample_10k_sonnet.csv  (Sample A, alles random)
#             control_sample_10k_b_sonnet.csv (Sample B, sample_type-Flag)
#    Text   : control_sample_10k{,_b}.csv ODER .parquet  (sentence + context_*)
#  -> Trainings-Pool = ~20k random (Booster sample_type=="booster_welfare_cut"
#     wird ausgeschlossen, sonst verzerrt er Klassenbalance + Korpus-Deflation).
#
#  LABEL-DEFINITION (FEHLTRENNUNG_AS, Default "drop"):
#    Sonnet setzt bei Segmentierungsbruch marpor="000" UND fehltrennung=true.
#    Das ist laut Kodieranweisung eine EIGENE Kategorie (Datenqualitaet), die
#    downstream symmetrisch zum Gold ueber das Flag ausgeschlossen wird — NICHT
#    inhaltliches 000. Default: Fehltrennungs-Zeilen aus Training/Eval ENTFERNEN,
#    Positivklasse = inhaltliches 000 (marpor=="000" & !fehltrennung).
#      "drop"     -> Fehltrennung raus (sauber, empfohlen)
#      "positive" -> Fehltrennung zaehlt als 000  (== Script-18-Definition, fuer
#                    direkten Kennzahlvergleich)
#      "negative" -> Fehltrennung zaehlt als codierbar
#
#  EVALUATION (gleiche Metriken wie Script 18):
#    PR-AUC, ROC-AUC, P/R/F1 fuer die 000-Klasse, Threshold-Sweep,
#    Operating Point ~90% Recall, und die Demokratie-Deflation
#    (dreht der Filter die +25-Punkte-305/Democracy-Inflation Richtung Oracle?).
#    EVAL_SCHEME="cv" -> stratifiziertes k-Fold mit Out-of-Fold-Preds ueber die
#    vollen 20k (vergleichbar mit 18). "holdout" -> ein 85/15-Split (schnell).
#
#  AUSGABE:
#    model_000_gbert/            finales Modell (auf ALLEN random-Daten) -> 20_apply_000_text.py
#    oof_000_text_predictions.csv  cs_id, y_true_000, pred_prob_000  (parallel zu 18)
#    model_000_text_metrics.json   alle Kennzahlen
#
#  SETUP (Windows / RTX 5070 / Blackwell — cu128-Build noetig):
#    pip install torch --index-url https://download.pytorch.org/whl/cu128
#    pip install "transformers>=4.40" accelerate scikit-learn pandas pyarrow sentencepiece protobuf
#  RUN:
#    python 19_train_000_text.py
# =============================================================================

import os, sys, json, random
import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# 0) KONFIGURATION
# ---------------------------------------------------------------------------
LABEL_FILES = ["control_sample_10k_sonnet.csv", "control_sample_10k_b_sonnet.csv"]
# Textquellen: pro Label-Datei die passende Input-Datei (Reihenfolge egal,
# Join laeuft ueber cs_id). Parquet bevorzugt; .csv funktioniert genauso.
TEXT_FILES  = ["control_sample_10k.parquet",   "control_sample_10k_b.parquet",
               "control_sample_10k.csv",       "control_sample_10k_b.csv"]

MODEL_NAME   = "deepset/gbert-large"     # Alternativen: microsoft/mdeberta-v3-base, ModernGBERT
USE_CONTEXT  = True                       # focal sentence = Seg A, Kontext = Seg B
MAX_LEN      = 256
FEHLTRENNUNG_AS = "drop"                  # "drop" | "positive" | "negative"
ONLY_RANDOM  = True                       # Booster aus dem Training nehmen

EVAL_SCHEME  = "cv"                        # "cv" (k-Fold OOF) oder "holdout"
N_SPLITS     = 5
HOLDOUT_FRAC = 0.15

EPOCHS       = 3
BATCH        = 8             # 8 GB Laptop-GPU: konservativ; auf groesserer Karte ruhig 16-32
GRAD_ACCUM   = 2             # effektive Batchgroesse = BATCH * GRAD_ACCUM = 16
GRAD_CHECKPOINT = True       # spart Aktivierungsspeicher (~20-30% langsamer) -> noetig auf 8 GB
LR           = 1.5e-5
WARMUP_RATIO = 0.06
WEIGHT_DECAY = 0.01
CLASS_WEIGHT = None                       # None oder "balanced"
SEED         = 20260616

OUT_MODEL_DIR = "model_000_gbert"
OOF_OUT       = "oof_000_text_predictions.csv"
METRICS_OUT   = "model_000_text_metrics.json"
DEM_KEY       = "Democracy"               # Teilstring in pred_bucket_A

# ---------------------------------------------------------------------------
# 1) DATEN: laden, joinen, Label bauen  (kein torch noetig -> separat testbar)
# ---------------------------------------------------------------------------
def _read_any(path):
    return pd.read_parquet(path) if path.endswith(".parquet") else pd.read_csv(path)

def load_labels(label_files):
    frames = []
    for fp in label_files:
        if not os.path.exists(fp):
            print(f"  [WARN] Label-Datei fehlt, uebersprungen: {fp}")
            continue
        d = pd.read_csv(fp)
        if "sample_type" not in d.columns:      # Sample A: alles random
            d["sample_type"] = "random"
        keep = ["cs_id", "marpor", "fehltrennung", "sample_type",
                "pred_bucket_A", "is_procedural", "status"]
        d = d[[c for c in keep if c in d.columns]].copy()
        frames.append(d)
    if not frames:
        sys.exit("FEHLER: keine Label-Datei gefunden.")
    lab = pd.concat(frames, ignore_index=True)
    lab = lab[lab.get("status", "ok") == "ok"].copy()
    lab["cs_id"] = lab["cs_id"].astype(str)
    return lab

def load_text(text_files):
    frames, seen = [], set()
    for fp in text_files:
        if not os.path.exists(fp):
            continue
        d = _read_any(fp)
        if "cs_id" not in d.columns or "sentence" not in d.columns:
            continue
        for c in ("context_before", "context_after"):
            if c not in d.columns:
                d[c] = ""
        d = d[["cs_id", "sentence", "context_before", "context_after"]].copy()
        d["cs_id"] = d["cs_id"].astype(str)
        d = d[~d["cs_id"].isin(seen)]
        seen.update(d["cs_id"].tolist())
        frames.append(d)
    if not frames:
        sys.exit("FEHLER: keine Textquelle mit Spalten cs_id + sentence gefunden.\n"
                 "        Lade control_sample_10k{,_b}.csv ODER .parquet ins Arbeitsverzeichnis.")
    return pd.concat(frames, ignore_index=True)

def _as000(marpor):
    return marpor.astype(str).str.strip().str.zfill(3) == "000"

def build_dataset(lab, txt):
    df = lab.merge(txt, on="cs_id", how="inner")
    n_join = len(df)
    if n_join == 0:
        sys.exit("FEHLER: Join Labels<->Text leer. Stimmen die cs_id ueberein?")

    if ONLY_RANDOM and "sample_type" in df.columns:
        n_boost = int((df["sample_type"] != "random").sum())
        df = df[df["sample_type"] == "random"].copy()
        print(f"  Booster entfernt: {n_boost} (sample_type != random)")

    is000 = _as000(df["marpor"])
    fehl  = df.get("fehltrennung", False)
    fehl  = fehl.astype(str).str.lower().isin(["true", "1", "1.0"]) if fehl.dtype == object else fehl.astype(bool)

    n_fehl = int(fehl.sum())
    if FEHLTRENNUNG_AS == "drop":
        df, is000, keep = df[~fehl].copy(), is000[~fehl], ~fehl
        df["y"] = is000.values.astype(int)
        print(f"  Fehltrennung: {n_fehl} Zeilen ENTFERNT (Default 'drop').")
    elif FEHLTRENNUNG_AS == "positive":
        df["y"] = is000.values.astype(int)          # 000 inkl. Fehltrennung (== Script 18)
        print(f"  Fehltrennung: {n_fehl} Zeilen als 000 gewertet ('positive', Script-18-kompatibel).")
    elif FEHLTRENNUNG_AS == "negative":
        y = (is000 & ~fehl).astype(int)
        df["y"] = y.values
        print(f"  Fehltrennung: {n_fehl} Zeilen als codierbar gewertet ('negative').")
    else:
        sys.exit(f"FEHLER: FEHLTRENNUNG_AS='{FEHLTRENNUNG_AS}' unbekannt.")

    # Text-Segmente
    s  = df["sentence"].fillna("").astype(str)
    cb = df["context_before"].fillna("").astype(str)
    ca = df["context_after"].fillna("").astype(str)
    df["text_a"] = s
    df["text_b"] = (cb + " " + ca).str.strip() if USE_CONTEXT else ""

    df = df.reset_index(drop=True)
    pos = int(df["y"].sum())
    print(f"  Join: {n_join} | nach Filtern: {len(df)} | 000-Positiv: {pos} ({pos/len(df):.3f})")
    if "pred_bucket_A" not in df.columns:
        df["pred_bucket_A"] = ""        # Deflation dann nicht aussagekraeftig
    return df[["cs_id", "text_a", "text_b", "y", "pred_bucket_A"]].copy()

# ---------------------------------------------------------------------------
# 2) METRIKEN (identische Logik wie Script 18) — arbeitet auf y, oof, bert_dem
# ---------------------------------------------------------------------------
def evaluate(y, oof, bert_dem, verbose=True):
    from sklearn.metrics import (precision_recall_fscore_support, average_precision_score,
                                 roc_auc_score, precision_recall_curve)
    y = np.asarray(y); oof = np.asarray(oof)

    def at(thr):
        pred = (oof >= thr).astype(int)
        p, r, f, _ = precision_recall_fscore_support(y, pred, average="binary", zero_division=0)
        return float(p), float(r), float(f), int(pred.sum())

    pr_auc  = float(average_precision_score(y, oof))
    roc_auc = float(roc_auc_score(y, oof))
    p50, r50, f50, n50 = at(0.5)

    prec, rec, thrs = precision_recall_curve(y, oof)
    idx  = np.where(rec[:-1] >= 0.90)[0]
    thr90 = float(thrs[idx[-1]]) if len(idx) else 0.0
    p90, r90, f90, n90 = at(thr90)

    # Demokratie-Deflation
    dem = {"unfiltered": None, "oracle": None, "model_0.5": None, "model_recall90": None}
    if bert_dem is not None and bert_dem.any():
        true_000 = (y == 1)
        dem["unfiltered"]     = float(bert_dem.mean())
        dem["oracle"]         = float(bert_dem[~true_000].mean())
        dem["model_0.5"]      = float(bert_dem[oof < 0.5].mean())
        dem["model_recall90"] = float(bert_dem[oof < thr90].mean())

    if verbose:
        print("\n================ EVALUATION (out-of-fold) ================")
        print(f"PR-AUC (000): {pr_auc:.3f}   ROC-AUC: {roc_auc:.3f}")
        print(f"@ Threshold 0.50:  Precision {p50:.3f} | Recall {r50:.3f} | F1 {f50:.3f}  (als 000: {n50})")
        print("\nThreshold-Sweep:")
        print(f"  {'thr':>5} {'Prec':>6} {'Recall':>7} {'F1':>6} {'#filtered':>10}")
        for thr in [0.30, 0.40, 0.50, 0.60, 0.70, 0.80]:
            p, r, f, n = at(thr)
            print(f"  {thr:>5.2f} {p:>6.3f} {r:>7.3f} {f:>6.3f} {n:>10}")
        print(f"\nOperating Point ~90% Recall: thr={thr90:.3f} -> Precision {p90:.3f}, Recall {r90:.3f} (filtert {n90})")
        if dem["unfiltered"] is not None:
            print("\n-- Democracy-Anteil in BERTs Kodierung (random) --")
            print(f"  ungefiltert            : {dem['unfiltered']:.3f}")
            print(f"  Oracle (echte 000 raus): {dem['oracle']:.3f}   <- Zielmarke")
            print(f"  Modell-Filter @0.50    : {dem['model_0.5']:.3f}")
            print(f"  Modell-Filter @{thr90:.2f} (90%R): {dem['model_recall90']:.3f}")

    return {"pr_auc": pr_auc, "roc_auc": roc_auc,
            "precision_at_0.5": p50, "recall_at_0.5": r50, "f1_at_0.5": f50,
            "thr_recall90": thr90, "precision_at_recall90": p90, "recall_at_recall90": r90,
            "democracy_deflation": dem}

# ---------------------------------------------------------------------------
# 3) TRAINING (torch/transformers erst hier importiert)
# ---------------------------------------------------------------------------
def set_all_seeds(seed):
    random.seed(seed); np.random.seed(seed)
    try:
        import torch
        torch.manual_seed(seed); torch.cuda.manual_seed_all(seed)
    except Exception:
        pass

def main():
    set_all_seeds(SEED)
    print("== 000-Text-Klassifikator (gbert-large) ==")
    lab = load_labels(LABEL_FILES)
    txt = load_text(TEXT_FILES)
    data = build_dataset(lab, txt)

    import torch
    from torch.utils.data import Dataset
    from transformers import (AutoTokenizer, AutoModelForSequenceClassification,
                              Trainer, TrainingArguments, DataCollatorWithPadding,
                              set_seed as hf_set_seed)
    from sklearn.model_selection import StratifiedKFold, train_test_split
    hf_set_seed(SEED)

    use_cuda = torch.cuda.is_available()
    bf16 = bool(use_cuda and getattr(torch.cuda, "is_bf16_supported", lambda: False)())
    fp16 = bool(use_cuda and not bf16)
    print(f"CUDA: {use_cuda} | bf16: {bf16} | fp16: {fp16}")

    # Tokenizer robust laden: neuere transformers (v5) scheitern bei gbert ueber
    # AutoTokenizer (kaputter SP/protobuf-Konverterpfad). from_slow=True baut den
    # Fast-WordPiece-Tokenizer direkt aus vocab.txt -> umgeht das komplett.
    def _load_tok(name):
        from transformers import BertTokenizerFast
        for attempt in (lambda: AutoTokenizer.from_pretrained(name),
                        lambda: AutoTokenizer.from_pretrained(name, from_slow=True),
                        lambda: BertTokenizerFast.from_pretrained(name, from_slow=True)):
            try:
                return attempt()
            except Exception:
                continue
        sys.exit(f"FEHLER: Tokenizer '{name}' nicht ladbar. Bei Nicht-WordPiece-Modellen "
                 "(z.B. mDeBERTa): pip install sentencepiece protobuf  und transformers<5.")
    tok = _load_tok(MODEL_NAME)
    texts_a = data["text_a"].tolist()
    texts_b = data["text_b"].tolist() if USE_CONTEXT else None
    y = data["y"].to_numpy()
    bert_dem = data["pred_bucket_A"].astype(str).str.contains(DEM_KEY, case=False, na=False).to_numpy()

    class DS(Dataset):
        def __init__(self, idx):
            self.idx = np.asarray(idx)
        def __len__(self):
            return len(self.idx)
        def __getitem__(self, i):
            j = int(self.idx[i])
            if USE_CONTEXT:
                enc = tok(texts_a[j], texts_b[j], truncation=True, max_length=MAX_LEN)
            else:
                enc = tok(texts_a[j], truncation=True, max_length=MAX_LEN)
            enc["labels"] = int(y[j])
            return enc

    collator = DataCollatorWithPadding(tok)

    cw = None
    if CLASS_WEIGHT == "balanced":
        n = len(y); pos = int(y.sum()); neg = n - pos
        cw = torch.tensor([n / (2 * neg), n / (2 * pos)], dtype=torch.float)
        print(f"Klassen-Gewichte (balanced): neg={cw[0]:.3f} pos={cw[1]:.3f}")

    class WTrainer(Trainer):
        def compute_loss(self, model, inputs, return_outputs=False, **kw):
            labels = inputs.pop("labels")
            out = model(**inputs)
            wt = cw.to(out.logits.device) if cw is not None else None
            loss = torch.nn.functional.cross_entropy(out.logits, labels, weight=wt)
            return (loss, out) if return_outputs else loss

    def new_model():
        return AutoModelForSequenceClassification.from_pretrained(MODEL_NAME, num_labels=2)

    def targs(out_dir):
        return TrainingArguments(
            output_dir=out_dir, num_train_epochs=EPOCHS,
            per_device_train_batch_size=BATCH, per_device_eval_batch_size=BATCH * 2,
            gradient_accumulation_steps=GRAD_ACCUM, learning_rate=LR,
            warmup_ratio=WARMUP_RATIO, weight_decay=WEIGHT_DECAY,
            bf16=bf16, fp16=fp16, logging_steps=50,
            gradient_checkpointing=GRAD_CHECKPOINT,
            save_strategy="no", report_to=[], seed=SEED,
            dataloader_num_workers=0,          # Windows: 0 vermeidet Multiprocessing-Aerger
            disable_tqdm=False)

    def predict_proba(trainer, ds):
        logits = trainer.predict(ds).predictions
        logits = logits[0] if isinstance(logits, tuple) else logits
        z = torch.tensor(logits)
        return torch.softmax(z, dim=1)[:, 1].numpy()

    # ---- Evaluation: OOF-Preds erzeugen (volle Daten bleiben unangetastet) ----
    oof = np.full(len(y), np.nan)
    if EVAL_SCHEME == "cv":
        splits = list(StratifiedKFold(n_splits=N_SPLITS, shuffle=True,
                                      random_state=SEED).split(np.zeros(len(y)), y))
    elif EVAL_SCHEME == "holdout":
        tr, te = train_test_split(np.arange(len(y)), test_size=HOLDOUT_FRAC,
                                  stratify=y, random_state=SEED)
        splits = [(tr, te)]
    else:
        sys.exit(f"FEHLER: EVAL_SCHEME='{EVAL_SCHEME}' unbekannt.")

    for k, (tr, te) in enumerate(splits, 1):
        tag = f"Fold {k}/{len(splits)}" if EVAL_SCHEME == "cv" else "Holdout"
        print(f"\n--- {tag}  (train {len(tr)} / test {len(te)}) ---")
        mdl = WTrainer(model=new_model(), args=targs(f"{OUT_MODEL_DIR}/_eval{k}"),
                       train_dataset=DS(tr), data_collator=collator)
        mdl.train()
        oof[te] = predict_proba(mdl, DS(te))
        del mdl
        if use_cuda:
            torch.cuda.empty_cache()

    mask = ~np.isnan(oof)            # bei holdout nur te-Zeilen; bei cv alle
    metrics = evaluate(y[mask], oof[mask], bert_dem[mask])
    metrics.update({"model": MODEL_NAME, "eval_scheme": EVAL_SCHEME,
                    "fehltrennung_as": FEHLTRENNUNG_AS, "use_context": USE_CONTEXT,
                    "n_eval": int(mask.sum()), "n_pos": int(y[mask].sum())})

    pd.DataFrame({"cs_id": data["cs_id"].values[mask],
                  "y_true_000": y[mask].astype(int),
                  "pred_prob_000": oof[mask]}).to_csv(OOF_OUT, index=False)
    json.dump(metrics, open(METRICS_OUT, "w"), indent=2, ensure_ascii=False)

    # ---- Finales, produktives Modell: IMMER auf ALLEN random-Daten -----------
    #  (== Script 18: final auf allem random; die Eval oben diente nur den
    #   ehrlichen Kennzahlen). Das ist das Modell fuer 20_apply_000_text.py.
    print(f"\nTrainiere finales Modell auf allen {len(data)} Saetzen ...")
    final = WTrainer(model=new_model(), args=targs(OUT_MODEL_DIR),
                     train_dataset=DS(np.arange(len(data))), data_collator=collator)
    final.train()
    final.save_model(OUT_MODEL_DIR)
    tok.save_pretrained(OUT_MODEL_DIR)
    json.dump({"thr_default": 0.5, "thr_recall90": metrics["thr_recall90"],
               "max_len": MAX_LEN, "use_context": USE_CONTEXT, "dem_key": DEM_KEY},
              open(os.path.join(OUT_MODEL_DIR, "filter_config.json"), "w"), indent=2)

    print(f"\nGespeichert: {OUT_MODEL_DIR}/ (Modell) | {OOF_OUT} | {METRICS_OUT}")
    print("==========================================================")
    print("Vergleich zu 18: F1/PR-AUC hoeher? Demokratie-Deflation NAEHER am Oracle?")
    print("Wenn ja -> der Text traegt das Inhalts-000, das die Probs nicht trennen.")
    print("Danach: 20_apply_000_text.py auf die 576 v2-Chunks -> is_000-Flag.")

if __name__ == "__main__":
    main()
