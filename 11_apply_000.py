#!/usr/bin/env python3
# =============================================================================
#  11_apply_000.py — 000-Filter auf den vollen Korpus anwenden
# -----------------------------------------------------------------------------
#  Laedt das in 10_train_000.py finetunte Modell (model_000_gbert/) und
#  laeuft ueber die 576 v2-Chunks. Schreibt pro Satz:
#     p_000   float  — Wahrscheinlichkeit der 000-Klasse (nicht codierbar)
#     is_000  bool   — p_000 >= THRESHOLD
#  ALLE Originalspalten (inkl. der 56 Probs) bleiben erhalten -> Output ist ein
#  Drop-in-Ersatz fuer die JSD-Pipeline, nur mit der zusaetzlichen Filterachse.
#
#  NICHT-DESTRUKTIV: schreibt in ein Schwester-Verzeichnis (CHUNK_DIR + "_000"),
#  gleiche Dateinamen. RESUME: bereits geschriebene Chunks werden uebersprungen,
#  Schreiben erfolgt atomar (.tmp -> rename), damit ein Abbruch nachts nichts
#  halb-Geschriebenes hinterlaesst.
#
#  Die JSD wird downstream NICHT hier gerechnet — hier entsteht nur das Flag.
#  p_000 wird mitgeschrieben, damit du downstream re-thresholden kannst, ohne
#  die ganze Inferenz zu wiederholen.
#
#  SETUP (Windows / RTX 5070 / Blackwell — cu128-Build):
#    pip install torch --index-url https://download.pytorch.org/whl/cu128
#    pip install "transformers>=4.40" pandas pyarrow numpy sentencepiece protobuf
#  RUN:
#    python 11_apply_000.py
# =============================================================================

import os, sys, json, time, glob
import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# 0) KONFIGURATION   (PROJECT_ROOT analog zu 00_config.R)
# ---------------------------------------------------------------------------
PROJECT_ROOT = r"C:/RProj_MSc/MScThesis"
CHUNK_DIR    = os.path.join(PROJECT_ROOT, "Data", "sentences_classified_v2")
OUT_DIR      = CHUNK_DIR + "_000"               # Schwester-Verzeichnis, gleiche Dateinamen
MODEL_DIR    = "model_000_gbert"

THRESHOLD    = 0.5            # is_000 = p_000 >= THRESHOLD (p_000 wird mitgeschrieben)
BATCH        = 64            # Inferenz-Batch (groesser als Training; kein Grad)
TEST_CHUNKS  = None           # None = alle; int = nur die ersten n Chunks (Smoke-Test)
OVERWRITE    = False          # True = fertige Chunks neu rechnen

SENT_COL     = "sentence"
BEFORE_COL   = "context_before"
AFTER_COL    = "context_after"

# Fallbacks, falls model_000_gbert/filter_config.json fehlt:
DEFAULT_MAX_LEN     = 256
DEFAULT_USE_CONTEXT = True

# ---------------------------------------------------------------------------
# 1) Modell + Tokenizer + Filter-Config laden
# ---------------------------------------------------------------------------
def load_model():
    import torch
    from transformers import AutoTokenizer, AutoModelForSequenceClassification
    if not os.path.isdir(MODEL_DIR):
        sys.exit(f"FEHLER: Modellordner '{MODEL_DIR}' fehlt. Erst 10_train_000.py laufen lassen.")
    cfg_path = os.path.join(MODEL_DIR, "filter_config.json")
    cfg = json.load(open(cfg_path)) if os.path.exists(cfg_path) else {}
    max_len     = int(cfg.get("max_len", DEFAULT_MAX_LEN))
    use_context = bool(cfg.get("use_context", DEFAULT_USE_CONTEXT))

    def _load_tok(src):
        from transformers import BertTokenizerFast
        for attempt in (lambda: AutoTokenizer.from_pretrained(src),
                        lambda: AutoTokenizer.from_pretrained(src, from_slow=True),
                        lambda: BertTokenizerFast.from_pretrained(src, from_slow=True)):
            try:
                return attempt()
            except Exception:
                continue
        return AutoTokenizer.from_pretrained(src)   # letzte Exception sichtbar machen
    tok = _load_tok(MODEL_DIR)
    model = AutoModelForSequenceClassification.from_pretrained(MODEL_DIR)
    device = "cuda" if torch.cuda.is_available() else "cpu"
    use_amp = (device == "cuda")
    amp_dtype = torch.bfloat16 if (use_amp and torch.cuda.is_bf16_supported()) else torch.float16
    model.eval().to(device)
    print(f"Modell: {MODEL_DIR} | device={device} | amp={use_amp} ({amp_dtype if use_amp else 'fp32'}) "
          f"| max_len={max_len} | use_context={use_context} | threshold={THRESHOLD}")
    return torch, tok, model, device, use_amp, amp_dtype, max_len, use_context

# ---------------------------------------------------------------------------
# 2) Inferenz fuer EINEN Chunk (laengensortiert -> wenig Padding; Reihenfolge
#    wird exakt wiederhergestellt, sodass p_000 zur Originalzeile passt)
# ---------------------------------------------------------------------------
def predict_chunk(df, torch, tok, model, device, use_amp, amp_dtype, max_len, use_context):
    if SENT_COL not in df.columns:
        sys.exit(f"FEHLER: Spalte '{SENT_COL}' fehlt im Chunk. Vorhanden: {list(df.columns)[:12]} ...")
    ta = df[SENT_COL].fillna("").astype(str).tolist()
    if use_context:
        cb = df.get(BEFORE_COL, pd.Series([""] * len(df))).fillna("").astype(str)
        ca = df.get(AFTER_COL,  pd.Series([""] * len(df))).fillna("").astype(str)
        tb = (cb + " " + ca).str.strip().tolist()
    else:
        tb = None

    n = len(df)
    lengths = np.array([len(t.split()) + (len(tb[i].split()) if tb else 0) for i, t in enumerate(ta)])
    order = np.argsort(-lengths)                    # lang zuerst: evtl. OOM zeigt sich sofort
    preds = np.empty(n, dtype=np.float32)

    for start in range(0, n, BATCH):
        idx = order[start:start + BATCH]
        a_b = [ta[j] for j in idx]
        b_b = [tb[j] for j in idx] if tb is not None else None
        enc = tok(a_b, b_b, truncation=True, max_length=max_len,
                  padding=True, return_tensors="pt").to(device)
        with torch.inference_mode():
            if use_amp:
                with torch.autocast(device_type="cuda", dtype=amp_dtype):
                    logits = model(**enc).logits
            else:
                logits = model(**enc).logits
        p = torch.softmax(logits.float(), dim=1)[:, 1].detach().cpu().numpy()
        preds[idx] = p                              # zurueck an Originalposition

    return preds

# ---------------------------------------------------------------------------
# 3) Hauptlauf ueber alle Chunks (mit Resume)
# ---------------------------------------------------------------------------
def main():
    import pyarrow  # nur Existenz-Check
    if not os.path.isdir(CHUNK_DIR):
        sys.exit(f"FEHLER: Chunk-Verzeichnis '{CHUNK_DIR}' fehlt. PROJECT_ROOT/CHUNK_DIR pruefen.")
    os.makedirs(OUT_DIR, exist_ok=True)

    chunks = sorted(glob.glob(os.path.join(CHUNK_DIR, "*.parquet")))
    if not chunks:
        sys.exit(f"FEHLER: keine .parquet-Chunks unter {CHUNK_DIR}")
    if TEST_CHUNKS:
        chunks = chunks[:TEST_CHUNKS]
        print(f"** TESTMODUS: nur {len(chunks)} Chunk(s). TEST_CHUNKS=None fuer den vollen Lauf. **")

    torch, tok, model, device, use_amp, amp_dtype, max_len, use_context = load_model()

    n_total, n_000, t0 = 0, 0, time.time()
    done = skipped = 0
    for ci, src in enumerate(chunks, 1):
        fname = os.path.basename(src)
        dst   = os.path.join(OUT_DIR, fname)
        if os.path.exists(dst) and not OVERWRITE:
            skipped += 1
            if skipped <= 3 or skipped % 50 == 0:
                print(f"  [{ci}/{len(chunks)}] {fname}: existiert -> skip (Resume)")
            # mitzaehlen fuer die Schlussstatistik
            d = pd.read_parquet(dst, columns=["is_000"])
            n_total += len(d); n_000 += int(d["is_000"].sum())
            continue

        t_c = time.time()
        df = pd.read_parquet(src)
        if len(df) == 0:
            pd.DataFrame(df).to_parquet(dst, index=False); continue
        p = predict_chunk(df, torch, tok, model, device, use_amp, amp_dtype, max_len, use_context)
        df["p_000"]  = p.astype(np.float32)
        df["is_000"] = (p >= THRESHOLD)

        tmp = dst + ".tmp"
        df.to_parquet(tmp, index=False)
        os.replace(tmp, dst)                         # atomar

        n_total += len(df); n_000 += int(df["is_000"].sum()); done += 1
        rate = df["is_000"].mean()
        rps  = len(df) / max(time.time() - t_c, 1e-6)
        eta  = (len(chunks) - ci) * (time.time() - t0) / max(ci - skipped, 1) / 60
        print(f"  [{ci}/{len(chunks)}] {fname}: {len(df):>6} Saetze | is_000 {rate:5.3f} "
              f"| {rps:6.0f} S/s | ETA ~{eta:5.1f} min")

    dt = (time.time() - t0) / 60
    print("\n================ FERTIG ================")
    print(f"Chunks: {done} gerechnet, {skipped} via Resume uebersprungen")
    print(f"Saetze gesamt: {n_total:,} | is_000 @ {THRESHOLD}: {n_000:,} ({n_000/max(n_total,1):.3f})")
    print(f"Laufzeit: {dt:.1f} min")
    print(f"Output: {OUT_DIR}  (gleiche Dateinamen, + Spalten p_000 / is_000)")
    print("------------------------------------------------------------------")
    print("SANITY: is_000-Rate sollte grob bei ~0,22 liegen (Korpus-000-Anteil).")
    print("        Stark daneben -> Modell/Eingabe pruefen (Segment A=Satz, B=Kontext).")
    print("NAECHSTER SCHRITT (R-Pipeline): in 12_aggregate.R den v2_000-Pfad")
    print("        einlesen und VOR der Aggregation auf is_000==FALSE filtern; dann")
    print("        H1a-JSD mit/ohne Filter vergleichen und pruefen, ob die surgische")
    print("        305-Exklusion neben dem 000-Filter noch noetig ist (oder doppelt zaehlt).")

if __name__ == "__main__":
    main()
