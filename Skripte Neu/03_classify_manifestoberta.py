"""
ManifestoBERTa sentence classification
Reads:  Data/sentences_for_classification.csv  (~6M rows)
Writes: Data/sentences_classified.csv          (same rows + 'label' and 'score' columns)

Resume-safe: skips already-written chunks on restart.
"""

import os
import math
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification

# ── Config ────────────────────────────────────────────────────────────────────
MODEL_ID   = "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2023-1-1"
INPUT_CSV  = os.path.join("Data", "sentences_for_classification.csv")
OUTPUT_CSV = os.path.join("Data", "sentences_classified.csv")
BATCH_SIZE = 32      # lower if you get OOM; raise for faster throughput
CHUNK_SIZE = 10_000  # rows read from CSV at a time
# ─────────────────────────────────────────────────────────────────────────────

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")

print("Loading model …")
tokenizer = AutoTokenizer.from_pretrained(MODEL_ID)
model     = AutoModelForSequenceClassification.from_pretrained(MODEL_ID).to(device)
model.eval()
id2label  = model.config.id2label
print(f"Loaded — {len(id2label)} labels")


def build_input(sentence: str, before: str, after: str) -> str:
    """
    ManifestoBERTa context format:
        [context_before] </s></s> [sentence] </s></s> [context_after]
    Empty / NaN context fields are replaced with an empty string.
    """
    b = before if isinstance(before, str) else ""
    a = after  if isinstance(after,  str) else ""
    parts = []
    if b:
        parts.append(b)
    parts.append(sentence)
    if a:
        parts.append(a)
    return " </s></s> ".join(parts)


def classify_batch(texts: list[str]) -> tuple[list[str], list[float]]:
    enc = tokenizer(
        texts,
        padding=True,
        truncation=True,
        max_length=512,
        return_tensors="pt",
    ).to(device)

    with torch.no_grad():
        logits = model(**enc).logits

    probs      = torch.softmax(logits, dim=-1).cpu()
    pred_ids   = probs.argmax(dim=-1).tolist()
    pred_probs = probs.max(dim=-1).values.tolist()

    labels = [id2label[i] for i in pred_ids]
    return labels, pred_probs


# ── Resume logic ──────────────────────────────────────────────────────────────
rows_done = 0
write_header = True

if os.path.exists(OUTPUT_CSV):
    try:
        existing = pd.read_csv(OUTPUT_CSV, usecols=["sentence_nr"])
        rows_done = len(existing)
        write_header = False
        print(f"Resuming — {rows_done:,} rows already classified.")
    except Exception:
        pass  # corrupted file → start fresh
# ─────────────────────────────────────────────────────────────────────────────

total_rows = sum(1 for _ in open(INPUT_CSV, encoding="utf-8")) - 1  # subtract header
print(f"Total rows to classify: {total_rows:,}")

chunks_done  = rows_done // CHUNK_SIZE
rows_to_skip = chunks_done * CHUNK_SIZE

reader = pd.read_csv(
    INPUT_CSV,
    chunksize=CHUNK_SIZE,
    skiprows=range(1, rows_to_skip + 1) if rows_to_skip > 0 else None,
    encoding="utf-8",
    dtype=str,
    keep_default_na=False,
    na_values=["NA", ""],
)

rows_processed = rows_to_skip

for chunk_idx, chunk in enumerate(reader, start=chunks_done):
    chunk = chunk.copy()

    # Build model inputs
    inputs = [
        build_input(row["sentence"], row.get("context_before"), row.get("context_after"))
        for _, row in chunk.iterrows()
    ]

    # Run in mini-batches
    all_labels, all_scores = [], []
    for start in range(0, len(inputs), BATCH_SIZE):
        batch  = inputs[start : start + BATCH_SIZE]
        labels, scores = classify_batch(batch)
        all_labels.extend(labels)
        all_scores.extend(scores)

    chunk["label"] = all_labels
    chunk["score"] = [round(s, 4) for s in all_scores]

    chunk.to_csv(
        OUTPUT_CSV,
        mode="a",
        header=write_header,
        index=False,
        encoding="utf-8",
    )
    write_header = False

    rows_processed += len(chunk)
    pct = rows_processed / total_rows * 100
    print(f"  chunk {chunk_idx + 1} done — {rows_processed:,} / {total_rows:,} rows ({pct:.1f}%)")

print(f"\nDone. Results saved to {OUTPUT_CSV}")
