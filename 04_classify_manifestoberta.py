"""
04_classify_manifestoberta.py  —  RE-RUN auf dem SoMaJo-Korpus (Schritt 1c)
===========================================================================

PROVENIENZ: wiederhergestellt aus C:/masterarbeit/classify_manifestoberta_v2.py
(2026-07-07). Dies ist das Skript, das den Produktionskorpus
Data/sentences_classified_v2/ (576 Chunks) gebaut hat. Geaendert wurden NUR die
Pfad-Defaults (Projekt-Konvention statt masterarbeit-Datenordner), die Run-
Anleitung und dieser Header; Modell (2024-1-1), Tokenizer, Kontext-Format,
MAX_LENGTH, Chunking, AMP/fp16 und Resume-Logik sind unveraendert.

IDENTISCH zum Produktionslauf (classify_manifestoberta.py) bis auf:
  1. Input  = sentences_somajo_flagged.csv (re-segmentiert + is_procedural-Flags)
  2. Output = sentences_classified_v2/ (NEUES Verzeichnis; alte Chunks bleiben)
  3. Die drei Filter-Spalten (is_procedural, proc_class, proc_source) werden
     eingelesen und unveraendert in die Parquet-Chunks durchgereicht.
Modell, Tokenizer, Kontext-Format, MAX_LENGTH, AMP/fp16, Chunking, Resume:
alles unveraendert, damit Ergebnisdifferenzen allein dem Korpus zuzurechnen sind.

--- Original-Dokumentation: ---

Classifies German Bundestag speech sentences (extracted via polmineR from
GERMAPARL2) into the 56 Manifesto Project policy domains using
manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1.

Design
------
- Input  : Data/sentences_for_classification.csv (~6M rows)
           Columns: speech_id, legislative_period, speaker, party, date,
                    sentence_nr, sentence, context_before, context_after
- Output : Data/sentences_classified/chunk_00001.parquet, chunk_00002.parquet, ...
           Each chunk contains all input columns + 56 probability columns
           (one per Manifesto code, e.g. per101, per102, ...) +
           pred_label (top-1 code) + pred_score (top-1 probability).
- Resume : If a chunk file already exists, it is skipped. To re-classify a
           chunk, delete its file. Output schema is kept stable across runs.

Model input format (per the official model card)
------------------------------------------------
The model is a sentence-pair classifier with a custom XLM-RoBERTa head.
The tokenizer is called as `tokenizer(sentence, context, ...)`, where:
  - `sentence` is the target sentence (truncated to ~100 tokens)
  - `context`  is the surrounding paragraph (greedily filled up to 200 tokens)
    The model card explicitly states that the context should INCLUDE the
    sentence itself plus its surroundings; if no context is available, pass
    the sentence as its own context.

We therefore build context as:
    [context_before] + " " + [sentence] + " " + [context_after]
dropping empty/NA pieces. If both before and after are missing, we fall back
to using the sentence as its own context.

Run
---
    cd <PROJECT_ROOT>   # GPU-venv aktivieren: C:/masterarbeit/.venv
    python 04_classify_manifestoberta.py --input Data/sentences_somajo_flagged.csv --output-dir Data/sentences_classified_v2
"""

from __future__ import annotations

import argparse
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from torch.utils.data import DataLoader, Dataset
from tqdm.auto import tqdm
from transformers import AutoModelForSequenceClassification, AutoTokenizer


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

MODEL_NAME = (
    "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1"
)
TOKENIZER_NAME = "xlm-roberta-large"

# Project paths (relative to project root)
DEFAULT_INPUT_CSV = Path("Data") / "sentences_somajo_flagged.csv"
DEFAULT_OUTPUT_DIR = Path("Data") / "sentences_classified_v2"

# Per the model card: sentence ~100 tokens, context greedily filled to 200,
# total max ~300 tokens including special tokens.
MAX_LENGTH = 300

# Defaults: tune on your hardware.
DEFAULT_CHUNK_SIZE = 10_000     # rows read from CSV per chunk -> one parquet file
DEFAULT_BATCH_SIZE = 32         # sentences per forward pass
# On Windows the DataLoader uses the 'spawn' start method, which pickles
# every worker argument and reloads the entire script per worker. For a
# GPU-bound workload like ours, in-process tokenization (num_workers=0)
# is simpler and roughly as fast. Override with --num-workers if you have
# a reason to.
DEFAULT_NUM_WORKERS = 0 if sys.platform.startswith("win") else 2


# ---------------------------------------------------------------------------
# Dataset that tokenizes on the fly (sentence pair: sentence + context)
# ---------------------------------------------------------------------------

class SentencePairDataset(Dataset):
    """Wraps a pandas DataFrame of sentences + context columns and tokenizes
    on __getitem__. Tokenization is parallelized via DataLoader workers."""

    def __init__(self, df: pd.DataFrame, tokenizer, max_length: int = MAX_LENGTH):
        self.sentences = df["sentence"].astype(str).tolist()
        # Build the context string per the model card convention.
        self.contexts = self._build_contexts(df)
        self.tokenizer = tokenizer
        self.max_length = max_length

    @staticmethod
    def _build_contexts(df: pd.DataFrame) -> list[str]:
        """context = context_before + sentence + context_after, with NA pieces
        dropped. Falls back to the sentence itself when both surroundings
        are missing (per model card guidance)."""
        before = df["context_before"].fillna("").astype(str).str.strip()
        sent   = df["sentence"].fillna("").astype(str).str.strip()
        after  = df["context_after"].fillna("").astype(str).str.strip()

        contexts = []
        for b, s, a in zip(before, sent, after):
            parts = [p for p in (b, s, a) if p]
            if not parts:
                contexts.append(s)  # degenerate; keep something non-empty
            else:
                contexts.append(" ".join(parts))
        return contexts

    def __len__(self) -> int:
        return len(self.sentences)

    def __getitem__(self, idx: int) -> dict:
        enc = self.tokenizer(
            self.sentences[idx],
            self.contexts[idx],
            truncation=True,
            max_length=self.max_length,
            padding=False,           # padding done by collate_fn
            return_tensors=None,
        )
        return enc


class Collator:
    """Pads a list of tokenized examples into batched tensors.

    Implemented as a top-level class (not a closure returned by a factory)
    so it can be pickled by the multiprocessing 'spawn' start method used
    on Windows. Closures defined inside functions cannot be pickled in
    Python 3.14+, which breaks DataLoader workers on Windows."""

    def __init__(self, tokenizer):
        self.tokenizer = tokenizer

    def __call__(self, batch):
        return self.tokenizer.pad(
            batch,
            padding=True,
            return_tensors="pt",
        )


# ---------------------------------------------------------------------------
# Inference for a single chunk
# ---------------------------------------------------------------------------

@torch.inference_mode()
def classify_chunk(
    df: pd.DataFrame,
    model,
    tokenizer,
    label_names: list[str],
    device: torch.device,
    batch_size: int,
    num_workers: int,
    use_amp: bool,
) -> pd.DataFrame:
    """Run ManifestoBERTa on every row of df. Returns df with 56 probability
    columns + pred_label + pred_score appended."""
    ds = SentencePairDataset(df, tokenizer)
    loader = DataLoader(
        ds,
        batch_size=batch_size,
        shuffle=False,
        num_workers=num_workers,
        collate_fn=Collator(tokenizer),
        pin_memory=(device.type == "cuda"),
    )

    n = len(ds)
    n_labels = len(label_names)
    probs = np.empty((n, n_labels), dtype=np.float32)

    cursor = 0
    amp_dtype = torch.float16 if use_amp else None
    autocast_ctx = (
        torch.autocast(device_type=device.type, dtype=amp_dtype)
        if use_amp else _NullContext()
    )

    for batch in tqdm(loader, desc="  batches", leave=False):
        batch = {k: v.to(device, non_blocking=True) for k, v in batch.items()}
        with autocast_ctx:
            logits = model(**batch).logits
        # Softmax in float32 for numerical stability before storing.
        batch_probs = torch.softmax(logits.float(), dim=-1).cpu().numpy()
        bs = batch_probs.shape[0]
        probs[cursor:cursor + bs] = batch_probs
        cursor += bs

    assert cursor == n, f"Inference cursor mismatch: {cursor} vs {n}"

    # Top-1 label and score
    top_idx = probs.argmax(axis=1)
    pred_label = np.array(label_names)[top_idx]
    pred_score = probs[np.arange(n), top_idx]

    out = df.copy()
    # Insert the 56 probability columns.
    prob_df = pd.DataFrame(probs, columns=label_names, index=out.index)
    out = pd.concat([out, prob_df], axis=1)
    out["pred_label"] = pred_label
    out["pred_score"] = pred_score.astype(np.float32)
    return out


class _NullContext:
    """Stand-in for torch.autocast when AMP is disabled."""
    def __enter__(self): return None
    def __exit__(self, *exc): return False


# ---------------------------------------------------------------------------
# Main driver: stream the CSV in chunks, write one parquet per chunk
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input",  type=Path, default=DEFAULT_INPUT_CSV)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--chunk-size", type=int, default=DEFAULT_CHUNK_SIZE)
    parser.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    parser.add_argument("--num-workers", type=int, default=DEFAULT_NUM_WORKERS)
    parser.add_argument("--no-amp", action="store_true",
                        help="Disable mixed-precision (fp16) inference.")
    parser.add_argument("--cpu", action="store_true",
                        help="Force CPU even if a GPU is available.")
    parser.add_argument("--limit-chunks", type=int, default=None,
                        help="Stop after N chunks (for testing).")
    args = parser.parse_args()

    if not args.input.is_file():
        sys.exit(f"ERROR: input CSV not found: {args.input.resolve()}")

    # Guard: das richtige (geflaggte) CSV erwischt?
    header = pd.read_csv(args.input, nrows=0).columns
    required = {"sentence", "context_before", "context_after",
                "is_procedural", "proc_class", "proc_source"}
    missing = required - set(header)
    if missing:
        sys.exit(f"ERROR: Spalten fehlen im Input (falsches CSV?): {sorted(missing)}")

    args.output_dir.mkdir(parents=True, exist_ok=True)

    # ---- Device --------------------------------------------------------
    if args.cpu or not torch.cuda.is_available():
        device = torch.device("cpu")
        use_amp = False
    else:
        device = torch.device("cuda")
        use_amp = not args.no_amp

    print(f"Device: {device}"
          + (f" ({torch.cuda.get_device_name(0)})" if device.type == "cuda" else ""))
    print(f"Mixed precision (fp16): {use_amp}")

    # ---- Model + tokenizer --------------------------------------------
    print(f"Loading tokenizer: {TOKENIZER_NAME}")
    tokenizer = AutoTokenizer.from_pretrained(TOKENIZER_NAME)

    print(f"Loading model: {MODEL_NAME}")
    model = AutoModelForSequenceClassification.from_pretrained(
        MODEL_NAME,
        trust_remote_code=True,   # required: model uses a custom classification head
    )
    model.eval().to(device)

    # The custom head's id2label is the Manifesto codebook.
    id2label = model.config.id2label
    label_names = [id2label[i] for i in range(len(id2label))]
    print(f"Model has {len(label_names)} labels (e.g. {label_names[:5]} ...)")

    # ---- Stream the CSV in chunks --------------------------------------
    # Use a 1-based chunk index in filenames to keep them human-readable.
    reader = pd.read_csv(
        args.input,
        chunksize=args.chunk_size,
        dtype={
            "speech_id": "string",
            "legislative_period": "Int32",
            "speaker": "string",
            "party": "string",
            "date": "string",
            "sentence_nr": "Int32",
            "sentence": "string",
            "context_before": "string",
            "context_after": "string",
            "is_procedural": "Int8",
            "proc_class": "string",
            "proc_source": "string",
        },
        keep_default_na=True,
    )

    total_rows = 0
    total_done = 0
    t_start = time.time()

    for chunk_idx, chunk in enumerate(reader, start=1):
        if args.limit_chunks is not None and chunk_idx > args.limit_chunks:
            print(f"Reached --limit-chunks={args.limit_chunks}, stopping.")
            break

        out_path = args.output_dir / f"chunk_{chunk_idx:05d}.parquet"
        total_rows += len(chunk)

        if out_path.exists():
            print(f"[chunk {chunk_idx:>5}] skip (exists): {out_path.name}  "
                  f"rows={len(chunk):,}")
            continue

        print(f"[chunk {chunk_idx:>5}] classifying {len(chunk):,} rows -> "
              f"{out_path.name}")
        t0 = time.time()
        out_df = classify_chunk(
            df=chunk.reset_index(drop=True),
            model=model,
            tokenizer=tokenizer,
            label_names=label_names,
            device=device,
            batch_size=args.batch_size,
            num_workers=args.num_workers,
            use_amp=use_amp,
        )

        # Atomic write: write to a tmp file then rename, so a crash mid-write
        # doesn't leave a half-finished parquet that the resume logic would
        # mistakenly skip.
        tmp_path = out_path.with_suffix(".parquet.tmp")
        out_df.to_parquet(tmp_path, index=False, compression="zstd")
        tmp_path.replace(out_path)

        elapsed = time.time() - t0
        rate = len(chunk) / max(elapsed, 1e-6)
        total_done += len(chunk)
        cum = time.time() - t_start
        print(f"             done in {elapsed:6.1f}s ({rate:6.1f} sent/s)  "
              f"cumulative: {total_done:,} sent in {cum/60:.1f} min")

    print("\nAll chunks processed.")
    print(f"Output directory: {args.output_dir.resolve()}")


if __name__ == "__main__":
    # Safe to call on all platforms; required on Windows when DataLoader
    # workers > 0 and the script is frozen into an executable.
    import multiprocessing
    multiprocessing.freeze_support()
    main()
