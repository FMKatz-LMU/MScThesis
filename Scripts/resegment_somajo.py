"""
Saubere Satztrennung fuer GermaParl-Reden mit SoMaJo (deutsch-spezifisch, deterministisch).

Ersetzt die naive Regex-Trennung aus Script_Speechrefinement.R (Schritt 6):
    strsplit(row$text, "(?<=[.!?])\\s+", perl = TRUE)
Der Regex bricht u. a. bei "1. Januar", "z. B.", "Dr.", "Art. 5 Abs. 1", "F.D.P.".
SoMaJo behandelt all diese Faelle korrekt (im Sandbox-Test verifiziert, 2026-06-12);
die im Korpus abgesetzten Anfuehrungszeichen/Gedankenstriche stoeren nicht, da die
Satzzeichen selbst angeheftet sind. Eine Detokenisierung ist daher NICHT noetig.

Input : CSV mit EINER Zeile pro Rede. Aus R exportieren:
        all_speeches <- data.table::rbindlist(readRDS(file.path(getwd(), "Data", "speeches_by_lp.rds")))
        data.table::fwrite(all_speeches, file.path(getwd(), "Data", "all_speeches.csv"))
        (benoetigte Spalten: speech_id, legislative_period, speaker, party, date, text;
         zusaetzliche Spalten wie word_count werden ignoriert)

Output: CSV mit EINER Zeile pro Satz — identisches Schema wie bisher, sodass die
        ManifestoBERTa-Inferenz unveraendert weiterlaeuft:
        speech_id, legislative_period, speaker, party, date,
        sentence_nr, sentence, context_before, context_after

Aufruf (Beispiel, im Projektroot S:/RProj_MSc/MScThesis):
        python resegment_somajo.py
        python resegment_somajo.py --in Data/all_speeches.csv --out Data/sentences_for_classification.csv --workers 8

Setup:  pip install somajo pandas
"""
import argparse
import csv
import multiprocessing as mp
import os
import sys
import time

import pandas as pd

OUT_COLS = [
    "speech_id", "legislative_period", "speaker", "party", "date",
    "sentence_nr", "sentence", "context_before", "context_after",
]

# ---------------------------------------------------------------- Worker ----
_TOKENIZER = None


def _init_worker():
    """Jeder Prozess bekommt seinen eigenen SoMaJo-Tokenizer (nicht picklebar)."""
    global _TOKENIZER
    from somajo import SoMaJo
    # de_CMC = deutsches Modell; split_camel_case=False schuetzt z. B. "BAfoeG".
    _TOKENIZER = SoMaJo("de_CMC", split_camel_case=False)


def _split_one(text):
    """Eine Rede -> Liste sauber getrennter Saetze (Reihenfolge erhalten)."""
    out = []
    for sent in _TOKENIZER.tokenize_text([text]):
        s = "".join(t.text + (" " if t.space_after else "") for t in sent).strip()
        # leere / buchstabenlose Fragmente (reine Satzzeichen, "...") verwerfen —
        # gleiche Regel wie im alten Pipeline-Schritt (nchar > 0, hier strenger)
        if any(ch.isalpha() for ch in s):
            out.append(s)
    return out


def _worker(text):
    return _split_one(text)


# ------------------------------------------------------------------ Main ----
def main():
    ap = argparse.ArgumentParser(description="GermaParl-Reden mit SoMaJo in Saetze trennen")
    ap.add_argument("--in", dest="in_path", default="Data/all_speeches.csv")
    ap.add_argument("--out", dest="out_path", default="Data/sentences_for_classification.csv")
    ap.add_argument("--workers", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    ap.add_argument("--progress-every", type=int, default=5000, help="Fortschritt alle N Reden")
    args = ap.parse_args()

    print(f"Lese {args.in_path} ...")
    df = pd.read_csv(
        args.in_path,
        encoding="utf-8",
        dtype={"speech_id": str, "speaker": str, "party": str, "date": str},
    )
    required = ["speech_id", "legislative_period", "speaker", "party", "date", "text"]
    missing = [c for c in required if c not in df.columns]
    if missing:
        sys.exit(f"FEHLER: Spalten fehlen im Input: {missing}")

    n_speeches = len(df)
    n_empty_text = int(df["text"].isna().sum())
    if n_empty_text:
        print(f"WARNUNG: {n_empty_text} Reden ohne Text werden uebersprungen.")
    texts = df["text"].fillna("").astype(str).tolist()
    meta = df[["speech_id", "legislative_period", "speaker", "party", "date"]].to_records(index=False)

    print(f"{n_speeches} Reden, {args.workers} Worker. Starte Segmentierung ...")
    t0 = time.time()
    n_sent_total = 0
    n_speeches_done = 0
    n_speeches_zero = 0
    n_short = 0          # Saetze mit <= 3 Woertern (Diagnose: Fragment-Anteil)
    len_sum = 0

    with open(args.out_path, "w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(OUT_COLS)

        def consume(idx, sents):
            nonlocal n_sent_total, n_speeches_done, n_speeches_zero, n_short, len_sum
            m = meta[idx]
            n = len(sents)
            if n == 0:
                n_speeches_zero += 1
            for j, s in enumerate(sents):
                writer.writerow([
                    m[0], m[1], m[2], m[3], m[4],
                    j + 1, s,
                    sents[j - 1] if j > 0 else "",
                    sents[j + 1] if j < n - 1 else "",
                ])
                wc = s.count(" ") + 1
                len_sum += wc
                if wc <= 3:
                    n_short += 1
            n_sent_total += n
            n_speeches_done += 1
            if n_speeches_done % args.progress_every == 0:
                rate = n_speeches_done / (time.time() - t0)
                eta = (n_speeches - n_speeches_done) / rate / 60
                print(f"  {n_speeches_done}/{n_speeches} Reden | {n_sent_total} Saetze | "
                      f"{rate:.0f} Reden/s | ETA {eta:.1f} min", flush=True)

        if args.workers > 1:
            with mp.Pool(args.workers, initializer=_init_worker) as pool:
                # imap erhaelt die Reihenfolge -> sentence_nr und Kontexte bleiben konsistent
                for idx, sents in enumerate(pool.imap(_worker, texts, chunksize=200)):
                    consume(idx, sents)
        else:
            _init_worker()
            for idx, text in enumerate(texts):
                consume(idx, _split_one(text))

    dt = (time.time() - t0) / 60
    print("\n================ Zusammenfassung ================")
    print(f"Reden verarbeitet      : {n_speeches_done} (davon {n_speeches_zero} ohne Satz)")
    print(f"Saetze geschrieben     : {n_sent_total}")
    print(f"Saetze pro Rede (Mittel): {n_sent_total / max(1, n_speeches_done):.1f}")
    print(f"Mittlere Satzlaenge    : {len_sum / max(1, n_sent_total):.1f} Woerter")
    print(f"Saetze mit <= 3 Woertern: {n_short} ({100 * n_short / max(1, n_sent_total):.2f} %)"
          f"  <- sollte deutlich unter dem alten Korpus liegen")
    print(f"Laufzeit               : {dt:.1f} min")
    print(f"Output                 : {args.out_path}")


if __name__ == "__main__":
    # Windows (spawn) braucht den __main__-Guard zwingend
    main()
