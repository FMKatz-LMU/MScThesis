"""
1b / Schritt 1: Inhaltsblinde Kandidatengenerierung fuer den Prozedursatz-Filter.

Streamt das SoMaJo-Korpus, normalisiert jeden Satz (lowercase, Whitespace kollabieren,
Randinterpunktion strippen, Namen nach Anrede-Woertern maskieren) und zaehlt exakte
Frequenzen der normalisierten Formen. Output: Top-N-Kandidatenliste mit Beispielsatz
und kumulativer Abdeckung — Grundlage fuer die formbasierte Typologie-Entscheidung
(Anrede / Dank-Schluss / Prozedural / Interjektion vs. behalten), die VOR jedem Blick
auf JSD/305-Effekte getroffen wird.

Designentscheidungen (dokumentieren!):
- Nur Saetze mit <= MAX_WORDS Woertern werden gezaehlt (Default 15): prozedurale
  Formeln sind kurz; lange Saetze sind praktisch nie exakte Duplikat-Templates.
  Das ist eine FORM-Regel, keine Inhaltsregel, und spart ~GB Speicher.
- Namens-Maskierung: "Herr Dr. Schäuble" -> "herr <NAME>", damit Anrede-Templates
  ueber Personen hinweg auf eine Form kollabieren. Die Maskierung ist bewusst
  konservativ; nicht erfasste Varianten landen einfach weiter hinten in der Liste
  bzw. in der Regex-Schicht (1b / Schritt 3).
- Zwei Pass-Durchlaeufe: Pass 1 zaehlt, Pass 2 sammelt je Top-Form einen
  Original-Beispielsatz (speicherschonend).

Aufruf:
  python build_procedural_candidates.py --in Data/sentences_somajo.csv --out results/procedural_candidates.csv
"""
import argparse
import csv
import re
import sys
from collections import Counter

csv.field_size_limit(10_000_000)

MAX_WORDS = 15
TOP_N = 1000

# Anrede-Maskierung (VOR dem Lowercasing): Anker (Herr/Frau/Kollege...) + optionale
# Titelkette (Dr., Prof., Präsidentin, Bundesminister, ...) + optionaler Nachname.
# Ersetzt wird nur, wenn nach dem Anker tatsaechlich Titel oder Name folgt —
# "Liebe Kolleginnen und Kollegen" bleibt unangetastet.
_TITLE = (r"(?:Dr\.|Prof\.|Vizepräsident(?:in)?|Präsident(?:in)?|Bundeskanzler(?:in)?|"
          r"Bundesminister(?:in)?|Staatsminister(?:in)?|Minister(?:in)?|"
          r"Staatssekretär(?:in)?|Kolleg(?:e|in)|Abgeordnete[rn]?|Senator(?:in)?)")
_NAME = r"[A-ZÄÖÜ][\wäöüß]+(?:-[A-ZÄÖÜ][\wäöüß]+)*"
NAME_AFTER = re.compile(
    rf"\b(Herrn?|Frau|Kolleg(?:e|in|en|innen))((?:\s+{_TITLE})*)(\s+{_NAME})?"
)


def _mask(m):
    if not m.group(2) and not m.group(3):
        return m.group(0)          # nur Ankerwort -> nicht maskieren
    return m.group(1) + " <name>"
EDGE_PUNCT = re.compile(r"^[\s\W]+|[\s\W]+$", re.UNICODE)
WS = re.compile(r"\s+")


def normalize(s):
    s = NAME_AFTER.sub(_mask, s)
    s = s.lower()
    s = WS.sub(" ", s)
    # Randinterpunktion strippen, aber <NAME>-Spitzklammern erhalten
    s = s.strip()
    s = re.sub(r"^[^\wäöüß<]+", "", s)
    s = re.sub(r"[^\wäöüß>]+$", "", s)
    return s.strip()


def iter_rows(path):
    with open(path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        if not reader.fieldnames or "sentence" not in reader.fieldnames:
            sys.exit(f"FEHLER: Spalte 'sentence' fehlt oder Datei leer: {path}")
        for row in reader:
            if row["sentence"]:
                yield row["sentence"]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="in_path", default="Data/sentences_somajo.csv")
    ap.add_argument("--out", dest="out_path", default="results/procedural_candidates.csv")
    ap.add_argument("--max-words", type=int, default=MAX_WORDS)
    ap.add_argument("--top-n", type=int, default=TOP_N)
    args = ap.parse_args()

    # ---- Pass 1: zaehlen --------------------------------------------------
    print("Pass 1: Frequenzen zaehlen ...")
    counts = Counter()
    n_total = 0
    n_counted = 0
    for s in iter_rows(args.in_path):
        n_total += 1
        if s.count(" ") + 1 > args.max_words:
            continue
        key = normalize(s)
        if key:
            counts[key] += 1
            n_counted += 1
        if n_total % 1_000_000 == 0:
            print(f"  {n_total:,} Saetze | {len(counts):,} Formen", flush=True)

    top = counts.most_common(args.top_n)
    top_keys = {k for k, _ in top}

    # ---- Pass 2: Beispielsaetze fuer Top-Formen sammeln --------------------
    print("Pass 2: Beispielsaetze sammeln ...")
    examples = {}
    for s in iter_rows(args.in_path):
        if s.count(" ") + 1 > args.max_words:
            continue
        key = normalize(s)
        if key in top_keys and key not in examples:
            examples[key] = s
            if len(examples) == len(top_keys):
                break

    # ---- Output ------------------------------------------------------------
    cum = 0
    with open(args.out_path, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["rank", "count", "share_of_corpus_pct", "cum_share_pct",
                    "normalized_form", "example_original", "typology"])
        for rank, (key, c) in enumerate(top, 1):
            cum += c
            w.writerow([rank, c,
                        round(100 * c / n_total, 4),
                        round(100 * cum / n_total, 3),
                        key, examples.get(key, ""), ""])

    # ---- Coverage-Kennzahlen (fuer die N-Wahl im Methodenteil) -------------
    print("\n================ Zusammenfassung ================")
    print(f"Saetze gesamt              : {n_total:,}")
    print(f"davon gezaehlt (<= {args.max_words} W.) : {n_counted:,} ({100*n_counted/n_total:.1f} %)")
    print(f"Eindeutige Formen          : {len(counts):,}")
    for N in (50, 100, 200, 500, 1000):
        cov = sum(c for _, c in counts.most_common(N))
        print(f"Top-{N:<5d} Abdeckung        : {cov:,} Saetze = {100*cov/n_total:.2f} % des Gesamtkorpus")
    print(f"\nKandidatenliste -> {args.out_path}")
    print("Naechster Schritt: Spalte 'typology' von Hand fuellen "
          "(anrede / dank_schluss / prozedural / interjektion / BEHALTEN), "
          "BEVOR irgendein JSD/305-Effekt angeschaut wird.")


if __name__ == "__main__":
    main()
