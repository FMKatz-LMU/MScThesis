"""
Vorher-Nachher-Vergleich der Satzsegmentierung (alter Regex vs. SoMaJo).

Streamt beide CSVs (speicherschonend, Spalte 'sentence') und misst Indikatoren,
die typisch fuer Regex-Fehltrennungen sind:

  1. Anteil Saetze <= 3 Woerter                  (Fragmente generell)
  2. Anteil Saetze, die auf " N." enden          (Ordinal-Bruch: "... vom 1.")
  3. Anteil Saetze, die auf Abkuerzung enden     ("z. B." / "Dr." / "Art." / "bzw." ...)
  4. Anteil Saetze, die mit Monatsnamen beginnen (Fortsetzungs-Fragment: "Januar 1985 ...")
  5. Anteil Saetze, die mit Kleinbuchstaben beginnen (Fortsetzungs-Fragment allgemein)
  6. Gesamtzahl Saetze, mittlere Satzlaenge

Aufruf (Variante 1, altes Satz-CSV vorhanden):
  python compare_segmentation.py --old Data/sentences_for_classification.csv --new Data/sentences_somajo.csv

Aufruf (Variante 2, altes Satz-CSV fehlt -> alte Segmentierung wird aus den Reden
mit dem Original-Regex aus Script_Speechrefinement.R reproduziert):
  python compare_segmentation.py --old-speeches Data/all_speeches.csv --new Data/sentences_somajo.csv
"""
import argparse
import csv
import re
import sys

csv.field_size_limit(10_000_000)

ORDINAL_END = re.compile(r"\b\d{1,3}\.$")
ABBREV_END = re.compile(
    r"\b(z\.\s?B|u\.\s?a|d\.\s?h|bzw|ca|Dr|Prof|Art|Abs|Nr|Str|usw|vgl|inkl|ggf|evtl|sog|St|Mio|Mrd)\.$",
    re.IGNORECASE,
)
MONTH_START = re.compile(
    r"^(Januar|Februar|März|Maerz|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)\b"
)
LOWER_START = re.compile(r"^[a-zäöüß]")


# Original-Regex aus Script_Speechrefinement.R, Zeile 105:
#   strsplit(row$text, "(?<=[.!?])\\s+", perl = TRUE)
OLD_SPLIT = re.compile(r"(?<=[.!?])\s+")


def iter_sentences_csv(path):
    """Saetze aus einem Satz-Level-CSV (Spalte 'sentence')."""
    with open(path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        if not reader.fieldnames or "sentence" not in reader.fieldnames:
            sys.exit(f"FEHLER: Spalte 'sentence' fehlt oder Datei leer: {path} "
                     f"(Spalten: {reader.fieldnames})")
        for row in reader:
            s = row["sentence"]
            if s:
                yield s


def iter_sentences_regex(path):
    """Alte Segmentierung deterministisch reproduzieren: Reden-CSV (Spalte 'text')
    mit dem Original-Regex splitten, gleiche Trim-Regeln wie im R-Skript."""
    with open(path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        if not reader.fieldnames or "text" not in reader.fieldnames:
            sys.exit(f"FEHLER: Spalte 'text' fehlt oder Datei leer: {path} "
                     f"(Spalten: {reader.fieldnames})")
        for row in reader:
            text = row["text"]
            if not text:
                continue
            for s in OLD_SPLIT.split(text):
                s = s.strip()
                if s:
                    yield s


def scan(sent_iter):
    n = 0
    words_sum = 0
    short3 = 0
    ord_end = 0
    abbr_end = 0
    month_start = 0
    lower_start = 0
    examples = {"ord": [], "abbr": [], "month": []}

    for s in sent_iter:
        n += 1
        wc = s.count(" ") + 1
        words_sum += wc
        if wc <= 3:
            short3 += 1
        if ORDINAL_END.search(s):
            ord_end += 1
            if len(examples["ord"]) < 3:
                examples["ord"].append(s[-80:])
        if ABBREV_END.search(s):
            abbr_end += 1
            if len(examples["abbr"]) < 3:
                examples["abbr"].append(s[-80:])
        if MONTH_START.match(s):
            month_start += 1
            if len(examples["month"]) < 3:
                examples["month"].append(s[:80])
        if LOWER_START.match(s):
            lower_start += 1
    return {
        "n": n, "mean_len": words_sum / max(1, n),
        "short3": short3, "ord_end": ord_end, "abbr_end": abbr_end,
        "month_start": month_start, "lower_start": lower_start,
        "examples": examples,
    }


def pct(x, n):
    return f"{100 * x / max(1, n):6.3f} %"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--old", help="altes Satz-Level-CSV (Spalte 'sentence')")
    ap.add_argument("--old-speeches", dest="old_speeches",
                    help="Reden-CSV (Spalte 'text') -> alte Segmentierung wird per Original-Regex reproduziert")
    ap.add_argument("--new", required=True)
    args = ap.parse_args()
    if bool(args.old) == bool(args.old_speeches):
        ap.error("Genau EINES von --old oder --old-speeches angeben.")

    if args.old:
        print("Scanne ALT (Satz-CSV) ...")
        a = scan(iter_sentences_csv(args.old))
    else:
        print("Scanne ALT (Regex-Reproduktion aus Reden-CSV) ...")
        a = scan(iter_sentences_regex(args.old_speeches))
    print("Scanne NEU ...")
    b = scan(iter_sentences_csv(args.new))

    print("\n==================== Vergleich ====================")
    print(f"{'':38s}{'ALT (Regex)':>14s}{'NEU (SoMaJo)':>14s}")
    print(f"{'Saetze gesamt':38s}{a['n']:>14,}{b['n']:>14,}")
    print(f"{'Mittlere Satzlaenge (Woerter)':38s}{a['mean_len']:>14.1f}{b['mean_len']:>14.1f}")
    print(f"{'Saetze <= 3 Woerter':38s}{pct(a['short3'], a['n']):>14s}{pct(b['short3'], b['n']):>14s}")
    print(f"{'Endet auf Ordinal (\" N.\")':38s}{pct(a['ord_end'], a['n']):>14s}{pct(b['ord_end'], b['n']):>14s}")
    print(f"{'Endet auf Abkuerzung':38s}{pct(a['abbr_end'], a['n']):>14s}{pct(b['abbr_end'], b['n']):>14s}")
    print(f"{'Beginnt mit Monatsnamen':38s}{pct(a['month_start'], a['n']):>14s}{pct(b['month_start'], b['n']):>14s}")
    print(f"{'Beginnt mit Kleinbuchstaben':38s}{pct(a['lower_start'], a['n']):>14s}{pct(b['lower_start'], b['n']):>14s}")

    print("\nBeispiele NEU (falls noch vorhanden):")
    for key, label in [("ord", "Ordinal-Ende"), ("abbr", "Abkuerzungs-Ende"), ("month", "Monats-Anfang")]:
        for ex in b["examples"][key]:
            print(f"  [{label}] ...{ex}" if key != "month" else f"  [{label}] {ex}...")


if __name__ == "__main__":
    main()
