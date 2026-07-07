# -*- coding: utf-8 -*-
"""
04_procedural_flag.py — Prozedursatz-Filter v2 (EXACT-ONLY) auf den SoMaJo-Korpus.

MERGE (pipeline reorg 2026-06): vereint die bisherigen zwei Dateien
  * procedural_rules.py  (Normalisierung + DORMANTE Regelschicht)
  * flag_procedural.py   (Anwendung des Filters auf den Korpus)
zu einem self-contained Skript. Der fruehere  from procedural_rules import ...
entfaellt; alle Funktionen leben jetzt in diesem Modul.

WAS ES TUT (v2, EXACT-ONLY): Geflaggt wird NUR auf Basis der handtypisierten
Top-500-Liste (procedural_filter_decisions_v1.csv, proc_source="exact"). Die
Regelschicht (rule_layer_class, Layer 2 / proc_source="rule") ist BEWUSST
ERHALTEN, aber DORMANT — sie wird vom Primary-Pfad NICHT aufgerufen und dient
nur als reproduzierbarer "exact+rule"-Robustness-Arm (v1-Verhalten). Begruendung
des Versionssprungs: exact-only haelt den Filter voll pre-committed/transparent
(keine forscher-definierte Regex-Generalisierung im Long-Tail); der nicht-
codierbare Long-Tail wird dem datengetriebenen gbert-000-Filter ueberlassen.

Flag, don't drop: Es wird NICHTS entfernt. Jeder Satz bekommt drei Zusatzspalten:
  is_procedural  : 0/1
  proc_class     : anrede | dank_schluss | prozedural | interjektion | gliederung | ""
  proc_source    : exact (Top-500-Liste) | ""        (in v2 nie "rule")
ManifestoBERTa klassifiziert anschliessend ALLE Saetze; der Filter wird erst bei
der Aggregation angewandt (gefiltert = primaer, ungefiltert = Robustheit).

Benoetigt im selben Ordner: procedural_filter_decisions_v1.csv
  (procedural_rules.py wird NICHT mehr benoetigt — es ist hier eingebettet.)

VERSION: v2 (2026-06-18, exact-only). v1 (2026-06-12) = exact+rule.

Aufruf:
  python 04_procedural_flag.py --in Data/sentences_somajo.csv ^
      --out Data/sentences_somajo_flagged.csv ^
      --decisions procedural_filter_decisions_v1.csv ^
      --stats results/procedural_flag_stats.csv
"""
import re
import argparse
import csv
import sys
import time
from collections import defaultdict

# ############################################################################
# ##  NORMALISIERUNG + DORMANTE REGELSCHICHT   (formerly procedural_rules.py) ##
# ############################################################################
VERSION = "v2"

# ---------------------------------------------------------- Normalisierung ----
_TITLE = (r"(?:Dr\.|Prof\.|Vizepräsident(?:in)?|Präsident(?:in)?|Bundeskanzler(?:in)?|"
          r"Bundesminister(?:in)?|Staatsminister(?:in)?|Minister(?:in)?|"
          r"Staatssekretär(?:in)?|Kolleg(?:e|in)|Abgeordnete[rn]?|Senator(?:in)?)")
_NAME = r"[A-ZÄÖÜ][\wäöüß]+(?:-[A-ZÄÖÜ][\wäöüß]+)*"
_NAME_AFTER = re.compile(
    rf"\b(Herrn?|Frau|Kolleg(?:e|in|en|innen))((?:\s+{_TITLE})*)(\s+{_NAME})?")
_WS = re.compile(r"\s+")


def _mask(m):
    if not m.group(2) and not m.group(3):
        return m.group(0)
    return m.group(1) + " <name>"


def normalize(s):
    """Identisch zur Kandidatengenerierung (build_procedural_candidates.py)."""
    s = _NAME_AFTER.sub(_mask, s)
    s = s.lower()
    s = _WS.sub(" ", s).strip()
    s = re.sub(r"^[^\wäöüß<]+", "", s)
    s = re.sub(r"[^\wäöüß>]+$", "", s)
    return s.strip()


# ================================================================================
# DORMANT ab v2 — nur noch fuer den optionalen exact+rule-Robustness-Arm.
# flag_procedural.py (v2) ruft rule_layer_class NICHT auf.
# ================================================================================

# ------------------------------------------------------- Segment-Klassen ----
_ANREDE_ADJ = {
    "meine", "mein", "sehr", "liebe", "lieber", "lieben", "werte", "werter",
    "werten", "verehrte", "verehrter", "verehrten", "verehrtes", "geehrte",
    "geehrter", "geehrten", "geschätzte", "geschätzter", "geschätzten",
    "guten", "morgen", "tag", "abend",
}
_ANREDE_NOUN = {
    "herr", "frau", "<name>", "präsident", "präsidentin", "präsidium",
    "kollege", "kollegin", "kollegen", "kolleginnen", "damen", "herren",
    "gäste", "gast", "zuhörer", "zuhörerinnen", "zuschauer", "zuschauerinnen",
    "besucher", "besucherinnen", "bürger", "bürgerinnen", "landsleute",
    "abgeordnete", "abgeordneten",
}
_ANREDE_FILLER = {
    "und", "auf", "den", "der", "die", "im", "in", "bei", "tribüne",
    "tribünen", "saal", "youtube", "demokratischen", "fraktionen", "parteien",
}
_ANREDE_ALLOWED = _ANREDE_ADJ | _ANREDE_NOUN | _ANREDE_FILLER

_FILLER_SEGMENTS = {"ja", "nein", "aber", "oh", "na", "also", "nun"}

_RE_DANK = re.compile(
    r"^(noch einmal |nochmals )?"
    r"(((ganz|recht|sehr|vielen|herzlichen|schönen|besten|lieben|tausend) )*dank(e)?( ?schön| sehr)?"
    r"|dankeschön|herzliches dankeschön"
    r"|haben sie vielen dank"
    r"|ich danke( ihnen| euch)?( sehr( herzlich)?| recht herzlich)?"
    r"|ich bedanke mich( sehr herzlich| bei ihnen)?"
    r"|dafür (herzlichen dank|vielen dank|bedanke ich mich|danke ich( ihnen)?))"
    r"( (dafür|dazu|sehr))?"
    r"( für (ihre|die|eure) (aufmerksamkeit|geduld|frage|nachfrage|zwischenfrage)| fürs zuhören)?$"
)
_RE_SCHLUSS = re.compile(
    r"^(ich komme( jetzt| nun| gleich)? zum (schluss|schluß|ende)"
    r"|lassen sie mich zum (schluss|schluß) kommen"
    r"|ich bin (sofort|gleich) fertig"
    r"|zum (schluss|schluß)"
    r"|in diesem sinne"
    r"|glück auf"
    r"|herzlichen glückwunsch)$"
)
_RE_STAGING = re.compile(
    r"^((aber|sehr|ja) )?(bitte|gern|gerne)( schön| sehr)?$"
    r"|^(nein, )?(danke|im moment nicht|jetzt nicht)$"
    r"|^entschuldigung$"
    r"|^ich bitte um ihre zustimmung$"
)
_NUM_ADV = "erstens|zweitens|drittens|viertens|fünftens|sechstens|siebtens|achtens|neuntens|zehntens"
_ORD = "erste[rnms]?|zweite[rnms]?|dritte[rnms]?|vierte[rnms]?|fünfte[rnms]?|sechste[rnms]?|letzte[rnms]?|allerletzte[rnms]?|weitere[rnms]?|nächste[rnms]?"
_RE_GLIED = re.compile(
    rf"^(und |zum )?({_NUM_ADV})$"
    rf"|^zum (ersten|zweiten|dritten|vierten|fünften)( punkt)?$"
    rf"|^(der |die |das |ein |eine |mein )?({_ORD}) (punkt|beispiel|satz)$"
    r"|^punkt (eins|zwei|drei|vier|fünf)$"
    r"|^ein letztes$"
)

_SPLIT = re.compile(r"\s*(?:[,;:]|\s-\s|\s–\s)\s*")


def _segment_class(seg):
    if not seg:
        return "leer"
    if seg in _FILLER_SEGMENTS:
        return "filler"
    toks = seg.split(" ")
    if all(t in _ANREDE_ALLOWED for t in toks) and any(t in _ANREDE_NOUN for t in toks):
        return "anrede"
    if _RE_DANK.match(seg):
        return "dank_schluss"
    if _RE_SCHLUSS.match(seg):
        return "dank_schluss"
    if _RE_STAGING.match(seg):
        return "prozedural"
    if _RE_GLIED.match(seg):
        return "gliederung"
    return None


def rule_layer_class(normalized_sentence):
    """DORMANT ab v2. None = kein Treffer. Sonst die prozedurale Klasse des Satzes.
    Wird vom v2-Flagger NICHT aufgerufen; nur fuer den exact+rule-Robustness-Arm."""
    segs = [s for s in _SPLIT.split(normalized_sentence) if s != ""]
    if not segs:
        return None
    classes = [_segment_class(s) for s in segs]
    if any(c is None for c in classes):
        return None
    core = [c for c in classes if c not in ("filler", "leer")]
    if not core:
        return None
    return core[0]

# ############################################################################
# ##  KORPUS-FLAGGER   (formerly flag_procedural.py)                         ##
# ############################################################################
csv.field_size_limit(10_000_000)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="in_path", default="Data/sentences_somajo.csv")
    ap.add_argument("--out", dest="out_path", default="Data/sentences_somajo_flagged.csv")
    ap.add_argument("--decisions", default="procedural_filter_decisions_v1.csv")
    ap.add_argument("--stats", default="results/procedural_flag_stats.csv")
    args = ap.parse_args()

    exact = {}
    with open(args.decisions, newline="", encoding="utf-8") as fh:
        for d in csv.DictReader(fh):
            if d["typology"] != "BEHALTEN":
                exact[d["normalized_form"]] = d["typology"]
    print(f"Filter {VERSION} (EXACT-ONLY): {len(exact)} exakte Formen geladen, KEINE Regelschicht.")

    t0 = time.time()
    n = 0
    n_flag = 0
    by_class = defaultdict(int)
    by_source = defaultdict(int)
    by_cell = defaultdict(lambda: [0, 0])   # (party, lp) -> [n, n_flagged]

    with open(args.in_path, newline="", encoding="utf-8") as fin, \
         open(args.out_path, "w", newline="", encoding="utf-8") as fout:
        reader = csv.DictReader(fin)
        if not reader.fieldnames or "sentence" not in reader.fieldnames:
            sys.exit(f"FEHLER: Spalte 'sentence' fehlt oder Datei leer: {args.in_path}")
        out_cols = reader.fieldnames + ["is_procedural", "proc_class", "proc_source"]
        writer = csv.DictWriter(fout, fieldnames=out_cols)
        writer.writeheader()

        for row in reader:
            n += 1
            nf = normalize(row["sentence"]) if row["sentence"] else ""
            cls, src = "", ""
            if nf in exact:
                cls, src = exact[nf], "exact"
            # v2: KEINE Regelschicht mehr — der else-Zweig (rule_layer_class) ist entfernt.
            flagged = 1 if cls else 0
            row["is_procedural"] = flagged
            row["proc_class"] = cls
            row["proc_source"] = src
            writer.writerow(row)

            if flagged:
                n_flag += 1
                by_class[cls] += 1
                by_source[src] += 1
            cell = by_cell[(row.get("party", ""), row.get("legislative_period", ""))]
            cell[0] += 1
            cell[1] += flagged
            if n % 1_000_000 == 0:
                print(f"  {n:,} Saetze | {n_flag:,} geflaggt | "
                      f"{n / (time.time() - t0):,.0f} Saetze/s", flush=True)

    with open(args.stats, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["party", "lp", "n_sent", "n_flagged", "pct_flagged"])
        for (party, lp), (tot, fl) in sorted(by_cell.items(), key=lambda x: (str(x[0][1]), str(x[0][0]))):
            w.writerow([party, lp, tot, fl, round(100 * fl / max(1, tot), 3)])

    print("\n================ Zusammenfassung (EXACT-ONLY, v2) ================")
    print(f"Saetze gesamt    : {n:,}")
    print(f"geflaggt         : {n_flag:,} ({100*n_flag/max(1,n):.3f} %)")
    print("nach Klasse      : " + ", ".join(f"{k}={v:,}" for k, v in sorted(by_class.items())))
    print("nach Quelle      : " + ", ".join(f"{k}={v:,}" for k, v in sorted(by_source.items())) + "  (v2: nur 'exact')")
    print(f"Stats pro (Partei, LP) -> {args.stats}")
    print(f"Output -> {args.out_path}")
    print("\nWICHTIG: Anteil pro (Partei, LP) in der Stats-Datei pruefen — faellt eine")
    print("Zelle stark aus der Reihe, ist das als potenzieller Confound zu dokumentieren.")


if __name__ == "__main__":
    main()
