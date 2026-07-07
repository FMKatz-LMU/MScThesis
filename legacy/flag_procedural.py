"""
flag_procedural.py — wendet den Prozedursatz-Filter v2 (EXACT-ONLY) auf den SoMaJo-Korpus an.

v2-AENDERUNG (Versionssprung ggue. v1): Die Regelschicht (procedural_rules.rule_layer_class,
proc_source="rule") ist aus dem aktiven Filter HERAUSGENOMMEN. Geflaggt wird NUR noch auf
Basis der handtypisierten Top-500-Liste (procedural_filter_decisions_v1.csv, proc_source="exact").
Begruendung: haelt den Prozedural-Filter voll pre-committed und transparent (keine
forscher-definierte Regex-Generalisierung); der nicht-codierbare Long-Tail wird dem
datengetriebenen gbert-000-Filter ueberlassen. Die exact+rule-Variante (v1) bleibt ueber
procedural_rules.rule_layer_class als optionaler Robustness-Arm reproduzierbar.

Flag, don't drop: Es wird NICHTS entfernt. Jeder Satz bekommt drei Zusatzspalten:
  is_procedural  : 0/1
  proc_class     : anrede | dank_schluss | prozedural | interjektion | gliederung | ""
  proc_source    : exact (Top-500-Liste) | ""        (in v2 nie "rule")
ManifestoBERTa klassifiziert anschliessend ALLE Saetze; der Filter wird erst bei
der Aggregation angewandt (gefiltert = primaer, ungefiltert = Robustheit).

Benoetigt im selben Ordner: procedural_rules.py, procedural_filter_decisions_v1.csv

Aufruf:
  python flag_procedural.py --in Data/sentences_somajo.csv ^
      --out Data/sentences_somajo_flagged.csv ^
      --decisions procedural_filter_decisions_v1.csv ^
      --stats results/procedural_flag_stats.csv
"""
import argparse
import csv
import sys
import time
from collections import defaultdict

# v2: nur noch VERSION + normalize importieren; rule_layer_class wird NICHT mehr aufgerufen.
from procedural_rules import VERSION, normalize

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
