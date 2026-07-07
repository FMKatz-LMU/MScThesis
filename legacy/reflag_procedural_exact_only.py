"""
reflag_procedural_exact_only.py
-------------------------------------------------------------------------------
Setzt is_procedural im bereits KLASSIFIZIERTEN v2_000-Korpus NEU, ausschliesslich
auf der handtypisierten Top-500-Liste (Layer 1 / proc_source="exact") — OHNE die
Regelschicht (Layer 2 / "rule"). = Prozedural-Filter v2 (exact-only), in-place.

WARUM SO: ManifestoBERTa klassifiziert unabhaengig von is_procedural. Der Filter
greift erst in 03 bei der Aggregation. Es reicht also, die Spalte is_procedural
(und proc_class/proc_source) in den bestehenden Chunks neu zu schreiben.
=> KEINE Re-Klassifikation, KEINE GPU, KEINE Re-Segmentierung. Der teure 000-Apply
   (p_000/is_000) bleibt unangetastet.

ZWEI PFADE (automatisch gewaehlt):
  A) proc_source ist als Spalte vorhanden  -> is_procedural := (proc_source == "exact").
     Exakt und sofort; keine Normalisierung noetig. proc_class/proc_source der
     ehemaligen rule-Treffer werden geleert.
  B) proc_source fehlt -> exact-Liste laden, jeden Satz mit dem KANONISCHEN
     normalize() (procedural_rules) normalisieren, is_procedural := normalize in exact.
     proc_class wird aus der Typologie der Liste rekonstruiert.

Nicht-destruktiv: schreibt nach --out-dir (atomar .tmp->rename, resume-faehig).
Mit --in-place werden die Chunks im Quellordner ueberschrieben (Backup empfohlen).

Benoetigt: pyarrow; procedural_rules.py (fuer normalize, nur Pfad B);
           procedural_filter_decisions_v1.csv (nur Pfad B oder zum proc_class-Recon).

Aufruf:
  python reflag_procedural_exact_only.py ^
      --parquet-dir Data/sentences_classified_v2_000 ^
      --decisions  procedural_filter_decisions_v1.csv ^
      --out-dir    Data/sentences_classified_v2_000_exactonly
"""
import argparse
import csv
import glob
import os
import sys
import time

import pyarrow as pa
import pyarrow.parquet as pq


def load_exact(path):
    """normalized_form -> typology (ohne BEHALTEN)."""
    exact = {}
    with open(path, newline="", encoding="utf-8") as fh:
        for d in csv.DictReader(fh):
            if d.get("typology", "") and d["typology"] != "BEHALTEN":
                exact[d["normalized_form"]] = d["typology"]
    return exact


def set_col(tbl, name, values, typ):
    arr = pa.array(values, type=typ)
    if name in tbl.column_names:
        return tbl.set_column(tbl.column_names.index(name), name, arr)
    return tbl.append_column(name, arr)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--parquet-dir", default="Data/sentences_classified_v2_000")
    ap.add_argument("--decisions", default="procedural_filter_decisions_v1.csv")
    ap.add_argument("--out-dir", default="Data/sentences_classified_v2_000_exactonly")
    ap.add_argument("--in-place", action="store_true",
                    help="Chunks im --parquet-dir ueberschreiben statt --out-dir (Backup machen!)")
    args = ap.parse_args()

    files = sorted(glob.glob(os.path.join(args.parquet_dir, "*.parquet")))
    if not files:
        sys.exit(f"FEHLER: keine .parquet-Chunks in {args.parquet_dir}")
    out_dir = args.parquet_dir if args.in_place else args.out_dir
    os.makedirs(out_dir, exist_ok=True)

    # Pfad-Erkennung am ersten Chunk
    first_cols = pq.read_schema(files[0]).names
    use_src = "proc_source" in first_cols
    print(f"[reflag] {len(files)} Chunks | Pfad {'A (proc_source vorhanden)' if use_src else 'B (Re-Normalisierung)'}")

    exact = None
    normalize = None
    if not use_src:
        exact = load_exact(args.decisions)
        from procedural_rules import normalize  # kanonisch, identisch zum Flagging
        print(f"[reflag] exact-Liste: {len(exact)} Formen; normalize() aus procedural_rules importiert.")

    n_tot = n_flag_new = n_changed = 0
    t0 = time.time()
    for fi, fp in enumerate(files, 1):
        out_fp = os.path.join(out_dir, os.path.basename(fp))
        if (not args.in_place) and os.path.exists(out_fp):
            continue  # resume

        t = pq.read_table(fp)
        cols = t.column_names
        n = t.num_rows

        old = ([bool(x) for x in t.column("is_procedural").to_pylist()]
               if "is_procedural" in cols else [False] * n)
        # Original-Datentyp von is_procedural beibehalten (03 nutzt as.logical()).
        old_type = t.schema.field("is_procedural").type if "is_procedural" in cols else pa.int64()

        if use_src:
            src = t.column("proc_source").to_pylist()
            new = [(s == "exact") for s in src]
            pcls = (t.column("proc_class").to_pylist() if "proc_class" in cols else [""] * n)
            new_src = ["exact" if s == "exact" else "" for s in src]
            new_pcls = [c if s == "exact" else "" for c, s in zip(pcls, src)]
        else:
            sent = t.column("sentence").to_pylist()
            nf = [normalize(x) if x else "" for x in sent]
            new = [k in exact for k in nf]
            new_src = ["exact" if v else "" for v in new]
            new_pcls = [exact.get(k, "") if (k in exact) else "" for k in nf]

        # Werte in den Original-Typ von is_procedural giessen
        def to_old_type(boolvals):
            if pa.types.is_boolean(old_type):
                return list(boolvals), pa.bool_()
            if pa.types.is_string(old_type):
                return ["1" if v else "0" for v in boolvals], pa.string()
            return [1 if v else 0 for v in boolvals], old_type  # int-Familie
        ip_vals, ip_type = to_old_type(new)

        t = set_col(t, "is_procedural", ip_vals, ip_type)
        t = set_col(t, "proc_source", new_src, pa.string())
        t = set_col(t, "proc_class", new_pcls, pa.string())

        tmp = out_fp + ".tmp"
        pq.write_table(t, tmp)
        os.replace(tmp, out_fp)

        nf_new = sum(new)
        ch = sum(1 for a, b in zip(old, new) if a != b)
        n_tot += n; n_flag_new += nf_new; n_changed += ch
        if fi % 25 == 0 or fi == len(files):
            rate = n_tot / max(1e-9, time.time() - t0)
            print(f"  [{fi}/{len(files)}] {os.path.basename(fp)}: "
                  f"{nf_new:,}/{n:,} proc (war {sum(old):,}) | {ch:,} geaendert | {rate:,.0f} Z/s", flush=True)

    print("\n================ FERTIG (exact-only, v2) ================")
    print(f"Saetze gesamt          : {n_tot:,}")
    print(f"is_procedural (exact)  : {n_flag_new:,} ({100*n_flag_new/max(1,n_tot):.3f} %)")
    print(f"Flags geaendert (rule->0): {n_changed:,}  <- diese Saetze kehren in den codierbaren Pool zurueck")
    print(f"Output -> {out_dir}")
    print("\nNAECHSTER SCHRITT:")
    print("  1) In 03_load_and_aggregate.R PARQUET_DIR_000 auf den exact-only-Korpus zeigen")
    print("     (oder --in-place nutzen und denselben Pfad behalten).")
    print("  2) run_all_v2_0-3.R (BUILD, baut is_primary neu)  ->  run_all_v2_3-11.R (04-11).")
    print("  3) GEGENCHECKEN: 305-Asymmetrie (11 PART 2), DPS-Anteil (10), H1a — ob der 000-Filter")
    print("     den zurueckkehrenden prozeduralen Long-Tail auffaengt (is_000) oder 305 re-inflationiert.")


if __name__ == "__main__":
    main()
