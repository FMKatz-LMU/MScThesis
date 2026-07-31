#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
run_pipeline.py — Orchestrator fuer die gesamte Thesis-Pipeline (01 .. 17).

Faehrt die nummerierte Pipeline der Reihe nach ab und startet R- wie Python-
Schritte selbst. Schwere Schritte (GPU-Klassifikation, gbert-Training/-Apply,
Sonnet-API) haben den Modus 'manual': der Treiber HAELT AN, zeigt den exakten
Befehl + Grund und wartet auf deine Bestaetigung, statt sie blind zu starten
(falsche venv / GPU / API-Kosten). Alles andere laeuft automatisch durch.

Schritt-Modi
  auto     -> wird direkt ausgefuehrt (R via Rscript, Python via --python)
  confirm  -> billiges Python: fragt einmal nach [y], dann fuehrt der Treiber aus
  manual   -> du fuehrst es extern aus (richtige venv/GPU/Key); Treiber wartet

Beispiele
  python run_pipeline.py --list                 # nur den Plan zeigen
  python run_pipeline.py --dry-run              # Plan + was passieren WUERDE
  python run_pipeline.py                        # ganze Pipeline (mit Pausen)
  python run_pipeline.py --stage analysis       # nur 12..17 (R-Auswertung)
  python run_pipeline.py --from 12 --to 16      # nur einen Bereich
  python run_pipeline.py --stage analysis --yes # 12..17 ohne Rueckfragen
  python run_pipeline.py --root D:/pfad/zum/Projekt

Voraussetzungen
  * Aus der Python-Umgebung starten, die fuer die confirm-Schritte (02/04)
    somajo + pandas hat. Die manual-Schritte (03/08/10/11) laufen NICHT hier,
    sondern in ihrer eigenen Umgebung (Anweisung wird angezeigt).
  * Rscript muss auf dem PATH liegen (oder via --rscript angeben).
  * Projektwurzel enthaelt 00_config.R + die .Rproj-Datei (fuer here::here()).
"""

import argparse
import os
import shutil
import subprocess
import sys

# Windows/Pipe-Robustheit: bei Umleitung (Tee-Object, > datei) faellt Python
# sonst auf cp1252 zurueck und crasht an Nicht-ASCII-Zeichen (UnicodeEncodeError).
for _stream in (sys.stdout, sys.stderr):
    if _stream is not None and hasattr(_stream, "reconfigure"):
        try:
            _stream.reconfigure(encoding="utf-8", errors="replace")
        except Exception:
            pass

# ---------------------------------------------------------------------------
# Projekt-Defaults (anpassbar via CLI)
# ---------------------------------------------------------------------------
DEFAULT_ROOT  = r"C:/RProj_MSc/MScThesis"
GBERT_VENV    = r"C:\masterarbeit\.venv"   # nur fuer die Anzeige der manual-Schritte

# ---------------------------------------------------------------------------
# Die Pipeline. Reihenfolge = Ausfuehrungsreihenfolge.
#   n      : Pipeline-Nummer (fuer --from/--to)
#   file   : Dateiname im Projektwurzel-Verzeichnis
#   lang   : "R" | "py"
#   stage  : "build" (01..11, selten/teuer) | "analysis" (12..17, oft/billig)
#   mode   : "auto" | "confirm" | "manual"
#   note   : Voraussetzung / Hinweis (eine Zeile)
# ---------------------------------------------------------------------------
PIPELINE = [
    dict(n=1,  file="01_build_speech_corpus.R",      lang="R",  stage="build",
         mode="auto",    note="braucht GERMAPARL2/CWB (polmineR)"),
    dict(n=2,  file="02_resegment_somajo.py",        lang="py", stage="build", pip=["somajo", "pandas"],
         mode="confirm", note="CPU, ~Stunden; braucht somajo + pandas; liest all_speeches.csv -> sentences_somajo.csv"),
    dict(n=3,  file="03_procedural_flag.py",         lang="py", stage="build",
         mode="confirm", note="CPU; braucht procedural_filter_decisions_v1.csv; liest sentences_somajo.csv"),
    dict(n=4,  file="04_classify_manifestoberta.py", lang="py", stage="build",
         mode="manual",  note="GPU, ~Stunden. ManifestoBERTa 2024-1-1 -> 56-Prob-Parquet-Chunks. Liest sentences_somajo_flagged.csv"),
    dict(n=5,  file="05_pull_marpor.R",              lang="R",  stage="build",
         mode="auto",    note="braucht MARPOR-API-Key (manifestoR)"),
    dict(n=6,  file="06_gold_realign_validate.R",    lang="R",  stage="build",
         mode="auto",    note="braucht kodiertes Gold + v2-Korpus (in results/empirics/goldstandard/)"),
    dict(n=7,  file="07_control_sample.R",           lang="R",  stage="build",
         mode="auto",    note="braucht v2-Korpus"),
    dict(n=8,  file="08_control_sonnet.py",          lang="py", stage="build",
         mode="manual",  note="Anthropic-API (kostet $); braucht ANTHROPIC_API_KEY"),
    dict(n=9,  file="09_control_validate.R",         lang="R",  stage="build",
         mode="auto",    note="braucht Sonnet-Output (08) + Key-RDS"),
    dict(n=10, file="10_train_000.py",               lang="py", stage="build",
         mode="manual",  note=f"GPU gbert-large; venv {GBERT_VENV}; braucht Sonnet-Labels (08)"),
    dict(n=11, file="11_apply_000.py",               lang="py", stage="build",
         mode="manual",  note=f"GPU gbert-Apply, ~Stunden; venv {GBERT_VENV}; schreibt _000_exactonly-Korpus"),
    dict(n=12, file="12_aggregate.R",                lang="R",  stage="analysis",
         mode="auto",    note="~6-25 min: streamt _000-Korpus + Bootstrap -> Caches"),
    dict(n=13, file="13_dps_speclock.R",             lang="R",  stage="analysis",
         mode="auto",    note="braucht 12-Caches + gold_305_precision (aus 06)"),
    dict(n=14, file="14_H1.R",                       lang="R",  stage="analysis",
         mode="auto",    note="braucht 12-Caches"),
    dict(n=15, file="15_H2.R",                       lang="R",  stage="analysis",
         mode="auto",    note="braucht 12-Caches"),
    dict(n=16, file="16_H3.R",                       lang="R",  stage="analysis",
         mode="auto",    note="braucht 12-Caches"),
    dict(n=17, file="17_robustness.R",               lang="R",  stage="analysis",
         mode="auto",    note="braucht 12-Caches (speech_soft/method/manifesto/bootstrap)"),
]

# ---------------------------------------------------------------------------
# Hilfen
# ---------------------------------------------------------------------------
class C:
    BOLD = "\033[1m"; DIM = "\033[2m"; RST = "\033[0m"
    BLUE = "\033[34m"; GREEN = "\033[32m"; YELLOW = "\033[33m"; RED = "\033[31m"; CYAN = "\033[36m"

def supports_colour():
    return sys.stdout.isatty() and os.environ.get("NO_COLOR") is None

def paint(s, col):
    return f"{col}{s}{C.RST}" if supports_colour() else s

def banner(text, col=C.BLUE):
    line = "=" * 76
    print("\n" + paint(line, col))
    print(paint(text, C.BOLD))
    print(paint(line, col))

def step_label(s):
    tag = {"auto": "AUTO", "confirm": "PY?", "manual": "MANUAL"}[s["mode"]]
    return f"[{s['n']:02d}] {s['file']:<32} {s['lang']:<2}  {tag}"


def cmd_for(step, args):
    """Der Befehl, der den Schritt ausfuehrt (Liste fuer subprocess)."""
    path = step["file"]
    if step["lang"] == "R":
        return [args.rscript, path]
    return [args.python, path]


def cmd_str(step, args):
    return " ".join(cmd_for(step, args))


def select_steps(args):
    steps = PIPELINE
    if args.stage != "all":
        steps = [s for s in steps if s["stage"] == args.stage]
    if args.frm is not None:
        steps = [s for s in steps if s["n"] >= args.frm]
    if args.to is not None:
        steps = [s for s in steps if s["n"] <= args.to]
    return steps


def print_plan(steps, args):
    banner("PIPELINE-PLAN", C.CYAN)
    print(f"  Projektwurzel : {args.root}")
    print(f"  Rscript       : {args.rscript}")
    print(f"  Python        : {args.python}")
    print(f"  Schritte      : {len(steps)}  (Stage={args.stage}"
          + (f", from={args.frm}" if args.frm else "")
          + (f", to={args.to}" if args.to else "") + ")\n")
    for s in steps:
        missing = "" if os.path.exists(os.path.join(args.root, s["file"])) else paint("  [FEHLT]", C.RED)
        print("  " + step_label(s) + missing)
        print(paint(f"        {s['note']}", C.DIM))


def ensure_py_deps(step, args):
    """Prueft im Schritt deklarierte Python-Pakete (key 'pip') im Ziel-Interpreter
    und installiert Fehlendes automatisch nach. Bewusst NUR fuer deklarierte
    CPU-Schritte — GPU-Schritte (04/10/11) laufen in der masterarbeit-venv mit
    gepinnten Versionen und deklarieren daher nichts."""
    mods = step.get("pip") or []
    if not mods:
        return True
    missing = [m for m in mods
               if subprocess.run([args.python, "-c", f"import {m}"],
                                 capture_output=True).returncode != 0]
    if not missing:
        return True
    print(paint(f"    Fehlende Pakete im Interpreter: {', '.join(missing)}"
                f" -> pip install ...", C.YELLOW))
    if args.dry_run:
        print(paint("    [dry-run] pip install uebersprungen", C.YELLOW))
        return True
    r = subprocess.run([args.python, "-m", "pip", "install", *missing])
    if r.returncode != 0:
        print(paint("    FEHLER: pip install fehlgeschlagen", C.RED))
        return False
    still = [m for m in missing
             if subprocess.run([args.python, "-c", f"import {m}"],
                               capture_output=True).returncode != 0]
    if still:
        print(paint(f"    FEHLER: weiterhin nicht importierbar: {', '.join(still)}", C.RED))
        return False
    print(paint(f"    Installiert: {', '.join(missing)}", C.GREEN))
    return True


def run_subprocess(step, args):
    """Fuehrt einen Schritt aus, gibt True bei Erfolg zurueck."""
    if step.get("lang") == "py" and not ensure_py_deps(step, args):
        return False
    cmd = cmd_for(step, args)
    print(paint(f"  $ {' '.join(cmd)}   (cwd={args.root})", C.DIM))
    if args.dry_run:
        print(paint("    [dry-run] nicht ausgefuehrt", C.YELLOW))
        return True
    try:
        res = subprocess.run(cmd, cwd=args.root)
    except FileNotFoundError as e:
        print(paint(f"    FEHLER: Interpreter nicht gefunden: {e}", C.RED))
        return False
    if res.returncode != 0:
        print(paint(f"    FEHLER: Exit-Code {res.returncode}", C.RED))
        return False
    print(paint("    OK", C.GREEN))
    return True


def ask(prompt, choices):
    """Fragt bis eine gueltige Auswahl kommt. choices = z.B. 'ysq'. Gibt den Buchstaben."""
    if not sys.stdin.isatty():
        # Nicht-interaktiv: konservativ abbrechen statt blind zu raten.
        print(paint("    (nicht-interaktiv -> abgebrochen)", C.RED))
        return "q"
    while True:
        a = input(prompt).strip().lower()
        if a == "" and "\n" in choices:   # Enter erlaubt
            return "\n"
        if a in choices:
            return a


def handle_step(step, args):
    """Gibt 'ok' | 'skip' | 'quit' | 'fail' zurueck."""
    mode = step["mode"]
    # --yes hebt confirm-Rueckfragen auf; --auto-python macht manual -> confirm
    if args.auto_python and mode == "manual":
        mode = "confirm"

    banner(step_label(step), C.BLUE)
    print(paint(f"  {step['note']}", C.DIM))

    if mode == "auto":
        return "ok" if run_subprocess(step, args) else "fail"

    if mode == "confirm":
        if args.yes:
            return "ok" if run_subprocess(step, args) else "fail"
        print(f"  Jetzt ausfuehren?  [{paint('y', C.GREEN)}] ausfuehren "
              f"· [s] ueberspringen · [q] abbrechen")
        a = ask("  > ", "ysq")
        if a == "q": return "quit"
        if a == "s": return "skip"
        return "ok" if run_subprocess(step, args) else "fail"

    # mode == "manual"
    print(paint("  >> MANUELLER SCHRITT — bitte EXTERN ausfuehren:", C.YELLOW))
    print(paint(f"     {cmd_str(step, args)}", C.BOLD))
    if step["file"] in ("04_classify_manifestoberta.py", "10_train_000.py", "11_apply_000.py"):
        print(paint(f"     (zuerst venv aktivieren: {GBERT_VENV}\\Scripts\\activate)", C.DIM))
    if step["file"] == "08_control_sonnet.py":
        print(paint("     (ANTHROPIC_API_KEY setzen; kostet API-Credits)", C.DIM))
    print("     [Enter] = erledigt, weiter · [s] = ueberspringen · [q] = abbrechen")
    a = ask("  > ", "\nsq")
    if a == "q": return "quit"
    if a == "s": return "skip"
    return "ok"


def main():
    ap = argparse.ArgumentParser(description="Thesis-Pipeline-Orchestrator (01..17).")
    ap.add_argument("--root", default=DEFAULT_ROOT, help="Projektwurzel (Default: %(default)s)")
    ap.add_argument("--rscript", default="Rscript", help="Rscript-Pfad/-Befehl")
    ap.add_argument("--python", default=sys.executable, help="Python-Interpreter fuer py-Schritte")
    ap.add_argument("--stage", choices=["all", "build", "analysis"], default="all")
    ap.add_argument("--from", dest="frm", type=int, help="ab Schritt-Nr.")
    ap.add_argument("--to", type=int, help="bis Schritt-Nr. (inkl.)")
    ap.add_argument("--list", action="store_true", help="nur Plan zeigen, nichts ausfuehren")
    ap.add_argument("--dry-run", action="store_true", help="Plan + was ausgefuehrt WUERDE, ohne es zu tun")
    ap.add_argument("--yes", action="store_true", help="confirm-Schritte ohne Rueckfrage ausfuehren")
    ap.add_argument("--auto-python", action="store_true",
                    help="manual-Schritte (GPU/API) zu confirm herabstufen (Vorsicht: laeuft in DIESER Umgebung)")
    args = ap.parse_args()

    steps = select_steps(args)
    print_plan(steps, args)
    if args.list:
        return 0

    if shutil.which(args.rscript) is None and any(s["lang"] == "R" for s in steps) and not args.dry_run:
        print(paint(f"\nWARNUNG: '{args.rscript}' nicht im PATH gefunden — R-Schritte werden scheitern.\n"
                    f"  -> R installieren oder --rscript C:/Pfad/zu/Rscript.exe angeben.", C.RED))

    done, skipped = [], []
    for s in steps:
        if not os.path.exists(os.path.join(args.root, s["file"])):
            print(paint(f"\n[{s['n']:02d}] {s['file']} FEHLT in {args.root} — abgebrochen.", C.RED))
            return 2
        result = handle_step(s, args)
        if result == "ok":
            done.append(s["n"])
        elif result == "skip":
            skipped.append(s["n"])
            print(paint("    -> uebersprungen", C.YELLOW))
        elif result == "quit":
            print(paint("\nAbgebrochen auf Wunsch.", C.YELLOW)); break
        elif result == "fail":
            print(paint(f"\nGESTOPPT: Schritt {s['n']} ({s['file']}) ist fehlgeschlagen.", C.RED))
            print(paint(f"  Nach dem Fix weiter mit:  --from {s['n']}", C.DIM))
            return 1

    banner("FERTIG", C.GREEN)
    print(f"  ausgefuehrt : {done}")
    if skipped:
        print(f"  uebersprungen: {skipped}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
