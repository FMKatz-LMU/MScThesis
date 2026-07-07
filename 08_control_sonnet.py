#!/usr/bin/env python3
# =============================================================================
#  08_control_sonnet.py — Kontrollstichprobe mit Sonnet kodieren (Array-Methode)
# -----------------------------------------------------------------------------
#  Parametrisiert über SAMPLE_TAG: "" = erstes Sample, "_b" = zweites Sample.
#  Schickt Gruppen von Sätzen als JSON-Array pro Request (so wie der Prompt es
#  vorsieht) und parst ein JSON-Array zurück -> ~20x billiger als ein Satz pro
#  Request. BERTs Vorhersage wird durchgereicht; Sonnet kodiert BLIND.
#
#  SETUP:
#    pip install "anthropic>=0.40" pandas pyarrow
#    export ANTHROPIC_API_KEY="sk-ant-..."        # PowerShell: $env:ANTHROPIC_API_KEY="..."
#    python 08_control_sonnet.py
# =============================================================================

import os, sys, json, time, re
import pandas as pd
import anthropic

# ----------------------------------------------------------------------------
# 0) KONFIGURATION
# ----------------------------------------------------------------------------
SAMPLE_TAG   = "_b"                          # "" = erstes Sample, "_b" = zweites
MODEL        = "claude-sonnet-4-6"
INPUT_PATH   = f"control_sample_10k{SAMPLE_TAG}.csv"          # Output von 07_control_sample.R
PROMPT_FILE  = "coding_instructions.txt"                     # ORIGINAL-Prompt (mit ## Ausgabeformat / JSON-Array)
OUT_JSONL    = f"control_sample_10k{SAMPLE_TAG}_sonnet.jsonl"
OUT_CSV      = f"control_sample_10k{SAMPLE_TAG}_sonnet.csv"

ID_COL       = "cs_id"
SENT_COL     = "sentence"
BEFORE_COL   = "context_before"
AFTER_COL    = "context_after"
PASSTHROUGH  = ["sample_type", "speech_id", "sentence_nr", "party", "lp", "date", "is_procedural",
                "pred_code", "pred_score", "p505", "pred_bucket_A", "pred_domain_B", "pred_bucket_B"]

GROUP_SIZE   = 20            # Sätze pro Request
TEST_N       = None           # zum Testen klein lassen (40 = 2 Gruppen); None = alle
MAX_TOKENS   = 4096
POLL_SECONDS = 30

MARPOR_CODES = {
    "000","101","102","103","104","105","106","107","108","109","110",
    "201","202","203","204","301","302","303","304","305",
    "401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416",
    "501","502","503","504","505","506","507",
    "601","602","603","604","605","606","607","608",
    "701","702","703","704","705","706",
}

# ----------------------------------------------------------------------------
# 1) Checks + Prompt + Client
# ----------------------------------------------------------------------------
if not os.environ.get("ANTHROPIC_API_KEY"):
    sys.exit("FEHLER: ANTHROPIC_API_KEY ist nicht gesetzt.")
if not os.path.exists(PROMPT_FILE):
    sys.exit(f"FEHLER: Prompt-Datei '{PROMPT_FILE}' nicht gefunden.")
if not os.path.exists(INPUT_PATH):
    sys.exit(f"FEHLER: Input '{INPUT_PATH}' nicht gefunden (erst 07_control_sample.R mit passendem SAMPLE_TAG).")

with open(PROMPT_FILE, encoding="utf-8") as f:
    INSTRUCTIONS = f.read()
client = anthropic.Anthropic()

# ----------------------------------------------------------------------------
# 2) Robuste Parser-Helfer
# ----------------------------------------------------------------------------
def parse_json_array(text):
    """Findet das JSON-Array in der Modellantwort — auch mit ```-Fences, Prosa
    drumherum oder abgeschnittenem Ende. Gibt eine Liste von Objekten."""
    t = text.strip()
    if t.startswith("```"):
        t = re.sub(r"^```(?:json)?\s*", "", t)
        t = re.sub(r"\s*```$", "", t).strip()
    try:
        out = json.loads(t)
        if isinstance(out, list):
            return out
    except Exception:
        pass
    i, j = t.find("["), t.rfind("]")
    if 0 <= i < j:
        try:
            out = json.loads(t[i:j + 1])
            if isinstance(out, list):
                return out
        except Exception:
            pass
    objs, depth, start, in_str, esc = [], 0, None, False, False
    for k, ch in enumerate(t):
        if in_str:
            if esc: esc = False
            elif ch == "\\": esc = True
            elif ch == '"': in_str = False
            continue
        if ch == '"': in_str = True
        elif ch == "{":
            if depth == 0: start = k
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0 and start is not None:
                try: objs.append(json.loads(t[start:k + 1]))
                except Exception: pass
                start = None
    return objs

def add_usage(acc, u):
    acc["input"]          += getattr(u, "input_tokens", 0) or 0
    acc["output"]         += getattr(u, "output_tokens", 0) or 0
    acc["cache_read"]     += getattr(u, "cache_read_input_tokens", 0) or 0
    acc["cache_creation"] += getattr(u, "cache_creation_input_tokens", 0) or 0

# ----------------------------------------------------------------------------
# 3) Eine Batch-Runde
# ----------------------------------------------------------------------------
def run_batch(rows, usage, label="Lauf"):
    groups = [rows[i:i+GROUP_SIZE] for i in range(0, len(rows), GROUP_SIZE)]
    requests = []
    for gi, grp in enumerate(groups):
        payload = [{
            "gs_id": str(r[ID_COL]),
            "context_before": str(r.get(BEFORE_COL, "") or ""),
            "sentence":       str(r.get(SENT_COL, "") or ""),
            "context_after":  str(r.get(AFTER_COL, "") or ""),
        } for r in grp]
        user_text = ("Kodiere die folgenden bereits segmentierten Sätze. Gib ausschließlich das "
                     "JSON-Array zurück, ein Objekt pro Satz, in derselben Reihenfolge:\n\n"
                     + json.dumps(payload, ensure_ascii=False))
        requests.append({
            "custom_id": f"G{gi:05d}",
            "params": {
                "model": MODEL,
                "max_tokens": MAX_TOKENS,
                "system": [{"type": "text", "text": INSTRUCTIONS, "cache_control": {"type": "ephemeral"}}],
                "messages": [{"role": "user", "content": user_text}],
            },
        })

    batch = client.messages.batches.create(requests=requests)
    print(f"[{label}] Batch {batch.id}: {len(requests)} Gruppen à <= {GROUP_SIZE} Sätze")
    while True:
        b = client.messages.batches.retrieve(batch.id)
        c = b.request_counts
        print(f"  status={b.processing_status} succeeded={c.succeeded} errored={c.errored}")
        if b.processing_status == "ended":
            break
        time.sleep(POLL_SECONDS)

    coded, errors = {}, []
    for res in client.messages.batches.results(batch.id):
        if res.result.type != "succeeded":
            e = getattr(res.result, "error", None)
            inner = getattr(e, "error", None)
            emsg = getattr(inner, "message", None) or getattr(e, "message", None) or repr(e)
            errors.append((res.custom_id, res.result.type, emsg))
            continue
        msg = res.result.message
        add_usage(usage, msg.usage)
        text = "".join(blk.text for blk in msg.content if blk.type == "text")
        for obj in parse_json_array(text):
            gid = str(obj.get("gs_id", "")).strip()
            code = str(obj.get("marpor", "")).strip()
            if gid and code in MARPOR_CODES:
                coded[gid] = {
                    "marpor": code,
                    "fehltrennung": bool(obj.get("fehltrennung", False)),
                    "flag_unsure": bool(obj.get("flag_unsure", False)),
                    "confidence": str(obj.get("confidence", "")),
                    "note": str(obj.get("note", "")),
                }
    if errors:
        print(f"  [{label}] {len(errors)} Request(s) mit API-Fehler (type={errors[0][1]}). Meldung:")
        print("    ", str(errors[0][2])[:600])
    return coded

# ----------------------------------------------------------------------------
# 4) Sample lesen, kodieren (mit einem Retry für Lücken)
# ----------------------------------------------------------------------------
print(f"== SAMPLE_TAG = '{SAMPLE_TAG}'  ->  Input: {INPUT_PATH} ==")
df = pd.read_parquet(INPUT_PATH) if INPUT_PATH.endswith(".parquet") else pd.read_csv(INPUT_PATH)
for col in (ID_COL, SENT_COL):
    if col not in df.columns:
        sys.exit(f"FEHLER: Spalte '{col}' fehlt. Vorhanden: {list(df.columns)}")
if df[ID_COL].duplicated().any():
    sys.exit("FEHLER: cs_id enthält Duplikate.")
if TEST_N:
    df = df.head(TEST_N)
    print(f"** TESTMODUS: {TEST_N} Sätze. TEST_N=None für den vollen Lauf. **")

passthrough = [c for c in PASSTHROUGH if c in df.columns]
meta = {str(r[ID_COL]): {c: r[c] for c in passthrough} for _, r in df.iterrows()}
all_rows = df.to_dict("records")
usage = {"input": 0, "output": 0, "cache_read": 0, "cache_creation": 0}

coded = run_batch(all_rows, usage, label="Hauptlauf")
missing_rows = [r for r in all_rows if str(r[ID_COL]) not in coded]
if missing_rows:
    print(f"[Retry] {len(missing_rows)} Sätze fehlten/ungültig -> erneuter Versuch")
    coded.update(run_batch(missing_rows, usage, label="Retry"))

# ----------------------------------------------------------------------------
# 5) Schreiben (cs_id | Metadaten | BERT-Vorhersage | SONNET | status)
# ----------------------------------------------------------------------------
records = []
with open(OUT_JSONL, "w", encoding="utf-8") as jf:
    for r in all_rows:
        cid = str(r[ID_COL])
        rec = {ID_COL: cid, **meta.get(cid, {})}
        if cid in coded:
            rec.update(coded[cid]); rec["status"] = "ok"
        else:
            rec["status"] = "failed"
        records.append(rec)
        jf.write(json.dumps(rec, ensure_ascii=False, default=str) + "\n")

sonnet_cols = ["marpor", "fehltrennung", "flag_unsure", "confidence", "note"]
cols = [ID_COL] + passthrough + sonnet_cols + ["status"]
out = pd.DataFrame(records)
for c in cols:
    if c not in out.columns:
        out[c] = pd.NA
out[cols].to_csv(OUT_CSV, index=False, encoding="utf-8")

n_ok = sum(1 for r in records if r["status"] == "ok")
print("\n================ FERTIG ================")
print(f"erfolgreich : {n_ok} / {len(records)}")
print(f"fehlerhaft  : {len(records) - n_ok}")
print(f"\nToken-Verbrauch (Summe):")
print(f"  input (uncached)        : {usage['input']:,}")
print(f"  cache-read (10 % Preis)  : {usage['cache_read']:,}")
print(f"  cache-write (einmalig)   : {usage['cache_creation']:,}")
print(f"  output                   : {usage['output']:,}")
print(f"\ngeschrieben:\n  {OUT_CSV}\n  {OUT_JSONL}")
print("========================================")
