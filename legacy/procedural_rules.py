# -*- coding: utf-8 -*-
"""
procedural_rules.py — Normalisierung + (deaktivierte) Regelschicht des Prozedursatz-Filters.

v2 (EXACT-ONLY, Versionssprung ggue. v1):
  Der aktive Filter (flag_procedural.py) ruft `rule_layer_class` NICHT mehr auf.
  `is_procedural` basiert ab v2 ausschliesslich auf der handtypisierten Top-500-Liste
  (Layer 1, proc_source="exact"). Die Regelschicht unten (Layer 2) ist BEWUSST ERHALTEN,
  aber DORMANT — sie dient nur noch als reproduzierbarer "exact+rule"-Robustness-Arm
  (v1-Verhalten), nicht als Primary. `normalize` bleibt der kanonische, mit der
  Kandidatengenerierung (build_procedural_candidates.py) identische Normalisierer und
  wird von flag_procedural.py weiterhin importiert.

  Begruendung des Versionssprungs: exact-only haelt den Filter voll pre-committed/transparent
  (keine forscher-definierte Regex-Generalisierung im Long-Tail); der nicht-codierbare
  Long-Tail wird dem datengetriebenen gbert-000-Filter ueberlassen.

Architektur (urspruenglich zweistufig, v2 nur noch Stufe 1 aktiv):
  1. EXAKTE LISTE: Top-500-Frequenzformen, von Hand typologisiert
     (procedural_filter_decisions_v1.csv; Pre-Commitment: formbasiert, vor jeder
     Effekt-Inspektion entschieden).  -> AKTIV (Primary)
  2. REGELSCHICHT (dieses Modul): faengt Namens-/Kombinationsvarianten derselben
     Templates im Long-Tail.  -> DORMANT in v2 (nur Robustness-Arm).

Segment-Logik (Regelschicht): Der normalisierte Satz wird an , ; : und " - " in Segmente
geteilt. Ein Satz gilt als prozedural, wenn JEDES Segment einer prozeduralen Klasse angehoert
(oder ein neutraler Fuellpartikel ist) UND mindestens ein Segment eine echte prozedurale
Klasse traegt. Full-Match pro Segment — niemals Praefix-Matching.

VERSION: v2 (2026-06-18, exact-only). v1 (2026-06-12) = exact+rule.
"""
import re

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
