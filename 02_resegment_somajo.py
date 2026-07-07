"""
Saubere Satztrennung fuer GermaParl-Reden mit SoMaJo (deutsch-spezifisch).
Ersetzt die naive Regex-Trennung aus 01_build_speech_corpus.R (Schritt 6):
    strsplit(row$text, "(?<=[.!?])\\s+", perl = TRUE)
SoMaJo respektiert Abkuerzungen, Ordinalzahlen und Datumsangaben
("4. August", "z. B.", "Dr.", "Art. 5 Abs. 1") und trennt nur an echten Grenzen.

Input : CSV mit EINER Zeile pro Rede. Aus R exportieren:
        fwrite(all_speeches, "Data/all_speeches.csv")
        (Spalten: speech_id, legislative_period, speaker, party, date, text)
Output: CSV mit EINER Zeile pro Satz -- gleiches Schema wie bisher, sodass deine
        ManifestoBERTa-Inferenz unveraendert weiterlaeuft:
        speech_id, legislative_period, speaker, party, date,
        sentence_nr, sentence, context_before, context_after

Setup:  pip install somajo pandas
"""
import pandas as pd
from somajo import SoMaJo

IN_PATH  = "Data/all_speeches.csv"
OUT_PATH = "Data/sentences_for_classification.csv"

# "de_CMC" = deutsches Modell; split_camel_case=False schuetzt z.B. "BAfoeG".
# KOMPATIBILITAET: aeltere somajo-Versionen nahmen number_of_threads im Konstruktor;
# aktuelle (>=2.x, z.B. 2.4.3) erwarten parallel= in tokenize_text(). Beides abgedeckt:
N_THREADS = 4
try:
    tokenizer = SoMaJo("de_CMC", split_camel_case=False)
    _TOKENIZE_KW = {"parallel": N_THREADS}
except TypeError:  # sehr alte API
    tokenizer = SoMaJo("de_CMC", split_camel_case=False, number_of_threads=N_THREADS)
    _TOKENIZE_KW = {}


def split_sentences(text) -> list:
    """Eine Rede -> Liste sauber getrennter Saetze."""
    out = []
    try:
        sents = tokenizer.tokenize_text([str(text)], **_TOKENIZE_KW)
    except TypeError:            # tokenize_text ohne parallel-Support
        sents = tokenizer.tokenize_text([str(text)])
    for sent in sents:
        s = "".join(tok.text + (" " if tok.space_after else "") for tok in sent).strip()
        if any(ch.isalpha() for ch in s):     # leere / reine Satzzeichen-Fragmente verwerfen
            out.append(s)
    return out


def main():
    df = pd.read_csv(IN_PATH)
    rows = []
    for r in df.itertuples(index=False):
        sents = split_sentences(r.text)
        n = len(sents)
        for j, s in enumerate(sents):
            rows.append({
                "speech_id":          r.speech_id,
                "legislative_period": r.legislative_period,
                "speaker":            r.speaker,
                "party":              r.party,
                "date":               r.date,
                "sentence_nr":        j + 1,
                "sentence":           s,
                "context_before":     sents[j - 1] if j > 0 else None,
                "context_after":      sents[j + 1] if j < n - 1 else None,
            })
    out = pd.DataFrame(rows)
    out.to_csv(OUT_PATH, index=False)
    print(f"{len(out)} Saetze aus {len(df)} Reden -> {OUT_PATH}")


if __name__ == "__main__":
    main()
