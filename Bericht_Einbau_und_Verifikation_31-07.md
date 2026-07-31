# Bericht: Einbau der drei Prüfberichte und Verifikation

## 1. Kopf

| | |
|---|---|
| **Vorgang** | Zusammenführung und Einbau der Prüfberichte zu 4.7, 4.8 und Kapitel 5 |
| **Ausgangsstand** | `MasterThesis_FKatzmann12293439_linked.docx`, 31.07.2026, unverändert |
| **Endstand** | **`MasterThesis_FKatzmann12293439_geprueft.docx`** |
| **Datum** | 31.07.2026 |
| **Befund-ID-Präfix dieses Berichts** | `EB` (Einbaubefunde), `V` (Korrekturen der Verifikationsrunde) |
| **Zugehörige Dokumente** | `Aenderungsprotokoll_31-07.md` (149 Einzeländerungen in Dokumentreihenfolge, alt/neu/Herkunft) · `Restliste_31-07.md` · `Verifikationsbericht_31-07.md` · `patch_18_agenda_centre_of_gravity.md` |

---

## 2. Für die Zusammenführung mit weiteren Prüfungen — das Wichtigste zuerst

**Der Dokumentstand hat sich geändert.** Jede weitere Prüfung muss gegen `MasterThesis_FKatzmann12293439_geprueft.docx` geschrieben werden. Prüfberichte, die gegen den Stand vom 31.07. früh geschrieben sind, treffen an den unten genannten Stellen nicht mehr wörtlich.

**Zwei Entscheidungen sind gefallen und sollten nicht neu verhandelt werden:**

1. **Analysebasis H2c/H3** — die Restriktion auf die 38 validen Zellen ist nachgezogen, die korrigierten `15_H2.R`/`16_H3.R` sind am 31.07. gelaufen. `n_parties` steht in LP 13–15 auf 4. Sämtliche Zahlen in 5.3.3, 5.4, Table 5.2, Fußnote 17, Introduction und Conclusion stehen auf dem neuen Lauf. Die Vorabrechnung des 4.8-Berichts ist über alle 40 Werte von Table 5.2 bestätigt worden (Härtetests: Europa/Manifest **+0.0769**, `slope_delta` **+0.0848**).
2. **Agenda centre of gravity** — Fließtext 5.2.1, Tabelle F.4.2 und Conclusion stehen auf der 38-Zellen-Rechnung (FDP 0.0257 · SPD 0.0379 · Grüne 0.0396 · CDU/CSU 0.0463 · Linke 0.0524 · AfD 0.0638). Die Werte sind unabhängig aus `speech_soft.rds` und `manifesto_dists.rds` nachgerechnet. **Abbildung 5.3 zeigt noch die alte 42-Zellen-Rechnung.**

---

## 3. Was eingebaut ist

| Bereich | Umfang |
|---|---|
| **Kapitel 4.7** | vollständig ersetzt durch die geprüfte Neufassung (4.7.1–4.7.4). Überschriften 4.7.3 und 4.7.4 neu, beide Gleichungen als OMML, neun Zitat- und Anhanganker wiederhergestellt, dazu ein neues Lesezeichen `_F.8_Migration_re-split` |
| **Kapitel 2.5, 4.3.1, 4.5.3, 4.6.2** | Hypothesenformulierungen H1a und H1b, Gewichtsaussage unter der Primärspezifikation, F1-Spanne, 305-Begründung auf der B-Ebene |
| **Kapitel 4.8** | 28 Änderungen inklusive dreier Blockersätze (H2c-Absatz, Abou-Chadi-Absatz, H2b-Deklaration) und fünf Zellen in Table 4.1 |
| **Kapitel 5** | 84 Änderungen, drei Absatzersetzungen (K5-A5, K5-A6, H1b-Vorspann), zwölf fehlende Abbildungs- und Tabellenunterschriften, Table 5.2 komplett neu, neuer Abschnitt **5.6 Summary of Findings** |
| **Conclusion** | vier Stellen (0.0257, zweimal K5-A6, K5-A7) |
| **Anhänge** | E (Schema-B-Beschreibung), F-Einleitung (Seed), F.3 (49.7 %), F.4 (Baseline-Herleitung, Tabelle F.4.2, Unterschrift), F.8 (Überschrift, F.8.1-Unterschrift), F.10.2 (Spaltenkopf) |
| **Fußnoten** | 14, 15, 17 |
| **Dokumentweite Sweeps** | Halbgeviertstriche, Bucket-Namen mit „&", polarisation/artefact/towards, manifesto–speech, Tausendertrennzeichen, abschließende Leerzeichen. Sperrzonen (Literaturverzeichnis, Anhang B, Anhang D, Erklärungen, MARPOR-Kategorienamen in Anhang E) unangetastet |

Gesamt: **155 Textänderungen**, 69 geänderte Bereiche gegen die Basisdatei, jede Abweichung einem Befund oder einem Sweep zuordenbar.

---

## 4. Befunde dieses Durchgangs, die in keinem der drei Berichte standen

| ID | Stelle | Befund | Status |
|---|---|---|---|
| **EB-1** | Introduction | trug denselben von K5-A6 widerlegten Halbsatz („as the established parties barely move") **und** die alten Europa-Slopes (+0.0667 → −0.0181) | eingebaut |
| **EB-2** | Conclusion, dritte Fundstelle | „since there the established parties barely move at all" — dritte Instanz desselben Befunds | eingebaut |
| **EB-3** | 5.5, Arm 6 | „localises the European rise to the party's entry" — dieselbe Überdehnung, die 48-A5 in 4.8.3 und 5.4 korrigiert | eingebaut |
| **EB-4** | 4.7.4 (Neufassung) | die gelieferte Neufassung trug den von 48-B7b beanstandeten Wortlaut zur Seed-Angabe noch | in der Neufassung nachgezogen |
| **EB-5** | Fußnote 17 | nannte für den Europa-Manifest-Slope **+0.0722**; der tatsächliche Wert der restringierten Rechnung ist **+0.0769**. Der Vorabwert war falsch | Fußnote neu gefasst |
| **EB-6** | 5.4, Migrationsdispersion | „nearly doubles, from 0.113 to 0.221" wird durch die Neurechnung zu 0.104 → 0.221, also Faktor 2,12 | „more than doubles" |
| **EB-7** | Introduction, 5.4, Conclusion | der Europa-Rede-Slope ohne AfD steigt durch die Neurechnung von +0.0050 (p = 0.090) auf **+0.0113 (p = 0.011)**. Drei Stellen beschrieben ihn weiterhin als „flat" bzw. „near zero" | auf „removes most of both trends" bzw. „cuts … to roughly a quarter of its size" umgestellt |
| **EB-8** | 4.7.2, Überschrift | **Word entzieht der Überschrift beim Öffnen die Formatvorlage Überschrift 3.** In der ausgelieferten Datei war sie gesetzt, in der zurückgespielten nicht mehr — der Abschnitt fällt damit aus Gliederung und Inhaltsverzeichnis | wiederhergestellt; **nach jedem weiteren Word-Durchgang nachsehen** |

Dazu vier Fehler aus dem Einbau selbst, alle in der Verifikationsrunde behoben: eine Satzdopplung in der Conclusion (V9), eine Wortwiederholung in 4.8.1 (V1), ein dreifaches „and" in 5.2.1 (V2) und drei Konsistenzstellen (V3/V6 zur 0.57-Schreibweise, V4 Halbgeviertstrich in einer Unterschrift, V10 „native τ" in Table 4.1).

---

## 5. Nicht-anfassen-Liste (konsolidiert, verifiziert)

Aus allen drei Berichten und der eigenen Nachrechnung geprüft und **korrekt**. Ein Bericht, der hier etwas „korrigiert", korrigiert etwas Richtiges kaputt.

- **n = 38** und die Zusammensetzung 8/8/8/7/5/2; Zellgrößen 41.562 / 106.641 / 279.947.
- **Tabelle 5.1 vollständig** (27 Werte) und die Anteilsaussagen in 5.3.1 — gegen die am 31.07. neu geschriebenen `H2a_per_bucket_summary.csv` und `signed_divergence_A.csv` nachgerechnet, **unverändert**.
- **Alle H2b-Werte**: vier Slopes, R² 0.53–0.78, n = 38 / 37, Intercept +0.40, acht Kanalmittel, sechs Migrationstrajektorien — ebenfalls neu nachgerechnet, unverändert.
- **„31 von 32 Domänen-Perioden-Paaren"**, Europa-LP-13-Ausnahme, alle LP-19/20-Werte, „about two thirds" (jetzt 0,659), Spalte *Δ slope* in Table 5.2.
- **Referenzskala** 0.0074 / 0.0520 / 0.0297, Faktoren 7 / 4 / 0.57, Nearest-Manifesto 42.1 % / 21.1 % / 1.87 / 2.95 / 18 von 38.
- **κ(F) = 0.363, κ(B) = 0.456**, alle sechs ρ in Tabelle 5.3 samt ✓/✗-Setzung.
- **4.6.2 und F.1: 49,8 %** ist korrekt (F.3 stand auf einem Rundungsfehler und ist jetzt 49,7 %).
- **4.5.3** (random gegen systematic), **5.2.2** („the mean JSD across the B buckets"), **3.3**, **4.9** (Akzeptanzkriterien, Tabelle 4.2), **2.5** H2b und die H2b/H2c-Abgrenzung.
- **„native τ = 1"** in 4.7.2 und 5.5 — Formelbezeichner, von der Terminologieregel ausgenommen.
- Anhang B, Anhang D, Literaturverzeichnis, Erklärungen, MARPOR-Kategorienamen in Anhang E.

---

## 6. Anker, die nicht mehr treffen

Für die Zusammenführung mit älteren Prüfungen: diese Formulierungen des Ausgangsstands existieren nicht mehr. Wer sie zitiert, prüft gegen eine überholte Fassung.

| nicht mehr im Dokument | jetzt |
|---|---|
| „The Measure: Jensen-Shannon Divergence" · „Uncertainty: Sentence Bootstrap" | „The Jensen–Shannon Divergence" · „The Sentence Bootstrap" |
| der gesamte Wortlaut von 4.7.1 bis 4.7.4 | Neufassung |
| „the within-domain JSD on scheme B" | „the per-cell divergence across the nine directional sub-buckets…" |
| „the intervention matrix of Section 4.6" | „the intervention ladder of Section 4.6…" |
| „right-coded pole" / „right-leaning pole" | „positive pole" / „negative pole" |
| „M1 assigns each legislative period…" | Label M1 gestrichen |
| „607/608 re-split" | „608/607 re-split" |
| „the most reliably identified axis" · „the most robust label of the scheme" · „the label the classifier assigns least reliably" | abgeschwächt bzw. ersetzt |
| „all of them with acceptance criteria fixed before the analysis" · „a placebo that shows the measure moves at all" | neu gefasst |
| „nearly twice as often" · „nearly doubles" · „the same rough U-shape" · „These same four cells" | neu gefasst |
| „every established party except the CDU/CSU" · „the established parties barely move" (3×) | neu gefasst |
| 0.0667 · −0.0181 · 0.0344 · 0.0050 · 0.0164 · 0.0252 · +0.0160 · +0.0112 · 0.113 · +0.544 · 0.2807 · 0.1785 · 41.8 % · 0.0253 · 0.0457 · 49.8 % (nur in F.3) | durch die Werte des Laufs vom 31.07. ersetzt |
| „Figure 5.1" … „Figure 5.10", „Table 5.2", „Table 5.3" als nackte Unterschrift | vollständige Unterschriften |
| Kapitel 5 endet mit 5.5 | endet mit **5.6 Summary of Findings** |

---

## 7. Offene Punkte

| # | Was | Wo |
|---|---|---|
| 1 | **Abbildungen 5.3, 5.9, 5.10, F.9.3, F.9.4 neu rendern** — sie zeigen die Reihen vor der Restriktion bzw. die 42-Zellen-Rechnung | R-Code |
| 2 | `18_agenda_centre_of_gravity.R` mit dem beiliegenden Patch laufen lassen | R-Code |
| 3 | Abgeschnittene Untertitel in vier Abbildungen (`14_H1.R`, `15_H2.R`, `16_H3.R`, `18_…R`) | R-Code |
| 4 | „polarization" im Innentext der Abbildungen | R-Code |
| 5 | **Arm 4, Rangkorrelation ρ** — kein Skript-PART, keine Ergebnisdatei. Die unbelegte Behauptung ist aus 5.5 entfernt statt mit einer Zahl gefüllt worden. Wenn der Lauf nachgeholt wird, kann der Satz wieder eine Zahl bekommen | R-Code, dann 5.5 |
| 6 | Nach dem Einsetzen der Abbildungen **einmal F9** über das Inhaltsverzeichnis; dabei **EB-8 kontrollieren** | Word |

---

## 8. Prüfprotokoll

| Prüfung | Methode | Ergebnis |
|---|---|---|
| Ankerverifikation vor dem Einbau | alle Originalzitate der drei Berichte normalisiert im Dokument gesucht | **148 von 148** eindeutig, keiner ins Leere |
| Einbau | 155 Ersetzungen, run-übergreifend, hyperlink-sicher | **155 von 155** angewandt, kein Hyperlink zerschnitten |
| Blockersätze | Belege vor dem Ersatz ausgelesen und danach wiederhergestellt | `Ref_Gentzkow_2019`, `Ref_AbouChadi_2020` intakt |
| Zahlen | Table 5.2 (40), H2c über alle 32 Paare, Pre/Post (8), Tabelle 5.1 (27), signed divergence, H2b (Slopes, R², Kanalmittel, Trajektorien), Centre of Gravity in zwei Zellbasen — gegen CSVs und Caches **neu gerechnet**, nicht nur abgeglichen | alle exakt |
| Wiederholte Zahlen | 0.0297, 0.0510, 41.562/106.641/279.947, n = 38, κ(F), κ(B), 20.9 %, 42.1 %, 21.1 %, 1.87, 2.95, 0.0074, 0.0520 | dokumentweit übereinstimmend |
| Alte Zahlen und Formulierungen | 48 Suchmuster über Fließtext, Tabellen und Fußnoten | keine Reste; verbliebene Ziffernkollisionen liegen in F.2 und F.10 und bezeichnen andere Größen |
| Querverweise | 37 Abschnitts- und alle Anhangverweise gegen die Überschriftenstruktur | alle lösen auf |
| Hyperlinks und Lesezeichen | Anker gegen Lesezeichen, Element- und Feldcode-Form | **255 Links, alle Ziele vorhanden** |
| Formelsatz | OMML beider Gleichungen strukturell gelesen | Σ mit echtem Subskript, ∈ U+2208, Mᵀ, ausgeschriebener Nenner; JSD/KL/with aufrecht, ∥ U+2225; beide in `oMathPara` |
| Formatvorlagen | alle 780 Blöcke vor und nach der Word-Speicherung verglichen | eine Abweichung → **EB-8** |
| Diff gegen die Basisdatei | blockweise, dann wortweise für jede Abweichung | 69 Bereiche, 135 Blöcke, keine unbeabsichtigte Änderung |
| Datei-Integrität | ZIP, XML, Neuöffnen, Testrendering | gültig, 150 Seiten |

**Nicht geprüft:** die Klassifikationsstufe selbst und die Satzsegmentierung (gerechnet wurde ab den Aggregations-Caches), die Literaturangaben gegen die Originalquellen, und die Abbildungen als Bilder.
