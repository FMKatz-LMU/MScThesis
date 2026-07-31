# Verifikationsbericht zur eingearbeiteten Fassung

**Geprüft:** `MasterThesis_FKatzmann12293439_1.docx` (die von dir in Word geöffnete und gespeicherte Fassung meiner Lieferung)
**Ergebnis:** `MasterThesis_FKatzmann12293439_geprueft.docx` — enthält **elf** Korrekturen aus dieser Prüfrunde
**Datum:** 31.07.2026

---

## 1. Was die Prüfung ergeben hat

| Prüfung | Ergebnis |
|---|---|
| Textstand gegenüber meiner Lieferung | **blockweise identisch** — Word hat nichts am Wortlaut geändert |
| 155 Textänderungen (149 Einbau + 6 aus dieser Runde) | alle vorhanden, kein alter Wortlaut übrig geblieben |
| Alte Zahlen (0.0667, −0.0181, 0.0344, 0.0050, 0.0164, 0.0252, +0.0160, 0.113, +0.544, 0.2807, 0.1785, 41.8 %, 0.0253, 0.0457, 49.8 % in F.3 …) | **keine Reste** — die verbliebenen Ziffernkollisionen liegen alle in Anhang F.2 und F.10 und bezeichnen andere Größen |
| Alte Formulierungen („nearly twice", „U-shape", „within-domain JSD", „intervention matrix", „right-coded", „M1 assigns", „607/608", „placebo", „silver standard", „1000-draw", „reduced simplex", „artifact") | **keine Reste** |
| Abschnittsverweise | alle 37 lösen auf |
| Anhangverweise | alle lösen auf |
| Hyperlinks | **255 Stück, alle Ziele existieren.** Word hat 26 davon von der Element- in die Feldcode-Form umgeschrieben und dabei Tooltips mit der vollen Literaturangabe ergänzt — kein Verlust, nur eine andere Darstellung |
| Beide Gleichungen in 4.7 | vorhanden, Struktur unverändert (Σ mit echtem Subskript, ∈ U+2208, Mᵀ, ausgeschriebener Nenner; JSD/KL/with aufrecht, ∥ U+2225, beide in `oMathPara`) |
| Inhaltsverzeichnis | **von Word aktualisiert**, 5.6 ist drin (71 statt 70 Einträge) |
| Zwölf Abbildungs- und Tabellenunterschriften, Abschnitt 5.6, drei Fußnoten | vorhanden und korrekt |
| Table 4.1, Table 5.2, Table F.4.2, Table F.10.2, F.8-Überschrift, F.8.1-Unterschrift | alle Zellen wie vorgesehen |
| Datei-Integrität | ZIP und XML gültig, Testrendering 150 Seiten |

### Unabhängig nachgerechnet (nicht nur gegen die Berichte abgeglichen)

| Größe | Quelle | Ergebnis |
|---|---|---|
| Table 5.2, alle 40 Werte | `H3_trend_fits.csv`, `H3_trend_fits_exafd.csv`, `H3_trend_exafd_compare.csv` | exakt |
| Vier H2c-Niveaupaare, „31 von 32", Europa-LP-13-Ausnahme, alle LP-19/20-Werte | `H2c_dispersion_table.csv`, alle 32 Paare durchgerechnet | exakt |
| Pre/Post-2017, acht Werte | `H3_pre_post_2017.csv` | exakt |
| Migrationsdispersion 0.104 → 0.221, gewichtetes Mittel +0.554 → +0.344, „about two thirds" (0,659) | `H3_system_polarization.csv`, `H2c_dispersion_table.csv` | exakt |
| Fensterdifferenzen −0.22 / −0.12 / +0.07 / −0.01 / −0.54 | Eigenrechnung auf `H3_system_polarization.csv` | exakt |
| Tabelle 5.1, alle 27 Werte | `H2a_per_bucket_summary.csv` (am 31.07. neu geschrieben) | **unverändert**, exakt |
| DPS 14.9 → 20.2 %, Foreign Policy 7.4 → 12.0 %, „alle sechs steigen" | `signed_divergence_A.csv` (neu geschrieben) | exakt, beide Buckets steigen bei allen sechs Parteien |
| Welfare-Anteile 31.9 / 24.0 / 22.6 / 12.4 / 9.7, Linke Rede 20.9, Band 12.7–20.9 %, CDU/CSU Law & Order 14.8 → 8.1 | ebd. | exakt |
| „Europe is the least salient of the four domains" | Redeanteile 3.20 % gegen Migration 4.49 % | trifft zu |
| H2b: vier Slopes, R², n = 38 / 37, Intercept +0.40, acht Kanalmittel und zwei SD | `H2b_regression_fits.csv`, `H2b_polarization_table.csv` (neu geschrieben) | **unverändert**, exakt — Migration +0.36 gilt auf den 37 gematchten Zellen |
| Sechs Migrationstrajektorien, AfD „above the entire previous range" | ebd. | exakt (0.685 gegen bisheriges Maximum 0.653) |
| Agenda centre of gravity, beide Varianten | Nachbau der JSD-Matrix aus `speech_soft.rds` und `manifesto_dists.rds` | 42-Zellen-Variante reproduziert die alte Abbildung, 38-Zellen-Variante die neuen Textwerte |

---

## 2. Die elf Korrekturen dieser Runde

| # | Stelle | Was war | Was jetzt | Warum |
|---|---|---|---|---|
| **V0** | **4.7.2, Überschrift** | Word hatte der Überschrift „4.7.2 Speech-Side Aggregation and the Primary Specification" die **Formatvorlage Überschrift 3 entzogen** — sie stand als Fließtext im Dokument | Formatvorlage wiederhergestellt | Beim Öffnen in Word verloren. In meiner Lieferung war sie korrekt gesetzt, in der zurückgespielten Datei nicht mehr. Ohne die Vorlage fällt der Abschnitt aus Gliederung und Verzeichnis. **Das war der ernsteste Fund.** |
| **V1** | 4.8.1, H1b | „…is computed on this layer. Code 305 requires no treatment on this layer" | „…is computed for it. Code 305 requires no treatment on this layer" | „on this layer" stand zweimal in aufeinanderfolgenden Sätzen |
| **V2** | 5.2.1 | „…sits above the third quartile of its own period, **and** all 38 above …, **and** 13 (34.2%)…" | „…of its own period, all 38 lie above …, and 13 (34.2%)…" | drei „and" hintereinander durch meine C59-Korrektur |
| **V3** | 5.2.1 | „but only **57%** of the distance between manifestos" | „but only **0.57 times** the distance between manifestos" | K5-C23 hätte hier als einzige von fünf Stellen eine Prozentangabe erzeugt; Introduction, 5.6, Conclusion und Anhang F.5 schreiben alle „0.57 times". Die sperrige Wendung „0.57 **of** the distance", die der Befund beanstandet, ist trotzdem weg |
| **V4** | Unterschrift Abbildung 5.2 | „Manifesto-speech" | „Manifesto–speech" | der Halbgeviert-Sweep hatte die großgeschriebene Variante am Satzanfang nicht erfasst |
| **V5** | 5.4, Europa | „Refitting without the AfD **turns both trends flat**, … the speech slope falling to 0.0113" | „…**removes most of both trends**, … the speech slope falling **from 0.0407 to** 0.0113" | Folge der Neurechnung: der Rede-Slope ohne AfD ist jetzt +0.0113 (p = 0.011), nicht mehr +0.0050 (p = 0.090). „flat" trägt bei diesem Wert nicht mehr |
| **V6** | 5.6 | „at 0.57 **of** that distance" | „at 0.57 **times** that distance" | siehe V3 |
| **V7** | Introduction | „**flattens** the speech trend **to near zero**" | „**cuts** the speech trend **to roughly a quarter of its size**" | siehe V5; 0.0113 / 0.0407 = 0,28 |
| **V8** | Conclusion | „drops the speech slope **to near zero**" | „cuts the speech slope **to roughly a quarter of its size**" | siehe V5 |
| **V9** | Conclusion | „…they converge sharply on one another. **The established parties do not move apart on this domain**, so the rise is composition…" | „…they converge sharply on one another. **The rise is therefore** composition…" | meine beiden K5-A6-Ersetzungen in der Conclusion hatten denselben Halbsatz zweimal in Folge erzeugt |
| **V10** | Table 4.1, Zeile H1b, Specification | „full filter × 305 incl × **native τ**, residual excluded, renormalised" | „…× **native decoding temperature**, residual excluded, renormalised" | die Zeile H1a war durch 48-B9b auf „decoding temperature" umgestellt, H1b nicht — innerhalb derselben Spalte inkonsistent |

**V5, V7 und V8 sind inhaltlich die wichtigsten:** die Neurechnung hat den Europa-Rede-Slope ohne AfD von +0.0050 auf +0.0113 gehoben, und drei Stellen im Dokument beschrieben ihn weiterhin als „flach" bzw. „nahe null". Der qualitative Befund bleibt (die Ausschlussrechnung nimmt gut 70 % des Trends heraus), nur die Wortwahl musste mit.

---

## 3. Was ich bewusst so gelassen habe

- **„native τ = 1" in 4.7.2 und 5.5** — τ ist dort Formelbezeichner, nicht Achsenname. Die Terminologieregel der Berichte nimmt das ausdrücklich aus.
- **„barely move" in 5.2.1, 5.2.2 und der Unterschrift zu F.8.1** — andere Bedeutung („barely moves the median", „the level barely moves"), nicht der von K5-A6 beanstandete Befund.
- **„0.57" in Kapitel 4.4 und 4.5** — andere Größen (Osnabrügge-Accuracy, κ), reine Ziffernkollision.
- **26 Feldcode-Hyperlinks** — Word-eigene Darstellung, alle Ziele vorhanden.

---

## 4. Offen bleibt weiterhin

Unverändert gegenüber der Restliste: Abbildungen 5.3, 5.9, 5.10, F.9.3 und F.9.4 neu rendern, `18_agenda_centre_of_gravity.R` mit dem Patch laufen lassen, abgeschnittene Untertitel in vier Abbildungen im R-Code beheben.

**Neu hinzugekommen:** Das Inhaltsverzeichnis hat Word bereits aktualisiert. Nach dem Einsetzen der neuen Abbildungen bitte **einmal F9** darüberlaufen lassen, damit die Seitenzahlen wieder stimmen — und danach kurz prüfen, ob „4.7.2 Speech-Side Aggregation and the Primary Specification" weiterhin als Überschrift 3 formatiert ist.
