# Restliste — was offen bleibt und warum

**Stand:** 31.07.2026 · gehört zu `MasterThesis_FKatzmann12293439_eingearbeitet.docx`

---

## 1. Muss noch von dir gemacht werden (im Dokument)

| # | Was | Warum |
|---|---|---|
| **1.1** | **Inhaltsverzeichnis aktualisieren** (in Word: Feld markieren → F9, oder Rechtsklick → *Feld aktualisieren*) | Die beiden geänderten 4.7-Überschriften habe ich im zwischengespeicherten Verzeichnistext direkt korrigiert, aber der **neue Abschnitt 5.6** fehlt dort noch und sämtliche Seitenzahlen haben sich verschoben. Ein Feldupdate erledigt beides. |
| **1.2** | **Abbildung 5.3 neu einsetzen**, nachdem `18_agenda_centre_of_gravity.R` mit dem beiliegenden Patch gelaufen ist | Fließtext, Tabelle F.4.2 und Conclusion stehen jetzt auf den 38-Zellen-Werten (E2, Variante A). Die eingebettete Grafik zeigt noch die 42-Zellen-Rechnung vom 22.07. |
| **1.3** | **Abbildungen 5.9 und 5.10 neu einsetzen** | Sie zeigen die Reihen vor der H2c/H3-Restriktion. Die Zahlen im Text sind nachgezogen, die Grafiken noch nicht. Dasselbe gilt für **F.9.3** und **F.9.4**. |

---

## 2. Muss im R-Code gemacht werden (nicht im Dokument möglich)

| # | Was | Wo |
|---|---|---|
| **2.1** | `18_agenda_centre_of_gravity.R` auf die 38 validen Zellen einschränken | Patch liegt bei: `patch_18_agenda_centre_of_gravity.md`. Erwartetes Ergebnis: FDP 0.0257 · SPD 0.0379 · Grüne 0.0396 · CDU/CSU 0.0463 · Linke 0.0524 · AfD 0.0638 (n = 12). Diese sechs Werte habe ich unabhängig aus `speech_soft.rds` und `manifesto_dists.rds` nachgerechnet, sie stehen so im Dokument. |
| **2.2** | **Abgeschnittener Text in vier Abbildungen** (K5-B21): Abb. 5.2 „…procedural + null-class sentences **remov**", Abb. 5.3 Panel B „points = party manifesto **age**", Abb. 5.9 „AfD enters the **Bund**" (alle vier Panels), Abb. 5.10 „…manifesto **char**" und „κ(B) = **0**" | `14_H1.R` (`p_h1a`, subtitle), `18_agenda_centre_of_gravity.R` (`pB`, subtitle), `15_H2.R` (`p_h2c`, annotate), `16_H3.R` (`p_exafd`, subtitle und caption) — Strings durch `str_wrap(..., 100)` schicken oder `ggsave(width = …)` erhöhen |
| **2.3** | „polarization" → „polarisation" **in den Grafiken** | Der Innentext der Abbildungen ist aus dem Dokument nicht änderbar. Betroffen: `15_H2.R` (`p_h2b`, subtitle), `16_H3.R`. Abbildung 5.7 mischt beides innerhalb einer Grafik (Untertitel „Polarization score", Achsen „Manifesto polarisation"). Variablennamen im Code (`polarization_score`, `dalton_polarization`, die CSV-Namen) bleiben unverändert. |
| **2.4** | Caption von `H3_trend_exafd` | Trägt laut 4.8-Bericht dieselbe Überdehnung wie 48-A5 („isolates whether the descriptive time trend is an AfD-entry artefact"). In der Skriptfassung vom 31.07. laut Bericht bereits nachgezogen — bitte gegenprüfen, bevor die Abbildung neu gerendert wird. |

---

## 3. Bewusst nicht umgesetzt

| # | Befund | Warum nicht |
|---|---|---|
| **3.1** | **K5-B19 · Arm 4, Rangkorrelation ρ** | Es gibt keine Ergebnisdatei und keinen Skript-PART dafür; `TIGHT_FILTER` in `00_config.R` wird von keinem Skript gelesen. Statt eine unbelegte Zahl oder ein „near-perfect" stehen zu lassen, habe ich die ρ-Behauptung **gestrichen**: „Arm 4 … was specified as a sensitivity check on H2a. It is subsumed by the full filter, which already removes the sentences it targets, and is therefore not reported separately." Wenn du den Lauf nachholst, kann der Satz wieder eine Zahl bekommen. |
| **3.2** | **K5-B17, stärkere Variante** · Arm 1 auf den Positionsscore anwenden | Optionaler Zusatzlauf (`polarization_score()` über die drei τ-Slices, dann Spearman über die 38 Zellen je Domäne). Der eingebaute Ersatztext kommt ohne aus: er nimmt die Abdeckungsbehauptung zurück, statt sie zu belegen. |
| **3.3** | **K5-A9, Alternative** · τ = 2 in Tabelle 4.2 als Placebo vorab deklarieren | Bewusst **nicht**. Der Ersatztext in 5.5 kennzeichnet die Placebo-Lesart als nachträglich; die Prä-Registrierung in 4.9 bleibt unangetastet. Das ist der ehrlichere Weg und die Empfehlung des Berichts. |
| **3.4** | **48-A1, Variante B** (Absatz in 4.8 ergänzen) | Entfällt, weil Variante A gewählt wurde: der Eröffnungssatz von 4.8 ist jetzt schlicht wahr und brauchte keine Änderung. |
| **3.5** | **Anhang F.9.4, Caption** („of the same scores") | Wird unter Variante A wahr und bleibt daher unverändert — Panel A und Panel B laufen jetzt beide auf der 38-Zellen-Basis. |
| **3.6** | **„native temperature" → „decoding temperature" dokumentweit** | Nur an den Stellen umgestellt, an denen ohnehin geändert wurde (4.7 komplett, 5.1, Table 4.1). Ein Blanko-Sweep über die restlichen acht Vorkommen hätte „native decoding temperature" achtmal im selben Absatzumfeld erzeugt; die Terminologieregel der Berichte ist „bei erster Nennung je Kapitel", und die ist erfüllt. |
| **3.7** | **MARPOR-Kategorienamen in Anhang E** | „Law and Order: Positive" (Code 605) ist ein Handbook-4-Label und wurde vom „&"-Sweep ausgenommen. |

---

## 4. Über die Berichte hinaus geändert — zur Kenntnis

Diese vier Stellen standen in keinem der drei Berichte. Ich habe sie mitgenommen, weil sie sonst nach dem Einbau einen frischen Widerspruch im Dokument erzeugt hätten. Alle sind im Änderungsprotokoll einzeln aufgeführt und rücknehmbar.

| # | Stelle | Was |
|---|---|---|
| **4.1** | **Introduction** (Block 6) | trug denselben von K5-A6 widerlegten Halbsatz („as the established parties barely move") **und** die alten Europa-Slopes (+0.0667 → −0.0181). Beides nachgezogen. Die Introduction lag außerhalb aller drei Prüfaufträge. |
| **4.2** | **Conclusion**, dritte Fundstelle | „since there the established parties barely move at all" — dritte Instanz desselben Befunds, die der Kapitel-5-Bericht nicht mitaufgeführt hatte. |
| **4.3** | **5.5, Arm 6** | „where it localises the European rise to the party's entry" → „to the party's own position and weight" — dieselbe Überdehnung, die 48-A5 in 4.8.3 und in 5.4 korrigiert. |
| **4.4** | **5.3.2, Formelzeile** | K5-C10 hätte „(right-wing direction − left-wing direction)" gesetzt, 48-B10 verwirft die Links-rechts-Benennung für die Europa-Achse. Nach Konfliktregel 3 (kapitelübergreifend schlägt lokal) steht dort jetzt „domain score = (positive pole − negative pole) / (sum of both poles)". |

Ebenfalls neu, aber rein technisch: ein Lesezeichen `_F.8_Migration_re-split` auf der Anhang-F.8-Überschrift, damit der Verweis aus 4.7.1 verlinkt werden konnte. Es gab im Dokument bisher keinen Anker für F.8.

---

## 5. Zahlen, die sich durch E1 geändert haben — zur Kontrolle

Alle aus dem Lauf vom 31.07., nicht aus einer Vorabrechnung.

| Stelle | alt | neu |
|---|---|---|
| 5.3.3, vier Dispersionspaare | 0.396/0.144 · 0.574/0.140 · 0.107/0.036 · 0.366/0.232 | 0.386/0.138 · 0.551/0.136 · 0.104/0.035 · 0.345/0.222 |
| 5.4, Europa Manifest | +0.0667 (SE 0.0191, p 0.013) | **+0.0769** (SE 0.0139, p 0.001) |
| 5.4, Europa Rede | +0.0344 (SE 0.0094, p 0.011) | **+0.0407** (SE 0.0080, p 0.002) |
| 5.4, Europa Refit | −0.0181 / 0.0050 | **−0.0079 / 0.0113** |
| 5.4, Economy Rede | +0.0164 (p 0.018) / +0.0171 | **+0.0195** (p 0.016) / **+0.0201** |
| 5.4, Welfare Manifest | −0.0252 (p 0.016) / −0.0250 | **−0.0240** (p 0.022) / **−0.0238** |
| 5.4, Welfare Rede | −0.0001 (p 0.96) | **+0.0002** (SE 0.0026) |
| 5.4, Migration Rede | +0.0160 (p 0.004) / +0.0112 | **+0.0181** (p 0.001) / **+0.0132** |
| 5.4, Migration Manifest | „flat rather than a second sign flip" | **+0.0154 / +0.0007** — der zweite Vorzeichenwechsel entfällt, der Halbsatz ist entschärft |
| 5.4, Migrationsdispersion | „nearly doubles, from 0.113 to 0.221" | „**more than doubles**, from **0.104** to 0.221" (Faktor 2,12) |
| 5.4, gewichtetes Mittel Migration | +0.544 → +0.344 | **+0.554** → +0.344 |
| 5.4, Pre/Post Europa | 0.2807 → 0.6219 (+0.3412) · 0.1785 → 0.3942 (+0.2157) | **0.2527** → 0.6219 (**+0.3692**) · **0.1650** → 0.3942 (**+0.2291**) |
| 5.4, Pre/Post übrige | +0.0826 · +0.0755 · −0.0774 | **+0.0878 · +0.0836 · −0.0738** |
| 5.4, Fensterdifferenzen | −0.21 · −0.12 · +0.06 · −0.02 · −0.47 | **−0.22** · −0.12 · **+0.07** · **−0.01** · **−0.54** |

**Unverändert geblieben und geprüft:** Tabelle 5.1 vollständig (H2a läuft ohnehin auf `valid`), alle H2b-Steigungen und R², die vier H2c-Niveaupaare in absteigender Ordnung, „31 von 32 Domänen-Perioden-Paaren" samt der Europa-LP-13-Ausnahme, alle LP-19/20-Werte, sämtliche Parteitrajektorien im Migrationsabsatz, „about two thirds" (jetzt 0,66), die Spalte *Δ slope* in Table 5.2.

---

## 6. Ungeprüft geblieben

- Die **Klassifikationsstufe selbst** (ManifestoBERTa, gbert-000) und die Satzsegmentierung — wie schon in den Berichten, es wurde ab den Aggregations-Caches gerechnet.
- Die **Literaturangaben** wurden nicht gegen die Originalquellen geprüft (Ausnahme: Sigelman und Buell, im 4.7-Durchgang).
- Ob die **abgeschnittenen Abbildungstexte** im gesetzten PDF genauso erscheinen — die Beschneidung sitzt in den PNG-Quellen, also ja.
