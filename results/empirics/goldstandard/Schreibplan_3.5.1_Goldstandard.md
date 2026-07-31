# Schreibplan 3.5.1 — Gold Standard (Human Coding)

Stand: 2026-07-15 · Alle Zahlen verifiziert gegen `results/empirics/goldstandard/` und `06_gold_realign_validate.R`.
Sechs Absätze. Jeder hat eine Aufgabe. Was nicht trägt, steht unter „Raus aus 3.5.1".

---

## P1 — Zweck, und daraus das Design

**Funktion:** Bindet die Stichprobe an ihren inferentiellen Zweck und macht die Stratifizierung zur *Folge* dieses Zwecks, nicht zur Vorgabe.

**Inhalt:**

- Zweck zuerst: Per-Klassen-Validierung und gezielte Präzisionsschätzung (v. a. 305), bewusst nicht korpusproportional.
- Daraus die Ziehung: **259 Sätze über 16 Strata**, Floor **n ≥ 10** je Stratum, DPS bewusst übersampelt (**n = 56**).
- Die Stratendefinition ist selbst ein Argument: Sie spiegelt exakt das Kodierschema — 9 gerichtete B-Buckets + 5 nicht-gerichtete A-Buckets + 2 Residuen („Economy (sonstige)", „Welfare & Social Policy (sonstige)") = 16.
- Entscheidend und leicht übersehen: **stratifiziert wurde auf der Modellvorhersage**, nicht auf dem menschlichen Urteil. Damit ist das natürliche Schätzziel Präzision.

**Anker:**

- *stratified on the model's predicted bucket, so the sample estimates precision: given the model predicts X, what does the human say?*
- *the sampling frame mirrors the coding scheme — directional buckets where the domain is directional, A-buckets where it is not*
- *readers seeking corpus-representative rates must turn to 3.5.2*

**Literatur:** Barberá et al. (2021) — Blaupause für Validierung gegen menschliche Benchmarks mit Zweckbindung und Stratifizierung. Eine Referenz reicht.

---

## P2 — Kodierprozedur und Granularität

**Funktion:** Macht die menschliche Referenz überprüfbar. Ohne diesen Absatz ist alles danach wertlos.

**Inhalt:**

- Wer kodierte, nach welchem Codebuch, **verblindet gegenüber der Modellvorhersage**.
- Granularität: top-down erst der A-Bucket (9 Domänen + „Nicht zuordenbar (000)"), dann die B-Richtung **nur dort, wo der A-Bucket gerichtet ist** — Economy, Welfare & Social Policy, Migration, European Integration.
- Begründung der Granularität (stärkstes Design-Argument): Validierung auf genau den Ebenen, die die Analyse verwendet — nicht auf der 56er-Feincode-Ebene, die die Pipeline nie berichtet.
- Zwei Grenzen offen deklarieren, je ein Halbsatz: `dps_fine` blieb faktisch leer (**1 von 249**), `flag_unsure` wurde nie gesetzt (**0 von 249**). Ersteres hat eine Konsequenz in P6; letzteres heißt: keine Unsicherheitsmarkierung behaupten.
- Falls allein kodiert: keine Zweitkodierer-Reliabilität. Die berichteten κ sind **Mensch gegen Modell**, nicht Mensch gegen Mensch.

**Anker:**

- *coding proceeds top-down: the A-level bucket first, then the B-level direction where the assigned bucket is one of the four directional domains*
- *reliability is estimated at the levels the analysis uses, not at the 56-code level the pipeline never reports*
- *human-versus-model agreement with the human as reference — not inter-coder reliability*

**Literatur:**

- Werner et al. (2011) — Handbook 4 als Codebuch; dasselbe wie Silber, Konsistenz explizit machen.
- Mikhaylov, Laver & Benoit (2012) — Doppeldienst: die bekannt begrenzte Feincode-Übereinstimmung geschulter Kodierer **rechtfertigt**, gar nicht erst auf Feincode-Ebene zu validieren.

---

## P3 — Eingefrorenes Artefakt und Carry-over

**Funktion:** Deklariert Nicht-Reproduzierbarkeit aktiv und entschärft sie mit Evidenz statt mit Rhetorik.

**Inhalt:**

- Eingefrorenes Artefakt: konsolidiert aus mehreren Batches, Ziehung pfadabhängig und nicht reproduzierbar, realisierte Stratenverteilung dokumentiert (Appendix).
- Carry-over (fehlt im alten Plan komplett): kodiert auf einer älteren Korpusversion, per Textabgleich innerhalb der Rede auf v2 getragen. **259 kodiert → 249 getragen → 10 Gaps (3,9 %)**, ausgeschlossen.
- Sofort die Verteidigung, weil sie stark ist: Gegen die bei der Ziehung gespeicherte Vorhersage lässt sich κ auf allen 259 rechnen — **0,570 gegenüber 0,562** auf den 249. Der Carry-over-Verlust bewegt die Schätzung um 0,008.
- Ehrlich bleiben: Zwei Strata sind durch den Carry-over unter den Floor gerutscht — **Contra-EU (10 → 9)** und **Sozialstaat Ausbau (10 → 9)**. Der automatische Check im Skript prüft nur die 9 A-Buckets (dort min = 12) und übersieht das. Ein Satz genügt — aber er muss dastehen.

**Anker:** *carry-over loss does not move the estimate (κ 0.562 on the 249 carried vs 0.570 on all 259 coded)*

**Literatur:** keine. Der Absatz lebt von Zahlen.

---

## P4 — Ergebnisse I: κ_A und die Null-Klassen-Asymmetrie

**Funktion:** Die Headline-Zahl — und der Punkt, an dem 3.5.1 Fehlermodus 1 vorbereitet, ohne ihn schon auszuspielen.

**Inhalt:**

- Hard κ_A auf dem Argmax als **konservativer Validitätsboden**: raw **0,562** (agree 0,614, n = 249), codeable-only **0,630** (agree 0,677, n = 226).
- Krippendorffs α liefert praktisch dasselbe (**0,561 / 0,630**) — Halbsatz oder Fußnote; zeigt, dass der Befund kein κ-Artefakt ist.
- Der entscheidende Satz: Der Sprung raw → codeable ist **kein Rauschen, sondern definitorisch**. Die **23 Sätze (9,2 %)**, die der Mensch „nicht zuordenbar" nennt, kann das Modell strukturell nicht treffen — 000 ist keiner der 56 Codes. κ_A raw enthält also eine Kategorie, die nur eine Seite vergeben kann.
- Das ist die quantitative Signatur von Fehlermodus 1. Hier hinlegen, nicht deuten (→ 3.5.3).

**Anker:** *the raw-to-codeable gap is structural, not noise: code 000 is not among the 56, so the model can never produce the human's 'not assignable' label — κ_A raw contains a category only one side can assign*

**Literatur:**

- Mikhaylov et al. (2012) — realistischer Maßstab; „perfect human coding" existiert im Feld nicht.
- Landis & Koch (1977) — Orientierung, ausdrücklich **nicht** Automatismus. Die Tragfähigkeitsfrage gehört nach 3.5.3, hier nicht vorwegnehmen.

---

## P5 — Ergebnisse II: Konfidenz und Soft Agreement

**Funktion:** Fitness-for-purpose. Fehlt im alten Schreibplan komplett, ist im Skript zentral begründet — und ist die Brücke zu 3.7.2.

**Inhalt:**

- Das Argument zuerst: Die Pipeline aggregiert über das **volle Posterior**, nicht über den Argmax. Ein Hard-κ bestraft das Modell für Sätze, auf die es sich kaum festgelegt hat, und unterschätzt seine Eignung für genau die Verwendung, die stattfindet.
- κ_A nach Konfidenzbins: **0,547** (< 0,5; n = 106) → **0,700** (0,5–0,8; n = 69) → **0,702** (≥ 0,8; n = 51). Übereinstimmung folgt der Konfidenz.
- Soft agreement, mittlere Modellmasse auf dem Bucket des Menschen: **0,58** (n = 226).
- Beides zusammen sagt eine Sache: Die Konfidenz des Modells ist informativ, das Posterior trägt Signal, das der Argmax wegwirft. Der Hard-κ bleibt die Headline; das hier ist das fairere Bild für die Soft-Pipeline.

> ⚠️ **Basis-Warnung:** Die Soft-Werte *je Konfidenzbin* (0,494 / 0,619 / 0,715) stehen nur in `agreement_soft_by_confidence.csv` und liegen auf der **236er-Basis**, nicht auf 226. Die v2-Version druckt Skript 06 nur in die Konsole, schreibt sie nicht als CSV. Entweder Basis deklarieren oder aus dem 06-Log ziehen. Der Gesamtwert **0,58** ist sauber auf 226.

**Anker:** *the pipeline aggregates over the full posterior, not the argmax, so hard κ under-credits the model on barely-committed sentences; agreement tracks confidence, and mean mass on the human's bucket is 0.58*

**Literatur:** keine. Vorverweis auf 3.7.2 statt Zitat.

---

## P6 — Ergebnisse III: 305-Präzision und κ_B

**Funktion:** Liefert die Zahl, auf der 3.6.2 steht — mit den Caveats, die sie tragen muss.

**Inhalt:**

- Von **23** AI-305-Sätzen (nicht-prozedural; 28 insgesamt) legte der Mensch:
  - **10 (43,5 %)** in den DPS-Bucket
  - **10 (43,5 %)** auf „nicht zuordenbar"
  - 2 auf Foreign Policy & Defence, 1 auf Economy
- Dass beide Anteile 43,5 % sind, ist echt und kein Tippfehler — so schreiben, dass es nicht wie einer aussieht.
- Drei Caveats, alle kurz, keines verzichtbar:
  1. **Bucket-Ebenen-Bestätigung**, keine Bestätigung des Codes 305 — weil `dps_fine` leer blieb (Rückgriff P2).
  2. **n = 23**.
  3. Der Nenner entscheidet: 43,5 % ist der v2-, prozedural-gefilterte Wert (10/23); die ältere **37 %-Zahl ist derselbe Zähler auf dem ungefilterten Sampling-Nenner (10/27)**. Kanonischen Wert nennen, den alten weglassen.
- κ_B: **0,456** (n = 98, agree 0,500), **exploratorisch** deklariert, Rückverweis 3.2.2.

**Anker:** *bucket-level confirmation, not code-level; n = 23*

**Literatur:** keine.

**Handshake zu 3.6.2 (bewusst setzen):** Die vorab festgelegte Regel lautet „Präzision < 0,30 → Exklusion zulässig". Mit **0,435** landet die Arbeit im **Vorsichts-Ast** (*non-trivial precision; lean on the asymmetry bound, treat excl cautiously*). 3.5.1 darf das nicht auflösen — aber die Zahl so schreiben, dass 3.6.2 daran anknüpfen kann, statt später davon überrascht zu wirken.

---

## Raus aus 3.5.1

| Element | Wohin | Grund |
|---|---|---|
| κ_A nach Partei (SPD 0,561 … AfD 0,714) | Appendix | Interessant, trägt hier kein Argument |
| Bucket-Balance-Tabelle | Appendix | Im Text genügt: Floor hält auf A-Bucket-Ebene nach dem Carry-over (min n = 12) |
| „305 im Gold unterrepräsentiert (10,4 %)" | 3.5.3, umformuliert | Der 305-Anteil ist **vom Ziehungsdesign gesetzt**, nicht zufällig. „Unterrepräsentiert" klingt nach Unfall und ist angreifbar. Sauber: *gold cannot estimate the corpus 305 rate at all — that is what silver is for* |
| 5 von 6 prozeduralen Sätzen → 305 | 3.5.3 (Fehlermodus 2) oder Fußnote | Schön, aber nicht hier |
| Confusion-Matrizen, PRF pro Klasse | Appendix | — |

Damit stehen **vier Zitate** in 3.5.1 — Barberá, Werner, Mikhaylov, Landis & Koch — und jedes tut Arbeit. Grimmer & Stewart steht schon im 3.5-Intro; hier nicht wiederholen.

---

## Verifizierte Zahlen (Referenz)

| Größe | Wert | Quelle |
|---|---|---|
| Kodierte Sätze | 259 | `goldstandard_key_slim_FINAL.csv` (259 Zeilen) |
| Strata | 16 (9 B-Buckets + 5 A-Buckets + 2 Residuen) | `goldstandard_key_slim_FINAL.csv` (`stratum`) |
| Floor je Stratum | n ≥ 10; DPS übersampelt auf 56 | ebd. |
| Auf v2 getragen | 249 | `goldstandard_v2_carried.csv` |
| Gaps | 10 (3,9 %) | `goldstandard_v2_gaps.csv` |
| Fehltrennungen / Platzhalter im getragenen Set | 0 / 0 | eigene Rechnung (upstream entfernt) |
| Auswertbar (evaluable) | 249 | eigene Rechnung |
| „Nicht zuordenbar" (Mensch) | 23 (9,2 %) | eigene Rechnung |
| Codeable | 226 | eigene Rechnung |
| κ_A raw | **0,562** (agree 0,614, n = 249) | `goldstandard_v2_validation_summary.csv` ✓ reproduziert |
| κ_A codeable | **0,630** (agree 0,677, n = 226) | ebd. ✓ reproduziert |
| Krippendorff α raw / codeable | 0,561 / 0,630 | ebd. |
| κ_A Cross-Check (Sampling-Vorhersage) | 0,570 / 0,635 (n = 259 / 236) | `agreement_kappa_flavors.csv` |
| Prädiktion v2 vs. Sampling-Zeit identisch | 247 / 249 (99,2 %) | eigene Rechnung |
| κ_A nach Konfidenz | 0,547 (n = 106) / 0,700 (n = 69) / 0,702 (n = 51) | `goldstandard_v2_kappaA_by_confidence.csv` |
| Soft agreement (gesamt) | **0,58** (n = 226) | `goldstandard_v2_validation_summary.csv` |
| Soft agreement je Bin | 0,494 / 0,619 / 0,715 — **Basis 236!** | `agreement_soft_by_confidence.csv` |
| AI-305 (alle / nicht-prozedural) | 28 / 23 | eigene Rechnung |
| 305: human → DPS-Bucket | 10 / 23 = **43,5 %** | `gold_305_precision_v2.csv` ✓ reproduziert |
| 305: human → „nicht zuordenbar" | 10 / 23 = **43,5 %** | ebd. (echt, kein Fehler) |
| 305: alte 37 %-Zahl | 10 / 27 (ungefilterter Sampling-Nenner) | eigene Rechnung — Ursprung geklärt |
| κ_B within-domain | **0,456** (agree 0,500, n = 98) | `goldstandard_v2_validation_summary.csv` ✓ reproduziert |
| Strata unter Floor nach Carry-over | Contra-EU (9), Sozialstaat Ausbau (9) | eigene Rechnung |
| `dps_fine` gefüllt | 1 / 249 (0 / 23 bei AI-305) | eigene Rechnung |
| `flag_unsure` gesetzt | 0 / 249 | eigene Rechnung |
| Prozedurale Sätze → 305 | 5 / 6 | eigene Rechnung (→ 3.5.3) |

### Offene Punkte

- **Kodierprozedur** — wer, Zweitkodierer ja/nein, Verblindung: nur du kannst das ausfüllen (P2).
- **Soft agreement je Konfidenzbin auf 226er-Basis** — aus dem 06-Konsolenlog ziehen oder Basis 236 deklarieren (P5).
- **κ_B im Plan mit „ca. 0,27"** — hat in den Dateien keine Grundlage. Kanonisch ist 0,456.
- **Plan-Wert „305-Präzision ca. 37 %"** und Fehlerliste („37 %, NICHT 44 %") — beide zementieren den veralteten Nenner. Kanonisch: 43,5 %.
