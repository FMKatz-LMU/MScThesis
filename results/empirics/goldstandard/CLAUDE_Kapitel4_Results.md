# CLAUDE.md — Handoff für Kapitel 4 (Results)

Stand: 2026-07-16. Dieses Dokument fasst alles zusammen, was aus der Arbeit an Kapitel 3 (v. a. 3.4–3.5.1) für das Schreiben von Kapitel 4 folgt: das Validitäts-Budget, die Zahlen-Disziplin, die vorab fixierten Entscheidungen und die konkreten Fallstricke. Es ersetzt nicht die Hypothesentexte aus 3.2 — die müssen beim Schreiben danebenliegen.

---

## 0. Sofort zu klären, bevor geschrieben wird

- **Die H-Ergebnis-CSVs sind aktuell nicht zugänglich.** Gemountet ist nur `results/empirics/goldstandard/`. Die Dateien für Kapitel 4 (`H1a_jsd_table.csv`, `H1b_*`, `H2a_per_bucket_summary.csv`, `H3_trend_fits.csv`, `H3_trend_fits_exafd.csv`, `H3_pre_post_2017.csv`, `dps_305_asymmetry.csv`, `intervention_dps_summary.csv`, `benchmark_nearest_manifesto.csv`, `benchmark_pairwise_jsd.csv`, `H1a_temperature_sweep.csv`, `H1a_threshold_robustness.csv`) liegen eine Ebene höher. **Zum Verifizieren: `results/empirics/` mounten oder die CSVs hochladen.** Ohne sie ist `chapter3_numbers.md` die einzige Quelle — und die trägt an mehreren Stellen `[MANUAL]`/`[MISSING]`.
- **Zahlen-Disziplin wie bei 3.5.1 anwenden:** jeden Wert vor dem Einbau gegen die Quell-CSV rechnen, nicht aus dem Plan oder aus `chapter3_numbers.md`-Arbeitswerten übernehmen. In Kapitel 3 wichen Plan-Arbeitswerte mehrfach von den kanonischen Dateiwerten ab.
- **Hypothesentexte bereitstellen.** H1a/H1b/H2a/H3 sind in diesem Handoff nur über ihre Ergebnisgrößen bekannt, nicht über ihren exakten Wortlaut. Der gehört aus 3.2 daneben, sonst lässt sich „bestätigt/nicht bestätigt" nicht sauber formulieren.

---

## 1. Das Validitäts-Budget — der wichtigste Rahmen

Kapitel 3.5.3 legt fest, **was die Reliabilität von κ ≈ 0,6 trägt und was nicht**. Das ist die Leitplanke für jede Formulierung in Kapitel 4. Nicht neu herleiten — auf 3.5.3 zurückverweisen — aber strikt einhalten.

**Getragen (stark formulierbar):**

- Aggregierte Verteilungsaussagen auf Zell-Ebene: JSD, Salienzprofile, Domänen-Agenden. Begründung: Satz-Rauschen mittelt sich über Tausende Sätze je Zelle weg.
- Vergleiche zwischen Zellen und über Zeit auf der **A-Ebene (Domäne)**.

**Nicht getragen (nur vorsichtig / mit explizitem Caveat):**

- Satzgenaue Einzelaussagen.
- Feinkörnige Einzelzell-Claims („in Zelle X bei Partei Y ist Z der Fall").
- Die **Economy-Richtungsachse** speziell — in Kapitel 3 mehrfach als schwach markiert.
- Alles auf der **B-Ebene (Richtung)** ohne Exploratory-Label (siehe §2).

Praktische Konsequenz: Kapitel 4 argumentiert über **Muster in Verteilungen**, nicht über einzelne Sätze oder einzelne Zellen. Wo ein Einzelzell-Befund auffällt, wird er als Illustration markiert, nicht als Evidenz.

---

## 2. B-Ebene ist exploratorisch — vorab festgelegt

- Die Richtungsachse (Aggregation B) wurde **vor der Analyse** als exploratorisch deklariert (3.2.2). κ(B) = 0,456 gegenüber κ(A) = 0,562/0,630 — Richtung wird unzuverlässiger rekonstruiert als Domäne.
- **Jedes B-Ergebnis in Kapitel 4 (insbesondere H1b) trägt das Exploratory-Framing.** Keine konfirmatorische Sprache, keine starken Schlüsse. Formulierungen wie „consistent with", „suggestive of", nicht „demonstrates".
- H1b läuft auf B-Ebene (Migration 601/608). Das Ergebnis existiert (`h1b_migration_601608_summary.csv`: n=37, mean_delta_jsd −0,0062), muss aber als exploratorisch gerahmt werden.

---

## 3. Die Primärspezifikation und das Pre-Commitment

- **Primärspezifikation A-Ebene:** `filtered_000 × excl × tau = 1.0`. B-Ebene: `code305 = incl` (CODE305_PRIMARY_DEFAULT). Quelle: `00_config.R`.
- Spezifikation und Prüfkriterien wurden **vor der Analyse fixiert**. Das ist ein zentrales Glaubwürdigkeitsargument — Kapitel 4 nutzt es, indem es **immer Primärspezifikation zuerst** berichtet, Robustheit danach.
- Die zwei Fehlermodi aus 3.4/3.5.3 sind **diagnostiziert, nicht offen**. Kapitel 4 re-litigiert sie nicht, sondern berichtet Ergebnisse auf der korrigierten Pipeline (000-Filter + 305-Exklusion). Rückverweis auf 3.6 für die Korrekturen, kein erneutes Aufrollen.

---

## 4. Code 305 / DPS — die heikelste Stelle

Hier steckt die größte Angriffsfläche der Arbeit, entsprechend vorsichtig.

- **305-Präzision (Gold): 43,5 % (n = 23), Obergrenze** (Bucket-Ebene, nicht Code-Ebene — `dps_fine` blieb leer). Die alte 37 %-Zahl ist derselbe Zähler auf anderem Nenner (10/27) — **nicht** verwenden. Details in `Schreibplan_3.5.1_Goldstandard.md`.
- Die **vorab fixierte Regel** war „Präzision < 0,30 → Exklusion zulässig". Mit 0,435 liegt die Arbeit im **Vorsichts-Ast** (*lean on the asymmetry bound, treat excl cautiously*). Das heißt für Kapitel 4: die 305-Exklusion wird **nicht** über die Gold-Präzision allein gerechtfertigt, sondern über die **Asymmetrie-Schranke**.
- **H2a / DPS-Asymmetrie (der eigentliche 305-Befund für Kapitel 4):** mittlerer Excess Rede-305 minus Manifest-305 = **+0,133**, und **38/38 Zellen (100 %)** haben Excess > 0 (min/median/max 0,002/0,14/0,227). Quelle `dps_305_asymmetry.csv`. Das ist ein starker, zellübergreifender Befund — genau die Art aggregierter Aussage, die das Validitäts-Budget trägt.
- **Redundanz-Zerlegung** (`intervention_000_vs_305_redundancy.csv`): Filter und 305-Exklusion sind komplementär, nicht redundant. Der Kandidatensatz „excl entfernt ~115 % dessen, was der Filter entfernt" ist ein **abgeleiteter** Wert — laut `chapter3_numbers.md` gegen das 17-PART-3-Konsolenverdikt gegenprüfen, bevor er in den Text geht.
- **Genuiner DPS-Anteil der AI-305 (~14–20 %)** ist `[MANUAL]` — kombinierter Befund aus Gold-Bestätigung + Silber-Zerlegung. **Exakte Herleitung aus dem 13-Summary-Log übernehmen, nicht rekonstruieren.**
- **Osnabrügge et al. (2023)** stützt, dass 305 generisch-politische Rhetorik absorbiert — publizierte Präzedenz, kein Pipeline-Artefakt. In diesem Chat noch nicht am Paper verifiziert; vor Verwendung prüfen.

---

## 5. H3 — der Sign-Flip ist der Kernbefund

- **H3-Kernbefund: Vorzeichenwechsel der Europa-Steigung bei AfD-Ausschluss.** Manifesto-Slope 0,0667 (p = 0,013) → **−0,0181** (p = 0,47) ohne AfD; Speech-Slope 0,0344 → 0,005. Quellen `H3_trend_fits.csv` / `H3_trend_fits_exafd.csv` / `H3_trend_exafd_compare.csv`.
- Das ist die Stelle, an der Kapitel 4 seine größte inhaltliche Aussage macht — **sorgfältig und ehrlich**: Der Effekt hängt an einer einzigen Partei. Das ist der Befund, nicht ein Störfaktor. Nicht als Robustheitsfußnote verstecken, sondern als Ergebnis führen.
- Pre/Post-2017-Zerlegung (`H3_pre_post_2017.csv`) stützt die Erzählung (Europa Manifesto delta +0,341, Speech +0,216), aber n = 2 post / 6 pre — **kleine n offen benennen**, keine Signifikanz-Sprache.
- Vorsicht mit dem Validitäts-Budget: H3 läuft über Domänen-Salienz (A-Ebene, getragen), nicht über Richtung — das ist gut. Solange die Aussage „Salienz von Europa steigt/fällt" ist und nicht „die Position zu Europa", bleibt sie im Budget.

---

## 6. Robustheit ehrlich berichten — mehrere Arme fallen durch

Die Akzeptanzkriterien waren vorab fixiert. **Nicht alle sind erfüllt**, und das gehört offen in den Text, nicht in eine kaschierende Fußnote.

- **Arm 1 (tau-Sweep), Kriterium ρ ≥ 0,90:** H1a sharp_vs_native = 0,97 (OK), aber sharp_vs_flat = **0,579** und native_vs_flat = **0,729** (fallen durch). H1b ähnlich (0,956 / 0,706 / 0,843).
- **Arm 2 (Threshold), Kriterium ρ ≥ 0,85:** H1a meist OK (0,996 / 0,943 / 0,929); H1b hard0.5~soft = **0,772** (fällt durch).
- Lesart: Die Ergebnisse sind robust gegen **moderate** tau-Variation (sharp↔native) und Threshold-Wahl, aber **nicht** gegen extreme Verflachung (flat, tau = 2,0). Das ist eine legitime, begrenzte Robustheitsaussage — genau so formulieren, nicht überdehnen.
- **Hard vs. soft Aggregation:** ρ ≈ 0,93–0,94. Das heißt: die Hard/Soft-Wahl ist für die Befunde **folgenlos**. Nützlich als Robustheitssatz — und es entwertet zugleich jede Diskussion, ob Soft „besser" sei (in Kapitel 3 P5 deshalb gestrichen).

---

## 7. Zelle, Gewichtung, Benchmark

- **Analyseeinheit:** Partei-×-LP-Zelle. 6 Parteien × 8 LPs = 48 möglich, **38 valide** (is_primary), 1 dokumentierter Ausschluss (**FDP LP18**, unter 5 % 2013 — n_sent sind Fehlattributionen, `excluded_cells.csv`). n_sent über valide Zellen: min/median/max = 41.562 / 106.641 / 279.947.
- **Gewichtungsquellen (VOTE_SHARES/SEAT_SHARES):** Bundeswahlleiterin (amtl. Endergebnisse) + Deutscher Bundestag (Sitzverteilung, LP-Start). `[Offener Punkt 14.6]` — im Text **zitierbar** machen, aktuell nur Config-Kommentar.
- **Benchmark (H1a-Kontext):** Nearest-Manifesto-Trefferquote **42,1 % (16/38)** gegen Zufallsbaseline **21,1 %**; mittlerer Rang des eigenen Programms 1,87 (`benchmark_nearest_manifesto.csv`). Between-Party-Referenz-JSD (Rede): mean 0,0074 (`benchmark_pairwise_jsd.csv`, n = 74 Paare). Die Zufallsbaseline 21,1 % ist **abgeleitet** (mean 1/n_manifestos) — als solche kennzeichnen.
- **H1a-Kernzahlen:** JSD mean/median/min/max = 0,0297/0,0238/0,0114/0,1125 über 38 Zellen (`H1a_jsd_table.csv`).

---

## 8. Was Kapitel 4 NICHT tun darf (aus diesem Chat)

- **Gold-Zahlen nicht als Korpusaussagen** verwenden. Gold ist prädiktions-stratifiziert und schätzt Präzision, keine korpusrepräsentativen Raten. Korpusweite Aussagen laufen über Silber/die Pipeline.
- **Konfidenz nicht als Validitätssignal** an der Grenze zum Nicht-Kodierbaren nehmen. Innerhalb des kodierbaren Sets ist Konfidenz informativ, an dessen Rand irreführend (3.4/3.5.3). Falls Kapitel 4 überhaupt Konfidenz erwähnt: nur innerhalb des kodierbaren Bereichs.
- **Die zwei Fehlermodi nicht neu aufrollen.** Sie sind in 3.5.3 diagnostiziert; Kapitel 4 berichtet auf der korrigierten Pipeline und verweist zurück.
- **Nicht über Einzelzellen oder Einzelsätze argumentieren** (Validitäts-Budget, §1).

---

## 9. Stil & Notation (konsistent mit Kapitel 3)

- Schreibweise **„codeable"** (nicht „codable"), **„observed agreement"** statt „raw agreement" wo Verwechslung droht, **κ(A)/κ(B)**, **„the coder"**, britisches Englisch (penalises, licence), **drei Nachkommastellen** durchgängig.
- **„we"-Perspektive**, Begründung vor Zahl, Semikolon-Konstruktionen erlaubt, Literatur **argumentativ** statt dekorativ.
- **Vorwärts-/Rückverweise als Service, nicht als einzige Begründung.** Der Grund steht im Satz, der Verweis ist Wegweiser.
- **Primärspezifikation zuerst, Robustheit danach.** Grenzen offen benennen (wie in 3.3.1/3.3.2), nicht kaschieren.
- Absätze sparsam: Der Methodenteil ist bereits ~¼ des Zielvolumens. Kapitel 4 nicht doppeln, was Kapitel 3 schon sagt; auf Validität per Verweis zurückgreifen.

---

## 10. Offene Verifikationspunkte, die nach Kapitel 4 durchschlagen

| Punkt | Status | Was tun |
|---|---|---|
| H-Ergebnis-CSVs | nicht gemountet | `results/empirics/` freigeben, dann jede Zahl nachrechnen |
| Genuiner DPS-Anteil ~14–20 % | `[MANUAL]` | aus 13-Summary-Log übernehmen, nicht rekonstruieren |
| „excl entfernt ~115 %" | abgeleitet | gegen 17-PART-3-Konsolenverdikt prüfen |
| Gewichtungsquellen | `[Offener Punkt 14.6]` | zitierfähig machen |
| Osnabrügge-305-Absorption | unverifiziert | am Paper prüfen |
| gBERT-Holdout-Metriken (PR-AUC 0,852 etc.) | `[MISSING]` in numbers.md | aus 10-Arbeitsverzeichnis holen, falls in Kap. 4 zitiert |
| Hypothesen-Wortlaut | nicht in diesem Handoff | aus 3.2 danebenlegen |

---

## 11. Empfohlene Grobstruktur Kapitel 4 (Vorschlag, anzupassen)

1. **Kurzer Rahmen:** Analyseeinheit (38 Zellen), Primärspezifikation, ein Satz Validitäts-Budget mit Verweis auf 3.5.3. Keine Wiederholung der Diagnose.
2. **H1a (Kongruenz-Niveau):** JSD-Verteilung + Nearest-Manifesto-Benchmark gegen Zufallsbaseline. Der Anker-Befund, voll im Budget.
3. **H1b (Richtung, exploratorisch):** Migration 601/608, klar als exploratorisch gerahmt.
4. **H2a (DPS-Beitrag / 305-Asymmetrie):** der +0,133-Excess über 38/38 Zellen; Rückverweis 3.6.2 für die Korrektur-Logik.
5. **H3 (Zeittrend Europa + Sign-Flip):** Kernbefund, AfD-Abhängigkeit ehrlich als Ergebnis, kleine n benennen.
6. **Robustheit:** ehrlich, welche Arme das vorab fixierte Kriterium erfüllen und welche nicht (§6). Hard/soft-Folgenlosigkeit als Stärke.

---

Referenzdateien im selben Ordner: `Schreibplan_3.5.1_Goldstandard.md` (verifizierte 3.5.1-Zahlen + Nenner-Klärung), `Appendix_Gold_Strata_Distribution.docx`.
