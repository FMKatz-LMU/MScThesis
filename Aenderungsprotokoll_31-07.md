# Änderungsprotokoll — Einarbeitung der drei Prüfberichte

**Basisdatei:** `MasterThesis_FKatzmann12293439_linked.docx` (Stand 31.07.2026)
**Ergebnis:** `MasterThesis_FKatzmann12293439_eingearbeitet.docx`
**Erstellt:** 31.07.2026

Alle Änderungen sind einzeln rücknehmbar: jede Zeile nennt Fundstelle, alten Wortlaut, neuen Wortlaut und den Bericht, aus dem der Befund stammt.

---

## 0. Entscheidungen, unter denen dieses Protokoll steht

| | |
|---|---|
| **E1 · Analysebasis H2c/H3** | **Variante A** — die korrigierten `15_H2.R`/`16_H3.R`/`19_…R` sind am 31.07. gelaufen, die Ergebnisdateien wurden neu eingelesen. Beide Härtetests des 4.8-Berichts treffen: Europa/Manifest **+0.0769**, `slope_delta` **+0.0848**. `n_parties` steht in LP 13–15 jetzt auf 4. Alle betroffenen Zahlen in 5.3.3, 5.4, Table 5.2, Fußnote 17, Introduction und Conclusion sind nachgezogen. |
| **E2 · Agenda centre of gravity** | **Variante A** — auf 38 valide Redezellen. Die sechs Werte wurden hier unabhängig aus `speech_soft.rds` und `manifesto_dists.rds` nachgerechnet und reproduzieren die Vorhersage des Kapitel-5-Berichts exakt. `18_agenda_centre_of_gravity.R` ist **noch nicht** neu gelaufen — Patch liegt bei, Abbildung 5.3 muss neu gerendert werden. |
| **E4 · Introduction** | mitgezogen (siehe Restliste, Punkt 1) |
| **E5 · neuer Abschnitt 5.6** | eingebaut |
| **E6 · Formelsatz 4.7** | die vom Nutzer hochgeladene `Kapitel_4-7_Neufassung.docx` wurde übernommen, beide Gleichungen als echtes OMML, Struktur geprüft |

---

## 1. Kapitel 4.7 — Komplettersatz (Übergabe Teil 5)

| Was | Wie |
|---|---|
| 4.7.1 – 4.7.4 | vollständig ersetzt durch die geprüfte Neufassung (26 Absätze), Anmerkungsseite **nicht** übernommen |
| Überschrift 4.7.3 | „The Measure: Jensen-Shannon Divergence" → **„The Jensen–Shannon Divergence"** (auch im Inhaltsverzeichnis) |
| Überschrift 4.7.4 | „Uncertainty: Sentence Bootstrap" → **„The Sentence Bootstrap"** (auch im Inhaltsverzeichnis) |
| Gleichung 1 (π_c) | OMML übernommen: echtes Σ-Subskript, ∈ = U+2208, Mᵀ als Hochstellung, ausgeschriebener Nenner, Einsvektor fett, in `<m:oMathPara>` |
| Gleichung 2 (JSD) | OMML übernommen: „JSD", „KL" und „with" aufrecht (`m:sty val="p"`, 4×), ∥ = U+2225, echte Brüche, Klammern über `<m:d>` |
| Hyperlink-Anker | 9 wiederhergestellt: `Ref_Mikhaylov_2012`, `Ref_Lin_1991`, `Ref_Sigelman_2004`, `Ref_Benoit_2009`, Anhang E (2×), F.3, G — **und ein neues Lesezeichen `_F.8_Migration_re-split`**, das es im Dokument bisher nicht gab |
| K8 (48-B7b) | in der Neufassung nachgezogen: „a fixed seed makes the procedure deterministic" → „a fixed base seed, from which the per-cell seeds are derived deterministically, makes the procedure reproducible" — die Neufassung trug den alten Wortlaut noch |
| K9 (K5) | Verweis in 4.7.1 auf die Re-Split-Ergebnisse: bereits „Section 4.9 … Appendix F.8"; Kapitel 5.2.2 ist im Fließtext von 5.5 verlinkt |
| Absatzformat | Einzug/Zeilenabstand der eingesetzten Absätze auf das Dokumentformat normalisiert (die Neufassung brachte eigene `spacing`/`jc`-Werte mit) |

---

## 2. Einzeländerungen in Dokumentreihenfolge

**Blk** = laufender Blockindex (Absätze und Tabellen) der Endfassung.

| Blk | Abschnitt | ID | alt | neu | Herkunft |
|---|---|---|---|---|---|
| 6 | Introduction | E4a | reverses the manifesto trend from +0.0667 to −0.0181 and flattens the speech trend to near zero | reverses the manifesto trend from +0.0769 to −0.0079 and flattens the speech trend to near zero | Folge aus K5-A6 |
| 6 | Introduction | E4b | as the established parties barely move. | as the established parties do not move apart on this domain. | Folge aus K5-A6 |
| 40 | 2.5 Expectations and Hypotheses | K5-B5/2.5 | A party's speech agenda stands closer to its own programme than to the programme of any other party. | A party's speech agenda stands closer to its own programme than party programmes stand to one another, and is identifiable among the other parties' programmes at a rate well above chance. | Kapitel-5-Bericht |
| 41 | 2.5 Expectations and Hypotheses | 48-A4a/2.5 | resembles the balance in its own programme more closely than the balance in any other party's programme. | resembles the balance struck in its own programme. | 4.8-Bericht |
| 47 | 2.5 Expectations and Hypotheses | 48-25X | after the entry of a Eurosceptic challenger, the AfD, into the Bundestag in 2017 | once the AfD becomes part of them, which happens with its 2013 programme on the manifesto side and with its entry into the Bundestag in 2017 on the speech side | 4.8-Bericht |
| 69 | 4.3.1 Sentence Segmentation (SoMaJo) | 47-M1 | but its total weight stays at one | but its total weight stays at one under the inclusive specification | 4.7-Bericht |
| 101 | 4.5.3 Diagnosis | K5-A4c | agreement ranges from an F1 of 0.800 for the Pro-EU direction to 0.148 for the limitation of the welfare state | agreement ranges from an F1 of 0.848 for welfare expansion and 0.800 for the Pro-EU direction down to 0.148 for the limitation of the welfare state | Kapitel-5-Bericht |
| 110 | 4.6.2 Exclusion of Code 305 | 47-S1 | because code 305 maps to no directional domain and there is nothing to exclude, which is consistent | because code 305 maps to no directional sub-bucket. Its mass sits in the residual category and drops out when the directional buckets are renormalised, so the exclusion has nothing to act on, which is consistent | 4.7-Bericht |
| 138 | 4.8 Empirical Strategy | 48-C1 | The base of every analysis is the set of party-period cells: six parties | The base of every analysis is the set of party-period cells. Six parties | 4.8-Bericht |
| 138 | 4.8 Empirical Strategy | 48-C2 | rather than a data constraint: coded PDS manifestos exist | rather than a data constraint. Coded PDS manifestos exist | 4.8-Bericht |
| 138 | 4.8 Empirical Strategy | 48-B11 | a pre-specified floor of 1,000 filtered sentences | a pre-specified floor of 1,000 sentences after the procedural filter | 4.8-Bericht |
| 138 | 4.8 Empirical Strategy | 48-A2 | At these counts, the sentence-level noise diagnosed in Section 4.5.3 averages out, and the analysis therefore proceeds on the aggregate level. | At these counts the random component of the sentence-level error averages out, while the two systematic failure modes diagnosed in Section 4.5.3 do not, which is why they are corrected at source in Section 4.6 rather than absorbed by the aggregation. The analysis therefore proceeds on the aggregate level. | 4.8-Bericht |
| 140 | 4.8.1 Hypothesis H1 | K5-B5/4.8.1 | H1a states that parties speak closer to their own manifesto than to the manifestos of other parties (Section 2.5). | H1a states that a party's speech agenda stands close to its own manifesto relative to how far party programmes stand apart, and that it is identifiable among the other parties' manifestos above chance (Section 2.5). | Kapitel-5-Bericht |
| 140 | 4.8.1 Hypothesis H1 | 48-B7a+C3 | (1,000 draws, seed 20260507); the full table of all 38 cells is given | (1,000 draws; the per-cell seeds are derived deterministically from the base seed 20260507). The full table of all 38 cells is given | 4.8-Bericht |
| 140 | 4.8.1 Hypothesis H1 | 48-A3+C4 | The first is a nearest-manifesto test: for each cell, the speech distribution is compared against the manifestos of all parties competing at that election, and we record | The first is a nearest-manifesto test. For each cell, the speech distribution is compared against the manifestos of all parties that hold a valid cell in the same legislative period, and we record | 4.8-Bericht |
| 140 | 4.8.1 Hypothesis H1 | 48-C5 | cell base as H1a itself: all unordered pairwise between-party JSDs | cell base as H1a itself. It comprises all unordered pairwise between-party JSDs | 4.8-Bericht |
| 140 | 4.8.1 Hypothesis H1 | 48-C6 | imposes on any distance between agendas; the manifesto-side reference states the ceiling | imposes on any distance between agendas. The manifesto-side reference states the ceiling | 4.8-Bericht |
| 141 | 4.8.1 Hypothesis H1 | 48-C7 | extends the consistency claim of H1a to the directional layer: within the four directional domains | extends the consistency claim of H1a to the directional layer. Within the four directional domains | 4.8-Bericht |
| 141 | 4.8.1 Hypothesis H1 | 48-C21a | As diagnosed in Section 4.5, all analyses on scheme B | As diagnosed in Section 4.5.3, all analyses on scheme B | 4.8-Bericht |
| 141 | 4.8.1 Hypothesis H1 | K1 | The objects of H1b are the within-domain JSD on scheme B and the directional composition of each domain; code 305 requires no treatment on this layer | The objects of H1b are the per-cell divergence across the nine directional sub-buckets, computed after the residual category is dropped and the remainder renormalised, and the within-domain composition of each of the four domains. The migration domain additionally carries a within-domain divergence, which is the quantity the re-split of arm (5) varies. H1b is read as a composition claim; no nearest-manifesto counterpart is computed on this layer. Code 305 requires no treatment on this layer | alle drei (kombiniert) |
| 141 | 4.8.1 Hypothesis H1 | 48-C22a | n = 36 under the 607/608 re-split | n = 36 under the 608/607 re-split | 4.8-Bericht |
| 141 | 4.8.1 Hypothesis H1 | 48-C9 | and appears as arm (5) in Section 4.9 | and appears as arm (5) in Section 4.9. | 4.8-Bericht |
| 143 | 4.8.2 Hypothesis H2 | 48-B1 | in the institutionally driven Democracy & Political System domain (Section 2.5). | in the institutionally driven Democracy & Political System domain, and that it points in the same direction for every party (Section 2.5). | 4.8-Bericht |
| 143 | 4.8.2 Hypothesis H2 | 48-C10 | aggregated across cells; the nine contributions sum exactly to the cell's JSD hence, concentration can be read off directly. | aggregated across cells. Because the nine contributions sum exactly to the cell JSD, concentration can be read off directly. | 4.8-Bericht |
| 143 | 4.8.2 Hypothesis H2 | 48-C11 | The second object records the direction of the divergence: the signed per-bucket differences | The second object records the direction of the divergence. The signed per-bucket differences | 4.8-Bericht |
| 143 | 4.8.2 Hypothesis H2 | 48-B6 | The intervention matrix of Section 4.6 remains diagnostic material for the corrections and is not an object of H2a. | The intervention ladder of Section 4.6, which crosses the filter with the code-305 treatment, remains diagnostic material for the corrections and is not an object of H2a; its results are reported in Appendices F.1 and F.3. | 4.8-Bericht |
| 144 | 4.8.2 Hypothesis H2 | 48-C21b | it carries the caution Section 4.5 sets out | it carries the caution Section 4.5.1 sets out | 4.8-Bericht |
| 144 | 4.8.2 Hypothesis H2 | 48-B10a | the score is the share of the right-coded pole minus the share of the left-coded pole, divided by the sum of the two poles | the score is the share of the positive pole minus the share of the negative pole, divided by the sum of the two | 4.8-Bericht |
| 144 | 4.8.2 Hypothesis H2 | 48-B3 | per domain (n = 38; migration n = 37, Section 4.8.1), and a slope below one against the 45-degree reference is read as a descriptive signature of moderation, not as a test. | per domain (n = 38; migration n = 37, Section 4.8.1) and reports slope and intercept. A slope below one against the 45-degree reference is the descriptive signature of compression, read as the between-party differences carrying over only in part, and the intercept states where the compressed positions sit. The two are read together, since compression towards a shifted level is not the same as compression towards a balanced position, and neither is read as a test. | 4.8-Bericht |
| 145 | 4.8.2 Hypothesis H2 | 48-Block-H2c | (ganzer Absatz) | H2c states that directional positions disperse less across parties in plenary speech than in their manifestos, the cross-channel convergence claim (Section 2.5), and it carries the same caution; measuring polarisation from what politicians say has established precedent (Gentzkow, Shapiro and Taddy, 2019). Its object is the Dalton-weighted standard deviation of position scores per legislative period, domain, and channel, defined in Section 4.8.3, compared between the two channels as levels, together with a compositional decomposition of system dispersion with and without the AfD. The two channels do not cover the same party set in every period, which matters here as it does for the trends of Section 4.8.3: in LP 18 the manifesto channel carries six parties against four in the speech channel, and the two additional programmes are those of the AfD and the FDP, so part of any cross-channel gap in that period is compositional rather than substantive. H2c owns the comparison of levels, H3 the change of those levels over time, and H2b the mapping of one channel onto the other within a party. A slope below one and a narrower spread are not the same finding, since a flatter mapping compresses the individual party while residual variation can leave the spread across parties where it was (Section 2.5). | 4.8-Bericht |
| 147 | 4.8.3 Hypothesis H3 | 48-C13 | entry in 2017 (Section 2.5); as an analysis of directional positions | entry in 2017 (Section 2.5). As an analysis of directional positions | 4.8-Bericht |
| 147 | 4.8.3 Hypothesis H3 | 48-C14 | defined in Section 4.8.2: per legislative period, domain, and channel | defined in Section 4.8.2. Per legislative period, domain, and channel | 4.8-Bericht |
| 147 | 4.8.3 Hypothesis H3 | 48-C15 | (Dalton, 2008); it is undefined where fewer than two parties carry a score. | (Dalton, 2008). The quantity is undefined where fewer than two parties carry a score. | 4.8-Bericht |
| 147 | 4.8.3 Hypothesis H3 | 48-C20b | which matters for every pre and post comparison | which matters for every pre/post comparison | 4.8-Bericht |
| 147 | 4.8.3 Hypothesis H3 | 48-C15 | ; it is undefined where fewer than two parties carry a score. | . The quantity is undefined where fewer than two parties carry a score. | 4.8-Bericht |
| 148 | 4.8.3 Hypothesis H3 | 48-C16 | with eight data points each, no significance language attaches to them. | with eight data points each. No significance language attaches to them. | 4.8-Bericht |
| 148 | 4.8.3 Hypothesis H3 | 48-C20a | Second, a pre and post 2017 contrast | Second, a pre/post-2017 contrast | 4.8-Bericht |
| 148 | 4.8.3 Hypothesis H3 | 48-A5a | Published work locates change at the parliamentary entry of a radical right competitor ( | Published work finds that the success of a radical right competitor also moves the established parties ( | 4.8-Bericht |
| 148 | 4.8.3 Hypothesis H3 | 48-A5b | ). Therefore, the refit isolates how much of any trend is carried by the entry itself. | ), so the refit cannot separate the two sources. What it isolates is the compositional part, the contribution the AfD's own position and weight make to a trend, while whatever its entry set in motion among the other parties remains in the refit and is not attributed to it. | 4.8-Bericht |
| 150 | 4.8.4 Analysis Overview | 48-C17 | Table 4.1 summarises the declarations of this section: for each analysis its layer, objects, benchmark, specification, and group. | Table 4.1 summarises the declarations of this section, giving for each analysis its layer, objects, benchmark, specification, and group. | 4.8-Bericht |
| 150 | 4.8.4 Analysis Overview | 48-C18 | The grouping binds the remainder of the thesis: Chapter 5 reports every analysis | The grouping binds the remainder of the thesis. Chapter 5 reports every analysis | 4.8-Bericht |
| 153 | 4.8.4 Analysis Overview | 48-B9a | Specifications refer to Section 4.7.2; M1 assigns each legislative period the manifesto of the preceding election. | Specifications refer to Section 4.7.2. The manifesto of each legislative period is that of the election from which the period emerged (Section 3.3). | 4.8-Bericht |
| 162 | 5.1 Analysis Base and Reading Conventions | K5-C18 | In this section we will discuss the results of our manifesto-speech comparison | This chapter reports the results of the manifesto-speech comparison | Kapitel-5-Bericht |
| 162 | 5.1 Analysis Base and Reading Conventions | K5-A1 | The results reported here follow the primary specification spelled out in Section 4.7.2, namely the full filter, code 305 excluded and native temperature. All deviations from this specification are marked as robustness arms. | The results reported here follow the primary specification of Section 4.7.2: the full filter at native decoding temperature throughout, with code 305 excluded on the salience layer (Aggregation A) and retained on the directional layer (Aggregation B), where it maps to no directional bucket and drops out under within-domain renormalisation. All deviations from this specification are marked as robustness arms. | Kapitel-5-Bericht |
| 163 | 5.1 Analysis Base and Reading Conventions | K5-C20 | A Jensen–Shannon divergence, our principal measure of manifesto-speech congruence as discussed in Section 4.7.3, carries no absolute reading | The Jensen–Shannon divergence, our principal measure of manifesto-speech congruence (Section 4.7.3), carries no absolute reading | Kapitel-5-Bericht |
| 163 | 5.1 Analysis Base and Reading Conventions | K5-B13b | the between-party reference scale introduced in Section 4.8. | the between-party reference scale introduced in Section 4.8.1. | Kapitel-5-Bericht |
| 163 | 5.1 Analysis Base and Reading Conventions | K5-B1a/term | That scale frames the size of a divergence between a lower and an upper bound | That scale frames the size of a divergence between a lower and an upper reference point | Kapitel-5-Bericht |
| 163 | 5.1 Analysis Base and Reading Conventions | K5-C19 | The numerical bounds will be introduced in detail in Section 5.2.1. | The numerical reference points are introduced in Section 5.2.1. | Kapitel-5-Bericht |
| 166 | 5.2.1 H1a | K5-B1a | Because a JSD carries no absolute reading, two bounds are introduced to give it a scale: a lower bound of 0.0074, the mean distance between speech agendas, and an upper bound of 0.0520, the mean distance between manifestos, about seven times higher. | Because a JSD carries no absolute reading, two reference points give it a scale: a lower reference of 0.0074, the mean distance between speech agendas, and an upper reference of 0.0520, the mean distance between manifestos, about seven times higher. Neither is a bound in the strict sense, since three cells, all in LP 13, exceed the upper reference. | Kapitel-5-Bericht |
| 166 | 5.2.1 H1a | K5-C23 | but only 0.57 of the distance between manifestos | but only 57% of the distance between manifestos | Kapitel-5-Bericht |
| 167 | 5.2.1 H1a | K5-C59 | Every one of the 38 points sits above the upper quartile of its own period, all 38 lie above the third quartile of the between-party agenda distances pooled across periods (0.0093) | Every one of the 38 points sits above the third quartile of its own period, and all 38 above the pooled third quartile of the between-party agenda distances (0.0093) | Kapitel-5-Bericht |
| 167 | 5.2.1 H1a | K5-C24 | The boxes sit low and stable from LP 14 to LP 18 and lift in LP 19 and 20. | The boxes sit low and remain stable from LP 14 to LP 18, and rise in LPs 19 and 20. | Kapitel-5-Bericht |
| 169 | 5.2.1 H1a | K5-C22 | as its mean sits at almost exactly halfway along the interval between the two bounds | as its mean sits almost exactly halfway between the two reference points | Kapitel-5-Bericht |
| 169 | 5.2.1 H1a | K5-B1b | Plenary speech neither reproduces the manifesto nor drifts as far from it as parties' programmes stand from one another, settling in between. | On average, plenary speech neither reproduces the manifesto nor drifts as far from it as parties' programmes stand from one another. | Kapitel-5-Bericht |
| 169 | 5.2.1 H1a | K5-B5+C21 | On the reference benchmark, then, the divergence stays below the programmatic differentiation between parties, as H1a expects, but not by a wide margin. Overall, it can be said that the divergence between channels is roughly in the middle of the scale between mean inner channel differences. | On the reference benchmark, the divergence stays below the programmatic differentiation between parties, but not by a wide margin. H1a as stated has two parts, and the two benchmarks separate them: the reference scale supports the first, since a party's speeches stay closer to its own manifesto than parties' programmes stand to one another, while the nearest-manifesto test of the next paragraph qualifies the second, since the own programme is recognisable far above chance but is not reliably the nearest. | Kapitel-5-Bericht |
| 170 | 5.2.1 H1a | K5-C3 | Their movement over time is not flat, however, as Figure 5.2 shows, in six diagrams, one for each party: within each, the party's divergence for every legislative period. | Their movement over time is not flat, however. Figure 5.2 shows this in six panels, one per party, each plotting that party's divergence for every legislative period. | Kapitel-5-Bericht |
| 170 | 5.2.1 H1a | K5-B3 | the points fall into the same rough U-shape, with divergence highest in LP 13, dropping to a trough in the middle of the window, and re-rising moderately from LP 18. | the points fall into a broadly similar shape: highest in LP 13, dropping to a trough somewhere between LP 14 and LP 17 depending on the party, and rising moderately again towards the end of the window. | Kapitel-5-Bericht |
| 170 | 5.2.1 H1a | K5-B2 | but its bars are so short that they are barely visible against the points, which means the shape of each party's series is not an artefact of sampling. | but its bars are so short that they are barely visible against the points, so the shape of each party's series is not an artefact of sentence-level sampling, which is the only source of uncertainty these intervals capture. | Kapitel-5-Bericht |
| 170 | 5.2.1 H1a | K5-C25 | its one exception the CDU/CSU in LP 19 at 0.0426 | its one exception being the CDU/CSU in LP 19, at 0.0426 | Kapitel-5-Bericht |
| 172 | 5.2.1 H1a | K5-A2 | These same four cells are the points that sit highest above their boxes in Figure 5.1. | The three highest points in Figure 5.1 are the LP-13 cells of the CDU/CSU, the Grüne and the SPD; the FDP's LP-13 cell sits high but not exceptionally so. | Kapitel-5-Bericht |
| 172 | 5.2.1 H1a | K5-C26 | we read it as a property of the period, not four coincident outliers | we read it as a property of the period, not as four coincident outliers | Kapitel-5-Bericht |
| 172 | 5.2.1 H1a | K5-B4 | The step from the 1990 to the 1994 manifesto is itself unremarkable by each party's long-run standard. | The step to the 1994 manifesto is unremarkable by the long-run standard of the three parties with a long coded record; for the Grüne, whose pre-1994 record is not captured by the party identifier used here, it is the largest of the observed transitions but small in absolute terms. | Kapitel-5-Bericht |
| 173 | 5.2.1 H1a | K5-B13c | The other benchmark discussed in Section 4.8 is | The other benchmark discussed in Section 4.8.1 is | Kapitel-5-Bericht |
| 173 | 5.2.1 H1a | K5-C9a | The own manifesto is the nearest in 42.1% of cells | A party's own manifesto is the nearest in 42.1% of cells | Kapitel-5-Bericht |
| 173 | 5.2.1 H1a | K5-C9b | and the own manifesto never ranks worse than fourth | and a party's own manifesto never ranks worse than fourth | Kapitel-5-Bericht |
| 173 | 5.2.1 H1a | K5-C7 | Where the non-matching agendas gravitate instead follows a roughly consistent pattern. | Which manifestos the non-matching agendas gravitate towards instead follows a roughly consistent pattern. | Kapitel-5-Bericht |
| 173 | 5.2.1 H1a | K5-A3+C8 | taking the mean divergence from all 38 speech cells to each party's manifesto in the same period, the FDP programme is the closest to the pool of parliamentary speech at 0.0253, ahead of the SPD (0.0379), the Grüne (0.0396), the Linke (0.0457), the CDU/CSU (0.0463) and the AfD (0.0638) | if we take the mean divergence from each of the 38 speech cells to every party's manifesto in the same period, the FDP programme sits closest to the pool of parliamentary speech at 0.0257, ahead of the SPD (0.0379), the Grüne (0.0396), the CDU/CSU (0.0463), the Linke (0.0524) and the AfD (0.0638) | Kapitel-5-Bericht |
| 173 | 5.2.1 H1a | K5-C28 | the institutional genre of a governing chamber | the institutional genre of a legislative chamber | Kapitel-5-Bericht |
| 177 | 5.2.2 H1b | K5-B6 | the restrictive share rises from 41.8% in manifestos to 68.4% in speeches. | the restrictive share rises from 42.9% in manifestos to 68.4% in speeches. | Kapitel-5-Bericht |
| 178 | 5.2.2 H1b | K5-B7 | a non-negligible part of the gap reflects national-way-of-life rather than immigration-control language | a non-negligible part of the gap reflects general national-identity rather than multiculturalism language | Kapitel-5-Bericht |
| 178 | 5.2.2 H1b | K5-C29 | (codes 607,608) | (607 and 608) | Kapitel-5-Bericht |
| 178 | 5.2.2 H1b | K5-A4a | while the pro-EU pole is the most reliably identified axis in the scheme. | while the pro-EU pole is among the most reliably identified labels in the scheme. | Kapitel-5-Bericht |
| 178 | 5.2.2 H1b | K5-C53a | (F1 0.148) | (F1 = 0.148) | Kapitel-5-Bericht |
| 179 | 5.2.2 H1b | K5-C51a | move furthest toward the restrictive pole | move furthest towards the restrictive pole | Kapitel-5-Bericht |
| 179 | 5.2.2 H1b | K5-B8 | while the two parties whose programmes are already restrictive move slightly the other way (CDU/CSU from 82.2% to 77.6%, AfD from 99.0% to 85.4%) | while the two parties whose programmes are already restrictive move in the other direction, the CDU/CSU marginally (82.2% to 77.6%) and the AfD more clearly (99.0% to 85.4%) | Kapitel-5-Bericht |
| 182 | 5.2.2 H1b | K5-B9+C4 | Read as consistent with, rather than as demonstrating, a systematic pattern, plenary speech leans more restrictive | H1b is read here as a composition claim rather than as an identification claim, since no nearest-manifesto counterpart is computed on the directional layer, which the weaker validation floor of Section 4.5 would not carry. These findings are read as consistent with, rather than as demonstrating, a systematic pattern: plenary speech leans more restrictive | Kapitel-5-Bericht |
| 188 | 5.3.1 H2a | K5-C31 | The breakdown of the mean divergence across the 9 topic buckets | The breakdown of the mean divergence across the nine topic buckets | Kapitel-5-Bericht |
| 188 | 5.3.1 H2a | K5-term-DPS | The DPS (Democracy & Political System) bucket holds | The Democracy & Political System bucket holds | Kapitel-5-Bericht |
| 188 | 5.3.1 H2a | K5-C33 | cross-domain topic classification artifact | cross-domain topic-classification artefact | Kapitel-5-Bericht |
| 188 | 5.3.1 H2a | K5-C5 | Still, this is to be treated as only partly substantive, although still likely holding some of the forced misclassification. | This contribution should nevertheless be treated as only partly substantive, since the bucket probably still absorbs some forced misclassification. | Kapitel-5-Bericht |
| 188 | 5.3.1 H2a | K5-C32 | The smallest part of the JSD distribution is carried by European Integration | The smallest contribution comes from European Integration | Kapitel-5-Bericht |
| 191 | 5.3.1 H2a | K5-C12 | Democracy & Political System, the language of government and Foreign Policy & Defence. | Democracy & Political System — the language of government — and Foreign Policy & Defence. | Kapitel-5-Bericht |
| 191 | 5.3.1 H2a | K5-C13 | Figure 5.6 shows this directly, as in the Democracy & Political System and Foreign Policy panels every party's speech share sits above its manifesto share | Figure 5.6 shows this directly: in the Democracy & Political System and Foreign Policy & Defence panels, every party's speech share sits above its manifesto share | Kapitel-5-Bericht |
| 192 | 5.3.1 H2a | K5-C51b | In speech these shares move toward one another. | In speech these shares move towards one another. | Kapitel-5-Bericht |
| 192 | 5.3.1 H2a | K5-C14 | What on first glance looks like the left playing welfare down | What at first glance looks like the left downplaying welfare | Kapitel-5-Bericht |
| 192 | 5.3.1 H2a | K5-C15 | is taken up in broader scope in Chapter 6. | is taken up more broadly in Chapter 6. | Kapitel-5-Bericht |
| 196 | 5.3.2 H2b | K5-C48 | a position score in [-1, 1] | a position score in [−1, 1] | Kapitel-5-Bericht |
| 196 | 5.3.2 H2b | 48-B10b | We code the right-leaning pole as positive and the left-leaning pole as negative. | We code one pole of each domain as positive, namely the market-liberal, retrenchment, restrictive and EU-sceptic side, and the opposing pole as negative. | 4.8-Bericht |
| 196 | 5.3.2 H2b | K5-C10 | Domain score = (right wing direction – left wing direction) / (sum both directions) | domain score = (positive pole − negative pole) / (sum of both poles) | Kapitel-5-Bericht |
| 196 | 5.3.2 H2b | K5-B10 | since it is the only domain with a sizeable non-directional category. | since Economy is the only domain in scheme B that carries a non-directional category at all (Section 4.7.1). | Kapitel-5-Bericht |
| 199 | 5.3.2 H2b | K5-C34 | (n = 38, all valid party-period cells, except for migration here n = 37) | (n = 38, all valid party-period cells; for migration, n = 37) | Kapitel-5-Bericht |
| 199 | 5.3.2 H2b | K5-C52a | The r² reports how closely the cells fall along the fitted line | R² reports how closely the cells fall along the fitted line | Kapitel-5-Bericht |
| 200 | 5.3.2 H2b | K5-B11 | Where the compressed positions sit is set by the intercept. | Where the compressed positions sit is read off the intercept together with the slope. | Kapitel-5-Bericht |
| 200 | 5.3.2 H2b | K5-C49 | the mean speech position equals β0 + β1 · mean manifesto position | the mean speech position equals β₀ + β₁ · mean manifesto position | Kapitel-5-Bericht |
| 201 | 5.3.2 H2b | K5-C52b | with r² between 0.53 and 0.78 | with R² between 0.53 and 0.78 | Kapitel-5-Bericht |
| 201 | 5.3.2 H2b | K5-B12 | so the parties are drawn closer together without changing their order. | so the parties are drawn closer together without changing their order. The 38 cells are six parties observed over eight periods rather than independent draws, so these fits are read as descriptions of the pooled cloud, not as estimates with a sampling interpretation (Section 4.8.2). | Kapitel-5-Bericht |
| 202 | 5.3.2 H2b | K5-C6 | its intercept of +0.40 lifting even a balanced manifesto across the line, a pooled comparison across cells whose development over time Section 5.4 takes up. | its intercept of +0.40 lifting even a balanced manifesto across the line. This is a comparison pooled across cells; its development over time is taken up in Section 5.4. | Kapitel-5-Bericht |
| 206 | 5.3.2 H2b | K5-A5 | (ganzer Absatz) | This result is read with caution, because the finding at issue, that migration shifts towards the restrictive pole, rests on one of the least reliably assigned labels in the scheme: restrictive-migration precision against the silver reference is 0.466, and contra-EU is only a little higher at 0.525. Only the welfare-retrenchment label is weaker still, and no reading in this chapter rests on it. Part of the migration shift may therefore be over-assignment of the restrictive label rather than a real relocation. The manifesto side is coded from MARPOR and is untouched, and the concern is the direction of the shift rather than its compression. | Kapitel-5-Bericht |
| 208 | 5.3.3 H2c | K5-B13a | See Section 4.8 for the methodological discussion of the measures we employ here. | See Section 4.8.3 for the methodological discussion of the measures we employ here. | Kapitel-5-Bericht |
| 209 | 5.3.3 H2c | K5-C17 | although the size of the effects is largely heterogeneous | although the size of the gap varies considerably across domains | Kapitel-5-Bericht |
| 209 | 5.3.3 H2c | K5-C11+E1 | Concretely for the economy domain the dispersion is 0.396 on the manifesto side vs. 0.144 on the speech side in mean value over all periods, on migration we see 0.574 on the manifesto side vs. 0.140 on the speech side, welfare 0.107 on the manifesto side vs. 0.036 on the speech side and Europe 0.366 on the manifesto side vs. 0.232 on the speech side. | Averaged over all periods, dispersion falls from 0.386 on the manifesto side to 0.138 on the speech side in the Economy domain, from 0.551 to 0.136 on Migration, from 0.104 to 0.035 on Welfare, and from 0.345 to 0.222 on Europe. | Kapitel-5-Bericht |
| 209 | 5.3.3 H2c | K5-C55 | because its parties start the furthest apart | because its parties start furthest apart | Kapitel-5-Bericht |
| 209 | 5.3.3 H2c | K5-C36 | as a response to the common parliamentary agenda, while why the chamber compresses positions in this way is taken up in Chapter 6. | as a response to the common parliamentary agenda; why the chamber compresses positions in this way is taken up in Chapter 6. | Kapitel-5-Bericht |
| 210 | 5.3.3 H2c | K5-C37 | an emergence Section 5.4 traces | an emergence that Section 5.4 traces | Kapitel-5-Bericht |
| 214 | 5.4 Polarisation over Time (H3) | K5-C38 | , measured by the Dalton-weighted standard deviation of position scores, the same measure as used in Section 5.3.3, increases |  — measured by the Dalton-weighted standard deviation of position scores, the same measure used in Section 5.3.3 — increases | Kapitel-5-Bericht |
| 217 | 5.4 Polarisation over Time (H3) | K5-C52c | r² reports how closely the eight points follow a straight line | R² reports how closely the eight points follow a straight line | Kapitel-5-Bericht |
| 217 | 5.4 Polarisation over Time (H3) | K5-C40 | and where they diverge it is carried by the changed composition | and where they diverge, it is carried by the changed composition | Kapitel-5-Bericht |
| 218 | 5.4 Polarisation over Time (H3) | E1-Europe-slopes | at a manifesto slope of 0.0667 per legislative period (SE 0.0191, p = 0.013) and a speech slope of 0.0344 (SE 0.0094, p = 0.011) | at a manifesto slope of 0.0769 per legislative period (SE 0.0139, p = 0.001) and a speech slope of 0.0407 (SE 0.0080, p = 0.002) | 4.8-Bericht (48-A1) + Neulauf |
| 218 | 5.4 Polarisation over Time (H3) | K5-C39 | Figure 5.9 shows the rise is concentrated | Figure 5.9 shows that the rise is concentrated | Kapitel-5-Bericht |
| 218 | 5.4 Polarisation over Time (H3) | E1-Europe-refit | the manifesto slope flipping from 0.0667 to −0.0181 and the speech slope falling to 0.0050 | the manifesto slope flipping from 0.0769 to −0.0079 and the speech slope falling to 0.0113 | 4.8-Bericht (48-A1) + Neulauf |
| 218 | 5.4 Polarisation over Time (H3) | 48-A5/5.4 | The sign-flip locates the rise in the AfD's entry rather than in any movement of the established parties | The sign-flip locates the rise in the AfD's own position rather than in any movement of the established parties | 4.8-Bericht |
| 219 | 5.4 Polarisation over Time (H3) | K5-A6 | (ganzer Absatz) | This is less surprising than it looks. That a weighted dispersion measure jumps when one party enters at the pole opposite all the others is arithmetically expected, and Europe is the least salient of the four domains, so its scores rest on little text. What the decomposition adds is the quantification: almost the entire rise is compositional. The established parties do not move apart on this domain — in the manifesto channel they in fact converge sharply, their dispersion excluding the AfD falling from 0.261 in LP 18 to 0.014 in LP 19, which is what turns the refitted slope negative. That is the caution a system-level polarisation series calls for: a rising curve need not mean parties are drifting apart, and here it coincides with the established parties moving closer together. The speech side may in addition be inflated by the over-assignment of contra-EU, taken up below, while the manifesto series is MARPOR-coded and unaffected. | Kapitel-5-Bericht |
| 226 | 5.4 Polarisation over Time (H3) | E1-econ | the speech slope is +0.0164 (p = 0.018) and stays practically unchanged without the AfD (+0.0171) | the speech slope is +0.0195 (p = 0.016) and stays practically unchanged without the AfD (+0.0201) | 4.8-Bericht (48-A1) + Neulauf |
| 226 | 5.4 Polarisation over Time (H3) | E1-welf | where the slope is −0.0252 (p = 0.016) and equally robust to the exclusion (−0.0250) | where the slope is −0.0240 (p = 0.022) and equally robust to the exclusion (−0.0238) | 4.8-Bericht (48-A1) + Neulauf |
| 226 | 5.4 Polarisation over Time (H3) | K5-B14+E1 | while the speech series stays flat (−0.0001, p = 0.96) | while the speech series stays flat (+0.0002 against a standard error of 0.0026, indistinguishable from zero at this resolution) | Kapitel-5-Bericht |
| 226 | 5.4 Polarisation over Time (H3) | E1-mig | the speech slope of +0.0160 (p = 0.004) drops to +0.0112 (p = 0.005) without the AfD | the speech slope of +0.0181 (p = 0.001) drops to +0.0132 (p = 0.001) without the AfD | 4.8-Bericht (48-A1) + Neulauf |
| 226 | 5.4 Polarisation over Time (H3) | E1-mig-man | The migration manifesto slope is close to zero on either side of the exclusion and we read it as flat rather than as a second sign flip. | The migration manifesto slope is close to zero on either side of the exclusion, at +0.0154 with all parties and +0.0007 without the AfD, so we read it as flat. | 4.8-Bericht (48-A1) + Neulauf |
| 226 | 5.4 Polarisation over Time (H3) | K5-B15 | the movements in the other domains are weaker but more substantive, since they reflect actual repositioning within the established party set. | the movements in the other domains are weaker but harder to attribute to composition, since they persist within the established party set. | Kapitel-5-Bericht |
| 227 | 5.4 Polarisation over Time (H3) | E1-migdisp | Speech dispersion nearly doubles across the window, from 0.113 in LP 13 to 0.221 in LP 20, while the weighted mean moves in the opposite direction, from +0.544 to +0.344 | Speech dispersion more than doubles across the window, from 0.104 in LP 13 to 0.221 in LP 20, while the weighted mean moves in the opposite direction, from +0.554 to +0.344 | 4.8-Bericht (48-A1) + Neulauf |
| 227 | 5.4 Polarisation over Time (H3) | K5-C42 | that is towards the liberal pole rather than the restrictive one | that is, towards the liberal pole rather than the restrictive one | Kapitel-5-Bericht |
| 227 | 5.4 Polarisation over Time (H3) | K5-C41 | Disaggregated to the parties the pattern is a fan rather than a drift. | Disaggregated by party, the pattern is a fan rather than a drift. | Kapitel-5-Bericht |
| 227 | 5.4 Polarisation over Time (H3) | K5-A7 | Between LP 13 and LP 20 every established party except the CDU/CSU moves towards the liberal pole, the Grüne from +0.33 to +0.12, the SPD from +0.48 to +0.20 and the FDP from +0.59 to +0.32, while the CDU/CSU moves least, from +0.65 to +0.54. | Between LP 13 and LP 20 every established party moves towards the liberal pole — the Grüne from +0.33 to +0.12, the SPD from +0.48 to +0.20, the FDP from +0.59 to +0.32 — and the CDU/CSU moves least, from +0.65 to +0.54. | Kapitel-5-Bericht |
| 228 | 5.4 Polarisation over Time (H3) | E1-window | the weighted mean moves by −0.21 on economy and by −0.12 on welfare in speech while the corresponding dispersions move by +0.06 and −0.02, and on the manifesto side the migration mean moves by −0.47 | the weighted mean moves by −0.22 on economy and by −0.12 on welfare in speech while the corresponding dispersions move by +0.07 and −0.01, and on the manifesto side the migration mean moves by −0.54 | 4.8-Bericht (48-A1) + Neulauf |
| 232 | 5.4 Polarisation over Time (H3) | E1-prepost | the manifesto mean rises from 0.2807 to 0.6219 (+0.3412) and the speech mean from 0.1785 to 0.3942 (+0.2157) | the manifesto mean rises from 0.2527 to 0.6219 (+0.3692) and the speech mean from 0.1650 to 0.3942 (+0.2291) | 4.8-Bericht (48-A1) + Neulauf |
| 232 | 5.4 Polarisation over Time (H3) | E1-prepost2 | +0.0826 for migration and +0.0755 for economy on the speech side and −0.0774 for welfare on the manifesto side | +0.0878 for migration and +0.0836 for economy on the speech side and −0.0738 for welfare on the manifesto side | 4.8-Bericht (48-A1) + Neulauf |
| 232 | 5.4 Polarisation over Time (H3) | K5-C43 | but as Figure 5.9 shows the Europe path is closer to a jump | but, as Figure 5.9 shows, the Europe path is closer to a jump | Kapitel-5-Bericht |
| 232 | 5.4 Polarisation over Time (H3) | K5-C44 | With six pre and only two post periods this is a description, not a test | With six pre-entry and only two post-entry periods, this is a description, not a test | Kapitel-5-Bericht |
| 233 | 5.4 Polarisation over Time (H3) | K5-B16 | (precision 0.525, F1 0.634), nearly twice as often as the Sonnet reference does, while the gold standard contains too few contra-EU sentences to quantify the label at all (n = 3). | (precision 0.525, F1 = 0.634), applying the label about one and a half times as often as the Sonnet reference does, while the gold standard contains too few contra-EU sentences to quantify it at all (n = 3). | Kapitel-5-Bericht |
| 233 | 5.4 Polarisation over Time (H3) | K5-A4b | The asymmetry matters for how far the two sides can be trusted, pro-EU being the most robust label of the scheme (F1 = 0.800), so the pro-EU side of the series is more reliable than the contra-EU side. | The asymmetry matters for how far the two sides can be trusted: the pro-EU pole reaches an F1 of 0.800 against the contra-EU pole's 0.634, so the pro-EU side of the series is the more reliable one. | Kapitel-5-Bericht |
| 235 | 5.5 Robustness | K5-A8 | In this section we discuss the robustness tests we have undertaken, all of them with acceptance criteria fixed before the analysis (Section 4.9), and report the outcomes as they came out. | This section reports the robustness battery of Section 4.9. All six arms were specified in advance; arms (1) to (3) carry pre-committed acceptance criteria, while arms (4) to (6) are sensitivity designs that report the shift a variant induces. The outcomes are reported as they came out. | Kapitel-5-Bericht |
| 235 | 5.5 Robustness | K5-B17 | We specifically test both aggregations by applying arms 1 and 2 to H1a on Aggregation A and to H1b on Aggregation B. Because H2 and H3 rest on these same two aggregations, testing both here covers the measurement construction they share, so the sweeps are not repeated on them. | Arms 1 and 2 are applied to H1a on Aggregation A and to H1b on Aggregation B, so both aggregations are tested at the layer at which they are constructed. They are not repeated for H2 and H3: those analyses use a different functional of the same distributions, a within-domain ratio rather than a divergence, so the rank stability established here does not transfer to them automatically, and their results carry the corresponding caution on top of the validation floor of Section 4.5. | Kapitel-5-Bericht |
| 239 | 5.5 Robustness | K5-C2 | since both are calibrations, the pipeline could plausibly have run under. | since both are calibrations under which the pipeline could plausibly have run. | Kapitel-5-Bericht |
| 239 | 5.5 Robustness | K5-A9 | The flat variant is not one of them. Raising the temperature to 2 pushes every posterior towards the uniform distribution, so it enters the battery as a lower bound rather than a rival specification, a placebo that shows the measure moves at all rather than returning the same ordering whatever it is fed. It fails on both layers, and it inflates the divergence itself, from 0.0297 to 0.0368 on H1a and from 0.0506 to 0.0852 on H1b. The reshuffling is what extreme flattening does rather than an accident, and that it reshuffles is the reassuring part, though the fixed criterion is missed all the same. | The flat variant is not one of them. It fails on both layers, and it inflates the divergence itself, from 0.0297 to 0.0368 on H1a and from 0.0506 to 0.0852 on H1b. The pre-committed criterion is missed, and we report it as missed. Read after the fact, the failure is uninformative about the primary specification rather than a threat to it: raising the temperature to 2 pushes every posterior towards the uniform distribution, which is not a calibration the pipeline could plausibly have run under. What the arm establishes in retrospect is that the measure responds at all rather than returning the same ordering whatever it is fed. | Kapitel-5-Bericht |
| 240 | 5.5 Robustness | K5-C60a | plots the temperature sweep cell by cell, where the flat variant visibly reshuffles | plots the temperature sweep cell by cell, showing that the flat variant visibly reshuffles | Kapitel-5-Bericht |
| 240 | 5.5 Robustness | K5-C60b | plots each hard-threshold variant against the soft baseline, where the points track the identity line | plots each hard-threshold variant against the soft baseline; in it the points track the identity line | Kapitel-5-Bericht |
| 241 | 5.5 Robustness | K5-B19 | Arm 4, a tightened prefilter of at least eight words per sentence, was computed as a sensitivity check on H2a and returns near-perfect rank agreement with the primary specification, so it is subsumed by the full filter and not tabulated separately. | Arm 4, a tightened prefilter of at least eight words per sentence, was specified as a sensitivity check on H2a. It is subsumed by the full filter, which already removes the sentences it targets, and is therefore not reported separately. | Kapitel-5-Bericht |
| 241 | 5.5 Robustness | 48-A5/5.5 | where it localises the European rise to the party's entry. | where it localises the European rise to the party's own position and weight. | 4.8-Bericht |
| 242 | 5.5 Robustness | K5-C46 | , while the mean divergence falls from 0.0715 and 0.0333 without a filter to 0.0641 and 0.0322 under the procedural filter and to 0.0510 and 0.0297 under the full filter, for the included and excluded bands respectively. | . For the code-305-included bands the mean divergence falls from 0.0715 without a filter to 0.0641 under the procedural filter and 0.0510 under the full filter; for the excluded bands the corresponding values are 0.0333, 0.0322 and 0.0297. | Kapitel-5-Bericht |
| 249 | Conclusion | K5-A3/Concl | on average the closest programme to the pooled parliamentary speech (0.0253) | on average the closest programme to the pooled parliamentary speech (0.0257) | Kapitel-5-Bericht |
| 250 | Conclusion | K5-A6/Concl | flips the manifesto slope from +0.0667 to −0.0181 and drops the speech slope to near zero, while the established parties barely move on this domain. | flips the manifesto slope from +0.0769 to −0.0079 and drops the speech slope to near zero, while the established parties do not move apart on this domain — in the manifesto channel they converge sharply on one another. | Kapitel-5-Bericht |
| 250 | Conclusion | K5-A6/Concl2 | The established parties neither converge on the entrant nor move away from it on this domain, so the rise is composition | The established parties do not move apart on this domain, so the rise is composition | Kapitel-5-Bericht |
| 251 | Conclusion | K5-A7/Concl | since every established party except the CDU/CSU moves towards the liberal pole in speech across the window while the CDU/CSU moves least | since every established party moves towards the liberal pole in speech across the window, the CDU/CSU least of all | Kapitel-5-Bericht |
| 251 | Conclusion | K5-A6/Concl3 | since there the established parties barely move at all and the rise is carried by the arrival alone | since there the established parties do not move apart at all and the rise is carried by the arrival alone | Kapitel-5-Bericht |
| 658 | Appendix D: Silver-Standard Prompt | 47-M2 | The 32 codes outside these four domains fall into a residual category that drops out when the directional domains are renormalised; code 305 in particular maps to no directional bucket | The remaining 32 codes fall into a residual category that drops out when the directional buckets are renormalised. Three of them — 406, 415 and 502 — lie inside the four domains under Aggregation A but carry no direction on the relevant axis, so scheme B is not a refinement of scheme A. Code 305 in particular maps to no directional bucket | 4.7-Bericht |
| 667 | Appendix E: Code-to-Bucket Crosswalk | 48-B7c | every interval is a 1000-draw non-parametric bootstrap (seed 20260507) | every interval is a 1,000-draw non-parametric bootstrap (per-cell seeds derived from the base seed 20260507) | 4.8-Bericht |
| 684 | F.2 Per-cell salience divergence (H1a) with bo | K5-F3 | 49.8 % under the procedural filter | 49.7 % under the procedural filter | Kapitel-5-Bericht |
| 699 | F.3 The code-305 correction: six specification | K5-F4b | against a 21.1 % chance baseline of one in the average field of 4.7 parties | against a 21.1 % chance baseline, the mean of one over the field size in each cell, with fields ranging from four to six parties | Kapitel-5-Bericht |
| 704 | F.4 Nearest-manifesto benchmark (identificatio | K5-F4a | the mean Jensen–Shannon divergence from all 38 speech cells to that manifesto in the same period | the mean Jensen–Shannon divergence from each of the 38 speech cells to that manifesto in the same period (n = 38 for every party except the AfD, n = 12) | Kapitel-5-Bericht |
| 732 | F.7 Soft-assignment versus hard-threshold clas | 47-M3/K6 | F.8 Migration re-split (H1b, buckets 601/608 vs 602/607; robustness arm 5) | F.8 Migration re-split (H1b, multiculturalism-only axis 608 vs 607; robustness arm 5) | 4.7-Bericht |
---

## 3. Tabellen

### Table 4.1 (Analysis overview)

| Zeile / Spalte | alt | neu | Herkunft |
|---|---|---|---|
| H1a · Specification | full filter × 305 excl × native τ, M1 | full filter × 305 excl × native decoding temperature | 48-B9b |
| H1b · Object | within-domain JSD + directional composition | per-cell divergence across the nine directional sub-buckets + within-domain composition | 48-A4b |
| H1b · Benchmark | 607/608 re-split as pre-committed sensitivity (arm 5) | 608/607 re-split as pre-committed sensitivity (arm 5) | 48-C22 |
| H2a · Benchmark | none | even-spread reference (1/9 = 11.1% per bucket); sign consistency of the per-bucket differences across parties | 48-B2 + 48-B1 |
| H2b · Object | polarisation score per cell; OLS speech ~ manifesto | polarisation score per cell; OLS speech ~ manifesto (slope and intercept) | 48-B3 |

### Table 5.2 (Polarisation trend slopes) — alle acht Zeilen neu (E1)

| Domain | Channel | Slope (SE) | p | excl. AfD | p | Δ slope |
|---|---|---|---|---|---|---|
| Economy | Manifesto | +0.0208 (0.0102) | 0.086 | +0.0143 | 0.242 | +0.0065 |
| Economy | Speech | +0.0195 (0.0059) | 0.016 | +0.0201 | 0.014 | −0.0007 |
| Welfare | Manifesto | −0.0240 (0.0078) | 0.022 | −0.0238 | 0.023 | −0.0002 |
| Welfare | Speech | +0.0002 (0.0026) | 0.932 | −0.0018 | 0.446 | +0.0020 |
| Migration | Manifesto | +0.0154 (0.0183) | 0.431 | +0.0007 | 0.969 | +0.0147 |
| Migration | Speech | +0.0181 (0.0032) | 0.001 | +0.0132 | 0.001 | +0.0049 |
| Europe | Manifesto | +0.0769 (0.0139) | 0.001 | −0.0079 | 0.725 | +0.0848 |
| Europe | Speech | +0.0407 (0.0080) | 0.002 | +0.0113 | 0.011 | +0.0295 |

Die Spalte *Δ slope* ist zahlengleich mit der alten Fassung, wie der 4.8-Bericht vorhergesagt hatte.

### Table F.4.2 (Agenda centre of gravity) — E2, Variante A

| Manifesto | Mean JSD (all) | Mean JSD (excl. own) | Times nearest |
|---|---|---|---|
| FDP | 0.0253 → **0.0257** | 0.0263 → **0.0267** | 18 (unverändert) |
| SPD | 0.0379 | 0.0400 | 6 |
| Grüne | 0.0396 | 0.0422 | 7 |
| **CDU/CSU** (rückt vor) | 0.0463 | 0.0484 | 5 |
| **Linke** (rückt zurück) | 0.0457 → **0.0524** | 0.0503 → **0.0564** | 0 |
| AfD | 0.0638 | 0.0705 | 2 |

Spalte *Times nearest* stammt aus `benchmark_nearest_manifesto.csv` und ist **unverändert** — sie ist eine andere Größe als die Centre-of-Gravity-Spalten.

### Table F.10.2

| alt | neu | Herkunft |
|---|---|---|
| Spaltenkopf „Rank of 1994" | „Rank of 1994 (1 = largest)" | K5-B4 |

---

## 4. Abbildungs- und Tabellenunterschriften (K5-B20, K5-B18)

Zehn Abbildungen trugen nur „Figure 5.x" in der Formatvorlage *Beschriftung*, Table 5.2 und Table 5.3 nur „Table 5.x" in *Standard*. Alle zwölf haben jetzt eine vollständige Unterschrift; die beiden Tabellenunterschriften stehen jetzt ebenfalls in der Formatvorlage *Beschriftung* und erscheinen damit in einem Tabellenverzeichnis.

Betroffen: Figure 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 5.10, Table 5.2, Table 5.3.

Zwei Unterschriften wurden gegenüber dem Berichtsvorschlag angepasst: Figure 5.2 nennt statt des Labels „M1" die Zuordnungsregel im Klartext (Folge aus 48-B9), Figure 5.3 nennt 38 Redezellen (Folge aus E2).

---

## 5. Fußnoten

| Fußnote | alt | neu | Herkunft |
|---|---|---|---|
| 14 (zu 5.2.2) | „FDP LP13 has no migration content and is taken as 0, over the 37 cells that carry migration content the manifesto figure is 42.9%." | „The FDP's 1994 manifesto carries no migration-coded material, so its migration score is undefined; all migration shares are computed over the 37 cells that carry migration content on the manifesto side." | K5-B6 / K5-C58 |
| 15 (zu 5.3.2) | „This is due to the fact that…" | „This is because…" | K5-C56 |
| 17 (zu 5.4) | „The dispersion series is computed over every party carrying weight in the period, which includes the PDS in LPs 13 to 15. … the migration speech slope moving from +0.0160 to +0.0181 and the European manifesto slope from +0.0667 to +0.0722." | „The dispersion series is computed on the 38-cell analysis sample, so the PDS cells of LPs 13 to 15 are outside it in both channels. The party-continuity decision of Section 4.8 applies to the assignment of manifestos to parties across the 2007 merger rather than to which parties held seats." | E1 |

Die alte Fußnote 17 nannte für den Europa-Manifest-Slope **+0.0722**; der tatsächliche Wert der restringierten Rechnung ist **+0.0769**. Der Vorabwert war falsch.

---

## 6. Neuer Abschnitt 5.6 (K5-B22)

„**5.6 Summary of Findings**" am Kapitelende, ein Urteilssatz je Hypothese. Alle Aussagen wurden gegen die neuen Zahlen geprüft und halten (H2c: 31 von 32 Paaren; H2a: DPS 20.9 %; H1a: 0.57 bzw. 42.1 %).

---

## 7. Dokumentweite Sweeps (Phase 4)

| Sweep | ersetzt | betroffene Blöcke |
|---|---|---|
| „Jensen-Shannon" → „Jensen–Shannon" | 7 | 3, 60, 63, 101, 247, 255, 671 |
| „Kullback-Leibler" → „Kullback–Leibler" | 0 (einziges Vorkommen lag in 4.7 und ist mit der Neufassung erledigt) | — |
| „Democracy and Political System" → „&" | 15 | 43, 85, 97, 106, 107, 109, 249, 671, 686, 688, 689, 699 |
| „Foreign Policy and Defence" → „&" | 3 | 249, 689 |
| „polarization" → „polarisation" | 4 | 741, 743, 747 (Anhang F.9) |
| „ toward " → „ towards " | 1 | 370 |
| „manifesto-speech" → „manifesto–speech" | 10 | 143, 162, 163, 167, 168, 186, 189, 190 |
| „artifact" → „artefact" | 0 (mit K5-C33 bereits erledigt) | — |
| „silver standard" → „silver-standard" | 0 (lag in 4.7, mit der Neufassung erledigt) | — |
| abschließende Leerzeichen am Absatzende (48-C19) | 18 Absätze | dokumentweit |

**Nicht angefasst** (Sperrzonen): Literaturverzeichnis, Anhang B, Anhang D, „Erklärung an Eidesstatt", sowie die MARPOR-Kategorienamen in der Tabelle von Anhang E — dort steht „Law and Order: Positive" als Code-Label (605) und ist ein Zitat.

---

## 8. Verifikation (Phase 5)

| Prüfung | Ergebnis |
|---|---|
| Ankerverifikation vor dem Einbau | **148 von 148** Ankern eindeutig im Dokument gefunden, keiner ins Leere |
| Angewandte Textänderungen | **149 von 149** Ersetzungen angewandt, keine Fehlschläge |
| Hyperlinks | keine Ersetzung hat einen Hyperlink zerschnitten; bei den vier Blockersätzen wurden die enthaltenen Belege (`Ref_Gentzkow_2019`, `Ref_AbouChadi_2020`) wiederhergestellt |
| Abschnittsverweise | alle 37 „Section x.y"-Verweise lösen gegen die Überschriftenstruktur auf; alle 15 Anhangverweise ebenfalls |
| Wiederholte Zahlen | 0.0297 (9×), 0.0510 (4×), 41.562 / 106.641 / 279.947, n = 38 (8×), κ(F) 0.363 (2×), κ(B) 0.456 (6×), 20.9 % (5×), 42.1 % (6×), 21.1 % (6×), 1.87, 2.95, 0.0074, 0.0520 — überall übereinstimmend |
| Zahlenkollision 49,8 % | aufgelöst: 4.6.2 und F.1 behalten 49,8 % (korrekt), F.3 steht jetzt auf 49,7 % |
| Formelsatz | beide Gleichungen als OMML vorhanden; π_c mit `nary`/`sub`/`sup`, ∈ = U+2208; JSD mit 4× `m:sty val="p"` und ∥ = U+2225; beide in `<m:oMathPara>` |
| Diff gegen die Basisdatei | 69 geänderte Bereiche, 135 betroffene Blöcke — jede einzelne Abweichung einem Befund oder einem Sweep zuordenbar, keine unbeabsichtigte Änderung |
| Datei-Integrität | ZIP und XML gültig, Datei öffnet sich wieder, Testrendering nach PDF (149 Seiten) läuft durch |

*Hinweis zum Testrendering:* LibreOffice stellt die beiden OMML-Gleichungen nicht dar — das gilt für die Neufassungsdatei genauso wie für die Endfassung und ist eine Grenze von LibreOffice, kein Schaden an der Datei. In Word werden sie gesetzt.
