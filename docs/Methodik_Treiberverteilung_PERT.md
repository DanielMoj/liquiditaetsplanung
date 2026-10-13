# Methodik-Notiz: Wahl der Treiberverteilung — PERT statt empirischem Fit

**Zweck.** Diese Notiz begründet eine Modellentscheidung im Detail: Warum werden die Jahreswerte der Treiber in Blatt `06_Verteilungen` als PERT-Verteilung über `p_min`/`mode`/`p_max` angegeben, statt eine Verteilungsfunktion an die historischen Werte zu fitten? Sie ergänzt die *Methodendokumentation* (dort §1.2 zu `distributions.R`, §2.9 zu den Annahmen, §2.11 zur Varianzzerlegung) und nutzt dieselbe Notation: $j$ Treiber, $\theta_j$ sein Jahreswert, $m$ Monat des Horizonts, $T = \min_m B_m$ der Liquiditäts-Tiefpunkt.

---

## 1 Die naheliegende Alternative

Der intuitive Weg lautet: historische Werte eines Treibers nehmen, eine Verteilung daran fitten, daraus ziehen — fertig. Das ist die **richtige** Methode, wenn drei Voraussetzungen erfüllt sind:

1. **Genug Beobachtungen**, um Lage, Streuung und Form (Schiefe, Ränder) zu schätzen.
2. **Stationarität** — die Vergangenheit und die Zukunft folgen demselben datenerzeugenden Prozess.
3. **Stichprobencharakter** — die interessierende Größe ist eine Ziehung aus einer Häufigkeitsverteilung.

Die Jahreswerte der hier relevanten Treiber (Tarifabschluss, Landesbasisfallwert, Pflegebudget-Anpassung, Mengen- und Materialentwicklung über ein Planjahr) verletzen alle drei. Genau deshalb wird ihr Niveau als Urteil modelliert, nicht gefittet.

---

## 2 Zwei Unsicherheiten, zwei Methoden

Das Modell trennt durchgehend zwei Arten von Unsicherheit (vgl. Kopf von `distributions.R`):

- **Aleatorisch** — die irreduzible monatliche Streuung um den projizierten Pfad. Sie wiederholt sich Monat für Monat und ist innerhalb eines Jahres näherungsweise stationär. Sie wird **aus Daten geschätzt** (die `cv`, §5).
- **Epistemisch** — das Unwissen über den künftigen *Jahreswert* eines Treibers. Das ist ein Grad des Glaubens über eine einzelne, noch nicht eingetretene Zahl. Er wird **als PERT-Urteil** angegeben (§4).

Formal überlagert ein Lauf der genesteten Simulation beide: ein gezogener Jahreswertvektor $\boldsymbol\theta = (\theta_1,\dots,\theta_k)$ (epistemisch, äußere Schicht) und ein multiplikatives Monatsrauschen $\varepsilon_{j,m}$ mit Standardabweichung $\approx \mathrm{cv}_j$ (aleatorisch, innere Schicht), beides durch dieselbe Funktion $f(\boldsymbol\theta,\boldsymbol\varepsilon)$. Die Aufteilung ist also keine Geschmacksfrage, sondern die Achse, um die das ganze Modell gebaut ist.

---

## 3 Warum ein Fit am Jahres-Niveau nicht trägt

### 3.1 Zu wenige Beobachtungen

Ein Treiber wie der jährliche Tarifabschluss hat typischerweise fünf bis zehn Jahresbeobachtungen. Aus so wenigen Punkten lässt sich keine Verteilung seriös schätzen: zentrale Lage vielleicht, aber Schiefe und vor allem die **Ränder** sind dominiert von Stichprobenrauschen — faktisch frei gewählt durch die Wahl der Verteilungsfamilie. Der Tiefpunkt $T$ ist als Monatsminimum stark randgetrieben (vgl. *Methodendokumentation* §2.11): er reagiert gerade auf die seltenen, extremen Realisationen. Ein Fit liefert dort scheinbare Präzision, wo keine Information vorliegt.

### 3.2 Nicht-Stationarität

Tarifabschluss, Landesbasisfallwert und Pflegebudget sind Ergebnisse von Verhandlung und Regulierung, keine Ziehungen aus einem festen Gesetz. Regimewechsel sind die Regel, nicht die Ausnahme (Inflationssprung 2022/23, Reformen, Einmaleffekte). Ein Fit unterstellt einen stabilen datenerzeugenden Prozess, den es bei diesen Größen nicht gibt; die Verteilung der letzten zehn Jahre ist nicht die Verteilung des nächsten einen Jahres.

### 3.3 Einzelwert statt Stichprobe

Die äußere Simulationsschicht zieht pro Lauf **einen** Jahreswert je Treiber. Die gesuchte Größe ist die Verteilung des Glaubens über diese eine Zahl — ein subjektives (bayesianisches) Objekt, keine Frequenz. Selbst bei reichlich Daten bliebe der konkrete Forward-Wert durch bekannte Einmaleffekte geprägt (eine bereits absehbare Tarifrunde, ein angekündigter Budgetschritt), die eine reine Häufigkeitsbetrachtung ignoriert.

### 3.4 Beschränkter Träger und Plananker

PERT ist auf $[a,b]$ begrenzt — die Simulation zieht keine absurden Werte. Der Modus ist konstruktionsbedingt der **Planwert aus Blatt 04**, nicht der historische Mittelwert. Das ist wesentlich, wenn das Forward-Wissen von der Vergangenheit abweicht: man kann die Verteilung um den tatsächlich erwarteten Wert zentrieren und mit dem eigenen Urteil begrenzen. Eine an die Historie gefittete (etwa unbegrenzte Normal-)Verteilung würde stattdessen auf den Vergangenheitsmittelwert zentrieren und implausible Ränder einbringen.

---

## 4 Die PERT-Verteilung (formal)

PERT ist eine auf $[a,b]$ reparametrisierte Beta-Verteilung (Vose). Mit unterem Wert $a=$ `p_min`, Modus $m=$ `mode`, oberem Wert $b=$ `p_max` und Formparameter $\lambda$ (Default $4$):

$$
\alpha = 1 + \lambda\,\frac{m-a}{b-a}, \qquad
\beta = 1 + \lambda\,\frac{b-m}{b-a}, \qquad
X = a + (b-a)\cdot \mathrm{Beta}(\alpha,\beta).
$$

Erwartungswert und Modus:

$$
\mathbb{E}[X] = \frac{a + \lambda\,m + b}{\lambda + 2} \;\overset{\lambda=4}{=}\; \frac{a + 4m + b}{6},
\qquad \mathrm{Modus}(X) = m.
$$

$\lambda$ steuert, wie stark sich die Masse um den Modus konzentriert; $\lambda=4$ ist der Standardwert und gibt eine moderate Konzentration. Im Code: `rpert`/`qpert` in `distributions.R` (Beta-basiert, ohne Zusatzpaket), mit Schutzregel bei $b \le a$. Die Funktion `draw_epistemic` zieht je Treiber einen Wert; `draw_epistemic_correlated` zieht $n$ korrelierte Werte über eine Gauß-Copula mit PERT-Rändern, $X_j = F_j^{-1}\!\big(\Phi(Z_j)\big)$, $Z\sim\mathcal N(0,R)$.

PERT ist damit das Standardwerkzeug, um eine Drei-Punkt-Einschätzung („bester Wert plus plausible Unter-/Obergrenze") in eine glatte, begrenzte Verteilung zu übersetzen — der etablierte Weg in der Experten-Elicitation (O'Hagan et al.), wenn ein Fit mangels Daten ausscheidet.

---

## 5 Wo sehr wohl gefittet wird: die aleatorische `cv`

Das Modell verweigert sich dem Schätzen aus Daten nicht — es tut es dort, wo die Datenlage es trägt. Die monatliche Streuung wird in `estimate_cv_harmonic` aus der Ist-Reihe geschätzt: eine harmonische Saisonregression auf den Logarithmus der Reihe, die Residual-Standardabweichung ist die Variabilität.

$$
\log y_t = \beta_0 + \sum_{k=1}^{K}\Big[\beta_{k}^{s}\sin\tfrac{2\pi k t}{12} + \beta_{k}^{c}\cos\tfrac{2\pi k t}{12}\Big] + r_t,
\qquad \mathrm{cv} \approx \mathrm{sd}(r_t),\quad K=2.
$$

Die Näherung $\mathrm{sd}(r_t)\approx\mathrm{cv}$ folgt aus $y=\mu(1+\varepsilon)\Rightarrow \log y \approx \log\mu + \varepsilon$ für kleine $\varepsilon$; die Harmonischen entfernen die Saison, sodass die Residuen die reine Schwankung tragen. Die Schätzung verlangt mindestens acht positive Monatswerte (sonst `NA` und Rückfall auf die Vorgabe in Blatt 06); ab 24 Monaten ist sie belastbar, bei zwölf nur grob (vgl. *Methodendokumentation* §2.9, §2.11). Aleatorisch behandelt werden nur Treiber mit echter Monatsschwankung (`menge`, `material`); reine Jahresgrößen (Tarif, Landesbasisfallwert, Pflegebudget) tragen $\mathrm{cv}=0$.

Die Grenze zwischen „fitten" und „urteilen" verläuft also entlang der **Datenlage**, nicht entlang einer Vorliebe: viele, wiederkehrende, näherungsweise stationäre Beobachtungen → Schätzung; ein einzelner, nicht-stationärer Forward-Wert mit wenigen Beobachtungen → Urteil.

---

## 6 Daten fließen trotzdem in die Bandbreite ein

Für den epistemischen Teil wird die Historie nicht verworfen, sondern zur **Plausibilisierung** der Grenzen genutzt:

- Die Streuung der *jährlichen* Ist-Werte der Vorjahre (jährlich, nicht monatlich — das Monatliche ist die `cv`) ist ein guter Anker für `p_min`/`p_max`.
- Liegt für einen Treiber eine externe vorwärtsgerichtete Prognose mit Konfidenzband vor, lässt sich diese auf die Grenzen abbilden (Median → Modus, Bandgrenzen → `p_min`/`p_max`).
- Phase 3 (`03_verteilungen.R`) druckt je Treiber P5/P50/P95 der resultierenden PERT; daran prüft man, ob die gewählte Bandbreite vernünftig sitzt.

`p_min`/`p_max` ist damit ein datengestütztes Urteil über eine Bandbreite, kein blindes Raten — und kein in die Daten hineingezwungener Verlauf, den fünf bis zehn Punkte nicht hergeben.

---

## 7 Wann ein echter Fit die bessere Wahl ist

PERT ist die Standardschnittstelle, weil sie über heterogene Treiber hinweg mit minimalem Aufwand ehrlich bleibt — nicht weil das Modell darauf festgenagelt wäre. Ein Fit (oder eine extern geschätzte Prognoseverteilung) ist vorzuziehen, wenn

- eine **lange, stationäre Reihe** mit vielen Beobachtungen und ohne Regimewechsel vorliegt, oder
- eine **modellbasierte Forward-Verteilung** existiert (z. B. eine Inflationsprognose mit ausgewiesenem Prognoseintervall).

Einbau: entweder die Prognose in die PERT-Grenzen übersetzen (so bleibt die bestehende Maschinerie unverändert), oder `draw_epistemic` bzw. `draw_epistemic_correlated` in `distributions.R` durch einen Sampler aus der gefitteten/prognostizierten Verteilung ersetzen — die Copula-Schnittstelle ($U_j = \Phi(Z_j)$, dann inverse Verteilungsfunktion $F_j^{-1}$) bleibt erhalten, nur $F_j^{-1}$ wird ausgetauscht.

---

## 8 Einbettung in die genestete Simulation

Die beiden Methoden koexistieren je Schicht (Phase 5, `05_simulation.R`):

- **Äußere Schicht (epistemisch):** $\boldsymbol\theta$ je Lauf über PERT gezogen, Treiber-Abhängigkeit per Gauß-Copula (Blatt 07, `draw_epistemic_correlated`).
- **Innere Schicht (aleatorisch):** multiplikatives Monatsrauschen mit `cv`, je Treiber ein gemeinsamer Monatsschock.

Der Varianzzerlegungssatz (vgl. *Methodendokumentation* §2.11)

$$
\mathrm{Var}(T) = \underbrace{\mathrm{Var}\big(\mathbb{E}[T\mid\boldsymbol\theta]\big)}_{\text{epistemisch — PERT}} + \underbrace{\mathbb{E}\big[\mathrm{Var}(T\mid\boldsymbol\theta)\big]}_{\text{aleatorisch — cv}}
$$

zeigt die Arbeitsteilung direkt: die PERT-Bandbreiten formen den ersten Term, die geschätzte `cv` den zweiten. Beide Quellen sind nötig, beide werden mit der ihnen angemessenen Methode bestimmt.

---

## 9 Entscheidungsregel

| Größe | Beobachtungen | Prozess | Methode |
|---|---|---|---|
| Monatsstreuung um den Pfad (aleatorisch) | viele, wiederkehrend | innerhalb des Jahres ~stationär | aus Daten geschätzt — `cv` (§5) |
| Jahres-Niveau des Treibers (epistemisch) | wenige (≈ 5–10) | Verhandlung/Regulierung, nicht stationär | Urteil als PERT (§4), Bandbreite datengestützt (§6) |

---

## Quellen

- Vose, D.: *Risk Analysis* — PERT als Beta-Reparametrisierung, Drei-Punkt-Schätzung.
- O'Hagan, A. et al. (2006): *Uncertain Judgements: Eliciting Experts' Probabilities* — Elicitation subjektiver Verteilungen (SHELF).
- Gleißner, W.: *Grundlagen des Risikomanagements* — Risikoaggregation, simulationsbasierte Bestandsgefährdung.
- IDW S 11 / IDW PS 340 — Fortbestehensprognose, Risikofrüherkennung.
- Saltelli, A. et al. (2008/2010): *Global Sensitivity Analysis* — Treiberauswahl (Phase 2), die der Verteilungswahl vorausgeht.
