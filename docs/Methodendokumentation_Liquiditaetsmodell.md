# Methodendokumentation — Stochastische Liquiditätsplanung

**Zweck.** Dieses Dokument beschreibt das Liquiditätsmodell vollständig: welche Datei was tut, die statistischen Grundlagen und die verwendeten Gleichungen. Ziel ist Nachvollziehbarkeit — jede Zahl im Ergebnis lässt sich auf eine Eingabe, eine Annahme oder eine dokumentierte Formel zurückführen.

**Grundidee.** Ein deterministischer Kern bildet eine BWA in eine GuV und eine direkte Liquiditätsrechnung ab. Dieser Kern ist eine Funktion `f(Annahmen) → {GuV, Cash, Kennzahlen}`. Sensitivität, Verteilungen, Abhängigkeit und Simulation rufen diese Funktion auf — sie wiederholen sie nicht. Daten (beobachtet) und Annahmen (Urteil) bleiben durchgehend getrennt.

---

## 1 Dateiübersicht

### 1.1 Eingabemappe — `Mandant_Liquiditaetsmodell_Vorlage.xlsx`

Eine Mappe je Mandat. Gelbe Zellen sind Urteilseingaben.

| Blatt | Inhalt | maschinell gelesen |
|---|---|---|
| `00_Anleitung` | Ablauf und Prinzip | — |
| `01_Positionen` | BWA-Istwerte je Monat + Klassifikation je Zeile (`grp`, `cashprofile`, `driver`, `ust`) | alle Spalten |
| `02_Zahlungsprofile` | Lag-Profile (Anteil je Folgemonat) aus OPOS | `profile`, `f1…f4` |
| `03_Stammdaten_Eroeffnung` | Eröffnungssalden, KK-Linie | `open_cash`, `kk_limit`, `kk_zins`, `mindestreserve` |
| `04_Annahmen` | vorwärtsgerichtete Treiber + `hmax` | `menge`, `preis`, `tarif`, `energie`, `material`, `pflegebudget`, `inflation`, `hmax` |
| `05_Finanzierungsplan` | Investitionen, Fördermittel, Tilgung, Aufnahme je Monat | alle Zeilen × Monate |
| `06_Verteilungen` | epistemische Bandbreite (`p_min`, `p_max`) + aleatorischer `cv` je Treiber | die fünf in Phase 2 ausgewählten Treiber |
| `07_Korrelation` | Korrelationsmatrix der epistemischen Treiber | 5×5 |
| `08_Steuern` | Rechtsform + Steuerstatus + Sätze (mandantenabhängig, optional) | `rechtsform`, `gewst_status`, `ust_status`, `ust_methode` + Sätze |
| `09_Darlehen` | einzelne Kredite mit Tilgungsplan (mandantenabhängig, optional) | `betrag`, `zinssatz`, `tilgungsart`, `laufzeit_m`, `start_m`, `disagio` |
| `10_OPOS` | offene Forderungen/Verbindlichkeiten mit Zahlungsziel (mandantenabhängig, optional) | `art`, `betrag`, `faellig_am`/`zahlungsziel_tage`, `wahrscheinlichkeit`, `verzug_tage`, `ust_satz` |

Klassifikation je Position:

- `grp` — GuV-Gruppe für die Summenbildung: `umsatz`, `bestand`, `material`, `betr_ertrag`, `sopo`, `personal`, `sachkosten`, `afa`, `zins_e`, `zins_a`, `neutral_e`, `neutral_a`, `steuer`.
- `cashprofile` — Zahlungswirksamkeit: `inmonth`, `noncash`, `annual_jan`, `abschlag`, `quarterly` oder ein Lag-Profilname aus Blatt 02.
- `driver` — Wachstumstreiber: `menge_preis`, `menge`, `material`, `tarif`, `energie`, `pflegebudget`, `infl`, `flat`.
- `ust` — USt-Satz der Zeile (`0`, `0.07`, `0.19`); nur bei `ust_status = pflichtig` (Blatt 08) ausgewertet, sonst ignoriert.

### 1.2 Module (wiederverwendbar, enthalten keine Mandantenzahlen)

**`engine.R`** — der deterministische Kern.

- `load_model(WB)` — liest die Mappe, gibt das Modellobjekt `mdl` zurück (Daten, Klassifikation, Profile, Annahmen, Eröffnung, optional Verteilungen/Korrelation).
- `run_model(mdl, assume, noise)` — das `f(params)`: projiziert die GuV, baut die Liquidität, liefert Kennzahlen; `noise` (optional) überlagert das aleatorische Monatsrauschen.
- `run_model_weekly(mdl, assume)` — leitet die Liquidität wochengenau aus dem Monatslauf ab (Kalendarisierung, voller Horizont, rückabgestimmt; siehe 2.12).
- `validate_history(mdl)` — rechnet die Ist-Historie mit Nullannahmen nach (Konsistenzprüfung der Klassifikation).
- Interne Funktionen: `growth_of`, `project`, `build_guv`, `conv_lag`, `apply_profile`, `build_cash` sowie — bei vorhandenem Blatt `08_Steuern` — `build_ust` (USt-Zahllast) und `build_ertragsteuer` (KSt/GewSt/Entnahme), bei Blatt `09_Darlehen` zusätzlich `build_darlehen` (Tilgungsplan, Zins/Tilgung/Auszahlung), bei Blatt `10_OPOS` `build_opos` (offene Posten → Zahlungstermine).

**`distributions.R`** — stochastische Bausteine.

- `rpert`, `qpert` — PERT-Verteilung (Beta-basiert).
- `estimate_cv_harmonic` — aleatorische Streuung aus Ist-Residuen.
- `build_dists` — Verteilungsspezifikation aus Modell + Blatt 06.
- `make_psd` — nächste positiv-semidefinite Korrelationsmatrix (Fallback ohne Zusatzpaket).
- `draw_epistemic`, `draw_epistemic_correlated` — Ziehungen der Jahreswerte (unkorreliert / Gauß-Copula).

### 1.3 Phasenskripte (dünn, binden die Module per `source()` ein)

| Skript | Phase | Tut | Erzeugt |
|---|---|---|---|
| `01_forecast.R` | 1 | Validierung + deterministischer Forecast | Konsole |
| `01b_wochen.R` | 1 | Wochenliquiditaet (Ableitung aus dem Monatslauf) | `wochenliquiditaet.png` |
| `02_sensitivitaet.R` | 2 | Tornado + Sobol auf den Tiefpunkt | `tornado_tiefpunkt.png` |
| `03_verteilungen.R` | 3 | Verteilungen festlegen, aleatorische CV schätzen | `verteilungen_spec.rds`, `verteilungen.png` |
| `04_abhaengigkeit.R` | 4 | Korrelation, nearPD, Copula-Validierung | `korrelation.rds`, `korrelation.png` |
| `05_simulation.R` | 5 | genestete Monte-Carlo, Wahrscheinlichkeitsmetriken | `simulation_fanchart.png`, `simulation_endbestand.rds` |
| `05b_varianzzerlegung.R` | 5 (Add-on) | Tiefpunkt-Varianz nach Quelle (epistemisch/aleatorisch) | `varianzzerlegung.png` |

### 1.4 Abhängigkeiten

```
Mandant_Liquiditaetsmodell_Vorlage.xlsx
            │  read_excel
            ▼
        engine.R ───────── sourced by ──────────► 01, 01b, 02, 03, 04, 05, 05b
            ▲
distributions.R ── sourced by ── 03, 04, 05, 05b
            ▲
   verteilungen_spec.rds (03) ─► 04, 05, 05b
        korrelation.rds (04) ─► 05, 05b
```

`engine.R` korrigiert man genau einmal; alle Phasen profitieren. Für ein anderes Mandat tauscht man nur die Mappe.

---

## 2 Statistische Grundlagen und Gleichungen

Notation: $i$ Position, $m$ Monat des Planungshorizonts ($m = 1,\dots,H$), Basisjahr = die letzten zwölf Ist-Monate. $y_{i,m}^{\text{B}}$ ist der Basis-Monatswert, $\hat y_{i,m}$ der projizierte Wert.

### 2.1 Deterministischer Kern (Phase 1)

**Fortschreibung.** Jeder Monat wird aus dem entsprechenden Vorjahresmonat skaliert:

$$\hat y_{i,m} = y_{i,m}^{\text{B}} \cdot g_i$$

Die Saisonform steckt damit im Basismonat selbst; $g_i$ trägt nur die Jahresveränderung. Der Wachstumsfaktor folgt dem Treiber der Position:

$$
g_i =
\begin{cases}
(1+\delta_{\text{menge}})(1+\delta_{\text{preis}}) & \text{menge\_preis}\\
(1+\delta_{\text{menge}})(1+\delta_{\text{material}}) & \text{material}\\
(1+\delta_{\text{tarif}}) & \text{tarif}\\
(1+\delta_{\text{energie}}) & \text{energie}\\
(1+\delta_{\text{pflegebudget}}) & \text{pflegebudget}\\
(1+\delta_{\text{menge}}) & \text{menge}\\
(1+\delta_{\text{inflation}}) & \text{infl}\\
1 & \text{flat}
\end{cases}
$$

Jede Annahme $\delta_\bullet$ hat damit genau einen Eintrittsknoten.

**GuV-Identitäten.** Summen über die Gruppen ($\Sigma_g$ = Summe aller Positionen der Gruppe $g$ im Monat $m$):

$$
\begin{aligned}
\text{Umsatzerlöse} &= \Sigma_{\text{umsatz}}\\
\text{Gesamtleistung} &= \Sigma_{\text{umsatz}} + \Sigma_{\text{bestand}}\\
\text{Rohertrag} &= \text{Gesamtleistung} - \Sigma_{\text{material}}\\
\text{Betr. Rohertrag} &= \text{Rohertrag} + \Sigma_{\text{betr\_ertrag}} + \Sigma_{\text{sopo}}\\
\text{Betriebsergebnis} &= \text{Betr. Rohertrag} - \Sigma_{\text{personal}} - (\Sigma_{\text{sachkosten}} + \Sigma_{\text{afa}})\\
\text{EBT} &= \text{Betriebsergebnis} + \Sigma_{\text{zins\_e}} - \Sigma_{\text{zins\_a}} + \Sigma_{\text{neutral\_e}} - \Sigma_{\text{neutral\_a}}\\
\text{Ergebnis} &= \text{EBT} - \Sigma_{\text{steuer}}
\end{aligned}
$$

**GuV → Cash.** Jede Position wird über ihr Zahlungsprofil periodisiert. Mit Profilgewichten $w_0,\dots,w_{L-1}$ (Summe 1) und Carry-in aus dem Basisjahresende:

$$
c_{i,m} =
\begin{cases}
\hat y_{i,m} & \text{inmonth}\\
0 & \text{noncash}\\
\sum_{k=0}^{L-1} w_k\, a_{i,\,m-k} & \text{Lag-Profil}\\
\dfrac{g_i \sum_{t=1}^{12} y_{i,t}^{\text{B}}}{12} & \text{abschlag}\\
g_i \sum_{t=1}^{12} y_{i,t}^{\text{B}} \ \ (\text{nur } m{=}1) & \text{annual\_jan}\\
\sum_{m'=3q-2}^{3q} \hat y_{i,m'} \ \ (m=3q) & \text{quarterly}
\end{cases}
$$

wobei die verlängerte Reihe $a_{i,\cdot} = \big(y^{\text{B}}_{i,\,14-L},\dots,y^{\text{B}}_{i,12},\ \hat y_{i,1},\dots,\hat y_{i,H}\big)$ die letzten $L-1$ Basisjahresmonate voranstellt. Diese Carry-in-Werte sind *ungewachsene* Ist-Werte — späte Vorjahresleistungen werden zu ihren tatsächlichen Beträgen früh im Planungsjahr kassiert. Die Lag-Faltung erzeugt so das Forderungs-/Verbindlichkeitenverhalten ohne separate Bilanzfortschreibung.

**Liquiditätsfortschreibung.** Mit Einzahlungsgruppen (umsatz, betr_ertrag, zins_e) und Auszahlungsgruppen (material, personal, sachkosten, zins_a, steuer):

$$
\begin{aligned}
E_m &= \textstyle\sum_{i \in \text{inflow}} c_{i,m}, \qquad A_m = \textstyle\sum_{i \in \text{outflow}} c_{i,m}\\
\text{NCF}_m &= E_m - A_m - \text{CapEx}_m + \text{Förder}_m - \text{Tilgung}_m + \text{Aufnahme}_m\\
Z_m &= \tfrac{\text{kk\_zins}}{12}\cdot \max(0,\,-B_{m-1}) \quad (\text{Zins auf die im Vormonat gezogene Linie})\\
B_m &= B_{m-1} + \text{NCF}_m - Z_m, \qquad B_0 = \text{open\_cash}\\
\text{KK}_m &= \max(0,\, -B_m)
\end{aligned}
$$

Einzahlungsgruppen sind umsatz, betr_ertrag, zins_e; Auszahlungsgruppen material, personal, sachkosten, zins_a, steuer. Die nicht zahlungswirksamen GuV-Posten — Bestandsveränderung, Auflösung Sonderposten, Abschreibungen, neutrale Erträge und Aufwendungen — fallen aus der Liquidität heraus. Je Position müssen deshalb `grp` (GuV-Zuordnung) und `cashprofile` (Zahlungswirksamkeit) zueinander passen.

Der Liquiditäts-Tiefpunkt ist $T = \min_m B_m$ — die für die Zahlungsfähigkeit kritische Größe.

**Validierung.** Mit $\delta_\bullet = 0$ gilt $g_i = 1$, also $\hat y_{i,m} = y_{i,m}^{\text{B}}$: die GuV reproduziert die Historie. Die Gegenprobe vergleicht das über die Identitäten gerechnete Ergebnis mit der flachen Summe $\sum_{\text{Ertrag}} - \sum_{\text{Aufwand}}$; Gleichheit bestätigt die Vorzeichen und die Gruppenzuordnung. Eine Cash-seitige Validierung leistet die Engine nicht — der historische Liquiditätspfad wird nicht automatisch gegen Kontoauszüge geprüft, weil die Eröffnung in der Mappe der Forecast-Start ist, nicht der Historienbeginn. Dieser Abgleich bleibt ein manueller Schritt.

### 2.2 Sensitivität (Phase 2)

Zielgröße $T = \min_m B_m$ (Tiefpunkt).

**Tornado.** Jeder Treiber wird einzeln um $\pm 1$ Prozentpunkt ausgelenkt:

$$\text{Swing}_i = T(\boldsymbol\theta_i^{+}) - T(\boldsymbol\theta_i^{-})$$

mit $\boldsymbol\theta_i^{\pm}$ = Basisannahmen, Treiber $i$ um $\pm 0{,}01$ verschoben. Sortierung nach $|\text{Swing}_i|$, Vorzeichen zeigt die Richtung.

**Sobol-Indizes (Schätzer nach Jansen 1999).** Zwei unabhängige Stichprobenmatrizen $A, B \in \mathbb{R}^{N\times k}$; $A_B^{(j)}$ entsteht aus $A$, indem Spalte $j$ durch die aus $B$ ersetzt wird. Modellauswertungen $y_A = f(A)$, $y_B = f(B)$, $y_{AB}^{(j)} = f(A_B^{(j)})$. Mit $V = \widehat{\mathrm{Var}}(y)$:

$$
S_j = 1 - \frac{\frac{1}{2N}\sum_{i}\big(y_B^{(i)} - y_{AB}^{(j),i}\big)^2}{V},
\qquad
S_{T_j} = \frac{\frac{1}{2N}\sum_{i}\big(y_A^{(i)} - y_{AB}^{(j),i}\big)^2}{V}
$$

$S_j$ ist der Haupteffekt-Varianzanteil, $S_{T_j}$ der Totaleffekt inklusive Wechselwirkungen. $S_{T_j} \gg S_j$ deutet auf Interaktion. Die Jansen-Form ist beschränkt und stabiler als der naive Produktschätzer (dieser kann bei dominanten Faktoren Werte außerhalb $[0,1]$ liefern). Die Schätzung hängt von den angenommenen Wertebereichen der Treiber ab; diese stehen derzeit als Variable `ranges` in `02_sensitivitaet.R`, unabhängig von den PERT-Rändern in Blatt 06, und ließen sich in ein eigenes Blatt verlagern. Die geschätzten Indizes werden auf $[0,1]$ begrenzt, damit Restrauschen bei endlichem $N$ nicht als unzulässiger Wert erscheint.

### 2.3 Verteilungen (Phase 3)

Zwei Arten von Unsicherheit, getrennt modelliert:

- **epistemisch** — Unwissen über die Jahresannahme; reduzierbar; als PERT.
- **aleatorisch** — irreduzible monatliche Streuung um den Pfad; aus Daten geschätzt.

**PERT.** Aus $(a, m, b)$ = (min, mode, max) mit Form $\lambda = 4$:

$$
\alpha = 1 + \lambda\,\frac{m-a}{b-a},\qquad
\beta = 1 + \lambda\,\frac{b-m}{b-a},\qquad
X = a + (b-a)\cdot \mathrm{Beta}(\alpha,\beta)
$$

Erwartungswert $\mathbb{E}[X] = \dfrac{a + \lambda m + b}{\lambda + 2}$. Der Mode ist der Punktwert aus Blatt 04 (eine Quelle), $a, b$ aus Blatt 06.

**Aleatorische CV aus Ist-Residuen.** Multiplikatives Monatsrauschen wird auf der Log-Skala additiv. Saison über harmonische Regression (Fourier, Ordnung $K=2$, fünf Parameter):

$$
\log y_t = \beta_0 + \sum_{k=1}^{K}\Big[\beta_{2k-1}\sin\tfrac{2\pi k t}{12} + \beta_{2k}\cos\tfrac{2\pi k t}{12}\Big] + \varepsilon_t,
\qquad
\widehat{\mathrm{CV}} = \mathrm{sd}(\hat\varepsilon_t)
$$

Harmonische statt zwölf Monats-Dummys, weil zwölf Dummys bei zwölf Beobachtungen alle Freiheitsgrade verbrauchen ($\Rightarrow \widehat{\mathrm{CV}}=0$). Fünf Harmonische lassen sieben Residual-Freiheitsgrade. Belastbar erst ab 24 Monaten (Saison aus dem Mehrjahresmittel, Rauschen aus Jahr-zu-Jahr-Abweichungen). Geschätzt nur für Treiber mit echter Monatsschwankung (Menge, Material); jährliche Stufen (Tarif, Preis, Pflegebudget) tragen $\mathrm{CV}=0$. Konkret wird die CV der Menge aus der Summe der mengengetriebenen Erlöse (DRG + Zusatzentgelte + Wahlleistungen + Ambulant) je Monat geschätzt, die des Materials aus den Materialaufwandspositionen; daraus die 4,5 % bzw. 4,1 % im Beispiel. In einem echten Mandat ersetzt man die Erlössumme durch echte Fallzahlen, wo vorhanden.

### 2.4 Abhängigkeit (Phase 4)

**Gauß-Copula mit PERT-Rändern** (Sklar). Zielkorrelation $R$ (Blatt 07). Mit Cholesky $R = U^\top U$ und $\boldsymbol\xi \sim \mathcal{N}(0,I)$:

$$
\mathbf z = \boldsymbol\xi\, U \ \sim\ \mathcal{N}(0,R),\qquad
u_j = \Phi(z_j),\qquad
x_j = F_j^{-1}(u_j)
$$

$F_j$ ist die PERT-Verteilung des Treibers $j$. Die Kopplung sitzt im Normal-Score $\mathbf z$, die Randverteilungen bleiben PERT. Rangkorrelationen (Spearman) sind unter den monotonen Transformationen $\Phi$ und $F_j^{-1}$ erhalten; die Pearson-Korrelation der $x_j$ weicht bei schiefen Rändern leicht ab (copulabedingte Dämpfung).

**Reparatur zur PSD-Matrix.** Eine handgesetzte Matrix ist nicht zwingend positiv semidefinit. `Matrix::nearPD` liefert die nächste gültige Korrelationsmatrix; der Fallback `make_psd` klippt die Eigenwerte:

$$
R = Q\,\Lambda\,Q^\top \ \Rightarrow\ \tilde\Lambda = \max(\Lambda,\epsilon),\quad
\tilde R = Q\,\tilde\Lambda\,Q^\top,\quad
R_{\text{psd}} = D^{-1/2}\,\tilde R\,D^{-1/2}
$$

mit $D = \mathrm{diag}(\tilde R)$ (Rückskalierung auf Einsdiagonale). Cholesky verlangt strikte Definitheit, daher $\epsilon > 0$.

### 2.5 Simulation und Wahrscheinlichkeitsmetriken (Phase 5)

Genestete Monte-Carlo, die die beiden Unsicherheiten getrennt hält. Für Lauf $s = 1,\dots,S$:

1. **äußere Schicht (epistemisch):** korrelierten Jahresvektor ziehen $\boldsymbol\theta^{(s)} = $ `draw_epistemic_correlated(spec, R, 1)`.
2. **innere Schicht (aleatorisch):** Pfad bauen $\hat y_{i,m} = f(\boldsymbol\theta^{(s)})$, dann monatlich überlagern

$$\tilde y_{i,m} = \hat y_{i,m}\cdot \exp(\varepsilon_{d(i),m}),\qquad \varepsilon_{d,m}\sim \mathcal N(0,\sigma_d^2)$$

wobei $\varepsilon_{d,m}$ ein je Treiber $d$ und Monat **gemeinsamer** Schock ist — alle Positionen desselben Treibers teilen ihn, so dass die strukturelle Kopplung (z. B. alle mengengetriebenen Erlöse schwanken im selben Monat gemeinsam) erhalten bleibt. $\sigma_d$ ist der geschätzte CV; nur Treiber mit $\sigma_d > 0$ (Menge, Material) tragen Rauschen. So bleibt zerlegbar, wie viel der Bandbreite aus „Annahme unbekannt" und wie viel aus „Monatsvolatilität" stammt.

Aus der Verteilung der $B_m^{(s)}$ folgen die eigentlichen Produkte — keine Bandbreite als Selbstzweck, sondern Schwellenwahrscheinlichkeiten:

$$
\hat P(\text{Unterdeckung in } m) = \frac{1}{S}\sum_{s} \mathbf 1\!\left[B_m^{(s)} < \text{Mindestreserve}\right]
$$
$$
\hat P(\text{KK-Linie reißt}) = \frac{1}{S}\sum_{s} \mathbf 1\!\left[\min_m B_m^{(s)} < -\,\text{kk\_limit}\right]
$$

**Rechtlicher Bezug.** Die Fortführungsprognose nach §§ 18/19 InsO verlangt „überwiegende Wahrscheinlichkeit" der Zahlungsfähigkeit über den Prognosezeitraum. Das übersetzt sich direkt:

$$P\big(\text{zahlungsfähig über } 1{:}H\big) > 0{,}5$$

Die stochastische Schicht operationalisiert damit den Wahrscheinlichkeitsmaßstab, den deterministische Szenarien nur qualitativ andeuten.

### 2.6 Steuern (mandantenabhängig, optional)

Das Steuer-Modul ist über Blatt `08_Steuern` zuschaltbar. Fehlt das Blatt, ist es inert — Engine und Forecast verhalten sich Euro-genau wie ohne Steuern (Rückwärtskompatibilität). Drei Schalter entscheiden, *welche* Steuer überhaupt zahlungswirksam ist; die Steuerart kippt mit der Rechtsform:

- `rechtsform` ∈ {`kapital`, `personen`, `einzel`} — eine Kapitalgesellschaft zahlt Körperschaftsteuer selbst; transparente Formen (Personengesellschaft, Einzelunternehmen) nicht, dort wird die Ertragsteuer als Entnahme des Gesellschafters abgebildet.
- `gewst_status` ∈ {`gewerblich`, `freiberuflich`, `gemeinnuetzig`} — nur `gewerblich` trägt Gewerbesteuer; `gemeinnuetzig` ist zusätzlich von der KSt befreit.
- `ust_status` ∈ {`pflichtig`, `frei`} — steuerfreie Heilbehandlung (§ 4 Nr. 14 UStG) erzeugt keine eigene USt-Zahllast; die nicht abziehbare Vorsteuer steckt dann bereits in den Bruttobeträgen der Positionen.
- `ust_methode` ∈ {`soll`, `ist`} — Soll-Versteuerung: die USt entsteht mit der Leistung; Ist-Versteuerung (§ 20 UStG, kleinere/freiberufliche Mandate): mit der Vereinnahmung. Nur bei `pflichtig` wirksam; bestimmt die Bemessungsbasis der Zahllast (Accrual vs. Cash).

Die Sätze (`kst_satz`, `soli_satz`, `gewst_messzahl`, `gewst_hebesatz`, `gewst_freibetrag`, `ust_lag`, `entnahme_quote`) sind Parameter, keine Konstanten — der Hebesatz variiert je Gemeinde, der KSt-Satz ändert sich per Gesetz.

**Umsatzsteuer (Soll oder Ist).** USt ist GuV-neutral, aber ein Liquiditäts-Timing-Posten. Mit dem USt-Satz $\tau_i$ je Position (Spalte `ust`) ist die Zahllast die Differenz aus Ausgangs- und Vorsteuer; `ust_methode` bestimmt die Basis:

$$
\text{Soll:}\quad Z_m^{\text{USt}} = \sum_{i\in\text{Erlöse}} \tau_i\,\hat y_{i,m} - \sum_{i\in\text{Vorsteuer}} \tau_i\,\hat y_{i,m}
\qquad
\text{Ist:}\quad Z_m^{\text{USt}} = \sum_{i\in\text{Einzahlung}} \tfrac{\tau_i}{1+\tau_i}\,c_{i,m} - \sum_{i\in\text{Auszahlung}} \tfrac{\tau_i}{1+\tau_i}\,c_{i,m}
$$

Soll bemisst auf der Leistung $\hat y$, Ist auf dem Brutto-Cashflow $c$ (inklusive der OPOS-Zahlungen). Beide werden mit Lag $\ell$ abgeführt (`ust_lag`; 1 Monat zum 10. des Folgemonats, 2 mit Dauerfristverlängerung), der Basisjahr-Carry-in (die ersten $\ell$ Monate) aus der Leistung approximiert. Die operativen Zu- und Abflüsse werden bei `pflichtig` brutto gerechnet (Faktor $1+\tau_i$ vor der Lag-Faltung), die Zahllast erscheint als eigene Cash-Zeile. Über den Horizont saldiert sich die USt; nur die Timing-Differenz zwischen Vereinnahmung, Vorsteuerabzug und Abführung wirkt.

**OPOS-Verzahnung.** Unter Soll ist die USt von den OPOS-Terminen unabhängig — sie entstand mit der bereits gestellten Rechnung, nicht mit deren Bezahlung; die offenen Posten steuern dann nur den Brutto-Zahlungsstrom. Unter Ist folgt sie ihnen unmittelbar: die Vereinnahmung einer offenen Forderung löst die Ausgangs-USt aus, die Bezahlung einer offenen Verbindlichkeit den Vorsteuerabzug — beides in dem Monat, in dem der Posten zahlungswirksam wird. Dazu trägt jede OPOS-Zeile ihren eingebetteten Satz (`ust_satz`). Damit ist die zuvor vereinfachte OPOS-Umsatzsteuer für Ist-Mandate sauber verzahnt (vgl. 2.8). Bei der Apotheke greift der Effekt, dass die USt auf den Kassenumsatz unter Soll bereits fällig ist, bevor das Rechenzentrum zahlt — unter Ist erst mit dem Zahlungseingang.

**Ertragsteuer.** Aus dem auf zwölf Monate hochgerechneten EBT, $\text{EBT}_a = \tfrac{12}{H}\sum_m \text{EBT}_m$:

$$
\text{GewSt} = \max(0,\ \text{EBT}_a - F)\cdot m_{\text{St}}\cdot h,\qquad
\text{KSt} = \max(0,\ \text{EBT}_a)\cdot s_{\text{KSt}}\,(1+s_{\text{Soli}})
$$

mit Messzahl $m_{\text{St}} = 3{,}5\,\%$, Hebesatz $h$, Freibetrag $F$ (24.500 € nur für EK/Personengesellschaft, sonst 0). GewSt fällt nur bei `gewerblich`, KSt nur bei `kapital` und nicht `gemeinnuetzig`. Beide werden als vier gleiche Vorauszahlungen auf den gesetzlichen Terminen verteilt (KSt 10.3/6/9/12, GewSt 15.2/5/8/11) und kalendarisch auf den Planmonat gemappt. Transparente Formen tragen keine KSt; stattdessen eine Steuerentnahme $q\cdot\max(0,\text{EBT}_a)$ (Quote `entnahme_quote`) auf den ESt-Terminen — ein realer Liquiditätsabfluss, der nur deshalb nicht „Steuer" heißt, weil er beim Gesellschafter anfällt.

Vereinfachung: Einpass-Näherung auf das EBT, ohne gewerbesteuerliche Hinzurechnungen/Kürzungen und ohne die exakte Verzahnung von KSt- und GewSt-Bemessung. Für die Liquiditätsplanung ausreichend — die Steuerlast ist näherungsweise, nicht veranlagungsgenau. Die laufende Ertragsteuer mindert zusätzlich das GuV-Ergebnis (Zeile `Ergebnis`); die GuV-Gruppe `steuer` aus 2.1 trägt dabei nur noch sonstige Steuern (Grund-/Kfz-), KSt und GewSt werden also nicht doppelt erfasst. Der KK-Zins fließt weiterhin nicht ins Ergebnis zurück (vgl. 2.9).

**Mandatszuordnung.** Krankenhaus gGmbH: `kapital`/`gemeinnuetzig`/`frei` → Modul inert (Status quo). Apotheke e.K./OHG: `einzel` bzw. `personen`, `gewerblich`, `pflichtig` plus Entnahme — das steuerlich vollste Mandat. Arztpraxis und ärztliches MVZ als Personengesellschaft: `freiberuflich`, `frei` → keine GewSt, USt frei, nur Entnahme. MVZ-GmbH: `kapital`, `gewerblich` (kraft Rechtsform), USt überwiegend frei. Pharmaunternehmen: `kapital`, `gewerblich`, `pflichtig` → Vollprogramm.

**Validierung.** Mit der Krankenhaus-Konfiguration (gemeinnützig/frei) erzeugt das Modul ausschließlich Nullzeilen; Historienvalidierung und deterministischer Forecast bleiben Euro-genau unverändert. Eine USt-pflichtige Konfiguration ist gegen die GuV-Neutralität geprüft (Saldo über den Horizont = reine Timing-Differenz).

### 2.7 Darlehen (mandantenabhängig, optional)

Blatt `09_Darlehen` modelliert einzelne Kredite explizit mit Tilgungsplan — feiner als die pauschale Aufnahme-/Tilgungszeile in Blatt 05. Fehlt das Blatt, ist das Modul inert (additiv, kein Effekt). Je Kredit: Nominalbetrag, Zinssatz p.a., Tilgungsart, Laufzeit, Auszahlungsmonat, optionales Disagio.

Pro Forecast-Monat und Kredit: Auszahlung im Startmonat (Neuvalutierung), danach Zins auf die fallende Restschuld und Tilgung nach Tilgungsart. Mit Monatszins $r = \text{Zinssatz}/12$ und Restschuld $K_m$:

$$
Z_m = K_{m-1}\cdot r,\qquad K_m = K_{m-1} - T_m
$$

und der Tilgung $T_m$ je nach Art:

- `annuitaet`: konstante Rate $A = K_0\,\dfrac{r}{1-(1+r)^{-n}}$, davon $T_m = A - Z_m$ (Zins fällt, Tilgung steigt).
- `linear`: $T_m = K_0/n$ konstant, Zins fällt.
- `endfaellig`: $T_m = 0$ bis zur letzten Periode, dann $K_0$; Zins konstant auf $K_0$.
- `nur_zins`: $T_m = 0$, nur Zins (revolvierend / endfällig ohne Tilgung im Horizont).

Wirkung: der **Zins** läuft in die GuV (mindert EBT und damit die Bemessung der Ertragsteuer aus 2.6) *und* als Auszahlung in die Liquidität. **Tilgung** und **Auszahlung** wirken nur in der Liquidität. Der KK-Zins bleibt davon unberührt — er ist weiter der separate Cash-Plug auf die gezogene Linie (2.1), nicht Teil des Darlehensbuchs.

Konvention: die Bedienung beginnt im Monat nach der Auszahlung; `start_m` ≤ 0 bezeichnet einen bereits vor Forecast-Beginn valutierten Kredit (Restschuld = `betrag`, keine Auszahlung, Bedienung ab M1). Das Disagio mindert nur die Auszahlung, der Zins läuft auf den Nominalbetrag — die periodengerechte Disagio-Verteilung in der GuV ist nicht abgebildet (für die Liquidität unerheblich).

Verhältnis zu Blatt 05: additiv. Bestehende Zinslast steckt weiter in der GuV-Position Zinsaufwand (`n_zia`), bestehende Tilgung in der Zeile Tilgung (Blatt 05). Wer einen Kredit explizit ins Darlehensbuch nimmt, entfernt ihn dort, um Doppelerfassung zu vermeiden. Im Beispiel ist die frühere Aufnahme über 4 Mio. (M6) ins Darlehensbuch verschoben; weil sie im letzten Planmonat valutiert, fällt im Horizont weder Zins noch Tilgung an — der Tiefpunkt bleibt Euro-genau unverändert.

### 2.8 Offene Posten (mandantenabhängig, optional)

Blatt `10_OPOS` ersetzt im Nahbereich die statistische Forderungs-/Verbindlichkeitsmodellierung durch die tatsächlichen offenen Posten. Die Lag-Profile (2.1) leiten das Zahlungsverhalten aus der Run-Rate ab; der Carry-in in `conv_lag` — die letzten Basisjahresmonate, die in die ersten Planmonate zahlen — ist nichts anderes als der Anfangsbestand an Forderungen/Verbindlichkeiten, nur statistisch geschätzt. Liegen die echten offenen Posten vor (aus der OPOS-Liste), sind sie genauer.

Je Zeile eine Einzelrechnung oder ein Aging-Bucket. Forderung: erwarteter Eingang = Betrag × Eingangswahrscheinlichkeit, terminiert auf Fälligkeit + Verzug. Verbindlichkeit: Zahlung am Fälligkeitstag, optional gestundet (`verzug_tage`) oder mit Vergleichsquote (`wahrscheinlichkeit` < 1). Der Termin kommt aus `faellig_am` (Datum) oder `zahlungsziel_tage` ab Forecast-Beginn; überfällige Posten fallen in den ersten Planmonat.

**Keine Doppelerfassung.** Sind offene Posten gesetzt, schaltet die Engine den Carry-in der betroffenen Seite ab und nutzt stattdessen die OPOS-Termine — getrennt je Seite: nur die Seite mit Zeilen (Forderungen bzw. Verbindlichkeiten) verliert ihren Carry-in, das Neugeschäft im Planzeitraum läuft weiter über die Lag-Profile. Übergabepunkt ist der Forecast-Beginn: offene Posten regeln den Ablauf des Anfangsbestands, die Profile das Neugeschäft. Inmonth-Positionen (etwa Sachkosten) hatten bisher gar keinen Eröffnungsbestand — deren offene Posten ergänzt OPOS, ist dort also Verfeinerung statt Austausch.

Die GuV bleibt unberührt — offene Posten verschieben nur den Zahlungszeitpunkt, nicht die Periodenabgrenzung. Ein Abgleich (Σ Forderungen gegen den Kontextwert in Blatt 03) dient der Plausibilität. Konsistenz zur USt: die Posten sind brutto. Unter Soll-Versteuerung steuern sie nur den Brutto-Zahlungsstrom — die USt ist leistungs-, nicht zahlungsgebunden und damit von den OPOS-Terminen unabhängig. Unter Ist-Versteuerung trägt jede Zeile ihren Satz (`ust_satz`), und die USt folgt der Vereinnahmung bzw. Verausgabung (vgl. 2.6). Bei leerem Blatt bleibt alles beim statistischen Carry-in; das Beispiel-Krankenhaus ist Euro-genau unverändert.

## 2.9 Modellannahmen und Vereinfachungen

- **KK-Zins:** auf die im Vormonat gezogene Linie berechnet ($Z_m$ in 2.1), Guthabenzins ignoriert. Der Zins mindert die Liquidität, fließt aber **nicht** in das GuV-Ergebnis zurück — die Ergebniswirkung ist insoweit minimal überzeichnet, die Liquiditätswirkung ist erfasst. Die Monatssicht rechnet auf dem Monats-Anfangsbestand und erfasst die intramonatliche Lage der Zahlungen nicht; fallen große Aus- und Einzahlungen innerhalb des Monats auseinander, unterschätzt sie den Zins gegenüber der wochengenauen Rechnung (2.10) deutlich. Der Wochenlauf (`01b_wochen.R`) weist beide Beträge und ihren Faktor aus; er ist die belastbarere KK-Zins-Schätzung.
- **Kein unterjähriger Trend:** jeder Planmonat ist Vorjahresmonat × Jahreswachstum; ein Trend innerhalb des Jahres wird nicht abgebildet.
- **Basisjahr:** aus mehr als zwölf Monaten Historie werden nur die letzten zwölf verwendet; die Monatszuordnung setzt voraus, dass der Forecast kalendarisch am Basisjahr ausgerichtet ist.
- **`abschlag` / `annual_jan`** verwenden die volle Basisjahressumme × g; bei Halbjahresläufen liegt damit die ganze Jahresprämie im ersten Monat und der Abschlag bei Jahressumme/12.
- **Energie und Inflation** bleiben in den Phasen 3–5 deterministisch (in Blatt 06 ohne Zeile), weil Phase 2 ihren Beitrag zum Tiefpunkt als am geringsten ausweist.
- **Aleatorische CV** nur für Treiber mit echter Monatsschwankung; jährliche Stufen tragen $\mathrm{CV}=0$. Bei zwölf Monaten ist die Schätzung grob, belastbar ab 24 Monaten.
- **Sobol-Bandbreiten** liegen im Skript, nicht in der Mappe (siehe 2.2).
- **Steuern (sofern Modul aktiv):** Einpass-Näherung auf das EBT ohne gewerbesteuerliche Hinzurechnungen/Kürzungen und ohne exakte KSt-/GewSt-Verzahnung; USt wahlweise nach Soll- oder Ist-Versteuerung mit konstantem Lag, Basisjahr-Carry-in aus der Leistung approximiert. Liquiditätswirkung erfasst, Steuerlast näherungsweise statt veranlagungsgenau (Detail in 2.6).
- **Darlehen (sofern Modul aktiv):** Zins auf den Nominalbetrag, Disagio mindert nur die Auszahlung (keine periodengerechte GuV-Verteilung); Bedienung ab dem Monat nach Auszahlung. Additiv zu Blatt 05 — Doppelerfassung bestehender Kredite vermeiden (Detail in 2.7).
- **Offene Posten (sofern Modul aktiv):** ersetzen den statistischen Carry-in der jeweiligen Seite; Termin monatsgenau (Tag erst in der Wochensicht), Posten brutto. Unter Soll ist die USt termin­unabhängig (Leistungsbezug); unter Ist folgt sie über `ust_satz` der Vereinnahmung (Detail in 2.6/2.8).

## 2.10 Durchgerechnetes Beispiel (eine Position)

Position DRG-Erlöse (`r_drg`), Treiber `menge_preis`, Zahlungsprofil `kassen` $= (0{,}60,\ 0{,}35,\ 0{,}05)$. Annahmen: $\delta_{\text{menge}} = 0{,}5\,\%$, $\delta_{\text{preis}} = 4{,}0\,\%$.

1. **Basiswert** (Ist 2025), Januar: $y^{\text{B}}_{\text{Jan}} = 8.111.200$ €.
2. **Wachstumsfaktor:** $g = (1+0{,}005)(1+0{,}040) = 1{,}0452$.
3. **Projektion (periodengerecht), Januar:** $\hat y_{\text{Jan}} = 8.111.200 \times 1{,}0452 = 8.477.826$ €.
4. **Zahlungswirksam, Januar** (Profil `kassen`; Carry-in Dezember und November als *Ist*-Werte $7.012.700$ bzw. $7.794.600$):

$$
c_{\text{Jan}} = 0{,}60 \cdot 8.477.826 + 0{,}35 \cdot 7.012.700 + 0{,}05 \cdot 7.794.600 = 7.930.871 \text{ €}
$$

Der periodengerechte Januar-Erlös ($8{,}48$ Mio.) und der Januar-Zahlungseingang ($7{,}93$ Mio.) fallen auseinander — die Differenz ist der Forderungsaufbau, den die Lag-Faltung abbildet. Dieselbe Rechnung läuft für jede Position mit ihrem Profil; die Spaltensummen ergeben die GuV- und die Liquiditätszeilen.

## 2.11 Varianzzerlegung des Tiefpunkts (Add-on)

Zerlegt die Streuung des Tiefpunkts $T = \min_m B_m$ in ihre beiden Quellen, über drei Läufe gleicher Größe:

- **gesamt:** Jahreswerte korreliert gezogen, Rauschen an $\Rightarrow \mathrm{Var}(T)$.
- **nur epistemisch:** Jahreswerte gezogen, Rauschen aus $\Rightarrow \mathrm{Var}\big(\mathbb{E}[T \mid \boldsymbol\theta]\big)$.
- **nur aleatorisch:** Jahreswerte auf den Modes fixiert, Rauschen an $\Rightarrow \mathbb{E}\big[\mathrm{Var}(T \mid \boldsymbol\theta)\big]$ am zentralen Szenario.

Nach dem Varianzzerlegungssatz $\mathrm{Var}(T) = \mathrm{Var}\big(\mathbb{E}[T\mid\boldsymbol\theta]\big) + \mathbb{E}\big[\mathrm{Var}(T\mid\boldsymbol\theta)\big]$ addieren sich beide Anteile bei Unabhängigkeit näherungsweise zur Gesamtvarianz; der Rest $\mathrm{Var}(T) - V_{\text{epi}} - V_{\text{ale}}$ ist Interaktion bzw. Schätzfehler und sollte klein sein. Berichtet werden Standardabweichungen (in €) und Varianzanteile.

Lesart: weil der Tiefpunkt das **Minimum** über die Monate ist, reagiert er stark auf monatliche Ausreißer — die aleatorische Quelle wiegt am Tiefpunkt typischerweise schwerer als am Jahresergebnis. Der konkrete Anteil hängt zudem an der (bei zwölf Monaten groben) CV-Schätzung; die Methode ist belastbar, der genaue Split indikativ.

---

## 2.12 Wochenauflösung (Ableitung aus dem Monatslauf)

Neben der Monatssicht liefert `run_model_weekly` die Liquidität wochengenau über den vollen Horizont. Die Wochensicht ist keine zweite Rechnung, sondern eine **kalendarisierte Ableitung** des Monatslaufs: jeder Monatsbetrag wird auf die Wochen verteilt, die Summe je Monat bleibt erhalten. Der geprüfte Monatskern bleibt unangetastet; beide Sichten beschreiben denselben Plan in unterschiedlicher Auflösung.

Verteilregeln (Defaults im Code, später über ein Blatt konfigurierbar):

- **Operativer Lauf, glatt:** Umsätze und laufende Sachkosten anteilig nach Tagesüberlappung der Woche mit dem Monat (eine Woche, die zwei Monate schneidet, wird tageweise aufgeteilt).
- **Personal als Lohn-Lump:** der ganze Monatsbetrag in der Woche des Zahltags (Default: Monatsende).
- **Steuern auf die gesetzlichen Tage:** USt-Zahllast, KSt-Vorauszahlung und Entnahme zum 10., GewSt-Vorauszahlung zum 15.
- **Darlehen:** Kapitaldienst (Zins + Tilgung) zum Monatsende, Auszahlung zu Monatsbeginn.
- **Offene Posten:** exakt auf ihren Zahlungstermin (Fälligkeit + Verzug), nicht verteilt — die Datierung aus 2.8 fällt direkt in die Woche.
- **Investitionen, Fördermittel, manuelle Tilgung/Aufnahme:** glatt über den Monat.

Der KK-Zins wird wochengenau auf die gezogene Linie gerechnet (Satz × Tage / 365).

**Rückabstimmung.** Die Summe der wöchentlichen Netto-Cashflows je Monat ist gleich dem monatlichen Netto-Cashflow — exakt, weil die Kalendarisierung die Monatssummen erhält und datierte Posten in ihrem Monat bleiben. Die Monatsend-Bestände der beiden Sichten unterscheiden sich nur um den **Zins-Pfad**: die Monatssicht verzinst den (negativen) Monatsanfangsbestand über den ganzen Monat, die Wochensicht den tatsächlich ablaufenden Bestand. Weil die Einzahlungen über den Monat eintreffen, ist der Bestand im Mittel weniger negativ als am Monatsanfang — die Monatssicht **überzeichnet** den KK-Zins und damit den Finanzbedarf. Im Beispiel-Krankenhaus liegt der wöchentliche Tiefpunkt deshalb bei −4,75 Mio (W13, Ende März) gegenüber −5,14 Mio in der Monatssicht; die rein operative Summe ist in beiden identisch.

**Dominanter Hebel ist der Zahltag.** Die Richtung dieser Differenz hängt am Timing: liegt der Lohnlauf am Monatsende (nach Eingang der Umsätze), ist die Monatssicht konservativ. Ein Zahltag in der Monatsmitte — also vor dem Großteil der Einzahlungen — kehrt das um und **vertieft** den wöchentlichen Tiefpunkt unter den monatlichen. Genau deshalb ist die Wochensicht in der Sanierung üblich: sie macht die untermonatigen Spitzen sichtbar, die der Monatsraster glättet. Die Verteilregeln sind Annahmen; in der Wochensicht wirken sie unmittelbar auf Tiefpunkt und Reservereichweite und sollten mandatsspezifisch gesetzt werden.

---

## 3 Reproduktion

Voraussetzung: R mit `readxl` und `Matrix`. Skripte und Mappe im selben Verzeichnis. Reihenfolge:

```bash
Rscript 01_forecast.R          Mandant_Liquiditaetsmodell_Vorlage.xlsx
Rscript 01b_wochen.R           Mandant_Liquiditaetsmodell_Vorlage.xlsx
Rscript 02_sensitivitaet.R     Mandant_Liquiditaetsmodell_Vorlage.xlsx
Rscript 03_verteilungen.R      Mandant_Liquiditaetsmodell_Vorlage.xlsx
Rscript 04_abhaengigkeit.R     Mandant_Liquiditaetsmodell_Vorlage.xlsx
Rscript 05_simulation.R        Mandant_Liquiditaetsmodell_Vorlage.xlsx
Rscript 05b_varianzzerlegung.R Mandant_Liquiditaetsmodell_Vorlage.xlsx
```

Phase 3 erzeugt `verteilungen_spec.rds`, Phase 4 `korrelation.rds`; beide werden von Phase 5 gelesen. Zufallsziehungen sind über `set.seed` fixiert, die Ergebnisse damit reproduzierbar.

---

## 4 Quellen (Hintergrund)

- Gleißner, W.: *Grundlagen des Risikomanagements* — Risikoaggregation, simulationsbasierte Bestandsgefährdung.
- IDW S 11 / IDW PS 340 — Fortbestehensprognose, Risikofrüherkennung.
- Saltelli, A. et al. (2008/2010): *Global Sensitivity Analysis* — Sobol-Indizes, Stichprobenkonstruktion.
- Jansen, M. (1999): Schätzer für Haupt- und Totaleffekt.
- Vose, D.: *Risk Analysis* — PERT als Beta-Reparametrisierung.
- Sklar (1959); Nelsen: *An Introduction to Copulas* — Copula-Konstruktion.
- Higham, N. (2002): nächste Korrelationsmatrix (positiv semidefinit).
