# Neuen Mandanten anlegen — Anleitung

Diese Mappe ist eine reine **Eingabemappe**. Gerechnet wird komplett in R (`engine.R` + die nummerierten Phasen-Skripte); in der Excel-Datei stehen keine Formeln. Du trägst Daten und Annahmen ein, die Engine liest sie und erzeugt Forecast, Sensitivität und Simulation.

## 1. Leere Mappe erzeugen

```bash
python3 build_workbook_neuer_mandant.py        # -> Mandant_NEU.xlsx
```

Vorher im Skript `IST_JAHR` auf das Kalenderjahr deiner 12 Ist-Monate setzen (Standard 2025). Danach die Datei je Mandant umbenennen, z. B. `Klinik_Musterstadt.xlsx`. Alle weiteren Aufrufe nehmen diesen Dateinamen als Argument.

## 2. Farbcodierung

| Farbe | Bedeutung | Tust du etwas? |
|---|---|---|
| **Gelb** | Eingabe, die du setzt (Annahmen, Plan, Eröffnungssalden, Steuersätze, Bandbreiten) | ausfüllen |
| **Blau** | Pflichtdaten, beobachtet/strukturell (Ist-/BWA-Werte + Klassifikation in 01) | ausfüllen |
| **Grau** | von der Engine erzeugt oder strukturell fix (geschätzte cv in 06, Diagonale 1,0 in 07) | stehen lassen |
| **Beige/kursiv** | reiner Kontext, von der Engine nicht gelesen (untere Zeilen in 03) | optional, egal |
| ohne Farbe | Beschriftungen, Schlüssel, Hinweise, Kopfzeilen | nicht ausfüllen |

Farben sind rein visuell — `read_excel` ignoriert sie.

## 3. Reihenfolge

Erst der Pflichtteil (Blätter 01–05, 08), dann Phase 1 fahren, dann die Unsicherheits-Phasen. Die Treiberauswahl in 06 ist **Ergebnis** von Phase 2, nicht Voraussetzung.

1. **01_Positionen** — je BWA-/GuV-Zeile eine Zeile: klassifizieren (`grp`, `cashprofile`, `driver`, `ust`) und die Monats-Istwerte eintragen.
2. **02_Zahlungsprofile** — Zahlungskurven aus dem OPOS-Aging; in 01 unter `cashprofile` per Namen referenzieren.
3. **03_Stammdaten & Eröffnung** — `Forecast-Beginn`, `open_cash`, `kk_limit`, `kk_zins`, `mindestreserve`.
4. **04_Annahmen** — Punktwerte der Treiber + `hmax`.
5. **05_Finanzierungsplan** — Investitionen, Fördermittel, Tilgung, Darlehensaufnahme je Monat (0, wo nichts).
6. **08_Steuern** — Rechtsform, Status, Sätze.
7. **Phase 1** laufen lassen, bis Validierung und Cash-Massenerhaltung sauber sind.
8. **Phase 2 (Tornado)** laufen lassen → sehen, welche Annahmen den Tiefpunkt bewegen.
9. **06_Verteilungen** mit genau diesen Treibern + `p_min`/`p_max` füllen; **Sobol** zum Schärfen.
10. **Phase 3** → Verteilungen.
11. Bei ≥2 abhängigen Treibern: **07_Korrelation** füllen, **Phase 4**.
12. **Phase 5** → Monte-Carlo.

**Minimal lauffähig (Phase 1):** 01, 02, 03, 04, 05, 08. Die Blätter 06/07/09/10 dürfen zunächst leer bleiben.

## 4. Die Blätter im Detail

### 01_Positionen
Pro Zeile: `pos` (Kurzschlüssel), `label` (Klartext), dann die drei Klassifikationsspalten und 12 Monatsspalten (EUR, **Aufwand positiv**).

- Es braucht **mindestens 12 Monatsspalten**; die Engine nimmt die letzten 12.
- `grp`, `cashprofile`, `driver`, `ust` müssen exakt aus den Token-Listen unten stammen.
- **Tippfehler bei `driver` fallen still durch**: die Zeile wächst dann mit Faktor 1 (kein Fehler, keine Warnung). Bei `cashprofile` dagegen bricht der Lauf hart ab. Also genau schreiben.
- Für einen Mandanten desselben Typs kannst du die Klassifikation (Spalten C–F) aus einer bestehenden Mappe übernehmen und nur die Monate neu eintragen.

### 02_Zahlungsprofile
`f1` = Anteil, der im Leistungs-/Rechnungsmonat zahlungswirksam wird, `f2` = Folgemonat, usw.

- **Summe je Profil ≈ 1,0** — es wird nicht normiert. Summe < 1 lässt Cash verschwinden, > 1 erzeugt Cash aus dem Nichts.
- **Lückenlos ab f1** — die Engine verdichtet gefüllte Felder der Reihe nach; eine Lücke verschiebt den Verzug.
- Der Profilname ist genau der String, den du in 01 unter `cashprofile` einträgst.
- Löschen nur, wenn keine 01-Zeile mehr darauf zeigt (sonst Abbruch). Hinzufügen ist harmlos; ein unreferenziertes Profil wird ignoriert.

### 03_Stammdaten & Eröffnung
Gelb füllen: `Forecast-Beginn` (Format `JJJJ-MM`), `open_cash` (Anfangsbestand Zahlungsmittel), `kk_limit`, `kk_zins`, `mindestreserve`. Die beigen Zeilen (Forderungen, Verbindlichkeiten, Vorräte, Rückstellungen, Darlehen) sind reiner Kontext und werden nicht gerechnet — leer lassen oder zur Orientierung füllen, egal.

### 04_Annahmen
Punktwerte als Anteil (z. B. `0.04` = 4 %). `hmax` = Anzahl Forecast-Monate, ganzzahlig und **≤ Spaltenzahl in 05** (Standardvorlage: 12).

### 05_Finanzierungsplan
Vier Pflichtzeilen, 12 Monate. Wo nichts passiert, bleibt **0** stehen (nicht leer — leere Zellen erzeugen NA und kippen den Cashflow).

### 06_Verteilungen
Erst **nach dem Tornado** befüllen. Pro relevantem Treiber eine Zeile:

- `driver` = **Annahmename aus 04** (`menge`, `preis`, `tarif`, `energie`, `material`, `pflegebudget`, `inflation`) — *nicht* die Transmissions-Tokens der 01-Spalte (`menge_preis`/`infl`/`flat`).
- `mode` steht nicht hier, sondern ist der Punktwert aus 04.
- `p_min`/`p_max` = epistemische Bandbreite (Urteil, kein Rechenwert).
- `cv` = aleatorische Monatsstreuung; 0 bei reinen Jahresgrößen. Für `menge`/`material` schätzt die Engine die cv aus der Historie — die Vorgabe hier ist nur Rückfall.
- Treiber ohne Zeile bleiben deterministisch.

### 07_Korrelation
Erst sinnvoll bei ≥2 abhängigen Treibern. Zeilen und Spalten sind dieselben Treiber wie in 06, gleiche Reihenfolge; Diagonale 1,0; symmetrisch; off-diagonal in [-1, 1].

### 08_Steuern
**Gesetzliche Sätze sind vorbelegt** und selten zu ändern: `kst_satz` 0,15, `soli_satz` 0,055, `gewst_messzahl` 0,035, `gewst_freibetrag` 24.500, `ust_lag` 1. **Mandantenspezifisch** setzen: `rechtsform`, `gewst_status`, `ust_status`, `ust_methode`, `gewst_hebesatz` (Gemeinde), `entnahme_quote`. Die Vorbelegung (Kapitalgesellschaft, gewerblich, USt-pflichtig, Soll) ist ein neutraler Start — anpassen.

### 09_Darlehen / 10_OPOS
Optional. `09` für einzelne Kredite mit Tilgungsplan, `10` für offene Posten im Nahbereich (ersetzt dort den statistischen Carry-in). Beide leer = bisheriges Verhalten.

## 5. Token-Referenz

**`grp`** (Spalte C) — bestimmt die GuV-Zuordnung:

| grp | fließt in |
|---|---|
| umsatz | Umsatzerlöse |
| bestand | Bestandsveränderung |
| material | Materialaufwand |
| betr_ertrag | sonst. betr. Erträge |
| sopo | Auflösung Sonderposten |
| personal | Personalaufwand |
| sachkosten | Sach-/Betriebskosten |
| afa | Abschreibungen |
| zins_e / zins_a | Zinsertrag / Zinsaufwand (EBT) |
| neutral_e / neutral_a | neutrale Erträge / Aufwendungen (EBT) |
| steuer | sonstige Steuern |

**`driver`** (Spalte E) — bestimmt das Jahreswachstum:

| driver | Wachstumsfaktor |
|---|---|
| menge_preis | (1+menge)·(1+preis) |
| menge | (1+menge) |
| material | (1+menge)·(1+material) |
| tarif | (1+tarif) |
| energie | (1+energie) |
| pflegebudget | (1+pflegebudget) |
| infl | (1+inflation) |
| flat | 1 (kein Wachstum) |

**`cashprofile`** (Spalte D) — bestimmt den Zahlungszeitpunkt:

| cashprofile | Wirkung |
|---|---|
| inmonth | Cash = Betrag im selben Monat |
| noncash | kein Cash-Effekt |
| annual_jan | ganzer Jahresbetrag im M1 |
| abschlag | Jahresbetrag gleichmäßig 1/12 |
| quarterly | Quartalssumme im Quartalsendmonat (M3/6/9/12) |
| *Profilname aus 02* | Lag-Kurve f1, f2, … |

**`ust`** (Spalte F) — `0` / `0.07` / `0.19`, nur wirksam bei `ust_status = pflichtig`.

## 6. Phasen-Skripte

Aufrufmuster: `Rscript <skript> Mandant_NEU.xlsx`

| Phase | Skript | Zweck | Braucht |
|---|---|---|---|
| 1 | `01_forecast.R` | Historie validieren + deterministischer Forecast | 01, 02, 03, 04, 05, 08 |
| 1b | `01b_wochen.R` | Wochenliquidität aus dem Monatslauf | wie Phase 1 |
| 2 | `02_sensitivitaet.R` | Tornado + Sobol: welche Treiber bewegen den Tiefpunkt | Tornado: wie 1; Sobol: zusätzlich Bandbreiten aus 06 |
| 3 | `03_verteilungen.R` | Verteilungen der gewählten Treiber (PERT + cv) | 06 |
| 4 | `04_abhaengigkeit.R` | Korrelation der Treiber (Gauß-Copula, PSD-Reparatur) | 06, 07 |
| 5 | `05_simulation.R` | genestete Monte-Carlo (Endbestand-Verteilung, Schwellen) | 06, 07 + Output aus 3/4 |
| 5b | `05b_varianzzerlegung.R` | Varianzzerlegung des Tiefpunkts | wie Phase 5 |

## 7. Häufige Stolpersteine

- **Unbekanntes cashprofile** → harter Abbruch `Unbekanntes Profil: …`. Jeder `cashprofile`-Wert in 01 muss ein Schlüsselwort sein oder als Profilname in 02 stehen.
- **Tippfehler bei `driver`** → kein Abbruch, aber die Zeile wächst nicht (Faktor 1). Stumm — deshalb prüfen.
- **Profilsummen / Cash-Massenerhaltung** → Phase 1 bricht mit `Cash-Massenerhaltung verletzt` ab. Profilsummen (≈ 1) und Klassifikation prüfen.
- **06-Treibernamen** müssen Annahmenamen aus 04 sein, nicht die 01-Tokens.
- **12 Monate** in 01 sind das Minimum; die Engine nimmt die letzten 12.
- **`hmax` ≤ Spaltenzahl in 05**, sonst Abbruch.
- **05 nie leer** lassen — 0 ist der neutrale Wert.
