# ARCHITECTURE — Liqui-App

## Vier Rollen
- **kernel/** — `engine.R`, `distributions.R`. Eingefroren. Die validierte Rechen-Engine, ohne Mandantenzahlen, ohne UI.
- **legacy/** — die sieben Phasenskripte + die Python-Generatoren. Vorlage: der Builder liest sie, um daraus `R/phase_*()` zu schreiben. Werden in der App NICHT ausgefuehrt.
- **R/** — neue Schicht: `phase_*()`-Funktionen (Engine-Aufruf → Ergebnisobjekt), `scenario.R` (Szenario-Abstraktion), spaeter `export.R`.
- **app/** — Shiny-Oberflaeche (bslib), ruft ausschliesslich `R/` auf.

## Engine-Schnittstelle (eingefroren — daran wird gebaut)
- `load_model(WB)` → `mdl`. Liest 10 Blaetter (`01_Positionen` … `10_OPOS`), Kopfzeile Zeile 4, Historie ≥ 12 Monate. Felder u. a.: `pos, base, assume, fin, dists_raw, corr_raw, tax, loans, opos, start_date, kk_limit, kk_zins, mindestreserve, HMAX`.
- `run_model(mdl, assume=NULL, noise=NULL)` → `list(guv, cash, kpis, et, darlehen, opos)`. `kpis = {trough, trough_month, maxKK, months_below, result, end}`. `cash` enthaelt u. a. `Endbestand`, `KK_Inanspruchnahme`, `KK_Zins`.
- `run_model_weekly(mdl)` → `list(cash, kpis, n_weeks, wk_start, wk_end)`, rueckabgestimmt auf den Monatslauf.
- `validate_history(mdl)` → `{erg_eng, erg_naiv, ok, mass_ok, mass_res}` (GuV-Identitaet + Cash-Massenerhaltung je Gruppe).
- `implied_dpo_dso(mdl, grps)` → DSO/DPO in Tagen.
- `fmt(x)` → DE-Format (Punkt = Tausender, Komma = Dezimal).
- distributions.R: `build_dists`, `estimate_cv_harmonic`, `draw_epistemic_correlated`, `make_psd`, `rpert`, `qpert`.

## Szenario
Ein Szenario = {Mappe + Annahme-Overrides + Sim-Parameter (Pfade, Seed)} mit eigenem Workspace-Verzeichnis (`R/scenario.R`). Vergleich = N Szenarien rechnen und ihre Ergebnisobjekte gegenueberstellen. Cache-Schluessel = Hash aus Annahmen + Sim-Parametern.

## Phasen-DAG (steuert, was beim Aendern neu gerechnet wird)
```
load_model(WB)
 ├─ phase_forecast        (eigenstaendig)
 ├─ phase_wochen          (eigenstaendig)
 ├─ phase_sensitivitaet   (eigenstaendig)
 ├─ phase_verteilungen    -> verteilungen_spec.rds          [Basis]
 ├─ phase_abhaengigkeit   -> korrelation.rds    braucht 07 + spec
 ├─ phase_simulation      -> endbestand.rds     braucht spec + korrelation
 └─ phase_varianzzerlegung                      braucht spec + korrelation
```

## Cache-Invalidierung (pro Szenario)
| Geaendert | Neu rechnen | Aus Cache |
|---|---|---|
| Eingaben in der Mappe | alles | — |
| nur Blatt 07 Korrelation | Abhaengigkeit, Simulation, Varianz | Forecast … Verteilungen |
| nur MC-Pfade / Seed | Simulation, Varianz | Forecast … Abhaengigkeit |
| nichts (Szenario erneut oeffnen) | — | alles |

## Nebenlaeufigkeit
Deterministische Phasen (Forecast, Wochen, Tornado) laufen synchron und erscheinen sofort. Die genestete Monte-Carlo (Simulation, Varianz) laeuft in einem `mirai`/`future`-Worker mit Fortschritt + Abbrechen; die Oberflaeche bleibt bedienbar.

## Plots
v1 nutzt die bestehenden Base-R-PNGs (rendern sofort, sobald erzeugt). Interaktive Plots (`plotly`/`ggiraph`) sind ein spaeteres Upgrade, kein Startbedarf.

## Verpacken (am Ende, ausserhalb des Autobuilds)
`.exe` ueber `electricShine` bzw. `DesktopDeployR` + Inno Setup, R-portable mitgebuendelt. Ein einmaliger Schritt, kein Baustein.
