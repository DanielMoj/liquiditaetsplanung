# CLAUDE.md — Liqui-App

Desktop-Software (R-Shiny) für stochastische Krankenhaus-Liquiditaetsplanung. Sie **verpackt** die bestehende, validierte Rechen-Engine in eine Oberflaeche — die Engine wird verpackt, **nicht neu geschrieben**.

DOD: Rscript tests/run_all.R

## Unverrueckbare Regeln
- `kernel/engine.R` und `kernel/distributions.R` werden NIE editiert. Sie sind die validierte Engine und schreibgeschuetzt. Jede gewuenschte Verhaltensaenderung gehoert in einen Wrapper in `R/`, niemals in den kernel.
- Die drei Tier-1-Kopfzahlen der Beispielklinik sind unantastbar: Historie −5.235.600 · Tiefpunkt −5.135.990 (M3) · Planergebnis 2.672.411. `tests/run_all.R` prueft sie bei jedem Lauf; driftet eine, ist die Aenderung unzulaessig.
- Die Mappe ist der Engine-Vertrag. Blattnamen exakt (`01_Positionen` … `10_OPOS`), Kopfzeile in Zeile 4 (`skip = 3`), Historie ≥ 12 Monate.
- Eine Laufzeit: alles in R. Kein Python im Lauf — die `build_workbook*.py`-Generatoren werden nach `openxlsx2` portiert.
- Schwere Monte-Carlo (Phase Simulation/Varianz) laeuft im Hintergrund-Worker (`mirai`/`future`), nie synchron im UI-Thread.
- Deutsche Zahlenformatierung ueberall ueber `fmt()` aus dem kernel.

## So wird eine Phase gebaut (Muster aus der fertigen Phase 1)
1. `R/phase_<name>.R`: eine Funktion `phase_<name>(sc)`, die die Engine aufruft und ein Ergebnisobjekt zurueckgibt (Zahlen + Plot-Pfade + Diagnose), statt zu drucken. Vorlage: das gleichnamige Skript in `legacy/`.
2. `tests/test_phase_<name>.R`: testthat-Test gegen die Engine-Werte; wo es ein Control gibt, zusaetzlich ein shinytest2-Test.
3. Shiny-Modul in `app/`: die Funktion an ein Control haengen, Ergebnis anzeigen.
4. Zeile in `CAPABILITY_MAP.md` auf ✅ setzen.

## Struktur
`kernel/` eingefroren · `legacy/` Vorlagen zum Wrappen · `fixtures/` Regressionsanker · `docs/` für den gefuehrten Modus · `R/` neue Phasenfunktionen · `app/` Shiny · `tests/` Tests.

Details: ARCHITECTURE.md (DAG, Cache, Szenario) · IMPLEMENTATION_RULES.md (Schwellen) · CAPABILITY_MAP.md (Funktion → UI → Test).
