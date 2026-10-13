# IMPLEMENTATION RULES — Liqui-App

Verbindliche Schwellen und Invarianten. Der Reviewer prueft jeden Baustein gegen diese Liste; ein Verstoss ist ein Critical-Finding.

## Korrektheit (hart)
- **Tier-1-Euro-Identitaet** ist der Anker und unantastbar: Historie −5.235.600 · Tiefpunkt −5.135.990 in M3 · Planergebnis 2.672.411. Jede Aenderung, die eine der drei Zahlen bewegt, ist abzulehnen. `tests/run_all.R` ist das Gate und laeuft bei jedem Baustein mit.
- **kernel/ wird nie editiert.** `engine.R`, `distributions.R` sind schreibgeschuetzt. Verhaltensaenderungen ausschliesslich in `R/`-Wrappern. Ein Diff, der `kernel/` beruehrt, ist Critical. Ausnahme: vom Menschen ausdruecklich autorisierte Instrumentierung (Assertions / INVARIANTE-Kommentare ohne Verhaltensaenderung im Golden Run) als eigener, freigegebener Commit — der autonome Builder bleibt ausgesperrt (Kernel-Guard).
- **Engine-Vertrag Mappe.** Blattnamen exakt: `01_Positionen, 02_Zahlungsprofile, 03_Stammdaten_Eroeffnung, 04_Annahmen, 05_Finanzierungsplan, 06_Verteilungen, 07_Korrelation, 08_Steuern, 09_Darlehen, 10_OPOS`. Kopfzeile Zeile 4 (`skip = 3`). Historie ≥ 12 Monate.
- **Konsistenz-Gates sichtbar lassen.** `validate_history` prueft GuV-Identitaet + Cash-Massenerhaltung; diese Pruefungen werden in der UI angezeigt, nicht versteckt. Das Gate prueft GuV/Masse, NICHT das Zahlungs-Timing (ein Lag-Tausch erhaelt die Masse) — diese Grenze offen benennen, nicht uebertuenchen.

## Technik
- **Eine Laufzeit (R).** Kein Python im Lauf. Die `build_workbook*.py`-Generatoren werden nach `openxlsx2` portiert; bis dahin nicht im DOD-Pfad.
- **Determinismus.** Simulationen mit gesetztem Seed (`set.seed`); Seed ist Teil des Szenario-Schluessels. Bestehende Seeds beibehalten: Phase Simulation 2026, Phase Abhaengigkeit 11, Sobol 1.
- **Nebenlaeufigkeit.** Genestete MC nur im `mirai`/`future`-Worker mit Fortschritt + Abbrechen; nie synchron im UI-Thread.
- **Cache-Invalidierung** folgt dem DAG (ARCHITECTURE.md). Keine Phase neu rechnen, deren Eingaben unveraendert sind; keine Phase aus dem Cache nehmen, deren Eingaben sich geaendert haben.
- **Zahlenformat DE** ausschliesslich ueber `fmt()`. Keine eigenen `format()`-Aufrufe mit `big.mark`-Abweichung.
- **Keine Browser-Storage-APIs** in der Shiny-UI; Zustand in Reactives/R.

## Test
- Jede Phase: ein testthat-Test gegen die Engine-Werte. Wo ein Control existiert, zusaetzlich ein `shinytest2`-Test, der das Control headless faehrt und das Ergebnis (Value-Box / Tabelle) prueft.
- Tests muessen echte Fehlerfaelle abdecken (z. B. fehlendes optionales Blatt → Engine faellt sauber zurueck), nicht nur den Happy-Path.
- Der Tier-1-Anker ist die Garantie, dass die UI die Engine nicht still verbogen hat — er wird nie umgangen oder mit gelockerten Toleranzen „gruen gemacht".
- **KK-Zins-Richtung Woche vs. Monat: nie eine feste Richtung behaupten** — weder als Test-Assert noch im UI-Text. Die Richtung haengt am Payday-Timing (F-12/§2.12): Monatsende-Payday → Monatssicht ueberzeichnet → Woche < Monat; mittmonatiger Payday kehrt es um.

## Ehrliche Grenzen (nicht verschweigen)
- Die Gates pruefen Zahlen und Logik, **nicht** die Optik der Oberflaeche. An der Phasengrenze prueft der Verifier (`scripts/verifier.sh`, ersetzt den manuellen HALT; das Skript gehoert zur Entwicklungsumgebung und liegt nicht in diesem Repository, siehe https://github.com/DanielMoj/harness-template): Anker und Kennzahl-Existenz deterministisch in Code ueber die festen DOM-IDs; Reiter-Existenz/Label und sichtbar kaputtes Layout per LLM-Check (je Kriterium ein Call, Verdikt strikt PASS/FAIL, UNVERIFIED zaehlt nicht als gruen). Feines Optik-Urteil bleibt eine ehrliche Grenze.
- Bei Mandaten mit engen Treiberbaendern hat das Korrelationsblatt nahezu keinen Effekt auf die Tiefpunktverteilung — das offenlegen statt ueberbetonen.
