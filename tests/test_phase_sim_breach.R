# ============================================================================
#  tests/test_phase_sim_breach.R  --  Mutations-Nachschub 2 fuer phase-06-fan (M5)
#
#  Gate-Ueberlebender M5 (P_solvent <- 1 + P_breach statt 1 - P_breach): auf der
#  Fixture liegt mit dem Mappen-kk_limit kein Pfad-Tiefpunkt unter der KK-Linie,
#  P_breach ist dort 0 -- und am trivialen Punkt sind 1 - 0 und 1 + 0
#  ununterscheidbar. Die bestehende Selbstkonsistenz-Pruefung (test_phase_sim.R:
#  P_solvent == 1 - P_breach) exerziert den Breach-Zweig also nie in einem
#  Regime, in dem der Flip sichtbar wuerde.
#
#  Dieses Orakel erzwingt ein NICHT-triviales Regime: kk_limit = 0 -- bei
#  Pfad-Tiefpunkten um -5 Mio (Plan-Tiefpunkt der Beispielklinik) reisst damit
#  jeder Pfad die Linie. Ein Regime-Waechter fordert P_breach_ext > 0 hart ein:
#  faellt das Regime je auf den trivialen Punkt zurueck, scheitert der Test
#  LAUT, statt nichts zu messen. Alle Solvenz-Kennzahlen werden danach
#  identitaetsbasiert aus out$B + dem tatsaechlich verwendeten Limit
#  nachgerechnet: kein Sollwert-Literal, keine Verteilungsannahme,
#  deterministisch ueber den gepinnten Seed 2026. Auf dem Mutanten M5 weicht
#  P_solvent um 2*P_breach ab -> rot.
#
#  Warum Feld-Override statt Konstruktor-Parameter: scenario_from_workbook
#  kennt kein kk_limit-Argument -- der Wert kommt ausschliesslich aus der Mappe
#  (load_model), und assume_overrides erreicht nur Eintraege von mdl$assume
#  (unbekannte Namen werden mit Warnung ignoriert, kk_limit steht dort nicht).
#  Die einzige Naht ist daher, am konstruierten Szenario sc$mdl$kk_limit VOR
#  dem phase_simulation-Aufruf zu ueberschreiben -- genau dieses mdl liest der
#  Wrapper (mdl <- sc$mdl), und dieselbe Naht nutzt bereits test_phase_sim.R
#  fuer seine Fehlerfaelle (sc$mdl$corr_raw / sc$mdl$dists_raw).
#
#  Kill-Logik ersetzt Rot-zuerst: rot auf dem Mutanten M5, gruen auf main.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_simulation.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

test_that("Breach-Regime (kk_limit = 0): P_breach/P_solvent identitaetsbasiert aus out$B — der Flip auf 1 + P_breach wird sichtbar", {
  # EIN Wrapper-Lauf; kleines S fuer die Laufzeit (der Konstruktor laesst sim
  # ueberschreiben), Seed bleibt der Szenario-Schluessel 2026 — wie die
  # Noise-Vorlage (test_phase_sim_noise.R), eigener Workspace ueber den Namen.
  S  <- 25L
  sc <- scenario_from_workbook(WB, name = "BreachRegime",
                               sim = list(paths = S, seed = 2026))

  # kk_limit = 0 direkt am konstruierten Objekt (Begruendung im Kopfkommentar):
  # phase_simulation prueft trough < -kk_limit, und die Endbestaende der
  # Fixture liegen tief im Minus — jeder Pfad reisst die Null-Linie.
  sc$mdl$kk_limit <- 0

  out <- phase_simulation(sc)

  # Externe Nachrechnung ALLEIN aus out$B + dem tatsaechlich verwendeten Limit
  # (dieselbe Definition wie der Wrapper: Tiefpunkt je Pfad, Anteil unter -KK).
  kk_used      <- sc$mdl$kk_limit
  trough_ext   <- apply(out$B, 1, min)
  P_breach_ext <- mean(trough_ext < -kk_used)

  # (a) Regime-Waechter: nie wieder nur der triviale Punkt P_breach = 0.
  expect_true(P_breach_ext > 0,
              info = paste("Regime-Waechter verletzt: P_breach_ext == 0 —",
                           "kein Pfad reisst die KK-Linie. Der Breach-Zweig",
                           "wird damit nur am trivialen Punkt exerziert, wo",
                           "der M5-Flip (1 + P_breach) unsichtbar ist.",
                           "kk_limit-Setzung des Tests pruefen."))

  # (b) P_breach ist exakt der extern nachgerechnete Anteil.
  expect_equal(out$metrics$P_breach, P_breach_ext, tolerance = 1e-12)

  # (c) P_solvent ist das Komplement 1 - P_breach_ext — hier, im nicht-trivialen
  # Regime, unterscheidet das Vorzeichen: M5 liefert 1 + P_breach (Abstand
  # 2*P_breach_ext > 0).
  expect_equal(out$metrics$P_solvent, 1 - P_breach_ext, tolerance = 1e-12)

  # (d) Die Prozent-Kennzahlen folgen denselben externen Groessen.
  expect_equal(out$metrics$P_breach_pct,  100 * P_breach_ext,       tolerance = 1e-12)
  expect_equal(out$metrics$P_solvent_pct, 100 * (1 - P_breach_ext), tolerance = 1e-12)
})
