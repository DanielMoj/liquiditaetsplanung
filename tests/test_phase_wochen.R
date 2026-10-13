# ============================================================================
#  tests/test_phase_wochen.R
#  Prueft, dass der Phase-2-Wrapper die Engine-Wochenwerte UNVERAENDERT
#  durchreicht (Treue), die Rueckabstimmung Σ Monat = Woche haelt (Engine-Vertrag)
#  und die ehrliche KK-Zins-Grenze ausgewiesen wird. Keine fest verdrahteten
#  Absolutzahlen — den Tier-1-Anker prueft der Golden-Master in run_all.R.
# ============================================================================
# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_wochen.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")
test_that("phase_wochen reicht die Engine-Wochenwerte unveraendert durch", {
  mdl <- load_model(WB); ref <- run_model_weekly(mdl)
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_equal(out$kpis$trough,        ref$kpis$trough)
  expect_equal(out$kpis$trough_week,   ref$kpis$trough_week)
  expect_equal(out$n_weeks,            ref$n_weeks)
  expect_equal(unname(out$cash["Endbestand", ]), unname(ref$cash["Endbestand", ]))
  expect_equal(unname(out$cash["NettoCashflow", ]), unname(ref$cash["NettoCashflow", ]))
})
test_that("Rueckabstimmung Σ NettoCashflow Monat = Woche haelt (Engine-Vertrag)", {
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_true(out$rueckabstimmung$ok)
  expect_equal(out$rueckabstimmung$netcf_woche, out$rueckabstimmung$netcf_monat,
               tolerance = 1e-6)
  # invariant gegenueber Annahmen: Kalendarisierung erhaelt die Summe
  alt <- phase_wochen(scenario_from_workbook(WB, assume_overrides = list(menge = -0.10)), plot = FALSE)
  expect_true(alt$rueckabstimmung$ok)
})
test_that("Wochen-Matrix und -Tabelle haben n_weeks Eintraege und die erwarteten Zeilen", {
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_equal(ncol(out$cash), out$n_weeks)
  expect_equal(nrow(out$table), out$n_weeks)
  expect_true(all(c("NettoCashflow", "Endbestand", "KK_Inanspruchnahme") %in% rownames(out$cash)))
  expect_true(all(c("Woche", "ab", "Endbestand", "unter_Reserve") %in% names(out$table)))
})
test_that("KK-Zins Monat und Woche werden beide ausgewiesen (ehrliche Grenze, Richtung payday-abhaengig)", {
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  # Beide Sichten muessen vorhanden und endlich sein.
  expect_true(is.finite(out$kk_zins$monat))
  expect_true(is.finite(out$kk_zins$woche))
  # Die Differenz wird ausgewiesen (ehrliche Grenze sichtbar gemacht, nicht versteckt).
  # KEINE feste Richtung woche>=monat: sie haengt am Zahltag (F-12 / §2.12) und
  # kippt mit dem Payday-Timing. Beim Beispiel-Mandanten (Lohnlauf Monatsende)
  # ueberzeichnet die Monatssicht, also liegt die Woche darunter.
  expect_true(is.finite(out$kk_zins$woche - out$kk_zins$monat))
})
test_that("KK-Zins Woche folgt der Vertragsformel: max(0,-Vorwochensaldo) * kk_zins * Tage/365", {
  mdl <- load_model(WB)
  wk  <- run_model_weekly(mdl)
  end     <- unname(wk$cash["Endbestand", ])
  kkz     <- unname(wk$cash["KK_Zins", ])
  netcf   <- unname(wk$cash["NettoCashflow", ])
  wk_days <- as.integer(wk$wk_end - wk$wk_start) + 1L
  prev    <- c(mdl$open_cash, end[-length(end)])
  expect_equal(kkz, pmax(0, -prev) * mdl$kk_zins * wk_days / 365, tolerance = 1e-9)
  # Rekursions-Identitaet der veroeffentlichten Wochenzeilen:
  # Endbestand = Vorwochensaldo + NettoCashflow - KK_Zins
  expect_equal(end, prev + netcf - kkz, tolerance = 1e-9)
})
test_that("Mengen -10% senkt den woechentlichen Tiefpunkt", {
  base <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  down <- phase_wochen(scenario_from_workbook(WB, assume_overrides = list(menge = -0.10)), plot = FALSE)
  expect_true(down$kpis$trough < base$kpis$trough)
})
test_that("Plot wird in den Szenario-Workspace geschrieben (sofern Grafikgeraet)", {
  sc  <- scenario_from_workbook(WB)
  out <- phase_wochen(sc, plot = TRUE)
  if (!is.na(out$plot_path)) {
    expect_true(file.exists(out$plot_path))
    expect_gt(file.info(out$plot_path)$size, 0)
  } else {
    succeed("kein Grafikgeraet verfuegbar — Plot uebersprungen (sauberer Fallback)")
  }
})
test_that("phase_wochen weist ein Nicht-Szenario zurueck", {
  expect_error(phase_wochen(list(mdl = NULL)), "liqui_scenario")
})
