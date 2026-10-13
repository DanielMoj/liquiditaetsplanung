# ============================================================================
#  tests/test_phase_forecast.R
#  Prueft, dass der Phase-1-Wrapper die Engine-Werte UNVERAENDERT durchreicht
#  (Treue), plus Struktur und Plausibilitaet. Keine fest verdrahteten Zahlen —
#  die absoluten Kopfzahlen prueft der Golden-Master in run_all.R.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis,
# darum nicht auf das aktuelle Arbeitsverzeichnis verlassen).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_forecast.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

test_that("phase_forecast reicht die Engine-Werte unveraendert durch", {
  mdl <- load_model(WB); ref <- run_model(mdl)
  out <- phase_forecast(scenario_from_workbook(WB))
  expect_equal(out$kpis$result,       ref$kpis$result)
  expect_equal(out$kpis$trough,       ref$kpis$trough)
  expect_equal(out$kpis$trough_month, ref$kpis$trough_month)
  expect_equal(unname(out$cash["Endbestand", ]), unname(ref$cash["Endbestand", ]))
})

test_that("Historie ist konsistent (GuV-Identitaet + Massenerhaltung)", {
  out <- phase_forecast(scenario_from_workbook(WB))
  expect_true(out$validation$ok)
  expect_true(out$validation$mass_ok)
})

test_that("GuV/Cash haben HMAX Spalten und die erwarteten Zeilen", {
  sc <- scenario_from_workbook(WB); out <- phase_forecast(sc)
  expect_equal(ncol(out$guv),  sc$mdl$HMAX)
  expect_equal(ncol(out$cash), sc$mdl$HMAX)
  expect_true("Ergebnis"   %in% rownames(out$guv))
  expect_true("Endbestand" %in% rownames(out$cash))
})

test_that("DSO/DPO liefert vier benannte Werte", {
  out <- phase_forecast(scenario_from_workbook(WB))
  expect_true(all(c("umsatz","betr_ertrag","material","sachkosten") %in% names(out$dso_dpo)))
})

test_that("Mengen -10% verschiebt den Tiefpunkt nach unten", {
  base <- phase_forecast(scenario_from_workbook(WB))
  down <- phase_forecast(scenario_from_workbook(WB, assume_overrides = list(menge = -0.10)))
  expect_true(down$kpis$trough < base$kpis$trough)
})
