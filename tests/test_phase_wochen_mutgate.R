# ============================================================================
#  tests/test_phase_wochen_mutgate.R
#  Nachgeschobene Tests aus dem Mutations-Score-Gate (bash orchestrate.sh
#  mutgate phase-02-wochen): ueberlebende Mutanten in R/phase_wochen.R waren
#  benannte Loecher der Suite. Jede Pruefung bindet ein Ausgabefeld des
#  Wrappers an Engine- bzw. Vertragswerte — keine hartcodierten
#  Erwartungswerte. Kill-Logik als Rot-zuerst-Ersatz: jeder Test ist auf dem
#  jeweiligen Mutanten rot (Retest des Gates, .orch/mut-phase-02-wochen/)
#  und auf dem echten Stand gruen (volle Suite).
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

test_that("Rueckabstimmungs-Differenz ist in der dokumentierten Richtung Monat minus Woche ausgewiesen", {
  # Loch (Mutant M1): das Vorzeichen von rueckabstimmung$diff war ungetestet.
  # Bit-genaue Identitaet der ausgewiesenen Felder untereinander — die
  # Differenz ist klein (Kalendarisierung erhaelt die Summe), aber nicht 0.
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_identical(out$rueckabstimmung$diff,
                   out$rueckabstimmung$netcf_monat - out$rueckabstimmung$netcf_woche)
})

test_that("kk_zins-Sichten sind treu zur Engine: Woche aus der Wochen-, Monat aus der Monatsmatrix", {
  # Loch (Mutant M3): kk_zins$woche/monat waren nur auf Endlichkeit geprueft —
  # eine falsche Datenquelle (Monatssicht als Wochensicht) blieb unsichtbar.
  mdl <- load_model(WB)
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_equal(out$kk_zins$woche, sum(run_model_weekly(mdl)$cash["KK_Zins", ]))
  expect_equal(out$kk_zins$monat, sum(run_model(mdl)$cash["KK_Zins", ]))
  # Nachbarloch aus Lauf 1 (Kehrwert ueberlebte): der Faktor ist Woche relativ
  # zu Monat — genau die im Schirm ausgewiesene ehrliche Grenze.
  expect_equal(out$kk_zins$faktor, out$kk_zins$woche / out$kk_zins$monat)
})

test_that("Wochentabelle spiegelt die Wochenmatrix spaltenweise treu", {
  # Loch (Mutant M4): die Tabellen-WERTE waren ungetestet, nur die Struktur —
  # eine vertauschte Spalte (Endbestand trug NettoCashflow) blieb unsichtbar.
  # Nachbarloch (Lauf 1 + Trockenlauf): unter_Reserve-Inversion ueberlebte.
  mdl <- load_model(WB)
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_identical(out$table$Endbestand,    unname(out$cash["Endbestand", ]))
  expect_identical(out$table$NettoCashflow, unname(out$cash["NettoCashflow", ]))
  expect_identical(out$table$KK,            unname(out$cash["KK_Inanspruchnahme", ]))
  expect_identical(out$table$unter_Reserve, unname(out$cash["Endbestand", ] < mdl$mindestreserve))
  # Engine-Anker: Anzahl markierter Wochen == Engine-KPI weeks_below.
  expect_equal(sum(out$table$unter_Reserve), out$kpis$weeks_below)
})

test_that("KK-Limit-Diagnose folgt Engine-maxKK gegen das Mandanten-Limit", {
  # Loch (Mutant M5): diagnostics$kk_innerhalb_limit war ungetestet — ein
  # invertierter Grenzvergleich meldete Einhaltung bei Ueberschreitung.
  # Erwartung aus Engine-Lauf + Mandantenwert (1e-6 = dokumentierte
  # Epsilon-Grenze des Wrappers), nie hartcodiert.
  mdl <- load_model(WB)
  wk  <- run_model_weekly(mdl)
  out <- phase_wochen(scenario_from_workbook(WB), plot = FALSE)
  expect_identical(out$diagnostics$kk_innerhalb_limit,
                   wk$kpis$maxKK <= mdl$kk_limit + 1e-6)
})
