# ============================================================================
#  tests/test_sobol_invarianten.R
#  Invarianten der Sobol-Auswertung (Phase 3, Jansen-Schaetzer) auf den ROHEN
#  Indizes VOR Klippen/Rundung: das Klippen pmin(1, pmax(0, .)) ist Anzeige-
#  Kosmetik und darf Schaetzfehler nicht verdecken (Audit; Gap-Beweis: eine
#  Faktor-2-Mutation im Totaleffekt blieb mit geklippten Werten gruen).
#
#  Seed-/Toleranz-Strategie: der Sobol pinnt seinen Seed 1 selbst
#  (IMPLEMENTATION_RULES, Teil des Szenario-Schluessels) -> alles hier ist
#  deterministisch, die Suite wuerfelt nie. Die Toleranz EPS = 4/sqrt(N)
#  ist am Jansen-MC-Standardfehler kalibriert und laeuft bei PRODUKTIONS-
#  N = 800 (App-Default): bei kleinem N ist der Schaetzfehler so gross, dass
#  [0,1] auch bei KORREKTEM Code reisst (gemessen: S1_raw = -0.14 bei N=48) —
#  ein enges Band dort lehrte nur Bis-gruen-Wuerfeln (Goodhart-Loch).
#  Gemessene Margen bei korrektem Code (N=800, Seed 1):
#    S1_raw in [0.081, 0.426]; max|S1_raw - S1_pred| = 0.047 << EPS = 0.141;
#    min(ST_raw - S1_raw) = -0.067 (gemeinsames VarY-Rauschen) > -EPS.
# ============================================================================
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_sensitivitaet.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# EIN Lauf bei Produktions-N fuer alle Bloecke (Seed 1 intern, deterministisch).
.sc  <- scenario_from_workbook(WB)
.out <- phase_sensitivitaet(.sc, sobol_N = 800, plot = FALSE)
.EPS <- 4 / sqrt(.out$sobol_N)

test_that("sobol_raw liegt vor und publizierte Werte sind exakt clip+round der Rohwerte", {
  expect_true(.out$sobol_available)
  expect_false(is.null(.out$sobol_raw))
  raw <- .out$sobol_raw
  expect_setequal(raw$Treiber, .out$sobol$Treiber)
  i <- match(raw$Treiber, .out$sobol$Treiber)
  expect_equal(.out$sobol$S1[i], round(pmin(1, pmax(0, raw$S1_raw)), 3))
  expect_equal(.out$sobol$ST[i], round(pmin(1, pmax(0, raw$ST_raw)), 3))
})

test_that("rohe Jansen-Indizes liegen VOR dem Klippen in [0,1] (bis auf MC-Standardfehler)", {
  # Der ROHE Index traegt die Information; systematisch < 0 oder > 1 heisst
  # kaputter Schaetzer (falsches VarY, Vorzeichen, fehlender Faktor) — das
  # Klippen wuerde genau das verdecken.
  raw <- .out$sobol_raw
  expect_true(all(raw$S1_raw >= -.EPS & raw$S1_raw <= 1 + .EPS))
  expect_true(all(raw$ST_raw >= -.EPS & raw$ST_raw <= 1 + .EPS))
})

test_that("Ordnung S_T >= S_1 je Treiber (bis auf MC-Standardfehler)", {
  # Population: Totaleffekt = Haupteffekt + Wechselwirkungen >= Haupteffekt.
  # Schaetzer teilen sich VarY -> gemeinsames Rauschen; -EPS deckt es
  # (gemessen -0.067 bei korrektem Code, Marge Faktor 2).
  raw <- .out$sobol_raw
  expect_true(all(raw$ST_raw - raw$S1_raw >= -.EPS))
})

test_that("Kreuzvalidierung: Sobol-Anteile stimmen mit der Tornado-Linearisierung ueberein", {
  # Unabhaengige Vorhersage aus deterministischer Rechnung: der Tornado misst
  # die lokale Steigung c_j = swing/(2*shock); ueber unabhaengigen Gleich-
  # verteilungen der Breite w_j gilt fuer das (nahezu lineare) Modell
  # S1_j ~ ST_j ~ (c_j*w_j)^2 / sum_i (c_i*w_i)^2. Faengt jede Skalen- oder
  # Rollen-Mutation der Jansen-Formeln (Faktor 2, yA/yB-Tausch, VarY-Fehler),
  # die das Klippen verdecken wuerde. Gemessen bei korrektem Code:
  # max|S1_raw - pred| = 0.047, max|ST_raw - pred| = 0.023, EPS = 0.141.
  raw <- .out$sobol_raw
  dr_raw <- as.character(.sc$mdl$dists_raw[[1]])
  expect_true(all(raw$Treiber %in% .out$tornado$driver))
  pred <- vapply(raw$Treiber, function(d) {
    sw <- .out$tornado$swing[.out$tornado$driver == d] / (2 * .out$shock)
    i  <- match(d, dr_raw)
    w  <- as.numeric(.sc$mdl$dists_raw[["p_max"]][i]) - as.numeric(.sc$mdl$dists_raw[["p_min"]][i])
    (sw * w)^2
  }, numeric(1))
  pred <- pred / sum(pred)
  expect_true(all(abs(raw$S1_raw - pred) <= .EPS))
  expect_true(all(abs(raw$ST_raw - pred) <= .EPS))
})

test_that("sobol_raw faellt mit dem Sobol zurueck (kein Blatt 06 / sobol_N = 0)", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$dists_raw <- NULL
  out <- phase_sensitivitaet(sc, sobol_N = 48, plot = FALSE)
  expect_null(out$sobol_raw)
  out0 <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 0, plot = FALSE)
  expect_null(out0$sobol_raw)
})
