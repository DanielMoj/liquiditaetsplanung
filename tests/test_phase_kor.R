# ============================================================================
#  tests/test_phase_kor.R   —   SPEZIFIKATION (rot zuerst) fuer Phase 5: Korrelation
#
#  Diese Tests spezifizieren den NOCH NICHT gebauten Wrapper
#  phase_korrelation()/.plot_abhaengigkeit() aus R/phase_korrelation.R.
#  Sie MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (die Datei existiert
#  noch nicht -> source() bricht ab; jeder Test spezifiziert zusaetzlich das
#  Zielverhalten). Der Builder erfuellt sie erst mit der Implementierung.
#
#  Vertrag (aus handoff/phase-05-korrelation.json):
#   - Treue: out$R ist bit-identisch zum direkten Engine-Lauf make_psd(M).
#     M wird UNABHAENGIG aus Blatt 07 und der Treiberreihenfolge von build_dists
#     rekonstruiert — die UI verbiegt Zielmatrix, Ausrichtung und Reparatur nicht.
#   - MUTATIONSFEST: PSD-Eigenschaft der reparierten Matrix via eigen()
#     nachgerechnet, NICHT ueber make_psd — faengt eine Mutation der Reparatur.
#   - Gueltige Korrelationsmatrix: Einheitsdiagonale, symmetrisch, Werte in [-1,1].
#   - Eingabevertrag Phase 6: dimnames(R) == names(spec), beide Dimensionen,
#     in der Treiberreihenfolge (die Simulation indiziert R[drivers, drivers]).
#   - Artefakt fuer Phase 6: korrelation.rds liest sich bit-identisch zurueck.
#   - Fehlerfaelle sauber; Plot-Fallback ohne Grafikgeraet; Nicht-Szenario zurueckgewiesen.
#
#  Keine fest verdrahteten Absolutzahlen — die Anker werden numerisch aus der
#  Mappe nachgerechnet; den Tier-1-Anker prueft der Golden-Master in run_all.R.
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
source(file.path(.root, "R", "phase_korrelation.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Zielmatrix aus Blatt 07 UNABHAENGIG vom Wrapper rekonstruiert: exakt die
# Schritte des Handoff-Ankers — Rohmatrix aus corr_raw, auf die Treiberreihen-
# folge von build_dists ausgerichtet. Fuer die Korrelation ist die CV irrelevant,
# daher build_dists ohne est (Vorgaben genuegen) — es zaehlt nur die Reihenfolge.
.kor_target <- function(m) {
  spec <- build_dists(m)
  d    <- names(spec)
  raw  <- m$corr_raw
  M    <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"
  M[d, d, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Treue: out$R == direkter Engine-Lauf make_psd(M). Toleranz 0 (bit-identisch)
# — die UI reicht Zielmatrix, Ausrichtung und Reparatur unveraendert durch.
# ---------------------------------------------------------------------------
test_that("out$R ist identisch mit dem direkten Engine-Lauf make_psd(M) (Treue)", {
  m   <- load_model(WB)
  M   <- .kor_target(m)
  ref <- make_psd(M)

  out <- phase_korrelation(scenario_from_workbook(WB))
  expect_equal(out$R, ref, tolerance = 0)
})

# ---------------------------------------------------------------------------
# MUTATIONSFEST: PSD-Eigenschaft der reparierten Matrix unabhaengig via eigen()
# nachgerechnet — NICHT ueber make_psd. Der kleinste Eigenwert ist >= 0
# (Toleranz 1e-6). Eine Mutation der Reparaturfunktion bricht genau hier.
# ---------------------------------------------------------------------------
test_that("min(eigen(out$R)) >= 0 — unabhaengig via eigen nachgerechnet (mutationsfest)", {
  out <- phase_korrelation(scenario_from_workbook(WB))
  e   <- eigen((out$R + t(out$R)) / 2, symmetric = TRUE, only.values = TRUE)$values
  expect_gte(min(e), -1e-6)
})

# ---------------------------------------------------------------------------
# Gueltige Korrelationsmatrix: Einheitsdiagonale, symmetrisch, Werte in [-1,1].
# ---------------------------------------------------------------------------
test_that("out$R ist eine gueltige Korrelationsmatrix (Diagonale 1, symmetrisch, [-1,1])", {
  out <- phase_korrelation(scenario_from_workbook(WB))
  R   <- out$R
  # Namensblind (Konvention test_verteilungsschicht.R): diag() einer symmetrisch
  # benannten Matrix erbt die dimnames, rep(1, n) ist namenlos — der Namens-
  # vertrag selbst steht im dimnames-Test unten, nicht hier.
  expect_lt(max(abs(diag(R) - 1)), 1e-9)
  expect_equal(R, t(R), tolerance = 1e-9)
  expect_true(all(R >= -1 - 1e-9 & R <= 1 + 1e-9))
})

# ---------------------------------------------------------------------------
# Eingabevertrag Phase 6: Zeilen- und Spaltennamen der reparierten Matrix sind
# genau die Treiber aus spec, in dessen Reihenfolge (die Simulation indiziert
# spaeter R[drivers, drivers]).
# ---------------------------------------------------------------------------
test_that("dimnames(out$R) sind genau names(out$spec) in Reihenfolge (Eingabevertrag Phase 6)", {
  m   <- load_model(WB)
  out <- phase_korrelation(scenario_from_workbook(WB))

  expect_identical(rownames(out$R), names(out$spec))
  expect_identical(colnames(out$R), names(out$spec))
  # Spec-Treiberreihenfolge = build_dists der eingefrorenen Engine (Treue der Ausrichtung).
  expect_identical(names(out$spec), names(build_dists(m)))
})

# ---------------------------------------------------------------------------
# Artefakt-Vertrag fuer Phase 6: korrelation.rds liest sich bit-identisch zu
# out$R zurueck und liegt im Szenario-Workspace.
# ---------------------------------------------------------------------------
test_that("kor_path: korrelation.rds liest sich identisch zu out$R zurueck", {
  sc  <- scenario_from_workbook(WB)
  out <- phase_korrelation(sc)
  expect_false(is.na(out$kor_path))
  expect_true(file.exists(out$kor_path))
  expect_identical(readRDS(out$kor_path), out$R)
  expect_true(startsWith(normalizePath(out$kor_path), normalizePath(sc$workspace)))
})

# ---------------------------------------------------------------------------
# Zielmatrix out$M ist die aus Blatt 07 rekonstruierte, auf die Treiber
# ausgerichtete Rohmatrix VOR Reparatur — unveraendert durchgereicht.
# ---------------------------------------------------------------------------
test_that("out$M ist die auf die Treiber ausgerichtete Zielmatrix aus Blatt 07 (vor Reparatur)", {
  m   <- load_model(WB)
  out <- phase_korrelation(scenario_from_workbook(WB))
  expect_equal(out$M, .kor_target(m), tolerance = 0)
  expect_identical(rownames(out$M), names(out$spec))
  expect_identical(colnames(out$M), names(out$spec))
})

# ---------------------------------------------------------------------------
# Reparatur-Kennzahlen: method nennt make_psd (der eingefrorene Kernel, NICHT
# nearPD); ev_before/ev_after sind die kleinsten Eigenwerte von Ziel/Reparatur;
# max_aenderung = groesste Abweichung Ziel -> repariert (0, falls Ziel schon PSD).
# ---------------------------------------------------------------------------
test_that("method/ev_before/ev_after/max_aenderung spiegeln die make_psd-Reparatur", {
  m   <- load_model(WB)
  M   <- .kor_target(m)
  R   <- make_psd(M)
  out <- phase_korrelation(scenario_from_workbook(WB))

  expect_match(out$method, "make_psd", ignore.case = TRUE)
  expect_equal(out$ev_before,
               min(eigen((M + t(M)) / 2, symmetric = TRUE, only.values = TRUE)$values),
               tolerance = 1e-9)
  expect_equal(out$ev_after,
               min(eigen((R + t(R)) / 2, symmetric = TRUE, only.values = TRUE)$values),
               tolerance = 1e-9)
  expect_gte(out$ev_after, -1e-6)
  expect_equal(out$max_aenderung, max(abs(R - M)), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Diagnose-Zeile spiegelt Struktur und PSD-Status konsistent (Kennzahlen des Schirms).
# n_treiber == Rang der Matrix; ziel_psd/repariert_psd sind die logischen Badges
# aus den kleinsten Eigenwerten (repariert immer TRUE).
# ---------------------------------------------------------------------------
test_that("diagnostics sind konsistent mit R, spec und PSD-Status", {
  m   <- load_model(WB)
  out <- phase_korrelation(scenario_from_workbook(WB))
  dg  <- out$diagnostics

  expect_equal(dg$n_treiber, length(out$spec))
  expect_equal(dg$n_treiber, nrow(out$R))

  expect_true(is.logical(dg$ziel_psd))
  expect_true(is.logical(dg$repariert_psd))
  # Zielmatrix-Badge folgt dem kleinsten Eigenwert der ROHEN Zielmatrix.
  ev_ziel <- min(eigen((out$M + t(out$M)) / 2, symmetric = TRUE, only.values = TRUE)$values)
  expect_equal(dg$ziel_psd, ev_ziel >= -1e-8)
  # Die reparierte Matrix ist per Konstruktion PSD.
  expect_true(dg$repariert_psd)
})

# ---------------------------------------------------------------------------
# Fehlerfall: fehlendes Blatt 07_Korrelation -> sauberer, benannter Fehler.
# ---------------------------------------------------------------------------
test_that("fehlendes Blatt 07_Korrelation fuehrt zu einem sauberen Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$corr_raw <- NULL                                 # Blatt fehlt
  expect_error(phase_korrelation(sc), regexp = "07", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Fehlerfall: ein Treiber aus Blatt 06 fehlt in Blatt 07 -> sauberer, benannter
# Fehler VOR dem Indizieren (nennt den fehlenden Treiber), nie ein "subscript
# out of bounds". Das erzwingt die Abdeckungspruefung des Wrappers.
# ---------------------------------------------------------------------------
test_that("Treiber aus 06 nicht in 07 fuehrt zu einem sauberen, benannten Fehler", {
  sc   <- scenario_from_workbook(WB)
  d    <- names(build_dists(sc$mdl))
  fehlt <- d[1]                                           # ein garantiert vorhandener Treiber
  raw  <- sc$mdl$corr_raw
  sc$mdl$corr_raw <- raw[as.character(raw[[1]]) != fehlt, , drop = FALSE]  # Zeile entfernt
  expect_error(phase_korrelation(sc), regexp = fehlt)
})

# ---------------------------------------------------------------------------
# Plot-Fallback: Pfad im Workspace, sonst NA — nie ein harter Abbruch ohne
# Grafikgeraet (sauberer Headless-Fallback von .plot_abhaengigkeit).
# ---------------------------------------------------------------------------
test_that("Korrelations-Heatmap wird in den Szenario-Workspace geschrieben (sofern Grafikgeraet)", {
  sc  <- scenario_from_workbook(WB)
  out <- phase_korrelation(sc)
  if (!is.na(out$plot_path)) {
    expect_true(file.exists(out$plot_path))
    expect_gt(file.info(out$plot_path)$size, 0)
    expect_true(startsWith(normalizePath(out$plot_path), normalizePath(sc$workspace)))
  } else {
    # Headless-Vertrag: der Fallback ist genau NA (nicht NULL, nicht "") —
    # echte Assertion statt succeed() (Red-Team-Befund: unfailbarer Zweig).
    expect_identical(out$plot_path, NA_character_)
  }
})

# ---------------------------------------------------------------------------
# .plot_abhaengigkeit liefert bei nicht schreibbarem Ziel NA statt zu stoppen
# (Headless-Fallback direkt am Helfer geprueft — faengt eine Mutation, die den
# tryCatch-Fallback durch stop()/succeed() ersetzt).
# ---------------------------------------------------------------------------
test_that(".plot_abhaengigkeit liefert NA bei nicht schreibbarem Pfad (Headless-Fallback)", {
  out <- phase_korrelation(scenario_from_workbook(WB))
  bad <- file.path(.root, "kein_solches_verzeichnis_xyz", "kor.png")
  expect_true(is.na(.plot_abhaengigkeit(out$R, bad)))
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_korrelation weist ein Nicht-Szenario zurueck", {
  expect_error(phase_korrelation(list(mdl = NULL)), "liqui_scenario")
})
