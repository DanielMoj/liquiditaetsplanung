# ============================================================================
#  tests/test_phase_kop.R   —   SPEZIFIKATION (rot zuerst) fuer Phase 5: Kopula
#
#  Diese Tests spezifizieren den NOCH NICHT gebauten Wrapper phase_kopula()
#  aus R/phase_kopula.R. Sie MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN
#  (die Datei existiert noch nicht -> source() bricht ab; jeder Test spezifiziert
#  zusaetzlich das Zielverhalten). Der Builder erfuellt sie erst mit der
#  Implementierung.
#
#  Vertrag (aus handoff/phase-05-kopula.json):
#   - Selbstenthalten: phase_kopula rechnet build_dists + make_psd selbst (derselbe
#     Engine-Pfad wie phase_korrelation) — bau- und testbar OHNE Vorphase.
#   - Treue: out$R ist bit-identisch zum direkten Engine-Lauf make_psd(M); M wird
#     UNABHAENGIG aus Blatt 07 + der Treiberreihenfolge von build_dists rekonstruiert.
#   - GAUSS-COPULA-VERTRAG (mutationsfest, unabhaengig nachgerechnet gegen die
#     VEROEFFENTLICHTE Formel, NICHT gegen die Engine selbst): der empirische
#     Spearman der korrelierten Ziehungen trifft (6/pi)*asin(R/2). Der Rang ist
#     copula-invariant -> die Formel ist der unabhaengige Orakel-Wert. Eine Mutation
#     im Cholesky-/pnorm-Teil (die die Raender allein unberuehrt liesse) bricht hier.
#   - PERT-Stuetzen-Vertrag: jeder korrelierte Zug liegt im deklarierten Traeger
#     [min,max] je Treiber (die Copula respektiert die Raender).
#   - Ehrliche Meldung: out$validierung$spearman_max_dev / pearson_max_dev sind
#     GENAU die aus Seed und n reproduzierten Kennzahlen (kein gefudgeter Wert).
#   - Determinismus unter set.seed 11; Fehlerfaelle sauber; Nicht-Szenario zurueck.
#
#  Keine fest verdrahteten Absolutzahlen — die Anker werden numerisch aus der
#  Mappe/Engine nachgerechnet; den Tier-1-Anker prueft der Golden-Master in
#  run_all.R. Die Toleranzen stammen 1:1 aus den phasen_anker des Handoffs.
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
source(file.path(.root, "R", "phase_kopula.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Zielmatrix aus Blatt 07 UNABHAENGIG vom Wrapper rekonstruiert: exakt die
# Schritte des Handoff-Ankers — Rohmatrix aus corr_raw, auf die Treiberreihen-
# folge von build_dists ausgerichtet (CV irrelevant, es zaehlt nur die Reihenfolge).
.kop_target <- function(m) {
  spec <- build_dists(m)
  d    <- names(spec)
  raw  <- m$corr_raw
  M    <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"
  M[d, d, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Selbstenthalten + Treue: out$R == direkter Engine-Lauf make_psd(M), bit-
# identisch (Toleranz 0). phase_kopula rechnet die reparierte Matrix selbst,
# ohne Artefakt aus einer Vorphase — genau derselbe Engine-Pfad wie Phase 5a.
# ---------------------------------------------------------------------------
test_that("out$R ist identisch mit dem direkten Engine-Lauf make_psd(M) (Treue, selbstenthalten)", {
  m   <- load_model(WB)
  ref <- make_psd(.kop_target(m))

  out <- phase_kopula(scenario_from_workbook(WB))
  expect_equal(out$R, ref, tolerance = 0)
})

# ---------------------------------------------------------------------------
# Eingabevertrag: dimnames(out$R) und names(out$spec) sind genau die Treiber
# aus build_dists, in Reihenfolge — draw_epistemic_correlated indiziert R und
# spec ueber dieselben Namen; eine Fehlausrichtung wuerde die Copula verdrehen.
# ---------------------------------------------------------------------------
test_that("dimnames(out$R) == names(out$spec) == names(build_dists(mdl)) (Ausrichtung der Copula)", {
  m   <- load_model(WB)
  out <- phase_kopula(scenario_from_workbook(WB))
  expect_identical(names(out$spec), names(build_dists(m)))
  expect_identical(rownames(out$R), names(out$spec))
  expect_identical(colnames(out$R), names(out$spec))
})

# ---------------------------------------------------------------------------
# GAUSS-COPULA-VERTRAG (Anker 1, mutationsfest): der empirische Spearman der
# korrelierten Ziehungen trifft die VEROEFFENTLICHTE Formel (6/pi)*asin(R/2) —
# unabhaengig von draw_epistemic_correlated nachgerechnet. Der Rang ist copula-
# invariant, daher ist die Formel der Orakel-Wert (nicht die Engine gegen sich
# selbst). n und Toleranz exakt aus dem Handoff-Anker. Diagonale ist bei beiden
# Seiten ~1 -> das Maximum sitzt off-diagonal; namensblind ueber max(abs(.)).
# ---------------------------------------------------------------------------
test_that("empirischer Spearman trifft (6/pi)*asin(R/2) — Copula reproduziert die Korrelation", {
  out <- phase_kopula(scenario_from_workbook(WB))
  set.seed(11)
  X  <- draw_epistemic_correlated(out$spec, out$R, 200000)
  es <- cor(X, method = "spearman")
  ex <- (6 / pi) * asin(out$R / 2)
  expect_lte(max(abs(es - ex)), 0.03)
})

# ---------------------------------------------------------------------------
# PERT-Stuetzen-Vertrag (Anker 2): jeder korrelierte Zug liegt im deklarierten
# Traeger [min,max] je Treiber — die Copula respektiert die Raender. n und
# Toleranz exakt aus dem Handoff-Anker; unabhaengig von der wrapper-internen
# Ziehung nachgerechnet (eigener set.seed 11).
# ---------------------------------------------------------------------------
test_that("jeder korrelierte Zug liegt in [min,max] je Treiber (PERT-Stuetzen)", {
  out <- phase_kopula(scenario_from_workbook(WB))
  set.seed(11)
  X  <- draw_epistemic_correlated(out$spec, out$R, 5000)
  ok <- TRUE
  for (j in names(out$spec)) {
    s  <- out$spec[[j]]
    ok <- ok && all(X[, j] >= s$min - 1e-9 & X[, j] <= s$max + 1e-9)
  }
  expect_true(ok)
})

# ---------------------------------------------------------------------------
# Determinismus: der Wrapper seedet intern mit 11; zwei unabhaengige Laeufe
# liefern identische Kennzahlen. draw_epistemic_correlated selbst ist unter
# set.seed(11) reproduzierbar (die Copula-Ziehung haengt am Seed des Szenario-
# schluessels). Beides gepruept — kein stiller RNG-Drift.
# ---------------------------------------------------------------------------
test_that("Determinismus unter Seed 11: Ziehung und Wrapper-Kennzahlen reproduzierbar", {
  out <- phase_kopula(scenario_from_workbook(WB))

  set.seed(11); X1 <- draw_epistemic_correlated(out$spec, out$R, 3000)
  set.seed(11); X2 <- draw_epistemic_correlated(out$spec, out$R, 3000)
  expect_identical(X1, X2)

  out2 <- phase_kopula(scenario_from_workbook(WB))
  expect_equal(out2$validierung$spearman_max_dev, out$validierung$spearman_max_dev, tolerance = 0)
  expect_equal(out2$validierung$pearson_max_dev,  out$validierung$pearson_max_dev,  tolerance = 0)
})

# ---------------------------------------------------------------------------
# Ehrliche Meldung (mutationsfest): out$validierung$spearman_max_dev ist GENAU
# die aus dem gemeldeten Seed und n reproduzierte Kennzahl. Der Wrapper darf die
# Zahl nicht schoenrechnen — recomputet mit out$validierung$seed/n und exakt
# gegengeprueft. Erzwingt zugleich, dass der Wrapper den Seed 11 wirklich nutzt.
# ---------------------------------------------------------------------------
test_that("validierung$spearman_max_dev ist die ehrlich reproduzierte Kennzahl (Seed/n aus dem Objekt)", {
  out <- phase_kopula(scenario_from_workbook(WB))
  v   <- out$validierung
  expect_true(is.numeric(v$spearman_max_dev) && is.finite(v$spearman_max_dev))

  set.seed(v$seed)
  X  <- draw_epistemic_correlated(out$spec, out$R, v$n)
  es <- cor(X, method = "spearman")
  ex <- (6 / pi) * asin(out$R / 2)
  expect_equal(v$spearman_max_dev, max(abs(es - ex)), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Ehrliche Meldung: out$validierung$pearson_max_dev ist die reproduzierte
# maximale Pearson-Abweichung von R (copulabedingte Daempfung bei schiefen
# Raendern) — recomputet mit demselben Seed/n, exakt gegengeprueft. Keine
# Richtungsbehauptung, nur die reine Vertragsformel (max abs Abweichung).
# ---------------------------------------------------------------------------
test_that("validierung$pearson_max_dev ist die ehrlich reproduzierte Kennzahl (Seed/n aus dem Objekt)", {
  out <- phase_kopula(scenario_from_workbook(WB))
  v   <- out$validierung
  expect_true(is.numeric(v$pearson_max_dev) && is.finite(v$pearson_max_dev))

  set.seed(v$seed)
  X   <- draw_epistemic_correlated(out$spec, out$R, v$n)
  ep  <- cor(X)
  ref <- max(abs(ep[upper.tri(ep)] - out$R[upper.tri(out$R)]))
  expect_equal(v$pearson_max_dev, ref, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Der gemeldete Seed ist 11 (Szenario-Schluessel Phase Abhaengigkeit, IMPL_RULES
# §Determinismus) und n ist eine sinnvolle grosse Stichprobe (>= 5000).
# ---------------------------------------------------------------------------
test_that("validierung nennt Seed 11 und eine hinreichend grosse Stichprobe", {
  out <- phase_kopula(scenario_from_workbook(WB))
  expect_identical(out$validierung$seed, 11)
  expect_gte(out$validierung$n, 5000)
})

# ---------------------------------------------------------------------------
# Diagnose-Badges konsistent: diagnostics$reproduziert ist der logische Badge
# "Gauss-Copula reproduziert Korrelation" und folgt der Spearman-Abweichung
# gegen die Reproduktions-Toleranz (0.03); diagnostics$stuetzen_ok ist der
# PERT-Stuetzen-Vertrag. Beide sind logisch und (fuer diese Mappe) TRUE.
# ---------------------------------------------------------------------------
test_that("diagnostics reproduziert/stuetzen_ok sind konsistente logische Badges", {
  out <- phase_kopula(scenario_from_workbook(WB))
  dg  <- out$diagnostics

  expect_true(is.logical(dg$reproduziert))
  expect_true(is.logical(dg$stuetzen_ok))
  # Badge folgt der gemeldeten Kennzahl gegen die Reproduktions-Toleranz.
  expect_equal(dg$reproduziert, out$validierung$spearman_max_dev <= 0.03)
  # Fuer diese Mappe reproduziert die Copula und respektiert die Raender.
  expect_true(dg$reproduziert)
  expect_true(dg$stuetzen_ok)
})

# ---------------------------------------------------------------------------
# Fehlerfall: fehlendes Blatt 07_Korrelation -> sauberer, benannter Fehler
# (nennt "07"), nie ein NULL-Griff.
# ---------------------------------------------------------------------------
test_that("fehlendes Blatt 07_Korrelation fuehrt zu einem sauberen Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$corr_raw <- NULL
  expect_error(phase_kopula(sc), regexp = "07", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Fehlerfall: ein Treiber aus Blatt 06 fehlt in Blatt 07 -> sauberer, benannter
# Fehler VOR dem Indizieren (nennt den fehlenden Treiber), nie ein "subscript
# out of bounds". draw_epistemic_correlated braucht R vollstaendig ueber alle
# Treiber — die Abdeckungspruefung ist Vertrag.
# ---------------------------------------------------------------------------
test_that("Treiber aus 06 nicht in 07 fuehrt zu einem sauberen, benannten Fehler", {
  sc    <- scenario_from_workbook(WB)
  fehlt <- names(build_dists(sc$mdl))[1]
  raw   <- sc$mdl$corr_raw
  sc$mdl$corr_raw <- raw[as.character(raw[[1]]) != fehlt, , drop = FALSE]
  expect_error(phase_kopula(sc), regexp = fehlt)
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_kopula weist ein Nicht-Szenario zurueck", {
  expect_error(phase_kopula(list(mdl = NULL)), "liqui_scenario")
})
