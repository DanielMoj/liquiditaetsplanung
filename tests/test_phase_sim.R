# ============================================================================
#  tests/test_phase_sim.R   —   SPEZIFIKATION (rot zuerst) fuer phase-06-fan
#
#  Einheit phase-06-fan (Simulation, Kern): der Planner hat die urspruengliche
#  Phase 6 wegen Diff-Budget in drei Einheiten geschnitten — phase-06-fan baut
#  den Wrapper phase_simulation()/.plot_simulation() (R/phase_simulation.R) und
#  rechnet den VOLLSTAENDIGEN Ergebnis-Vertrag `out`; die Schwester-Einheiten
#  phase-06-monate (Monats-Tabelle) und phase-06-deckung (Solvenz-Verdikt)
#  projizieren dieses `out` nur. Diese Datei spezifiziert daher genau den
#  `out`-Kontrakt der Fan-Einheit.
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/phase_simulation.R
#  existiert noch nicht -> source() bricht ab; jeder Test spezifiziert zusaetzlich
#  das Zielverhalten). Der Builder erfuellt sie erst mit der Implementierung.
#
#  Vertrag (aus handoff/phase-06-fan.json):
#   - Genestete Monte-Carlo ueber die EINGEFRORENE Engine: aeussere Schicht
#     korrelierte epistemische Jahreswerte je Treiber (draw_epistemic_correlated,
#     Seed 2026), innere Schicht monatliches multiplikatives Rauschen; jeder Pfad
#     laeuft durch run_model. Ergebnisobjekt statt Konsolendruck.
#   - SELBSTENTHALTEN wie phase_kopula: spec (build_dists mit estimate_cv_harmonic-
#     CV, dieselbe dokumentierte Basis-Allokation wie Phase 4) und R (make_psd aus
#     Blatt 07, derselbe Engine-Pfad wie Phase 5) werden SELBST gerechnet — bau-
#     und testbar OHNE Vorphase.
#   - Treue/Selbstkonsistenz der abgeleiteten Metriken: fan == unabhaengig aus B
#     nachgerechnete Monatsquantile; trough == Minimum je Pfad; P_under == Anteil
#     unter Mindestreserve; P_breach == Anteil Tiefpunkte unter -kk_limit;
#     P_solvent == 1 - P_breach (faengt vertauschte Quantil-Etiketten / falsche
#     Schwellen). Quantil-Monotonie P5<=P25<=P50<=P75<=P95 je Monat.
#   - PERT-Stuetzen-Vertrag: jeder Theta[s,d] in [min,max] je Treiber.
#   - Determinismus unter set.seed 2026 (Seed = Szenario-Schluessel, kein
#     vorzeitiger RNG-Verbrauch): der erste simulierte Treibervektor trifft eine
#     frisch gesetzte Ziehung.
#   - Plan-Tiefpunkt-Treue: metrics$plan_trough == run_model ohne Rauschen
#     (die Simulation verbiegt die Tier-1-Planrechnung nicht).
#   - Artefakt fuer Phase 7 (Varianzzerlegung): endbestand.rds liest sich
#     bit-identisch zu out$B zurueck.
#   - Fehlerfaelle sauber (fehlendes Blatt 07 / 06); Plot-Fallback ohne
#     Grafikgeraet (NA, kein harter Abbruch); Nicht-Szenario zurueckgewiesen.
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
source(file.path(.root, "R", "phase_simulation.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Unabhaengige Rekonstruktion der SELBSTENTHALTENEN Bausteine — exakt die
# Schritte der Handoff-Anker, ohne den Wrapper zu befragen:
#   spec = build_dists mit estimate_cv_harmonic-CV auf der dokumentierten
#          Basis-Allokation (menge = {menge, menge_preis}, material = {material}).
#   R    = make_psd der auf die build_dists-Treiberreihenfolge ausgerichteten
#          Rohmatrix aus Blatt 07 (identischer Engine-Pfad wie phase_kopula).
# ---------------------------------------------------------------------------
.sim_est <- function(m) {
  D   <- m$pos$driver
  vol <- colSums(m$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  mat <- colSums(m$base[D == "material",                  , drop = FALSE])
  list(menge = estimate_cv_harmonic(vol), material = estimate_cv_harmonic(mat))
}
.sim_spec <- function(m) build_dists(m, .sim_est(m))
.sim_R <- function(m) {
  d   <- names(.sim_spec(m))
  raw <- m$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"
  R <- make_psd(M[d, d, drop = FALSE])
  dimnames(R) <- list(d, d)
  R
}

# Ein gemeinsames Szenario + EIN schwerer MC-Lauf, LAZY memoisiert ueber ein
# Promise: die genestete Simulation ist teuer (jeder Pfad laeuft durch run_model).
# Die lesenden Selbstkonsistenz-Tests teilen sich dasselbe Ergebnisobjekt; die
# Fehler-/Mutationsfaelle bauen eigene Szenarien (sie brechen ohnehin vor der MC
# ab). Der phase_simulation()-Aufruf steht bewusst als Promise-AUSDRUCK auf Top-
# Level (nicht im Rumpf einer Closure) — erst der erste sim_out()-Zugriff forciert
# und cached ihn. Die Closure referenziert nur das bekannte Top-Level-Binding
# .cache (keinen im Rumpf aufgeloesten Namen).
.sc    <- scenario_from_workbook(WB)
.cache <- new.env(parent = emptyenv())
delayedAssign("out", phase_simulation(.sc), assign.env = .cache)
sim_out <- function() .cache$out

# ---------------------------------------------------------------------------
# Selbstenthalten (Treue, Toleranz 0): out$spec und out$R sind bit-identisch zu
# den direkt aus Mappe + eingefrorener Engine rekonstruierten Bausteinen — der
# Wrapper rechnet ohne Artefakt einer Vorphase und verbiegt die Spezifikation
# nicht. dimnames(out$R) == names(out$spec) (Ausrichtung fuer die Copula).
# ---------------------------------------------------------------------------
test_that("out$spec und out$R sind identisch zum direkten Engine-Lauf (selbstenthalten, Treue)", {
  m   <- load_model(WB)
  out <- sim_out()

  expect_equal(out$spec, .sim_spec(m), tolerance = 0)
  expect_equal(out$R,    .sim_R(m),    tolerance = 0)

  expect_identical(names(out$spec),   names(.sim_spec(m)))
  expect_identical(rownames(out$R),   names(out$spec))
  expect_identical(colnames(out$R),   names(out$spec))
})

# ---------------------------------------------------------------------------
# Anker "fan": Selbstkonsistenz + Monotonie der Quantilbaender. out$fan
# entspricht den UNABHAENGIG aus B nachgerechneten Monatsquantilen (Toleranz
# 1e-9), und je Monat sind die Quantile geordnet P5<=P25<=P50<=P75<=P95 —
# faengt vertauschte Etiketten und eine falsche Quantil-Ableitung.
# ---------------------------------------------------------------------------
test_that("out$fan == aus B nachgerechnete Monatsquantile und je Monat monoton geordnet", {
  out <- sim_out()
  q   <- apply(out$B, 2, quantile, probs = c(.05, .25, .5, .75, .95))
  expect_equal(unname(out$fan), unname(q), tolerance = 1e-9)

  mono <- all(q["5%", ]  <= q["25%", ] &
              q["25%", ] <= q["50%", ] &
              q["50%", ] <= q["75%", ] &
              q["75%", ] <= q["95%", ])
  expect_true(mono)
})

# ---------------------------------------------------------------------------
# Anker "trough / P_under / P_breach / P_solvent": Selbstkonsistenz der
# Metriken, alles UNABHAENGIG aus B und den Modell-Schwellen nachgerechnet
# (Schwellen aus dem Modell, nicht hartcodiert). Toleranz 1e-12.
# ---------------------------------------------------------------------------
test_that("Metriken selbstkonsistent aus B und Modell-Schwellen (trough/P_under/P_breach/P_solvent)", {
  m   <- load_model(WB)
  out <- sim_out()
  u   <- function(x) unname(x)

  expect_equal(u(out$trough),  u(apply(out$B, 1, min)),                 tolerance = 1e-12)
  expect_equal(u(out$P_under), u(colMeans(out$B < m$mindestreserve)),   tolerance = 1e-12)
  expect_equal(u(out$metrics$P_breach),  mean(out$trough < -m$kk_limit), tolerance = 1e-12)
  expect_equal(out$metrics$P_solvent, 1 - out$metrics$P_breach,          tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Abgeleitete Schirm-Kennzahlen folgen konsistent aus trough / P_breach:
# trough_median/p05/p95 aus der Tiefpunktverteilung, *_pct als 100 x Anteil.
# ---------------------------------------------------------------------------
test_that("abgeleitete metrics (trough_median/p05/p95, *_pct) folgen konsistent aus trough/P_breach", {
  out <- sim_out()
  expect_equal(out$metrics$trough_median, unname(median(out$trough)),          tolerance = 1e-12)
  expect_equal(out$metrics$trough_p05,    unname(quantile(out$trough, .05)),   tolerance = 1e-12)
  expect_equal(out$metrics$trough_p95,    unname(quantile(out$trough, .95)),   tolerance = 1e-12)
  expect_equal(out$metrics$P_breach_pct,  100 * out$metrics$P_breach,          tolerance = 1e-12)
  expect_equal(out$metrics$P_solvent_pct, 100 * out$metrics$P_solvent,         tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Anker "Theta in [min,max]" (PERT-Stuetzen-Vertrag): jeder epistemische
# Jahreswert out$Theta[s, d] liegt im deklarierten Traeger [min, max] je Treiber
# — die Copula respektiert die Raender. spec UNABHAENGIG aus Blatt 06 + der
# dokumentierten CV-Basis rekonstruiert. Toleranz 1e-9.
# ---------------------------------------------------------------------------
test_that("jeder Theta[s,d] liegt im Traeger [min,max] je Treiber (PERT-Stuetzen)", {
  m    <- load_model(WB)
  spec <- .sim_spec(m)
  out  <- sim_out()

  ok <- TRUE
  for (d in names(spec)) {
    s  <- spec[[d]]
    ok <- ok && all(out$Theta[, d] >= s$min - 1e-9 & out$Theta[, d] <= s$max + 1e-9)
  }
  expect_true(ok)
})

# ---------------------------------------------------------------------------
# Anker "Determinismus (Seed 2026)": set.seed(seed) direkt vor der epistemischen
# Ziehung (erster RNG-Verbraucher) — der erste simulierte Treibervektor trifft
# eine frisch gesetzte Ziehung ohne vorzeitigen RNG-Verbrauch. Toleranz 0.
# Der Seed ist Teil des Szenario-Schluessels (IMPLEMENTATION_RULES: Phase
# Simulation 2026).
# ---------------------------------------------------------------------------
test_that("Determinismus: set.seed(seed) vor der Ziehung reproduziert Theta[1, ] exakt", {
  out <- sim_out()
  set.seed(out$metrics$seed)
  T1  <- draw_epistemic_correlated(out$spec, out$R, out$metrics$S)
  expect_identical(unname(T1[1L, ]), unname(out$Theta[1L, ]))
})

# ---------------------------------------------------------------------------
# Der gemeldete Seed ist 2026 (Szenario-Schluessel Phase Simulation,
# IMPLEMENTATION_RULES §Determinismus).
# ---------------------------------------------------------------------------
test_that("metrics$seed ist 2026 (Szenario-Schluessel, IMPL_RULES Determinismus)", {
  out <- sim_out()
  expect_equal(out$metrics$seed, 2026)
})

# ---------------------------------------------------------------------------
# Anker "metrics$plan_trough": Treue der deterministischen Wirbelsaeule — der
# ohne Rauschen gerechnete Plan-Tiefpunkt des Wrappers (assume = Mode,
# noise = NULL) ist identisch mit dem direkten Engine-Lauf
# run_model(load_model(WB))$kpis$trough. Toleranz 0.
# ---------------------------------------------------------------------------
test_that("metrics$plan_trough == run_model(load_model(WB))$kpis$trough (Tier-1-Wirbelsaeule)", {
  m   <- load_model(WB)
  out <- sim_out()
  expect_equal(out$metrics$plan_trough, run_model(m)$kpis$trough, tolerance = 0)
})

# ---------------------------------------------------------------------------
# Anker "endbestand_path round-trip": Artefakt-Vertrag fuer Phase 7
# (Varianzzerlegung) — die gesicherte endbestand.rds liest sich bit-identisch
# zur Ergebnis-Matrix out$B und liegt im Szenario-Workspace.
# ---------------------------------------------------------------------------
test_that("endbestand.rds liest sich bit-identisch zu out$B zurueck (Eingabe Phase 7)", {
  out <- sim_out()
  expect_false(is.na(out$endbestand_path))
  expect_true(file.exists(out$endbestand_path))
  expect_identical(readRDS(out$endbestand_path), out$B)
  expect_true(startsWith(normalizePath(out$endbestand_path), normalizePath(.sc$workspace)))
})

# ---------------------------------------------------------------------------
# Struktur: B ist SxH, fan ist 5xH, P_under Laenge H, trough Laenge S,
# Theta ist Sxk (k = Anzahl Treiber), metrics$H == HMAX.
# ---------------------------------------------------------------------------
test_that("Struktur: B SxH, fan 5xH, P_under laenge H, trough laenge S, Theta Sxk", {
  m   <- load_model(WB)
  out <- sim_out()
  S   <- out$metrics$S
  H   <- m$HMAX

  expect_equal(dim(out$B),   c(S, H))
  expect_equal(dim(out$fan), c(5L, H))
  expect_length(out$P_under, H)
  expect_length(out$trough,  S)
  expect_equal(nrow(out$Theta), S)
  expect_equal(ncol(out$Theta), length(out$spec))
  expect_equal(out$metrics$H, H)
  expect_true(is.numeric(S) && S >= 1)
})

# ---------------------------------------------------------------------------
# Diagnose-Badges konsistent: n_treiber == Anzahl Treiber; deterministisch und
# stuetzen_ok sind logische Badges, die GENAU der jeweils real gepruueften
# Eigenschaft folgen (keine lazily-immer-TRUE Badges). Fuer diese Mappe sind
# beide TRUE.
# ---------------------------------------------------------------------------
test_that("diagnostics n_treiber/deterministisch/stuetzen_ok sind konsistente Badges", {
  out <- sim_out()
  dg  <- out$diagnostics

  expect_equal(dg$n_treiber, length(out$spec))
  expect_true(is.logical(dg$deterministisch))
  expect_true(is.logical(dg$stuetzen_ok))

  # deterministisch folgt der reproduzierten Ziehung (Badge an reale Eigenschaft binden).
  set.seed(out$metrics$seed)
  T1 <- draw_epistemic_correlated(out$spec, out$R, out$metrics$S)
  expect_equal(dg$deterministisch, identical(unname(T1[1L, ]), unname(out$Theta[1L, ])))

  # stuetzen_ok folgt dem PERT-Stuetzen-Vertrag ueber alle Theta-Zuege.
  st <- TRUE
  for (d in names(out$spec)) {
    s  <- out$spec[[d]]
    st <- st && all(out$Theta[, d] >= s$min - 1e-9 & out$Theta[, d] <= s$max + 1e-9)
  }
  expect_equal(dg$stuetzen_ok, st)

  # Fuer diese Mappe reproduziert die Simulation deterministisch und haelt die Raender.
  expect_true(dg$deterministisch)
  expect_true(dg$stuetzen_ok)
})

# ---------------------------------------------------------------------------
# Plot-Fallback: .plot_simulation schreibt den Fan-Chart in den Workspace, sonst
# genau NA — nie ein harter Abbruch ohne Grafikgeraet. Echte Assertion je Zweig
# (Pfad -> Datei existiert; headless -> is.na), kein unfailbarer succeed()-Zweig.
# ---------------------------------------------------------------------------
test_that("Plot-Pfad im Workspace, sonst NA (sauberer Headless-Fallback)", {
  out <- sim_out()
  if (!is.na(out$plot_path)) {
    expect_true(file.exists(out$plot_path))
    expect_gt(file.info(out$plot_path)$size, 0)
    expect_true(startsWith(normalizePath(out$plot_path), normalizePath(.sc$workspace)))
  } else {
    # Headless-Vertrag: der Fallback ist genau NA (nicht NULL, nicht "").
    expect_true(is.na(out$plot_path))
  }
})

# ---------------------------------------------------------------------------
# Fehlerfall: fehlendes Blatt 07_Korrelation -> sauberer, benannter Fehler
# (nennt "07"), nie ein NULL-Griff. draw_epistemic_correlated braucht R.
# ---------------------------------------------------------------------------
test_that("fehlendes Blatt 07_Korrelation fuehrt zu einem sauberen, benannten Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$corr_raw <- NULL
  expect_error(phase_simulation(sc), regexp = "07", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Fehlerfall: fehlendes Blatt 06_Verteilungen -> sauberer, benannter Fehler
# (nennt "06"); ohne Rand-Verteilungen ist die epistemische Ziehung nicht
# ableitbar.
# ---------------------------------------------------------------------------
test_that("fehlendes Blatt 06_Verteilungen fuehrt zu einem sauberen, benannten Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$dists_raw <- NULL
  expect_error(phase_simulation(sc), regexp = "06", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Fehlerfall: Blatt 06 vorhanden, aber ohne Treiberzeilen -> sauberer Fehler
# (build_dists liefert eine leere Spec; der Wrapper stoppt statt leer zu
# simulieren).
# ---------------------------------------------------------------------------
test_that("leeres Blatt 06 ohne Treiber fuehrt zu einem sauberen Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$dists_raw <- sc$mdl$dists_raw[0, , drop = FALSE]
  expect_error(phase_simulation(sc))
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_simulation weist ein Nicht-Szenario zurueck", {
  expect_error(phase_simulation(list(mdl = NULL)), "liqui_scenario")
})
