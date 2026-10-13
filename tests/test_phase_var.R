# ============================================================================
#  tests/test_phase_var.R   —   SPEZIFIKATION (rot zuerst) fuer
#  phase-07-varianzzerlegung
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN
#  (R/phase_varianzzerlegung.R existiert noch nicht -> source() bricht ab;
#  jeder Test spezifiziert zusaetzlich das Zielverhalten). Der Builder erfuellt
#  sie erst mit der Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche.
#
#  Vertrag (Vorlage legacy/05b_varianzzerlegung.R, App-Konventionen):
#   - phase_varianzzerlegung(sc, S = sc$sim$paths) auf einem liqui_scenario;
#     Nicht-Szenarien werden zurueckgewiesen. Drei Laeufe gleicher Groesse S
#     ueber die EINGEFRORENE Engine (draw_epistemic_correlated + run_model,
#     selbstenthalten wie phase_simulation, kein Aufruf von phase_simulation):
#       T_full — epistemische Jahreswerte korreliert gezogen, Rauschen AN;
#       T_epi  — DIESELBEN Ziehungen Theta, Rauschen AUS (Noise = Einsen);
#       T_ale  — Treiber fix auf mdl$assume (Modes), Rauschen AN.
#     Tiefpunkt je Pfad = min ueber die Endbestand-Zeile von run_model.
#     Rauschlogik bit-treu zur inneren Schicht von R/phase_simulation.R
#     (gemeinsamer Monatsschock je Basis menge/material, exp-transformiert,
#     sigma aus spec$cv) — nicht neu erfunden.
#   - Zerlegung als REINE Funktion getrennt vom Wrapper (Muster
#     deckung_from_out): varianz_from_troughs(T_full, T_epi, T_ale) -> Liste
#     mit var_gesamt, var_epi, var_ale, var_inter (= var_gesamt - var_epi -
#     var_ale), sd_gesamt/sd_epi/sd_ale (sqrt, bei epi/ale mit max(., 0)),
#     anteil_epi/anteil_ale/anteil_inter (Anteile an var_gesamt, Summe exakt
#     1), S (Laenge der Vektoren; stopifnot gleiche Laengen).
#   - Ergebnisobjekt des Wrappers: phase = "varianzzerlegung", scenario =
#     sc$name, die drei ROH-Vektoren T_full/T_epi/T_ale (Laenge S), Theta
#     (S x k, dimnames = Treiber), alle Felder von varianz_from_troughs,
#     seed, S.
#   - Determinismus: set.seed(seed) DIREKT vor der ersten epistemischen
#     Ziehung (IMPLEMENTATION_RULES §Determinismus, Seed = Szenario-
#     Schluessel); zweiter Aufruf mit identischem sc reproduziert T_full und
#     T_ale exakt.
#
#  Orakel-Politik: EXTERNE Orakel statt Selbstkonsistenz — der Test rechnet
#  var()/sqrt()/Anteile SELBST aus den zurueckgegebenen Roh-Vektoren und
#  rekonstruiert T_epi[s] fuer herausgegriffene Pfade komplett ausserhalb des
#  Wrappers (assume + Theta[s, ], Noise = Einsen, min der Endbestand-Zeile).
#  Keine fest verdrahteten Absolutzahlen; den Tier-1-Anker prueft der
#  Golden-Master in run_all.R.
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
source(file.path(.root, "R", "phase_varianzzerlegung.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Unabhaengige Rekonstruktion der Treiberliste — exakt der selbstenthaltene
# spec-Pfad aus test_phase_sim.R (build_dists mit estimate_cv_harmonic-CV auf
# der dokumentierten Basis-Allokation), ohne den Wrapper zu befragen.
# ---------------------------------------------------------------------------
.var_est <- function(m) {
  D   <- m$pos$driver
  vol <- colSums(m$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  mat <- colSums(m$base[D == "material",                  , drop = FALSE])
  list(menge = estimate_cv_harmonic(vol), material = estimate_cv_harmonic(mat))
}
.var_spec <- function(m) build_dists(m, .var_est(m))

# Ein gemeinsames Szenario + EIN Zerlegungslauf, LAZY memoisiert ueber ein
# Promise (Muster test_phase_sim.R): drei Laeufe je S Pfade durch run_model
# sind teuer, deshalb haelt ein kleines S die Suite schnell (Muster
# test_phase_sim_deckung.R: sim = list(paths = klein, seed = 2026)).
.sc    <- scenario_from_workbook(WB, name = "VarSpec",
                                 sim = list(paths = 16L, seed = 2026))
.cache <- new.env(parent = emptyenv())
delayedAssign("out", phase_varianzzerlegung(.sc), assign.env = .cache)
var_out <- function() .cache$out

# ---------------------------------------------------------------------------
# Reine Funktion, EXTERNES Orakel an synthetischen Vektoren: der Test rechnet
# var()/sqrt()/Anteile selbst und vergleicht mit JEDEM Feld — kein
# Feld-gegen-Feld-Vergleich derselben Funktion. Toleranz 1e-12.
# ---------------------------------------------------------------------------
test_that("varianz_from_troughs: alle Felder extern nachgerechnet (synthetische Vektoren)", {
  Tf <- c(120.5, -340.25, 505.0, -80.75, 260.5, -15.0)
  Te <- c(100.0, -300.50, 480.5, -60.25, 240.0, -40.5)
  Ta <- c( 10.5,  -25.00,  40.0,  -5.50,  30.0,   0.0)
  z  <- varianz_from_troughs(Tf, Te, Ta)

  Vf <- var(Tf); Ve <- var(Te); Va <- var(Ta)
  expect_equal(z$var_gesamt, Vf,              tolerance = 1e-12)
  expect_equal(z$var_epi,    Ve,              tolerance = 1e-12)
  expect_equal(z$var_ale,    Va,              tolerance = 1e-12)
  expect_equal(z$var_inter,  Vf - Ve - Va,    tolerance = 1e-12)

  expect_equal(z$sd_gesamt,  sqrt(Vf),         tolerance = 1e-12)
  expect_equal(z$sd_epi,     sqrt(max(Ve, 0)), tolerance = 1e-12)
  expect_equal(z$sd_ale,     sqrt(max(Va, 0)), tolerance = 1e-12)

  expect_equal(z$anteil_epi,   Ve / Vf,             tolerance = 1e-12)
  expect_equal(z$anteil_ale,   Va / Vf,             tolerance = 1e-12)
  expect_equal(z$anteil_inter, (Vf - Ve - Va) / Vf, tolerance = 1e-12)
  expect_equal(z$anteil_epi + z$anteil_ale + z$anteil_inter, 1, tolerance = 1e-12)

  expect_equal(z$S, length(Tf))
})

# ---------------------------------------------------------------------------
# Reine Funktion, Eingabevertrag: ungleiche Laengen werden zurueckgewiesen
# (stopifnot gleiche Laengen).
# ---------------------------------------------------------------------------
test_that("varianz_from_troughs verlangt gleiche Vektorlaengen", {
  expect_error(varianz_from_troughs(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), c(1, 2, 3, 4)))
  expect_error(varianz_from_troughs(c(1, 2, 3, 4, 5), c(1, 2, 3), c(1, 2, 3, 4, 5)))
})

# ---------------------------------------------------------------------------
# Wrapper, EXTERNES Orakel: var/sd/Anteile werden vom Test SELBST aus den
# zurueckgegebenen Roh-Vektoren T_full/T_epi/T_ale gerechnet und mit den
# Feldern verglichen (Toleranz 1e-12) — faengt eine Zerlegung, die andere
# Vektoren nutzt als sie zurueckgibt.
# ---------------------------------------------------------------------------
test_that("Wrapper-Felder == extern aus den Roh-Vektoren gerechnete var/sd/Anteile", {
  out <- var_out()
  Vf  <- var(out$T_full); Ve <- var(out$T_epi); Va <- var(out$T_ale)

  expect_equal(out$var_gesamt, Vf,           tolerance = 1e-12)
  expect_equal(out$var_epi,    Ve,           tolerance = 1e-12)
  expect_equal(out$var_ale,    Va,           tolerance = 1e-12)
  expect_equal(out$var_inter,  Vf - Ve - Va, tolerance = 1e-12)

  expect_equal(out$sd_gesamt, sqrt(Vf),         tolerance = 1e-12)
  expect_equal(out$sd_epi,    sqrt(max(Ve, 0)), tolerance = 1e-12)
  expect_equal(out$sd_ale,    sqrt(max(Va, 0)), tolerance = 1e-12)

  expect_equal(out$anteil_epi,   Ve / Vf,             tolerance = 1e-12)
  expect_equal(out$anteil_ale,   Va / Vf,             tolerance = 1e-12)
  expect_equal(out$anteil_inter, (Vf - Ve - Va) / Vf, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Zerlegungsidentitaet + Anteilssumme, beides EXTERN gerechnet: var_inter ist
# exakt der Interaktionsrest var(T_full) - var(T_epi) - var(T_ale), und die
# drei Anteile summieren sich exakt zu 1 (all.equal, Toleranz 1e-12).
# ---------------------------------------------------------------------------
test_that("Zerlegungsidentitaet und Anteilssumme halten extern (Toleranz 1e-12)", {
  out <- var_out()
  expect_equal(out$var_inter,
               var(out$T_full) - var(out$T_epi) - var(out$T_ale),
               tolerance = 1e-12)
  expect_equal(out$anteil_epi + out$anteil_ale + out$anteil_inter, 1,
               tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Regime-Orakel T_epi: fuer herausgegriffene Pfade s rekonstruiert der Test
# den Tiefpunkt KOMPLETT AUSSERHALB des Wrappers — Annahmenvektor aus
# mdl$assume mit Theta[s, ] ueberschrieben, run_model mit Noise-Matrix aus
# Einsen, min der Endbestand-Zeile — und verlangt exakte Gleichheit mit
# T_epi[s] (deterministisch ohne Rauschen, Toleranz 0).
# ---------------------------------------------------------------------------
test_that("T_epi[s] == extern rekonstruierter Tiefpunkt (assume + Theta[s, ], Noise = Einsen)", {
  m   <- load_model(WB)
  out <- var_out()
  drivers <- colnames(out$Theta)
  N1  <- matrix(1, nrow(m$pos), m$HMAX)

  for (s in c(1L, out$S)) {
    a <- m$assume
    for (d in drivers) a[[d]] <- out$Theta[s, d]
    t_ref <- min(run_model(m, a, N1)$cash["Endbestand", ])
    expect_identical(out$T_epi[[s]], t_ref)
  }
})

# ---------------------------------------------------------------------------
# Regime-Orakel T_ale, Nicht-Trivialitaets-Guard: auf der Fixture MUSS
# var(T_ale) > 0 (Rauschen an, Modes fix) UND var(T_epi) > 0 (epistemische
# Streuung ohne Rauschen) gelten. Schlaegt der Guard fehl, unterscheidet die
# Zerlegung die Regime nicht und der Test ist blind — harter Fail, KEIN skip.
# ---------------------------------------------------------------------------
test_that("Nicht-Trivialitaets-Guard: var(T_epi) > 0 UND var(T_ale) > 0 auf der Fixture", {
  out <- var_out()
  expect_gt(var(out$T_epi), 0)
  expect_gt(var(out$T_ale), 0)
})

# ---------------------------------------------------------------------------
# Determinismus (Seed = Szenario-Schluessel, IMPLEMENTATION_RULES
# §Determinismus): set.seed(seed) direkt vor der ersten epistemischen Ziehung
# — ein zweiter Aufruf mit identischem sc liefert bit-identische T_full und
# T_ale.
# ---------------------------------------------------------------------------
test_that("Determinismus: zweiter Aufruf mit identischem sc reproduziert T_full und T_ale exakt", {
  out1 <- var_out()
  out2 <- phase_varianzzerlegung(.sc)
  expect_identical(out2$T_full, out1$T_full)
  expect_identical(out2$T_ale,  out1$T_ale)
})

# ---------------------------------------------------------------------------
# Struktur: phase/scenario, Vektorlaengen == S, Theta S x k mit den
# unabhaengig rekonstruierten Treibern als Spaltennamen, sd_* >= 0, seed und
# S aus dem Szenario durchgereicht.
# ---------------------------------------------------------------------------
test_that("Struktur: Laengen == S, Theta S x k mit Treiber-dimnames, sd_* >= 0, seed/S durchgereicht", {
  m   <- load_model(WB)
  out <- var_out()

  expect_identical(out$phase,    "varianzzerlegung")
  expect_identical(out$scenario, .sc$name)
  expect_equal(out$S,    .sc$sim$paths)
  expect_equal(out$seed, .sc$sim$seed)

  expect_length(out$T_full, out$S)
  expect_length(out$T_epi,  out$S)
  expect_length(out$T_ale,  out$S)
  expect_true(is.numeric(out$T_full) && is.numeric(out$T_epi) && is.numeric(out$T_ale))

  expect_equal(nrow(out$Theta), out$S)
  expect_identical(colnames(out$Theta), names(.var_spec(m)))

  expect_true(out$sd_gesamt >= 0)
  expect_true(out$sd_epi    >= 0)
  expect_true(out$sd_ale    >= 0)
})

# ---------------------------------------------------------------------------
# S ist Argument mit Default sc$sim$paths (analog der S-Steuerung von
# phase_simulation ueber das Szenario): ein explizites kleines S uebersteuert
# den Szenario-Wert und dimensioniert alle drei Laeufe.
# ---------------------------------------------------------------------------
test_that("S-Argument uebersteuert sc$sim$paths und dimensioniert alle drei Laeufe", {
  out <- phase_varianzzerlegung(.sc, S = 6L)
  expect_equal(out$S, 6L)
  expect_length(out$T_full, 6L)
  expect_length(out$T_epi,  6L)
  expect_length(out$T_ale,  6L)
  expect_equal(nrow(out$Theta), 6L)
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_varianzzerlegung weist ein Nicht-Szenario zurueck", {
  expect_error(phase_varianzzerlegung(list(mdl = NULL)), "liqui_scenario")
})
