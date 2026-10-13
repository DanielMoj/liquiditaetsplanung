# ============================================================================
#  tests/test_compare.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-08-szenarienvergleich
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN
#  (R/compare_scenarios.R existiert noch nicht -> source() bricht ab; jeder
#  Test spezifiziert zusaetzlich das Zielverhalten). Der Builder erfuellt sie
#  erst mit der Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche.
#
#  Vertrag (handoff/phase-08-szenarienvergleich.json; KEIN Legacy-Vorbild —
#  fachliche Grundlage ist die Szenario-Abstraktion in R/scenario.R):
#   - vergleich_from_outs(outs, kk_limits, namen) — REINE Funktion (Muster
#     deckung_from_out): outs = Liste von phase_simulation-Ergebnisobjekten,
#     kk_limits = numerischer Vektor, namen = character; stopifnot gleiche
#     Laengen >= 1 und eindeutige namen. Rueckgabe: data.frame mit einer
#     Zeile je Element in EINGABEREIHENFOLGE und den Spalten szenario,
#     plan_trough (aus out$metrics$plan_trough), trough_median/trough_p05/
#     trough_p95 (median bzw. unname(quantile(out$trough, .05/.95))),
#     P_breach (mean(out$trough < -kk_limit), strikt <), P_solvent
#     (1 - P_breach), P_breach_pct/P_solvent_pct (100 x Anteil),
#     insO_erfuellt (P_solvent > 0.5, STRIKT >, exakt 0.5 -> FALSE),
#     kk_limit, S, seed (aus out$metrics). Keine Formatierung, keine
#     Simulation.
#   - compare_scenarios(scs) — Wrapper in derselben Datei: scs = Liste von
#     liqui_scenario, stopifnot Laenge >= 1, jedes Element liqui_scenario,
#     Namen (sapply sc$name) eindeutig. Je Szenario GENAU EIN Aufruf
#     phase_simulation(sc) — keine zweite Monte-Carlo, keine Kernel-
#     Aenderung, kein eigenes Rauschen; kk_limit je Szenario aus
#     sc$mdl$kk_limit. Rueckgabe: list(phase = "szenarienvergleich",
#     n = Laenge, tabelle = vergleich_from_outs(...)).
#
#  Orakel-Politik: EXTERNE Orakel statt Selbstkonsistenz — der Test rechnet
#  median/quantile/mean/Prozent/insO je Zeile SELBST (an synthetischen Stubs
#  bzw. aus einem EIGENEN, deterministischen phase_simulation-Lauf) und
#  benutzt dafuer weder vergleich_from_outs noch deckung_from_out. Keine
#  Anker-Literale, keine Tier-1-Werte; den Tier-1-Anker prueft der
#  Golden-Master in run_all.R. Kleines S (paths = 12) und EIN memoisierter
#  compare_scenarios-Lauf halten die Suite schnell.
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
source(file.path(.root, "R", "compare_scenarios.R"))  # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Exakter Spaltenvertrag der Vergleichstabelle (Namen UND Reihenfolge).
.spalten <- c("szenario", "plan_trough", "trough_median", "trough_p05",
              "trough_p95", "P_breach", "P_solvent", "P_breach_pct",
              "P_solvent_pct", "insO_erfuellt", "kk_limit", "S", "seed")

# ---------------------------------------------------------------------------
# Synthetischer Stub im phase_simulation-Ergebnisvertrag (Muster .mk_sim_out
# der Schwester-Orakel): aus trough + kk_limit werden out$trough und dazu
# KONSISTENTE metrics gebaut, damit der Stub beide denkbaren Quellen gleich
# haelt; plan_trough wird BEWUSST unabhaengig gesetzt (nicht min(trough)),
# damit eine Implementation, die plan_trough selbst rechnet statt aus
# out$metrics durchzureichen, rot faellt. KEINE Engine, kein Seed-Lauf,
# KEINE Tier-1-Werte.
# ---------------------------------------------------------------------------
.mk_sim_out <- function(trough, kk_limit, seed, plan_trough) {
  Pb <- mean(trough < -kk_limit)
  list(
    phase           = "simulation",
    trough          = trough,
    B               = matrix(trough, ncol = 1L, dimnames = list(NULL, "M1")),
    plot_path       = NA_character_,
    endbestand_path = NA_character_,
    metrics = list(
      S             = length(trough),
      seed          = seed,
      H             = 1L,
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95)),
      P_breach      = Pb,
      P_breach_pct  = 100 * Pb,
      P_solvent     = 1 - Pb,
      P_solvent_pct = 100 * (1 - Pb),
      plan_trough   = plan_trough
    )
  )
}

# Drei Stubs mit PAARWEISE UNVERWECHSELBAREN trough-Vektoren, kk_limits und
# namen (Mediane -400 / -675 / -50, P_breach 0.4 / 2/3 / 0.5, kk 500/600/200,
# S 5/6/4, Seeds und plan_trough alle verschieden):
#   Stub 1: P_solvent 0.6  -> klar UEBER der InsO-Schwelle (TRUE).
#   Stub 2: P_solvent 1/3  -> klar UNTER der Schwelle (FALSE).
#   Stub 3: exakt haelftiger Breach-Anteil (2 von 4, strikt <) ->
#           P_solvent EXAKT 0.5 -> insO_erfuellt FALSE (strikt >, kein >=).
.t1 <- c(-800, -600, -400, 300, 500);        .kk1 <- 500
.t2 <- c(-1000, -900, -700, -650, 200, 400); .kk2 <- 600
.t3 <- c(-300, -250, 150, 250);              .kk3 <- 200
.namen <- c("Alpha", "Beta", "Gamma")
.seeds <- c(111L, 222L, 333L)
.plans <- c(-1250, -2750, -975)
.o1 <- .mk_sim_out(.t1, .kk1, .seeds[1], .plans[1])
.o2 <- .mk_sim_out(.t2, .kk2, .seeds[2], .plans[2])
.o3 <- .mk_sim_out(.t3, .kk3, .seeds[3], .plans[3])

# ---------------------------------------------------------------------------
# Wrapper-Naht auf der Fixture: zwei kleine Szenarien (Basis + Stress mit
# deutlich negativer Mengen-Annahme und EIGENEM Namen), EIN memoisierter
# compare_scenarios-Lauf (delayedAssign-Muster test_phase_var.R). Die beiden
# externen Referenzen sind EIGENE phase_simulation-Laeufe des Tests —
# deterministisch (gleicher Seed 2026 -> bit-identisches trough-Sample wie
# die internen Laeufe des Wrappers), ebenfalls memoisiert.
# ---------------------------------------------------------------------------
.sc1 <- scenario_from_workbook(WB, name = "VglBasis",
                               sim = list(paths = 12L, seed = 2026))
.sc2 <- scenario_from_workbook(WB, name = "VglStress",
                               assume_overrides = list(menge = -0.30),
                               sim = list(paths = 12L, seed = 2026))
.cache <- new.env(parent = emptyenv())
delayedAssign("cmp",  compare_scenarios(list(.sc1, .sc2)), assign.env = .cache)
delayedAssign("ref1", phase_simulation(.sc1),              assign.env = .cache)
delayedAssign("ref2", phase_simulation(.sc2),              assign.env = .cache)
cmp_out <- function() .cache$cmp
sim_ref <- function(i) if (i == 1L) .cache$ref1 else .cache$ref2

# ===========================================================================
#  (1) Reine Funktion an synthetischen Stubs: EXTERNES Orakel je Spalte
# ===========================================================================

# ---------------------------------------------------------------------------
# Jede Spalte wird vom Test SELBST aus trough/kk/metrics des Stubs gerechnet
# (median/quantile/mean/Prozent/insO) und mit Zeile i verglichen (all.equal,
# Toleranz 1e-12) — Zeile i gegen Stub i. Der anschliessende Kreuz-Check
# verlangt, dass Zeile 1 NICHT die extern gerechneten Werte von Stub 2
# traegt: killt Zeilentausch und Off-by-one in der Zuordnung.
# ---------------------------------------------------------------------------
test_that("vergleich_from_outs: jede Spalte extern nachgerechnet, Zeile i == Stub i, Kreuz-Check gegen Zeilentausch", {
  tb <- vergleich_from_outs(list(.o1, .o2, .o3), c(.kk1, .kk2, .kk3), .namen)

  expect_true(is.data.frame(tb))
  expect_equal(nrow(tb), 3L)
  expect_identical(names(tb), .spalten)

  troughs <- list(.t1, .t2, .t3)
  kks     <- c(.kk1, .kk2, .kk3)
  for (i in 1:3) {
    tr <- troughs[[i]]; kk <- kks[i]
    Pb <- mean(tr < -kk)                       # strikt <, extern gerechnet

    expect_identical(tb$szenario[i], .namen[i])
    expect_equal(tb$plan_trough[i],   .plans[i],                    tolerance = 1e-12)
    expect_equal(tb$trough_median[i], unname(median(tr)),           tolerance = 1e-12)
    expect_equal(tb$trough_p05[i],    unname(quantile(tr, .05)),    tolerance = 1e-12)
    expect_equal(tb$trough_p95[i],    unname(quantile(tr, .95)),    tolerance = 1e-12)
    expect_equal(tb$P_breach[i],      Pb,                           tolerance = 1e-12)
    expect_equal(tb$P_solvent[i],     1 - Pb,                       tolerance = 1e-12)
    expect_equal(tb$P_breach_pct[i],  100 * Pb,                     tolerance = 1e-12)
    expect_equal(tb$P_solvent_pct[i], 100 * (1 - Pb),               tolerance = 1e-12)
    expect_identical(tb$insO_erfuellt[i], (1 - Pb) > 0.5)
    expect_equal(tb$kk_limit[i], kk,                                tolerance = 1e-12)
    expect_equal(tb$S[i],    length(tr))
    expect_equal(tb$seed[i], .seeds[i])
  }

  # Kreuz-Check: Zeile 1 traegt NICHT die extern gerechneten Werte von Stub 2
  # (Zeilentausch / Off-by-one faellt hier zwingend rot).
  expect_false(identical(tb$szenario[1], .namen[2]))
  expect_false(isTRUE(all.equal(tb$trough_median[1], unname(median(.t2)), tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(tb$P_breach[1],      mean(.t2 < -.kk2),   tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(tb$plan_trough[1],   .plans[2],           tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(tb$kk_limit[1],      .kk2,                tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(tb$seed[1],          .seeds[2],           tolerance = 1e-12)))
})

# ---------------------------------------------------------------------------
# InsO-Grenzwert am Stub (§§ 18/19 InsO, STRIKT >): Stub 3 hat exakt
# haelftigen Breach-Anteil -> P_solvent EXAKT 0.5 -> insO_erfuellt identical
# FALSE (ein >= kippt hier auf TRUE -> rot). Stub 1 liegt klar darueber
# (TRUE, faengt ein hartcodiertes FALSE / invertiertes <), Stub 2 klar
# darunter (FALSE) — die Schwelle ist beidseitig plus am Rand gepinnt.
# ---------------------------------------------------------------------------
test_that("insO-Grenzwert: P_solvent exakt 0.5 -> FALSE (strikt >), klar darueber TRUE, klar darunter FALSE", {
  tb <- vergleich_from_outs(list(.o1, .o2, .o3), c(.kk1, .kk2, .kk3), .namen)

  expect_identical(tb$P_solvent[3], 0.5)       # Regime-Waechter: wirklich der Rand
  expect_identical(tb$insO_erfuellt[3], FALSE) # strikt > 0.5, kein >=

  expect_gt(tb$P_solvent[1], 0.5)
  expect_identical(tb$insO_erfuellt[1], TRUE)

  expect_lt(tb$P_solvent[2], 0.5)
  expect_identical(tb$insO_erfuellt[2], FALSE)
})

# ---------------------------------------------------------------------------
# Eingabevertraege der reinen Funktion: ungleiche Laengen (kk_limits bzw.
# namen zu kurz), doppelte namen und leere Listen werden zurueckgewiesen
# (stopifnot gleiche Laengen >= 1 und eindeutige namen).
# ---------------------------------------------------------------------------
test_that("vergleich_from_outs weist ungleiche Laengen, doppelte namen und leere Listen zurueck", {
  expect_error(vergleich_from_outs(list(.o1, .o2), c(.kk1),       c("A", "B")))
  expect_error(vergleich_from_outs(list(.o1, .o2), c(.kk1, .kk2), c("A")))
  expect_error(vergleich_from_outs(list(.o1, .o2), c(.kk1, .kk2), c("A", "A")))
  expect_error(vergleich_from_outs(list(), numeric(0), character(0)))
})

# ===========================================================================
#  (2) Wrapper-Naht auf der Fixture: jede Zeile EXTERN gerechnet
# ===========================================================================

# ---------------------------------------------------------------------------
# Erwartung je Zeile EXTERN: der Test ruft phase_simulation(sc_i) SELBST auf
# (deterministisch, gleicher Seed 2026) und rechnet median/quantile/mean/insO
# direkt aus dessen trough/metrics — OHNE vergleich_from_outs und OHNE
# deckung_from_out. Vergleich jeder Spalte inkl. plan_trough, S, seed,
# kk_limit (aus sc$mdl$kk_limit). Der Nicht-Trivialitaets-Guard ist ein
# HARTER Fail: unterscheiden sich die trough_median der beiden Zeilen nicht
# (Override wirkungslos), waere der Zeilenzuordnungs-Check blind.
# ---------------------------------------------------------------------------
test_that("compare_scenarios: jede Zeile == extern aus einem EIGENEN phase_simulation-Lauf gerechnet", {
  out <- cmp_out()
  tb  <- out$tabelle
  scs <- list(.sc1, .sc2)

  # Nicht-Trivialitaets-Guard (harter Fail, KEIN skip): die beiden Zeilen
  # muessen sich im trough_median unterscheiden, sonst ist der Test blind.
  expect_false(isTRUE(all.equal(unname(median(sim_ref(1L)$trough)),
                                unname(median(sim_ref(2L)$trough)),
                                tolerance = 1e-12)),
               info = "Nicht-Trivialitaets-Guard verletzt: beide Szenarien liefern denselben trough_median (assume_overrides pruefen) — der Zeilenzuordnungs-Check waere blind.")

  for (i in 1:2) {
    ref <- sim_ref(i)
    kk  <- scs[[i]]$mdl$kk_limit
    Pb  <- mean(ref$trough < -kk)              # strikt <, extern gerechnet

    expect_identical(tb$szenario[i], scs[[i]]$name)
    expect_equal(tb$plan_trough[i],   ref$metrics$plan_trough,        tolerance = 1e-12)
    expect_equal(tb$trough_median[i], unname(median(ref$trough)),     tolerance = 1e-12)
    expect_equal(tb$trough_p05[i],    unname(quantile(ref$trough, .05)), tolerance = 1e-12)
    expect_equal(tb$trough_p95[i],    unname(quantile(ref$trough, .95)), tolerance = 1e-12)
    expect_equal(tb$P_breach[i],      Pb,                             tolerance = 1e-12)
    expect_equal(tb$P_solvent[i],     1 - Pb,                         tolerance = 1e-12)
    expect_equal(tb$P_breach_pct[i],  100 * Pb,                       tolerance = 1e-12)
    expect_equal(tb$P_solvent_pct[i], 100 * (1 - Pb),                 tolerance = 1e-12)
    expect_identical(tb$insO_erfuellt[i], (1 - Pb) > 0.5)
    expect_equal(tb$kk_limit[i], kk,                                  tolerance = 1e-12)
    expect_equal(tb$S[i],    ref$metrics$S)
    expect_equal(tb$seed[i], ref$metrics$seed)
  }
})

# ---------------------------------------------------------------------------
# Reihenfolge- und Struktur-Vertrag: tabelle$szenario ist identical
# c(name1, name2) in EINGABEREIHENFOLGE, n == 2, Spaltennamen exakt (Namen
# UND Reihenfolge), P-Spalten in [0,1], insO_erfuellt logical.
# ---------------------------------------------------------------------------
test_that("Reihenfolge/Struktur: szenario in Eingabereihenfolge, n, exakte Spalten, P in [0,1], insO logical", {
  out <- cmp_out()
  tb  <- out$tabelle

  expect_identical(out$phase, "szenarienvergleich")
  expect_equal(out$n, 2L)
  expect_true(is.data.frame(tb))
  expect_equal(nrow(tb), 2L)
  expect_identical(tb$szenario, c(.sc1$name, .sc2$name))
  expect_identical(names(tb), .spalten)

  expect_true(all(tb$P_breach  >= 0 & tb$P_breach  <= 1))
  expect_true(all(tb$P_solvent >= 0 & tb$P_solvent <= 1))
  expect_true(is.logical(tb$insO_erfuellt))
})

# ---------------------------------------------------------------------------
# Determinismus (Seed = Szenario-Schluessel): ein zweiter compare_scenarios-
# Aufruf mit denselben Szenarien liefert eine identical(tabelle) — die
# internen phase_simulation-Laeufe sind ueber set.seed(seed) reproduzierbar.
# ---------------------------------------------------------------------------
test_that("Determinismus: zweiter compare_scenarios-Aufruf liefert identical(tabelle)", {
  out2 <- compare_scenarios(list(.sc1, .sc2))
  expect_identical(out2$tabelle, cmp_out()$tabelle)
})

# ---------------------------------------------------------------------------
# Fehlerfaelle des Wrappers: leere Liste, Element ohne liqui_scenario-Klasse,
# zwei Szenarien mit gleichem Namen -> expect_error (stopifnot VOR jeder
# Simulation, daher billig).
# ---------------------------------------------------------------------------
test_that("compare_scenarios weist leere Listen, Nicht-Szenarien und doppelte Namen zurueck", {
  expect_error(compare_scenarios(list()))
  expect_error(compare_scenarios(list(list(mdl = NULL))))
  expect_error(compare_scenarios(list(.sc1, .sc1)))   # gleicher Name "VglBasis"
})
