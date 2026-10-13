# ============================================================================
#  tests/test_compare_mutgate.R  —  Mutgate-Nachschub (rot_zuerst: false)
#  fuer phase-08-szenarienvergleich
#
#  Zusatz-Orakel zur Spezifikation tests/test_compare.R: der Mutgate-Lauf
#  endete 3/5, zwei Ueberlebende, zwei Loecher — diese Datei schliesst beide.
#  Sie ist gegen den AKTUELLEN (korrekten) Stand gruen und toetet die
#  Ueberlebenden. Keine Anker-Literale, keine Tier-1-Werte.
#
#  Block 1 — Grenzwert-Orakel (killt M1, Grenzwert-Klasse):
#    M1 rechnet P_breach in vergleich_from_outs mit <= statt strikt < —
#    ein Pfad EXAKT auf -kk_limit zaehlt dann faelschlich als Breach. Die
#    Spec ist dagegen blind, weil keiner ihrer Stubs und kein Monte-Carlo-
#    Pfad exakt auf -kk_limit liegt (stetige Simulationswerte treffen die
#    Grenze praktisch nie): < und <= sind dort ununterscheidbar. Dieses
#    Orakel legt GENAU EINEN Stub-Pfad EXAKT auf -kk_limit plus Pfade klar
#    darunter/darueber und verlangt strikt EXTERN gerechnet, dass NUR die
#    Pfade strikt unter -kk_limit zaehlen — ein <= zaehlt den Grenzpfad mit
#    und faellt zwingend rot. Regime-Waechter als HARTE Fails (kein skip)
#    sichern, dass der Grenzpfad wirklich auf der Grenze liegt.
#
#  Block 2 — Bau-Orakel (killt die M5-Klasse am neuen Ort):
#    M5 baute im mirai-Ausdruck von start_vergleich (app/app.R) den Override
#    vertauscht (ov <- list(sp$treiber); names(ov) <- sp$wert) — unsichtbar,
#    weil der Ausdruck nur im echten Worker-Lauf evaluiert wird: das
#    Bindungs-Orakel (test_compare_ui.R) stubbt vergleich_rv$out HINTER dem
#    Ausdruck, die Spec (test_compare.R) testet compare_scenarios DAVOR —
#    der Szenariobau im Worker hatte keinen Testpfad. Geschlossen durch
#    Herausziehen in die benannte Funktion scenarios_from_specs
#    (R/compare_scenarios.R), die der mirai-Ausdruck jetzt aufruft und die
#    hier DIREKT auf der Fixture getestet wird: Basis + Spec-Szenario in
#    Reihenfolge, Override greift EXAKT am benannten Treiber (assume UND
#    mdl$assume), alle uebrigen Annahmen und die sim-Parameter bleiben wie
#    Basis. KEIN compare_scenarios-Aufruf, kein Monte-Carlo — nur
#    load_model via scenario_from_workbook.
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
source(file.path(.root, "R", "compare_scenarios.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Block 1 — Grenzwert-Orakel: synthetischer Stub im phase_simulation-
# Ergebnisvertrag (Muster .mk_sim_out aus test_compare.R, lokal nachgebaut) —
# KEINE Engine, kein Seed-Lauf, KEINE Tier-1-Werte. metrics werden konsistent
# zum trough gebaut, damit der Stub beide denkbaren Quellen gleich haelt.
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

# Vier Pfade, kk_limit 500: EINER klar darunter (-700), EINER EXAKT auf der
# Grenze (-500 == -kk_limit), zwei klar darueber (-100, 300). Strikt < zaehlt
# nur -700 -> P_breach exakt 1/4; ein <= zaehlt -500 mit -> 2/4 und faellt rot.
.tg  <- c(-700, -500, -100, 300)
.kkg <- 500
.og  <- .mk_sim_out(.tg, .kkg, seed = 777L, plan_trough = -1234)

test_that("Grenzwert: Pfad EXAKT auf -kk_limit zaehlt NICHT als Breach (strikt <, ein <= faellt rot)", {
  # Regime-Waechter als HARTE Fails (kein skip): liegt kein Pfad exakt auf
  # der Grenze bzw. keiner strikt darunter, ist das Orakel blind und der
  # Test muss rot sein, nicht schweigen.
  expect_true(any(.tg == -.kkg),
              label = "Regime-Waechter: mindestens ein Stub-Pfad liegt EXAKT auf -kk_limit (sonst waere < vs. <= ununterscheidbar und das Orakel blind fuer M1)")
  expect_true(any(.tg < -.kkg),
              label = "Regime-Waechter: mindestens ein Stub-Pfad liegt strikt unter -kk_limit (sonst degenerierte P_breach zu 0 und der Anteils-Check waere trivial)")

  tb <- vergleich_from_outs(list(.og), .kkg, "Grenz")

  # Erwartung strikt EXTERN (Literale, nicht aus der Funktion abgelesen):
  # NUR der eine Pfad strikt unter -500 zaehlt -> exakt 1 von 4. Alle Werte
  # sind exakt binaer darstellbar, daher identical statt Toleranz.
  expect_identical(tb$P_breach,      0.25)
  expect_identical(tb$P_solvent,     0.75)
  expect_identical(tb$P_breach_pct,  25)
  expect_identical(tb$P_solvent_pct, 75)
  # Ableitung insO: 0.75 > 0.5 -> TRUE; unter <= kippte P_solvent auf exakt
  # 0.5 und insO_erfuellt (STRIKT >) auf FALSE — zweites rotes Signal.
  expect_identical(tb$insO_erfuellt, TRUE)
})

# ---------------------------------------------------------------------------
# Block 2 — Bau-Orakel fuer scenarios_from_specs auf der Fixture: nur
# Szenariobau (load_model), KEIN compare_scenarios, kein Monte-Carlo.
# ---------------------------------------------------------------------------
test_that("scenarios_from_specs: Basis + Spec-Szenario in Reihenfolge, Override greift EXAKT am benannten Treiber (vertauschter Bau faellt rot)", {
  scs <- scenarios_from_specs(WB, specs = list(
    list(name = "MutStress", treiber = "menge", wert = -0.31)))

  expect_length(scs, 2L)
  expect_true(all(vapply(scs, inherits, logical(1L), what = "liqui_scenario")))
  expect_identical(vapply(scs, function(sc) sc$name, character(1L)),
                   c("Basis", "MutStress"))

  # Guard als HARTER Fail (kein skip): entspraeche der Basis-Default schon
  # dem Override, waere der Exakt-Check darunter blind — ein vertauschter
  # Bau (Override-Name "-0.31", Wert "menge" -> als unbekannte Annahme
  # ignoriert, menge bleibt Default) saehe dann korrekt aus.
  expect_false(isTRUE(all.equal(scs[[1]]$assume$menge, -0.31)),
               label = "Basis-Default assume$menge muss sich vom Override -0.31 unterscheiden (sonst waere das Bau-Orakel blind fuer die M5-Klasse)")

  # Der Override greift EXAKT — in BEIDEN Quellen (assume ist der Cache-
  # Schluessel, mdl$assume rechnet): ein vertauschter Bau laesst beide auf
  # dem Basis-Default stehen und faellt hier rot.
  expect_identical(scs[[2]]$assume$menge,     -0.31)
  expect_identical(scs[[2]]$mdl$assume$menge, -0.31)

  # Alle UEBRIGEN Annahmen von Szenario 2 identisch zur Basis — der Override
  # trifft nur den benannten Treiber, nichts sonst.
  expect_identical(names(scs[[2]]$assume), names(scs[[1]]$assume))
  for (k in setdiff(names(scs[[1]]$assume), "menge"))
    expect_identical(scs[[2]]$assume[[k]], scs[[1]]$assume[[k]])

  # sim bleibt jeweils Default: beide Szenarien rechnen mit denselben
  # Sim-Parametern (der Seed ist der Szenario-Schluessel).
  expect_identical(scs[[2]]$sim, scs[[1]]$sim)
})

# ---------------------------------------------------------------------------
# Eingabevertrag: kaputte specs werden VOR dem ersten load_model
# zurueckgewiesen (stopifnot) — fehlendes Feld, nicht-numerisches oder
# nicht-endliches wert, Element ohne list-Struktur.
# ---------------------------------------------------------------------------
test_that("scenarios_from_specs weist kaputte specs zurueck (fehlendes Feld, nicht-numerisches/nicht-endliches wert)", {
  expect_error(scenarios_from_specs(WB, specs = list(
    list(name = "X", wert = -0.1))))                            # treiber fehlt
  expect_error(scenarios_from_specs(WB, specs = list(
    list(name = "X", treiber = "menge"))))                      # wert fehlt
  expect_error(scenarios_from_specs(WB, specs = list(
    list(name = "X", treiber = "menge", wert = "-0.1"))))       # wert nicht numerisch
  expect_error(scenarios_from_specs(WB, specs = list(
    list(name = "X", treiber = "menge", wert = Inf))))          # wert nicht endlich
  expect_error(scenarios_from_specs(WB, specs = list("kein-spec-Element")))
  expect_error(scenarios_from_specs(WB, specs = "keine-Liste"))
})
