# ============================================================================
#  tests/test_phase_sim_deckung.R  —  SPEZIFIKATION (rot zuerst) fuer phase-06-deckung
#
#  Einheit phase-06-deckung (Tiefpunkt & Solvenz-Verdict): der Planner hat die
#  urspruengliche Phase 6 wegen Diff-Budget in drei Einheiten geschnitten —
#  phase-06-fan rechnet den VOLLSTAENDIGEN Ergebnis-Vertrag `out` von
#  phase_simulation(); diese Schwester-Einheit projiziert dieses `out` NUR auf den
#  Deckungs-/Solvenz-Verdict (keine zweite Monte-Carlo, keine Kernel-Aenderung).
#  Die Datei spezifiziert daher genau den Projektions-Vertrag von
#  phase_sim_deckung() (R/phase_sim_deckung.R).
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/phase_sim_deckung.R
#  existiert noch nicht -> source() bricht ab; jeder Test spezifiziert zusaetzlich
#  das Zielverhalten). Der Builder erfuellt sie erst mit der Implementierung.
#
#  Vertrag (aus handoff/phase-06-deckung.json):
#   - phase_sim_deckung(sc) ruft den DAG-gecachten Wrapper phase_simulation(sc)
#     auf und projiziert dessen trough/metrics auf ein Ergebnisobjekt (statt in die
#     Konsole zu drucken):
#       * trough_median / trough_p05 / trough_p95 — Tiefpunkt-Verteilung aus
#         out$trough (median / quantile .05 / .95).
#       * P_breach   = Anteil Pfade mit trough < -kk_limit, Limit aus dem MODELL
#         (sc$mdl$kk_limit). P_solvent = 1 - P_breach.
#       * P_breach_pct / P_solvent_pct = 100 * Anteil.
#       * insO_erfuellt = §§ 18/19 InsO-Verdict: P_solvent > 0.5 (STRIKT, kein
#         hartcodiertes Urteil; Rand bei exakt 0.5 -> FALSE).
#       * kk_limit / seed / S — Szenario-Schluessel + Pfadanzahl.
#   - Reine Projektion (keine zweite MC): phase_sim_deckung loest GENAU eine
#     phase_simulation()-Ziehung aus; ein synchroner Re-Run faellt rot.
#   - Struktur: alle Kennzahlen skalar/numerisch, P in [0,1]; Nicht-Szenario wird
#     zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
#
#  Keine fest verdrahteten Absolutzahlen — die Referenz wird numerisch aus der
#  Mappe/Engine (ueber den unabhaengigen phase_simulation-Lauf und dessen trough/B)
#  nachgerechnet; den Tier-1-Anker prueft der Golden-Master in run_all.R.
#
#  Warum drei Regime statt nur des Fixture-Laufs:
#   (1) Fixture-Lauf (Mappen-kk_limit): P_breach == 0 (kein Pfad reisst die Linie,
#       vgl. test_phase_sim_breach.R) -> P_solvent == 1, insO == TRUE. Prueft die
#       Treue der Projektion, exerziert die InsO-SCHWELLE aber nur trivial.
#   (2) Erzwungenes Breach-Regime (kk_limit = 0, wie test_phase_sim_breach.R):
#       P_breach > 0 (Regime-Waechter), Solvenz-Kennzahlen extern aus dem
#       Simulations-B/trough nachgerechnet -> faengt einen Komplement-Flip
#       (1 + P_breach) und eine falsche Schwellen-Richtung.
#   (3) Zaehlender/kontrollierter Stub von phase_simulation: pinnt die InsO-
#       Schwelle EXAKT am Rand P_solvent == 0.5 (strikt > -> FALSE) plus je ein
#       Fall darueber/darunter, und beweist "GENAU eine Monte-Carlo".
#   Mindestens ein Orakel rekonstruiert die Referenz AUSSERHALB der geprueften
#   Funktion (LESSON: Selbstkonsistenz aus demselben Objekt hat keine
#   Mutationskraft).
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
source(file.path(.root, "R", "phase_sim_deckung.R"))  # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# EIN gemeinsames Szenario + je EIN Lauf, LAZY memoisiert ueber Promises: die
# genestete Simulation ist teuer (jeder Pfad laeuft durch run_model), daher ein
# kleines S wie in den Schwester-Orakeln (test_phase_sim_noise/monate/breach).
# Seed bleibt der Szenario-Schluessel 2026 -> beide Laeufe sind deterministisch
# und rechnen bit-identisch auf demselben Tiefpunkt-Sample.
#   .ref = phase_simulation(sc)   — unabhaengige Referenz (liefert trough/B)
#   .out = phase_sim_deckung(sc)  — die geprueften Projektion
.sc    <- scenario_from_workbook(WB, name = "DeckungSpec",
                                 sim = list(paths = 25L, seed = 2026))
.cache <- new.env(parent = emptyenv())
delayedAssign("ref", phase_simulation(.sc),   assign.env = .cache)
delayedAssign("out", phase_sim_deckung(.sc),  assign.env = .cache)
sim_ref <- function() .cache$ref
sim_out <- function() .cache$out

# ---------------------------------------------------------------------------
# Kontrollierter Stub-Ergebnisvertrag (phase_simulation-Form) fuer die
# Schwellen-Tests: aus einem Tiefpunkt-Vektor `trough` und `kk_limit` werden
# out$trough + ein DAZU KONSISTENTES out$metrics gebaut (wie der Fan-Wrapper es
# rechnen wuerde). So besteht der Stub-Test unabhaengig davon, ob phase_sim_deckung
# P_breach aus out$trough neu rechnet oder aus out$metrics uebernimmt — beide
# Quellen sind hier gleich. Die TEST-Erwartungen rekonstruieren aber IMMER
# unabhaengig aus `trough` (nicht aus dem Stub-metrics), damit die Projektion
# echte Mutationskraft behaelt. KEINE Engine, kein Seed — rein synthetisch.
.mk_out <- function(trough, kk_limit) {
  Pb <- mean(trough < -kk_limit)
  list(
    phase   = "simulation",
    trough  = trough,
    B       = matrix(trough, ncol = 1L, dimnames = list(NULL, "M1")),
    endbestand_path = NA_character_,
    metrics = list(
      S             = length(trough),
      seed          = 2026L,
      H             = 1L,
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95)),
      P_breach      = Pb,
      P_breach_pct  = 100 * Pb,
      P_solvent     = 1 - Pb,
      P_solvent_pct = 100 * (1 - Pb),
      plan_trough   = min(trough)
    )
  )
}

# phase_simulation durch einen ZAEHLENDEN Fake ersetzen (in globalenv, wo die
# gesourcte phase_sim_deckung ihn aufloest), phase_sim_deckung(sc) laufen lassen
# und den Fake wiederherstellen. Gibt Ergebnis + Aufrufzahl zurueck.
.run_deckung_stubbed <- function(sc, out) {
  calls    <- 0L
  fake     <- function(...) { calls <<- calls + 1L; out }
  had_orig <- exists("phase_simulation", envir = globalenv(), inherits = FALSE)
  orig     <- if (had_orig) get("phase_simulation", envir = globalenv()) else NULL
  assign("phase_simulation", fake, envir = globalenv())
  on.exit({
    if (had_orig) assign("phase_simulation", orig, envir = globalenv())
    else if (exists("phase_simulation", envir = globalenv(), inherits = FALSE))
      rm("phase_simulation", envir = globalenv())
  }, add = TRUE)

  # get() statt Direktaufruf: phase_sim_deckung wird per source() (Zeile 63) in
  # globalenv geladen, ist fuer den statischen Linter aber unsichtbar (rot zuerst).
  # Der String-Lookup ist runtime-identisch und spiegelt das phase_simulation-Idiom
  # zwei Zeilen weiter oben.
  res <- get("phase_sim_deckung")(sc)
  list(res = res, calls = calls)
}

# ===========================================================================
#  (1) Fixture-Lauf: Treue der Projektion auf trough/metrics
# ===========================================================================

# ---------------------------------------------------------------------------
# Anker "Tiefpunkt-Verteilung": trough_median/p05/p95 sind exakt median /
# quantile(.05) / quantile(.95) des UNABHAENGIGEN phase_simulation-Laufs
# (out$trough) — kein Etikettentausch, keine eigene Ziehung. Toleranz 1e-12.
# ---------------------------------------------------------------------------
test_that("trough_median/p05/p95 == median/quantile(out$trough) (Treue der Tiefpunkt-Verteilung)", {
  ref <- sim_ref()
  out <- sim_out()

  expect_equal(out$trough_median, unname(median(ref$trough)),        tolerance = 1e-12)
  expect_equal(out$trough_p05,    unname(quantile(ref$trough, .05)), tolerance = 1e-12)
  expect_equal(out$trough_p95,    unname(quantile(ref$trough, .95)), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Anker "P_breach/P_solvent" (Treue): P_breach == Anteil Pfade mit
# trough < -kk_limit, Limit aus dem MODELL (sc$mdl$kk_limit == m$kk_limit),
# unabhaengig aus ref$trough nachgerechnet; P_solvent == 1 - P_breach. Toleranz
# 1e-12. Auf der Fixture ist P_breach == 0 (kein Pfad reisst die Linie) — die
# Schwelle wird nicht-trivial erst in (2)/(3) exerziert.
# ---------------------------------------------------------------------------
test_that("P_breach == mean(trough < -kk_limit) (Limit aus dem Modell) und P_solvent == 1 - P_breach", {
  m   <- load_model(WB)
  ref <- sim_ref()
  out <- sim_out()

  Pb <- mean(ref$trough < -m$kk_limit)
  expect_equal(out$P_breach,  Pb,     tolerance = 1e-12)
  expect_equal(out$P_solvent, 1 - Pb, tolerance = 1e-12)
  expect_equal(out$kk_limit,  m$kk_limit)
})

# ---------------------------------------------------------------------------
# Anker "Selbstkonsistenz des Solvenz-Verdicts" (phasen_anker, Toleranz 1e-12):
# P_solvent == 1 - P_breach, die Prozent-Kennzahlen == 100 * Anteil, und das
# InsO-Urteil folgt EXAKT der Schwelle P_solvent > 0.5 (strikt). Faengt einen
# falschen <= und einen hartcodierten Status.
# ---------------------------------------------------------------------------
test_that("Selbstkonsistenz: P_solvent==1-P_breach, *_pct==100*Anteil, insO==(P_solvent>0.5)", {
  out <- sim_out()
  expect_true(isTRUE(all.equal(out$P_solvent,     1 - out$P_breach,   tolerance = 1e-12)))
  expect_true(isTRUE(all.equal(out$P_breach_pct,  100 * out$P_breach, tolerance = 1e-12)))
  expect_true(isTRUE(all.equal(out$P_solvent_pct, 100 * out$P_solvent, tolerance = 1e-12)))
  expect_identical(out$insO_erfuellt, out$P_solvent > 0.5)
})

# ---------------------------------------------------------------------------
# Szenario-Schluessel + Pfadanzahl werden durchgereicht: seed == 2026, S ==
# metrics$S des Laufs (kein zweites Sample). kk_limit == Modell-Limit (oben).
# ---------------------------------------------------------------------------
test_that("seed/S werden aus dem Simulationslauf durchgereicht (Szenario-Schluessel)", {
  ref <- sim_ref()
  out <- sim_out()
  expect_equal(out$seed, ref$metrics$seed)
  expect_equal(out$seed, 2026)
  expect_equal(out$S,    ref$metrics$S)
})

# ---------------------------------------------------------------------------
# Struktur: alle gemeldeten Kennzahlen sind SKALAR (Laenge 1) und numerisch,
# insO_erfuellt ist ein logischer Skalar, und die Wahrscheinlichkeiten liegen in
# [0,1]. (EUR-/Prozent-Formatierung ueber fmt()/sprintf erst im app-Render.)
# ---------------------------------------------------------------------------
test_that("Struktur: Kennzahlen skalar/numerisch, insO logisch, P in [0,1]", {
  out <- sim_out()

  num_keys <- c("trough_median", "trough_p05", "trough_p95",
                "P_breach", "P_solvent", "P_breach_pct", "P_solvent_pct",
                "kk_limit", "seed", "S")
  for (k in num_keys) {
    expect_true(is.numeric(out[[k]]), info = k)
    expect_length(out[[k]], 1L)
  }

  expect_true(is.logical(out$insO_erfuellt))
  expect_length(out$insO_erfuellt, 1L)

  expect_true(out$P_breach  >= 0 && out$P_breach  <= 1)
  expect_true(out$P_solvent >= 0 && out$P_solvent <= 1)
})

# ===========================================================================
#  (2) Erzwungenes Breach-Regime (kk_limit = 0): nicht-trivialer P_breach,
#      Solvenz-Kennzahlen EXTERN aus dem Simulations-Sample nachgerechnet.
# ===========================================================================

# ---------------------------------------------------------------------------
# Wie test_phase_sim_breach.R erzwingt kk_limit = 0 ein nicht-triviales Regime:
# bei Pfad-Tiefpunkten tief im Minus (Plan-Tiefpunkt ~ -5 Mio) reisst jeder Pfad
# die Null-Linie -> P_breach > 0. Ein Regime-Waechter fordert das hart ein. Alle
# Solvenz-Kennzahlen werden danach ALLEIN aus dem unabhaengigen Simulationslauf
# (trough + tatsaechlich verwendetes Limit) rekonstruiert — der Komplement-Flip
# (1 + P_breach) weicht hier um 2*P_breach ab, eine falsche Schwellen-Richtung
# beim InsO-Urteil ebenfalls sichtbar. Kein Sollwert-Literal, Seed 2026 gepinnt.
# ---------------------------------------------------------------------------
test_that("Breach-Regime (kk_limit=0): P_breach/P_solvent/insO extern nachgerechnet, Komplement-Flip sichtbar", {
  S   <- 25L
  sc  <- scenario_from_workbook(WB, name = "DeckungBreach",
                                sim = list(paths = S, seed = 2026))
  # kk_limit direkt am Objekt (die einzige Naht; scenario_from_workbook kennt kein
  # kk_limit-Argument — vgl. Kopf von test_phase_sim_breach.R). Denselben mdl
  # liest sowohl der interne phase_simulation als auch phase_sim_deckung.
  sc$mdl$kk_limit <- 0

  out <- phase_sim_deckung(sc)

  # Unabhaengige Referenz: EIGENER, deterministischer phase_simulation(sc)-Lauf
  # (gleicher Seed 2026 -> bit-identisches trough-Sample wie der interne Lauf).
  ref          <- phase_simulation(sc)
  kk_used      <- sc$mdl$kk_limit
  trough_ext   <- apply(ref$B, 1, min)
  P_breach_ext <- mean(trough_ext < -kk_used)

  # Regime-Waechter: nie wieder nur der triviale Punkt P_breach = 0 (sonst misst
  # der Test die Schwelle/den Flip nicht).
  expect_true(P_breach_ext > 0,
              info = "Regime-Waechter verletzt: kein Pfad reisst die KK-Linie (kk_limit-Setzung pruefen).")

  expect_equal(out$P_breach,      P_breach_ext,             tolerance = 1e-12)
  expect_equal(out$P_solvent,     1 - P_breach_ext,         tolerance = 1e-12)  # Flip 1+P weicht um 2*P ab
  expect_equal(out$P_breach_pct,  100 * P_breach_ext,       tolerance = 1e-12)
  expect_equal(out$P_solvent_pct, 100 * (1 - P_breach_ext), tolerance = 1e-12)
  expect_equal(isTRUE(out$insO_erfuellt), (1 - P_breach_ext) > 0.5)  # name-robust
})

# ===========================================================================
#  (3) Kontrollierter Stub: InsO-Schwelle EXAKT am Rand + "genau eine MC"
# ===========================================================================

# ---------------------------------------------------------------------------
# InsO-Rand (§§ 18/19 InsO, STRIKT >): am exakten Punkt P_solvent == 0.5 muss das
# Urteil FALSE sein (nicht zahlungsfaehig — 50 % reicht nicht). trough so gewaehlt,
# dass mean(trough < -kk_limit) == 0.5 EXAKT. Ein >= statt > kippt hier auf TRUE
# -> rot; ein hartcodiertes TRUE ebenfalls.
# ---------------------------------------------------------------------------
test_that("InsO-Rand: P_solvent == 0.5 exakt -> insO_erfuellt == FALSE (strikt >, kein >=)", {
  kk     <- 50
  trough <- c(-100, -100, 100, 100)          # 2/4 reissen die Linie -> P_breach 0.5
  sc     <- structure(list(name = "DeckungRand", mdl = list(kk_limit = kk)),
                      class = "liqui_scenario")

  got <- .run_deckung_stubbed(sc, .mk_out(trough, kk))
  res <- got$res

  Pb <- mean(trough < -kk)
  expect_equal(Pb, 0.5)                       # Regime-Waechter: wirklich der Rand
  expect_equal(res$P_breach,  0.5, tolerance = 1e-12)
  expect_equal(res$P_solvent, 0.5, tolerance = 1e-12)
  expect_false(res$insO_erfuellt)                         # strikt > 0.5, kein >=
  expect_identical(res$insO_erfuellt, res$P_solvent > 0.5)
})

# ---------------------------------------------------------------------------
# InsO knapp UEBER dem Rand: P_solvent == 0.6 > 0.5 -> TRUE. Faengt ein
# hartcodiertes FALSE und einen invertierten Vergleich (<).
# ---------------------------------------------------------------------------
test_that("InsO ueber dem Rand: P_solvent == 0.6 -> insO_erfuellt == TRUE", {
  kk     <- 50
  trough <- c(rep(-100, 4L), rep(100, 6L))   # 4/10 reissen -> P_breach 0.4, P_solvent 0.6
  sc     <- structure(list(name = "DeckungUeber", mdl = list(kk_limit = kk)),
                      class = "liqui_scenario")

  res <- .run_deckung_stubbed(sc, .mk_out(trough, kk))$res

  expect_equal(res$P_breach,  0.4, tolerance = 1e-12)
  expect_equal(res$P_solvent, 0.6, tolerance = 1e-12)
  expect_true(res$insO_erfuellt)
  expect_identical(res$insO_erfuellt, res$P_solvent > 0.5)
})

# ---------------------------------------------------------------------------
# InsO knapp UNTER dem Rand: P_solvent == 0.4 < 0.5 -> FALSE. Faengt einen
# invertierten Vergleich (<) und einen hartcodierten Status. Zusammen mit den
# beiden Faellen oben ist die strikte Schwelle beidseitig gepinnt.
# ---------------------------------------------------------------------------
test_that("InsO unter dem Rand: P_solvent == 0.4 -> insO_erfuellt == FALSE", {
  kk     <- 50
  trough <- c(rep(-100, 6L), rep(100, 4L))   # 6/10 reissen -> P_breach 0.6, P_solvent 0.4
  sc     <- structure(list(name = "DeckungUnter", mdl = list(kk_limit = kk)),
                      class = "liqui_scenario")

  res <- .run_deckung_stubbed(sc, .mk_out(trough, kk))$res

  expect_equal(res$P_breach,  0.6, tolerance = 1e-12)
  expect_equal(res$P_solvent, 0.4, tolerance = 1e-12)
  expect_false(res$insO_erfuellt)
  expect_identical(res$insO_erfuellt, res$P_solvent > 0.5)
})

# ---------------------------------------------------------------------------
# Reine Projektion (keine zweite MC): phase_sim_deckung loest phase_simulation
# GENAU EINMAL aus. Ein zweiter synchroner Re-Run liesse den Zaehler auf 2 laufen
# (schwere Monte-Carlo laeuft nie doppelt — CLAUDE.md). Bit-identische
# deterministische Outputs allein wuerden einen Re-Run NICHT sichtbar machen,
# daher der zaehlende Fake (LESSON: "genau EIN teurer Aufruf" braucht ein
# zaehlendes Fake-Orakel).
# ---------------------------------------------------------------------------
test_that("phase_sim_deckung ruft phase_simulation GENAU einmal (kein synchroner Re-Run)", {
  kk     <- 50
  trough <- c(-100, -100, 100, 100)
  sc     <- structure(list(name = "DeckungCount", mdl = list(kk_limit = kk)),
                      class = "liqui_scenario")

  got <- .run_deckung_stubbed(sc, .mk_out(trough, kk))
  expect_equal(got$calls, 1L)                # nicht 0 (Projektion braucht out), nicht >=2 (keine zweite MC)
  expect_equal(got$res$P_solvent, 0.5, tolerance = 1e-12)  # Projektion aus dem Fake-out gebaut
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_sim_deckung weist ein Nicht-Szenario zurueck", {
  expect_error(phase_sim_deckung(list(mdl = NULL)), "liqui_scenario")
})
