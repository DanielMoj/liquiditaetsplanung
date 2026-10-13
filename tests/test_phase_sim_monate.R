# ============================================================================
#  tests/test_phase_sim_monate.R  —  SPEZIFIKATION (rot zuerst) fuer phase-06-monate
#
#  Einheit phase-06-monate (Endbestand je Monat): der Planner hat die
#  urspruengliche Phase 6 wegen Diff-Budget in drei Einheiten geschnitten —
#  phase-06-fan rechnet den VOLLSTAENDIGEN Ergebnis-Vertrag `out` von
#  phase_simulation(); diese Schwester-Einheit projiziert dieses `out` NUR auf
#  eine Endbestand-Tabelle je Monat (keine zweite Monte-Carlo, keine Kernel-
#  Aenderung). Die Datei spezifiziert daher genau den Projektions-Vertrag von
#  phase_sim_monate() (R/phase_sim_monate.R).
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/phase_sim_monate.R
#  existiert noch nicht -> source() bricht ab; jeder Test spezifiziert zusaetzlich
#  das Zielverhalten). Der Builder erfuellt sie erst mit der Implementierung.
#
#  Vertrag (aus handoff/phase-06-monate.json):
#   - phase_sim_monate(sc) ruft den Wrapper phase_simulation(sc) auf und gibt ein
#     Ergebnisobjekt zurueck statt in die Konsole zu drucken:
#       * table  : data.frame mit H Zeilen (M1..MH) und den numerischen Spalten
#                  Monat / P05 / P50 / P95 (EUR, numerisch) sowie P_unter (Anteil
#                  in [0,1]). Die EUR-/Prozent-Formatierung passiert erst im
#                  app-Render (fmt()/sprintf) — das Ergebnisobjekt bleibt numerisch.
#       * fan    : 5xH, aus phase_simulation durchgereicht.
#       * P_under: Vektor je Monat, aus phase_simulation durchgereicht.
#       * H      : metrics$H.
#   - Treue der Projektion (faengt vertauschte Etiketten / falsche Quantil-
#     Zuordnung): table$P05/P50/P95 == fan-Zeilen 5%/50%/95% UND == unabhaengig
#     aus B nachgerechnete Monatsquantile (apply(B, 2, quantile, c(.05,.5,.95)));
#     table$P_unter == colMeans(B < mindestreserve) mit der Schwelle aus dem
#     Modell. Quantil-Monotonie je Zeile P05 <= P50 <= P95.
#   - Reine Projektion (keine zweite MC): fan/P_under sind bit-identisch zu
#     phase_simulation(sc), und die Tabellenquantile stammen aus GENAU dieser
#     Verteilung (aus B rekonstruiert) — nicht aus einer frischen Ziehung.
#   - Struktur/Zurueckweisung: H Zeilen, Spalten Monat/P05/P50/P95/P_unter,
#     P_unter in [0,1]; Nicht-Szenario wird zurueckgewiesen (Klassenvertrag).
#
#  Keine fest verdrahteten Absolutzahlen — die Referenz wird numerisch aus der
#  Mappe/Engine (ueber den unabhaengigen phase_simulation-Lauf und dessen B)
#  nachgerechnet; den Tier-1-Anker prueft der Golden-Master in run_all.R. Die
#  Toleranzen stammen 1:1 aus den phasen_anker des Handoffs (Fan 1e-9, P_unter
#  1e-12).
#
#  Warum die Referenz ein EIGENER phase_simulation(sc)-Lauf ist (nicht out$fan
#  gegen sich selbst): eine reine Selbstkonsistenz (table$P05 == out$fan) haette
#  keine Mutationskraft, weil out$fan bereits aus phase_simulation stammt. Das
#  mutationsstarke Orakel rekonstruiert die Monatsquantile UNABHAENGIG aus ref$B
#  (dem Endbestand-Sample desselben, deterministischen Simulationslaufs, Seed
#  2026) AUSSERHALB von phase_sim_monate — ein Etikettentausch in der Tabelle
#  faellt damit zwingend rot. (LESSON: mindestens ein Orakel rekonstruiert die
#  Referenz ausserhalb der geprueften Funktion.)
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
source(file.path(.root, "R", "phase_sim_monate.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# EIN gemeinsames Szenario + je EIN Lauf, LAZY memoisiert ueber Promises: die
# genestete Simulation ist teuer (jeder Pfad laeuft durch run_model), daher ein
# kleines S wie in den Schwester-Orakeln (test_phase_sim_noise/breach). Seed
# bleibt der Szenario-Schluessel 2026 -> beide Laeufe sind deterministisch und
# rechnen bit-identisch auf demselben Endbestand-Sample.
#   .ref = phase_simulation(sc)   — unabhaengige Referenz (liefert B/fan/P_under)
#   .out = phase_sim_monate(sc)   — die geprueften Projektion
.sc    <- scenario_from_workbook(WB, name = "MonateSpec",
                                 sim = list(paths = 25L, seed = 2026))
.cache <- new.env(parent = emptyenv())
delayedAssign("ref", phase_simulation(.sc),  assign.env = .cache)
delayedAssign("out", phase_sim_monate(.sc),  assign.env = .cache)
sim_ref <- function() .cache$ref
sim_out <- function() .cache$out

# ---------------------------------------------------------------------------
# Reine Projektion (keine zweite MC): out$fan und out$P_under sind bit-identisch
# zum unabhaengigen phase_simulation(sc)-Lauf (durchgereicht, Toleranz 0), und
# out$H == metrics$H. Verbiegt phase_sim_monate die Simulation, faellt das rot.
# ---------------------------------------------------------------------------
test_that("out$fan/out$P_under sind bit-identisch zu phase_simulation (reine Projektion, durchgereicht)", {
  ref <- sim_ref()
  out <- sim_out()

  expect_equal(out$fan,     ref$fan,     tolerance = 0)
  expect_equal(out$P_under, ref$P_under, tolerance = 0)
  expect_equal(out$H,       ref$metrics$H)
})

# ---------------------------------------------------------------------------
# Anker "table$P05/P50/P95 == fan" (Selbstkonsistenz der Projektion, Toleranz
# 1e-9): die Quantilspalten der Monats-Tabelle entsprechen genau den fan-Zeilen
# 5%/50%/95% — kein Etikettentausch, keine falsche Quantil-Zuordnung. Der
# Vergleich ist namensblind (unname), weil fan-Zeilen benannt sind ("5%"..).
# ---------------------------------------------------------------------------
test_that("table$P05/P50/P95 == fan-Zeilen 5%/50%/95% (Selbstkonsistenz der Projektion)", {
  out <- sim_out()
  expect_equal(unname(out$table$P05), unname(out$fan["5%",  ]), tolerance = 1e-9)
  expect_equal(unname(out$table$P50), unname(out$fan["50%", ]), tolerance = 1e-9)
  expect_equal(unname(out$table$P95), unname(out$fan["95%", ]), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Mutationsstarkes Orakel (faengt vertauschte Etiketten): die Quantilspalten
# stammen aus GENAU der Simulationsverteilung — UNABHAENGIG aus ref$B
# nachgerechnet (apply(B, 2, quantile, c(.05,.5,.95))), ausserhalb von
# phase_sim_monate. Bringt ein Mutant P05/P95 durcheinander oder rechnet eine
# frische Ziehung, weicht die Tabelle vom ref$B-Quantil ab -> rot. Toleranz 1e-9.
# ---------------------------------------------------------------------------
test_that("table$P05/P50/P95 == unabhaengig aus B nachgerechnete Monatsquantile (Etikettentausch faellt rot)", {
  ref <- sim_ref()
  out <- sim_out()
  q   <- apply(ref$B, 2, quantile, probs = c(.05, .50, .95))  # Zeilen "5%","50%","95%"

  expect_equal(unname(out$table$P05), unname(q["5%",  ]), tolerance = 1e-9)
  expect_equal(unname(out$table$P50), unname(q["50%", ]), tolerance = 1e-9)
  expect_equal(unname(out$table$P95), unname(q["95%", ]), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Anker "P_unter": table$P_unter je Monat == colMeans(B < mindestreserve) mit
# der Schwelle aus dem MODELL (nicht hartcodiert), unabhaengig aus ref$B
# nachgerechnet; zusaetzlich == der durchgereichte out$P_under. Toleranz 1e-12.
# ---------------------------------------------------------------------------
test_that("table$P_unter == colMeans(B < mindestreserve) mit Modell-Schwelle (und == durchgereichter P_under)", {
  m   <- load_model(WB)
  ref <- sim_ref()
  out <- sim_out()

  expect_equal(unname(out$table$P_unter),
               unname(colMeans(ref$B < m$mindestreserve)), tolerance = 1e-12)
  expect_equal(unname(out$table$P_unter), unname(out$P_under), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Anker "Monotonie": je Tabellenzeile P05 <= P50 <= P95 (Quantil-Ordnung; faengt
# eine vertauschte Zuordnung, die die Selbstkonsistenz allein nicht sieht).
# ---------------------------------------------------------------------------
test_that("Quantil-Monotonie je Tabellenzeile: P05 <= P50 <= P95", {
  out  <- sim_out()
  mono <- all(out$table$P05 <= out$table$P50 & out$table$P50 <= out$table$P95)
  expect_true(mono)
})

# ---------------------------------------------------------------------------
# Struktur: table hat genau H Zeilen und die Spalten Monat/P05/P50/P95/P_unter;
# die Quantilspalten sind numerisch (EUR-Formatierung erst im Render), P_unter
# liegt in [0,1]. nrow(table) == out$H == metrics$H.
# ---------------------------------------------------------------------------
test_that("Struktur: H Zeilen, Spalten Monat/P05/P50/P95/P_unter, Quantile numerisch, P_unter in [0,1]", {
  ref <- sim_ref()
  out <- sim_out()
  H   <- ref$metrics$H

  expect_s3_class(out$table, "data.frame")
  expect_equal(nrow(out$table), H)
  expect_equal(out$H, H)
  expect_true(all(c("Monat", "P05", "P50", "P95", "P_unter") %in% names(out$table)))

  expect_true(is.numeric(out$table$P05))
  expect_true(is.numeric(out$table$P50))
  expect_true(is.numeric(out$table$P95))
  expect_true(is.numeric(out$table$P_unter))

  expect_true(all(out$table$P_unter >= 0 & out$table$P_unter <= 1))
})

# ---------------------------------------------------------------------------
# Die Monatsbeschriftung deckt M1..MH ab (dieselbe Beschriftung wie der Fan /
# die Legacy-Vorlage), damit die Projektion Zeilen nicht verschiebt.
# ---------------------------------------------------------------------------
test_that("table$Monat beschriftet M1..MH in Reihenfolge", {
  ref <- sim_ref()
  out <- sim_out()
  H   <- ref$metrics$H
  expect_equal(as.character(out$table$Monat), paste0("M", seq_len(H)))
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_sim_monate weist ein Nicht-Szenario zurueck", {
  expect_error(phase_sim_monate(list(mdl = NULL)), "liqui_scenario")
})
