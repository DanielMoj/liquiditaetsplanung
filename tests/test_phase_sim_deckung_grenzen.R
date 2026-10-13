# ============================================================================
#  tests/test_phase_sim_deckung_grenzen.R  -  Mutgate-Nachschub phase-06-deckung
#
#  Zwei Grenz-Orakel gegen die Ueberlebenden des Mutationslaufs (3/5):
#
#  (1) Grenzwert-Semantik von deckung_from_out (Mutant: < -> <= in P_breach):
#      Ein Pfad mit trough == -kk_limit liegt EXAKT AUF der KK-Linie - das
#      Limit ist AUSGESCHOEPFT, nicht GERISSEN (legacy/05_simulation.R:52,
#      strikt <). Die Spec-Tests (test_phase_sim_deckung.R) meiden diesen
#      Punkt (ihre trough-Vektoren liegen nie auf der Linie), daher hatte <=
#      dort keine Mutationskraft. Rein synthetisches out im phase_simulation-
#      Vertrag, KEIN Engine-Lauf, KEINE Monte-Carlo.
#
#  (2) Naht run_forecast -> rv$kk_limit (Mutant: Vorzeichen-Flip an der Naht):
#      app/app.R sichert beim Mappe-Laden die Modell-Schwelle sc$mdl$kk_limit
#      in rv$kk_limit (Quelle der Deckungs-Anzeige deckung_recon). Das Orakel
#      rekonstruiert die Schwelle EXTERN als load_model(WB)$kk_limit und
#      prueft die Naht in-Process (testServer-Muster wie
#      test_phase_sim_deckung_ui.R): run_forecast laeuft nur phase_forecast -
#      deterministisch, keine MC, kein DOM-Parse (kein xml2, KEIN skip).
#      Ein Waechter kk_ext > 0 stellt sicher, dass der Vorzeichen-Flip
#      (-kk_ext != kk_ext) vom Sollwert unterscheidbar ist.
#
#  Keine Sollwert-Literale aus der Mappe, keine Tier-1-Anker: die Referenzen
#  werden unabhaengig rekonstruiert (synthetischer trough bzw. load_model).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))        # load_model fuer Orakel (2)
source(file.path(.root, "R", "phase_sim_deckung.R"))  # deckung_from_out fuer Orakel (1)
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ===========================================================================
#  (1) Grenzwert-Orakel: Pfad EXAKT AUF der Linie ist KEIN Bruch (strikt <)
# ===========================================================================
test_that("Grenzwert: trough == -kk_limit liegt AUF der Linie und zaehlt NICHT als Bruch (strikt <, kein <=)", {
  # Grenzwert = Limit ausgeschoepft, nicht gerissen (legacy/05_simulation.R:52,
  # strikt <): bei kk_limit = 50 liegt der Pfad -50 EXAKT auf der Linie; nur
  # -100 reisst sie -> P_breach = 1/4. Ein <= zaehlte -50 mit -> 0.5 -> rot.
  out <- list(trough = c(-50, -100, 100, 100), metrics = list(seed = 1L, S = 4L))
  res <- deckung_from_out(out, 50)

  expect_equal(res$P_breach,  0.25, tolerance = 1e-12)
  expect_equal(res$P_solvent, 0.75, tolerance = 1e-12)
  expect_true(res$insO_erfuellt)                       # P_solvent 0.75 > 0.5
  # Sprechende Identitaet: exakt der STRIKTE Anteil, unabhaengig rekonstruiert.
  expect_identical(res$P_breach, mean(c(-50, -100, 100, 100) < -50))
})

# ===========================================================================
#  (2) Naht-Orakel: run_forecast sichert rv$kk_limit == load_model(WB)$kk_limit
# ===========================================================================

# Beim Sourcen gestartete mirai-Daemons schliessen (Suite-Invariante: kein Leak).
.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

# App in-Process laden (Idiom wie test_phase_sim_deckung_ui.R). chdir=TRUE
# laesst find_root() der App aus app/ heraus die Repo-Wurzel bestimmen.
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()  # ein halb gesourctes app.R darf keinen Daemon zuruecklassen
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

test_that("Naht run_forecast: rv$kk_limit ist EXAKT die Modell-Schwelle der Mappe (kein Vorzeichen-Flip)", {
  # Externe Referenz VOR dem testServer-Block: dieselbe Schwelle, die die Mappe
  # traegt - unabhaengig von der App-Naht ueber den Kernel rekonstruiert.
  kk_ext <- load_model(WB)$kk_limit
  # Waechter: nur bei kk_ext > 0 ist ein Vorzeichen-Flip (-kk_ext) vom
  # Sollwert unterscheidbar (bei 0 waere das Orakel blind).
  expect_true(kk_ext > 0)

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # Flush #1 mit rv$wbpath == NULL ->
                                           # Auto-Sim-Trigger und Lazy-Phasen inert.
    run_forecast(WB, "KkNahtOrakel")       # laeuft nur phase_forecast (deterministisch,
                                           # keine MC) und sichert die Naht rv$kk_limit.
    expect_equal(rv$kk_limit, kk_ext)      # Flip (-sc$mdl$kk_limit) faellt hier rot.
    rv$wbpath <- NULL                      # Hygiene: haelt die lazy Schirme (sens/vert/
                                           # kor/kopula) auch bei einem spaeteren Flush
                                           # inert - keine schwere Rechnung im Test.
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht).
.daemons_zu()
