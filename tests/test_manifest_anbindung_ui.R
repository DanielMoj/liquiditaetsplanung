# ============================================================================
#  tests/test_manifest_anbindung_ui.R  —  UI-Bindungs-Orakel der GoBD-Zeile
#  (phase-12-runmanifest-anbindung; ZUSATZTEST Runde I, kein rot-zuerst)
#
#  Prueft die semantische Bindung der drei GoBD-Kennzahlen des Sim-Schirms
#  (Handoff-Schirmvertrag: sim-run-id, sim-iks-bestanden '<n>/<n>',
#  sim-iks-badge) auf shiny::testServer-Basis — Idiom wie
#  tests/test_phase_sim_ui.R: app/app.R in-Process gesourct, sim_rv$out mit
#  unverwechselbaren Stubs belegt (bewusst 3 statt der echten 4 Kontroll-
#  zeilen und Teilbestand 2/3: sum/nrow-Bindung statt hartcodiertem '4/4'),
#  das gerenderte sim_diag-HTML je DOM-ID gegen GENAU sein Quellfeld
#  verglichen. Zusaetzlich der DEFENSIV-Pfad aus dem Handoff: ein
#  Ergebnisobjekt OHNE gobd-Beobachterfeld rendert den Schirm weiter und
#  laesst nur die GoBD-Zeile weg (kein Crash, keine GoBD-IDs im DOM).
#
#  Nebenlaeufigkeits-Hygiene wie im Vorbild: das Sourcen von app/app.R
#  startet bei HAS_ASYNC mirai::daemons(1); die Daemons werden hier
#  deterministisch geschlossen (Fehlerpfad eingeschlossen).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

if (!requireNamespace("xml2", quietly = TRUE))
  stop("Paket 'xml2' fehlt (DOM-Parse des sim_diag-HTML): install.packages(\"xml2\")")

.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

.kz_node <- function(doc, id) xml2::xml_find_first(doc, sprintf("//*[@id='%s']", id))
.kz_text <- function(doc, id) {
  n <- .kz_node(doc, id)
  if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
}

# Basis-Stub mit allen von output$sim_diag referenzierten Feldern (wie im
# Vorbild); plot_path = NA bedient den validate-Fallback von sim_plot, weil
# die MockShinySession beim Flush alle Outputs rechnet.
.stub_basis <- list(
  metrics         = list(S = 777, H = 13, seed = 4242, plan_trough = -123456),
  diagnostics     = list(n_treiber = 9, deterministisch = TRUE, stuetzen_ok = TRUE),
  endbestand_path = "stub-workspace/endbestand_ui_orakel.rds",
  plot_path       = NA_character_
)

# Unverwechselbarer gobd-Stub: 3 Kontrollzeilen, davon 2 bestanden — die
# Anzeige muss '2/3' rechnen (sum/nrow), nicht '4/4' hartcodieren; die
# run_id ist ein reiner Marker-String (kein echter Lauf).
.stub_gobd_teil <- list(
  run_id        = "run-20990101T000000Z-cafe0123",
  manifest_pfad = "stub-runs/run-20990101T000000Z-cafe0123/manifest.json",
  kontrollen    = data.frame(
    name      = c("anker_plan_tiefpunkt", "massenerhaltung", "gv_konsistenz"),
    bestanden = c(TRUE, FALSE, TRUE),
    soll      = c("0", "TRUE", "TRUE"),
    ist       = c("0", "FALSE", "TRUE"),
    details   = c("stub", "stub", "stub"),
    stringsAsFactors = FALSE)
)

test_that("sim_diag GoBD-Zeile: sim-run-id/sim-iks-bestanden/sim-iks-badge binden an gobd; ohne gobd degradiert nur die Zeile", {
  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> der
                                           # Auto-Sim-Trigger bleibt inert.

    # (1) Teilbestand 2/3: Werte-Bindung + Badge in der Gefahrenklasse.
    stub <- .stub_basis; stub$gobd <- .stub_gobd_teil
    sim_rv$out <- stub
    session$flushReact()
    ui   <- output$sim_diag
    doc  <- xml2::read_html(as.character(if (is.list(ui)) ui$html else ui))
    expect_identical(.kz_text(doc, "sim-run-id"), .stub_gobd_teil$run_id)
    expect_identical(.kz_text(doc, "sim-iks-bestanden"), "2/3")
    b <- .kz_node(doc, "sim-iks-badge")
    expect_false(inherits(b, "xml_missing"),
                 label = "Badge 'sim-iks-badge' fehlt im sim_diag-HTML")
    expect_match(xml2::xml_attr(b, "class"), "text-bg-danger", fixed = TRUE,
                 label = "class von 'sim-iks-badge' bei Teilbestand")

    # (2) Alle Kontrollen bestanden: Badge wechselt in die Erfolgsklasse,
    #     der Zaehler folgt sum/nrow ('3/3').
    stub$gobd$kontrollen$bestanden <- c(TRUE, TRUE, TRUE)
    stub$gobd$kontrollen$ist       <- c("0", "TRUE", "TRUE")
    sim_rv$out <- stub
    session$flushReact()
    ui  <- output$sim_diag
    doc <- xml2::read_html(as.character(if (is.list(ui)) ui$html else ui))
    expect_identical(.kz_text(doc, "sim-iks-bestanden"), "3/3")
    expect_match(xml2::xml_attr(.kz_node(doc, "sim-iks-badge"), "class"),
                 "text-bg-success", fixed = TRUE,
                 label = "class von 'sim-iks-badge' bei Vollbestand")

    # (3) DEFENSIV-Pfad (Handoff): Ergebnisobjekt OHNE gobd -> der Schirm
    #     rendert weiter (sim-s vorhanden), die GoBD-IDs fehlen im DOM.
    sim_rv$out <- .stub_basis
    session$flushReact()
    ui  <- output$sim_diag
    doc <- xml2::read_html(as.character(if (is.list(ui)) ui$html else ui))
    expect_identical(.kz_text(doc, "sim-s"), "777")
    for (id in c("sim-run-id", "sim-iks-bestanden", "sim-iks-badge")) {
      expect_true(inherits(.kz_node(doc, id), "xml_missing"),
                  label = sprintf("'%s' darf ohne gobd nicht im DOM stehen", id))
    }
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht).
.daemons_zu()
