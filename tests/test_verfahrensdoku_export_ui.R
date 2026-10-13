# ============================================================================
#  tests/test_verfahrensdoku_export_ui.R  —  Verdrahtungs-/Bindungs-Orakel
#  (rot_zuerst: TRUE, Teil der Runde-T-SPEZIFIKATION) fuer
#  phase-15-verfahrensdoku-export
#
#  Praezedenz tests/test_export_z3_ui.R / test_export_pdf_ui.R (Lehre aus
#  xlsx-M4: die App-zu-Funktion-Naht lag dort ausserhalb jedes Tests). Das
#  Orakel prueft die GANZE Kette downloadHandler export_verfahrensdoku_btn ->
#  verfahrensdoku_erstellen() (Default umgebung_erfassen, live Systemumgebung)
#  -> export_verfahrensdoku(vd, file) -> geschriebene xlsx-Datei, ueber den
#  echten Abruf output$export_verfahrensdoku_btn im testServer.
#
#  Rot zuerst: der Output export_verfahrensdoku_btn existiert im Server noch
#  nicht (app/app.R sourct R/verfahrensdoku.R noch nicht und traegt den Handler
#  noch nicht) — der Abruf faellt als Fehler.
#
#  BEWUSSTE ABWEICHUNG von den Ergebnis-Schwestern xlsx/docx/pdf/z3: KEIN
#  validate/need. Die Verfahrensdoku ist der Prozedur gewidmet, nicht
#  berechneten Ergebnissen — der Abruf gelingt auch dann, wenn alle Ergebnis-rvs
#  (rv$out, sim_rv$out, var_rv$out, vergleich_rv$out) NULL sind; eine an
#  Ergebnisse gebundene Vorbedingung stirbt hier rot (kein "Erst rechnen"-Hinweis,
#  kein shiny.silent.error). Kein flushReact (Download-Abruf rechnet pull-basiert
#  frisch); input$phase = "forecast" haelt die Sim-/Varianz-Auto-Trigger inert.
#  Kein skip, kein succeed. Daemon-Hygiene (mirai::daemons(0) am Dateiende, auch
#  im Fehlerpfad).
# ============================================================================

library(testthat)

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Beim Sourcen gestartete mirai-Daemons schliessen (Suite-Invariante: kein Leak).
.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

# App in-Process laden (chdir=TRUE wie shiny::runApp("app")).
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

# Unabhaengiges Orakel fuer die Blattliste: ein FRISCH gebautes
# verfahrensdoku_erstellen-Objekt (Default umgebung_erfassen — umgebung_erfassen
# liegt nach dem App-Sourcen im globalen Namensraum, R/manifest.R ist geladen).
# R/verfahrensdoku.R fuer verfahrensdoku_erstellen selbst sourcen (der Generator
# aus phase-14; aendert den roten Kern nicht: der Server-Output fehlt weiter).
source(file.path(.root, "R", "verfahrensdoku.R"))
.vd_frisch <- verfahrensdoku_erstellen()
.blaetter_soll <- names(.vd_frisch)  # meta, allgemeine_beschreibung, betriebsdoku, bestand, review
.meta_feld <- c("titel", "geltungsbereich", "r_version", "plattform",
                "git_sha", "git_dirty", "renv_lock_sha256")

test_that("export_verfahrensdoku_btn liefert die VD-xlsx via verfahrensdoku_erstellen()+export_verfahrensdoku; verfuegbar OHNE gerechnete Ergebnisse", {
  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # Sim-/Varianz-Auto-Trigger inert.

    # BEWUSST alle Ergebnis-Quellen NULL: die VD haengt NICHT an Ergebnissen.
    rv$out           <- NULL
    sim_rv$out       <- NULL
    var_rv$out       <- NULL
    vergleich_rv$out <- NULL

    # Abruf gelingt (KEIN validate/need, kein shiny.silent.error) und liefert
    # den vom downloadHandler geschriebenen Pfad.
    pfad <- output$export_verfahrensdoku_btn
    expect_identical(basename(pfad), "liquiditaetsplanung_verfahrensdoku.xlsx")
    expect_true(file.exists(pfad))

    # Ladbar; EXAKT die fuenf Blaetter eines frisch gebauten VD-Objekts.
    wb <- openxlsx2::wb_load(pfad)
    sh <- unname(openxlsx2::wb_get_sheet_names(wb))
    expect_identical(sh, .blaetter_soll)

    # review-Blatt: die acht Reviewer-Punkte mit nummer 1..8.
    rvdf <- openxlsx2::wb_to_df(wb, sheet = "review", detectDates = FALSE)
    expect_equal(as.numeric(rvdf$nummer), 1:8, tolerance = 1e-12)

    # meta-Blatt: feld-Keys gepinnt (wert umgebungsabhaengig -> nur Struktur).
    mdf <- openxlsx2::wb_to_df(wb, sheet = "meta", detectDates = FALSE)
    expect_identical(names(mdf), c("feld", "wert"))
    expect_identical(as.character(mdf$feld), .meta_feld)
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht).
.daemons_zu()
