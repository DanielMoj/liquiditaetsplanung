# ============================================================================
#  tests/test_export_z3_ui.R  —  Verdrahtungs-/Bindungs-Orakel (rot_zuerst:
#  TRUE, Teil der Runde-T-SPEZIFIKATION) fuer phase-13-z3-ui
#
#  Praezedenz tests/test_export_pdf_ui.R (Lehre aus xlsx-M4): das Orakel prueft
#  die GANZE Kette downloadHandler export_z3_btn -> export_ergebnisse
#  (rv-Bindung) + scenario_from_workbook(rv$wbpath, name = rv$wbname) ->
#  export_z3 -> utils::zip -> geschriebene Datei, ueber den echten Abruf
#  output$export_z3_btn im testServer. Rot zuerst: der Output export_z3_btn
#  existiert im Server noch nicht (app/app.R sourct R/z3.R noch nicht und traegt
#  den Handler noch nicht) — der Abruf faellt als Fehler.
#
#  Kill-Mechanik der scenario_from_workbook-Naht: die Forecast-Quelle rv$out
#  wird aus einem Szenario mit dem Namen .FC_NAME gerechnet, der Handler baut
#  sein sc aber aus rv$wbname (bewusst UNTERSCHIEDLICH gesetzt: .HANDLER_NAME).
#  In eingaben.csv steht die Szenario-Zelle == sc$name — sie MUSS .HANDLER_NAME
#  tragen (Handler-sc aus rv$wbname), nicht .FC_NAME; eine an das Forecast-
#  Szenario gebundene sc-Konstruktion faellt hier rot. Die Mappe-Zelle ==
#  basename(rv$wbpath) pinnt die rv$wbpath-Bindung.
#
#  ABWEICHUNG wie in den Schwestern, bewusst: nach der rv-Belegung KEIN
#  session$flushReact() — der Download-Abruf rechnet pull-basiert frisch; kein
#  Render muss die Zwischenstaende verdauen (input$phase = "forecast" haelt die
#  Sim-/Varianz-Auto-Trigger inert). Kein skip, kein succeed. Daemon-Hygiene
#  wie in den Schwestern (mirai::daemons(0) am Dateiende, auch im Fehlerpfad).
#
#  Der reale phase_forecast-Lauf auf der Tier-1-Mappe ist deterministisch und
#  leichtgewichtig (keine Monte-Carlo). scenario_from_workbook/phase_forecast
#  liegen nach dem Sourcen von app/app.R im globalen Namensraum (die inneren
#  source()-Aufrufe der App laufen mit dem source-Default local=FALSE gegen
#  .GlobalEnv).
# ============================================================================

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

WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Paarweise unterscheidbare Szenario-Namen: die Forecast-Quelle traegt einen
# anderen Namen als der Handler ihn aus rv$wbname baut — so pinnt die
# Szenario-Zelle in eingaben.csv die scenario_from_workbook(name=rv$wbname)-Naht.
.FC_NAME      <- "z3uiForecastSrc"
.HANDLER_NAME <- "z3uiHandlerMandat"

# Realer, deterministischer Forecast auf der Tier-1-Mappe (kleine Laufzeit).
.sc_fc <- scenario_from_workbook(WB, name = .FC_NAME)
.fc    <- phase_forecast(.sc_fc)

# ---------------------------------------------------------------------------
# Leseform: Key/Wert aus einer entpackten CSV (readLines + Semikolon-Split).
# ---------------------------------------------------------------------------
.csv_kv <- function(pf, key) {
  if (!file.exists(pf)) return(NA_character_)
  ll <- readLines(pf, warn = FALSE)
  if (length(ll) < 2L) return(NA_character_)
  for (ln in ll[-1L]) {
    f <- strsplit(ln, ";", fixed = TRUE)[[1L]]
    if (length(f) >= 2L && identical(f[[1L]], key)) return(f[[2L]])
  }
  NA_character_
}

test_that("export_z3_btn zippt das export_z3-Buendel (CSV+index.xml) mit Handler-sc aus rv$wbpath/rv$wbname; NULL-Sammlung wirft validate", {
  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # Sim-/Varianz-Auto-Trigger inert.

    # --- Leerpfad zuerst: alle Quellen NULL -> validate-Fehler ---------------
    rv$out           <- NULL
    sim_rv$out       <- NULL
    var_rv$out       <- NULL
    vergleich_rv$out <- NULL
    e <- tryCatch(output$export_z3_btn, error = function(e) e)
    expect_s3_class(e, "shiny.silent.error")
    expect_true(grepl("Erst rechnen, dann exportieren.",
                      conditionMessage(e), fixed = TRUE))

    # --- Teilprobe nur forecast: Buendel + Handler-sc-Bindung ---------------
    rv$wbpath <- WB
    rv$wbname <- .HANDLER_NAME
    rv$out    <- .fc            # export_ergebnisse() -> list(forecast = .fc)

    pfad <- output$export_z3_btn
    expect_identical(basename(pfad), "liquiditaetsplanung_export_z3.zip")
    expect_true(file.exists(pfad))
    # PK-Magic: die ersten zwei Bytes eines ZIP sind 'P' 'K'.
    expect_identical(rawToChar(readBin(pfad, "raw", 2L)), "PK")

    # Buendel-Inhalt: die ZIP mit Basis-R entpacken (utils::unzip nutzt einen
    # internen Entpacker, kein externes Binary) und die Dateien pruefen.
    ex <- file.path(tempdir(), "z3-ui-unzip")
    unlink(ex, recursive = TRUE)
    dir.create(ex, recursive = TRUE)
    utils::unzip(pfad, exdir = ex)
    dateien <- list.files(ex, recursive = TRUE, full.names = TRUE)
    bn <- basename(dateien)

    # Genau das deterministische CSV+index.xml-Buendel der Teilprobe.
    expect_true(all(c("eingaben.csv", "forecast.csv", "forecast_kpis.csv",
                      "index.xml") %in% bn))
    # PDF/A-Entscheid: KEINE .pdf/.pdfa im Buendel.
    expect_false(any(grepl("\\.(pdf|pdfa)$", bn, ignore.case = TRUE)))

    # scenario_from_workbook(rv$wbpath, name = rv$wbname)-Naht: die Szenario-
    # Zelle traegt rv$wbname (Handler-sc), NICHT den Forecast-Szenario-Namen;
    # die Mappe-Zelle traegt basename(rv$wbpath).
    egb <- dateien[bn == "eingaben.csv"][[1L]]
    expect_identical(.csv_kv(egb, "Szenario"), .HANDLER_NAME)
    expect_false(identical(.csv_kv(egb, "Szenario"), .FC_NAME))
    expect_identical(.csv_kv(egb, "Mappe"), basename(WB))
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht).
.daemons_zu()
