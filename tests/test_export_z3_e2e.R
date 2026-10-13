# ============================================================================
#  tests/test_export_z3_e2e.R  —  E2E-CONTROL (zusaetzlich) fuer phase-13-z3-ui:
#  headless-Download des GoBD-Z3-Buendels ueber den downloadButton export_z3_btn.
#
#  WARUM DIESE DATEI EXISTIERT
#  ---------------------------
#  Die REGEL (CLAUDE.md Schritt 3 / IMPLEMENTATION_RULES.md) verlangt fuer ein
#  neues Shiny-Control zusaetzlich zum shiny::testServer-Orakel einen
#  shinytest2-Headless-Test — ODER einen SICHTBAREN, begruendeten Skip, wenn die
#  Browser-Infrastruktur fehlt (etablierte E2E-Regel: kein stop(), das den Build
#  braeche, und kein stiller Skip, der ueber gruen luegt). test_export_z3_ui.R
#  prueft die Handler-Verdrahtung nur IM PROZESS (shiny::testServer); diese Datei
#  schliesst die Control-Luecke, indem sie den Download ueber den ECHTEN,
#  gerenderten Button in einem echten Browser ausloest und den ZIP-Inhalt prueft.
#
#  Muster: die etablierten E2E-Controls test_phase_kop_e2e.R / test_phase_kor_e2e.R
#   - AppDriver$new() faehrt die App in einem EIGENEN Unterprozess samt Browser;
#     die Suite-interne RSS-Last (Gate-A) waechst dadurch NICHT — der App-/MC-
#     Speicher liegt im Kind, nicht im run_all.R-Prozess (relevant fuer die
#     Speicher-Naehte der Suite).
#   - OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, wird SICHTBAR
#     uebersprungen (skip mit Begruendung) — kein stiller Pass, kein succeed().
#     NOT_CRAN setzt der Runner (tests/run_all.R), sonst skippt AppDriver$new()
#     intern still als "On CRAN".
#   - app$get_download(output) loest den downloadHandler ueber die echte
#     Download-URL aus und liefert den Pfad der geladenen Datei zurueck.
#
#  BESTANDSSCHUTZ: reine Neudatei; beruehrt weder kernel/ noch R/ noch app/app.R
#  noch eine bestehende test_*-Datei. Startet die App nur als Unterprozess.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Leseform: Wert zu einem Schluessel aus einer entpackten Semikolon-CSV.
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

# ----------------------------------------------------------------------------
# shinytest2: laedt die Demo-Mappe (rv$out/rv$wbpath/rv$wbname gesetzt), loest
# den Download von export_z3_btn ueber den echten Button aus und prueft, dass
# das geladene ZIP das deterministische CSV+index.xml-Buendel traegt (kein
# PDF/A) und dass die Handler-sc-Naht scenario_from_workbook(rv$wbpath,
# name = rv$wbname) durch den ganzen Browser-Pfad haelt.
# ----------------------------------------------------------------------------
test_that("shinytest2: export_z3_btn laedt das GoBD-Z3-Buendel (CSV+index.xml, kein PDF) mit Handler-sc aus der geladenen Mappe", {
  for (pkg in c("shinytest2", "chromote"))
    skip_if_not_installed(pkg)
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome),
          "E2E uebersprungen: kein Chrome/Chromium (shinytest2-Infrastruktur noch nicht eingerichtet)")

  DEMO_WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

  app <- shinytest2::AppDriver$new(
    file.path(.root, "app"), name = "z3",
    load_timeout = 60000, timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Demo laden: run_forecast setzt rv$out (export_ergebnisse nicht leer),
  # rv$wbpath = DEMO_WB und rv$wbname = "Tier-1-Beispielklinik".
  app$click("demo")
  app$wait_for_idle(timeout = 60000)

  # Download ueber den echten downloadHandler ausloesen.
  pfad <- app$get_download("export_z3_btn")
  expect_true(file.exists(pfad))
  # PK-Magic: die ersten zwei Bytes eines ZIP sind 'P' 'K'.
  expect_identical(rawToChar(readBin(pfad, "raw", 2L)), "PK")

  # Buendel entpacken (utils::unzip nutzt internes miniz, kein externes Binary).
  ex <- file.path(tempdir(), "z3-e2e-unzip")
  unlink(ex, recursive = TRUE)
  dir.create(ex, recursive = TRUE)
  utils::unzip(pfad, exdir = ex)
  dateien <- list.files(ex, recursive = TRUE, full.names = TRUE)
  bn <- basename(dateien)

  # Genau das deterministische CSV+index.xml-Buendel (Forecast-Sammlung).
  expect_true(all(c("eingaben.csv", "forecast.csv", "forecast_kpis.csv",
                    "index.xml") %in% bn))
  # PDF/A-Entscheid: KEINE .pdf/.pdfa im Buendel.
  expect_false(any(grepl("\\.(pdf|pdfa)$", bn, ignore.case = TRUE)))

  # Handler-sc-Naht durch den ganzen Browser-Pfad: die Szenario-Zelle traegt
  # rv$wbname (aus dem Demo-Ladeweg), die Mappe-Zelle basename(rv$wbpath).
  egb <- dateien[bn == "eingaben.csv"][[1L]]
  expect_identical(.csv_kv(egb, "Szenario"), "Tier-1-Beispielklinik")
  expect_identical(.csv_kv(egb, "Mappe"), basename(DEMO_WB))
})
