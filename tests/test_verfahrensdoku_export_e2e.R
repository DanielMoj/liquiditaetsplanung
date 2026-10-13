# ============================================================================
#  tests/test_verfahrensdoku_export_e2e.R  —  E2E-CONTROL (zusaetzlich) fuer
#  phase-15-verfahrensdoku-export: headless-Download der GoBD-Verfahrensdoku
#  (Rz. 153, .xlsx) ueber den downloadButton export_verfahrensdoku_btn.
#
#  WARUM DIESE DATEI EXISTIERT
#  ---------------------------
#  Die REGEL (CLAUDE.md Schritt 3 / IMPLEMENTATION_RULES.md) verlangt fuer ein
#  neues Shiny-Control zusaetzlich zum shiny::testServer-Orakel einen
#  shinytest2-Headless-Test — ODER einen SICHTBAREN, begruendeten Skip, wenn die
#  Browser-Infrastruktur fehlt (etablierte E2E-Regel: kein stop(), das den Build
#  braeche, und kein stiller Skip, der ueber gruen luegt). Die bestandsgeschuetzte
#  tests/test_verfahrensdoku_export_ui.R prueft die Handler-Verdrahtung bereits
#  IM PROZESS (shiny::testServer); diese Datei schliesst die Control-Luecke, indem
#  sie den Download ueber den ECHTEN, gerenderten Button in einem echten Browser
#  ausloest und die gelieferte xlsx + Blattstruktur prueft.
#
#  Muster: das etablierte E2E-Control tests/test_export_z3_e2e.R
#   - AppDriver$new() faehrt die App in einem EIGENEN Unterprozess samt Browser;
#     die Suite-interne RSS-Last (Gate-A) waechst dadurch NICHT — der App-/MC-
#     Speicher liegt im Kind, nicht im run_all.R-Prozess. DESHALB sourct diese
#     Datei app/app.R NICHT in-Process (die in-Process-Naht deckt bereits die
#     bestandsgeschuetzte _ui.R ab; ein zweiter in-Process-App-Load hier wuerde
#     die Suite-RSS unnoetig verdoppeln).
#   - OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, wird SICHTBAR
#     uebersprungen (skip mit Begruendung) — kein stiller Pass, kein succeed().
#     NOT_CRAN setzt der Runner (tests/run_all.R), sonst skippt AppDriver$new()
#     intern still als "On CRAN".
#   - app$get_download(output) loest den downloadHandler ueber die echte
#     Download-URL aus und liefert den Pfad der geladenen Datei zurueck.
#
#  Die Blattnamen sind UNABHAENGIGE Literale (nicht aus verfahrensdoku_erstellen
#  abgeleitet — keine Selbstbestaetigung).
#
#  BESTANDSSCHUTZ: reine Neudatei; beruehrt weder kernel/ noch R/ noch app/app.R
#  noch eine bestehende test_*-Datei. Startet die App nur als Unterprozess.
# ============================================================================

library(testthat)

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Blattnamen als UNABHAENGIGE Literale (nicht aus verfahrensdoku_erstellen
# abgeleitet).
.blaetter_soll <- c("meta", "allgemeine_beschreibung", "betriebsdoku",
                    "bestand", "review")

# ---------------------------------------------------------------------------
# (Control) shinytest2-Headless: echter downloadButton im DOM-Container
# #export-verfahrensdoku, echter Abruf, gelieferte xlsx + Blattstruktur.
# OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, SICHTBARER Skip mit
# Begruendung (kein stop(), kein stiller Pass). NOT_CRAN setzt der Runner.
# ---------------------------------------------------------------------------
test_that("shinytest2: #export-verfahrensdoku laedt die VD-xlsx (fuenf Blaetter) headless ueber den echten Button", {
  for (pkg in c("shinytest2", "chromote"))
    skip_if_not_installed(pkg)
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome),
          "E2E uebersprungen: kein Chrome/Chromium (shinytest2-Infrastruktur noch nicht eingerichtet)")

  app <- shinytest2::AppDriver$new(
    file.path(.root, "app"), name = "verfahrensdoku",
    load_timeout = 60000, timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # DOM-Naht: Container #export-verfahrensdoku traegt den downloadButton.
  html <- app$get_html("#export-verfahrensdoku")
  expect_true(is.character(html) && nzchar(html))
  expect_true(grepl("export_verfahrensdoku_btn", html, fixed = TRUE))

  # VD ist der Prozedur gewidmet, nicht Ergebnissen: KEIN Demo-Lauf noetig, der
  # Download ist sofort verfuegbar.
  pfad <- app$get_download("export_verfahrensdoku_btn")
  expect_true(file.exists(pfad))
  expect_identical(basename(pfad), "liquiditaetsplanung_verfahrensdoku.xlsx")
  # xlsx ist ein ZIP-Container: die ersten zwei Bytes sind 'P' 'K'.
  expect_identical(rawToChar(readBin(pfad, "raw", 2L)), "PK")

  # Gelieferte Blattstruktur: EXAKT die fuenf VD-Blaetter, review-nummer 1..8.
  wb <- openxlsx2::wb_load(pfad)
  sh <- unname(openxlsx2::wb_get_sheet_names(wb))
  expect_identical(sh, .blaetter_soll)
  rvdf <- openxlsx2::wb_to_df(wb, sheet = "review", detectDates = FALSE)
  expect_equal(as.numeric(rvdf$nummer), 1:8, tolerance = 1e-12)
})
