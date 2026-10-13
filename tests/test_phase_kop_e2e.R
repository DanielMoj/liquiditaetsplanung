# ============================================================================
#  tests/test_phase_kop_e2e.R   —   E2E-CONTROL (zusaetzlich) fuer Phase 5: Kopula
#
#  shinytest2-Control fuer den Kopula-Schirm (handoff/phase-05-kopula.json:
#  menue_wert "kopula", Reiter "Copula-Validierung", output_id "kop_diag",
#  dom_ids kop-reproduziert / kop-spearman-dev). Prueft, dass der verdrahtete
#  Schirm die ECHTEN Engine-Wrapper-Kennzahlen aus phase_kopula() anzeigt.
#
#  Muster: reparierter E2E-Block aus test_phase_kor_e2e.R —
#   - AppDriver$get_value(output=) liefert fuer uiOutputs list(html, deps);
#     ein Output-Fehler bricht LAUT ab; sonst wird der GERENDERTE TEXT gematcht
#     (Tags raus, HTML-Entities zurueck) — dank .noWS zeichenidentisch.
#   - Outputs auf verdeckten Reitern sind suspendiert: erst den Reiter ueber
#     sein woertliches Label anklicken, dann lesen.
#   - OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, wird SICHTBAR
#     uebersprungen (skip mit Begruendung) — kein stiller Pass, kein succeed().
#     NOT_CRAN setzt der Runner (tests/run_all.R).
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
source(file.path(.root, "R", "phase_kopula.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

.output_text <- function(v) {
  if (is.list(v) && !is.null(v$message))
    stop("Shiny-Output-Fehler: ", v$message,
         if (!is.null(v$call)) sprintf(" [Call: %s]", v$call))
  h <- if (is.list(v)) v$html else v
  if (is.null(h) || !length(h))
    stop("Output ohne Wert (NULL) — Reiter nicht aktiviert / Output nicht gerendert.")
  h <- gsub("<[^>]+>", "", as.character(h))
  h <- gsub("&gt;",   ">",  h, fixed = TRUE)
  h <- gsub("&lt;",   "<",  h, fixed = TRUE)
  h <- gsub("&quot;", "\"", h, fixed = TRUE)
  h <- gsub("&#39;",  "'",  h, fixed = TRUE)
  gsub("&amp;", "&", h, fixed = TRUE)
}
.aktiviere_reiter <- function(app, titel) {
  app$run_js(sprintf(
    "var ls = document.querySelectorAll('#phase_body .nav-link'); for (var i = 0; i < ls.length; i++) { if (ls[i].textContent.trim() === %s) { ls[i].click(); break; } }",
    jsonlite::toJSON(titel, auto_unbox = TRUE)))
  app$wait_for_idle(timeout = 60000)
}

# ----------------------------------------------------------------------------
# shinytest2: faehrt den Kopula-Schirm headless und prueft, dass die Diagnose-
# Zeile (kop_diag) die reproduzierten Copula-Kennzahlen aus dem Engine-Wrapper
# anzeigt — Spearman-/Pearson-Abweichung, n und Seed 11 (UI-Control-Gate).
# ----------------------------------------------------------------------------
test_that("shinytest2: Kopula-Schirm zeigt die reproduzierten Copula-Engine-Werte", {
  for (pkg in c("shinytest2", "chromote"))
    skip_if_not_installed(pkg)
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome),
          "E2E uebersprungen: kein Chrome/Chromium (shinytest2-Infrastruktur noch nicht eingerichtet)")

  # Engine-Referenz (gleicher Wrapper, gleiche Mappe wie die App).
  ref <- phase_kopula(scenario_from_workbook(WB))
  v   <- ref$validierung

  app <- shinytest2::AppDriver$new(
    file.path(.root, "app"), name = "kop",
    load_timeout = 60000, timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("demo")
  app$set_inputs(phase = "kopula")
  app$wait_for_idle(timeout = 60000)

  # Diagnose spiegelt die reproduzierten Kennzahlen (Seed 11, n) und die
  # Abweichungen gegen Formel bzw. R — zeichenidentisch zur Wrapper-Formatierung.
  .aktiviere_reiter(app, "Copula-Validierung")
  diag <- .output_text(app$get_value(output = "kop_diag"))
  expect_match(diag, sprintf("%.4f", v$spearman_max_dev), fixed = TRUE)
  expect_match(diag, sprintf("%.4f", v$pearson_max_dev),  fixed = TRUE)
  expect_match(diag, sprintf("%d", as.integer(v$n)),      fixed = TRUE)
  expect_match(diag, "Seed 11",                            fixed = TRUE)
})
