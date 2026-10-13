# ============================================================================
#  tests/test_phase_kor_e2e.R   —   E2E-CONTROL (rot zuerst) fuer Phase 5: Korrelation
#
#  shinytest2-Control fuer den NOCH NICHT verdrahteten Korrelations-Schirm
#  (handoff/phase-05-korrelation.json: menue_wert "korrelation", Reiter
#  "Korrelations-Heatmap" / "Treiber-Matrix" / "Reparatur & Vertrag").
#  Dieser Test MUSS gegen den aktuellen Stand FEHLSCHLAGEN: der Menuepunkt
#  "korrelation" existiert in app/app.R noch nicht — set_inputs(phase =
#  "korrelation") findet keine Radio-Option, der Schirm bleibt unverdrahtet.
#  Der Builder erfuellt ihn erst mit der Verdrahtung.
#
#  Muster: reparierter E2E-Block aus test_phase_vert.R —
#   - AppDriver$get_value(output=) liefert fuer uiOutputs list(html, deps),
#     fuer fehlgeschlagene Outputs list(message, call, type). Ein Output-
#     Fehler bricht LAUT ab; sonst wird der GERENDERTE TEXT gematcht (Tags
#     raus, HTML-Entities zurueck) — dank .noWS in app/app.R zeichenidentisch
#     zum DOM-Text, die Sollwerte matchen woertlich.
#   - Outputs auf verdeckten Reitern sind suspendiert (suspendWhenHidden):
#     erst den Reiter ueber sein woertliches Label anklicken, dann lesen.
#   - OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, wird SICHTBAR
#     uebersprungen (skip mit konkreter Begruendung) — kein stiller Pass,
#     kein succeed(). NOT_CRAN setzt der Runner (tests/run_all.R).
#
#  Sollwerte kommen aus dem Wrapper phase_korrelation() selbst (gleiche Mappe
#  wie die App); Pfade werden NICHT voll verglichen (der Szenario-Workspace
#  liegt in tempdir() und ist im App-Prozess ein anderer als hier) — vom
#  Artefakt-Pfad wird nur der deterministische Dateiname gematcht.
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
source(file.path(.root, "R", "phase_korrelation.R"))
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
# shinytest2: faehrt den Korrelations-Schirm headless und prueft, dass die
# Diagnose-Zeile (kor_diag) und Reparatur & Vertrag (kor_recon) konkrete
# Kennzahlen aus dem Engine-Wrapper anzeigen (UI-Control-Gate: wo es ein
# Control gibt, zusaetzlich ein shinytest2-Test).
# ----------------------------------------------------------------------------
test_that("shinytest2: Korrelations-Schirm zeigt Diagnose- und Reparatur-Engine-Werte", {
  for (pkg in c("shinytest2", "chromote"))
    skip_if_not_installed(pkg)
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome),
          "E2E uebersprungen: kein Chrome/Chromium (shinytest2-Infrastruktur noch nicht eingerichtet)")

  # Engine-Referenz (gleicher Wrapper, gleiche Mappe wie die App).
  ref <- phase_korrelation(scenario_from_workbook(WB))
  dg  <- ref$diagnostics

  app <- shinytest2::AppDriver$new(
    file.path(.root, "app"), name = "kor",
    load_timeout = 60000, timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("demo")
  app$set_inputs(phase = "korrelation")
  app$wait_for_idle(timeout = 60000)

  # Diagnose spiegelt Treiberzahl, Reparaturmethode und max. Aenderung.
  .aktiviere_reiter(app, "Korrelations-Heatmap")
  diag <- .output_text(app$get_value(output = "kor_diag"))
  expect_match(diag, sprintf("%d", dg$n_treiber),          fixed = TRUE)
  expect_match(diag, ref$method,                           fixed = TRUE)
  expect_match(diag, sprintf("%.4f", ref$max_aenderung),   fixed = TRUE)

  # Reparatur & Vertrag nennt das Artefakt (Dateiname deterministisch, der
  # tempdir()-Workspace nicht) und die Eigenwerte der make_psd-Reparatur.
  .aktiviere_reiter(app, "Reparatur & Vertrag")
  recon <- .output_text(app$get_value(output = "kor_recon"))
  expect_match(recon, "korrelation.rds",                   fixed = TRUE)
  expect_match(recon, sprintf("%.4f", ref$ev_before),      fixed = TRUE)
  expect_match(recon, sprintf("%.4f", ref$ev_after),       fixed = TRUE)
  expect_match(recon, sprintf("%d", dg$n_treiber),         fixed = TRUE)
})
