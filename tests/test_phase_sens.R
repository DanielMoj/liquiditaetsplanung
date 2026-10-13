# ============================================================================
#  tests/test_phase_sens.R
#  Prueft, dass der Phase-3-Wrapper (Sensitivitaet) die Engine-Werte UNVERAENDERT
#  benutzt: der Tornado-Swing eines Treibers stimmt mit einem direkten run_model-
#  Aufruf ueberein (Treue), die Tornado-Tabelle ist nach |Swing| sortiert, der
#  Sobol ist unter festem Seed 1 deterministisch und faellt bei fehlendem Blatt 06
#  sauber zurueck (Tornado unberuehrt). Keine fest verdrahteten Absolutzahlen —
#  den Tier-1-Anker prueft der Golden-Master in run_all.R.
# ============================================================================
# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_sensitivitaet.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

test_that("Basis-Tiefpunkt stimmt mit der Engine ueberein", {
  mdl <- load_model(WB); ref <- run_model(mdl)
  out <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 0, plot = FALSE)
  expect_equal(out$base_trough,  ref$kpis$trough)
  expect_equal(out$trough_month, ref$kpis$trough_month)
})

test_that("Tornado-Swing eines Treibers ist treu zur Engine (direkte run_model-Auslenkung)", {
  mdl <- load_model(WB)
  out <- phase_sensitivitaet(scenario_from_workbook(WB), shock = 0.01, sobol_N = 0, plot = FALSE)
  d   <- out$tornado$driver[1]                      # dominanter Treiber
  aL  <- mdl$assume; aL[[d]] <- aL[[d]] - 0.01
  aH  <- mdl$assume; aH[[d]] <- aH[[d]] + 0.01
  ref_low  <- run_model(mdl, aL)$kpis$trough
  ref_high <- run_model(mdl, aH)$kpis$trough
  row <- out$tornado[out$tornado$driver == d, ]
  expect_equal(row$low,   ref_low)
  expect_equal(row$high,  ref_high)
  expect_equal(row$swing, ref_high - ref_low)
})

test_that("Tornado-Tabelle ist nach |Swing| absteigend sortiert und vollstaendig", {
  out <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 0, plot = FALSE)
  expect_true(all(c("menge", "preis", "tarif") %in% out$tornado$driver))
  expect_equal(out$tornado$driver, out$drivers)         # drivers folgen der Sortierung
  sw <- abs(out$tornado$swing)
  expect_false(is.unsorted(rev(sw)))                    # absteigend
  expect_equal(out$diagnostics$top_driver, out$tornado$driver[1])
})

test_that("Sobol liefert S1/ST in [0,1] je Blatt-06-Treiber und ist unter Seed 1 deterministisch", {
  s1 <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 48, plot = FALSE)
  expect_true(s1$sobol_available)
  expect_false(is.null(s1$sobol))
  expect_equal(nrow(s1$sobol), length(s1$sobol_drivers))
  expect_true(all(s1$sobol$S1 >= 0 & s1$sobol$S1 <= 1))
  expect_true(all(s1$sobol$ST >= 0 & s1$sobol$ST <= 1))
  expect_true(all(s1$sobol_drivers %in% s1$drivers))    # nur dokumentierte Treiber
  # Determinismus: gleicher Seed, gleiches N -> bitidentisch (Szenario-Schluessel).
  s2 <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 48, plot = FALSE)
  expect_equal(s1$sobol, s2$sobol)
  expect_equal(s1$sobol_seed, 1L)
})

test_that("Fehlendes Blatt 06 laesst nur den Sobol sauber zurueckfallen — Tornado unberuehrt", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$dists_raw <- NULL                              # optionales Blatt fehlt
  out <- phase_sensitivitaet(sc, sobol_N = 48, plot = FALSE)
  expect_false(out$sobol_available)
  expect_null(out$sobol)
  expect_true(is.character(out$sobol_reason) && nzchar(out$sobol_reason))
  # Tornado bleibt voll funktionsfaehig.
  expect_equal(nrow(out$tornado), length(out$drivers))
  expect_true(all(is.finite(out$tornado$swing)))
})

test_that("Mengen -10% verschiebt den Basis-Tiefpunkt konsistent zur Engine", {
  base <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 0, plot = FALSE)
  down <- phase_sensitivitaet(scenario_from_workbook(WB, assume_overrides = list(menge = -0.10)),
                              sobol_N = 0, plot = FALSE)
  mdl  <- load_model(WB)
  a    <- mdl$assume; a$menge <- -0.10
  expect_equal(down$base_trough, run_model(mdl, a)$kpis$trough)
  expect_true(down$base_trough < base$base_trough)
})

test_that("Tornado-Plot wird in den Szenario-Workspace geschrieben (sofern Grafikgeraet)", {
  sc  <- scenario_from_workbook(WB)
  out <- phase_sensitivitaet(sc, sobol_N = 48, plot = TRUE)
  if (!is.na(out$tornado_plot)) {
    expect_true(file.exists(out$tornado_plot))
    expect_gt(file.info(out$tornado_plot)$size, 0)
    if (!is.na(out$sobol_plot)) {
      expect_true(file.exists(out$sobol_plot))
      expect_gt(file.info(out$sobol_plot)$size, 0)
    }
  } else {
    succeed("kein Grafikgeraet verfuegbar — Plot uebersprungen (sauberer Fallback)")
  }
})

test_that("phase_sensitivitaet weist ein Nicht-Szenario zurueck", {
  expect_error(phase_sensitivitaet(list(mdl = NULL)), "liqui_scenario")
})

# E2E-Lesehelfer (Reparatur — diese Controls liefen wegen des stillen
# On-CRAN-Skips nie und waren doppelt defekt):
#  - AppDriver$get_value(output=) liefert fuer uiOutputs list(html, deps),
#    fuer fehlgeschlagene Outputs list(message, call, type) und fuer
#    renderTable einen HTML-String. Ein Output-Fehler bricht hier LAUT mit
#    der echten Fehlermeldung ab; sonst wird der GERENDERTE TEXT gematcht
#    (Tags raus, HTML-Entities zurueck) — dank .noWS in app/app.R
#    zeichenidentisch zum DOM-Text, die Sollwerte matchen woertlich.
#  - Outputs auf verdeckten Reitern sind suspendiert (suspendWhenHidden):
#    erst den Reiter ueber sein woertliches Label anklicken und Idle
#    abwarten, dann lesen.
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
# shinytest2: faehrt den verdrahteten Sensitivitaets-Schirm headless und prueft,
# dass Tornado- und Sobol-Tab konkrete Werte aus dem Engine-Wrapper anzeigen.
# OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, wird SICHTBAR
# uebersprungen (skip mit Begruendung) — kein stiller Pass, aber auch kein
# harter Abbruch. NOT_CRAN erzwingt der Runner (tests/run_all.R) vor test_dir;
# ohne die Variable skippte AppDriver$new() still als "On CRAN".
# ----------------------------------------------------------------------------
test_that("shinytest2: Sensitivitaets-Schirm zeigt Tornado- und Sobol-Engine-Werte", {
  for (pkg in c("shinytest2", "chromote"))
    skip_if_not_installed(pkg)
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome),
          "E2E uebersprungen: kein Chrome/Chromium (shinytest2-Infrastruktur noch nicht eingerichtet)")

  # Engine-Referenz (gleicher Wrapper, gleiche Mappe wie die App).
  ref_t <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 0,  plot = FALSE)
  ref_s <- phase_sensitivitaet(scenario_from_workbook(WB), sobol_N = 48, plot = FALSE)

  app <- shinytest2::AppDriver$new(
    file.path(.root, "app"), name = "sens",
    load_timeout = 60000, timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("demo")
  app$set_inputs(phase = "sens")
  app$wait_for_idle(timeout = 60000)

  # Tornado-Reiter explizit aktivieren (beim Oeffnen aktiv; der Test soll
  # nicht an der Default-Reiterwahl haengen), dann Diagnose + Tabelle lesen.
  .aktiviere_reiter(app, "Tornado")
  diag <- .output_text(app$get_value(output = "sens_diag"))
  expect_match(diag, ref_t$diagnostics$top_driver, fixed = TRUE)
  expect_match(diag, fmt(ref_t$base_trough),        fixed = TRUE)

  tor_tbl <- .output_text(app$get_value(output = "sens_tornado_tbl"))
  for (d in ref_t$tornado$driver)
    expect_match(tor_tbl, d, fixed = TRUE)

  # Die Sobol-Outputs liegen auf einem VERDECKTEN Reiter (suspendiert, ohne
  # Aktivierung NULL) — Reiter woertlich anklicken, dann lesen.
  .aktiviere_reiter(app, "Sobol")
  sob_note <- .output_text(app$get_value(output = "sens_sobol_note"))
  expect_match(sob_note, "Seed 1", fixed = TRUE)
  expect_match(sob_note, "N=800",  fixed = TRUE)

  sob_tbl <- .output_text(app$get_value(output = "sens_sobol_tbl"))
  for (d in ref_s$sobol$Treiber)
    expect_match(sob_tbl, d, fixed = TRUE)
})
