# ============================================================================
#  tests/test_kpi_theme_ui.R  —  Ampel-Orakel fuer die KPI-Themes des
#  Forecast-Schirms (Mutgate-Nachschub Runde 2, phase-10-ui)
#
#  Der Rerun ueberlebte mit M2/M4 in der value_box-Ampellogik von
#  output$kpi_row (app/app.R): das Planergebnis-Theme haengt am Vorzeichen
#  von o$kpis$result (theme = if (result >= 0) "success" else "danger").
#  Eine INVERSION (success/danger getauscht) und eine RANDVERSCHIEBUNG
#  (> 0 statt >= 0) ueberlebten, weil die Ampel Praesentationslogik OHNE
#  Phasenvertrag ist: die angezeigten WERTE (fmt(result) usw.) sind ueber
#  die Tier-1-Verankerung im Verifier (tests/run_all.R, Kennzahl-IDs)
#  geprueft — die FARBE der Ampel hing an keinem Orakel. Aufgedeckt hat das
#  Loch der Datei-Scope des Mutgates: Alt-Code in app/app.R ausserhalb
#  jeder Phaseneinheit.
#
#  Kill-Mechanik (drei Faelle, ein testServer):
#    (1) kpis$result POSITIV  -> Planergebnis-Segment traegt bg-success
#        und NICHT bg-danger   (killt die Inversion in eine Richtung),
#    (2) kpis$result NEGATIV  -> Segment traegt bg-danger und NICHT
#        bg-success            (killt die Inversion in die andere),
#    (3) kpis$result EXAKT 0  -> bg-success (Randpin: der Vertrag ist
#        >= 0 — null Planergebnis ist kein Verlust; killt jede Verschiebung
#        der Schwelle, z. B. > 0 oder >= 1).
#  Die Segment-Extraktion arbeitet wie die Chip-Segmente der Schwester
#  test_new_mandate_ui.R: das Planergebnis-Segment reicht vom HTML-Anfang
#  bis zur ID des naechsten Kastens (kpi-tiefpunkt) — die Theme-Klasse des
#  Kastens steht in seinem oeffnenden div VOR der Kennzahl-ID. solvent ist
#  in ALLEN Stubs TRUE, damit bg-danger im gerenderten kpi_row nur aus
#  der result-Bindung stammen kann (Box 3 bliebe sonst eine zweite Quelle).
#
#  Die konkrete bslib-HTML-Auspraegung von theme = "success"/"danger" ist
#  die Karten-Klasse bg-success/bg-danger (value_box_theme haengt
#  "text-bg-" vor blanke Bootstrap-Namen); sie ist unten EXAKT gepinnt und
#  wird per Regime-Waechter am gerenderten HTML einer Standalone-value_box
#  verifiziert — aendert bslib die Auspraegung, faellt der Waechter laut,
#  statt dass die Bindungs-Orakel stumm fehlgreifen.
#
#  Stubs synthetisch (keine Tier-1-Werte, keine Engine, keine Mappe); der
#  Forecast-Stub traegt ALLE Felder, die die rv$out-Renders beim Flush
#  verdauen (kpi_row, diag, guv, cash, dsodpo, statusbar), weil die
#  MockShinySession beim flushReact alle Outputs rechnet (Muster
#  test_phase_sim_ui.R). Kein skip, kein Browser, kleine Laufzeit (EIN
#  Source, EIN testServer). Nebenlaeufigkeits-Hygiene wie in den
#  Schwestern: das Sourcen von app/app.R startet bei HAS_ASYNC
#  mirai::daemons(1); die Daemons werden am Dateiende deterministisch
#  geschlossen — auch im Fehlerpfad.
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

# App in-Process laden. chdir=TRUE laesst find_root() der App aus app/ heraus
# die Repo-Wurzel bestimmen — wie bei shiny::runApp("app").
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()  # ein halb gesourctes app.R darf keinen Daemon zuruecklassen
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

# Gepinnte bslib-Auspraegung der beiden Ampel-Themes (Regime-Waechter unten).
.theme_klasse <- list(success = "bg-success", danger = "bg-danger")

# Die drei Ampel-Faelle: positiv, negativ, exakt 0 (Randpin). Synthetische
# Werte ohne Wiederverwendung aus anderen Tests, keine Tier-1-Werte.
.res_pos  <- 517.25
.res_neg  <- -483.5
.res_null <- 0

# ---------------------------------------------------------------------------
# Forecast-Stub-Fabrik: nur kpis$result variiert je Fall; alle uebrigen
# Felder konstant und minimal konsistent (trough negativ, maxKK = |trough|,
# end = letzter cash-Endbestand). Der Stub bedient JEDEN rv$out-Render der
# App (Kopfkommentar), damit der flushReact keinen Output verhungern laesst.
# solvent = TRUE in allen Faellen — siehe Kill-Mechanik im Kopfkommentar.
# ---------------------------------------------------------------------------
.mk_fc_stub <- function(result) {
  list(
    phase = "forecast",
    guv   = matrix(c(701.5, 702.25,
                     -63.5,  41.75),
                   nrow = 2L, byrow = TRUE,
                   dimnames = list(c("Erloese", "Ergebnis"), c("M1", "M2"))),
    cash  = matrix(c(-708.75, 712.5,
                     -708.75, 712.5),
                   nrow = 2L, byrow = TRUE,
                   dimnames = list(c("Zufluss", "Endbestand"), c("M1", "M2"))),
    dso_dpo = c(umsatz = 31, material = 46),
    kpis  = list(result = result, trough = -708.75, trough_month = 1L,
                 maxKK = 708.75, months_below = 1L, end = 712.5),
    solvent = TRUE,
    diagnostics = list(gv_konsistenz = TRUE, massenerhaltung = TRUE,
                       zahlungsfaehig = TRUE, monate_unter_reserve = 1L)
  )
}

# Planergebnis-Segment aus dem kpi_row-HTML: vom Anfang bis zur ID des
# naechsten Kastens (kpi-tiefpunkt). Die Theme-Klasse des Planergebnis-
# Kastens steht in dessen oeffnendem div VOR der Kennzahl-ID; der
# layout_columns-Rahmen davor traegt keine text-bg-Klassen.
.segment_planergebnis <- function(html) {
  p_erg  <- regexpr('id="kpi-planergebnis"', html, fixed = TRUE)
  p_tief <- regexpr('id="kpi-tiefpunkt"',    html, fixed = TRUE)
  expect_true(p_erg > 0,
              label = "kpi_row-HTML traegt die Kennzahl-ID kpi-planergebnis")
  expect_true(p_tief > 0,
              label = "kpi_row-HTML traegt die Kennzahl-ID kpi-tiefpunkt (Segmentgrenze)")
  expect_true(p_erg < p_tief,
              label = "Planergebnis-Kasten steht vor dem Tiefpunkt-Kasten (Grundlage der Segment-Extraktion)")
  substr(html, 1L, p_tief - 1L)
}

test_that("Regime-Waechter: Stub-results paarweise verschieden, Positiv-Fall nicht 0; bslib-Auspraegung der Themes wie gepinnt", {
  # Waechter als HARTE Fails (kein skip): ist einer verletzt, sind die
  # Ampel-Orakel unten unterscheidungsblind und muessen rot sein.
  expect_true(length(unique(c(.res_pos, .res_neg, .res_null))) == 3L,
              label = "Waechter: die drei Stub-results sind paarweise verschieden (sonst pruefen zwei Faelle dasselbe)")
  expect_true(.res_pos != 0,
              label = "Waechter: der Positiv-Fall ist nicht 0 (der Randpin ist ein EIGENER Fall — sonst bliebe die Schwelle ungeprueft)")
  expect_true(.res_pos > 0 && .res_neg < 0,
              label = "Waechter: Positiv-Fall > 0 und Negativ-Fall < 0 (sonst pruefen die Inversions-Faelle nicht beide Zweige)")

  # Gepinnte bslib-Auspraegung am gerenderten HTML einer Standalone-value_box
  # verifizieren — dieselbe value_box/theme-Mechanik wie in output$kpi_row.
  probe_success <- as.character(htmltools::renderTags(
    bslib::value_box("Probe", "0", theme = "success"))$html)
  probe_danger  <- as.character(htmltools::renderTags(
    bslib::value_box("Probe", "0", theme = "danger"))$html)
  expect_match(probe_success, .theme_klasse$success, fixed = TRUE,
               info = "Regime-Waechter: bslib rendert theme=\"success\" als Karten-Klasse bg-success (der gepinnte Marker der Orakel unten)")
  expect_match(probe_danger, .theme_klasse$danger, fixed = TRUE,
               info = "Regime-Waechter: bslib rendert theme=\"danger\" als Karten-Klasse bg-danger (der gepinnte Marker der Orakel unten)")
  expect_false(grepl(.theme_klasse$danger, probe_success, fixed = TRUE),
               label = "Regime-Waechter: die success-Auspraegung traegt NICHT den danger-Marker (sonst waeren die NICHT-Proben unten blind)")
})

test_that("kpi_row-Ampel bindet das Planergebnis-Theme an das Vorzeichen von kpis$result: positiv success, negativ danger, exakt 0 success (Randpin)", {
  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> die
                                           # Auto-Trigger (Sim/Varianz) inert.

    lese_kpi_html <- function() {
      ui <- output$kpi_row               # renderUI: list(html, deps)
      as.character(if (is.list(ui)) ui$html else ui)
    }

    # Fall 1 — POSITIV: success, nicht danger (Inversion faellt hier rot).
    rv$out <- .mk_fc_stub(.res_pos)
    session$flushReact()
    seg <- .segment_planergebnis(lese_kpi_html())
    expect_match(seg, .theme_klasse$success, fixed = TRUE,
                 info = "positives Planergebnis: der Planergebnis-Kasten traegt bg-success")
    expect_false(grepl(.theme_klasse$danger, seg, fixed = TRUE),
                 label = "positives Planergebnis: der Planergebnis-Kasten traegt NICHT bg-danger")

    # Fall 2 — NEGATIV: danger, nicht success (Inversion faellt auch hier).
    rv$out <- .mk_fc_stub(.res_neg)
    session$flushReact()
    seg <- .segment_planergebnis(lese_kpi_html())
    expect_match(seg, .theme_klasse$danger, fixed = TRUE,
                 info = "negatives Planergebnis: der Planergebnis-Kasten traegt bg-danger")
    expect_false(grepl(.theme_klasse$success, seg, fixed = TRUE),
                 label = "negatives Planergebnis: der Planergebnis-Kasten traegt NICHT bg-success")

    # Fall 3 — EXAKT 0 (Randpin): der Vertrag ist >= 0 — null Planergebnis
    # ist kein Verlust. Eine verschobene Schwelle (> 0, >= 1, ...) faellt
    # GENAU hier rot, waehrend die Faelle 1/2 sie ueberleben wuerden.
    rv$out <- .mk_fc_stub(.res_null)
    session$flushReact()
    seg <- .segment_planergebnis(lese_kpi_html())
    expect_match(seg, .theme_klasse$success, fixed = TRUE,
                 info = "Planergebnis exakt 0: der Kasten traegt bg-success (Vertrag >= 0)")
    expect_false(grepl(.theme_klasse$danger, seg, fixed = TRUE),
                 label = "Planergebnis exakt 0: der Kasten traegt NICHT bg-danger")
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
