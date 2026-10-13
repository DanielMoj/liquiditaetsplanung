# ============================================================================
#  tests/test_phase_vert.R   —   SPEZIFIKATION (rot zuerst) fuer Phase 4: Verteilungen
#
#  Diese Tests spezifizieren den NOCH NICHT gebauten Wrapper
#  phase_verteilungen()/.plot_verteilungen() aus R/phase_verteilungen.R.
#  Sie MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (die Datei existiert
#  noch nicht -> source() bricht ab; jeder Test spezifiziert zusaetzlich das
#  Zielverhalten). Der Builder erfuellt sie erst mit der Implementierung.
#
#  Vertrag (aus handoff/phase-04-verteilungen.json):
#   - Treue: out$spec ist bit-identisch zum direkten build_dists-Lauf der
#     eingefrorenen Engine — die UI verbiegt die Spezifikation nicht.
#   - out$cv_est folgt der DOKUMENTIERTEN Basis-Allokation ueber den driver:
#       menge    = colSums der Positionen mit driver in {menge, menge_preis}
#       material = colSums der Positionen mit driver == material
#     (=> m_leb, grp=material aber driver=menge, liegt in der MENGEN-Basis).
#   - mode == Jahresannahme je Treiber; Spec-Abdeckung: jeder Treiber steht
#     in Blatt 06_Verteilungen UND in mdl$assume.
#   - UNABHAENGIG nachgerechnete aleatorische CV der Menge (lm.fit, NICHT ueber
#     estimate_cv_harmonic) — faengt eine Mutation der Schaetzfunktion.
#   - PERT-Randvertrag qpert(0)=min, qpert(1)=max; Quantil-Monotonie P5<=P50<=P95.
#   - Artefakt fuer Phase 5/6: verteilungen_spec.rds liest sich identisch zurueck.
#   - Fehlerfaelle sauber; Plot-Fallback ohne Grafikgeraet; Nicht-Szenario zurueckgewiesen.
#
#  Keine fest verdrahteten Absolutzahlen — die Anker werden numerisch aus der
#  Mappe nachgerechnet; den Tier-1-Anker prueft der Golden-Master in run_all.R.
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
source(file.path(.root, "R", "phase_verteilungen.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Die dokumentierte Basis-Allokation, unabhaengig vom Wrapper nachgebaut:
# menge ueber driver in {menge, menge_preis}, material ueber driver == material.
.basis_series <- function(mdl) {
  D <- mdl$pos$driver
  list(
    menge    = colSums(mdl$base[D %in% c("menge", "menge_preis"), , drop = FALSE]),
    material = colSums(mdl$base[D == "material",                  , drop = FALSE])
  )
}

# ---------------------------------------------------------------------------
# Treue: out$spec == direkter Engine-Lauf build_dists(load_model(WB), est).
# Toleranz 0 (bit-identisch) — die UI reicht die Spezifikation unveraendert durch.
# ---------------------------------------------------------------------------
test_that("out$spec ist identisch mit dem direkten build_dists-Lauf der Engine (Treue)", {
  m   <- load_model(WB)
  ser <- .basis_series(m)
  est <- list(menge = estimate_cv_harmonic(ser$menge), material = estimate_cv_harmonic(ser$material))
  ref <- build_dists(m, est)

  out <- phase_verteilungen(scenario_from_workbook(WB))
  expect_equal(out$spec, ref, tolerance = 0)
})

# ---------------------------------------------------------------------------
# cv_est folgt der dokumentierten Basis-Allokation (menge/material ueber driver).
# Hier ueber estimate_cv_harmonic (Treue der Allokation); die MUTATIONSFESTE,
# von estimate_cv_harmonic unabhaengige Nachrechnung steht im naechsten Test.
# ---------------------------------------------------------------------------
test_that("out$cv_est folgt der Basis-Allokation menge={menge,menge_preis}, material={material}", {
  m   <- load_model(WB)
  ser <- .basis_series(m)
  out <- phase_verteilungen(scenario_from_workbook(WB))

  expect_true(all(c("menge", "material") %in% names(out$cv_est)))
  expect_equal(unname(out$cv_est$menge),    estimate_cv_harmonic(ser$menge),    tolerance = 1e-12)
  expect_equal(unname(out$cv_est$material), estimate_cv_harmonic(ser$material), tolerance = 1e-12)

  # m_leb (grp=material, driver=menge) gehoert in die MENGEN-Basis, nicht in die Material-Basis.
  D          <- m$pos$driver
  menge_rows <- which(D %in% c("menge", "menge_preis"))
  mat_rows   <- which(D == "material")
  expect_length(intersect(menge_rows, mat_rows), 0)                 # die Basen sind disjunkt
  expect_true(any(m$pos$grp[menge_rows] == "material"))             # m_leb (grp=material) liegt in der MENGEN-Basis
  expect_false(any(m$pos$grp[mat_rows] == "material" & D[mat_rows] == "menge"))  # nichts driver=menge in der Material-Basis
})

# ---------------------------------------------------------------------------
# MUTATIONSFEST: aleatorische CV der Menge unabhaengig nachgerechnet.
# Harmonische Saisonregression (K=2) via lm.fit auf log(Mengenbasis), sd der
# Residuen — OHNE estimate_cv_harmonic aufzurufen. Eine Mutation der
# Schaetzfunktion (z. B. K, Regressoren, sd->var) bricht genau hier.
# ---------------------------------------------------------------------------
test_that("cv_est$menge stimmt mit unabhaengiger lm.fit-Saisonregression ueberein (mutationsfest)", {
  m <- load_model(WB)
  D <- m$pos$driver
  s <- colSums(m$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  n <- length(s); t <- seq_len(n)
  X <- matrix(1, n, 1)
  for (k in 1:2) X <- cbind(X, sin(2 * pi * k * t / 12), cos(2 * pi * k * t / 12))
  soll <- sd(lm.fit(X, log(s))$residuals)

  out <- phase_verteilungen(scenario_from_workbook(WB))
  expect_equal(unname(out$cv_est$menge), soll, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Engine-Vertrag Spec-Zusammensetzung: jeder Treiber hat eine Annahme im Modell
# UND eine Zeile in Blatt 06_Verteilungen; sein mode ist genau die Jahresannahme.
# ---------------------------------------------------------------------------
test_that("Spec-Abdeckung: jeder Treiber steht in assume und in Blatt 06, mode == assume", {
  m    <- load_model(WB)
  dr06 <- as.character(m$dists_raw[[1]])
  out  <- phase_verteilungen(scenario_from_workbook(WB))

  expect_gt(length(out$spec), 0)
  expect_true(all(names(out$spec) %in% names(m$assume)))   # Annahme vorhanden
  expect_true(all(names(out$spec) %in% dr06))              # Zeile in Blatt 06 vorhanden
  for (d in names(out$spec))
    expect_equal(out$spec[[d]]$mode, m$assume[[d]], tolerance = 0)   # mode = Punktannahme
})

# ---------------------------------------------------------------------------
# PERT-Randvertrag der eingefrorenen Engine: qpert(0)=min, qpert(1)=max je Treiber.
# ---------------------------------------------------------------------------
test_that("PERT-Randvertrag: qpert(0)=min und qpert(1)=max fuer jeden Treiber", {
  out <- phase_verteilungen(scenario_from_workbook(WB))
  for (d in names(out$spec)) {
    s <- out$spec[[d]]
    expect_equal(qpert(0, s$min, s$mode, s$max), s$min, tolerance = 1e-12)
    expect_equal(qpert(1, s$min, s$mode, s$max), s$max, tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
# Quantil-Monotonie der epistemischen Bandbreite: P5 <= P50 <= P95 je Treiber.
# ---------------------------------------------------------------------------
test_that("Quantil-Monotonie P5 <= P50 <= P95 je Treiber", {
  out <- phase_verteilungen(scenario_from_workbook(WB))
  for (d in names(out$spec)) {
    s <- out$spec[[d]]
    q <- qpert(c(.05, .5, .95), s$min, s$mode, s$max)
    expect_true(all(diff(q) >= 0))                          # nichtfallend
    expect_true(q[1] >= s$min - 1e-12 && q[3] <= s$max + 1e-12)
  }
})

# ---------------------------------------------------------------------------
# Treiber-Tabelle: eine Zeile je Treiber, Pflichtspalten, Quelle in {geschaetzt, Vorgabe}.
# ---------------------------------------------------------------------------
test_that("Treiber-Tabelle hat eine Zeile je Treiber und die erwarteten Spalten", {
  out <- phase_verteilungen(scenario_from_workbook(WB))
  expect_true(is.data.frame(out$table))
  expect_equal(nrow(out$table), length(out$spec))
  expect_true(all(c("Treiber", "Mode", "P5", "P50", "P95", "CV", "Quelle") %in% names(out$table)))
  expect_setequal(as.character(out$table$Treiber), names(out$spec))
  expect_true(all(out$spec[[1]]$cv_source %in% c("geschaetzt", "Vorgabe")))  # smoke
  qs <- vapply(out$spec, function(s) s$cv_source, character(1))
  expect_true(all(qs %in% c("geschaetzt", "Vorgabe")))
})

# ---------------------------------------------------------------------------
# Diagnose-Zeile spiegelt die Spec konsistent (Kennzahlen des Schirms).
# ---------------------------------------------------------------------------
test_that("diagnostics sind konsistent mit spec und Historie", {
  m   <- load_model(WB)
  out <- phase_verteilungen(scenario_from_workbook(WB))
  dg  <- out$diagnostics

  expect_equal(dg$n_treiber, length(out$spec))
  expect_equal(dg$historie_monate, ncol(m$base))
  # Badge belastbar = Historie >= 24 Monate (Fixture: 12 -> FALSE).
  expect_true(is.logical(dg$cv_belastbar))
  expect_equal(dg$cv_belastbar, dg$historie_monate >= 24)
  # geschaetzt/Vorgabe zerlegen die Treiber vollstaendig, entsprechend cv_source.
  src <- vapply(out$spec, function(s) s$cv_source, character(1))
  expect_equal(dg$cv_geschaetzt_n, sum(src == "geschaetzt"))
  expect_equal(dg$cv_vorgabe_n,    sum(src == "Vorgabe"))
  expect_equal(dg$cv_geschaetzt_n + dg$cv_vorgabe_n, dg$n_treiber)
  expect_true(is.character(dg$basis_text) && nzchar(dg$basis_text))
})

# ---------------------------------------------------------------------------
# Artefakt-Vertrag fuer Phase 5/6: verteilungen_spec.rds liest sich bit-identisch
# zum Ergebnisobjekt zurueck und liegt im Szenario-Workspace.
# ---------------------------------------------------------------------------
test_that("spec_path: verteilungen_spec.rds liest sich identisch zu out$spec zurueck", {
  sc  <- scenario_from_workbook(WB)
  out <- phase_verteilungen(sc)
  expect_false(is.na(out$spec_path))
  expect_true(file.exists(out$spec_path))
  expect_identical(readRDS(out$spec_path), out$spec)
  expect_true(startsWith(normalizePath(out$spec_path), normalizePath(sc$workspace)))
})

# ---------------------------------------------------------------------------
# Fehlerfall: fehlendes Blatt 06_Verteilungen -> sauberer, benannter Fehler.
# ---------------------------------------------------------------------------
test_that("fehlendes Blatt 06_Verteilungen fuehrt zu einem sauberen Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$dists_raw <- NULL                                 # optionales Blatt fehlt
  expect_error(phase_verteilungen(sc), regexp = "06", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Fehlerfall: Blatt 06 vorhanden, aber ohne Treiberzeilen -> sauberer Fehler
# (build_dists liefert eine leere Spec; der Wrapper stoppt statt leer zu liefern).
# ---------------------------------------------------------------------------
test_that("leeres Blatt 06 ohne Treiber fuehrt zu einem sauberen Fehler", {
  sc <- scenario_from_workbook(WB)
  sc$mdl$dists_raw <- sc$mdl$dists_raw[0, , drop = FALSE]  # 0 Treiberzeilen
  expect_error(phase_verteilungen(sc))
})

# ---------------------------------------------------------------------------
# Plot-Fallback: Pfad im Workspace, sonst NA — nie ein harter Abbruch ohne
# Grafikgeraet (sauberer Headless-Fallback von .plot_verteilungen).
# ---------------------------------------------------------------------------
test_that("Verteilungs-Plot wird in den Szenario-Workspace geschrieben (sofern Grafikgeraet)", {
  sc  <- scenario_from_workbook(WB)
  out <- phase_verteilungen(sc)
  if (!is.na(out$plot_path)) {
    expect_true(file.exists(out$plot_path))
    expect_gt(file.info(out$plot_path)$size, 0)
    expect_true(startsWith(normalizePath(out$plot_path), normalizePath(sc$workspace)))
  } else {
    # Headless-Vertrag: der Fallback ist genau NA (nicht NULL, nicht "") —
    # echte Assertion statt succeed() (Red-Team-Befund: unfailbarer Zweig).
    expect_true(is.na(out$plot_path))
  }
})

# ---------------------------------------------------------------------------
# Nicht-Szenario wird zurueckgewiesen (Klassenvertrag wie die uebrigen Phasen).
# ---------------------------------------------------------------------------
test_that("phase_verteilungen weist ein Nicht-Szenario zurueck", {
  expect_error(phase_verteilungen(list(mdl = NULL)), "liqui_scenario")
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
# shinytest2: faehrt den verdrahteten Verteilungs-Schirm headless und prueft,
# dass Diagnose- und Treiber-Tabelle konkrete Werte aus dem Engine-Wrapper
# anzeigen (UI-Control-Gate: wo es ein Control gibt, zusaetzlich ein
# shinytest2-Test).
# OPTIONALES E2E-Gate: fehlt shinytest2/chromote/Chrome, wird SICHTBAR
# uebersprungen (skip mit konkreter Begruendung) — kein stiller Pass, aber
# auch kein harter Abbruch. NOT_CRAN erzwingt der Runner (tests/run_all.R)
# vor test_dir; ohne die Variable skippte AppDriver$new() still als "On CRAN".
# ----------------------------------------------------------------------------
test_that("shinytest2: Verteilungs-Schirm zeigt Diagnose- und Treiber-Engine-Werte", {
  for (pkg in c("shinytest2", "chromote"))
    skip_if_not_installed(pkg)
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome),
          "E2E uebersprungen: kein Chrome/Chromium (shinytest2-Infrastruktur noch nicht eingerichtet)")

  # Engine-Referenz (gleicher Wrapper, gleiche Mappe wie die App).
  ref <- phase_verteilungen(scenario_from_workbook(WB))
  dg  <- ref$diagnostics

  app <- shinytest2::AppDriver$new(
    file.path(.root, "app"), name = "vert",
    load_timeout = 60000, timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("demo")
  app$set_inputs(phase = "vert")
  app$wait_for_idle(timeout = 60000)

  # Diagnose spiegelt die Spec-Kennzahlen (Treiberzahl, Historie, CV-Zerlegung).
  .aktiviere_reiter(app, "Verteilungen je Treiber")
  diag <- .output_text(app$get_value(output = "vert_diag"))
  expect_match(diag, sprintf("%d", dg$n_treiber),       fixed = TRUE)
  expect_match(diag, sprintf("%d", dg$historie_monate), fixed = TRUE)

  # Treiber-Tabelle nennt jeden Treiber der Engine-Spec (verdeckter Reiter:
  # suspendiert, ohne Aktivierung NULL).
  .aktiviere_reiter(app, "Treiber-Tabelle")
  tbl <- .output_text(app$get_value(output = "vert_tbl"))
  for (d in ref$table$Treiber)
    expect_match(tbl, d, fixed = TRUE)

  # Vertrag/Rueckabstimmung zeigt die dokumentierte CV-Basis-Allokation.
  .aktiviere_reiter(app, "Spezifikation & Vertrag")
  recon <- .output_text(app$get_value(output = "vert_recon"))
  expect_match(recon, dg$basis_text, fixed = TRUE)
})
