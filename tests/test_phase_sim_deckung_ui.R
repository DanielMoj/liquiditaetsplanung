# ============================================================================
#  tests/test_phase_sim_deckung_ui.R  —  UI-Bindungs-Orakel fuer den Deckungs-
#  Schirm (Tiefpunkt & Deckung)
#
#  Zusatz-Orakel (rot_zuerst: false) zur Spezifikation test_phase_sim_deckung.R:
#  jene prueft den NUMERISCHEN Projektions-Vertrag von phase_sim_deckung();
#  dieses prueft die SEMANTISCHE Bindung der Anzeige — output$deckung_recon in
#  app/app.R legt jede Kennzahl-ID auf GENAU ihr Quellfeld der gemeinsamen
#  Projektion deckung_from_out(sim_rv$out, rv$kk_limit): deckung-trough-median/
#  -p05/-p95 (fmt), deckung-p-breach/-p-solvent (eine Nachkommastelle) und das
#  InsO-Badge, dessen Klasse an insO_erfuellt haengt. Ein Anzeige-Tausch (z. B.
#  p-breach zoege aus P_solvent_pct, Median aus P95) oder ein hartcodiertes
#  Badge faellt damit zwingend rot — dieselbe Luecke, die die Schwester-Orakel
#  test_phase_sim_ui.R (sim_diag) und test_phase_sim_monate_ui.R
#  (sim_monate_tbl) schliessen (Verifier prueft Kennzahl-IDs nur auf Format/
#  Befuelltheit, nicht auf die Bindung DOM -> Quellfeld).
#
#  testServer-Basis (kein Browser, keine Monte-Carlo): app/app.R wird in-Process
#  gesourct, sim_rv$out mit einem paarweise unverwechselbaren Stub im
#  phase_simulation-Vertrag belegt (trough-Vektor + dazu KONSISTENTE metrics wie
#  .mk_out der Spec; bewusst KEINE Tier-1-Ankerwerte; plot_path = NA_character_
#  bedient den validate-Fallback von sim_plot, weil die MockShinySession beim
#  Flush alle Outputs rechnet), rv$kk_limit mit der Modell-Schwelle, die
#  run_forecast beim Mappe-Laden sichert. Die Erwartungen werden UNABHAENGIG aus
#  dem Stub-trough rekonstruiert (median/quantile/mean wie die Spec), nicht aus
#  deckung_from_out — sonst pruefte der Test die Funktion gegen sich selbst.
#
#  Nebenlaeufigkeits-Hygiene wie in den Schwester-Orakeln: das Sourcen von
#  app/app.R startet bei HAS_ASYNC mirai::daemons(1); der onStop-Handler feuert
#  unter testServer nie. Die Daemons werden deterministisch geschlossen (auch im
#  Fehlerpfad) — die Suite darf keinen R-Prozess leaken.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# xml2 (DOM-Parse des deckung_recon-HTML) ist reines Test-Tooling, keine App-
# Abhaengigkeit: fehlt es, wird der Test SICHTBAR uebersprungen (skip_if mit
# Begruendung, im test_that unten) — kein harter stop(), der den DoD bricht, und
# kein stiller Skip. Die beim Sourcen der App gestarteten mirai-Daemons
# schliesst .daemons_zu() am Dateiende auch im Skip-Pfad.

# Beim Sourcen gestartete mirai-Daemons schliessen (Suite-Invariante: kein Leak).
.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

# App in-Process laden. chdir=TRUE laesst find_root() der App aus app/ heraus die
# Repo-Wurzel bestimmen — wie bei shiny::runApp("app").
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()  # ein halb gesourctes app.R darf keinen Daemon zuruecklassen
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

.kz_node <- function(doc, id) xml2::xml_find_first(doc, sprintf("//*[@id='%s']", id))
.kz_text <- function(doc, id) {
  n <- .kz_node(doc, id)
  if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
}

# Stub im phase_simulation-Ergebnisvertrag, wie .mk_out der Spec: aus einem
# Tiefpunkt-Vektor `trough` und `kk_limit` werden out$trough + dazu KONSISTENTE
# metrics gebaut (die Projektion rechnet aus trough; ein konsistenter Stub haelt
# beide moeglichen Quellen gleich). KEINE Engine, kein Seed-Lauf — synthetisch.
.mk_sim_out <- function(trough, kk_limit) {
  Pb <- mean(trough < -kk_limit)
  list(
    phase   = "simulation",
    trough  = trough,
    B       = matrix(trough, ncol = 1L, dimnames = list(NULL, "M1")),
    endbestand_path = "stub-workspace/endbestand_deckung_orakel.rds",
    plot_path       = NA_character_,  # sim_plot-Vollflush: validate-Fallback statt file.exists-Wurf
    metrics = list(
      S             = length(trough),
      seed          = 4711L,
      H             = 1L,
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95)),
      P_breach      = Pb,
      P_breach_pct  = 100 * Pb,
      P_solvent     = 1 - Pb,
      P_solvent_pct = 100 * (1 - Pb),
      plan_trough   = min(trough)
    )
  )
}

test_that("deckung_recon bindet jede Kennzahl-ID an GENAU ihr Quellfeld; Badge-Klasse folgt insO_erfuellt", {
  # xml2 ist reines Test-Tooling (DOM-Parse des gerenderten HTML) — fehlt es,
  # sichtbar ueberspringen statt den Build zu brechen.
  skip_if(!requireNamespace("xml2", quietly = TRUE),
          "Paket 'xml2' fehlt (DOM-Parse des deckung_recon-HTML): install.packages(\"xml2\")")

  kk <- 250000
  # Fall A (UEBER der Schwelle): 1 von 4 Pfaden reisst die Linie -> P_breach
  # 25.0 %, P_solvent 75.0 % -> insO_erfuellt TRUE (Erfolgs-Badge). Median
  # (-25.000), P05 (-370.000), P95 (447.500), Limit (250.000) und die beiden
  # Prozentwerte sind paarweise unverwechselbar; KEINE Tier-1-Werte.
  trough_a <- c(-400000, -200000, 150000, 500000)
  # Fall B (UNTER der Schwelle): 3 von 4 Pfaden reissen -> P_solvent 25.0 %
  # -> insO_erfuellt FALSE (Gefahr-Badge). Beweist die BINDUNG des Badges an
  # insO_erfuellt (kein hartcodierter Status, keine invertierte Klasse).
  trough_b <- c(-900000, -800000, -700000, 100000)

  # fmt aus der Aufloesungskette der gesourcten App (app-Env -> Kernel), nicht
  # nachgebaut — die Erwartung ist exakt die Darstellung, die die App rendert.
  fmt_app <- get("fmt", envir = .app_env)

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> der
                                           # Auto-Sim-Trigger bleibt inert.
    rv$kk_limit <- kk                      # Naht von run_forecast (Mappe-Laden)
    sim_rv$out  <- .mk_sim_out(trough_a, kk)
    session$flushReact()

    ui   <- output$deckung_recon           # renderUI: list(html, deps)
    html <- if (is.list(ui)) ui$html else ui
    doc  <- xml2::read_html(as.character(html))

    # Erwartungen unabhaengig aus dem Stub-trough rekonstruiert (Spec-Idiom):
    Pb_a <- mean(trough_a < -kk)
    expect_identical(.kz_text(doc, "deckung-trough-median"),
                     fmt_app(unname(median(trough_a))))          # median, nicht P05/P95
    expect_identical(.kz_text(doc, "deckung-trough-p05"),
                     fmt_app(unname(quantile(trough_a, .05))))   # 5%-Quantil, nicht 95%
    expect_identical(.kz_text(doc, "deckung-trough-p95"),
                     fmt_app(unname(quantile(trough_a, .95))))   # 95%-Quantil, nicht 5%
    expect_identical(.kz_text(doc, "deckung-p-breach"),
                     sprintf("%.1f", 100 * Pb_a))                # P_breach_pct, nicht P_solvent_pct
    expect_identical(.kz_text(doc, "deckung-p-solvent"),
                     sprintf("%.1f", 100 * (1 - Pb_a)))          # P_solvent_pct, nicht P_breach_pct

    # Badge: vorhanden + Klasse folgt insO_erfuellt (P_solvent 0.75 > 0.5).
    b <- .kz_node(doc, "deckung-badge-inso")
    expect_false(inherits(b, "xml_missing"),
                 label = "Badge 'deckung-badge-inso' fehlt im deckung_recon-HTML (Fall A)")
    expect_match(xml2::xml_attr(b, "class"), "text-bg-success", fixed = TRUE,
                 label = "class von Badge 'deckung-badge-inso' (Fall A, insO_erfuellt=TRUE)")

    # Fall B: Stub unter der Schwelle -> Re-Render; die Kennzahl folgt dem neuen
    # trough und das Badge kippt in die Gefahr-Klasse.
    sim_rv$out <- .mk_sim_out(trough_b, kk)
    session$flushReact()

    ui   <- output$deckung_recon
    html <- if (is.list(ui)) ui$html else ui
    doc  <- xml2::read_html(as.character(html))

    Pb_b <- mean(trough_b < -kk)
    expect_identical(.kz_text(doc, "deckung-p-solvent"),
                     sprintf("%.1f", 100 * (1 - Pb_b)))
    b <- .kz_node(doc, "deckung-badge-inso")
    expect_false(inherits(b, "xml_missing"),
                 label = "Badge 'deckung-badge-inso' fehlt im deckung_recon-HTML (Fall B)")
    expect_match(xml2::xml_attr(b, "class"), "text-bg-danger", fixed = TRUE,
                 label = "class von Badge 'deckung-badge-inso' (Fall B, insO_erfuellt=FALSE)")
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
