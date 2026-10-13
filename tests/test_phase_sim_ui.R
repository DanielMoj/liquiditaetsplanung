# ============================================================================
#  tests/test_phase_sim_ui.R  —  UI-Bindungs-Orakel fuer den Sim-Schirm
#
#  Gate-Ueberlebender aus dem ersten Mutgate-Lauf auf app/app.R: ein Anzeige-
#  Tausch in output$sim_diag (kz("sim-s", ... m$H) / kz("sim-horizont", ... m$S))
#  ueberlebte, weil der Verifier die Kennzahl-IDs nur auf Format/Befuelltheit
#  prueft — die SEMANTISCHE Bindung DOM-ID -> Metrik-Feld war ungeprueft.
#
#  Dieses Orakel prueft die Bindung direkt auf shiny::testServer-Basis (kein
#  Browser, keine Monte-Carlo): app/app.R wird in-Process gesourct (das
#  shinyApp() am Dateiende gibt nur ein Objekt zurueck, gestartet wird nichts),
#  sim_rv$out mit einem paarweise unverwechselbaren Stub belegt (bewusst KEIN
#  Tier-1-Ankerwert) und das gerenderte sim_diag-HTML per xml2 je DOM-ID gegen
#  GENAU sein Quellfeld verglichen. Ein Tausch zweier Bindungen (z. B.
#  sim-s <-> sim-horizont) faellt damit zwingend rot.
#
#  Nebenlaeufigkeits-Hygiene: das Sourcen von app/app.R startet bei HAS_ASYNC
#  mirai::daemons(1); der onStop-Handler der App feuert unter testServer nie.
#  Die Daemons werden hier deterministisch geschlossen (Fehlerpfad des Sourcens
#  eingeschlossen) — die Suite darf keinen R-Prozess leaken.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Harte Voraussetzung VOR dem Sourcen der App pruefen (das Sourcen startet ggf.
# schon Daemons): fehlt xml2, bricht der Test benannt ab — kein stiller Skip.
if (!requireNamespace("xml2", quietly = TRUE))
  stop("Paket 'xml2' fehlt (DOM-Parse des sim_diag-HTML): install.packages(\"xml2\")")

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

.kz_node <- function(doc, id) xml2::xml_find_first(doc, sprintf("//*[@id='%s']", id))
.kz_text <- function(doc, id) {
  n <- .kz_node(doc, id)
  if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
}

test_that("sim_diag bindet jede Kennzahl-ID an GENAU ihr Metrik-Feld (kein Anzeige-Tausch)", {
  # Stub mit paarweise verschiedenen, unverwechselbaren Werten — traegt ALLE von
  # output$sim_diag referenzierten Felder (metrics: S/H/seed/plan_trough,
  # diagnostics: n_treiber/deterministisch/stuetzen_ok, endbestand_path).
  # plot_path = NA bedient den validate-Fallback von sim_plot, weil die
  # MockShinySession beim Flush alle Outputs rechnet.
  stub <- list(
    metrics         = list(S = 777, H = 13, seed = 4242, plan_trough = -123456),
    diagnostics     = list(n_treiber = 9, deterministisch = TRUE, stuetzen_ok = TRUE),
    endbestand_path = "stub-workspace/endbestand_ui_orakel.rds",
    plot_path       = NA_character_
  )
  # fmt aus der Aufloesungskette der gesourcten App (app-Env -> Kernel), nicht
  # nachgebaut — die Erwartung ist exakt die Darstellung, die die App rendert.
  fmt_app <- get("fmt", envir = .app_env)

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # UI-Default der radioButtons;
                                           # rv$wbpath bleibt NULL -> der
                                           # Auto-Sim-Trigger bleibt inert.
    sim_rv$out <- stub
    session$flushReact()

    ui   <- output$sim_diag                # renderUI: list(html, deps)
    html <- if (is.list(ui)) ui$html else ui
    doc  <- xml2::read_html(as.character(html))

    # Semantische Bindung ID -> Stub-Feld (nicht bloss Format/Befuelltheit):
    expect_identical(.kz_text(doc, "sim-s"),        "777")  # m$S, nicht m$H
    expect_identical(.kz_text(doc, "sim-horizont"), "13")   # m$H, nicht m$S
    expect_identical(.kz_text(doc, "sim-seed"),     "4242")
    expect_identical(.kz_text(doc, "sim-treiber"),  "9")
    expect_identical(.kz_text(doc, "sim-plan-tiefpunkt"), fmt_app(-123456))
    expect_identical(.kz_text(doc, "sim-endbestand-pfad"), stub$endbestand_path)

    # Badges: vorhanden und bei TRUE in der Erfolgsklasse.
    for (id in c("sim-determinismus", "sim-stuetzen")) {
      n <- .kz_node(doc, id)
      expect_false(inherits(n, "xml_missing"),
                   label = sprintf("Badge '%s' fehlt im sim_diag-HTML", id))
      expect_match(xml2::xml_attr(n, "class"), "text-bg-success", fixed = TRUE,
                   label = sprintf("class von Badge '%s'", id))
    }
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
