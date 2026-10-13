# ============================================================================
#  tests/test_phase_sim_monate_ui.R  —  UI-Bindungs-Orakel fuer den Monats-Schirm
#
#  Zusatz-Orakel (rot_zuerst: false) zur Spezifikation test_phase_sim_monate.R:
#  jene prueft den NUMERISCHEN Projektions-Vertrag von phase_sim_monate(); dieses
#  prueft die SEMANTISCHE Bindung der Anzeige — output$sim_monate_tbl in app/app.R
#  legt je Monat GENAU die richtige fan-Zeile in die richtige Spalte (P05<-5%,
#  P50<-50%, P95<-95%) und den durchgereichten P_under als Prozent. Ein Spalten-
#  tausch im Render (z. B. P05 zoege aus fan["95%",]) faellt damit zwingend rot —
#  dieselbe Luecke, die das Schwester-Orakel test_phase_sim_ui.R fuer sim_diag
#  schliesst (Verifier prueft Kennzahl-IDs nur auf Format/Befuelltheit, nicht auf
#  die Bindung DOM -> Quellfeld).
#
#  testServer-Basis (kein Browser, keine Monte-Carlo): app/app.R wird in-Process
#  gesourct, sim_rv$out mit einem paarweise unverwechselbaren Stub belegt (bewusst
#  KEIN Tier-1-Ankerwert) und die gerenderte Tabelle Zelle fuer Zelle gegen die
#  App-eigene Formatierung (fmt()/sprintf) geprueft.
#
#  Nebenlaeufigkeits-Hygiene wie im Schwester-Orakel: das Sourcen von app/app.R
#  startet bei HAS_ASYNC mirai::daemons(1); der onStop-Handler feuert unter
#  testServer nie. Die Daemons werden deterministisch geschlossen (auch im
#  Fehlerpfad) — die Suite darf keinen R-Prozess leaken.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# xml2 (DOM-Parse des sim_monate_tbl-HTML) ist reines Test-Tooling, keine App-
# Abhaengigkeit: fehlt es, wird der Test SICHTBAR uebersprungen (skip_if mit
# Begruendung, im test_that unten) — kein harter stop(), der den DoD bricht, und
# kein stiller Skip. Die beim Sourcen der App gestarteten mirai-Daemons schliesst
# .daemons_zu() am Dateiende auch im Skip-Pfad — die Suite leakt keinen Prozess.

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

# Die tbody-Zeilen der gerenderten Tabelle als Liste von Zell-Textvektoren.
.tbody_rows <- function(html) {
  doc  <- xml2::read_html(as.character(html))
  rows <- xml2::xml_find_all(doc, "//tbody/tr")
  lapply(rows, function(r) trimws(xml2::xml_text(xml2::xml_find_all(r, ".//td"))))
}

test_that("sim_monate_tbl bindet je Monat P05/P50/P95 an GENAU ihre fan-Zeile (kein Spaltentausch)", {
  # xml2 ist reines Test-Tooling (DOM-Parse des gerenderten HTML) — fehlt es,
  # sichtbar ueberspringen statt den Build zu brechen.
  skip_if(!requireNamespace("xml2", quietly = TRUE),
          "Paket 'xml2' fehlt (DOM-Parse des sim_monate_tbl-HTML): install.packages(\"xml2\")")

  # Stub: fan mit paarweise verschiedenen, klar geordneten Zeilen (5% < 50% < 95%)
  # UND je Monat unterschiedlichen Werten, damit ein Zeilen-/Spaltentausch im
  # Render zwingend eine falsche Zelle erzeugt. P_under paarweise verschieden.
  H   <- 3L
  fan <- rbind(
    "5%"  = c(-500000, -400000, -300000),
    "25%" = c(-200000, -100000,       0),
    "50%" = c( 100000,  200000,  300000),
    "75%" = c( 500000,  600000,  700000),
    "95%" = c( 900000,  850000,  800000)
  )
  colnames(fan) <- paste0("M", seq_len(H))
  P_under <- c(0.12, 0.34, 0.00)

  stub <- list(
    metrics = list(H = H),
    fan     = fan,
    P_under = P_under
  )

  # fmt aus der Aufloesungskette der gesourcten App (app-Env -> Kernel), nicht
  # nachgebaut — die Erwartung ist exakt die Darstellung, die die App rendert.
  fmt_app <- get("fmt", envir = .app_env)

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> der
                                           # Auto-Sim-Trigger bleibt inert.
    sim_rv$out <- stub
    session$flushReact()

    tbl  <- output$sim_monate_tbl
    html <- if (is.list(tbl)) tbl$html else tbl
    rows <- .tbody_rows(html)

    expect_equal(length(rows), H)
    for (i in seq_len(H)) {
      expect_identical(
        rows[[i]],
        c(paste0("M", i),
          fmt_app(fan["5%",  i]),
          fmt_app(fan["50%", i]),
          fmt_app(fan["95%", i]),
          sprintf("%.0f%%", 100 * P_under[i])),
        label = sprintf("Zeile M%d der Monats-Tabelle", i)
      )
    }
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
