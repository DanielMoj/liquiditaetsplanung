# ============================================================================
#  app/app.R  —  Liqui-App: bslib-Oberflaeche ueber den Phasenfunktionen in R/
#
#  Die Rechen-Engine (kernel/) wird unveraendert aufgerufen. Alle Schirme des
#  Menues (PHASEN) sind verdrahtet; urspruenglich enthielt die Datei nur Phase 1.
#  Start aus der Repo-Wurzel:  R -e 'shiny::runApp("app")'
# ============================================================================
suppressMessages({ library(shiny); library(bslib) })

# --- Repo-Wurzel finden (funktioniert aus app/ wie aus der Wurzel) ----------
find_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:6) { if (dir.exists(file.path(d, "kernel"))) return(d); d <- dirname(d) }
  getwd()
}
ROOT <- find_root()
source(file.path(ROOT, "kernel", "engine.R"))
source(file.path(ROOT, "kernel", "distributions.R"))
source(file.path(ROOT, "R", "scenario.R"))
source(file.path(ROOT, "R", "phase_forecast.R"))
source(file.path(ROOT, "R", "phase_wochen.R"))
source(file.path(ROOT, "R", "phase_sensitivitaet.R"))
source(file.path(ROOT, "R", "phase_verteilungen.R"))
source(file.path(ROOT, "R", "phase_korrelation.R"))
source(file.path(ROOT, "R", "phase_kopula.R"))
source(file.path(ROOT, "R", "phase_simulation.R"))
source(file.path(ROOT, "R", "phase_sim_monate.R"))
source(file.path(ROOT, "R", "phase_sim_deckung.R"))
source(file.path(ROOT, "R", "phase_varianzzerlegung.R"))
source(file.path(ROOT, "R", "compare_scenarios.R"))
source(file.path(ROOT, "R", "export.R"))
source(file.path(ROOT, "R", "z3.R"))
source(file.path(ROOT, "R", "verfahrensdoku.R"))
source(file.path(ROOT, "R", "new_mandate.R"))
source(file.path(ROOT, "R", "massnahmen.R"))
source(file.path(ROOT, "R", "manifest.R"))
source(file.path(ROOT, "R", "manifest_anbindung.R"))
source(file.path(ROOT, "R", "herkunft.R"))

DEMO_WB <- file.path(ROOT, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# --- Die Packer-Naht des GoBD-Z3-Downloads lebt in R/z3.R (oben via
# source(R/z3.R) geladen): .z3_zip zippt das Buendel per Handoff-Vorgabe primaer
# via utils::zip (Basis-R) und faellt nur dann auf den Basis-R-Store-ZIP
# .z3_zip_store zurueck, wenn kein externes zip-Binary ein gueltiges Archiv
# schreibt. Beide sind reine Wrapper (nur digest fuer die CRC-32 im Fallback) und
# gehoeren laut CLAUDE.md in R/, nicht in die App-Huelle. Die Verschiebung
# entlastet zugleich die DOD-Suite: der Roundtrip-Test erreicht den Packer ueber
# source(R/z3.R) statt das ganze app/app.R nur dafuer zu sourcen. Der
# z3-downloadHandler unten ruft .z3_zip auf (Namensaufloesung ueber die Quelle
# am Kopf).

# --- Hintergrund-Worker fuer die schwere Monte-Carlo (Phase 6/7) -------------
# CLAUDE.md / IMPLEMENTATION_RULES §Nebenlaeufigkeit: die genestete MC laeuft im
# mirai-Worker mit Fortschritt + Abbrechen, NIE synchron im UI-Thread. Fehlt die
# Worker-Infrastruktur (mirai/promises nicht installiert), degradiert der Schirm
# SICHTBAR mit Begruendung — er rechnet dann bewusst NICHT synchron (kein stiller
# Fallback, der die Regel unterlaeuft; E2E-/Worker-Infra wird separat eingerichtet).
HAS_ASYNC <- requireNamespace("mirai", quietly = TRUE) && requireNamespace("promises", quietly = TRUE)
if (HAS_ASYNC) {
  mirai::daemons(1)
  # Kernel + Wrapper EINMAL in den Daemon laden (statt je Lauf) — jeder mirai()-
  # Aufruf ruft danach nur noch phase_simulation() im Worker auf.
  mirai::everywhere({
    source(file.path(ROOT, "kernel", "engine.R"))
    source(file.path(ROOT, "kernel", "distributions.R"))
    source(file.path(ROOT, "R", "scenario.R"))
    source(file.path(ROOT, "R", "phase_simulation.R"))
    source(file.path(ROOT, "R", "phase_varianzzerlegung.R"))
    source(file.path(ROOT, "R", "compare_scenarios.R"))
    source(file.path(ROOT, "R", "massnahmen.R"))
    # GoBD-Schicht (phase-12-runmanifest-anbindung): der Sim-Lauf laeuft im
    # Worker ueber simulation_mit_manifest; der Hook braucht phase_forecast
    # (IKS-Vorstufe) und den Manifest-Kern.
    source(file.path(ROOT, "R", "phase_forecast.R"))
    source(file.path(ROOT, "R", "manifest.R"))
    source(file.path(ROOT, "R", "manifest_anbindung.R"))
  }, ROOT = ROOT)
  # Daemons beim App-Stop sauber schliessen (Verifier startet/stoppt die App).
  onStop(function() try(mirai::daemons(0), silent = TRUE))
}

# Phasen — Menue-Label = Anzeigename, Wert = Schirm-Schluessel in phase_body.
PHASEN <- c("Forecast" = "forecast", "Wochen" = "wochen", "Sensitivitaet" = "sens",
            "Verteilungen" = "vert", "Korrelation" = "korrelation", "Kopula" = "kopula",
            "Simulation" = "sim", "Endbestand je Monat" = "sim_monate",
            "Tiefpunkt & Deckung" = "sim_deckung", "Varianzzerlegung" = "varianzzerlegung",
            "Szenario-Vergleich" = "vergleich", "Massnahmen" = "massnahmen")

badge   <- function(ok, txt, id = NULL) span(id = id, class = paste("badge", if (isTRUE(ok)) "text-bg-success" else "text-bg-danger"), txt)
fmt_tbl <- function(m) { d <- as.data.frame(m); d[] <- lapply(d, fmt); d }
# Kennzahl-Span: feste, sprechende DOM-ID je angezeigter Kennzahl, damit Tests
# (Playwright/shinytest2) Werte aus dem DOM lesen statt aus Pixeln. .noWS haelt
# den gerenderten Text zeichenidentisch (keine Whitespace-Naehte um den Span).
kz <- function(id, x) span(id = id, x, .noWS = "outside")

# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Liquiditaetsplanung — Krankenhaus",
  # Theme: die Kernwerte doppeln die CSS-Tokens aus app/www/custom.css
  # (:root --ui-*), weil Sass zur Buildzeit keine Runtime-CSS-Variablen lesen
  # kann. Figtree liegt lokal in app/www/fonts/ (@font-face in custom.css).
  theme = bs_theme(
    version   = 5,
    primary   = "#0A0A0A",  # --ui-primary (Ink)
    bg        = "#F4F7FB",  # --ui-bg (Seitengrund)
    fg        = "#1A1A1A",  # --ui-text
    base_font = font_collection("Figtree", "Helvetica Neue", "Arial", "sans-serif")
  ),
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),

  sidebar = sidebar(
    width = 320,
    card(
      card_header("Mandat"),
      actionButton("demo", "Demo laden (Tier-1)", class = "btn-primary btn-sm"),
      div(class = "small text-muted mt-2", "oder eigene Mappe:"),
      fileInput("wb", NULL, accept = ".xlsx", buttonLabel = "Waehlen", placeholder = "keine Datei"),
      div(class = "small", textOutput("wb_name"))
    ),
    # Export (phase-09-xlsx + phase-09-docx + phase-09-pdf): schirm-los laut
    # Handoff — Sidebar-Downloads statt Phasen-Schirm. Die Container
    # #export-xlsx, #export-docx und #export-pdf umschliessen genau ihren
    # Button (pruefbare DOM-Elemente, keine Zahlwerte); die downloadHandler
    # sammeln die vorhandenen Ergebnisobjekte und werfen bei leerer Sammlung
    # den validate-Hinweis.
    card(
      card_header("Datei -> Export"),
      div(id = "export-xlsx",
          downloadButton("export_xlsx_btn", "Als .xlsx exportieren")),
      div(id = "export-docx", class = "mt-2",
          downloadButton("export_docx_btn", "Als .docx exportieren")),
      div(id = "export-pdf", class = "mt-2",
          downloadButton("export_pdf_btn", "Als .pdf exportieren")),
      # z3-Buendel (phase-13-z3-ui): GoBD-Datentraegerueberlassung als zip
      # (CSV + index.xml, kein PDF/A). Container #export-z3 umschliesst genau
      # den downloadButton (pruefbares DOM-Element, kein Zahlwert).
      div(id = "export-z3", class = "mt-2",
          downloadButton("export_z3_btn", "Als GoBD-Z3 (.zip) exportieren")),
      # Verfahrensdokumentation (phase-15-verfahrensdoku-export): GoBD Rz. 153
      # als .xlsx. Container #export-verfahrensdoku umschliesst genau den
      # downloadButton (pruefbares DOM-Element, kein Zahlwert). BEWUSST OHNE
      # validate/need: die VD ist der Prozedur gewidmet, nicht berechneten
      # Ergebnissen -- der Download ist immer verfuegbar (kein Erst-rechnen-
      # Hinweis, anders als die Ergebnis-Schwestern xlsx/docx/pdf/z3).
      div(id = "export-verfahrensdoku", class = "mt-2",
          downloadButton("export_verfahrensdoku_btn",
                         "Verfahrensdokumentation (.xlsx)"))
    ),
    # Neues Mandat (phase-10-ui): schirm-los laut Handoff — Sidebar-Download
    # statt Phasen-Schirm. Der Container #new-mandate umschliesst Button und
    # Vier-Farb-Legende in Anleitungs-Reihenfolge; Farbcodes und Kurztexte
    # woertlich aus docs/Anleitung_neuer_Mandant.md Abschnitt 2, je Chip
    # steht der Kurztext direkt hinter seinem Farbfeld.
    card(
      card_header("Datei -> Neues Mandat"),
      div(id = "new-mandate",
          downloadButton("new_mandate_btn", "Leere Mandatsmappe (.xlsx)"),
          div(class = "small mt-2 d-flex align-items-center gap-2",
              span(style = "display:inline-block;width:1em;height:1em;flex:none;border:1px solid #999;background:#FFFF00;"),
              "Eingabe, die du setzt"),
          div(class = "small mt-1 d-flex align-items-center gap-2",
              span(style = "display:inline-block;width:1em;height:1em;flex:none;border:1px solid #999;background:#DDEBF7;"),
              "Pflichtdaten, beobachtet/strukturell"),
          div(class = "small mt-1 d-flex align-items-center gap-2",
              span(style = "display:inline-block;width:1em;height:1em;flex:none;border:1px solid #999;background:#D9D9D9;"),
              "von der Engine erzeugt oder strukturell fix"),
          div(class = "small mt-1 d-flex align-items-center gap-2",
              span(style = "display:inline-block;width:1em;height:1em;flex:none;border:1px solid #999;background:#EDE7DD;"),
              "reiner Kontext, von der Engine nicht gelesen"))
    ),
    card(
      card_header("Analyse"),
      radioButtons("phase", NULL, choices = PHASEN, selected = "forecast")
    )
  ),

  uiOutput("kpi_row"),

  uiOutput("phase_body"),

  # Statusleiste: dieselben Konsistenzpruefungen, die die Skripte in die Konsole schreiben.
  card(
    class = "mt-2",
    card_body(class = "py-2", uiOutput("statusbar"))
  )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(out = NULL, wbname = NULL, wbpath = NULL, kk_limit = NULL,
                       assume_namen = NULL)

  run_forecast <- function(path, label) {
    sc <- scenario_from_workbook(path, name = label)
    rv$out      <- phase_forecast(sc)
    # KK-Limit der geladenen Mappe fuer die Deckungs-Projektion (phase-06-
    # deckung): dieselbe Modell-Schwelle (sc$mdl$kk_limit), die auch der
    # Simulationslauf im Worker aus derselben Mappe liest — Anzeige und
    # Wrapper-Vertrag koennen nicht auf verschiedene Limits laufen.
    rv$kk_limit <- sc$mdl$kk_limit
    # Treibernamen der geladenen Mappe fuer das Szenario-Formular (Phase 8):
    # das selectInput bietet genau die Annahmen an, die die Mappe kennt.
    rv$assume_namen <- names(sc$assume)
    rv$wbname   <- label
    rv$wbpath   <- path
  }

  # Wochensicht lazy aus derselben Mappe; nur gerechnet, wenn der Schirm offen ist.
  wochen_out <- reactive({
    req(rv$wbpath, input$phase == "wochen")
    phase_wochen(scenario_from_workbook(rv$wbpath, name = rv$wbname))
  })

  # Szenario aus der geladenen Mappe — haengt NUR an der Mappe, nicht an input$phase.
  sens_scenario <- reactive({
    req(rv$wbpath)
    scenario_from_workbook(rv$wbpath, name = rv$wbname)
  })

  # Sensitivitaet lazy (die Sobol-Ausgaben sind versteckt, solange der Schirm zu ist,
  # und werden von Shiny suspendiert) UND per Szenario-Schluessel gecacht: Mappe +
  # Annahmen + Sim-Seed (scenario_key) plus der feste Sobol-Seed. Ein Weg-/Zurueck-
  # schalten der Phase rechnet damit NICHT neu, solange die Mappe unveraendert ist
  # (DAG-Cache-Invalidierung). input$phase steuert nur die Anzeige, nie die Rechnung.
  sens_out <- bindCache(
    reactive({ phase_sensitivitaet(sens_scenario()) }),
    scenario_key(sens_scenario()), "sobol-seed-1"
  )

  # Verteilungen (Phase 4) lazy und per Szenario-Schluessel gecacht: haengt nur an
  # der Mappe, nicht an input$phase (Weg-/Zurueckschalten rechnet nicht neu).
  vert_out <- bindCache(
    reactive({ phase_verteilungen(sens_scenario()) }),
    scenario_key(sens_scenario()), "vert"
  )

  # Korrelation (Phase 5) lazy und per Szenario-Schluessel gecacht: haengt nur an
  # der Mappe, nicht an input$phase (Weg-/Zurueckschalten rechnet nicht neu).
  kor_out <- bindCache(
    reactive({ phase_korrelation(sens_scenario()) }),
    scenario_key(sens_scenario()), "kor"
  )

  # Kopula (Phase 5b) lazy und per Szenario-Schluessel gecacht: haengt nur an
  # der Mappe, nicht an input$phase (Weg-/Zurueckschalten rechnet nicht neu).
  # Der feste Copula-Seed (11) ist Teil des Cache-Schluessels — dieselbe Mappe
  # liefert dieselbe Validierung.
  kop_out <- bindCache(
    reactive({ phase_kopula(sens_scenario()) }),
    scenario_key(sens_scenario()), "kop-seed-11"
  )

  observeEvent(input$demo, {
    validate(need(file.exists(DEMO_WB), "Tier-1-Mappe nicht gefunden (fixtures/)."))
    run_forecast(DEMO_WB, "Tier-1-Beispielklinik")
  })

  observeEvent(input$wb, {
    req(input$wb)
    p <- input$wb$datapath
    if (!grepl("\\.xlsx$", p, ignore.case = TRUE)) {
      p2 <- tempfile(fileext = ".xlsx"); file.copy(p, p2, overwrite = TRUE); p <- p2
    }
    run_forecast(p, input$wb$name)
  })

  output$wb_name <- renderText(if (is.null(rv$wbname)) "— keine Mappe geladen —" else rv$wbname)

  output$kpi_row <- renderUI({
    o <- rv$out; if (is.null(o)) return(NULL)
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Planergebnis", span(kz("kpi-planergebnis", fmt(o$kpis$result)), " EUR"),
                theme = if (o$kpis$result >= 0) "success" else "danger"),
      value_box("Liquiditaets-Tiefpunkt", span(kz("kpi-tiefpunkt", fmt(o$kpis$trough)), " EUR"),
                p("Monat M", kz("kpi-tiefpunkt-monat", sprintf("%d", o$kpis$trough_month))), theme = "primary"),
      value_box("Max. KK-Inanspruchnahme", span(kz("kpi-maxkk", fmt(o$kpis$maxKK)), " EUR"),
                p(id = "kpi-kk-status", if (o$solvent) "innerhalb Limit" else "Limit ueberschritten"),
                theme = if (o$solvent) "secondary" else "danger")
    )
  })

  output$phase_body <- renderUI({
    if (input$phase == "forecast")
      return(navset_card_tab(
        nav_panel("Kennzahlen & Diagnose", uiOutput("diag")),
        nav_panel("GuV / BWA",   div(style = "overflow-x:auto", tableOutput("guv"))),
        nav_panel("Liquiditaet", div(style = "overflow-x:auto", tableOutput("cash"))),
        nav_panel("DSO / DPO",   tableOutput("dsodpo"))
      ))
    if (input$phase == "wochen") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Wochensicht zu rechnen.")))
      return(navset_card_tab(
        nav_panel("Endbestand-Verlauf",
                  uiOutput("wochen_diag"),
                  imageOutput("wochen_plot", height = "470px")),
        nav_panel("Wochen-Tabelle", div(style = "overflow-x:auto", tableOutput("wochen_tbl"))),
        nav_panel("Rueckabstimmung Monat = Woche", uiOutput("wochen_recon"))
      ))
    }
    if (input$phase == "sens") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Sensitivitaet zu rechnen.")))
      return(navset_card_tab(
        nav_panel("Tornado",
                  uiOutput("sens_diag"),
                  imageOutput("sens_tornado_plot", height = "520px"),
                  div(style = "overflow-x:auto", tableOutput("sens_tornado_tbl"))),
        nav_panel("Sobol",
                  uiOutput("sens_sobol_note"),
                  imageOutput("sens_sobol_plot", height = "520px"),
                  div(style = "overflow-x:auto", tableOutput("sens_sobol_tbl")))
      ))
    }
    if (input$phase == "vert") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Verteilungen je Treiber zu rechnen.")))
      return(navset_card_tab(
        nav_panel("Verteilungen je Treiber",
                  uiOutput("vert_diag"),
                  imageOutput("vert_plot", height = "640px")),
        nav_panel("Treiber-Tabelle",
                  div(style = "overflow-x:auto", tableOutput("vert_tbl"))),
        nav_panel("Spezifikation & Vertrag", uiOutput("vert_recon"))
      ))
    }
    if (input$phase == "korrelation") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Treiber-Korrelation zu rechnen.")))
      return(navset_card_tab(
        nav_panel("Korrelations-Heatmap",
                  uiOutput("kor_diag"),
                  imageOutput("kor_plot", height = "600px")),
        nav_panel("Treiber-Matrix",
                  div(style = "overflow-x:auto", tableOutput("kor_tbl"))),
        nav_panel("Reparatur & Vertrag", uiOutput("kor_recon"))
      ))
    }
    if (input$phase == "kopula") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Gauss-Copula zu validieren.")))
      return(navset_card_tab(
        nav_panel("Copula-Validierung", uiOutput("kop_diag"))
      ))
    }
    if (input$phase == "sim") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Simulation zu rechnen.")))
      # Statuszeile (Fortschritt + Abbrechen) ausserhalb von sim_diag: sim_diag
      # bleibt leer, bis das Ergebnis da ist, und traegt dann GENAU die Kennzahlen
      # (der DOM-Check wartet auf die Befuelltheit, nicht auf eine Zwischenmeldung).
      return(tagList(
        uiOutput("sim_status"),
        navset_card_tab(
          nav_panel("Tiefpunkt-Verteilung",
                    uiOutput("sim_diag"),
                    imageOutput("sim_plot", height = "780px"))
        )
      ))
    }
    if (input$phase == "sim_monate") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Endbestand-Tabelle je Monat zu rechnen.")))
      # Reine Projektion des Simulationsergebnisses (phase-06-monate): dieselbe
      # Hintergrund-MC wie der Simulations-Schirm (sim_status), nur auf die
      # Monats-Tabelle projiziert — keine zweite Monte-Carlo im UI-Thread.
      return(tagList(
        uiOutput("sim_status"),
        navset_card_tab(
          nav_panel("Endbestand je Monat",
                    div(style = "overflow-x:auto", tableOutput("sim_monate_tbl")))
        )
      ))
    }
    if (input$phase == "sim_deckung") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um Tiefpunkt & Deckung zu rechnen.")))
      # Reine Projektion des Simulationsergebnisses (phase-06-deckung): dieselbe
      # Hintergrund-MC wie der Simulations-Schirm (sim_status), nur auf das
      # Deckungs-/Solvenz-Verdikt projiziert — keine zweite Monte-Carlo im
      # UI-Thread.
      return(tagList(
        uiOutput("sim_status"),
        navset_card_tab(
          nav_panel("Tiefpunkt & Deckung", uiOutput("deckung_recon"))
        )
      ))
    }
    if (input$phase == "varianzzerlegung") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um die Varianzzerlegung zu rechnen.")))
      # Drei Regime-Laeufe (voll/epistemisch/aleatorisch) sind teuer: dasselbe
      # Hintergrund-Worker-Muster wie der Simulations-Schirm (eigener Lauf
      # phase_varianzzerlegung im mirai-Worker), NIE im UI-Thread.
      return(tagList(
        uiOutput("var_status"),
        navset_card_tab(
          nav_panel("Varianzzerlegung", uiOutput("varianz_recon"))
        )
      ))
    }
    if (input$phase == "vergleich") {
      if (is.null(rv$wbpath))
        return(card(card_body(class = "text-muted", "Mappe laden, um Szenarien zu vergleichen.")))
      # Szenarien-Verwaltung schlank: die geladene Mappe ist immer Szenario 1
      # ("Basis"); das Formular legt weitere Szenarien mit EINEM Annahme-
      # Override an. N Simulationslaeufe sind teuer: KEIN Auto-Trigger beim
      # Schirmwechsel — der Lauf startet nur ueber 'Vergleich rechnen'.
      return(tagList(
        card(class = "mb-2",
          card_header("Szenarien (Szenario 1 = geladene Mappe, Name 'Basis')"),
          card_body(
            layout_columns(
              col_widths = c(3, 2, 3, 2, 2),
              selectInput("vgl_treiber", "Treiber", choices = rv$assume_namen),
              numericInput("vgl_wert", "Wert", value = 0, step = 0.01),
              textInput("vgl_name", "Szenarioname", placeholder = "z. B. Stress"),
              actionButton("vgl_add",   "Hinzufuegen",         class = "btn-outline-primary btn-sm mt-4"),
              actionButton("vgl_reset", "Liste zuruecksetzen", class = "btn-outline-secondary btn-sm mt-4")),
            uiOutput("vergleich_liste"),
            actionButton("vgl_run", "Vergleich rechnen", class = "btn-primary"))),
        uiOutput("vergleich_status"),
        navset_card_tab(
          nav_panel("Szenario-Vergleich",
                    uiOutput("vergleich_info"),
                    div(style = "overflow-x:auto", tableOutput("vergleich_tbl"))))
      ))
    }
    if (input$phase == "massnahmen") {
      # Massnahmen (Phase 11): Katalog & Paket sind reine DATEN
      # (massnahmen_katalog()) — der Schirm braucht KEINE Mappe und ist
      # initial deterministisch befuellt; Delta-Vorschau und Paket-Vergleich
      # degradieren in ihren Outputs mit regulaerem Hinweis statt Fehler.
      return(navset_card_tab(
        nav_panel("Katalog & Paket",
                  uiOutput("massnahmen_info"),
                  div(style = "overflow-x:auto", tableOutput("massnahmen_katalog_tbl")),
                  uiOutput("massnahmen_paket_ui")),
        nav_panel("Delta-Vorschau", uiOutput("vorschau_ui")),
        nav_panel("Paket-Vergleich", uiOutput("paket_vergleich_ui"))
      ))
    }
    card(card_body(class = "text-muted",
      "Dieser Schirm wird ueber den Autobuild ergaenzt. Aktuell sind die Phasen 'Forecast', 'Wochen', 'Sensitivitaet', 'Verteilungen', 'Korrelation', 'Kopula' und 'Simulation' verdrahtet."))
  })

  output$diag <- renderUI({
    o <- rv$out
    if (is.null(o)) return(p(class = "text-muted", "Mappe laden, um die Analyse zu starten."))
    tags$ul(
      tags$li("Ergebnis Planungszeitraum: ", kz("diag-ergebnis", fmt(o$kpis$result)), " EUR"),
      tags$li("Liquiditaets-Tiefpunkt: ", kz("diag-tiefpunkt", fmt(o$kpis$trough)), " EUR  (M",
              kz("diag-tiefpunkt-monat", sprintf("%d", o$kpis$trough_month)), ")"),
      tags$li("Max. KK-Inanspruchnahme: ", kz("diag-maxkk", fmt(o$kpis$maxKK)), " EUR"),
      tags$li("Monate unter Mindestreserve: ", kz("diag-monate-unter-reserve", sprintf("%d", o$kpis$months_below))),
      tags$li("DSO Umsatz ", kz("diag-dso-umsatz", sprintf("%.0f", o$dso_dpo["umsatz"])),
              " T | DPO Material ", kz("diag-dpo-material", sprintf("%.0f", o$dso_dpo["material"])), " T")
    )
  })

  output$guv    <- renderTable(if (!is.null(rv$out)) fmt_tbl(rv$out$guv),  rownames = TRUE, striped = TRUE, spacing = "xs")
  output$cash   <- renderTable(if (!is.null(rv$out)) fmt_tbl(rv$out$cash), rownames = TRUE, striped = TRUE, spacing = "xs")
  output$dsodpo <- renderTable({
    o <- rv$out; if (is.null(o)) return(NULL)
    data.frame(Groesse = names(o$dso_dpo), `Tage` = round(as.numeric(o$dso_dpo)), check.names = FALSE)
  })

  # --- Wochensicht (Phase 2) -------------------------------------------------
  output$wochen_diag <- renderUI({
    o <- wochen_out()
    div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
        strong("Wochen:"),
        span(class = "text-muted",
             "Tiefpunkt ", kz("wochen-tiefpunkt", fmt(o$kpis$trough)), " EUR (W",
             kz("wochen-tiefpunkt-woche", sprintf("%d", o$kpis$trough_week)),
             ") | Vergleich Monatssicht ", kz("wochen-monatssicht-tiefpunkt", fmt(o$monatssicht$trough)),
             " EUR (M", kz("wochen-monatssicht-monat", sprintf("%d", o$monatssicht$trough_month)), ")"),
        badge(o$diagnostics$rueckabstimmung_ok, "Rueckabstimmung Monat=Woche", id = "wochen-badge-rueckabstimmung"),
        badge(o$diagnostics$kk_innerhalb_limit, "KK innerhalb Limit", id = "wochen-badge-kk-limit"),
        span(class = "text-muted ms-2",
             kz("wochen-unter-reserve", sprintf("%d", o$diagnostics$wochen_unter_reserve)),
             " von ", kz("wochen-anzahl", sprintf("%d", o$n_weeks)), " Wochen unter Mindestreserve"))
  })

  output$wochen_plot <- renderImage({
    o <- wochen_out()
    validate(need(!is.na(o$plot_path) && file.exists(o$plot_path), "Kein Grafikgeraet verfuegbar."))
    list(src = o$plot_path, contentType = "image/png", width = 1150, height = 470,
         alt = "Wochenliquiditaet — Endbestand je Woche")
  }, deleteFile = FALSE)

  output$wochen_tbl <- renderTable({
    o <- wochen_out()
    data.frame(
      Woche         = o$table$Woche,
      ab            = o$table$ab,
      NettoCashflow = fmt(o$table$NettoCashflow),
      Endbestand    = fmt(o$table$Endbestand),
      KK            = fmt(o$table$KK),
      `unter Reserve` = ifelse(o$table$unter_Reserve, "ja", "-"),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "xs", align = "r")

  output$wochen_recon <- renderUI({
    o <- wochen_out(); r <- o$rueckabstimmung; z <- o$kk_zins
    tags$ul(
      tags$li("Σ NettoCashflow Monatssicht: ", kz("wochen-recon-netcf-monat", fmt(r$netcf_monat)), " EUR"),
      tags$li("Σ NettoCashflow Wochensicht: ", kz("wochen-recon-netcf-woche", fmt(r$netcf_woche)), " EUR"),
      tags$li("Differenz: ", kz("wochen-recon-differenz", fmt(r$diff)), " EUR — ",
              kz("wochen-recon-status",
                 if (isTRUE(r$ok)) "abgestimmt (Kalendarisierung erhaelt die Summe)" else "DRIFT")),
      tags$li(HTML(sprintf(
        "KK-Zins Σ Monatssicht <span id=\"wochen-recon-kkzins-monat\">%s</span> EUR vs. Wochensicht <span id=\"wochen-recon-kkzins-woche\">%s</span> EUR%s. <em>Die Wochensicht ist die belastbarere Schaetzung. Welche Sicht hoeher liegt, haengt am Zahltag: beim Beispiel-Mandanten (Lohnlauf Monatsende) ueberzeichnet die Monatssicht den KK-Zins; ein Payday in der Monatsmitte kehrt das um.</em>",
        fmt(z$monat), fmt(z$woche),
        if (!is.na(z$faktor)) sprintf(" (Faktor <span id=\"wochen-recon-kkzins-faktor\">%.1f</span>x)", z$faktor) else "")))
    )
  })

  # --- Sensitivitaet (Phase 3) ----------------------------------------------
  output$sens_diag <- renderUI({
    o <- sens_out()
    div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
        strong("Tornado:"),
        span(class = "text-muted",
             "Basis-Tiefpunkt ", kz("sens-basis-tiefpunkt", fmt(o$base_trough)), " EUR (M",
             kz("sens-basis-tiefpunkt-monat", sprintf("%d", o$trough_month)),
             ") | staerkster Treiber: ", kz("sens-top-treiber", o$diagnostics$top_driver),
             " (Swing ", kz("sens-top-swing", fmt(o$diagnostics$top_swing)), " EUR bei +/-1 Prozentpunkt)"))
  })

  output$sens_tornado_plot <- renderImage({
    o <- sens_out()
    validate(need(!is.na(o$tornado_plot) && file.exists(o$tornado_plot), "Kein Grafikgeraet verfuegbar."))
    list(src = o$tornado_plot, contentType = "image/png", width = 950, height = 520,
         alt = "Tornado — Sensitivitaet des Liquiditaets-Tiefpunkts")
  }, deleteFile = FALSE)

  output$sens_tornado_tbl <- renderTable({
    o <- sens_out()
    data.frame(
      Treiber       = o$tornado$driver,
      `Tief -1pp`   = fmt(o$tornado$low),
      `Tief +1pp`   = fmt(o$tornado$high),
      Swing         = fmt(o$tornado$swing),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "xs", align = "r")

  output$sens_sobol_note <- renderUI({
    o <- sens_out()
    if (o$sobol_available)
      return(div(class = "small text-muted mb-2",
                 "Varianzanteil am Tiefpunkt (Saltelli/Jansen, N=", kz("sens-sobol-n", sprintf("%d", o$sobol_N)),
                 ", Seed ", kz("sens-sobol-seed", sprintf("%d", o$sobol_seed)),
                 ", Bandbreiten aus Blatt 06). S1 = Haupteffekt, ST = inkl. Wechselwirkungen; ST deutlich > S1 => Interaktion."))
    div(class = "small text-warning mb-2",
        "Sobol nicht verfuegbar: ", kz("sens-sobol-grund", o$sobol_reason))
  })

  output$sens_sobol_plot <- renderImage({
    o <- sens_out()
    validate(need(o$sobol_available && !is.na(o$sobol_plot) && file.exists(o$sobol_plot),
                  if (!o$sobol_available) o$sobol_reason else "Kein Grafikgeraet verfuegbar."))
    list(src = o$sobol_plot, contentType = "image/png", width = 950, height = 520,
         alt = "Sobol — globale Sensitivitaet des Liquiditaets-Tiefpunkts")
  }, deleteFile = FALSE)

  output$sens_sobol_tbl <- renderTable({
    o <- sens_out()
    if (!o$sobol_available) return(NULL)
    data.frame(Treiber = o$sobol$Treiber, S1 = o$sobol$S1, ST = o$sobol$ST, check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "r")

  # --- Verteilungen (Phase 4) -----------------------------------------------
  output$vert_diag <- renderUI({
    o <- vert_out(); dg <- o$diagnostics
    div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
        strong("Verteilungen:"),
        span(class = "text-muted",
             kz("vert-treiber-anzahl", sprintf("%d", dg$n_treiber)), " Treiber mit Verteilung | ",
             kz("vert-historie-monate", sprintf("%d", dg$historie_monate)), " Monate Historie | ",
             kz("vert-cv-geschaetzt", sprintf("%d", dg$cv_geschaetzt_n)), " geschaetzt / ",
             kz("vert-cv-vorgabe", sprintf("%d", dg$cv_vorgabe_n)), " Vorgabe"),
        badge(dg$cv_belastbar, "CV-Schaetzung belastbar (>=24 Monate)", id = "vert-cv-belastbar"))
  })

  output$vert_plot <- renderImage({
    o <- vert_out()
    validate(need(!is.na(o$plot_path) && file.exists(o$plot_path), "Kein Grafikgeraet verfuegbar."))
    list(src = o$plot_path, contentType = "image/png", width = 1150, height = 640,
         alt = "PERT-Histogramme je Treiber + aleatorisches Ist-Residuen-Histogramm der Menge")
  }, deleteFile = FALSE)

  output$vert_tbl <- renderTable({
    o <- vert_out()
    data.frame(
      Treiber   = o$table$Treiber,
      Mode      = o$table$Mode,
      P5        = o$table$P5,
      P50       = o$table$P50,
      P95       = o$table$P95,
      CV_aleat  = o$table$CV,
      Quelle    = o$table$Quelle,
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "xs", align = "r")

  output$vert_recon <- renderUI({
    o <- vert_out(); dg <- o$diagnostics
    tags$ul(
      tags$li("Gesicherte Spezifikation (Eingabe fuer Phase 5/6): ",
              kz("vert-spec-pfad", o$spec_path)),
      tags$li("CV-Basis-Allokation: ", kz("vert-basis", dg$basis_text)),
      tags$li("Fuer eine belastbare aleatorische Schaetzung sind >=24 Monate Historie noetig ",
              "(Basisjahr: ", kz("vert-historie-monate-recon", sprintf("%d", dg$historie_monate)), " Monate).")
    )
  })

  # --- Korrelation (Phase 5) ------------------------------------------------
  output$kor_diag <- renderUI({
    o <- kor_out(); dg <- o$diagnostics
    div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
        strong("Korrelation:"),
        span(class = "text-muted",
             kz("kor-treiber-anzahl", sprintf("%d", dg$n_treiber)), " Treiber in der Matrix"),
        badge(dg$ziel_psd,      "Zielmatrix (Blatt 07) PSD", id = "kor-psd-vorher"),
        badge(dg$repariert_psd, "reparierte Matrix PSD",     id = "kor-psd-nachher"),
        span(class = "text-muted ms-2",
             "Reparatur ", kz("kor-methode", o$method), " | max. Aenderung Ziel -> repariert ",
             kz("kor-max-aenderung", sprintf("%.4f", o$max_aenderung))))
  })

  output$kor_plot <- renderImage({
    o <- kor_out()
    validate(need(!is.na(o$plot_path) && file.exists(o$plot_path), "Kein Grafikgeraet verfuegbar."))
    list(src = o$plot_path, contentType = "image/png", width = 640, height = 600,
         alt = "Heatmap der reparierten Treiber-Korrelation")
  }, deleteFile = FALSE)

  # Reparierte Matrix je Treiberpaar: der Wrapper liefert out$R (bit-identisch
  # zur Engine, keine vorformatierte Tabelle) — digits = 2 formatiert die Anzeige.
  output$kor_tbl <- renderTable(
    as.data.frame(kor_out()$R),
    rownames = TRUE, striped = TRUE, spacing = "xs", align = "r", digits = 2
  )

  output$kor_recon <- renderUI({
    o <- kor_out()
    tags$ul(
      tags$li("Gesicherte Korrelationsmatrix (Eingabe fuer Phase 6): ",
              kz("kor-kor-pfad", o$kor_path)),
      tags$li("Reparaturmethode: ", kz("kor-methode-recon", o$method)),
      tags$li("Kleinster Eigenwert vorher ", kz("kor-ev-vorher", sprintf("%.4f", o$ev_before)),
              " -> nachher ", kz("kor-ev-nachher", sprintf("%.4f", o$ev_after)),
              " (>= 0 = positiv-semidefinit)"),
      tags$li("Max. Aenderung Ziel -> repariert: ",
              kz("kor-max-aenderung-recon", sprintf("%.4f", o$max_aenderung))),
      tags$li("Treiber-Abdeckung: alle ", kz("kor-treiber-anzahl-recon", sprintf("%d", o$diagnostics$n_treiber)),
              " Treiber aus Blatt 06 stehen in Blatt 07 (sonst waere die Phase mit benanntem Fehler abgebrochen).")
    )
  })

  # --- Kopula (Phase 5b) ----------------------------------------------------
  # Diagnosezeile der Gauss-Copula-Validierung: Badge "reproduziert Korrelation"
  # (dom_id kop-reproduziert), max. Spearman-Abweichung vs. Formel (kop-spearman-
  # dev), max. Pearson-Abweichung (copulabedingte Daempfung), n Ziehungen, Seed,
  # PERT-Stuetzen-Vertrag. Ehrlicher Hinweis auf enge Treiberbaender.
  output$kop_diag <- renderUI({
    o <- kop_out(); v <- o$validierung; dg <- o$diagnostics
    div(
      div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
          strong("Gauss-Copula:"),
          badge(dg$reproduziert, "reproduziert Korrelation", id = "kop-reproduziert"),
          badge(dg$stuetzen_ok,  "PERT-Stuetzen-Vertrag (jeder Zug in [min,max])", id = "kop-stuetzen"),
          span(class = "text-muted ms-2",
               "max. Spearman-Abweichung vs. Formel (6/pi)*asin(R/2) ",
               kz("kop-spearman-dev", sprintf("%.4f", v$spearman_max_dev)),
               " | max. Pearson-Abweichung (copulabedingte Daempfung) ",
               kz("kop-pearson-dev", sprintf("%.4f", v$pearson_max_dev)),
               " | ", kz("kop-n", sprintf("%d", as.integer(v$n))), " Ziehungen, Seed ",
               kz("kop-seed", sprintf("%d", as.integer(v$seed))))),
      div(class = "small text-muted",
          "Ehrlicher Hinweis: bei engen Treiberbaendern hat das Korrelationsblatt (07) kaum ",
          "Effekt auf die Tiefpunktverteilung — die Copula bindet die Raender, aber schmale ",
          "[min,max]-Baender lassen der Abhaengigkeit wenig Spielraum.")
    )
  })

  # --- Simulation (Phase 6, phase-06-fan) -----------------------------------
  # Die genestete Monte-Carlo laeuft im mirai-Worker (nie im UI-Thread). Der
  # Schirm startet den Lauf automatisch, sobald er offen ist und eine Mappe
  # geladen wurde — ein Weiterschalten waehrend der Rechnung laesst die App
  # bedienbar (Fortschritt + Abbrechen). Der volle Ergebnis-Vertrag `out`
  # (phase_simulation) speist auch die Schwester-Schirme (Monate/Deckung).
  sim_rv <- reactiveValues(out = NULL, running = FALSE, err = NULL,
                           key = NULL, handle = NULL, cancelled = FALSE)

  start_sim <- function(wbpath, wbname) {
    sim_rv$running   <- TRUE;  sim_rv$out <- NULL; sim_rv$err <- NULL
    sim_rv$cancelled <- FALSE; sim_rv$key <- paste0(wbpath, "|", wbname)
    # GoBD-Hook (Rz. 100/152): der Lauf wird revisionsfest protokolliert --
    # runs/<run_id>/ mit Mappen-Kopie + write-once-Manifest. runs_dir ABSOLUT
    # ueber ROOT, weil der Worker-cwd nicht die Repo-Wurzel ist; Fehlerpfade
    # (harte Treiber-Whitelist, write-once) laufen ueber onRejected in
    # sim_rv$err.
    m <- mirai::mirai(
      simulation_mit_manifest(scenario_from_workbook(wbpath, name = wbname),
                              runs_dir = runs_dir),
      wbpath = wbpath, wbname = wbname, runs_dir = file.path(ROOT, "runs"))
    sim_rv$handle <- m
    promises::then(m,
      onFulfilled = function(res) {
        sim_rv$out <- res; sim_rv$running <- FALSE; sim_rv$handle <- NULL
      },
      onRejected = function(err) {
        sim_rv$running <- FALSE; sim_rv$handle <- NULL
        if (!isTRUE(sim_rv$cancelled)) sim_rv$err <- conditionMessage(err)
      })
    invisible()
  }

  # Auto-Trigger: Schirm offen + Mappe geladen + noch nicht fuer diese Mappe
  # gerechnet (Schluessel = Pfad|Name). Ein Abbruch laesst den Schluessel stehen,
  # damit NICHT sofort neu gestartet wird — Neustart nur ueber 'Neu rechnen'.
  observe({
    req(HAS_ASYNC, input$phase %in% c("sim", "sim_monate", "sim_deckung"), rv$wbpath)
    key <- paste0(rv$wbpath, "|", rv$wbname)
    if (!identical(sim_rv$key, key)) start_sim(rv$wbpath, rv$wbname)
  })

  observeEvent(input$sim_cancel, {
    sim_rv$cancelled <- TRUE
    if (!is.null(sim_rv$handle)) try(mirai::stop_mirai(sim_rv$handle), silent = TRUE)
    sim_rv$running <- FALSE; sim_rv$handle <- NULL
    sim_rv$err <- "Berechnung abgebrochen."
  })

  observeEvent(input$sim_rerun, { req(rv$wbpath); start_sim(rv$wbpath, rv$wbname) })

  output$sim_status <- renderUI({
    if (!HAS_ASYNC)
      return(div(class = "alert alert-warning py-2 mb-2",
        "Hintergrund-Worker nicht verfuegbar: die Pakete ", tags$code("mirai"), " / ",
        tags$code("promises"), " fehlen. Die genestete Monte-Carlo laeuft bewusst NICHT ",
        "synchron im UI-Thread — bitte ", tags$code("Rscript tests/deps.R"), " ausfuehren."))
    if (isTRUE(sim_rv$running))
      return(div(class = "d-flex gap-2 align-items-center py-2 mb-2",
        tags$div(class = "spinner-border spinner-border-sm text-primary", role = "status"),
        span(class = "text-muted",
             "Monte-Carlo laeuft im Hintergrund-Worker (mirai) — die Oberflaeche bleibt bedienbar."),
        actionButton("sim_cancel", "Abbrechen", class = "btn-outline-danger btn-sm ms-2")))
    if (!is.null(sim_rv$err))
      return(div(class = "alert alert-danger py-2 mb-2",
        "Simulation nicht abgeschlossen: ", sim_rv$err, " ",
        actionButton("sim_rerun", "Neu rechnen", class = "btn-outline-primary btn-sm ms-2")))
    div(class = "d-flex gap-2 align-items-center py-2 mb-2",
        span(class = "text-success small", "Simulation abgeschlossen."),
        actionButton("sim_rerun", "Neu rechnen", class = "btn-outline-primary btn-sm ms-2"))
  })

  # Diagnosezeile: erst befuellt, wenn das Ergebnis da ist (req) — sie traegt die
  # festen DOM-IDs (sim-s, sim-seed, sim-horizont, sim-treiber, sim-plan-tiefpunkt,
  # sim-endbestand-pfad) und die Badges (Determinismus, PERT-Stuetzen).
  output$sim_diag <- renderUI({
    o <- sim_rv$out; req(o)
    m <- o$metrics; dg <- o$diagnostics
    div(
      div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
          strong("Simulation:"),
          span(class = "text-muted",
               kz("sim-s", sprintf("%d", as.integer(m$S))), " Pfade ueber Horizont ",
               kz("sim-horizont", sprintf("%d", as.integer(m$H))), " Monate | Seed ",
               kz("sim-seed", sprintf("%d", as.integer(m$seed))), " | ",
               kz("sim-treiber", sprintf("%d", as.integer(dg$n_treiber))),
               " epistemische Treiber | det. Plan-Tiefpunkt (Tier-1) ",
               span(kz("sim-plan-tiefpunkt", fmt(m$plan_trough)), " EUR")),
          badge(dg$deterministisch, "Determinismus (Seed reproduzierbar)", id = "sim-determinismus"),
          badge(dg$stuetzen_ok,     "PERT-Stuetzen (Treiber-Zuege in [min,max])", id = "sim-stuetzen")),
      div(class = "small text-muted",
          "Endbestand-Matrix gesichert (Eingabe Phase 7): ",
          kz("sim-endbestand-pfad", o$endbestand_path),
          " — Hinweis: die schwere Monte-Carlo laeuft im Hintergrund-Worker ",
          "(mirai/future), nicht im UI-Thread."),
      # GoBD-Zeile (phase-12-runmanifest-anbindung): run_id + IKS-Kontrollstand
      # des protokollierten Laufs. DEFENSIV per is.null-Guard: ein altes/
      # fremdes Ergebnisobjekt ohne gobd-Beobachterfeld degradiert nur diese
      # Zeile, nicht den Schirm.
      if (!is.null(o$gobd)) div(class = "small text-muted",
          "GoBD-Protokoll: Lauf ", kz("sim-run-id", o$gobd$run_id),
          " | IKS-Kontrollen ",
          kz("sim-iks-bestanden",
             sprintf("%d/%d", sum(o$gobd$kontrollen$bestanden),
                     nrow(o$gobd$kontrollen))), " bestanden ",
          badge(all(o$gobd$kontrollen$bestanden),
                "IKS (Anker, Masse, GuV, Whitelist)", id = "sim-iks-badge"))
    )
  })

  output$sim_plot <- renderImage({
    o <- sim_rv$out; req(o)
    # length()-Guard VOR is.na(): ein Ergebnisobjekt ohne plot_path (NULL/leer)
    # liefert bei !is.na() ein logical(0) und `logical(0) && ...` wirft (R >= 4.3)
    # — dann degradiert der Schirm zum Fallback statt zu crashen.
    validate(need(length(o$plot_path) == 1 && !is.na(o$plot_path) && file.exists(o$plot_path),
                  "Kein Grafikgeraet verfuegbar — Fan-Chart uebersprungen."))
    list(src = o$plot_path, contentType = "image/png", width = 1100, height = 780,
         alt = "Endbestand-Fan (P5-P95, P25-P75, Median) und P(unter Mindestreserve) je Monat")
  }, deleteFile = FALSE)

  # --- Endbestand je Monat (Phase 6, phase-06-monate) -----------------------
  # Reine Projektion des Simulationsergebnisses ueber die GEMEINSAME Hilfsfunktion
  # project_sim_monate() (R/phase_sim_monate.R) — dieselbe Quelle wie der Wrapper
  # phase_sim_monate(), damit Vertrag und Anzeige nicht auseinanderdriften. Der
  # Render uebernimmt die numerische Projektion und formatiert erst hier: EUR
  # ueber fmt(), P_unter als Prozent (sprintf %.0f%%). Quelle ist sim_rv$out
  # (phase_simulation im Worker).
  output$sim_monate_tbl <- renderTable({
    o <- sim_rv$out; req(o)
    proj <- project_sim_monate(o)
    data.frame(
      Monat               = proj$Monat,
      P05                 = fmt(proj$P05),
      P50                 = fmt(proj$P50),
      P95                 = fmt(proj$P95),
      `P(unter Reserve)`  = sprintf("%.0f%%", 100 * proj$P_unter),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "xs", align = "r")

  # --- Tiefpunkt & Deckung (Phase 6, phase-06-deckung) -----------------------
  # Reine Projektion des Simulationsergebnisses ueber die GEMEINSAME Hilfsfunktion
  # deckung_from_out() (R/phase_sim_deckung.R) — dieselbe Quelle wie der Wrapper
  # phase_sim_deckung(), damit Vertrag und Anzeige nicht auseinanderdriften.
  # KEINE zweite Monte-Carlo: projiziert wird das im Worker gerechnete sim_rv$out;
  # das KK-Limit ist die beim Mappe-Laden gesicherte Modell-Schwelle
  # (rv$kk_limit). Formatierung erst hier: EUR ueber fmt(), Wahrscheinlichkeiten
  # via sprintf mit einer Nachkommastelle; das InsO-Urteil (§§ 18/19) haengt als
  # Badge an insO_erfuellt.
  output$deckung_recon <- renderUI({
    o <- sim_rv$out; req(o, rv$kk_limit)
    d <- deckung_from_out(o, rv$kk_limit)
    tags$ul(
      tags$li("Tiefpunkt Median: ", kz("deckung-trough-median", fmt(d$trough_median)), " EUR"),
      tags$li("Tiefpunkt P05: ", kz("deckung-trough-p05", fmt(d$trough_p05)), " EUR"),
      tags$li("Tiefpunkt P95: ", kz("deckung-trough-p95", fmt(d$trough_p95)), " EUR"),
      tags$li("P(KK-Linie reisst, Limit ", kz("deckung-kk-limit", fmt(d$kk_limit)), " EUR): ",
              kz("deckung-p-breach", sprintf("%.1f", d$P_breach_pct)), " %"),
      tags$li("P(zahlungsfaehig ueber den Horizont): ",
              kz("deckung-p-solvent", sprintf("%.1f", d$P_solvent_pct)),
              " % — §§ 18/19 InsO: > 50 % gefordert — ",
              badge(d$insO_erfuellt,
                    if (isTRUE(d$insO_erfuellt)) "P(zahlungsfaehig) > 50 %: erfuellt"
                    else "P(zahlungsfaehig) > 50 %: nicht erfuellt",
                    id = "deckung-badge-inso")),
      tags$li("Seed ", kz("deckung-seed", sprintf("%d", as.integer(d$seed))),
              " | ", kz("deckung-s", sprintf("%d", as.integer(d$S))), " Pfade"),
      tags$li("Gesicherte Endbestand-Matrix (Eingabe Phase 7): ",
              kz("deckung-endbestand-pfad", o$endbestand_path))
    )
  })

  # --- Varianzzerlegung (Phase 7) ---------------------------------------------
  # Eigener Hintergrund-Lauf (phase_varianzzerlegung: drei Regime-Laeufe je S
  # Pfade), dasselbe mirai-Worker-Muster wie der Simulations-Schirm — Auto-
  # Trigger je Mappe (Schluessel = Pfad|Name), Abbrechen laesst den Schluessel
  # stehen (Neustart nur ueber 'Neu rechnen'). Der Render projiziert
  # AUSSCHLIESSLICH das Ergebnisobjekt; Formatierung passiert erst dort.
  var_rv <- reactiveValues(out = NULL, running = FALSE, err = NULL,
                           key = NULL, handle = NULL, cancelled = FALSE)

  start_var <- function(wbpath, wbname) {
    var_rv$running   <- TRUE;  var_rv$out <- NULL; var_rv$err <- NULL
    var_rv$cancelled <- FALSE; var_rv$key <- paste0(wbpath, "|", wbname)
    m <- mirai::mirai(
      phase_varianzzerlegung(scenario_from_workbook(wbpath, name = wbname)),
      wbpath = wbpath, wbname = wbname)
    var_rv$handle <- m
    promises::then(m,
      onFulfilled = function(res) {
        var_rv$out <- res; var_rv$running <- FALSE; var_rv$handle <- NULL
      },
      onRejected = function(err) {
        var_rv$running <- FALSE; var_rv$handle <- NULL
        if (!isTRUE(var_rv$cancelled)) var_rv$err <- conditionMessage(err)
      })
    invisible()
  }

  observe({
    req(HAS_ASYNC, input$phase == "varianzzerlegung", rv$wbpath)
    key <- paste0(rv$wbpath, "|", rv$wbname)
    if (!identical(var_rv$key, key)) start_var(rv$wbpath, rv$wbname)
  })

  observeEvent(input$var_cancel, {
    var_rv$cancelled <- TRUE
    if (!is.null(var_rv$handle)) try(mirai::stop_mirai(var_rv$handle), silent = TRUE)
    var_rv$running <- FALSE; var_rv$handle <- NULL
    var_rv$err <- "Berechnung abgebrochen."
  })

  observeEvent(input$var_rerun, { req(rv$wbpath); start_var(rv$wbpath, rv$wbname) })

  output$var_status <- renderUI({
    if (!HAS_ASYNC)
      return(div(class = "alert alert-warning py-2 mb-2",
        "Hintergrund-Worker nicht verfuegbar: die Pakete ", tags$code("mirai"), " / ",
        tags$code("promises"), " fehlen. Die Varianzzerlegung laeuft bewusst NICHT ",
        "synchron im UI-Thread — bitte ", tags$code("Rscript tests/deps.R"), " ausfuehren."))
    if (isTRUE(var_rv$running))
      return(div(class = "d-flex gap-2 align-items-center py-2 mb-2",
        tags$div(class = "spinner-border spinner-border-sm text-primary", role = "status"),
        span(class = "text-muted",
             "Varianzzerlegung laeuft im Hintergrund-Worker (mirai) — die Oberflaeche bleibt bedienbar."),
        actionButton("var_cancel", "Abbrechen", class = "btn-outline-danger btn-sm ms-2")))
    if (!is.null(var_rv$err))
      return(div(class = "alert alert-danger py-2 mb-2",
        "Varianzzerlegung nicht abgeschlossen: ", var_rv$err, " ",
        actionButton("var_rerun", "Neu rechnen", class = "btn-outline-primary btn-sm ms-2")))
    div(class = "d-flex gap-2 align-items-center py-2 mb-2",
        span(class = "text-success small", "Varianzzerlegung abgeschlossen."),
        actionButton("var_rerun", "Neu rechnen", class = "btn-outline-primary btn-sm ms-2"))
  })

  # Kennzahlen-Projektion des Ergebnisobjekts (phase_varianzzerlegung im
  # Worker): SD in EUR ueber fmt(), Anteile in Prozent mit einer
  # Nachkommastelle — Formatierung NUR hier, die Zahlen kommen unveraendert
  # aus dem Ergebnisobjekt.
  output$varianz_recon <- renderUI({
    o <- var_rv$out; req(o)
    tags$ul(
      tags$li("SD Tiefpunkt gesamt: ", kz("varianz-sd-gesamt", fmt(o$sd_gesamt)), " EUR"),
      tags$li("davon epistemisch (Annahmen): ", kz("varianz-sd-epi", fmt(o$sd_epi)), " EUR"),
      tags$li("davon aleatorisch (Volatilitaet): ", kz("varianz-sd-ale", fmt(o$sd_ale)), " EUR"),
      tags$li("Varianzanteil epistemisch: ", kz("varianz-anteil-epi", sprintf("%.1f", 100 * o$anteil_epi)), " %"),
      tags$li("Varianzanteil aleatorisch: ", kz("varianz-anteil-ale", sprintf("%.1f", 100 * o$anteil_ale)), " %"),
      tags$li("Varianzanteil Interaktion: ", kz("varianz-anteil-inter", sprintf("%.1f", 100 * o$anteil_inter)),
              " % (Rest; klein bei naeherungsweiser Unabhaengigkeit der Quellen)"),
      tags$li(kz("varianz-s", sprintf("%d", as.integer(o$S))), " Pfade je Lauf | Seed ",
              kz("varianz-seed", sprintf("%d", as.integer(o$seed))), " (Szenario-Schluessel)")
    )
  })

  # --- Szenario-Vergleich (Phase 8) -------------------------------------------
  # compare_scenarios(scs) im mirai-Worker — dasselbe Muster wie start_var;
  # Auto-Trigger nur fuer die Basis (N-Szenarien-Laeufe sind teuer). Der Name
  # vergleich_rv und das Feld out sind VERTRAG (handoff phase-08 /
  # tests/test_compare_ui.R stubbt vergleich_rv$out direkt). specs haelt nur
  # leichte Szenario-Definitionen (name/treiber/wert); die liqui_scenario-
  # Objekte entstehen erst im Worker aus der aktuell geladenen Mappe.
  vergleich_rv <- reactiveValues(out = NULL, running = FALSE, err = NULL,
                                 key = NULL, handle = NULL, cancelled = FALSE,
                                 specs = list(), hinweis = NULL)

  observeEvent(input$vgl_add, {
    req(rv$wbpath)
    name      <- trimws(input$vgl_name)
    vorhanden <- c("Basis", vapply(vergleich_rv$specs, `[[`, character(1L), "name"))
    # Doppelte/leere Namen und fehlende Werte abfangen: Hinweis statt Absturz.
    if (!nzchar(name)) {
      vergleich_rv$hinweis <- "Bitte einen Szenarionamen angeben."; return()
    }
    if (name %in% vorhanden) {
      vergleich_rv$hinweis <- sprintf("Name '%s' ist schon vergeben — bitte eindeutig benennen.", name); return()
    }
    if (is.null(input$vgl_treiber) || !isTRUE(is.finite(input$vgl_wert))) {
      vergleich_rv$hinweis <- "Bitte Treiber und einen numerischen Wert angeben."; return()
    }
    vergleich_rv$specs   <- c(vergleich_rv$specs,
                              list(list(name = name, treiber = input$vgl_treiber,
                                        wert = input$vgl_wert)))
    vergleich_rv$hinweis <- NULL
  })

  observeEvent(input$vgl_reset, {
    vergleich_rv$specs <- list(); vergleich_rv$hinweis <- NULL
  })

  output$vergleich_liste <- renderUI({
    specs <- vergleich_rv$specs
    div(class = "small text-muted mb-2",
        tags$ul(
          tags$li("1. Basis (geladene Mappe, ohne Override)"),
          lapply(seq_along(specs), function(i)
            tags$li(sprintf("%d. %s (%s = %s)", i + 1L, specs[[i]]$name,
                            specs[[i]]$treiber, format(specs[[i]]$wert))))),
        if (!is.null(vergleich_rv$hinweis))
          div(class = "text-warning", vergleich_rv$hinweis))
  })

  start_vergleich <- function(wbpath, wbname, specs) {
    vergleich_rv$running   <- TRUE;  vergleich_rv$out <- NULL; vergleich_rv$err <- NULL
    vergleich_rv$cancelled <- FALSE; vergleich_rv$key <- paste0(wbpath, "|", wbname)
    m <- mirai::mirai({
      # Szenariobau (Basis + Formular-Szenarien, sim Default) und Vergleich in
      # der EINEN benannten, direkt getesteten Funktion — der everywhere-Block
      # sourct R/compare_scenarios.R, scenarios_from_specs ist im Worker da.
      compare_scenarios(scenarios_from_specs(wbpath, specs))
    }, wbpath = wbpath, specs = specs)
    vergleich_rv$handle <- m
    promises::then(m,
      onFulfilled = function(res) {
        vergleich_rv$out <- res; vergleich_rv$running <- FALSE; vergleich_rv$handle <- NULL
      },
      onRejected = function(err) {
        vergleich_rv$running <- FALSE; vergleich_rv$handle <- NULL
        if (!isTRUE(vergleich_rv$cancelled)) vergleich_rv$err <- conditionMessage(err)
      })
    invisible()
  }

  # Auto-Lauf NUR fuer die Basis beim ersten Schirmbesuch je Mappe (leere
  # specs, n = 1 — ein Lauf, Kostenstruktur wie der Varianz-Schirm; der
  # Verifier prueft den Live-Pfad). Formular-Szenarien starten weiterhin
  # ausschliesslich ueber den Button 'Vergleich rechnen', der denselben key
  # setzt und damit den Auto-Lauf nicht erneut ausloest.
  observe({
    req(HAS_ASYNC, input$phase == "vergleich", rv$wbpath)
    key <- paste0(rv$wbpath, "|", rv$wbname)
    if (!identical(vergleich_rv$key, key)) start_vergleich(rv$wbpath, rv$wbname, list())
  })

  observeEvent(input$vgl_run, {
    req(HAS_ASYNC, rv$wbpath)
    start_vergleich(rv$wbpath, rv$wbname, vergleich_rv$specs)
  })

  observeEvent(input$vergleich_cancel, {
    vergleich_rv$cancelled <- TRUE
    if (!is.null(vergleich_rv$handle)) try(mirai::stop_mirai(vergleich_rv$handle), silent = TRUE)
    vergleich_rv$running <- FALSE; vergleich_rv$handle <- NULL
    vergleich_rv$err <- "Berechnung abgebrochen."
  })

  output$vergleich_status <- renderUI({
    if (!HAS_ASYNC)
      return(div(class = "alert alert-warning py-2 mb-2",
        "Hintergrund-Worker nicht verfuegbar: die Pakete ", tags$code("mirai"), " / ",
        tags$code("promises"), " fehlen. Der Szenario-Vergleich laeuft bewusst NICHT ",
        "synchron im UI-Thread — bitte ", tags$code("Rscript tests/deps.R"), " ausfuehren."))
    if (isTRUE(vergleich_rv$running))
      return(div(class = "d-flex gap-2 align-items-center py-2 mb-2",
        tags$div(class = "spinner-border spinner-border-sm text-primary", role = "status"),
        span(class = "text-muted",
             "Szenario-Vergleich laeuft im Hintergrund-Worker (mirai) — die Oberflaeche bleibt bedienbar."),
        actionButton("vergleich_cancel", "Abbrechen", class = "btn-outline-danger btn-sm ms-2")))
    if (!is.null(vergleich_rv$err))
      return(div(class = "alert alert-danger py-2 mb-2",
        "Szenario-Vergleich nicht abgeschlossen: ", vergleich_rv$err))
    if (is.null(vergleich_rv$out))
      return(div(class = "small text-muted py-1 mb-2",
        "Noch kein Vergleich gerechnet — Szenarien anlegen und 'Vergleich rechnen' starten."))
    div(class = "py-2 mb-2",
        span(class = "text-success small", "Szenario-Vergleich abgeschlossen."))
  })

  # Kennzahl vergleich-n (Anzahl Szenarien, sprintf %d) plus kompakter Status
  # Seed/Pfade je Lauf — Quelle AUSSCHLIESSLICH vergleich_rv$out.
  output$vergleich_info <- renderUI({
    o <- vergleich_rv$out; req(o)
    tb <- o$tabelle
    div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
        strong("Vergleich:"),
        span(class = "text-muted",
             kz("vergleich-n", sprintf("%d", as.integer(o$n))), " Szenarien | ",
             paste0(tb$szenario, " (", as.integer(tb$S), " Pfade, Seed ",
                    as.integer(tb$seed), ")", collapse = " | ")))
  })

  # Projiziert AUSSCHLIESSLICH vergleich_rv$out$tabelle; Formatierung NUR hier
  # (EUR ueber fmt(), Prozente mit einer Nachkommastelle, insO als ja/—), die
  # Zahlen kommen unveraendert aus dem Ergebnisobjekt.
  output$vergleich_tbl <- renderTable({
    o <- vergleich_rv$out; req(o)
    tb <- o$tabelle
    data.frame(
      Szenario             = tb$szenario,
      `Plan-Tiefpunkt`     = fmt(tb$plan_trough),
      `Tiefpunkt Median`   = fmt(tb$trough_median),
      `Tiefpunkt P05`      = fmt(tb$trough_p05),
      `Tiefpunkt P95`      = fmt(tb$trough_p95),
      `P(KK-Linie reisst)` = sprintf("%.1f%%", tb$P_breach_pct),
      `P(zahlungsfaehig)`  = sprintf("%.1f%%", tb$P_solvent_pct),
      `insO erfuellt`      = ifelse(tb$insO_erfuellt, "ja", "—"),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "xs", align = "r")

  # --- Massnahmen (Phase 11, phase-11-massnahmen-ui) ---------------------------
  # Katalog & Paket sind reine DATEN (massnahmen_katalog(), R/massnahmen.R) —
  # die Outputs des ersten Reiters rufen weder Engine noch Monte-Carlo und
  # sind initial deterministisch befuellt (Phasen-Anker). Zweipfadiges
  # Wirkmodell: treiber-Massnahmen wirken als Annahme-Override erst ueber den
  # naechsten Simulationslauf (Paket-Vergleich), delta-Massnahmen sofort als
  # Overlay auf das vorhandene Simulationsergebnis (Delta-Vorschau).
  output$massnahmen_info <- renderUI({
    n <- nrow(massnahmen_katalog())
    div(
      div(class = "d-flex gap-2 align-items-center flex-wrap mb-2",
          strong("Massnahmen:"),
          span(class = "text-muted",
               kz("massnahmen-katalog-n", sprintf("%d", n)),
               " Massnahmen im Startkatalog (drei Kategorien)")),
      div(class = "small text-muted",
          "Zweipfadiges Wirkmodell: treiber-Massnahmen wirken als ",
          "Annahme-Override erst ueber den naechsten Simulationslauf ",
          "(Paket-Vergleich); delta-Massnahmen wirken sofort als Overlay ",
          "auf das vorhandene Simulationsergebnis (Delta-Vorschau).")
    )
  })

  # Alle Katalogzeilen mit allen sechs Spalten (id/name/kategorie/typ/treiber/
  # hinweis) unveraendert — deterministische Daten, keine Formatierung noetig.
  output$massnahmen_katalog_tbl <- renderTable(
    massnahmen_katalog(),
    striped = TRUE, spacing = "xs"
  )

  # Schalterliste: je Katalogzeile eine Checkbox mn_<id>; je AKTIVIERTER
  # Massnahme die Parametrisierung (treiber: mn_<id>_delta; delta:
  # mn_<id>_betrag/_start/_dauer/_verlauf). Die Parameter-Defaults lesen den
  # letzten Stand isoliert — der Output rendert nur bei Checkbox-Wechsel neu,
  # Tippen in den Parameterfeldern loest keinen Re-Render aus.
  output$massnahmen_paket_ui <- renderUI({
    kat <- massnahmen_katalog()
    letzter <- function(iid, default) {
      v <- isolate(input[[iid]])
      if (is.null(v)) default else v
    }
    tagList(lapply(seq_len(nrow(kat)), function(i) {
      id    <- kat$id[i]
      aktiv <- isTRUE(input[[paste0("mn_", id)]])
      div(class = "border-bottom pb-1 mb-1",
          checkboxInput(paste0("mn_", id),
                        sprintf("%s [%s]", kat$name[i], kat$kategorie[i]),
                        value = aktiv, width = "100%"),
          if (aktiv && identical(kat$typ[i], "treiber"))
            numericInput(paste0("mn_", id, "_delta"),
                         sprintf("Delta auf Treiber '%s'", kat$treiber[i]),
                         value = letzter(paste0("mn_", id, "_delta"), 0),
                         step = 0.01),
          if (aktiv && identical(kat$typ[i], "delta"))
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              numericInput(paste0("mn_", id, "_betrag"), "Betrag (EUR)",
                           value = letzter(paste0("mn_", id, "_betrag"), 0)),
              numericInput(paste0("mn_", id, "_start"), "Startmonat",
                           value = letzter(paste0("mn_", id, "_start"), 1),
                           min = 1, step = 1),
              numericInput(paste0("mn_", id, "_dauer"), "Dauer (Monate)",
                           value = letzter(paste0("mn_", id, "_dauer"), 1),
                           min = 1, step = 1),
              selectInput(paste0("mn_", id, "_verlauf"), "Verlauf",
                          choices = c("einmalig", "linear"),
                          selected = letzter(paste0("mn_", id, "_verlauf"),
                                             "einmalig"))))
    }))
  })

  # Paket-Reactive: sammelt aus den mn_-Inputs die paket-Liste im
  # quantifiziere_paket-Vertrag (je Eintrag list(id, params); treiber:
  # params$delta — delta: betrag/start_m/dauer/verlauf). Unvollstaendig
  # parametrisierte Massnahmen (NA im numericInput) bleiben draussen, statt
  # Vorschau und Vergleich mit einem Validierungsfehler zu fluten.
  massnahmen_paket <- reactive({
    kat <- massnahmen_katalog()
    pk  <- list()
    for (i in seq_len(nrow(kat))) {
      id <- kat$id[i]
      if (!isTRUE(input[[paste0("mn_", id)]])) next
      if (identical(kat$typ[i], "treiber")) {
        d <- input[[paste0("mn_", id, "_delta")]]
        if (!isTRUE(is.finite(d))) next
        pk[[length(pk) + 1L]] <- list(id = id, params = list(delta = d))
      } else {
        betrag  <- input[[paste0("mn_", id, "_betrag")]]
        start_m <- input[[paste0("mn_", id, "_start")]]
        dauer   <- input[[paste0("mn_", id, "_dauer")]]
        verlauf <- input[[paste0("mn_", id, "_verlauf")]]
        if (!isTRUE(is.finite(betrag)) || !isTRUE(is.finite(start_m)) ||
            !isTRUE(is.finite(dauer))  || is.null(verlauf)) next
        pk[[length(pk) + 1L]] <- list(id = id, params = list(
          betrag = betrag, start_m = as.integer(start_m),
          dauer = as.integer(dauer), verlauf = verlauf))
      }
    }
    pk
  })

  # Delta-Vorschau (Reiter 2): MC-FREIE Sofort-Projektion des delta-Anteils
  # auf den VORHANDENEN Simulationsstand sim_rv$out (massnahmen_overlay) —
  # kein neuer Lauf, kein RNG. kk_limit ist dieselbe beim Mappe-Laden
  # gesicherte Modell-Schwelle wie im Deckungs-Schirm (rv$kk_limit).
  # Formatierung NUR hier: EUR ueber fmt(), Prozent eine Nachkommastelle.
  # Ohne Simulationslauf ein regulaerer Hinweis (kein Fehlerzustand).
  output$vorschau_ui <- renderUI({
    out <- sim_rv$out
    kk  <- rv$kk_limit
    if (is.null(out) || is.null(kk))
      return(div(class = "small text-muted",
        "Noch kein Simulationslauf vorhanden — die Delta-Vorschau projiziert ",
        "den delta-Anteil des Pakets auf das Ergebnis des Simulations-Schirms ",
        "(zuerst Mappe laden und die Phase 'Simulation' rechnen). Der ",
        "treiber-Anteil wirkt ohnehin erst im Paket-Vergleich."))
    paket <- massnahmen_paket()
    H     <- ncol(out$B)
    q     <- tryCatch(quantifiziere_paket(paket, H), error = function(e) e)
    if (inherits(q, "error"))
      return(div(class = "alert alert-warning py-2",
                 "Paket ungueltig: ", conditionMessage(q)))
    res <- massnahmen_overlay(out, q$delta_monat)
    div(
      tags$ul(
        tags$li("Tiefpunkt Median: ",
                kz("vorschau-trough-median-vorher", fmt(median(out$trough))),
                " EUR -> ",
                kz("vorschau-trough-median-nachher", fmt(median(res$trough))),
                " EUR"),
        tags$li("P(KK-Linie reisst, Limit ", fmt(kk), " EUR): ",
                kz("vorschau-p-breach-vorher",
                   sprintf("%.1f", 100 * mean(out$trough < -kk))),
                " % -> ",
                kz("vorschau-p-breach-nachher",
                   sprintf("%.1f", 100 * mean(res$trough < -kk))),
                " %"),
        tags$li("Plan-Tiefpunkt: ",
                kz("vorschau-plan-trough-vorher", fmt(out$metrics$plan_trough)),
                " EUR -> ",
                kz("vorschau-plan-trough-nachher", fmt(res$metrics$plan_trough)),
                " EUR")),
      if (length(q$treiber_overrides))
        div(class = "small text-warning",
            "Das Paket traegt treiber-Anteile — sie wirken erst im ",
            "Paket-Vergleich (naechster Simulationslauf); die Vorschau ",
            "projiziert NUR den delta-Anteil.")
    )
  })

  # Paket-Vergleich (Reiter 3): Basis vs. Basis+Paket im mirai-Worker —
  # dasselbe Muster wie start_vergleich (HAS_ASYNC-Degradation,
  # promises::then, Abbrechen ueber stop_mirai). Der Worker komponiert die
  # compare_scenarios-BAUSTEINE statt des Wrappers: compare_scenarios gibt
  # nur die fertige tabelle zurueck und laesst keinen Overlay-Einschub
  # zwischen Simulation und Projektion zu — hier muss der delta-Anteil des
  # Pakets als massnahmen_overlay auf das Paket-out, BEVOR
  # vergleich_from_outs projiziert. Je Szenario GENAU EIN
  # phase_simulation-Lauf, keine zweite Monte-Carlo.
  paket_rv <- reactiveValues(out = NULL, running = FALSE, err = NULL,
                             key = NULL, handle = NULL, cancelled = FALSE)

  start_paket_vergleich <- function(wbpath, paket) {
    paket_rv$running   <- TRUE;  paket_rv$out <- NULL; paket_rv$err <- NULL
    paket_rv$cancelled <- FALSE
    paket_rv$key       <- paste0(wbpath, "|paket|", length(paket))
    m <- mirai::mirai({
      # Die Massnahmen-Funktionen sind im Worker da (der everywhere-Block
      # sourct R/massnahmen.R); wbpath und paket kommen als Datenargumente.
      sc_basis  <- scenario_from_workbook(wbpath)
      sc_paket  <- szenario_mit_paket(wbpath, paket, "Basis")
      q         <- quantifiziere_paket(paket, sc_paket$mdl$HMAX)
      out_basis <- phase_simulation(sc_basis, plot = FALSE)
      out_paket <- massnahmen_overlay(phase_simulation(sc_paket, plot = FALSE),
                                      q$delta_monat)
      list(phase = "massnahmen_vergleich", n = 2L,
           tabelle = vergleich_from_outs(
             list(out_basis, out_paket),
             kk_limits = c(sc_basis$mdl$kk_limit, sc_paket$mdl$kk_limit),
             namen     = c("Basis", sc_paket$name)))
    }, wbpath = wbpath, paket = paket)
    paket_rv$handle <- m
    promises::then(m,
      onFulfilled = function(res) {
        paket_rv$out <- res; paket_rv$running <- FALSE; paket_rv$handle <- NULL
      },
      onRejected = function(err) {
        paket_rv$running <- FALSE; paket_rv$handle <- NULL
        if (!isTRUE(paket_rv$cancelled)) paket_rv$err <- conditionMessage(err)
      })
    invisible()
  }

  # KEIN Auto-Trigger (zwei Simulationslaeufe sind teuer): der Lauf startet
  # nur ueber den Button; Abbrechen ueber stop_mirai (Muster sim/vergleich).
  observeEvent(input$paket_vergleich_start, {
    req(HAS_ASYNC, rv$wbpath)
    start_paket_vergleich(rv$wbpath, massnahmen_paket())
  })

  observeEvent(input$paket_vergleich_abbruch, {
    paket_rv$cancelled <- TRUE
    if (!is.null(paket_rv$handle)) try(mirai::stop_mirai(paket_rv$handle), silent = TRUE)
    paket_rv$running <- FALSE; paket_rv$handle <- NULL
    paket_rv$err <- "Berechnung abgebrochen."
  })

  # Bedienung + Status + Ergebnis in EINEM Output, initial deterministisch
  # befuellt ('kein Vergleich gestartet'). Projiziert AUSSCHLIESSLICH
  # paket_rv$out$tabelle (Spaltenvertrag vergleich_from_outs); Formatierung
  # NUR hier: EUR ueber fmt(), Prozente sprintf('%.1f%%'), insO als ja/—.
  output$paket_vergleich_ui <- renderUI({
    knoepfe <- div(class = "d-flex gap-2 align-items-center mb-2",
      actionButton("paket_vergleich_start", "Paket-Vergleich rechnen",
                   class = "btn-primary btn-sm"),
      actionButton("paket_vergleich_abbruch", "Abbrechen",
                   class = "btn-outline-danger btn-sm"))
    status <-
      if (!HAS_ASYNC)
        div(class = "alert alert-warning py-2 mb-2",
            "Hintergrund-Worker nicht verfuegbar: die Pakete ", tags$code("mirai"),
            " / ", tags$code("promises"), " fehlen. Der Paket-Vergleich laeuft ",
            "bewusst NICHT synchron im UI-Thread — bitte ",
            tags$code("Rscript tests/deps.R"), " ausfuehren.")
      else if (isTRUE(paket_rv$running))
        div(class = "d-flex gap-2 align-items-center py-2 mb-2",
            tags$div(class = "spinner-border spinner-border-sm text-primary", role = "status"),
            span(class = "text-muted",
                 "Paket-Vergleich laeuft im Hintergrund-Worker (mirai) — die Oberflaeche bleibt bedienbar."))
      else if (!is.null(paket_rv$err))
        div(class = "alert alert-danger py-2 mb-2",
            "Paket-Vergleich nicht abgeschlossen: ", paket_rv$err)
      else if (is.null(paket_rv$out))
        div(class = "small text-muted py-1 mb-2",
            "kein Vergleich gestartet — Massnahmen im Reiter 'Katalog & Paket' ",
            "aktivieren und 'Paket-Vergleich rechnen' starten.")
      else
        div(class = "py-1 mb-2",
            span(class = "text-success small", "Paket-Vergleich abgeschlossen."))
    tabelle <- if (!is.null(paket_rv$out)) {
      tb <- paket_rv$out$tabelle
      div(style = "overflow-x:auto",
        tags$table(class = "table table-striped table-sm w-auto",
          tags$thead(tags$tr(lapply(
            c("Szenario", "Plan-Tiefpunkt", "Tiefpunkt Median", "Tiefpunkt P05",
              "Tiefpunkt P95", "P(KK-Linie reisst)", "P(zahlungsfaehig)",
              "insO erfuellt", "KK-Limit", "S", "Seed"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(tb)), function(i) tags$tr(
            tags$td(tb$szenario[i]),
            tags$td(fmt(tb$plan_trough[i])),
            tags$td(fmt(tb$trough_median[i])),
            tags$td(fmt(tb$trough_p05[i])),
            tags$td(fmt(tb$trough_p95[i])),
            tags$td(sprintf("%.1f%%", tb$P_breach_pct[i])),
            tags$td(sprintf("%.1f%%", tb$P_solvent_pct[i])),
            tags$td(if (isTRUE(tb$insO_erfuellt[i])) "ja" else "—"),
            tags$td(fmt(tb$kk_limit[i])),
            tags$td(sprintf("%d", as.integer(tb$S[i]))),
            tags$td(sprintf("%d", as.integer(tb$seed[i])))
          )))))
    }
    div(knoepfe, status, tabelle)
  })

  # --- Export (Phase 9: phase-09-xlsx + phase-09-docx) -------------------------
  # Die Sammel-Logik lebt als benannte, reine Funktion export_ergebnisse_sammeln
  # in R/export.R (dort dokumentiert; Mutgate-Nachschub — im Reactive hatte sie
  # keinen Testpfad). Der Reactive reicht nur die ROHEN Session-Staende durch;
  # rv$kk_limit ist die beim Mappe-Laden gesicherte Modell-Schwelle.
  export_ergebnisse <- reactive({
    export_ergebnisse_sammeln(
      forecast  = rv$out,
      sim       = sim_rv$out,
      varianz   = var_rv$out,
      vergleich = vergleich_rv$out,
      kk_limit  = rv$kk_limit
    )
  })

  # GoBD-Herkunft (phase-13-pruefbarkeit): die reine Funktion herkunft_bauen
  # traegt die ganze Logik (direkt testbar in tests/test_herkunft.R); der
  # Reactive reduziert sich auf den einen Aufruf. DEFENSIV per is.null-Guard —
  # ohne protokollierten Sim-Lauf (fremdes/altes Ergebnisobjekt ohne gobd-
  # Beobachterfeld aus phase-12-runmanifest-anbindung) liefert er NULL und der
  # Export geht wie bisher OHNE Herkunft-Blaetter raus (Beobachter-Invarianz
  # fuer den reinen Forecast-Export; kein Crash, keine Kaskade). rv$out liefert
  # die drei Anker, sim_rv$out$gobd$manifest_pfad den Lauf-Nachweis — beide aus
  # derselben geladenen Mappe.
  export_herkunft <- reactive({
    g <- sim_rv$out$gobd
    if (is.null(g)) return(NULL)
    req(rv$out)
    herkunft_bauen(rv$out, g$manifest_pfad)
  })

  # downloadHandler schreibt DIREKT im Handler (xlsx-Schreiben ist leicht-
  # gewichtig, kein Worker); unformatierte Zahlen — fmt() bleibt der Anzeige.
  # Bei leerer Sammlung ein validate-Hinweis statt eines Abbruchs.
  output$export_xlsx_btn <- downloadHandler(
    filename = function() "liquiditaetsplanung_export.xlsx",
    content  = function(file) {
      erg <- export_ergebnisse()
      validate(need(length(erg) > 0L, "Erst rechnen, dann exportieren."))
      export_xlsx(erg, file, herkunft = export_herkunft())
    }
  )

  # docx-Schwester (phase-09-docx): derselbe Sammel-Reactive, Medium officer
  # (R/export.R, Zell-Text-Regel dort dokumentiert); Schreiben ist leicht-
  # gewichtig — direkt im Handler, kein Worker.
  output$export_docx_btn <- downloadHandler(
    filename = function() "liquiditaetsplanung_export.docx",
    content  = function(file) {
      erg <- export_ergebnisse()
      validate(need(length(erg) > 0L, "Erst rechnen, dann exportieren."))
      export_docx(erg, file, herkunft = export_herkunft())
    }
  )

  # pdf-Schwester (phase-09-pdf): derselbe Sammel-Reactive, Medium Quarto/
  # typst (R/export.R; Render ~4 s, verprobt) — bleibt im Handler, kein
  # Worker (Praezedenz xlsx/docx).
  output$export_pdf_btn <- downloadHandler(
    filename = function() "liquiditaetsplanung_export.pdf",
    content  = function(file) {
      erg <- export_ergebnisse()
      validate(need(length(erg) > 0L, "Erst rechnen, dann exportieren."))
      export_pdf(erg, file, herkunft = export_herkunft())
    }
  )

  # z3-Schwester (phase-13-z3-ui): GoBD-Datentraegerueberlassung. export_z3
  # komponiert das deterministische CSV+index.xml-Buendel (Schicht 1/3 + 2/3)
  # in ein frisches tempdir-Verzeichnis; der Handler baut sein sc frisch aus
  # rv$wbpath/rv$wbname (Praezedenz Z. 191, bewusst NICHT das Forecast-Szenario)
  # und zippt das Buendel als duenne App-Huelle via .z3_zip (Handoff-Vorgabe:
  # primaer utils::zip, Basis-R; faellt nur auf den byte-kompatiblen Basis-R-
  # Store-ZIP zurueck, wenn kein externes zip-Binary ein gueltiges Archiv
  # schreibt). Bei leerer Sammlung derselbe validate-Hinweis wie die Schwestern.
  output$export_z3_btn <- downloadHandler(
    filename = function() "liquiditaetsplanung_export_z3.zip",
    content  = function(file) {
      erg <- export_ergebnisse()
      validate(need(length(erg) > 0L, "Erst rechnen, dann exportieren."))
      sc  <- scenario_from_workbook(rv$wbpath, name = rv$wbname)
      # Pro Download ein EIGENES Buendel-Verzeichnis (tempfile liefert einen im
      # tempdir noch nicht existierenden, prozess-eindeutigen Namen). Kein
      # unlink auf einen festen, prozessweit geteilten Namen: parallele oder
      # reentrante Downloads im selben R-Prozess wuerden sich sonst gegenseitig
      # das Verzeichnis unter den Fuessen wegloeschen/ueberschreiben.
      buendel <- tempfile(pattern = "z3-export-")
      dir.create(buendel, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(buendel, recursive = TRUE), add = TRUE)
      z <- export_z3(erg, sc, buendel)
      .z3_zip(file, z$pfad, z$dateien)
    }
  )

  # Verfahrensdokumentation (phase-15-verfahrensdoku-export): GoBD Rz. 153 als
  # .xlsx. Der Handler baut das VD-Objekt FRISCH ueber verfahrensdoku_erstellen()
  # (Default umgebung_erfassen -- live Systemumgebung) und serialisiert es mit
  # export_verfahrensdoku(vd, file). BEWUSST KEIN validate/need: die VD ist der
  # Prozedur gewidmet, nicht den berechneten Ergebnissen -- der Abruf gelingt
  # auch dann, wenn keine Ergebnis-rvs vorliegen (kein Erst-rechnen-Hinweis,
  # bewusste Abweichung von den Ergebnis-Schwestern). Schreiben ist leicht-
  # gewichtig -- direkt im Handler, kein Worker.
  output$export_verfahrensdoku_btn <- downloadHandler(
    filename = function() "liquiditaetsplanung_verfahrensdoku.xlsx",
    content  = function(file) {
      vd <- verfahrensdoku_erstellen()
      export_verfahrensdoku(vd, file)
    }
  )

  # Neues Mandat (phase-10-ui): content ruft GENAU new_mandate(file) — keine
  # Session-Objekte, keine Simulation; Mappen-Schreiben ist leichtgewichtig
  # (Praezedenz Export-Handler), kein Worker. Das Datum steht nur im
  # DATEINAMEN — der Zellinhalt bleibt der deterministische new_mandate-
  # Vertrag.
  output$new_mandate_btn <- downloadHandler(
    filename = function() sprintf("Mandant_NEU_%s.xlsx", format(Sys.Date(), "%Y-%m-%d")),
    content  = function(file) new_mandate(file)
  )

  output$statusbar <- renderUI({
    o <- rv$out
    if (is.null(o)) return(span(class = "text-muted", "Bereit. Mappe ueber 'Demo laden' oder Datei-Upload waehlen."))
    div(class = "d-flex gap-2 align-items-center flex-wrap",
        strong("Konsistenz:"),
        badge(o$diagnostics$gv_konsistenz,   "GuV-Identitaet",  id = "status-guv-identitaet"),
        badge(o$diagnostics$massenerhaltung, "Massenerhaltung", id = "status-massenerhaltung"),
        badge(o$diagnostics$zahlungsfaehig,  "Zahlungsfaehig",  id = "status-zahlungsfaehig"),
        span(class = "text-muted ms-2",
             kz("status-monate-unter-reserve", sprintf("%d", o$diagnostics$monate_unter_reserve)),
             " Monat(e) unter Mindestreserve"))
  })
}

shinyApp(ui, server)
