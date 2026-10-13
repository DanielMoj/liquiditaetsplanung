# ============================================================================
#  tests/test_new_mandate_ui.R  —  Bindungs- und Struktur-Orakel (rot_zuerst:
#  TRUE, Teil der Runde-T-SPEZIFIKATION) fuer phase-10-ui
#
#  Bindender Vertrag (Implementation folgt in Runde I): app/app.R traegt in
#  der Sidebar DIREKT UNTER dem Export-Block einen Block 'Datei -> Neues
#  Mandat' als div mit fester DOM-ID new-mandate; darin der downloadButton
#  new_mandate_btn mit Beschriftung 'Leere Mandatsmappe (.xlsx)' und eine
#  kompakte Vier-Farb-Legende in Anleitungs-Reihenfolge (gelb FFFF00, blau
#  DDEBF7, grau D9D9D9, Kontext/beige EDE7DD; Chip-Hintergruende als
#  Inline-Styles im Container — Teil des Vertrags) mit den WOERTLICHEN
#  Kurztexten aus docs/Anleitung_neuer_Mandant.md Abschnitt 2 (Bedeutungs-
#  Spalte vor der Klammer): gelb 'Eingabe, die du setzt', blau
#  'Pflichtdaten, beobachtet/strukturell', grau 'von der Engine erzeugt
#  oder strukturell fix', Kontext 'reiner Kontext, von der Engine nicht
#  gelesen'. Der downloadHandler ruft im content GENAU new_mandate(file) —
#  keine Session-Objekte, keine Simulation; darum braucht dieses Orakel
#  KEINE Stubs. filename endet auf .xlsx und traegt 'Mandant' im Namen
#  (deterministisch bis auf ein optionales Datum im DATEINAMEN, nie im
#  Inhalt — der Zellinhalt bleibt der deterministische new_mandate-Vertrag).
#
#  Orakel-Politik: der volle zellweise Referenz-Abgleich des Generats lebt
#  in tests/test_new_mandate.R (Einheit phase-10-generator) und wird hier
#  NICHT dupliziert. HIER zaehlt die BINDUNG Handler -> new_mandate: drei
#  billige Vertragsproben gegen die eingefrorene Python-Referenz
#  fixtures/Mandant_NEU.xlsx (Blattliste identical, Z1-Titel von Blatt 01
#  identical, eine 08-Steuersatz-Zelle identical — stets Generat GEGEN
#  Referenz, keine eigenen Wert-Literale) plus Kreuz-Waechter: die Datei
#  traegt NICHT die Blattliste eines export_xlsx-Outputs (der haette z. B.
#  kein 00_Anleitung) — ein Handler, der die falsche Funktion ruft, faellt
#  rot.
#
#  Struktur-Orakel ohne Browser: das Sidebar-HTML wird aus der ui-Definition
#  der App-Umgebung gerendert (htmltools::renderTags). Der Kreuz-Check
#  Chip <-> Text arbeitet segmentweise: die vier Farbcodes stehen in
#  Anleitungs-Reihenfolge, der Kurztext eines Chips steht ZWISCHEN seinem
#  Farbcode und dem des naechsten Chips (Teil des bindenden Vertrags) — so
#  ist beweisbar, dass der gelbe Chip nicht den blauen Text traegt.
#
#  Rot zuerst: weder existiert das div new-mandate im UI noch der Output
#  new_mandate_btn im Server — der Struktur-Check faellt am fehlenden
#  Element, der Handler-Abruf als Fehler; kein skip, kein succeed. Keine
#  Anker-Literale, keine Tier-1-Werte, keine Engine, kleine Laufzeit (EINE
#  Mappen-Erzeugung). Nebenlaeufigkeits-Hygiene wie in den Schwestern
#  (test_export_docx_ui.R): das Sourcen von app/app.R startet bei
#  HAS_ASYNC mirai::daemons(1); die Daemons werden am Dateiende
#  deterministisch geschlossen — auch im Fehlerpfad.
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

# Vier-Farb-Legende des bindenden Vertrags, in Anleitungs-Reihenfolge
# (docs/Anleitung_neuer_Mandant.md, Abschnitt 2 'Farbcodierung'; Kurztext =
# Bedeutungs-Spalte vor der Klammer, woertlich).
.legende <- list(
  gelb    = list(code = "FFFF00", text = "Eingabe, die du setzt"),
  blau    = list(code = "DDEBF7", text = "Pflichtdaten, beobachtet/strukturell"),
  grau    = list(code = "D9D9D9", text = "von der Engine erzeugt oder strukturell fix"),
  kontext = list(code = "EDE7DD", text = "reiner Kontext, von der Engine nicht gelesen")
)

# Rohzellen-Leseform der Generator-Spezifikation (tests/test_new_mandate.R):
# rownames = Blattzeilen, Spalten = Excel-Buchstaben; ohne Zeilen-/Spalten-
# Kompression, damit Koordinaten stabil adressierbar bleiben.
.zelle <- function(wb, blatt, zeile, spalte) {
  d <- openxlsx2::wb_to_df(wb, sheet = blatt, col_names = FALSE,
                           skip_empty_rows = FALSE, skip_empty_cols = FALSE)
  d[zeile, spalte]
}

test_that("Sidebar traegt den Block 'Datei -> Neues Mandat': div new-mandate mit Download-Button und Vier-Farb-Legende", {
  html <- as.character(htmltools::renderTags(.app_env$ui)$html)

  p_nm <- regexpr('id="new-mandate"', html, fixed = TRUE)
  expect_true(p_nm > 0,
              label = "das Sidebar-HTML traegt den Container div id=\"new-mandate\"")
  # Alle weiteren Proben im Bereich AB dem Container — billige Eingrenzung
  # gegen Zufallstreffer ausserhalb des Blocks.
  nm <- substr(html, p_nm, nchar(html))

  # Download-Button mit .xlsx-Beschriftung.
  expect_match(nm, 'id="new_mandate_btn"', fixed = TRUE,
               info = "downloadButton new_mandate_btn im new-mandate-Block")
  expect_match(nm, "Leere Mandatsmappe (.xlsx)", fixed = TRUE,
               info = "Button-Beschriftung mit .xlsx-Endung, woertlich laut Vertrag")

  # Alle VIER Farbcodes als HINTERGRUENDE (background/background-color im
  # Inline-Style), Position je Chip fuer die Segment-Checks.
  pos <- vapply(.legende, function(l) {
    as.integer(regexpr(paste0("background[^;<>]*", l$code), nm,
                       ignore.case = TRUE))
  }, integer(1))
  expect_true(all(pos > 0),
              label = paste0("alle vier Farbcodes als Chip-Hintergrund im Block (fehlend: ",
                             paste(names(.legende)[pos < 0], collapse = ", "), ")"))
  expect_true(all(diff(pos) > 0),
              label = "Chips in Anleitungs-Reihenfolge gelb, blau, grau, kontext (Vertrag; Grundlage des segmentweisen Kreuz-Checks)")

  # Je Chip der Anleitungs-Kurztext IN SEINEM Segment (vom eigenen Farbcode
  # bis zum Farbcode des naechsten Chips).
  grenzen <- c(pos, nchar(nm) + 1L)
  for (i in seq_along(.legende)) {
    seg <- substr(nm, grenzen[i], grenzen[i + 1L] - 1L)
    expect_match(seg, .legende[[i]]$text, fixed = TRUE,
                 info = paste0("Kurztext des Chips '", names(.legende)[i],
                               "' steht in dessen Segment"))
  }

  # Kreuz-Check: der gelbe Chip traegt NICHT den blauen Text — ein Legenden-
  # Bau, der Farben und Texte falsch paart, faellt rot.
  seg_gelb <- substr(nm, grenzen[1L], grenzen[2L] - 1L)
  expect_false(grepl(.legende$blau$text, seg_gelb, fixed = TRUE),
               label = "Kreuz-Check: das gelbe Chip-Segment traegt nicht den blauen Kurztext")
})

test_that("new_mandate_btn-Handler ruft GENAU new_mandate: Vertragsproben gegen die eingefrorene Referenz; filename-Vertrag", {
  ref_pfad <- file.path(.root, "fixtures", "Mandant_NEU.xlsx")
  expect_true(file.exists(ref_pfad),
              label = "Referenz-Waechter: die eingefrorene Python-Referenz fixtures/Mandant_NEU.xlsx liegt vor")
  ref <- openxlsx2::wb_load(ref_pfad)
  ref_blaetter <- unname(openxlsx2::wb_get_sheet_names(ref))

  # Regime-Waechter als harter Fail (kein skip): traegt die Referenz selbst
  # kein 00_Anleitung, ist der Kreuz-Waechter unten unterscheidungsblind.
  expect_true("00_Anleitung" %in% ref_blaetter,
              label = "Regime-Waechter: die Referenz traegt 00_Anleitung — das Unterscheidungsblatt gegen einen export_xlsx-Output")

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> die
                                           # Auto-Trigger (Sim/Varianz) inert.

    # ABRUF: der downloadHandler schreibt die Mappe und liefert ihren Pfad;
    # KEINE Stubs noetig — der Vertrag verbietet Session-Objekte im Handler.
    pfad <- output$new_mandate_btn
    expect_true(file.exists(pfad))

    # (c) filename-Vertrag: .xlsx-Endung, 'Mandant' im Namen; deterministisch
    # bis auf ein optionales Datum im DATEINAMEN — darum kein gepinnter
    # Voll-Name wie bei den Export-Schwestern.
    expect_match(basename(pfad), "\\.xlsx$",
                 info = "Download-Dateiname endet auf .xlsx")
    expect_match(basename(pfad), "Mandant", fixed = TRUE,
                 info = "Download-Dateiname traegt 'Mandant'")

    gen <- openxlsx2::wb_load(pfad)

    # Probe 1: Blattliste identical zur Referenz (Reihenfolge inklusive).
    gen_blaetter <- unname(openxlsx2::wb_get_sheet_names(gen))
    expect_identical(gen_blaetter, ref_blaetter)

    # Kreuz-Waechter: die Datei traegt NICHT die Blattliste eines
    # export_xlsx-Outputs (der haette z. B. kein 00_Anleitung) — ein
    # Handler, der die falsche Funktion ruft, faellt hier rot.
    expect_true("00_Anleitung" %in% gen_blaetter,
                label = "Kreuz-Waechter: das Generat traegt 00_Anleitung und ist damit keine export_xlsx-Ergebnismappe")

    # Probe 2: Z1-Titel von Blatt 01 identical zur Referenz.
    titel_ref <- .zelle(ref, "01_Positionen", "1", "A")
    expect_false(is.na(titel_ref),
                 label = "Referenz-Waechter: die Referenz traegt in 01_Positionen!A1 einen Titel")
    expect_identical(.zelle(gen, "01_Positionen", "1", "A"), titel_ref)

    # Probe 3: eine 08-Steuersatz-Zelle identical zur Referenz (08!B9 —
    # gepinnte Satz-Zelle der Generator-Spezifikation). KEIN voller
    # Zellabgleich: der lebt in tests/test_new_mandate.R.
    satz_ref <- .zelle(ref, "08_Steuern", "9", "B")
    expect_false(is.na(suppressWarnings(as.numeric(as.character(satz_ref)))),
                 label = "Referenz-Waechter: 08_Steuern!B9 traegt in der Referenz einen numerischen Steuersatz")
    expect_identical(.zelle(gen, "08_Steuern", "9", "B"), satz_ref)
  })
})

# Kill-Mechanik M1: der Anker ^Mandant_NEU_\d{4}- faellt bei %d-%m-%Y am zwei-
# stelligen Tagesblock; der Jahres-Waechter killt jede weitere Block-Permutation.
test_that("filename-Vertrag: ISO-Datum (Mutgate-Nachschub M1)", {
  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> die
                                           # Auto-Trigger (Sim/Varianz) inert.
    fname <- basename(output$new_mandate_btn)
    expect_match(fname, "^Mandant_NEU_\\d{4}-\\d{2}-\\d{2}\\.xlsx$",
                 info = "Dateiname traegt ein ISO-Datum %Y-%m-%d (sortierfreundlich); ein Tages-zuerst-Format wie %d-%m-%Y faellt am vierstelligen ersten Block")
    expect_identical(substr(sub("^Mandant_NEU_", "", fname), 1L, 4L),
                     format(Sys.Date(), "%Y"),
                     label = "Plausibilitaets-Waechter: der Jahresblock des Datums ist das laufende Jahr")
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
