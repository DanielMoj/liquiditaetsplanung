# ============================================================================
#  R/new_mandate.R  —  Phase 10 (Einheit phase-10-generator)
#
#  new_mandate(pfad, ist_jahr = 2025, n_pos = 60) schreibt die LEERE
#  Mandatsmappe fuer das Onboarding eines neuen Mandanten: 1:1-Port der
#  Vorlage legacy/build_workbook_neuer_mandant.py (openpyxl) nach openxlsx2.
#  Gleiche 11 Blaetter in gleicher Reihenfolge, gleiche Zellkoordinaten und
#  Texte, gleiche Zahlformat-Strings, Fuellfarben und Arial-10-Schnitte.
#  Layout je Blatt: Titel Z1, Untertitel Z2, Kopfzeile Z4 (die Engine liest
#  read_excel(skip = 3)). Gefuellt bleiben nur strukturelle Schluessel/
#  Beschriftungen, die gesetzlichen Steuersaetze in 08 und der
#  Finanzierungsplan 05 mit 0; alles Mandantenspezifische ist leer, aber
#  vorformatiert und farbcodiert (gelb FFFF00 Eingabe, blau DDEBF7
#  Pflichtdaten, grau D9D9D9 strukturell, beige EDE7DD Kontext/kursiv).
#
#  Parameter: ist_jahr setzt die 12 Ist-Monats-Spaltenkoepfe in 01 (und die
#  Jahres-Beispiele in 01-Untertitel und 03); n_pos die Zahl vorformatierter
#  Leerzeilen (paint-Bereich) in 01_Positionen.
#
#  openxlsx2-Formwahl (bindend fuer tests/test_new_mandate.R):
#   * Leere formatierte Zellen entstehen ueber wb_add_font/wb_add_numfmt/
#     wb_add_fill auf dims OHNE Daten — die Zelle existiert dann nur mit
#     Style-Index (wb_get_cell_style liefert ihn), wb_to_df liest NA.
#   * Text-Zellen erhalten ihren numFmt ueber wb_add_numfmt("@"); das loest
#     rueckwaerts stabil zu "@" auf (Builtin 49 bzw. Custom-Code "@").
#   * Leere Strings der Vorlage ("" in 00_Anleitung) schreiben KEINEN Wert:
#     openpyxl legt solche Zellen nur mit Style an (Wert-Guard im Writer),
#     die Referenz liest dort NA — .nm_put ueberspringt "" daher wie NULL.
#   * Umlaute/Sonderzeichen der Zelltexte stehen literal (UTF-8, exakt wie
#     die Vorlage; R >= 4.2 sourct nativ UTF-8) — so bleibt jede Zeile
#     direkt gegen die Python-Vorlage vergleichbar.
#
#  Deterministisch: kein Zeitstempel und kein Pfad im Zellinhalt. Die
#  Anleitungstexte nennen die Datei mit dem Vorlagen-Literal
#  "Mandant_NEU.xlsx" (DATEINAME der Vorlage), bewusst NICHT mit `pfad` —
#  zwei Laeufe in verschiedene Dateien tragen identischen Zellinhalt.
#  Rueckgabe: list(pfad, blaetter) in Schreibreihenfolge; keine Prints.
#
#  BUDGET-AUSNAHME (dokumentiert): der 1:1-Port traegt bewusst ~300+ Zeilen.
#  NICHTS aus der Vorlage ist weggelassen oder zusammengefasst; jede
#  put/header_sheet/paint-Sequenz steht zeilennah zur Vorlage (Helfer
#  .nm_put/.nm_header_sheet/.nm_paint mit den Parameternamen der Vorlage).
# ============================================================================

# ---------------- Farben (identisch zur Beispielmappe) ----------------
.NM_YEL <- "FFFF00"   # Eingabe: du setzt den Wert
.NM_BLU <- "DDEBF7"   # Pflichtdaten: Ist/BWA + Klassifikation
.NM_GRY <- "D9D9D9"   # von der Engine erzeugt / strukturell fix
.NM_CTX <- "EDE7DD"   # reiner Kontext: Engine liest es nicht

# ---------------- Zahlformate (Strings exakt wie die Vorlage) ----------------
.NM_EUR  <- "#,##0\" €\""
.NM_PCT  <- "0.0%"
.NM_CORR <- "0.00"    # in der Vorlage definiert, aber an keine Zelle vergeben
.NM_DAYS <- "0\" Tage\""
.NM_MON  <- "0\" Mon.\""
.NM_HEB  <- "0\"%\""
.NM_DATE <- "DD.MM.YYYY"

# Vorlagen-Literal DATEINAME: erscheint in den Anleitungstexten von 00 —
# fest verdrahtet, damit der Zellinhalt nicht vom Zielpfad abhaengt.
.NM_DATEINAME <- "Mandant_NEU.xlsx"

# put() der Vorlage: EIN Wert an (zeile, spalte) plus Schrift, Fuellung und
# Zahlformat. Fuellungs-Vorrang und Schriftwahl (fett > kursiv bei context)
# exakt wie put(); text = TRUE hat Vorrang vor fmt. wert = NULL oder ""
# schreibt keinen Wert (Zelle nur mit Style).
.nm_put <- function(wb, sheet, zeile, spalte, wert = NULL,
                    yellow = FALSE, blue = FALSE, grey = FALSE, context = FALSE,
                    bold = FALSE, text = FALSE, fmt = NULL) {
  dims <- openxlsx2::wb_dims(rows = zeile, cols = spalte)
  if (!is.null(wert) && !identical(wert, "")) {
    wb <- openxlsx2::wb_add_data(wb, sheet = sheet, x = wert, dims = dims)
  }
  wb <- openxlsx2::wb_add_font(wb, sheet = sheet, dims = dims,
                               name = "Arial", size = "10",
                               bold   = if (bold) "1" else "",
                               italic = if (!bold && context) "1" else "")
  fill <- if (yellow) .NM_YEL else if (blue) .NM_BLU else if (grey) .NM_GRY else if (context) .NM_CTX
  if (!is.null(fill)) {
    wb <- openxlsx2::wb_add_fill(wb, sheet = sheet, dims = dims,
                                 color = openxlsx2::wb_color(hex = fill), pattern = "solid")
  }
  if (text) {          # auch leere Textzellen als "@" -> getippte Werte bleiben Text
    wb <- openxlsx2::wb_add_numfmt(wb, sheet = sheet, dims = dims, numfmt = "@")
  } else if (!is.null(fmt)) {
    wb <- openxlsx2::wb_add_numfmt(wb, sheet = sheet, dims = dims, numfmt = fmt)
  }
  wb
}

# header_sheet() der Vorlage: Titel Z1 (fett), Untertitel Z2, Kopfzeile Z4
# (fett + Textformat "@").
.nm_header_sheet <- function(wb, sheet, titel, untertitel, kopf) {
  wb <- .nm_put(wb, sheet, 1, 1, titel, bold = TRUE)
  wb <- .nm_put(wb, sheet, 2, 1, untertitel)
  for (j in seq_along(kopf)) {
    wb <- .nm_put(wb, sheet, 4, j, kopf[[j]], bold = TRUE, text = TRUE)
  }
  wb
}

# paint() der Vorlage: leere Eingabezellen vorformatieren und faerben
# (Einheit sichtbar, bevor getippt wird). fmt_map: Spaltennummer -> Format;
# je Spalte wird der Zeilenbereich in EINEM dims-Zug gestylt (ohne Daten).
.nm_paint <- function(wb, sheet, zeilen, fmt_map, fill) {
  for (sp in names(fmt_map)) {
    dims <- openxlsx2::wb_dims(rows = zeilen, cols = as.integer(sp))
    wb <- openxlsx2::wb_add_font(wb, sheet = sheet, dims = dims, name = "Arial", size = "10")
    wb <- openxlsx2::wb_add_numfmt(wb, sheet = sheet, dims = dims, numfmt = fmt_map[[sp]])
    wb <- openxlsx2::wb_add_fill(wb, sheet = sheet, dims = dims,
                                 color = openxlsx2::wb_color(hex = fill), pattern = "solid")
  }
  wb
}

new_mandate <- function(pfad, ist_jahr = 2025, n_pos = 60) {
  # ---------------- Eingabevertraege (vor jedem Schreiben) ----------------
  if (!is.character(pfad) || length(pfad) != 1L || is.na(pfad) || !nzchar(pfad)) {
    stop("new_mandate: pfad muss ein nicht-leerer Pfad (character, Laenge 1) sein.")
  }
  if (!is.numeric(ist_jahr) || length(ist_jahr) != 1L || !is.finite(ist_jahr) ||
      ist_jahr %% 1 != 0) {
    stop("new_mandate: ist_jahr muss eine einzelne ganze Zahl sein.")
  }
  if (!is.numeric(n_pos) || length(n_pos) != 1L || !is.finite(n_pos) ||
      n_pos %% 1 != 0 || n_pos < 1) {
    stop("new_mandate: n_pos muss ganzzahlig >= 1 sein.")
  }
  ist_jahr <- as.integer(ist_jahr)
  n_pos    <- as.integer(n_pos)

  blaetter <- c("00_Anleitung", "01_Positionen", "02_Zahlungsprofile",
                "03_Stammdaten_Eroeffnung", "04_Annahmen", "05_Finanzierungsplan",
                "06_Verteilungen", "07_Korrelation", "08_Steuern",
                "09_Darlehen", "10_OPOS")

  # Feste Metadaten (docProps): der Zellinhalt haengt ohnehin nicht daran,
  # aber so ist auch die Datei selbst reproduzierbar.
  wb <- openxlsx2::wb_workbook(creator = "liqui-app",
                               datetime_created = as.POSIXct("2000-01-01", tz = "UTC"))

  # =========================== 00_Anleitung ===========================
  bl <- "00_Anleitung"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  schritte <- list(
    list("Liquiditätsmodell — Mappe für einen NEUEN Mandanten (leer)", TRUE),
    list(paste0("Eine Mappe je Mandat. Reihenfolge unten einhalten. ",
                "Gelb/Blau füllst du; Grau/Beige nicht."), FALSE),
    list("", FALSE),
    list("Onboarding-Reihenfolge", TRUE),
    list(paste0("1.  01_Positionen: je BWA-/GuV-Zeile eine Zeile. Klassifizieren ",
                "(grp, cashprofile, driver, ust) + 12 Ist-Monate (EUR, Aufwand positiv). ",
                "Es müssen GENAU 12 Monate sein."), FALSE),
    list(paste0("2.  02_Zahlungsprofile: Forderungs-/Verbindlichkeitskurven aus dem ",
                "OPOS-Aging. f1 = Anteil im Monat, f2 = Folgemonat … Summe je Profil ≈ 1, ",
                "lückenlos ab f1. Profilname = was du in 01 unter cashprofile ",
                "referenzierst."), FALSE),
    list(paste0("3.  03_Stammdaten & Eröffnung: Forecast-Beginn, ",
                "Zahlungsmittel-Anfangsbestand (open_cash), KK-Linie/-Zins, ",
                "Mindestreserve."), FALSE),
    list(paste0("4.  04_Annahmen: vorwärtsgerichtete Treiber (menge, preis, tarif …) ",
                "als Punktwerte + hmax (Forecast-Monate, ≤ Spalten in 05)."), FALSE),
    list(paste0("5.  05_Finanzierungsplan: Investitionen, Fördermittel, Tilgung, ",
                "Darlehensaufnahme je Monat. 0 lassen, wo nichts passiert."), FALSE),
    list(paste0("6.  08_Steuern: Rechtsform + Status + Sätze. Gesetzliche Sätze sind ",
                "vorbelegt; mandantenspezifisch sind rechtsform, gewst_status, ust_status, ",
                "ust_methode, gewst_hebesatz, entnahme_quote."), FALSE),
    list(paste0("7.  Phase 1:  Rscript 01_forecast.R ", .NM_DATEINAME,
                "   — bis Validierung & Cash-Massenerhaltung sauber sind."), FALSE),
    list(paste0("8.  Phase 2:  Rscript 02_sensitivitaet.R ", .NM_DATEINAME,
                "   — der Tornado zeigt, welche Annahmen den Tiefpunkt bewegen ",
                "(braucht 06 NICHT)."), FALSE),
    list(paste0("9.  06_Verteilungen: NUR die Treiber aus dem Tornado eintragen, mit ",
                "p_min/p_max. Danach Sobol (liest 06) zum Schärfen; Unwichtiges ",
                "wieder raus."), FALSE),
    list(paste0("10. Phase 3:  Rscript 03_verteilungen.R ", .NM_DATEINAME,
                "   — Verteilungen der gewählten Treiber."), FALSE),
    list(paste0("11. 07_Korrelation (nur bei ≥2 abhängigen Treibern) füllen, ",
                "dann Phase 4:  Rscript 04_abhaengigkeit.R ", .NM_DATEINAME), FALSE),
    list(paste0("12. Phase 5:  Rscript 05_simulation.R ", .NM_DATEINAME,
                "   — Monte-Carlo (Endbestand-Verteilung, Schwellen). ",
                "Optional 05b_varianzzerlegung.R."), FALSE),
    list(paste0("Optional jederzeit: 09_Darlehen (Kredite mit Tilgungsplan), ",
                "10_OPOS (offene Posten, Nahbereich), 01b_wochen.R (Wochensicht)."), FALSE),
    list("", FALSE),
    list(paste0("Minimal lauffähig (Phase 1): 01, 02, 03, 04, 05, 08. ",
                "Die Blätter 06/07/09/10 dürfen zunächst leer bleiben."), FALSE),
    list(paste0("Engine + Klassifikation bleiben über Mandate gleich: für denselben ",
                "Typ kannst du die Klassifikation (Spalten C–F in 01) aus einer ",
                "bestehenden Mappe übernehmen und nur die Monate neu eintragen."), FALSE),
    list("", FALSE)
  )
  for (i in seq_along(schritte)) {
    wb <- .nm_put(wb, bl, i, 1, schritte[[i]][[1L]], bold = schritte[[i]][[2L]])
  }
  base <- length(schritte)

  wb <- .nm_put(wb, bl, base + 1L, 1, "Farbcodierung der Felder", bold = TRUE)
  wb <- .nm_put(wb, bl, base + 2L, 1,
                paste0("Gelb — Eingabe: du setzt den Wert (Annahmen, Plan, ",
                       "Eröffnungssalden, Steuersätze, Bandbreiten)."), yellow = TRUE)
  wb <- .nm_put(wb, bl, base + 3L, 1,
                paste0("Blau — Pflichtdaten, beobachtet/strukturell: Ist-/BWA-Werte ",
                       "und Klassifikation in 01."), blue = TRUE)
  wb <- .nm_put(wb, bl, base + 4L, 1,
                paste0("Grau — von der Engine erzeugt oder strukturell fix; stehen ",
                       "lassen (geschätzte cv in 06, Diagonale 1,0 in 07)."), grey = TRUE)
  wb <- .nm_put(wb, bl, base + 5L, 1,
                paste0("Beige/kursiv — reiner Kontext: Engine liest es nicht; ",
                       "nur Orientierung (untere Zeilen in 03)."), context = TRUE)
  wb <- .nm_put(wb, bl, base + 6L, 1,
                paste0("Ohne Farbe — Beschriftungen, Schlüssel, Hinweise, Kopfzeilen. ",
                       "Nicht ausfüllen."))

  t <- base + 8L
  wb <- .nm_put(wb, bl, t, 1, "Erlaubte Werte (von der Engine erkannt)", bold = TRUE)
  wb <- .nm_put(wb, bl, t + 1L, 1,
                "grp  (Spalte C in 01) — bestimmt die GuV-Zuordnung. Genau einer von:")
  wb <- .nm_put(wb, bl, t + 2L, 1,
                paste0("      umsatz · bestand · material · betr_ertrag · sopo · personal ",
                       "· sachkosten · afa · zins_e · zins_a · neutral_e · neutral_a · steuer"))
  wb <- .nm_put(wb, bl, t + 3L, 1,
                paste0("driver  (Spalte E) — bestimmt das Jahreswachstum. Einer von ",
                       "(Tippfehler wächst STILL nicht — also exakt schreiben):"))
  wb <- .nm_put(wb, bl, t + 4L, 1,
                paste0("      menge_preis (Menge×Preis) · menge · material ",
                       "(Menge×Materialinflation) · tarif · energie · pflegebudget ",
                       "· infl (allg. Inflation) · flat (kein Wachstum)"))
  wb <- .nm_put(wb, bl, t + 5L, 1,
                paste0("cashprofile  (Spalte D) — Zahlungszeitpunkt. Schlüsselwort ODER ",
                       "ein Profilname aus 02 (unbekannt = harter Abbruch):"))
  wb <- .nm_put(wb, bl, t + 6L, 1,
                paste0("      inmonth (sofort) · noncash (nie) · annual_jan ",
                       "(Jahresbetrag im M1) · abschlag (gleichmäßig 1/12) · quarterly ",
                       "(Quartalsende) · <Profilname aus 02>"))
  wb <- .nm_put(wb, bl, t + 7L, 1,
                "ust  (Spalte F) — 0 / 0.07 / 0.19. Nur wirksam, wenn 08:ust_status = pflichtig.")
  wb <- .nm_put(wb, bl, t + 8L, 1,
                paste0("Hinweis: 06_Verteilungen nutzt als driver die ANNAHMENAMEN aus 04 ",
                       "(menge, preis, tarif, energie, material, pflegebudget, inflation) — ",
                       "nicht die Transmissions-Tokens der 01-Spalte (menge_preis/infl/flat)."))
  wb <- .nm_put(wb, bl, t + 10L, 1,
                paste0("Farben sind rein visuell; die R-Engine liest die Werte unverändert ",
                       "(read_excel ignoriert Zellfarben)."))
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 122)

  # =========================== 01_Positionen ===========================
  bl <- "01_Positionen"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  monate <- sprintf("%d-%02d", ist_jahr, 1:12)
  wb <- .nm_header_sheet(wb, bl,
    "01_Positionen — Ist-Werte je Monat + Klassifikation (leer)",
    paste0("Eine Zeile je BWA-Position. Spalten C–F klassifizieren (gültige Werte ",
           "siehe 00_Anleitung). Monatsspalten = Ist in EUR, Aufwand positiv. ",
           "GENAU 12 Monate. Monatslabels = ", ist_jahr, "; oben im Skript IST_JAHR ",
           "ändern, um das Jahr zu setzen."),
    c("pos", "label", "grp", "cashprofile", "driver", "ust", monate))
  zeilen <- 5:(4 + n_pos)
  # Klassifikation C,D,E als Text-blau
  wb <- .nm_paint(wb, bl, zeilen, list(`3` = "@", `4` = "@", `5` = "@"), .NM_BLU)
  wb <- .nm_paint(wb, bl, zeilen, list(`6` = .NM_PCT), .NM_BLU)          # ust
  eur12 <- as.list(rep(.NM_EUR, 12)); names(eur12) <- 7:18               # 12 Monatsspalten
  wb <- .nm_paint(wb, bl, zeilen, eur12, .NM_BLU)
  wb <- openxlsx2::wb_freeze_pane(wb, sheet = bl, first_active_row = 5, first_active_col = 3)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl,
                                     cols = c("A", "B", "C", "D", "E", "F"),
                                     widths = c(10, 30, 13, 13, 14, 7))
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = 7:18, widths = 12)

  # =========================== 02_Zahlungsprofile ===========================
  bl <- "02_Zahlungsprofile"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "02_Zahlungsprofile — Anteil der Zahlung je Monat ab Leistung/Rechnung (leer)",
    paste0("Aus OPOS-Aging ableiten. f1 = Anteil im Monat, f2 = Folgemonat usw. ",
           "Summe je Profil ≈ 1, lückenlos ab f1. Der Profilname ist genau das, ",
           "was du in 01 unter cashprofile schreibst."),
    c("profile", "f1", "f2", "f3", "f4", "Anwendung"))
  wb <- .nm_paint(wb, bl, 5:14,
                  list(`2` = .NM_PCT, `3` = .NM_PCT, `4` = .NM_PCT, `5` = .NM_PCT), .NM_YEL)
  wb <- .nm_put(wb, bl, 16, 1,
                paste0("Schlüsselwörter (kein Lag, brauchen keine Zeile): inmonth ",
                       "· noncash · annual_jan · abschlag · quarterly"))
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 14)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "F", widths = 42)

  # =========================== 03_Stammdaten_Eroeffnung ===========================
  bl <- "03_Stammdaten_Eroeffnung"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "03_Stammdaten & Eröffnung (leer)",
    "Gelb = von der Engine gelesen, ausfüllen. Beige/kursiv = reiner Kontext (nicht gerechnet).",
    c("key", "wert", "Hinweis"))
  stamm <- list(   # key, Hinweis, art (none|yellow|ctx), text?
    list("Mandant",           "Name des Mandanten",                          "none",   TRUE),
    list("Branche",           "Branche / Versorgungsstufe",                  "none",   TRUE),
    list("Ist-Zeitraum",      paste0("z. B. ", ist_jahr, "-01 bis ", ist_jahr, "-12"),
                                                                             "none",   TRUE),
    list("Forecast-Beginn",   paste0("erster Forecast-Monat, z. B. ", ist_jahr + 1L, "-01"),
                                                                             "yellow", TRUE),
    list("open_cash",         "Zahlungsmittel zu Forecast-Beginn (= Endbestand Vorperiode)",
                                                                             "yellow", FALSE),
    list("kk_limit",          "Kontokorrentlinie",                           "yellow", FALSE),
    list("kk_zins",           "KK-Zinssatz p.a.",                            "yellow", FALSE),
    list("mindestreserve",    "angestrebte Mindestliquidität",               "yellow", FALSE),
    list("Forderungen",       "Kontext (nicht gerechnet)",                   "ctx",    FALSE),
    list("Verbindlichkeiten", "Kontext",                                     "ctx",    FALSE),
    list("Vorraete",          "Kontext",                                     "ctx",    FALSE),
    list("Rueckstellungen",   "Kontext",                                     "ctx",    FALSE),
    list("Darlehen",          "Kontext",                                     "ctx",    FALSE)
  )
  f03 <- list(open_cash = .NM_EUR, kk_limit = .NM_EUR, mindestreserve = .NM_EUR,
              kk_zins = .NM_PCT, Forderungen = .NM_EUR, Verbindlichkeiten = .NM_EUR,
              Vorraete = .NM_EUR, Rueckstellungen = .NM_EUR, Darlehen = .NM_EUR)
  r <- 5L
  for (s in stamm) {
    ist_ctx <- identical(s[[3L]], "ctx")
    wb <- .nm_put(wb, bl, r, 1, s[[1L]], context = ist_ctx)
    wb <- .nm_put(wb, bl, r, 2, NULL, yellow = identical(s[[3L]], "yellow"),
                  context = ist_ctx, text = s[[4L]], fmt = f03[[s[[1L]]]])
    wb <- .nm_put(wb, bl, r, 3, s[[2L]], context = ist_ctx)
    r <- r + 1L
  }
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = c("A", "B", "C"),
                                     widths = c(18, 22, 48))

  # =========================== 04_Annahmen ===========================
  bl <- "04_Annahmen"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "04_Annahmen — vorwärtsgerichtete Treiber (leer)",
    paste0("Punktwerte des Szenarios. Ab Phase 3 wird der ausgewählte Treiber ",
           "durch eine Verteilung ersetzt."),
    c("key", "wert", "Bedeutung"))
  ann <- list(
    list("menge",        "Fallzahl-/Mengenentwicklung vs. Vorjahr"),
    list("preis",        "Erlöspreis / Landesbasisfallwert"),
    list("tarif",        "Tarifabschluss Personal"),
    list("energie",      "Energiepreis-Pfad"),
    list("material",     "Materialkosteninflation (zusätzlich zur Menge)"),
    list("pflegebudget", "Anpassung Pflegebudget"),
    list("inflation",    "allg. Kostentrend (übrige Sachkosten)"),
    list("hmax",         "Forecast-Monate (ganzzahlig, ≤ Spaltenzahl in 05)")
  )
  r <- 5L
  for (a in ann) {
    wb <- .nm_put(wb, bl, r, 1, a[[1L]])
    wb <- .nm_put(wb, bl, r, 2, NULL, yellow = TRUE,
                  fmt = if (identical(a[[1L]], "hmax")) .NM_MON else .NM_PCT)
    wb <- .nm_put(wb, bl, r, 3, a[[2L]])
    r <- r + 1L
  }
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = c("A", "B", "C"),
                                     widths = c(14, 10, 48))

  # =========================== 05_Finanzierungsplan ===========================
  bl <- "05_Finanzierungsplan"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "05_Finanzierungsplan — je Forecast-Monat (EUR)",
    paste0("Auszahlungen/Zuflüsse außerhalb der GuV. 0 lassen, wo nichts passiert. ",
           "M1 = erster Forecast-Monat."),
    c("Position", sprintf("M%d", 1:12)))
  r <- 5L
  for (name in c("Investitionen", "Foerdermittel", "Tilgung", "Darlehensaufnahme")) {
    wb <- .nm_put(wb, bl, r, 1, name)
    for (j in 2:13) {
      wb <- .nm_put(wb, bl, r, j, 0, yellow = TRUE, fmt = .NM_EUR)
    }
    r <- r + 1L
  }
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 20)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = 2:13, widths = 12)

  # =========================== 06_Verteilungen ===========================
  bl <- "06_Verteilungen"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "06_Verteilungen — Unsicherheit der AUSGEWÄHLTEN Treiber (leer, Phase 3)",
    paste0("ERST nach dem Tornado (Phase 2) füllen: nur die Treiber eintragen, ",
           "die den Tiefpunkt bewegen. mode = Punktwert aus 04. p_min/p_max = ",
           "epistemische Bandbreite (PERT). cv = aleatorische Monatsstreuung ",
           "(0 bei reinen Jahresgrößen)."),
    c("driver", "p_min", "p_max", "cv", "Hinweis"))
  wb <- .nm_paint(wb, bl, 5:12, list(`2` = .NM_PCT, `3` = .NM_PCT, `4` = .NM_PCT), .NM_YEL)
  wb <- .nm_put(wb, bl, 14, 1,
                paste0("Treiber ohne Zeile bleiben deterministisch. driver = Annahmename ",
                       "aus 04 (menge, preis, tarif, energie, material, pflegebudget, ",
                       "inflation)."))
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 14)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "E", widths = 60)

  # =========================== 07_Korrelation ===========================
  bl <- "07_Korrelation"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "07_Korrelation — Abhängigkeit der epistemischen Treiber (leer, Phase 4)",
    paste0("Erst sinnvoll bei ≥2 abhängigen Treibern. Zeilen UND Spalten = ",
           "dieselben Treiber wie in 06, gleiche Reihenfolge."),
    c("driver"))
  wb <- .nm_put(wb, bl, 6, 1,
                paste0("Aufbau nach 06: in A5 abwärts die Treiber, in der Kopfzeile ",
                       "(B4 …) dieselben Namen in gleicher Reihenfolge. Diagonale 1,0 ",
                       "(grau), übrige Zellen gelb mit Wert in [-1, 1], symmetrisch."))
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 14)

  # =========================== 08_Steuern ===========================
  bl <- "08_Steuern"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "08_Steuern — Rechtsform, Status und Sätze",
    paste0("Gesetzliche Sätze sind vorbelegt (selten zu ändern). Mandantenspezifisch: ",
           "rechtsform, gewst_status, ust_status, ust_methode, gewst_hebesatz, ",
           "entnahme_quote."),
    c("key", "wert", "Hinweis"))
  steuern <- list(   # key, default, Hinweis, kind
    list("rechtsform",       "kapital",
         "kapital | personen | einzel  (transparent = personen/einzel)",         "sw"),
    list("gewst_status",     "gewerblich",
         "gewerblich | freiberuflich | gemeinnuetzig",                           "sw"),
    list("ust_status",       "pflichtig",
         "pflichtig | frei (§4 Nr.14 Heilbehandlung = frei)",                    "sw"),
    list("ust_methode",      "soll",
         "soll (nach Leistung) | ist (nach Vereinnahmung, §20 UStG)",            "sw"),
    list("kst_satz",         0.15,
         "Körperschaftsteuersatz — gesetzlich (nur Kapitalgesellschaft)",        "pct"),
    list("soli_satz",        0.055,
         "Solidaritätszuschlag auf die KSt — gesetzlich",                        "pct"),
    list("gewst_messzahl",   0.035,
         "Steuermesszahl Gewerbesteuer — gesetzlich",                            "pct"),
    list("gewst_hebesatz",   400,
         "Hebesatz in % der Gemeinde (z. B. 400 = 400%) — mandantenspezifisch",  "heb"),
    list("gewst_freibetrag", 24500,
         "GewSt-Freibetrag — gesetzlich (nur EK/Personengesellschaft)",          "eur"),
    list("ust_lag",          1,
         "Monate bis USt-Abführung (1; mit Dauerfristverlängerung 2)",           "mon"),
    list("entnahme_quote",   0.30,
         "Steuerentnahme als Anteil des Plangewinns (nur transparente Formen)",  "pct")
  )
  f08 <- list(pct = .NM_PCT, heb = .NM_HEB, eur = .NM_EUR, mon = .NM_MON)
  r <- 5L
  for (s in steuern) {
    ist_text <- identical(s[[4L]], "sw")
    wb <- .nm_put(wb, bl, r, 1, s[[1L]])
    wb <- .nm_put(wb, bl, r, 2, s[[2L]], yellow = TRUE, text = ist_text,
                  fmt = if (ist_text) NULL else f08[[s[[4L]]]])
    wb <- .nm_put(wb, bl, r, 3, s[[3L]])
    r <- r + 1L
  }
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = c("A", "B", "C"),
                                     widths = c(18, 14, 60))

  # =========================== 09_Darlehen ===========================
  bl <- "09_Darlehen"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "09_Darlehen — explizite Kredite mit Tilgungsplan (optional, leer)",
    paste0("Je Zeile ein Kredit. tilgungsart: annuitaet | linear | endfaellig | nur_zins. ",
           "start_m = Auszahlungsmonat (1..hmax); <=0 = bereits valutiert. disagio ",
           "mindert nur die Auszahlung. Leer = ohne Darlehensmodul."),
    c("id", "betrag", "zinssatz", "tilgungsart", "laufzeit_m", "start_m", "disagio"))
  wb <- .nm_paint(wb, bl, 5:10,
                  list(`1` = "@", `2` = .NM_EUR, `3` = .NM_PCT, `4` = "@",
                       `5` = .NM_MON, `6` = .NM_MON, `7` = .NM_PCT), .NM_YEL)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 16)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl,
                                     cols = c("B", "C", "D", "E", "F", "G"),
                                     widths = c(12, 10, 13, 12, 9, 9))

  # =========================== 10_OPOS ===========================
  bl <- "10_OPOS"
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = bl)
  wb <- .nm_header_sheet(wb, bl,
    "10_OPOS — offene Forderungen/Verbindlichkeiten mit Zahlungsziel (optional, leer)",
    paste0("Aus der OPOS-Liste. art = forderung | verbindlichkeit. betrag brutto. ",
           "Termin per faellig_am (ISO-Datum) ODER zahlungsziel_tage. ",
           "wahrscheinlichkeit = Quote (default 1). verzug_tage (default 0). ",
           "ust_satz nur bei ust_methode=ist. Leer = statistische Profile (Carry-in)."),
    c("art", "bezeichnung", "betrag", "faellig_am", "zahlungsziel_tage",
      "wahrscheinlichkeit", "verzug_tage", "ust_satz"))
  wb <- .nm_paint(wb, bl, 5:30,
                  list(`1` = "@", `2` = "@", `3` = .NM_EUR, `4` = .NM_DATE,
                       `5` = .NM_DAYS, `6` = .NM_PCT, `7` = .NM_DAYS, `8` = .NM_PCT), .NM_YEL)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl, cols = "A", widths = 15)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet = bl,
                                     cols = c("B", "C", "D", "E", "F", "G", "H"),
                                     widths = c(26, 12, 13, 17, 17, 12, 10))

  openxlsx2::wb_save(wb, file = pfad, overwrite = TRUE)
  list(pfad = pfad, blaetter = blaetter)
}
