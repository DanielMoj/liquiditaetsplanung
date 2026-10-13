# ============================================================================
#  R/export.R  —  Phase 9 (Export der Ergebnisobjekte; Einheiten
#  phase-09-xlsx, phase-09-docx und phase-09-pdf)
#
#  Gemeinsamer Kern ist export_bloecke_bauen: Validierung + benannte
#  Block-Liste (data.frames) in EINGABEREIHENFOLGE — EINE Quelle fuer die
#  Vertragskonstanten, die alle Medienschichten projizieren:
#    * export_xlsx — je Block ein Blatt gleichen Namens (openxlsx2),
#      native Typen; Rueckgabe list(pfad, blaetter).
#    * export_docx — je Block ein Abschnitt (officer): Ueberschrift im Stil
#      'heading 1' mit dem Blocknamen + GENAU EINE Tabelle; Rueckgabe
#      list(pfad, abschnitte). abschnitte == blaetter derselben Eingabe.
#    * export_pdf  — je Block ein Abschnitt (Quarto-CLI, typst-Format):
#      Ueberschrift Ebene 1 + GENAU EINE Pipe-Tabelle; Rueckgabe
#      list(pfad, abschnitte) wie docx. Text-Zellen beider Medien kommen
#      aus der gemeinsamen Zellregel export_zellen_als_text.
#
#  Blockformen (bindend: tests/test_export_xlsx.R + tests/test_export_docx.R;
#  KEIN Legacy-Vorbild — Zielort R/export.R laut ARCHITECTURE.md):
#    * forecast         — zaehlt als ZWEI Bloecke (gepinnte Blattstruktur-
#      Entscheidung): Block "forecast" = cash-Matrix als data.frame mit den
#      Zeilennamen als erster Spalte "Position", danach M1..MH; Block
#      "forecast_kpis" = kpis als Key/Wert-Paare (kennzahl, wert) in
#      Vertragsreihenfolge trough, trough_month, maxKK, months_below,
#      result, end. Weitere Felder (guv, dso_dpo, ...) landen NICHT im Block.
#    * sim_monate       — table (data.frame) unveraendert.
#    * sim_deckung      — Key/Wert-Block (kennzahl, wert) in Vertragsreihen-
#      folge trough_median, trough_p05, trough_p95, P_breach, P_solvent,
#      P_breach_pct, P_solvent_pct, insO_erfuellt, kk_limit, seed, S
#      (seed VOR S). insO_erfuellt als 0/1 via as.numeric, damit die
#      wert-Spalte homogen numerisch bleibt (gepinnte Entscheidung).
#    * varianzzerlegung — Key/Wert-Block der var_/sd_/anteil_-Felder plus
#      S/seed in Vertragsreihenfolge var_gesamt, var_epi, var_ale, var_inter,
#      sd_gesamt, sd_epi, sd_ale, anteil_epi, anteil_ale, anteil_inter, S,
#      seed (S VOR seed — bewusst ASYMMETRISCH zu sim_deckung; die Asymmetrie
#      killt jede Block-/Objektvertauschung). T_*/Theta landen NICHT im Block.
#    * vergleich        — tabelle (data.frame) unveraendert.
#
#  Nur Zahlen/Strings, unformatiert — fmt() bleibt der UI; keine Simulation,
#  kein Grafikgeraet, deterministisch (kein Zeitstempel im Inhalt; die
#  docx-Metadaten in core.xml stammen von officer und zaehlen nicht zum
#  Inhalt — verglichen wird, was docx_summary liest). Fehler (aus dem
#  Bauer): unbekannter Listenname (Fehlertext enthaelt den Namen), leere
#  Liste, unbenannte Liste.
#
#  Nebenlaeufigkeit: worker-/downloadHandler-sicher — keine Globals, keine
#  Konsolen-Prints; alle Vertragskonstanten leben in den Funktionen.
# ============================================================================

export_bloecke_bauen <- function(ergebnisse) {
  stopifnot(
    is.list(ergebnisse),
    length(ergebnisse) >= 1L,
    !is.null(names(ergebnisse)),
    all(nzchar(names(ergebnisse)))
  )

  bekannt   <- c("forecast", "sim_monate", "sim_deckung", "varianzzerlegung",
                 "vergleich")
  unbekannt <- setdiff(names(ergebnisse), bekannt)
  if (length(unbekannt))
    stop(sprintf("export_bloecke_bauen: unbekannte Ergebnisnamen: %s (unterstuetzt: %s).",
                 paste(unbekannt, collapse = ", "),
                 paste(bekannt,   collapse = ", ")))

  # Key/Wert-Block: kennzahl/wert in der UEBERGEBENEN Vertragsreihenfolge;
  # as.numeric haelt die wert-Spalte homogen numerisch (insO_erfuellt -> 0/1).
  kv <- function(obj, keys)
    data.frame(kennzahl = keys,
               wert     = unname(vapply(keys, function(k) as.numeric(obj[[k]]),
                                        numeric(1L))),
               stringsAsFactors = FALSE)

  # Vertragsreihenfolgen der Key/Wert-Bloecke: deckung mit seed VOR S,
  # varianz mit S VOR seed — die Asymmetrie ist Teil des Vertrags.
  kpi_keys     <- c("trough", "trough_month", "maxKK", "months_below",
                    "result", "end")
  deckung_keys <- c("trough_median", "trough_p05", "trough_p95", "P_breach",
                    "P_solvent", "P_breach_pct", "P_solvent_pct",
                    "insO_erfuellt", "kk_limit", "seed", "S")
  varianz_keys <- c("var_gesamt", "var_epi", "var_ale", "var_inter",
                    "sd_gesamt", "sd_epi", "sd_ale",
                    "anteil_epi", "anteil_ale", "anteil_inter", "S", "seed")

  # Bloecke in Eingabereihenfolge aufbauen (forecast expandiert zu zwei).
  bloecke <- list()
  for (nm in names(ergebnisse)) {
    obj <- ergebnisse[[nm]]
    bloecke <- c(bloecke, switch(nm,
      forecast = {
        cash <- obj$cash
        fc <- cbind(data.frame(Position = rownames(cash),
                               stringsAsFactors = FALSE),
                    as.data.frame(cash, stringsAsFactors = FALSE))
        rownames(fc) <- NULL
        list(forecast = fc, forecast_kpis = kv(obj$kpis, kpi_keys))
      },
      sim_monate       = list(sim_monate       = obj$table),
      sim_deckung      = list(sim_deckung      = kv(obj, deckung_keys)),
      varianzzerlegung = list(varianzzerlegung = kv(obj, varianz_keys)),
      vergleich        = list(vergleich        = obj$tabelle)
    ))
  }
  bloecke
}

# herkunft = NULL (Default) liefert BIT-IDENTISCHES Verhalten wie bisher
# (Regressionspin der Schwester-Tests). herkunft = list(stempel, herkunft)
# (aus herkunft_bauen) haengt ZWEI Beobachter-Blaetter 'GoBD_Stempel' +
# 'Herkunft' ANS ENDE an, NACH den Datenblaettern — die Datenblaetter werden
# NICHT beruehrt (Beobachter-Invarianz, Blockprinzip c17d29a); export_bloecke_
# bauen bleibt unangetastet (Herkunft ist kein ergebnis-Block).
export_xlsx <- function(ergebnisse, pfad, herkunft = NULL) {
  blaetter <- export_bloecke_bauen(ergebnisse)

  wb <- openxlsx2::wb_workbook()
  for (b in names(blaetter)) {
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = b)
    wb <- openxlsx2::wb_add_data(wb, sheet = b, x = blaetter[[b]])
  }

  zusatz <- .export_herkunft_blaetter(herkunft)
  for (nm in names(zusatz)) {
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = nm)
    wb <- openxlsx2::wb_add_data(wb, sheet = nm, x = zusatz[[nm]])
  }
  openxlsx2::wb_save(wb, file = pfad, overwrite = TRUE)

  list(pfad = pfad, blaetter = c(names(blaetter), names(zusatz)))
}

# ---------------------------------------------------------------------------
# .export_herkunft_blaetter: gemeinsame Projektion der optionalen Herkunft auf
# die zwei Beobachter-Bloecke in Vertragsreihenfolge (GoBD_Stempel VOR
# Herkunft). herkunft = NULL -> leere benannte Liste (keine Zusatzblaetter,
# bit-identisches Alt-Verhalten). Die Namen sind EXAKT die xlsx-Blattnamen und
# zugleich die docx-/pdf-Abschnittsnamen (Symmetrie ueber die Medien).
# ---------------------------------------------------------------------------
.export_herkunft_blaetter <- function(herkunft) {
  if (is.null(herkunft)) return(stats::setNames(list(), character(0)))
  list(GoBD_Stempel = herkunft$stempel, Herkunft = herkunft$herkunft)
}

# ---------------------------------------------------------------------------
# export_docx: derselbe Block-Bauer, Medium docx (officer). Je Block ein
# Abschnitt in Eingabereihenfolge: Ueberschrift im Stil 'heading 1' (Text =
# Blockname, exakt die xlsx-Blattnamen) + GENAU EINE Tabelle.
#
# ZELL-TEXT-REGEL: siehe export_zellen_als_text — gemeinsame Regel der
# Text-Medien (docx + pdf), hier nur angewendet.
# ---------------------------------------------------------------------------
# herkunft = NULL -> bit-identisch (Regressionspin). herkunft =
# list(stempel, herkunft) -> ZWEI zusaetzliche Abschnitte 'GoBD_Stempel' +
# 'Herkunft' ANS ENDE, je Ueberschrift 'heading 1' + GENAU EINE Tabelle ueber
# die gemeinsame Zellregel — symmetrisch zu export_xlsx(...)$blaetter.
export_docx <- function(ergebnisse, pfad, herkunft = NULL) {
  bloecke <- export_bloecke_bauen(ergebnisse)
  zusatz  <- .export_herkunft_blaetter(herkunft)

  d <- officer::read_docx()
  for (b in c(names(bloecke), names(zusatz))) {
    df <- if (b %in% names(bloecke)) bloecke[[b]] else zusatz[[b]]
    d <- officer::body_add_par(d, b, style = "heading 1")
    d <- officer::body_add_table(d, export_zellen_als_text(df))
  }
  print(d, target = pfad)

  list(pfad = pfad, abschnitte = c(names(bloecke), names(zusatz)))
}

# ---------------------------------------------------------------------------
# export_zellen_als_text: GEMEINSAME Zell-Text-Regel der Text-Medien (docx
# ueber officer, pdf ueber Quarto/typst) — gepinnte Entscheidung
# (tests/test_export_docx.R + tests/test_export_pdf.R): JEDE Spalte per
# as.character — Zahlen in der R-eigenen 15-Stellen-Darstellung, logicals
# als TRUE/FALSE, die wert-Spalte der Key/Wert-Bloecke als Text der homogen
# numerischen Werte (insO als '1'/'0'). Medienseitige Formatierer (officer-
# format() beim body_add_table wuerde '1.50' auffuellen) werden damit
# bewusst umgangen; die Ruecklesung via as.numeric bzw. der stringgenaue
# Zeilenvergleich ist fuer Vertragszahlen exakt.
# ---------------------------------------------------------------------------
export_zellen_als_text <- function(df) {
  df[] <- lapply(df, as.character)
  df
}

# ---------------------------------------------------------------------------
# quarto_bin_finden: loest das Quarto-CLI env-unabhaengig auf. Noetig, weil
# laufende Prozessbaeume nach einer frischen Quarto-Installation den alten
# PATH erben (beobachtet 2026-07-07: CLI unter C:/Program Files/Quarto
# vorhanden, PATH und quarto::quarto_path() leer). Kette: QUARTO_PATH ->
# quarto::quarto_path() -> Sys.which -> Windows-Standardpfade -> benannter
# stop. Dieselbe Kette prueft tests/deps.R als Sichtpruefung.
# ---------------------------------------------------------------------------
quarto_bin_finden <- function() {
  kand <- Sys.getenv("QUARTO_PATH")
  if (!nzchar(kand))
    kand <- tryCatch(as.character(quarto::quarto_path()), error = function(e) "")
  if (!length(kand) || is.na(kand) || !nzchar(kand))
    kand <- as.character(Sys.which("quarto"))
  if (!length(kand) || !nzchar(kand)) {
    for (k in c("C:/Program Files/Quarto/bin/quarto.exe",
                file.path(Sys.getenv("LOCALAPPDATA"),
                          "Programs/Quarto/bin/quarto.exe"))) {
      if (file.exists(k)) { kand <- k; break }
    }
  }
  if (!length(kand) || !nzchar(kand) || !file.exists(kand))
    stop("quarto_bin_finden: kein Quarto-CLI gefunden — QUARTO_PATH setzen ",
         "oder Quarto installieren (https://quarto.org/docs/get-started/).")
  kand
}

# ---------------------------------------------------------------------------
# export_pdf: derselbe Block-Bauer, Medium pdf (Quarto-CLI, typst-Format —
# LaTeX-frei, typst ist im CLI gebuendelt). Je Block ein Abschnitt in
# Eingabereihenfolge: Ueberschrift Ebene 1 ('# <name>') + GENAU EINE
# Pipe-Tabelle; Zellen ueber die gemeinsame Zellregel, Pipe-Zeichen in
# Zellen escaped.
#
# LAYOUT-REGIME (gepinnt, Teil des Vertrags — die Ruecklese-Orakel matchen
# whitespace-gestripte ZEILEN): margin x=1cm/y=1.2cm, fontsize 9pt. Unter
# diesem Regime brechen Datenzeilen nicht um (verprobt bis 13 Spalten im
# vergleich und 17-Zeichen-Zellen der Fixture-Naht); typst setzt das
# Minuszeichen als U+2212 — das normalisiert die Leseform der Tests.
# KEIN Datum/Autor/Zeitstempel im Dokumentinhalt: pdf_text zweier Renders
# derselben Eingabe ist identisch (PDF-Metadaten duerfen abweichen).
# Der Render laeuft in einem Temp-Arbeitsverzeichnis; das erzeugte pdf
# wird nach pfad kopiert.
# ---------------------------------------------------------------------------
# herkunft = NULL -> bit-identisch (Regressionspin; KEIN Datum/Autor/
# Zeitstempel im Inhalt bleibt gewahrt). herkunft = list(stempel, herkunft) ->
# ZWEI zusaetzliche Abschnitte ANS ENDE, je Ebene-1-Ueberschrift +
# GENAU EINE Pipe-Tabelle im gepinnten Layout-Regime — symmetrisch zu xlsx/docx.
export_pdf <- function(ergebnisse, pfad, herkunft = NULL) {
  bloecke <- c(export_bloecke_bauen(ergebnisse), .export_herkunft_blaetter(herkunft))

  pipe_zeile <- function(zellen)
    paste0("| ", paste(gsub("|", "\\|", zellen, fixed = TRUE),
                       collapse = " | "), " |")
  tab_md <- function(df) {
    df <- export_zellen_als_text(df)
    c(pipe_zeile(names(df)),
      paste0("|", paste(rep(":---", ncol(df)), collapse = "|"), "|"),
      vapply(seq_len(nrow(df)),
             function(i) pipe_zeile(unlist(df[i, ], use.names = FALSE)),
             character(1)))
  }

  zeilen <- c("---",
              "format:",
              "  typst:",
              "    margin:",
              "      x: 1cm",
              "      y: 1.2cm",
              "    fontsize: 9pt",
              "---",
              "")
  for (b in names(bloecke))
    zeilen <- c(zeilen, paste0("# ", b), "", tab_md(bloecke[[b]]), "")

  arbeits <- tempfile("export_pdf_")
  dir.create(arbeits)
  qmd <- file.path(arbeits, "bericht.qmd")
  writeLines(enc2utf8(zeilen), qmd, useBytes = TRUE)

  bin <- quarto_bin_finden()
  out <- suppressWarnings(system2(bin, c("render", shQuote(qmd), "--quiet"),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  erzeugt <- file.path(arbeits, "bericht.pdf")
  if (status != 0L || !file.exists(erzeugt))
    stop("export_pdf: Quarto-Render fehlgeschlagen (Status ", status, "):\n",
         paste(utils::tail(out, 15L), collapse = "\n"))
  file.copy(erzeugt, pfad, overwrite = TRUE)

  list(pfad = pfad, abschnitte = names(bloecke))
}

# ---------------------------------------------------------------------------
# export_ergebnisse_sammeln: baut die BENANNTE Eingabeliste fuer export_xlsx
# aus den ROHEN Session-Staenden. Herausgezogen aus dem Sammel-Reactive
# export_ergebnisse in app/app.R (Mutgate-Nachschub phase-09-xlsx, Praezedenz
# scenarios_from_specs aus phase-08): die Sammel-Logik lag im Shiny-Reactive
# ohne Testpfad — als benannte, reine Funktion ist sie direkt testbar
# (tests/test_export_xlsx_mutgate.R) und der Reactive reduziert sich auf den
# einen Aufruf. KEINE Simulation, KEINE Formatierung, keine Shiny-Abhaengigkeit.
#
#   * forecast  -> "forecast" UNVERAENDERT (phase_forecast-Vertrag; export_xlsx
#     projiziert selbst selektiv auf cash/kpis).
#   * sim (phase_simulation-Ergebnisvertrag) -> ZWEI Eintraege ueber die
#     GEMEINSAMEN Projektionen, KEINE zweite Monte-Carlo:
#     "sim_monate"  = list(table = project_sim_monate(sim)) — export_xlsx
#     liest ausschliesslich $table; "sim_deckung" = deckung_from_out(sim,
#     kk_limit) — Argumentreihenfolge (out, kk_limit) wie die Signatur in
#     R/phase_sim_deckung.R. kk_limit ist die beim Mappe-Laden gesicherte
#     Modell-Schwelle; fehlt sie bei vorhandenem sim -> benannter stop
#     (die Deckungs-Projektion darf weder stumm entfallen noch mit falscher
#     Schwelle rechnen). Die phase/scenario-Etiketten des alten Reactives
#     entfallen — export_xlsx exportiert sie ohnehin nicht.
#   * varianz   -> "varianzzerlegung" unveraendert.
#   * vergleich -> "vergleich" unveraendert.
#
# NULL-Quellen fallen raus (Filter(Negate(is.null), ...)) — exportiert wird
# nur, was in der Session vorliegt; auch die LEERE Liste ist zulaessige
# Rueckgabe (der validate-Hinweis "Erst rechnen" bleibt Sache des
# downloadHandlers in app/app.R).
#
# Setzt voraus, dass R/phase_sim_monate.R (project_sim_monate) und
# R/phase_sim_deckung.R (deckung_from_out) gesourct sind.
# ---------------------------------------------------------------------------
export_ergebnisse_sammeln <- function(forecast = NULL, sim = NULL,
                                      varianz = NULL, vergleich = NULL,
                                      kk_limit = NULL) {
  if (!is.null(sim) && is.null(kk_limit))
    stop("export_ergebnisse_sammeln: kk_limit fehlt bei vorhandenem sim — ",
         "die sim_deckung-Projektion braucht die Modell-Schwelle.")

  erg <- list(
    forecast    = forecast,
    sim_monate  = if (!is.null(sim)) list(table = project_sim_monate(sim)),
    sim_deckung = if (!is.null(sim)) deckung_from_out(sim, kk_limit),
    varianzzerlegung = varianz,
    vergleich        = vergleich
  )
  Filter(Negate(is.null), erg)
}
