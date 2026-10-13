# ============================================================================
#  tests/test_export_z3_csv.R  —  SPEZIFIKATION (rot zuerst) fuer phase-13-z3-csv
#
#  Schicht 1/3 des GoBD-Z3-Exports (Datentraegerueberlassung, Rz. 158 ff.):
#  export_z3_csv(ergebnisse, sc, pfad) schreibt die EINGABEN, ZWISCHENGROESSEN
#  und ERGEBNISSE eines Laufs als CSV-Dateien in ein Verzeichnis. Die
#  index.xml-Beschreibung baut Schicht 2/3 (export_z3_index), die
#  Komposition + App Schicht 3/3 (export_z3).
#
#  ROT ZUERST: das ungeschuetzte source(R/z3.R) am Kopf bricht ab, solange die
#  Datei fehlt bzw. export_z3_csv undefiniert ist — der Test faellt gegen den
#  AKTUELLEN Stand. Der Builder erfuellt ihn erst mit der Implementierung.
#  KEIN skip(), KEIN succeed(), keine Existenz-Weiche, keine zweite
#  Monte-Carlo (phase_forecast ist deterministisch, kleine Laufzeit).
#
#  Vertrag (handoff/phase-13-z3-csv.json):
#   - Signatur export_z3_csv(ergebnisse, sc, pfad):
#       ergebnisse = BENANNTE Ergebnisobjekt-Liste (gleicher Vertrag wie
#         export_xlsx: forecast, sim_monate, sim_deckung, varianzzerlegung,
#         vergleich — nur die vorhandenen Eintraege);
#       sc = liqui_scenario (liefert die Eingaben);
#       pfad = Zielverzeichnis (rekursiv angelegt, vorhandene Dateien
#         ueberschrieben — wiederausfuehrbar).
#   - CSV-Dateien: UTF-8, Semikolon-Trenner, Dezimalkomma OHNE Tausenderpunkt,
#     CRLF-Zeilenende, kein Quoting. Kopfzeile = Spaltennamen.
#       (1) eingaben.csv = Key/Wert (kennzahl/wert): Mappe, Szenario, HMAX,
#           Sim_Pfade, Sim_Seed, Open_Cash, KK_Zins, KK_Limit, Historie_GuV
#           (= forecast$validation$erg_eng, nur wenn forecast vorliegt) sowie
#           je eine Zeile pro Treiber in sc$assume.
#       (2) je Ergebnis-Block aus export_bloecke_bauen EINE CSV gleichen
#           Namens: forecast -> forecast.csv (Position + M1..MH) und
#           forecast_kpis.csv (kennzahl/wert: trough, trough_month, maxKK,
#           months_below, result, end); sim_monate/sim_deckung/
#           varianzzerlegung/vergleich je eine CSV.
#   - Rueckgabe list(pfad = <uebergebenes Verzeichnis, unveraendert>,
#     dateien = <geschriebene CSV-Namen in Schreibreihenfolge, OHNE index.xml>).
#   - KEIN PDF/A, KEIN index.xml (das ist Schicht 2/3).
#   - Fehler (benannt, Stil manifest_validieren): ergebnisse keine benannte
#     nicht-leere Liste (delegiert an export_bloecke_bauen); sc ohne Klasse
#     liqui_scenario; pfad kein nicht-leerer String / nicht anlegbar (MIT Pfad).
#
#  Orakel-Politik: das Binding-Orakel ist eine EINGEFRORENE Fixture
#  (Blockprinzip c17d29a — vorlagenfreie Einheit bekommt das Orakel in
#  Runde T): ein deterministischer phase_forecast-Lauf auf der Tier-1-Mappe.
#  Die drei Tier-1-Anker werden aus dem VERTRAG gezogen (tests/tier1_baseline.R),
#  NIE aus forecast — sonst haette der Roundtrip-Test null Mutationskraft
#  (Selbstkonsistenz-Loch, s. LESSONS). Toleranz 0: round()==Literal.
#  Datentreue-Check gegen die xlsx-Ruecklesung beweist die EINE Quelle
#  export_bloecke_bauen (R/export.R, unangetastet).
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
source(file.path(.root, "R", "phase_forecast.R"))
source(file.path(.root, "R", "export.R"))
source(file.path(.root, "tests", "tier1_baseline.R"))
# <- existiert noch nicht: ROT ZUERST (ungeschuetzt, bricht den Lauf ab,
#    solange R/z3.R fehlt bzw. export_z3_csv undefiniert ist).
source(file.path(.root, "R", "z3.R"))

WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Leseform (readLines + Semikolon-Split + Dezimalkomma-zu-Punkt) und
# CRLF/Byte-Pruefung ueber readBin — unabhaengig von der Schreibimplementierung.
# ---------------------------------------------------------------------------
.read_raw <- function(pf) readBin(pf, "raw", n = file.info(pf)$size)
.csv_kv <- function(pf, key) {
  if (!file.exists(pf)) return(NA_character_)
  ll <- readLines(pf, warn = FALSE)
  if (length(ll) < 2L) return(NA_character_)
  for (ln in ll[-1L]) {
    f <- strsplit(ln, ";", fixed = TRUE)[[1L]]
    if (length(f) >= 2L && identical(f[[1L]], key)) return(f[[2L]])
  }
  NA_character_
}
.num <- function(s) as.numeric(sub(",", ".", s, fixed = TRUE))
# Optionales UTF-8-BOM tolerieren (der Vertrag verbietet keins; die Kopfzeile
# wird auf die Spaltennamen geprueft, nicht auf BOM-Freiheit).
.strip_bom <- function(s) sub("^﻿", "", s)

# ---------------------------------------------------------------------------
# EINGEFRORENE Fixture: ein deterministischer phase_forecast-Lauf auf der
# Tier-1-Mappe, LAZY memoisiert. Ein gemeinsamer Export in ein frisches
# tempdir-Buendel, ebenfalls memoisiert (kein Doppel-Export im Fan-out).
# ---------------------------------------------------------------------------
.cache <- new.env(parent = emptyenv())
.sc <- scenario_from_workbook(WB, name = "z3csvSpec")
delayedAssign("fc", phase_forecast(.sc), assign.env = .cache)
delayedAssign("bundle", local({
  out <- file.path(tempdir(), "z3-csv-spec")
  unlink(out, recursive = TRUE)
  r  <- export_z3_csv(list(forecast = .cache$fc), .sc, out)
  list(pfad = out, rueck = r, dateien = list.files(out))
}), assign.env = .cache)
fc     <- function() .cache$fc
bundle <- function() .cache$bundle

# ===========================================================================
#  (1) Rueckgabe-Vertrag
# ===========================================================================

# ---------------------------------------------------------------------------
# export_z3_csv liefert list(pfad, dateien); pfad ist identical dem
# uebergebenen Verzeichnis und existiert; dateien enthaelt eingaben.csv,
# forecast.csv, forecast_kpis.csv und KEIN index.xml (Schicht 2/3) und keine
# .pdf/.pdfa-Datei.
# ---------------------------------------------------------------------------
test_that("Rueckgabe: list(pfad, dateien); pfad == Zielverzeichnis, CSV-Buendel ohne index.xml/PDF", {
  b <- bundle()
  expect_true(is.list(b$rueck))
  expect_identical(names(b$rueck), c("pfad", "dateien"))
  expect_identical(b$rueck$pfad, b$pfad)
  expect_true(dir.exists(b$pfad))

  bn <- basename(b$dateien)
  expect_true(all(c("eingaben.csv", "forecast.csv", "forecast_kpis.csv") %in% bn))
  # Rueckgabe-dateien vs. tatsaechlich geschriebene Dateien: dieselbe Menge.
  expect_setequal(basename(b$rueck$dateien), bn)
  # index.xml ist Schicht 2/3 — hier tabu; kein PDF/A.
  expect_false("index.xml" %in% bn)
  expect_false("index.xml" %in% basename(b$rueck$dateien))
  expect_false(any(grepl("\\.(pdf|pdfa)$", bn, ignore.case = TRUE)))
  # Alle zurueckgemeldeten Dateien sind .csv (Schicht 1/3 schreibt nur CSV).
  expect_true(all(grepl("\\.csv$", basename(b$rueck$dateien), ignore.case = TRUE)))
})

# ===========================================================================
#  (2) CSV-Format: UTF-8, CRLF, Semikolon, Dezimalkomma ohne Tausenderpunkt
# ===========================================================================

# ---------------------------------------------------------------------------
# Jede geschriebene CSV: gueltiges UTF-8; jedes \n von \r eingeleitet (CRLF,
# roh ueber readBin); jede nicht-leere Zeile enthaelt ';'; KEINE Zelle matcht
# [0-9][.][0-9] (Dezimalkomma statt -punkt, kein Tausenderpunkt) und keine
# wissenschaftliche Notation. Integrale bleiben punkt- und kommalos.
# ---------------------------------------------------------------------------
test_that("CSV-Format: UTF-8 + CRLF + Semikolon + Dezimalkomma (kein '.', keine Exp-Notation)", {
  b <- bundle()
  csvs <- file.path(b$pfad, basename(b$dateien))
  csvs <- csvs[grepl("\\.csv$", csvs, ignore.case = TRUE)]
  expect_true(length(csvs) >= 3L)

  for (pf in csvs) {
    rb <- .read_raw(pf)
    ch <- rawToChar(rb)
    Encoding(ch) <- "UTF-8"
    # UTF-8-Gueltigkeit.
    expect_true(validUTF8(ch), info = basename(pf))
    # CRLF: jedes Zeilenvorschub-Byte (0x0A) hat unmittelbar davor 0x0D.
    nl <- which(rb == as.raw(0x0a))
    expect_true(length(nl) >= 1L, info = paste("kein Zeilenumbruch:", basename(pf)))
    expect_true(all(nl > 1L) && all(rb[nl - 1L] == as.raw(0x0d)),
                info = paste("CRLF verletzt:", basename(pf)))
    # Semikolon: jede nicht-leere Zeile traegt ';'.
    ll <- readLines(pf, warn = FALSE)
    expect_true(all(grepl(";", ll[nzchar(ll)], fixed = TRUE)),
                info = paste("Semikolon fehlt:", basename(pf)))
    # Dezimalkomma: keine Zelle traegt Dezimalpunkt/Tausenderpunkt ...
    expect_false(grepl("[0-9][.][0-9]", ch),
                 info = paste("Dezimal-/Tausenderpunkt:", basename(pf)))
    # ... und keine wissenschaftliche Notation (1e+06 etc.).
    expect_false(grepl("[0-9][eE][+-]?[0-9]", ch),
                 info = paste("wissenschaftliche Notation:", basename(pf)))
  }

  # Kopfzeilen der Key/Wert-Dateien exakt kennzahl;wert; forecast.csv beginnt
  # mit Position;M1 (cash-Matrix, erste Spalte Position).
  eg <- readLines(file.path(b$pfad, "eingaben.csv"), warn = FALSE)
  kp <- readLines(file.path(b$pfad, "forecast_kpis.csv"), warn = FALSE)
  fc <- readLines(file.path(b$pfad, "forecast.csv"), warn = FALSE)
  expect_identical(.strip_bom(eg[[1L]]), "kennzahl;wert")
  expect_identical(.strip_bom(kp[[1L]]), "kennzahl;wert")
  expect_true(grepl("^Position;M1", .strip_bom(fc[[1L]])))
})

# ===========================================================================
#  (3) Tier-1-Roundtrip — UNABHAENGIGES Orakel, Toleranz 0
# ===========================================================================

# ---------------------------------------------------------------------------
# Die drei Tier-1-Anker ueberleben den Dezimalkomma-CSV-Roundtrip mit
# Toleranz 0 (round()==Literal). Die Referenzwerte kommen aus dem VERTRAG
# (tests/tier1_baseline.R), NICHT aus forecast — sonst waere der Test
# selbstkonsistent und ohne Mutationskraft.
# ---------------------------------------------------------------------------
test_that("Tier-1-Roundtrip: trough/result (forecast_kpis) und Historie_GuV (eingaben), Toleranz 0", {
  b   <- bundle()
  fkp <- file.path(b$pfad, "forecast_kpis.csv")
  egb <- file.path(b$pfad, "eingaben.csv")

  expect_identical(round(.num(.csv_kv(fkp, "trough"))),  TIER1_BASELINE$trough)
  expect_identical(round(.num(.csv_kv(fkp, "result"))),  TIER1_BASELINE$result)
  expect_identical(round(.num(.csv_kv(egb, "Historie_GuV"))), TIER1_BASELINE$history)

  # Literale explizit gegen die eingefrorenen Kopfzahlen — der Vertrag
  # (CLAUDE.md) haelt sie unantastbar; driftet einer, ist die Aenderung
  # unzulaessig.
  expect_identical(TIER1_BASELINE$trough,  -5135990)
  expect_identical(TIER1_BASELINE$result,   2672411)
  expect_identical(TIER1_BASELINE$history, -5235600)
})

# ===========================================================================
#  (4) Dezimalkomma positiv — der KK-Zins traegt genau ein ','
# ===========================================================================

# ---------------------------------------------------------------------------
# Der KK_Zins-Eingabewert (sc$mdl$kk_zins, nicht-ganzzahlig) steht in
# eingaben.csv mit genau einem ',' (kein '.') und rundet identisch zum
# Modellwert. Positive Gegenprobe zur Punkt-Verbots-Regel aus (2): das
# Format ist nicht durch Wegformatieren aller Nachkommastellen erschlichen.
# ---------------------------------------------------------------------------
test_that("Dezimalkomma positiv: KK_Zins mit genau einem ',' und identischem Wert", {
  b  <- bundle()
  kk <- .sc$mdl$kk_zins
  # Fixture-Vorbedingung: der KK-Zins ist dezimal (6.5% p.a. = 0.065).
  expect_true(kk != round(kk))

  zelle <- .csv_kv(file.path(b$pfad, "eingaben.csv"), "KK_Zins")
  expect_false(is.na(zelle))
  expect_true(grepl(",", zelle, fixed = TRUE))
  expect_false(grepl(".", zelle, fixed = TRUE))
  expect_identical(lengths(regmatches(zelle, gregexpr(",", zelle, fixed = TRUE))), 1L)
  expect_equal(.num(zelle), kk, tolerance = 1e-12)
})

# ===========================================================================
#  (5) Datentreue — EINE Quelle export_bloecke_bauen (CSV == xlsx-Ruecklesung)
# ===========================================================================

# ---------------------------------------------------------------------------
# Die forecast_kpis-CSV-Werte (trough, result) sind dieselben wie die
# xlsx-Ruecklesung desselben Ergebnisobjekts (export_xlsx + wb_to_df, Blatt
# forecast_kpis). Beweist: CSV- und xlsx-Schicht projizieren aus derselben
# Quelle export_bloecke_bauen; die CSV-Schicht verbiegt keine Zahl.
# ---------------------------------------------------------------------------
test_that("Datentreue: forecast_kpis-CSV == xlsx-Ruecklesung (eine Quelle export_bloecke_bauen)", {
  b   <- bundle()
  fkp <- file.path(b$pfad, "forecast_kpis.csv")

  xlsx_tmp <- tempfile(fileext = ".xlsx")
  export_xlsx(list(forecast = fc()), xlsx_tmp)
  wb <- openxlsx2::wb_load(xlsx_tmp)
  df <- openxlsx2::wb_to_df(wb, sheet = "forecast_kpis")
  x_val <- function(key) as.numeric(df$wert[match(key, df$kennzahl)])

  expect_equal(.num(.csv_kv(fkp, "trough")), x_val("trough"), tolerance = 1e-9)
  expect_equal(.num(.csv_kv(fkp, "result")), x_val("result"), tolerance = 1e-9)
})

# ===========================================================================
#  (6) Fehlerfaelle — benannt (Stil manifest_validieren)
# ===========================================================================

# ---------------------------------------------------------------------------
# ergebnisse keine benannte nicht-leere Liste (delegiert an
# export_bloecke_bauen); sc ohne Klasse liqui_scenario; pfad kein nicht-leerer
# String / nicht anlegbar (MIT Pfad im Text) -> je benannter Fehler.
# ---------------------------------------------------------------------------
test_that("Fehlerfaelle: ergebnisse leer/unbenannt, sc ohne Klasse, pfad ungueltig/nicht anlegbar", {
  gutdir <- file.path(tempdir(), "z3-csv-err")
  unlink(gutdir, recursive = TRUE)

  # (a) ergebnisse leer bzw. unbenannt -> Fehler (delegiert an export_bloecke_bauen).
  expect_error(export_z3_csv(list(), .sc, gutdir))
  expect_error(export_z3_csv(list(fc()), .sc, gutdir))

  # (b) sc ohne Klasse liqui_scenario -> Fehler.
  sc_roh <- unclass(.sc)
  expect_error(export_z3_csv(list(forecast = fc()), sc_roh, gutdir))
  expect_error(export_z3_csv(list(forecast = fc()), list(name = "x"), gutdir))

  # (c) pfad kein nicht-leerer String -> Fehler.
  expect_error(export_z3_csv(list(forecast = fc()), .sc, ""))
  expect_error(export_z3_csv(list(forecast = fc()), .sc, NA_character_))

  # (d) pfad nicht anlegbar (Verzeichnis unter einer regulaeren Datei) ->
  #     Fehler MIT dem Pfad im Text.
  blocker <- tempfile()
  writeLines("blockiert", blocker)
  bad <- file.path(blocker, "z3")
  err <- tryCatch(export_z3_csv(list(forecast = fc()), .sc, bad),
                  error = function(e) conditionMessage(e))
  expect_true(is.character(err))
  expect_true(grepl(bad, err, fixed = TRUE))
})

# ===========================================================================
#  (7) RNG-Waechter — kein RNG im GoBD-Pfad
# ===========================================================================

# ---------------------------------------------------------------------------
# .Random.seed vor/nach export_z3_csv unveraendert: der GoBD-Export ist
# deterministisch und zieht keinen Zufall (kein RNG, kein Grafikgeraet).
# ---------------------------------------------------------------------------
test_that("RNG-Waechter: .Random.seed vor/nach export_z3_csv identisch", {
  invisible(runif(1))            # .Random.seed sicher initialisieren
  seed_vor <- get(".Random.seed", envir = globalenv())
  out <- file.path(tempdir(), "z3-csv-rng")
  unlink(out, recursive = TRUE)
  export_z3_csv(list(forecast = fc()), .sc, out)
  seed_nach <- get(".Random.seed", envir = globalenv())
  expect_identical(seed_vor, seed_nach)
})
