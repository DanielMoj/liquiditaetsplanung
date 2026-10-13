# ============================================================================
#  tests/test_export_z3.R  —  SPEZIFIKATION (rot zuerst) fuer phase-13-z3-ui
#
#  Schicht 3/3 des GoBD-Z3-Exports (Datentraegerueberlassung, Rz. 158 ff.):
#  export_z3(ergebnisse, sc, pfad) KOMPONIERT das Z3-Buendel aus Schicht 1/3
#  (export_z3_csv schreibt die CSVs) und Schicht 2/3 (export_z3_index baut die
#  index.xml). Reine Kern-Funktion, deterministisch (kein Sys.time() im Inhalt,
#  kein RNG, kein Grafikgeraet); ruft KEINE Kernel-Funktion — sie ruft nur
#  export_z3_csv + export_z3_index (unantastbarer Bestand der Vorgaenger-
#  Schichten) auf. Die App-Anbindung (Sidebar-Download als zip) spezifiziert
#  tests/test_export_z3_ui.R.
#
#  ROT ZUERST: das ungeschuetzte source(R/z3.R) am Kopf laedt die Datei;
#  export_z3_csv/export_z3_index sind aus den Vorgaenger-Schichten bereits
#  vorhanden, export_z3 aber noch NICHT. Die Tests rufen export_z3 direkt (ohne
#  Schutz-Weiche) auf — solange die Funktion fehlt, bricht "could not find
#  function" jeden Test ab (Fehler == rot). Der Builder erfuellt sie erst mit
#  der Implementierung. KEIN skip(), KEIN succeed(), keine Existenz-Weiche,
#  keine zweite Monte-Carlo (phase_forecast ist deterministisch, kleine
#  Laufzeit).
#
#  Vertrag (handoff/phase-13-z3-ui.json):
#   - Signatur export_z3(ergebnisse, sc, pfad) wie export_z3_csv; pfad =
#     Zielverzeichnis. Ablauf: (1) csv <- export_z3_csv(ergebnisse, sc, pfad);
#     (2) export_z3_index(pfad, csv$dateien); (3) Rueckgabe
#     list(pfad = <uebergebenes Verzeichnis, unveraendert>,
#          dateien = c(csv$dateien, 'index.xml') in Schreibreihenfolge,
#          index_xml = 'index.xml').
#   - PDF/A-ENTSCHEID (Bestandteil des Vertrags): export_z3 erzeugt KEIN PDF/A.
#     Das Buendel ist CSV + index.xml; die Signatur hat KEINEN PDF/A-Parameter,
#     das Buendel enthaelt KEINE .pdf/.pdfa-Datei, index.xml referenziert
#     ausschliesslich .csv-Dateien.
#   - Fehlerfaelle (ergebnisse/sc/pfad) an export_z3_csv delegiert, wirken
#     ungeaendert durch.
#
#  Orakel-Politik: das Binding-Orakel ist eine EINGEFRORENE Fixture
#  (Blockprinzip c17d29a) — ein deterministischer phase_forecast-Lauf auf der
#  Tier-1-Mappe. Die drei Tier-1-Anker werden aus dem VERTRAG gezogen
#  (tests/tier1_baseline.R), NIE aus forecast — sonst haette der Roundtrip-Test
#  null Mutationskraft (Selbstkonsistenz-Loch, s. LESSONS). Die index.xml-Hashes
#  werden EXTERN ueber sha256_datei(csv) gerechnet (eine Quelle Schicht 2/3),
#  NIE aus index.xml gelesen — kein Selbst-Hash. Toleranz 0.
#
#  BESTANDSSCHUTZ: dieser Test beruehrt (modifiziert) weder kernel/ NOCH
#  R/export.R NOCH R/manifest.R NOCH eine test_export_*/test_manifest*-Datei —
#  alle genannten Dateien werden nur GESOURCT (gelesen).
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
source(file.path(.root, "R", "manifest.R"))   # liefert sha256_datei (externes Orakel)
source(file.path(.root, "R", "export.R"))
source(file.path(.root, "tests", "tier1_baseline.R"))
# <- export_z3 existiert noch NICHT: ROT ZUERST. export_z3_csv/export_z3_index
#    sind aus den Vorgaenger-Schichten bereits in R/z3.R; export_z3 fehlt und
#    faellt beim direkten Aufruf mit "could not find function" gegen den
#    AKTUELLEN Stand jeden Test rot.
source(file.path(.root, "R", "z3.R"))

WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Leseformen — implementierungsunabhaengig ueber readBin/readLines.
# ---------------------------------------------------------------------------
.read_raw <- function(pf) readBin(pf, "raw", n = file.info(pf)$size)
.read_txt <- function(pf) {
  ch <- rawToChar(.read_raw(pf))
  Encoding(ch) <- "UTF-8"
  ch
}
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
# Inhalte zwischen <tag> und </tag> (nicht-gierig, dotall), in Dokumentreihenfolge.
.zwischen <- function(hay, tag) {
  pat <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
  m <- regmatches(hay, gregexpr(pat, hay, perl = TRUE))[[1L]]
  if (!length(m)) return(character(0))
  trimws(sub(sprintf("(?s)^<%s>(.*)</%s>$", tag, tag), "\\1", m, perl = TRUE))
}

# ---------------------------------------------------------------------------
# EINGEFRORENE Fixture: ein deterministischer phase_forecast-Lauf auf der
# Tier-1-Mappe, LAZY memoisiert; ein gemeinsamer export_z3 in ein frisches
# tempdir-Buendel, ebenfalls memoisiert (kein Doppel-Export im Fan-out).
# ---------------------------------------------------------------------------
.cache <- new.env(parent = emptyenv())
.sc <- scenario_from_workbook(WB, name = "z3Spec")
delayedAssign("fc", phase_forecast(.sc), assign.env = .cache)
delayedAssign("bundle", local({
  out <- file.path(tempdir(), "z3-spec-bundle")
  unlink(out, recursive = TRUE)
  r  <- export_z3(list(forecast = .cache$fc), .sc, out)
  list(pfad = out, rueck = r,
       dateien = if (dir.exists(out)) list.files(out) else character(0))
}), assign.env = .cache)
fc     <- function() .cache$fc
bundle <- function() .cache$bundle

# ===========================================================================
#  (1) Rueckgabe-Vertrag — Komposition aus Schicht 1 + 2
# ===========================================================================

# ---------------------------------------------------------------------------
# export_z3(list(forecast=fc), sc, tmpdir) liefert list(pfad, dateien,
# index_xml); pfad identical dem Eingabeverzeichnis und existiert; index_xml ==
# 'index.xml'; dateien enthaelt eingaben.csv, forecast.csv, forecast_kpis.csv
# UND 'index.xml' — index.xml am ENDE (Schreibreihenfolge: erst die CSVs aus
# Schicht 1, dann die index.xml aus Schicht 2). Die zurueckgemeldeten Dateien
# sind exakt die tatsaechlich geschriebenen.
# ---------------------------------------------------------------------------
test_that("Rueckgabe: list(pfad, dateien, index_xml); pfad == Zielverzeichnis; index.xml am Ende", {
  b <- bundle()
  expect_true(is.list(b$rueck))
  expect_identical(names(b$rueck), c("pfad", "dateien", "index_xml"))
  expect_identical(b$rueck$pfad, b$pfad)
  expect_true(dir.exists(b$pfad))
  expect_identical(b$rueck$index_xml, "index.xml")

  d <- b$rueck$dateien
  expect_true(is.character(d))
  expect_true(all(c("eingaben.csv", "forecast.csv", "forecast_kpis.csv",
                    "index.xml") %in% d))
  # index.xml ist die Naht der Komposition: als LETZTES geschrieben.
  expect_identical(d[[length(d)]], "index.xml")
  # ... und genau EINMAL in der Liste.
  expect_identical(sum(d == "index.xml"), 1L)
  # Die zurueckgemeldeten Dateien == die tatsaechlich im Verzeichnis liegenden.
  expect_setequal(d, b$dateien)
  # Alle nicht-index-Dateien sind .csv (Schicht 1 schreibt nur CSV).
  csvs <- d[d != "index.xml"]
  expect_true(all(grepl("\\.csv$", csvs, ignore.case = TRUE)))
})

# ===========================================================================
#  (2) Tier-1-Roundtrip — UNABHAENGIGES Orakel, Toleranz 0
# ===========================================================================

# ---------------------------------------------------------------------------
# Die drei Tier-1-Anker ueberleben den Dezimalkomma-CSV-Roundtrip durch die
# KOMPOSITION mit Toleranz 0 (round()==Literal). Die Referenzwerte kommen aus
# dem VERTRAG (tests/tier1_baseline.R), NICHT aus forecast — sonst waere der
# Test selbstkonsistent und ohne Mutationskraft. Die Komposition reicht die
# CSV-Werte ungefiltert durch.
# ---------------------------------------------------------------------------
test_that("Tier-1-Roundtrip: trough/result (forecast_kpis) und Historie_GuV (eingaben), Toleranz 0", {
  b   <- bundle()
  fkp <- file.path(b$pfad, "forecast_kpis.csv")
  egb <- file.path(b$pfad, "eingaben.csv")

  expect_identical(round(.num(.csv_kv(fkp, "trough"))),  TIER1_BASELINE$trough)
  expect_identical(round(.num(.csv_kv(fkp, "result"))),  TIER1_BASELINE$result)
  expect_identical(round(.num(.csv_kv(egb, "Historie_GuV"))), TIER1_BASELINE$history)

  # Literale explizit gegen die eingefrorenen Kopfzahlen — CLAUDE.md haelt sie
  # unantastbar; driftet einer, ist die Aenderung unzulaessig.
  expect_identical(TIER1_BASELINE$trough,  -5135990)
  expect_identical(TIER1_BASELINE$result,   2672411)
  expect_identical(TIER1_BASELINE$history, -5235600)
})

# ===========================================================================
#  (3) CSV-Format ueberlebt die Komposition — CRLF, Semikolon, Dezimalkomma
# ===========================================================================

# ---------------------------------------------------------------------------
# Die Komposition verbiegt das CSV-Format der Schicht 1 nicht: jede .csv im
# Buendel traegt CRLF-Zeilenenden (jedes 0x0A von 0x0D eingeleitet), Semikolon
# je nicht-leerer Zeile, KEINE Zelle mit Dezimal-/Tausenderpunkt ([0-9][.][0-9])
# und keine wissenschaftliche Notation. Positive Gegenprobe (kein Wegformatieren
# aller Nachkommastellen): der dezimale KK_Zins traegt genau ein ','.
# ---------------------------------------------------------------------------
test_that("CSV-Format durch die Komposition: CRLF + Semikolon + Dezimalkomma (kein '.', keine Exp)", {
  b    <- bundle()
  csvs <- file.path(b$pfad, b$dateien[grepl("\\.csv$", b$dateien, ignore.case = TRUE)])
  expect_true(length(csvs) >= 3L)

  for (pf in csvs) {
    rb <- .read_raw(pf)
    ch <- rawToChar(rb); Encoding(ch) <- "UTF-8"
    expect_true(validUTF8(ch), info = basename(pf))
    nl <- which(rb == as.raw(0x0a))
    expect_true(length(nl) >= 1L, info = paste("kein Zeilenumbruch:", basename(pf)))
    expect_true(all(nl > 1L) && all(rb[nl - 1L] == as.raw(0x0d)),
                info = paste("CRLF verletzt:", basename(pf)))
    ll <- readLines(pf, warn = FALSE)
    expect_true(all(grepl(";", ll[nzchar(ll)], fixed = TRUE)),
                info = paste("Semikolon fehlt:", basename(pf)))
    expect_false(grepl("[0-9][.][0-9]", ch),
                 info = paste("Dezimal-/Tausenderpunkt:", basename(pf)))
    expect_false(grepl("[0-9][eE][+-]?[0-9]", ch),
                 info = paste("wissenschaftliche Notation:", basename(pf)))
  }

  # Dezimalkomma positiv: der KK_Zins ist dezimal (nicht ganzzahlig) und traegt
  # in eingaben.csv genau ein ',' (nicht durch Wegformatieren erschlichen).
  kk <- .sc$mdl$kk_zins
  expect_true(kk != round(kk))
  zelle <- .csv_kv(file.path(b$pfad, "eingaben.csv"), "KK_Zins")
  expect_false(is.na(zelle))
  expect_true(grepl(",", zelle, fixed = TRUE))
  expect_false(grepl(".", zelle, fixed = TRUE))
  expect_equal(.num(zelle), kk, tolerance = 1e-12)
})

# ===========================================================================
#  (4) PDF/A-ENTSCHEID gepinnt — kein PDF/A, Signatur ohne PDF/A-Parameter
# ===========================================================================

# ---------------------------------------------------------------------------
# Das Buendel enthaelt KEINE .pdf/.pdfa-Datei; index.xml referenziert
# ausschliesslich .csv-Dateien; die Signatur export_z3 hat KEINEN PDF/A-
# Parameter (kein Formal-Name enthaelt 'pdf'). Der PDF/A-Container ist bewusst
# NICHT Teil des Z3-Buendels (eigene Toolchain, nicht in deps.R).
# ---------------------------------------------------------------------------
test_that("Kein PDF/A: keine .pdf/.pdfa-Datei, index.xml nur .csv, Signatur ohne PDF/A-Parameter", {
  b <- bundle()
  # (a) keine .pdf/.pdfa im Buendel.
  expect_false(any(grepl("\\.(pdf|pdfa)$", b$dateien, ignore.case = TRUE)))

  # (b) index.xml referenziert nur .csv (jede <URL> endet auf .csv).
  idx <- .read_txt(file.path(b$pfad, "index.xml"))
  urls <- basename(.zwischen(idx, "URL"))
  expect_true(length(urls) >= 1L)
  expect_true(all(grepl("\\.csv$", urls, ignore.case = TRUE)))
  expect_false(any(grepl("\\.(pdf|pdfa)$", urls, ignore.case = TRUE)))

  # (c) Signatur ohne PDF/A-Parameter — kein Formal-Name enthaelt 'pdf'.
  fn <- names(formals(export_z3))
  expect_false(any(grepl("pdf", fn, ignore.case = TRUE)))
  # Positive Signatur-Pinnung: genau (ergebnisse, sc, pfad) wie export_z3_csv.
  expect_identical(fn, c("ergebnisse", "sc", "pfad"))
})

# ===========================================================================
#  (5) Komposition integriert die Index-Hashes — EXTERNES Orakel, kein Selbst-Hash
# ===========================================================================

# ---------------------------------------------------------------------------
# Die aus index.xml gelesenen <Hash>-Werte (ohne 'sha256:'-Praefix) sind
# identical den EXTERN ueber die CSV-Bytes gerechneten sha256_datei(csv) (eine
# Quelle Schicht 2/3) — kein Hash prueft sich selbst. index.xml traegt keinen
# Hash seiner selbst; die XML-Wurzel <DataSet> ist vorhanden.
# ---------------------------------------------------------------------------
test_that("Index-Hashes extern == sha256_datei(csv), kein Selbst-Hash, <DataSet>-Wurzel", {
  b   <- bundle()
  idx_path <- file.path(b$pfad, "index.xml")
  idx <- .read_txt(idx_path)

  # XML-Deklaration + Wurzel (der Beschreibungsstandard aus Schicht 2/3).
  expect_true(grepl("<?xml",       idx, fixed = TRUE))
  expect_true(grepl("<DataSet",    idx, fixed = TRUE))
  expect_true(grepl("</DataSet>",  idx, fixed = TRUE))

  hh <- sub("^sha256:", "", .zwischen(idx, "Hash"))
  expect_true(length(hh) >= 1L)
  # Jeder gelistete Hash ist ein 64-Hex-String ...
  expect_true(all(grepl("^[0-9a-f]{64}$", hh)))

  # ... und die Hash-Menge == Menge der EXTERN gerechneten CSV-SHA-256.
  csvs <- b$dateien[grepl("\\.csv$", b$dateien, ignore.case = TRUE)]
  soll <- vapply(file.path(b$pfad, csvs), sha256_datei, character(1L),
                 USE.NAMES = FALSE)
  expect_setequal(hh, soll)
  expect_identical(length(hh), length(soll))

  # Kein Hash prueft sich selbst: die SHA-256 der index.xml ist NICHT gelistet.
  expect_false(sha256_datei(idx_path) %in% hh)
})

# ===========================================================================
#  (6) Fehlerfaelle — an export_z3_csv delegiert, benannt
# ===========================================================================

# ---------------------------------------------------------------------------
# ergebnisse leer/unbenannt (delegiert an export_bloecke_bauen via
# export_z3_csv); sc ohne Klasse liqui_scenario; pfad kein nicht-leerer String
# / nicht anlegbar (MIT Pfad im Text) -> je benannter Fehler. Die Delegation
# wirkt ungeaendert durch die Komposition.
# ---------------------------------------------------------------------------
test_that("Fehlerfaelle: ergebnisse leer/unbenannt, sc ohne Klasse, pfad ungueltig/nicht anlegbar", {
  gutdir <- file.path(tempdir(), "z3-comp-err")
  unlink(gutdir, recursive = TRUE)

  # (a) ergebnisse leer bzw. unbenannt.
  expect_error(export_z3(list(), .sc, gutdir))
  expect_error(export_z3(list(fc()), .sc, gutdir))

  # (b) sc ohne Klasse liqui_scenario.
  expect_error(export_z3(list(forecast = fc()), unclass(.sc), gutdir))
  expect_error(export_z3(list(forecast = fc()), list(name = "x"), gutdir))

  # (c) pfad kein nicht-leerer String.
  expect_error(export_z3(list(forecast = fc()), .sc, ""))
  expect_error(export_z3(list(forecast = fc()), .sc, NA_character_))

  # (d) pfad nicht anlegbar (Verzeichnis unter einer regulaeren Datei) ->
  #     Fehler MIT dem Pfad im Text.
  blocker <- tempfile()
  writeLines("blockiert", blocker)
  bad <- file.path(blocker, "z3")
  err <- tryCatch(export_z3(list(forecast = fc()), .sc, bad),
                  error = function(e) conditionMessage(e))
  expect_true(is.character(err))
  expect_true(grepl(bad, err, fixed = TRUE))
})

# ===========================================================================
#  (7) RNG-Waechter — kein RNG im GoBD-Pfad
# ===========================================================================

# ---------------------------------------------------------------------------
# .Random.seed vor/nach export_z3 unveraendert: die Komposition ist
# deterministisch und zieht keinen Zufall (kein RNG, kein Grafikgeraet).
# ---------------------------------------------------------------------------
test_that("RNG-Waechter: .Random.seed vor/nach export_z3 identisch", {
  invisible(runif(1))            # .Random.seed sicher initialisieren
  seed_vor <- get(".Random.seed", envir = globalenv())
  out <- file.path(tempdir(), "z3-comp-rng")
  unlink(out, recursive = TRUE)
  export_z3(list(forecast = fc()), .sc, out)
  seed_nach <- get(".Random.seed", envir = globalenv())
  expect_identical(seed_vor, seed_nach)
})
