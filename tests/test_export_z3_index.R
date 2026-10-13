# ============================================================================
#  tests/test_export_z3_index.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-13-z3-index
#
#  Schicht 2/3 des GoBD-Z3-Exports (Datentraegerueberlassung, Rz. 158 ff.):
#  export_z3_index(pfad, dateien) baut die index.xml des Z3-Beschreibungs-
#  standards (Anlage GoBD 2024) fuer ein Verzeichnis mit CSV-Dateien. Die CSVs
#  schreibt Schicht 1/3 (export_z3_csv), die Komposition + App Schicht 3/3
#  (export_z3).
#
#  ROT ZUERST: das ungeschuetzte source(R/z3.R) am Kopf laedt die Datei; die
#  Tests rufen export_z3_index DIREKT (ohne Schutz-Weiche) auf — solange die
#  Funktion nicht existiert, bricht "could not find function" jeden Test ab
#  (Fehler == rot). Der Builder erfuellt sie erst mit der Implementierung.
#  KEIN skip(), KEIN succeed(), keine Existenz-Weiche.
#
#  Vertrag (handoff/phase-13-z3-index.json):
#   - Signatur export_z3_index(pfad, dateien):
#       pfad    = Verzeichnis der CSVs (existiert, sonst benannter Fehler);
#       dateien = character-Vektor nicht-leerer CSV-Basisnamen (bestimmt
#                 DETERMINISTISCH, welche Dateien ein <Table> bekommen — KEIN
#                 Verzeichnis-Scan). Schreibt index.xml in pfad, liefert
#                 'index.xml' (den Basisnamen der geschriebenen Datei).
#   - VON HAND gebaut, KEIN xml2 (deps.R bleibt unveraendert): XML-Deklaration,
#     Wurzel <DataSet version='2.0'> ... </DataSet> mit <DataName>/
#     <DataDescription>; je CSV EIN <Table>-Element (keine Attribute) mit
#     <URL><dateiname></URL>, <Name>, <Description>,
#     <Hash>sha256:<64-Hex></Hash>, <Validity>/<Range>/<From>/<To> und je
#     Spalte <Variable>/<Name>/<Type>.
#   - KEIN HASH PRUEFT SICH SELBST: jeder <Hash> ist die SHA-256 ueber die
#     rohen CSV-Bytes via sha256_datei (R/manifest.R) — EXTERN nachpruefbar;
#     index.xml traegt KEINEN Hash seiner selbst.
#   - Fehler (benannt, Stil manifest_validieren): pfad kein nicht-leerer
#     String / nicht existierend -> stop MIT Pfad; dateien kein character-
#     Vektor nicht-leerer Strings, oder eine genannte Datei fehlt in pfad ->
#     stop MIT Namen.
#
#  Orakel-Politik: EXTERNE Orakel, KEIN Selbst-Bezug. Der Prueflauf versorgt
#  sich selbst — ein frisches tempdir mit ZWEI synthetischen CSVs (2 bzw. 3
#  Spalten, Dezimalkomma-Zellen) OHNE Tier-1-/Anker-Bezug, ohne Fixture-Mappe,
#  ohne MC, ohne Engine. Die Hash-Erwartung wird EXTERN ueber die Datei-Bytes
#  gerechnet (sha256_datei), NIE aus index.xml gelesen; die Spalten-/Variablen-
#  Erwartung kommt aus der selbst geschriebenen CSV-Kopfzeile, NIE aus der
#  index.xml. Toleranz 0 (String-/Identitaets-/Existenzpruefungen).
#
#  BESTANDSSCHUTZ: dieser Test beruehrt (modifiziert) weder kernel/ NOCH
#  R/export.R NOCH R/manifest.R NOCH eine test_export_*/test_manifest*-Datei —
#  R/manifest.R und R/z3.R werden nur GESOURCT (gelesen).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))  # liefert sha256_datei (externes Orakel)
# <- z3.R existiert, definiert export_z3_index aber noch NICHT: ROT ZUERST.
#    Die Tests rufen export_z3_index ungeschuetzt auf -> "could not find
#    function" bricht gegen den AKTUELLEN Stand jeden Test ab.
source(file.path(.root, "R", "z3.R"))

# ---------------------------------------------------------------------------
# Leseform der index.xml: roh ueber readBin (implementierungsunabhaengig),
# Textextraktion per Basis-Regex (kein xml2 — der Test darf das verbotene
# Paket nicht selbst einschleppen).
# ---------------------------------------------------------------------------
.read_txt <- function(pf) {
  rb <- readBin(pf, "raw", n = file.info(pf)$size)
  ch <- rawToChar(rb)
  Encoding(ch) <- "UTF-8"
  ch
}
# Zaehlt nicht-ueberlappende Vorkommen eines Literals (gregexpr liefert -1 bei
# 0 Treffern -> hier sauber 0L statt faelschlich 1L).
.count <- function(hay, lit) {
  m <- gregexpr(lit, hay, fixed = TRUE)[[1L]]
  if (length(m) == 1L && m[[1L]] == -1L) 0L else length(m)
}
# Inhalte zwischen <tag> und </tag> (nicht-gierig, dotall), in Dokumentreihenfolge.
.zwischen <- function(hay, tag) {
  pat <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
  m <- regmatches(hay, gregexpr(pat, hay, perl = TRUE))[[1L]]
  if (!length(m)) return(character(0))
  trimws(sub(sprintf("(?s)^<%s>(.*)</%s>$", tag, tag), "\\1", m, perl = TRUE))
}
# Erster <tag>-Wert innerhalb eines Fragments (leerer String, wenn keiner).
.eins <- function(hay, tag) {
  v <- .zwischen(hay, tag)
  if (length(v)) v[[1L]] else ""
}
# Spaltennamen aus der CSV-Kopfzeile (unabhaengig von index.xml gelesen).
.csv_spalten <- function(pf) {
  ll <- readLines(pf, warn = FALSE)
  strsplit(ll[[1L]], ";", fixed = TRUE)[[1L]]
}

# ---------------------------------------------------------------------------
# EINGEFRORENE, selbstversorgende Fixture: ein frisches tempdir mit zwei
# synthetischen CSVs (t1: 2 Spalten mit Dezimalkomma-Zellen; t2: 3 Spalten),
# ein einziger export_z3_index-Lauf, LAZY memoisiert. Kein Tier-1-/Anker-
# Bezug, keine Engine, kleine Laufzeit.
# ---------------------------------------------------------------------------
.cache <- new.env(parent = emptyenv())
.DATEIEN <- c("t1.csv", "t2.csv")
.setup_dir <- function(dir) {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  # CRLF + UTF-8-Bytes wie der GoBD-CSV-Vertrag (writeBin umgeht jede
  # plattformseitige Zeilenumbruch-Uebersetzung).
  writeLines(c("a;b", "1;2,5", "3;4,5"), file.path(dir, "t1.csv"),
             sep = "\r\n", useBytes = TRUE)
  writeLines(c("x;y;z", "1;2;3"), file.path(dir, "t2.csv"),
             sep = "\r\n", useBytes = TRUE)
  dir
}
delayedAssign("bundle", local({
  d <- .setup_dir(file.path(tempdir(), "z3-index-spec"))
  rueck <- export_z3_index(d, .DATEIEN)          # rot zuerst: Funktion fehlt noch
  idx_path <- file.path(d, "index.xml")
  list(pfad = d, rueck = rueck, idx_path = idx_path,
       idx = if (file.exists(idx_path)) .read_txt(idx_path) else NA_character_)
}), assign.env = .cache)
bundle <- function() .cache$bundle

# ===========================================================================
#  (1) Rueckgabe-Vertrag
# ===========================================================================

# ---------------------------------------------------------------------------
# export_z3_index(tmpdir, c('t1.csv','t2.csv')) liefert 'index.xml' und
# schreibt index.xml ins uebergebene Verzeichnis.
# ---------------------------------------------------------------------------
test_that("Rueckgabe: 'index.xml' und Datei geschrieben", {
  b <- bundle()
  expect_identical(b$rueck, "index.xml")
  expect_true(file.exists(b$idx_path))
  expect_identical(basename(b$idx_path), "index.xml")
})

# ===========================================================================
#  (2) Beschreibungsstandard — XML-Deklaration, Wurzel, je CSV ein <Table>/<URL>
# ===========================================================================

# ---------------------------------------------------------------------------
# index.xml traegt eine XML-Deklaration und die Wurzel <DataSet ...> ...
# </DataSet>; je genannter CSV genau EIN <Table>-Element (keine Attribute) mit
# <URL><dateiname></URL>; Anzahl <Table> == Anzahl dateien; die URL-Basisnamen
# decken exakt die uebergebenen Dateien ab.
# ---------------------------------------------------------------------------
test_that("Beschreibungsstandard: XML-Deklaration + <DataSet>; je CSV genau ein <Table>/<URL>", {
  b   <- bundle()
  idx <- b$idx
  expect_true(is.character(idx) && !is.na(idx))

  # XML-Deklaration + Wurzel.
  expect_true(grepl("<?xml", idx, fixed = TRUE))
  expect_true(grepl("<DataSet", idx, fixed = TRUE))
  expect_true(grepl("</DataSet>", idx, fixed = TRUE))

  # Genau ein <Table> je Datei.
  expect_identical(.count(idx, "<Table>"), length(.DATEIEN))
  expect_identical(.count(idx, "</Table>"), length(.DATEIEN))

  # <Table> traegt keine Attribute (Content-Modell): der Roh-Tag ist exakt
  # "<Table>", kein "<Table " mit Attributen.
  expect_false(grepl("<Table ", idx, fixed = TRUE))

  # Je <Table> genau ein <URL>; die URL-Basisnamen == uebergebene Dateien.
  urls <- .zwischen(idx, "URL")
  expect_identical(length(urls), length(.DATEIEN))
  expect_setequal(basename(urls), .DATEIEN)
})

# ===========================================================================
#  (3) Von Hand gebaut — KEIN xml2 (deps.R unveraendert)
# ===========================================================================

# ---------------------------------------------------------------------------
# Die Schicht ist von Hand gebaut (export_z3_index existiert und baut die
# index.xml) OHNE ein neues Paket: tests/deps.R traegt kein xml2 (grep 'xml2'
# ohne Treffer). Der Bezug auf bundle() haelt den Test rot-zuerst (baut die
# index.xml erst, wenn die Funktion existiert) UND verankert die Semantik
# "hand-gebaut, kein xml2" — ein spaeter unbemerkt eingeschlepptes xml2 faellt
# auf.
# ---------------------------------------------------------------------------
test_that("Kein xml2: hand-gebaut (export_z3_index) ohne xml2 in tests/deps.R", {
  b <- bundle()                       # rot zuerst: baut nur mit der Funktion
  expect_true(file.exists(b$idx_path))
  deps <- readLines(file.path(.root, "tests", "deps.R"), warn = FALSE)
  expect_false(any(grepl("xml2", deps, fixed = TRUE)))
})

# ===========================================================================
#  (4) Kein Hash prueft sich selbst — EXTERNES Orakel sha256_datei
# ===========================================================================

# ---------------------------------------------------------------------------
# Jeder <Hash>sha256:<64-Hex></Hash> ist identical der EXTERN ueber die
# rohen CSV-Bytes gerechneten sha256_datei(csv) (NICHT aus index.xml gelesen);
# die Zuordnung laeuft ueber die <URL> im selben <Table>. Die Hash-Menge in
# index.xml == Menge der CSV-SHA-256, und index.xml traegt KEINEN Hash seiner
# selbst.
# ---------------------------------------------------------------------------
test_that("Kein Selbst-Hash: jeder <Hash> == EXTERN sha256_datei(csv), keine Selbstreferenz", {
  b   <- bundle()
  idx <- b$idx

  tables <- regmatches(idx, gregexpr("(?s)<Table>.*?</Table>", idx, perl = TRUE))[[1L]]
  expect_identical(length(tables), length(.DATEIEN))

  for (tab in tables) {
    url  <- basename(.eins(tab, "URL"))
    hash <- .eins(tab, "Hash")
    csv  <- file.path(b$pfad, url)
    expect_true(file.exists(csv), info = url)
    # Form: sha256:<64 Kleinbuchstaben-Hexzeichen>.
    expect_true(grepl("^sha256:[0-9a-f]{64}$", hash), info = url)
    # EXTERNES Orakel: Hash aus den Datei-Bytes, NICHT aus index.xml.
    soll <- paste0("sha256:", sha256_datei(csv))
    expect_identical(hash, soll, info = url)
  }

  # Hash-Menge (ohne Praefix) == Menge der extern gerechneten CSV-SHA-256.
  hh  <- sub("^sha256:", "", .zwischen(idx, "Hash"))
  soll_menge <- vapply(file.path(b$pfad, .DATEIEN), sha256_datei,
                       character(1L), USE.NAMES = FALSE)
  expect_setequal(hh, soll_menge)
  expect_identical(length(hh), length(soll_menge))

  # Kein Hash prueft sich selbst: die SHA-256 der index.xml ist NICHT unter den
  # gelisteten Hashes.
  expect_false(sha256_datei(b$idx_path) %in% hh)
})

# ===========================================================================
#  (5) Spalten/Variablen — je Spalte der CSV-Kopfzeile ein <Variable>/<Name>
# ===========================================================================

# ---------------------------------------------------------------------------
# Je CSV traegt sein <Table> ein <Variable>/<Name>/<Type> je Spalte der
# CSV-Kopfzeile: Anzahl <Variable> == Spaltenzahl, und die <Name> der Variablen
# decken die Spaltennamen exakt ab. Die Erwartung kommt aus der selbst
# geschriebenen CSV-Kopfzeile, NICHT aus index.xml (t1: a;b -> 2; t2: x;y;z -> 3).
# ---------------------------------------------------------------------------
test_that("Variablen: je Spalte ein <Variable>/<Name>; Anzahl == Spaltenzahl", {
  b   <- bundle()
  idx <- b$idx

  tables <- regmatches(idx, gregexpr("(?s)<Table>.*?</Table>", idx, perl = TRUE))[[1L]]

  for (tab in tables) {
    url     <- basename(.eins(tab, "URL"))
    spalten <- .csv_spalten(file.path(b$pfad, url))   # unabhaengiges Orakel

    vars <- regmatches(tab, gregexpr("(?s)<Variable>.*?</Variable>", tab, perl = TRUE))[[1L]]
    expect_identical(length(vars), length(spalten), info = url)

    # <Name> je Variable == Spaltennamen (namensblind ueber die Menge).
    vnamen <- vapply(vars, function(v) .eins(v, "Name"), character(1L), USE.NAMES = FALSE)
    expect_setequal(vnamen, spalten)
  }
})

# ===========================================================================
#  (6) Fehlerfaelle — benannt (Stil manifest_validieren)
# ===========================================================================

# ---------------------------------------------------------------------------
# pfad nicht existierend -> Fehler MIT dem Pfad im Text; dateien mit einer
# nicht in pfad liegenden CSV -> Fehler MIT dem Namen; pfad kein nicht-leerer
# String -> Fehler; dateien kein character-Vektor nicht-leerer Strings ->
# Fehler.
# ---------------------------------------------------------------------------
test_that("Fehlerfaelle: pfad fehlt (mit Pfad), Datei fehlt (mit Name), Typfehler pfad/dateien", {
  b <- bundle()

  # (a) pfad existiert nicht -> Fehler MIT dem Pfad.
  fehl_pfad <- file.path(tempdir(), "z3-index-gibt-es-nicht-4711")
  unlink(fehl_pfad, recursive = TRUE)
  msg <- tryCatch(export_z3_index(fehl_pfad, .DATEIEN),
                  error = function(e) conditionMessage(e))
  expect_true(is.character(msg))
  expect_true(grepl(fehl_pfad, msg, fixed = TRUE))

  # (b) dateien nennt eine nicht in pfad liegende CSV -> Fehler MIT dem Namen.
  msg2 <- tryCatch(export_z3_index(b$pfad, c("t1.csv", "fehlt.csv")),
                   error = function(e) conditionMessage(e))
  expect_true(is.character(msg2))
  expect_true(grepl("fehlt.csv", msg2, fixed = TRUE))

  # (c) pfad kein nicht-leerer String -> Fehler.
  expect_error(export_z3_index("", .DATEIEN))
  expect_error(export_z3_index(NA_character_, .DATEIEN))
  expect_error(export_z3_index(c(b$pfad, b$pfad), .DATEIEN))

  # (d) dateien kein character-Vektor nicht-leerer Strings -> Fehler.
  expect_error(export_z3_index(b$pfad, character(0)))
  expect_error(export_z3_index(b$pfad, 1:2))
  expect_error(export_z3_index(b$pfad, c("t1.csv", "")))
  expect_error(export_z3_index(b$pfad, c("t1.csv", NA_character_)))
})

# ===========================================================================
#  (7) RNG-Waechter — kein RNG im GoBD-Pfad
# ===========================================================================

# ---------------------------------------------------------------------------
# .Random.seed vor/nach export_z3_index unveraendert: die Schicht ist
# deterministisch und zieht keinen Zufall (kein RNG, kein Grafikgeraet).
# ---------------------------------------------------------------------------
test_that("RNG-Waechter: .Random.seed vor/nach export_z3_index identisch", {
  invisible(runif(1))            # .Random.seed sicher initialisieren
  seed_vor <- get(".Random.seed", envir = globalenv())
  d <- .setup_dir(file.path(tempdir(), "z3-index-rng"))
  export_z3_index(d, .DATEIEN)
  seed_nach <- get(".Random.seed", envir = globalenv())
  expect_identical(seed_vor, seed_nach)
})
