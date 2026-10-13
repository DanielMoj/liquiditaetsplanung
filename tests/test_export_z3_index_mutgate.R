# ============================================================================
#  tests/test_export_z3_index_mutgate.R  —  Mutgate-Nachschub fuer
#  phase-13-z3-index (Schicht 2/3 des GoBD-Z3-Exports, export_z3_index).
#
#  Die Spezifikation (tests/test_export_z3_index.R) prueft Rueckgabe, <Table>-
#  Anzahl, Hash-Externalitaet, Variablen-ANZAHL und Fehlerfaelle — aber NICHT
#  (a) den Spalten-TYP <Type> (numeric vs. alphanumeric), (b) den Zeilen-
#  bereich <To> (Zahl der Datenzeilen) und (c) die Zeilenstruktur zwischen den
#  <Table>-Elementen. Genau dort sassen die ueberlebenden Mutanten:
#
#    M1  .z3_spalten_typ: all(...) -> any(...)  — eine Spalte gaelte schon als
#        numeric, wenn EINE Zelle dem Zahlformat entspricht statt ALLE.
#    M3  .z3_table_xml:   length(ll) >= 2L -> > 2L  — bei einer 2-Zeilen-CSV
#        (Kopf + 1 Datenzeile) verschwindet die Datenzeile (<To> faellt auf 0).
#    M4  export_z3_index: paste(tabellen, collapse="\n") -> collapse=""  — die
#        <Table>-Elemente werden ohne Newline aneinandergehaengt.
#
#  Jeder Test prueft die verletzte EIGENSCHAFT gegen ein EXTERNES, aus den
#  selbst geschriebenen CSVs rekonstruiertes Orakel — kein Erwartungswert ist
#  hartcodiert, kein Mutanten-Diff woertlich nachbuchstabiert. Auf dem echten
#  Stand gruen, auf dem jeweiligen Mutanten rot (Gate-Leiter).
#
#  BESTANDSSCHUTZ: beruehrt (modifiziert) weder kernel/ NOCH R/export.R NOCH
#  R/manifest.R NOCH eine test_export_*/test_manifest*-Datei — R/manifest.R und
#  R/z3.R werden nur GESOURCT. Kein skip, kein succeed, keine Tier-1-Literale,
#  kein MC, keine Engine, keine Fixture-Mappe — nur synthetische tempdir-CSVs.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))  # sha256_datei (nur gelesen)
source(file.path(.root, "R", "z3.R"))        # export_z3_index

# ---------------------------------------------------------------------------
# Hilfen: index.xml roh lesen; Fragmente/Werte per Basis-Regex extrahieren
# (kein xml2 — der Test darf das verbotene Paket nicht einschleppen).
# ---------------------------------------------------------------------------
.mg_read <- function(pf) {
  ch <- rawToChar(readBin(pf, "raw", n = file.info(pf)$size))
  Encoding(ch) <- "UTF-8"
  ch
}
# Alle <Table>...</Table>-Fragmente in Dokumentreihenfolge.
.mg_tables <- function(idx) {
  regmatches(idx, gregexpr("(?s)<Table>.*?</Table>", idx, perl = TRUE))[[1L]]
}
# Werte aller <tag>...</tag> in einem Fragment (getrimmt).
.mg_werte <- function(hay, tag) {
  pat <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
  m <- regmatches(hay, gregexpr(pat, hay, perl = TRUE))[[1L]]
  if (!length(m)) return(character(0))
  trimws(sub(sprintf("(?s)^<%s>(.*)</%s>$", tag, tag), "\\1", m, perl = TRUE))
}
# Erster <tag>-Wert (leerer String, wenn keiner).
.mg_eins <- function(hay, tag) {
  v <- .mg_werte(hay, tag)
  if (length(v)) v[[1L]] else ""
}
# Name -> Type ueber die <Variable>-Bloecke eines <Table>-Fragments.
.mg_typen <- function(tab) {
  vars <- regmatches(tab, gregexpr("(?s)<Variable>.*?</Variable>", tab, perl = TRUE))[[1L]]
  if (!length(vars)) return(setNames(character(0), character(0)))
  nm <- vapply(vars, function(v) .mg_eins(v, "Name"), character(1L), USE.NAMES = FALSE)
  ty <- vapply(vars, function(v) .mg_eins(v, "Type"), character(1L), USE.NAMES = FALSE)
  setNames(ty, nm)
}
# CSV als CRLF/UTF-8-Bytes (GoBD-Vertrag) schreiben.
.mg_csv <- function(pf, zeilen) {
  writeLines(zeilen, pf, sep = "\r\n", useBytes = TRUE)
}

# ===========================================================================
#  (M1) Spalten-TYP: eine Spalte mit MINDESTENS einer nicht-numerischen Zelle
#  ist alphanumeric — numeric verlangt, dass ALLE Zellen dem Zahlformat
#  entsprechen. Das Orakel liest die Zellen aus der selbst geschriebenen CSV
#  und leitet den Soll-Typ per GoBD-Kontrakt (all-numerisch) ab.
# ===========================================================================
test_that("Spalten-Typ: gemischte Spalte ist alphanumeric (all, nicht any)", {
  d <- file.path(tempdir(), "z3-index-mg-typ")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  # Spalte m: durchweg numerisch -> numeric. Spalte n: gemischt (Text 'abc'
  # neben Zahl '5') -> alphanumeric. Spalte t: durchweg Text -> alphanumeric.
  .mg_csv(file.path(d, "typ.csv"), c("m;n;t", "1;abc;foo", "2;5;bar"))

  export_z3_index(d, "typ.csv")
  idx <- .mg_read(file.path(d, "index.xml"))
  typen <- .mg_typen(.mg_tables(idx)[[1L]])

  # Externes Orakel: Soll-Typ je Spalte aus den CSV-Zellen (all-numerisch?).
  ll     <- readLines(file.path(d, "typ.csv"), warn = FALSE)
  spalten <- strsplit(ll[[1L]], ";", fixed = TRUE)[[1L]]
  zeilen  <- lapply(ll[-1L], function(z) strsplit(z, ";", fixed = TRUE)[[1L]])
  soll <- vapply(seq_along(spalten), function(j) {
    zellen <- vapply(zeilen, function(z) if (j <= length(z)) z[[j]] else "",
                     character(1L))
    zellen <- zellen[nzchar(zellen)]
    if (length(zellen) && all(grepl("^-?[0-9]+(,[0-9]+)?$", zellen)))
      "numeric" else "alphanumeric"
  }, character(1L))
  names(soll) <- spalten

  # Jede Spalte traegt exakt ihren Kontrakt-Typ. Die gemischte Spalte n ist
  # der Kill fuer M1: unter any(...) waere sie faelschlich numeric.
  expect_identical(typen[spalten], soll[spalten])
  expect_identical(unname(soll[["n"]]), "alphanumeric")  # Kontrakt, nicht Mutant
  expect_identical(unname(typen[["n"]]), "alphanumeric")
  unlink(d, recursive = TRUE)
})

# ===========================================================================
#  (M3) Zeilenbereich <To>: fuer eine 2-Zeilen-CSV (Kopf + 1 Datenzeile) ist
#  <To> == Zahl der Datenzeilen (1), nicht 0. Das Orakel zaehlt die
#  nicht-leeren Datenzeilen der selbst geschriebenen CSV.
# ===========================================================================
test_that("Zeilenbereich: <To> == Zahl der CSV-Datenzeilen, auch bei 2-Zeilen-CSV", {
  d <- file.path(tempdir(), "z3-index-mg-range")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  # Genau Kopfzeile + 1 Datenzeile: der Grenzfall, den >= 2L abdeckt, > 2L nicht.
  .mg_csv(file.path(d, "one.csv"), c("p;q", "7;8"))

  export_z3_index(d, "one.csv")
  idx <- .mg_read(file.path(d, "index.xml"))
  tab <- .mg_tables(idx)[[1L]]

  # Externes Orakel: Datenzeilen = nicht-leere Zeilen der CSV minus Kopfzeile.
  ll        <- readLines(file.path(d, "one.csv"), warn = FALSE)
  datenzahl <- sum(nzchar(ll[-1L]))
  expect_identical(datenzahl, 1L)                    # Kontrakt: 1 Datenzeile

  von <- .mg_eins(tab, "From")
  bis <- .mg_eins(tab, "To")
  expect_identical(von, "1")
  expect_identical(bis, as.character(datenzahl))     # M3: unter > 2L waere To 0
  unlink(d, recursive = TRUE)
})

# ===========================================================================
#  (M4) Zeilenstruktur: die <Table>-Elemente sind newline-getrennt — jedes
#  <Table> oeffnet auf einer eigenen Zeile. Unter collapse="" klebt das zweite
#  <Table> an das </Table> des ersten, dann steht nur EIN <Table> allein auf
#  seiner Zeile. Orakel: Zahl der Zeilen "== <Table>" muss der Dateizahl gleichen.
# ===========================================================================
test_that("Zeilenstruktur: jedes <Table> oeffnet auf eigener Zeile (Newline-Trenner)", {
  d <- file.path(tempdir(), "z3-index-mg-sep")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  dateien <- c("s1.csv", "s2.csv", "s3.csv")
  .mg_csv(file.path(d, "s1.csv"), c("a;b", "1;2"))
  .mg_csv(file.path(d, "s2.csv"), c("c;d", "3;4"))
  .mg_csv(file.path(d, "s3.csv"), c("e;f", "5;6"))

  export_z3_index(d, dateien)
  idx <- .mg_read(file.path(d, "index.xml"))

  # Zeilen, deren getrimmter Inhalt exakt "<Table>" ist: bei Newline-Trennung
  # oeffnet jede Tabelle auf eigener Zeile -> Zahl == Dateizahl. Unter collapse=""
  # kleben die inneren Tabellen an das vorherige </Table> und fehlen hier.
  zeilen <- strsplit(idx, "\n", fixed = TRUE)[[1L]]
  offen  <- sum(trimws(zeilen) == "<Table>")
  expect_identical(offen, length(dateien))

  # Ergaenzend: kein </Table> teilt sich eine Zeile mit einem folgenden <Table>.
  expect_false(any(grepl("</Table>[^\n]*<Table>", zeilen)))
  unlink(d, recursive = TRUE)
})
