# ============================================================================
#  tests/test_export_z3_index_mutgate3.R  —  Mutgate-Nachschub (Rerun 2) fuer
#  phase-13-z3-index (Schicht 2/3 des GoBD-Z3-Exports, export_z3_index).
#
#  Herkunft: .orch-Mutgate-RERUN 2 (.orch/mut-phase-13-z3-index, regeneriertes
#  Set; ueberlebende.txt: M2, M3, M4). Die .orch-Beschreibungen:
#
#    M2  .z3_xml_escape: Reihenfolge der XML-Escape-gsub vertauscht — & NACH <
#        maskiert, dadurch wird das eingefuegte &lt; zu &amp;lt; DOPPELT
#        maskiert. Ein '<' im Spaltennamen erschiene als &amp;lt; statt &lt;.
#    M3  .z3_table_xml: j <= length(z)  ->  j < length(z) — die LETZTE Spalte
#        jeder Datenzeile wird als leer gelesen; der Spaltentyp der letzten
#        Spalte faellt damit immer auf alphanumeric zurueck.
#    M4  .z3_table_xml: length(ll) >= 1L  ->  > 1L (Off-by-one) — eine CSV mit
#        NUR der Kopfzeile bekommt einen leeren Header, keine <Variable>.
#
#  Warum sie ueberlebten: die Spezifikation und die aelteren Mutgate-Tests
#  nutzen (a) keine XML-Metazeichen in Spaltennamen, (b) numerische Spalten
#  nie an LETZTER Position und (c) keine Kopf-only-CSV ohne Datenzeile.
#  Jeder Block prueft die verletzte Eigenschaft: auf dem echten Stand gruen,
#  auf dem jeweiligen Mutanten rot (Gate-Leiter).
#
#  BESTANDSSCHUTZ: beruehrt (modifiziert) weder kernel/ NOCH R/export.R NOCH
#  R/manifest.R NOCH eine bestehende test_*-Datei — R/manifest.R und R/z3.R
#  werden nur GESOURCT. Kein skip, kein succeed, keine Tier-1-Literale,
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
#  (M2) XML-Escape: jedes Metazeichen im Spaltennamen wird GENAU EINMAL
#  maskiert. Der Unterscheider ist '<': korrekt (& zuerst) ergibt &lt;,
#  unter dem Mutanten (& NACH <) wird das eingefuegte &lt; erneut durch die
#  &-Maskierung gezogen -> &amp;lt; (Doppelmaskierung). '&' und '>' sind
#  unter beiden Reihenfolgen gleich — die Doppelmaskierungs-Verbote decken
#  vollstaendigkeitshalber alle drei Entities ab.
# ===========================================================================
test_that("XML-Escape: Metazeichen in Namen werden einfach maskiert, nie doppelt (M2: gsub-Reihenfolge)", {
  d <- file.path(tempdir(), "z3-index-mg3-escape")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  .mg_csv(file.path(d, "escape.csv"), c("a<b;c&d;e>f", "x;y;z"))

  export_z3_index(d, "escape.csv")
  idx <- .mg_read(file.path(d, "index.xml"))

  # Korrekte, EINFACHE Entities in den <Variable>-Namen.
  expect_true(grepl("<Name>a&lt;b</Name>", idx, fixed = TRUE))
  expect_true(grepl("<Name>c&amp;d</Name>", idx, fixed = TRUE))
  expect_true(grepl("<Name>e&gt;f</Name>", idx, fixed = TRUE))
  # KEINE Doppelmaskierung irgendwo im Dokument. '&amp;lt;' ist der M2-Kill
  # (unter dem Mutanten wird 'a<b' zu 'a&amp;lt;b').
  expect_false(grepl("&amp;lt;", idx, fixed = TRUE))
  expect_false(grepl("&amp;gt;", idx, fixed = TRUE))
  expect_false(grepl("&amp;amp;", idx, fixed = TRUE))
  unlink(d, recursive = TRUE)
})

# ===========================================================================
#  (M3) Spalten-TYP der LETZTEN Spalte: die numerische Spalte steht bewusst
#  an letzter Position. Unter j <= length(z) werden ihre Zellen gelesen ->
#  numeric; unter dem Mutanten (j < length(z)) ist die letzte Zelle jeder
#  Zeile leer -> die Spalte faellt faelschlich auf alphanumeric. Das Orakel
#  rekonstruiert den Soll-Typ EXTERN aus den selbst geschriebenen CSV-Zellen
#  (GoBD-Kontrakt: alle nicht-leeren Zellen numerisch -> numeric).
# ===========================================================================
test_that("Spaltentyp: numerische LETZTE Spalte bleibt numeric (M3: <= -> < leert die letzte Zelle)", {
  d <- file.path(tempdir(), "z3-index-mg3-letzte")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  .mg_csv(file.path(d, "letzte.csv"), c("text;betrag", "x;1", "y;2,5"))

  export_z3_index(d, "letzte.csv")
  idx <- .mg_read(file.path(d, "index.xml"))
  typen <- .mg_typen(.mg_tables(idx)[[1L]])

  # Externes Orakel: Soll-Typ je Spalte aus den CSV-Zellen (all-numerisch?).
  ll      <- readLines(file.path(d, "letzte.csv"), warn = FALSE)
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

  expect_identical(typen[spalten], soll[spalten])
  # Kontrakt explizit: die LETZTE Spalte ist numeric — genau hier verliert
  # der Mutant die Zellen und faellt auf alphanumeric.
  expect_identical(unname(soll[["betrag"]]), "numeric")   # Kontrakt, nicht Mutant
  expect_identical(unname(typen[["betrag"]]), "numeric")
  expect_identical(unname(typen[["text"]]), "alphanumeric")
  unlink(d, recursive = TRUE)
})

# ===========================================================================
#  (M4) Kopf-only-CSV: eine CSV aus NUR der Kopfzeile (keine Datenzeile)
#  traegt ihre Variablen aus der Kopfzeile und den leeren Zeilenbereich
#  <From>1</From>/<To>0</To>. Unter length(ll) >= 1L wird die Kopfzeile
#  geparst; unter dem Mutanten (> 1L) ist der Header leer und die
#  <Variable>-Elemente verschwinden.
# ===========================================================================
test_that("Kopf-only-CSV: Variablen aus der Kopfzeile, To == 0 (M4: >=1 -> >1 verliert den Header)", {
  d <- file.path(tempdir(), "z3-index-mg3-kopf")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  .mg_csv(file.path(d, "kopf.csv"), "nur;kopf")

  export_z3_index(d, "kopf.csv")
  idx <- .mg_read(file.path(d, "index.xml"))
  tab <- .mg_tables(idx)[[1L]]

  # Genau zwei <Variable>-Elemente, Namen aus der Kopfzeile, beide
  # alphanumeric (ohne Datenzeile gibt es keinen numerischen Nachweis).
  typen <- .mg_typen(tab)
  expect_identical(length(typen), 2L)
  expect_identical(names(typen), c("nur", "kopf"))
  expect_identical(unname(typen[["nur"]]), "alphanumeric")
  expect_identical(unname(typen[["kopf"]]), "alphanumeric")

  # Leerer Zeilenbereich: From bleibt 1, To ist die Zahl der Datenzeilen (0).
  expect_identical(.mg_eins(tab, "From"), "1")
  expect_identical(.mg_eins(tab, "To"), "0")
  unlink(d, recursive = TRUE)
})
