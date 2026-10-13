# ============================================================================
#  tests/test_export_z3_index_mutgate2.R  —  Mutgate-Nachschub (Rerun) fuer
#  phase-13-z3-index (Schicht 2/3 des GoBD-Z3-Exports, export_z3_index).
#
#  Herkunft: .orch-Mutgate-RERUN. Ueberlebt hatte
#
#    M5  .z3_spalten_typ: Regex ^-?[0-9]+(,[0-9]+)?$ -> ^-?[0-9]+(,[0-9]*)?$
#        (+ zu * nach dem Dezimalkomma) — eine Zelle "1," (Komma OHNE
#        Nachkommaziffer) matcht dann faelschlich, die Spalte gaelte als
#        numeric, obwohl das GoBD-Zahlformat nach dem Komma mindestens
#        EINE Ziffer verlangt.
#
#  Warum die Grenzzelle "1," der Unterscheider ist: fuer jede andere Zellform
#  sind Original- und Mutanten-Regex aequivalent — "1" (kein Komma) und "1,0"
#  (Komma + Ziffern) matchen unter BEIDEN, "abc" unter KEINEM. Nur die
#  nachkommalose Komma-Form liegt exakt in der Differenzmenge (+ verlangt
#  >= 1 Ziffer, * erlaubt 0). Der bestehende Mutgate-Test (M1) und die
#  Spezifikation nutzen keine solche Zelle — deshalb ueberlebte M5.
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
#  (M5) Spalten-TYP an der Regex-Grenze: die Spalte k traegt AUSSCHLIESSLICH
#  Zellen der Form Ziffer+Komma OHNE Nachkommaziffer ("1,", "2,"). Unter dem
#  Original-Regex ^-?[0-9]+(,[0-9]+)?$ matcht keine davon (+ verlangt nach
#  dem Komma mindestens eine Ziffer) -> alphanumeric. Unter dem Mutanten
#  (* statt +) matchen beide -> k wuerde faelschlich numeric. Spalte v
#  (reiner Text) ist die Kontrast-Kontrolle: unter beiden alphanumeric.
# ===========================================================================
test_that("Spalten-Typ: Komma ohne Nachkommaziffer ist kein GoBD-Zahlformat (Mutgate-Survivor M5: Regex + -> *)", {
  d <- file.path(tempdir(), "z3-index-mg2-komma")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  .mg_csv(file.path(d, "komma.csv"), c("k;v", "1,;x", "2,;y"))

  export_z3_index(d, "komma.csv")
  idx <- .mg_read(file.path(d, "index.xml"))
  typen <- .mg_typen(.mg_tables(idx)[[1L]])

  # Grenzzelle "1,": kein gueltiges GoBD-Zahlformat -> alphanumeric. Genau
  # hier trennen sich Original (+) und Mutant (*).
  expect_identical(unname(typen[["k"]]), "alphanumeric")
  # Kontrast-Kontrolle: reine Textspalte ist unter beiden alphanumeric.
  expect_identical(unname(typen[["v"]]), "alphanumeric")
  unlink(d, recursive = TRUE)
})

# ===========================================================================
#  Positiv-Regime: der Pin verbietet NUR die nachkommalose Komma-Form.
#  Echte Dezimalzahlen ("1,0", "2,50") bleiben numeric — sonst wuerde der
#  Test das legitime Dezimalkomma gleich mitverbieten.
# ===========================================================================
test_that("Spalten-Typ: Dezimalzahlen mit Nachkommaziffern bleiben numeric", {
  d <- file.path(tempdir(), "z3-index-mg2-dezimal")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE)
  .mg_csv(file.path(d, "dezimal.csv"), c("w;v", "1,0;x", "2,50;y"))

  export_z3_index(d, "dezimal.csv")
  idx <- .mg_read(file.path(d, "index.xml"))
  typen <- .mg_typen(.mg_tables(idx)[[1L]])

  expect_identical(unname(typen[["w"]]), "numeric")
  expect_identical(unname(typen[["v"]]), "alphanumeric")
  unlink(d, recursive = TRUE)
})
