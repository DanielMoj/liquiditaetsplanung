# ============================================================================
#  tests/test_z3_index_extra.R  —  ZUSATZ-Tests (rot_zuerst = false) fuer
#  phase-13-z3-index. Ergaenzen die Spezifikation tests/test_export_z3_index.R
#  um Mutationsdruck auf Elemente, die die Spec strukturell, aber nicht im WERT
#  prueft: den GoBD-Spaltentyp <Type> und den Zeilenbereich <Validity>/<Range>.
#
#  Politik wie die Spec: EXTERNE Orakel, kein Selbst-Bezug — der Prueflauf
#  versorgt sich selbst (frisches tempdir, eine synthetische CSV mit einer
#  KLAR numerischen und einer KLAR alphanumerischen Spalte). Kein skip, kein
#  succeed, keine Tier-1-Literale, keine Engine, keine Fixture-Mappe.
#  BESTANDSSCHUTZ: beruehrt weder kernel/ NOCH R/export.R NOCH R/manifest.R
#  NOCH eine test_export_*/test_manifest*-Datei — R/manifest.R und R/z3.R
#  werden nur gesourct.
# ============================================================================

.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))  # sha256_datei (ungenutzt hier, Konsistenz)
source(file.path(.root, "R", "z3.R"))

.x_read <- function(pf) {
  ch <- rawToChar(readBin(pf, "raw", n = file.info(pf)$size))
  Encoding(ch) <- "UTF-8"
  ch
}
.x_zwischen <- function(hay, tag) {
  pat <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
  m <- regmatches(hay, gregexpr(pat, hay, perl = TRUE))[[1L]]
  if (!length(m)) return(character(0))
  trimws(sub(sprintf("(?s)^<%s>(.*)</%s>$", tag, tag), "\\1", m, perl = TRUE))
}
.x_eins <- function(hay, tag) {
  v <- .x_zwischen(hay, tag)
  if (length(v)) v[[1L]] else ""
}

# Selbstversorgende Fixture: eine CSV mit numerischer Spalte "betrag"
# (Dezimalkomma) und alphanumerischer Spalte "text"; drei Datenzeilen.
.x_dir <- local({
  d <- file.path(tempdir(), "z3-index-extra")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  writeLines(c("betrag;text", "1000,5;alpha", "-2,25;beta", "3;gamma"),
             file.path(d, "mix.csv"), sep = "\r\n", useBytes = TRUE)
  d
})

test_that("Spaltentyp: numerische Spalte -> numeric, Textspalte -> alphanumeric", {
  export_z3_index(.x_dir, "mix.csv")
  idx <- .x_read(file.path(.x_dir, "index.xml"))
  vars <- regmatches(idx, gregexpr("(?s)<Variable>.*?</Variable>", idx, perl = TRUE))[[1L]]
  typ <- setNames(vapply(vars, function(v) .x_eins(v, "Type"), character(1L)),
                  vapply(vars, function(v) .x_eins(v, "Name"), character(1L)))
  expect_identical(unname(typ[["betrag"]]), "numeric")
  expect_identical(unname(typ[["text"]]),   "alphanumeric")
})

test_that("Validity/Range: <To> == Anzahl der Datenzeilen (extern gezaehlt)", {
  export_z3_index(.x_dir, "mix.csv")
  idx <- .x_read(file.path(.x_dir, "index.xml"))
  # Externes Orakel: Datenzeilen der CSV unabhaengig von index.xml zaehlen.
  ll <- readLines(file.path(.x_dir, "mix.csv"), warn = FALSE)
  n_daten <- sum(nzchar(ll[-1L]))
  expect_true(grepl("<Validity>", idx, fixed = TRUE))
  expect_true(grepl("<Range>", idx, fixed = TRUE))
  expect_identical(.x_eins(idx, "From"), "1")
  expect_identical(.x_eins(idx, "To"), as.character(n_daten))
})

test_that("Determinismus: zwei Laeufe erzeugen bit-identische index.xml", {
  export_z3_index(.x_dir, "mix.csv")
  a <- readBin(file.path(.x_dir, "index.xml"), "raw",
               n = file.info(file.path(.x_dir, "index.xml"))$size)
  export_z3_index(.x_dir, "mix.csv")
  b <- readBin(file.path(.x_dir, "index.xml"), "raw",
               n = file.info(file.path(.x_dir, "index.xml"))$size)
  expect_identical(a, b)
})
