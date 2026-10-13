# ============================================================================
#  tests/test_protokoll_anhaengen_extra.R  —  ZUSATZ-Tests (rot_zuerst: false)
#  fuer phase-12-protokoll-anhaengen.
#
#  Ergaenzen die Spezifikation tests/test_protokoll_anhaengen.R um zwei
#  Implementierungs-Zweige, die die Spec nicht anfaehrt:
#   (a) leere daten (list()) — vom Vertrag ausdruecklich erlaubt — werden als
#       JSON-Objekt {} geschrieben und roundtrippen als benannte Leerliste,
#       nicht als Array; der Hash deckt sich mit dem daten-leer-Zweig von
#       .protokoll_hash (daten_kanon '').
#   (b) APPEND-ONLY als reines Byte-Wachstum: die Dateigroesse waechst monoton,
#       der Bestand vor dem Anhaengen ist ein exaktes Byte-Praefix danach.
#
#  Kein skip, kein succeed, keine Anker-/Tier-1-Literale, kein MC, keine Engine,
#  keine Fixture-Mappe — nur Tempdateien mit injizierter POSIXct-zeit.
# ============================================================================

.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "protokoll.R"))

.z1x <- as.POSIXct("2026-07-09 08:00:00", tz = "UTC")
.z2x <- as.POSIXct("2026-07-09 09:00:00", tz = "UTC")


test_that("protokoll_anhaengen: leere daten werden als JSON-Objekt {} geschrieben", {
  p <- file.path(tempdir(), "protokoll-extra-leer.jsonl")
  unlink(p)

  e <- protokoll_anhaengen(p, "finalisierung", list(), zeit = .z1x, user = "anker")

  # Rueckgabe traegt die Original-daten (leere Liste) und die sieben Felder.
  expect_identical(names(e),
                   c("seq", "typ", "zeitstempel", "user", "daten",
                     "vorheriger_hash", "hash"))
  expect_length(e$daten, 0L)

  # Auf der Platte: daten steht als Objekt {} da (nicht als Array []).
  zeile <- readLines(p, warn = FALSE)
  expect_length(zeile, 1L)
  expect_true(grepl('"daten":\\{\\}', zeile[[1]]))

  # Roundtrip: fromJSON parst daten als benannte (leere) Liste, kein Vektor.
  ev <- jsonlite::fromJSON(zeile[[1]])
  expect_type(ev$daten, "list")
  expect_length(ev$daten, 0L)

  # Delegation: der Hash deckt sich mit dem daten-leer-Zweig von .protokoll_hash.
  expect_identical(
    e$hash,
    .protokoll_hash(e$seq, e$typ, e$zeitstempel, e$user, list(),
                    e$vorheriger_hash))

  unlink(p)
})

test_that("protokoll_anhaengen: append-only als monotones Byte-Praefix", {
  p <- file.path(tempdir(), "protokoll-extra-bytes.jsonl")
  unlink(p)

  protokoll_anhaengen(p, "import", list(name = "M.xlsx", menge = -0.1),
                      zeit = .z1x, user = "anker")
  roh_vorher <- readBin(p, "raw", n = file.info(p)$size)

  protokoll_anhaengen(p, "lauf", list(run_id = "run-20260709T080000Z-abcd1234"),
                      zeit = .z2x, user = "anker")
  roh_nachher <- readBin(p, "raw", n = file.info(p)$size)

  # Die Datei ist gewachsen ...
  expect_gt(length(roh_nachher), length(roh_vorher))
  # ... und der alte Inhalt ist ein exaktes Byte-Praefix des neuen (kein Byte
  # des Bestands wurde beruehrt).
  expect_identical(roh_nachher[seq_along(roh_vorher)], roh_vorher)

  unlink(p)
})
