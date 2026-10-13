# ============================================================================
#  tests/test_z3_zip_haertung.R  —  SPEC-COMMIT (rot_zuerst: TRUE) fuer die
#  .z3_zip-Haertung (Review-Nachholungs-Befund,
#  .orch/review-pakete/befund-paket-phase-13-z3-ui-hybridfix.md):
#    High   z3.R:526 — die Zip-Slip-Wall fehlt im PRIMAERPFAD von .z3_zip
#                      (utils::zip packt einen traversierenden Eintrag wie
#                      "../ausserhalb.txt" widerspruchslos; die Wall greift
#                      heute erst im .z3_zip_store-Fallback).
#    Medium z3.R:532 — der PK-Prefix-Check ist KEINE Gueltigkeitspruefung
#                      (ein 28-Byte-Torso mit PK-Kopf gilt als "gueltig").
#
#  ZIELSEMANTIK (Impl folgt wortgleich):
#    .z3_zip_wall(dateien)          — die Zip-Slip-Wall als EINE benannte
#      Funktion (Extraktion aus .z3_zip_store, kuenftig von .z3_zip UND
#      .z3_zip_store am Anfang gerufen). Fuer jeden Eintrag dn: ist dn nicht
#      (character && length 1 && !is.na && nzchar && identical(dn, basename(dn))
#      && kein Treffer grepl("[/\\\\]", dn) && nicht "." und nicht "..")
#      -> stop mit einer Meldung, die den Substring
#      "unsicherer ZIP-Eintrag (kein flacher Basisname)" und den Eintrag
#      traegt, call. = FALSE. Rueckgabe invisible(TRUE).
#    .z3_zip_gueltig(zipfile, dateien) — echte Gueltigkeitspruefung statt
#      PK-Prefix: FALSE wenn !file.exists(zipfile) oder
#      isTRUE(file.info(zipfile)$size < 4); sonst
#      le <- tryCatch(utils::unzip(zipfile, list = TRUE),
#                     error = function(e) NULL, warning = function(w) NULL);
#      FALSE bei is.null(le); sonst setequal(le$Name, dateien).
#      Rein lesend, internes miniz, kein externes Binary.
#    .z3_zip ruft kuenftig ZUERST .z3_zip_wall(dateien) (vor setwd/unlink/
#    utils::zip — kein Byte wird vorher angefasst) und ersetzt die
#    PK-Prefix-Entscheidung durch gueltig <- .z3_zip_gueltig(zipfile, dateien).
#
#  ROT-ZUERST: gegen den aktuellen Stand von R/z3.R sind diese Tests rot —
#  .z3_zip_wall und .z3_zip_gueltig existieren noch nicht (H1/H2), und der
#  Primaerpfad von .z3_zip packt die Traversierung widerspruchslos (H3, auf
#  einer Box MIT externem zip-Binary).
#
#  EHRLICH BENANNTE GRENZE: die VERDRAHTUNG des gueltig-Helfers im Torso-Fall
#  (utils::zip schreibt ein kaputtes Archiv mit PK-Kopf) ist ohne Mock des
#  Namespace-Aufrufs nicht deterministisch integrationstestbar — die
#  Entscheidungslogik selbst ist darum als benannter purer Helfer extrahiert
#  und hier direkt gepinnt (etabliertes Muster: Logik als benannte pure
#  Funktion extrahieren).
#
#  SPEICHER-ENTLASTUNG (Gate-A): wie test_z3_zip_wrapper.R sourct dieser Test
#  NUR R/z3.R (keine Shiny-UI/-Server-Konstruktion, keine 15 R-Phasen-Quellen,
#  kein openxlsx2, kein mirai). Kein source(app.R) => keine mirai-Daemons =>
#  keine Daemon-Hygiene noetig.
#
#  BESTANDSSCHUTZ: reine Neudatei; beruehrt weder kernel/ noch app/app.R noch
#  eine bestehende tests/-Datei — R/z3.R wird nur GESOURCT.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Nur die Wrapper-Heimat des Packers laden (leichtgewichtig, in eigene Umgebung).
.z3_env <- new.env(parent = globalenv())
source(file.path(.root, "R", "z3.R"), local = .z3_env)

# H1 — die Zip-Slip-Wall als benannte Funktion: jede Traversierungs-/
# Nicht-Basisnamen-Form stoppt benannt, flache Basisnamen passieren still.
test_that(".z3_zip_wall: benannte Wall — Traversierung stoppt, flache Basisnamen passieren still", {
  expect_true(is.function(.z3_env$.z3_zip_wall))
  wall <- .z3_env$.z3_zip_wall

  # Jeder unsichere Eintrag einzeln — die Meldung traegt den Wall-Substring.
  expect_error(wall("../ausserhalb.txt"), regexp = "unsicherer ZIP-Eintrag")
  expect_error(wall("a/b.csv"),           regexp = "unsicherer ZIP-Eintrag")
  expect_error(wall("a\\b.csv"),          regexp = "unsicherer ZIP-Eintrag")
  expect_error(wall("."),                 regexp = "unsicherer ZIP-Eintrag")
  expect_error(wall(".."),                regexp = "unsicherer ZIP-Eintrag")

  # Flache Basisnamen: still (invisible) und TRUE.
  res <- expect_invisible(wall(c("eingaben.csv", "index.xml")))
  expect_true(res)
})

# H2 — echte Gueltigkeitspruefung statt PK-Prefix: Eintragsmenge muss stimmen,
# ein PK-Torso ist KEIN gueltiges Archiv.
test_that(".z3_zip_gueltig: echte Archiv-Pruefung (Eintragsmenge, Torso, fehlender Pfad)", {
  expect_true(is.function(.z3_env$.z3_zip_gueltig))
  gueltig <- .z3_env$.z3_zip_gueltig

  # Frisches Archiv per Basis-R-Store-Schreiber (kein externes Binary).
  src <- file.path(tempdir(), "z3-haert-h2-src")
  unlink(src, recursive = TRUE); dir.create(src, recursive = TRUE)
  writeBin(charToRaw("a;b\r\n1;2\r\n"), file.path(src, "eingaben.csv"))
  writeBin(charToRaw("<DataSet/>\n"),   file.path(src, "index.xml"))
  dateien <- c("eingaben.csv", "index.xml")

  zf <- file.path(tempdir(), "z3-haert-h2-out.zip")
  unlink(zf)
  .z3_env$.z3_zip_store(zf, src, dateien)

  # (a) exakt passende Eintragsmenge -> TRUE.
  expect_true(gueltig(zf, dateien))

  # (b) FALSCHE Eintragsmenge (ein Name ergaenzt) -> FALSE.
  expect_false(gueltig(zf, c(dateien, "fehlt.csv")))

  # (c) PK-TORSO: PK\003\004-Kopf plus 24 Nullbytes — kein gueltiges Archiv.
  #     Der ALTE PK-Prefix-Check (>= 4 Byte + "PK"-Signatur) haette hier TRUE
  #     gesagt; die echte Pruefung muss FALSE liefern.
  torso <- file.path(tempdir(), "z3-haert-h2-torso.zip")
  writeBin(c(as.raw(c(0x50, 0x4b, 0x03, 0x04)), as.raw(rep(0x00, 24))), torso)
  expect_false(gueltig(torso, dateien))

  # (d) nicht existierender Pfad -> FALSE.
  expect_false(gueltig(file.path(tempdir(), "z3-haert-h2-gibtsnicht.zip"), dateien))

  # Aufraeumen aller tempdir-Artefakte dieses Tests.
  unlink(c(zf, torso))
  unlink(src, recursive = TRUE)
})

# H3 — Integration: die Wall steht kuenftig VOR dem Primaerpfad von .z3_zip
# (vor setwd/unlink/utils::zip). Gegen den Ist-Stand ist dieser Test auf einer
# Box MIT externem zip-Binary rot: der Primaerpfad packt die Traversierung
# widerspruchslos (genau der High-Befund z3.R:526). Auf einer binaerlosen Box
# griffe erst die Fallback-Wall in .z3_zip_store — die Zielsemantik verlangt
# den Stopp aber bereits an der Naht, bevor irgendein Byte angefasst wird.
test_that(".z3_zip: Zip-Slip-Wall im Primaerpfad — Traversierung stoppt, kein Archiv entsteht", {
  basis <- file.path(tempdir(), "z3-haert-h3")
  unlink(basis, recursive = TRUE)
  dir <- file.path(basis, "buendel")
  dir.create(dir, recursive = TRUE)

  # Echte Datei im Buendel-Verzeichnis + eine echte Datei AUSSERHALB (im
  # Parent von dir), die ein traversierender Eintrag packen wuerde.
  writeBin(charToRaw("kennzahl;wert\r\n"), file.path(dir, "eingaben.csv"))
  writeBin(charToRaw("GEHEIM-AUSSERHALB"), file.path(basis, "ausserhalb.txt"))

  zf <- file.path(basis, "z3-haert-h3-out.zip")
  expect_error(.z3_env$.z3_zip(zf, dir, c("../ausserhalb.txt")),
               regexp = "unsicherer ZIP-Eintrag")

  # Kein Archiv entstanden, kein Byte gepackt — die Wall stand VOR
  # setwd/unlink/utils::zip.
  expect_false(file.exists(zf))

  # Aufraeumen aller tempdir-Artefakte dieses Tests.
  unlink(basis, recursive = TRUE)
})
