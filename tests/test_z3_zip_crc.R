# ============================================================================
#  tests/test_z3_zip_crc.R  —  Mutgate-Nachschub fuer phase-13-z3-ui:
#  pinnt die Byte-Reihenfolge des CRC-32 im Local-File-Header des
#  Store-ZIP-Fallbacks (.z3_zip_store in R/z3.R).
#
#  HERKUNFT: Mutgate-Survivor M3 (.orch/mut-phase-13-z3-ui/M3.diff) entfernt
#  in crc_le das rev(...) um den as.raw-Ausdruck — der CRC landet dann
#  Big-Endian statt (ZIP-konform) Little-Endian im Header. Der bestehende
#  Roundtrip-Test (test_z3_zip_wrapper.R) toetet das nicht zuverlaessig:
#  utils::unzip (internes miniz) WARNT bei falscher CRC evtl. nur und
#  schreibt die Datei trotzdem byte-identisch heraus (Lehre:
#  [[utils-zip-kein-binary]]) — die CRC-Validierung ist dort kein harter
#  Fehlerpfad. Darum hier der direkte Byte-Pin auf das Archiv selbst.
#
#  BESTANDSSCHUTZ: reine Neudatei; R/z3.R wird nur GESOURCT (leichtgewichtig,
#  wie in test_z3_zip_wrapper.R — kein app.R, kein mirai, kein openxlsx2).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Nur die Wrapper-Heimat des Packers laden (in eigene Umgebung).
.z3_env <- new.env(parent = globalenv())
source(file.path(.root, "R", "z3.R"), local = .z3_env)

test_that("Store-Zip: CRC-32 steht Little-Endian im Local-File-Header (Mutgate-Survivor M3: rev() entfernt)", {
  zip_store <- .z3_env$.z3_zip_store
  expect_true(is.function(zip_store))

  # Selbstversorgend: EINE kleine Datei bekannten Inhalts. Direkt der
  # Fallback-Schreiber .z3_zip_store — er ist die Zielzone des Mutanten
  # (der Hybrid .z3_zip nimmt bei vorhandenem zip-Binary utils::zip und
  # liefe am mutierten Code vorbei).
  inhalt <- "GoBD-CRC-Probe"
  src <- file.path(tempdir(), "z3-crc-src")
  unlink(src, recursive = TRUE); dir.create(src, recursive = TRUE)
  writeBin(charToRaw(inhalt), file.path(src, "probe.txt"))

  zf <- file.path(tempdir(), "z3-crc-out.zip")
  unlink(zf)
  zip_store(zf, src, "probe.txt")

  # Archiv roh lesen und den Local-File-Header der ersten (einzigen) Datei
  # parsen: Signatur PK\003\004 an Byte 1:4, CRC-32-Feld ab Offset 14
  # (1-basiert 15:18).
  archiv <- readBin(zf, "raw", n = file.info(zf)$size)
  expect_identical(archiv[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04)))

  # EXTERNES Orakel, eigenstaendig formuliert (nicht der Produktionscode):
  # digest liefert den CRC-32 als Big-Endian-Hexstring; das ZIP-Format
  # verlangt die vier Bytes Little-Endian — also Hex-Bytepaare zu raw
  # wandeln und die Reihenfolge umdrehen.
  crc_hex <- digest::digest(charToRaw(inhalt), algo = "crc32", serialize = FALSE)
  soll_le <- rev(as.raw(strtoi(substring(crc_hex, c(1, 3, 5, 7), c(2, 4, 6, 8)), 16L)))
  expect_identical(archiv[15:18], soll_le)

  # Zusatz-Pin: Big-Endian (= soll_le rueckwaerts) ist exakt der Mutant M3.
  # Der Pin greift nur, wenn die vier CRC-Bytes kein Palindrom sind — der
  # Probeninhalt "GoBD-CRC-Probe" erfuellt das (LE != BE), sonst waere die
  # Little-Endian-Erwartung oben ohnehin unentscheidbar.
  expect_false(identical(archiv[15:18], rev(soll_le)))
})
