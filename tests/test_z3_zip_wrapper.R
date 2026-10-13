# ============================================================================
#  tests/test_z3_zip_wrapper.R  —  ZUSAETZLICHER Test (rot_zuerst: false) fuer
#  phase-13-z3-ui: die Packer-Naht .z3_zip (Handoff-Vorgabe "via utils::zip").
#
#  Warum ueber die Spec hinaus: der Red-Team-Befund (Scope-Drift) verlangt, dass
#  der Download das Buendel PRIMAER via utils::zip zippt und nur bei fehlendem
#  externem zip-Binary auf den Basis-R-Store-ZIP zurueckfaellt. Dieser Test
#  pinnt genau die Naht .z3_zip: ganz gleich WELCHER Weg laeuft (utils::zip mit
#  vorhandenem Rtools-zip ODER der .z3_zip_store-Fallback), das Ergebnis ist ein
#  gueltiges ZIP mit PK-Signatur, das utils::unzip verlustfrei und BYTE-IDENTISCH
#  (Toleranz 0) entpackt. utils::unzip WARNT bei falscher CRC evtl. nur und
#  schreibt die korrupte Datei trotzdem (Lehre: [[utils-zip-kein-binary]]) —
#  darum die Byte-Identitaet, nicht nur Existenz.
#
#  SPEICHER-ENTLASTUNG (Gate-A): .z3_zip ist reines Basis-R (+ digest im Fallback)
#  und liegt in R/z3.R. Dieser Test sourct darum NUR R/z3.R (keine Shiny-UI/
#  -Server-Konstruktion, keine 15 R-Phasen-Quellen, kein openxlsx2, kein mirai).
#  Kein source(app.R) => keine mirai-Daemons => keine Daemon-Hygiene noetig.
#
#  BESTANDSSCHUTZ: reine Neudatei; beruehrt weder kernel/ noch app/app.R noch
#  eine test_export_*/test_manifest*-Datei — R/z3.R wird nur GESOURCT.
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

test_that(".z3_zip: PK-Magic + byte-identischer Roundtrip (utils::zip-primaer, Fallback egal)", {
  z3_zip <- .z3_env$.z3_zip
  expect_true(is.function(z3_zip))

  # Quell-Verzeichnis mit gemischten Inhalten: ASCII/CRLF, UTF-8-Umlaute und
  # ein roh-binaerer Block (alle 256 Byte-Werte) — deckt Encoding-/Zeilenende-/
  # Nullbyte-Fallen beider Wege ab.
  src <- file.path(tempdir(), "z3-zipwrap-src")
  unlink(src, recursive = TRUE); dir.create(src, recursive = TRUE)
  writeBin(charToRaw("a;b;c\r\n1;2;3\r\n"), file.path(src, "eingaben.csv"))
  writeBin(charToRaw(enc2utf8("Groesse;Wert\r\nKK_Zins;0,03\r\nUmlaut;äöü\r\n")),
           file.path(src, "forecast_kpis.csv"))
  writeBin(as.raw(0:255), file.path(src, "index.xml"))
  dateien <- c("eingaben.csv", "forecast_kpis.csv", "index.xml")

  zf <- file.path(tempdir(), "z3-zipwrap-out.zip")
  unlink(zf)
  ret <- z3_zip(zf, src, dateien)

  # Der Handler schreibt in exakt den uebergebenen Pfad (Shiny-downloadHandler).
  expect_true(file.exists(zf))
  # PK-Magic (ZIP-Local-Header-Signatur) — die harte Spec-Zusage des Handlers.
  expect_identical(rawToChar(readBin(zf, "raw", 2L)), "PK")

  # Entpacken mit Basis-R (internes miniz, kein externes Binary).
  ex <- file.path(tempdir(), "z3-zipwrap-unzip")
  unlink(ex, recursive = TRUE); dir.create(ex, recursive = TRUE)
  utils::unzip(zf, exdir = ex)

  bn <- basename(list.files(ex, recursive = TRUE))
  expect_setequal(bn, dateien)

  # Byte-Identitaet je Datei (Toleranz 0) — der Packer darf nichts verbiegen.
  for (dn in dateien) {
    q <- file.path(src, dn); z <- file.path(ex, dn)
    qb <- readBin(q, "raw", n = file.info(q)$size)
    zb <- readBin(z, "raw", n = file.info(z)$size)
    expect_identical(zb, qb, info = dn)
  }

  # Der Fallback-Pfad allein liefert ebenfalls ein gueltiges, byte-treues Archiv
  # — so ist der Roundtrip auch auf einer Rtools-losen Box (kein utils::zip)
  # bewiesen, nicht nur auf einer mit externem zip-Binary.
  zf2 <- file.path(tempdir(), "z3-zipwrap-fallback.zip")
  unlink(zf2)
  .z3_env$.z3_zip_store(zf2, src, dateien)
  expect_identical(rawToChar(readBin(zf2, "raw", 2L)), "PK")
  ex2 <- file.path(tempdir(), "z3-zipwrap-fallback-unzip")
  unlink(ex2, recursive = TRUE); dir.create(ex2, recursive = TRUE)
  utils::unzip(zf2, exdir = ex2)
  for (dn in dateien) {
    q <- file.path(src, dn); z <- file.path(ex2, dn)
    qb <- readBin(q, "raw", n = file.info(q)$size)
    zb <- readBin(z, "raw", n = file.info(z)$size)
    expect_identical(zb, qb, info = paste("fallback", dn))
  }
})
