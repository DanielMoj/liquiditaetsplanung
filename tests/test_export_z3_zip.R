# ============================================================================
#  tests/test_export_z3_zip.R  —  ZUSAETZLICHER Test (rot_zuerst: false) fuer
#  phase-13-z3-ui: byte-identischer Roundtrip des Basis-R-Store-ZIP-Schreibers
#  .z3_zip_store.
#
#  Warum ueber die Spec hinaus: die Spec-Tests pruefen PK-Magic + Dateipraesenz
#  nach utils::unzip, NICHT die Byte-Treue des Inhalts. utils::unzip (internes
#  miniz) WARNT bei falscher CRC evtl. nur und schreibt die korrupte Datei
#  trotzdem — ein reiner Existenz-/Namens-Check ist gegen eine verbogene
#  Groessen-/CRC-/Offset-Kodierung blind (Lehre: [[utils-zip-kein-binary]]).
#  Dieser Test schliesst das Loch: er zippt ein Verzeichnis mit gemischten
#  Inhalten (ASCII, UTF-8-Umlaute, CRLF, roh-binaer) und verlangt, dass jede
#  entpackte Datei BYTE-IDENTISCH zur Quelle ist (Toleranz 0). Ausserdem: die
#  ersten zwei Bytes sind 'P' 'K' (ZIP-Local-Header-Signatur) und utils::zip
#  wird NICHT gebraucht (der Store-ZIP entsteht rein aus Basis-R + digest-CRC).
#
#  SPEICHER-ENTLASTUNG (Gate-A): .z3_zip_store ist eine reine Basis-R-Funktion
#  und liegt jetzt in R/z3.R (vorher inline in app/app.R). Dieser Test sourct
#  darum NUR R/z3.R (leichtgewichtig: keine Shiny-UI/-Server-Konstruktion, keine
#  15 R-Phasen-Quellen, kein openxlsx2, kein mirai) statt das GANZE app/app.R
#  nur fuer diese eine Funktion — eine schwere app.R-Allokation weniger im EINEN
#  run_all.R-Prozess. Die Funktionsruempfe der uebrigen z3.R-Funktionen werden
#  beim Sourcen NICHT ausgewertet; nur .z3_zip_store (Basis-R + digest) laeuft.
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

# Nur die Wrapper-Heimat des Packers laden. R/z3.R zieht sich seine
# Format-Heimat bei Bedarf selbst nach (siehe Kopf von R/z3.R); .z3_zip_store
# selbst braucht ausser digest keine weitere Quelle. In eine eigene Umgebung
# sourcen, damit der Test-Namensraum sauber bleibt.
.z3_env <- new.env(parent = globalenv())
source(file.path(.root, "R", "z3.R"), local = .z3_env)

test_that(".z3_zip_store: PK-Magic, kein utils::zip, byte-identischer Roundtrip (Toleranz 0)", {
  zip_store <- .z3_env$.z3_zip_store
  expect_true(is.function(zip_store))

  # Quell-Verzeichnis mit gemischten Inhalten: ASCII, UTF-8-Umlaute, CRLF und
  # ein roh-binaerer Block (alle 256 Byte-Werte) — deckt Encoding-/Zeilenende-
  # /Nullbyte-Fallen ab.
  src <- file.path(tempdir(), "z3-zip-src")
  unlink(src, recursive = TRUE); dir.create(src, recursive = TRUE)

  writeBin(charToRaw("a;b;c\r\n1;2;3\r\n"), file.path(src, "eingaben.csv"))
  writeBin(charToRaw(enc2utf8("Groesse;Wert\r\nKK_Zins;0,03\r\nUmlaut;äöü\r\n")),
           file.path(src, "forecast.csv"))
  writeBin(as.raw(0:255), file.path(src, "index.xml"))
  dateien <- c("eingaben.csv", "forecast.csv", "index.xml")

  zf <- file.path(tempdir(), "z3-zip-out.zip")
  unlink(zf)
  zip_store(zf, src, dateien)

  # PK-Magic (ZIP-Local-Header-Signatur).
  expect_true(file.exists(zf))
  expect_identical(rawToChar(readBin(zf, "raw", 2L)), "PK")

  # Entpacken mit Basis-R (internes miniz, kein externes Binary).
  ex <- file.path(tempdir(), "z3-zip-unzip")
  unlink(ex, recursive = TRUE); dir.create(ex, recursive = TRUE)
  utils::unzip(zf, exdir = ex)

  bn <- basename(list.files(ex, recursive = TRUE))
  expect_setequal(bn, dateien)

  # Byte-Identitaet je Datei (Toleranz 0) — der store-ZIP darf nichts verbiegen.
  for (dn in dateien) {
    q <- file.path(src, dn); z <- file.path(ex, dn)
    qb <- readBin(q, "raw", n = file.info(q)$size)
    zb <- readBin(z, "raw", n = file.info(z)$size)
    expect_identical(zb, qb, info = dn)
  }
})
