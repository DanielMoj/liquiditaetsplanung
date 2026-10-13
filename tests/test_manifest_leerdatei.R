# ============================================================================
#  tests/test_manifest_leerdatei.R  —  Mutgate-Nachschub Runde 5 fuer
#  phase-12-runmanifest-kern
#
#  Zweck: der Randwert 0 Byte wird durch den GANZEN Pfad getrieben
#  (erstellen -> validieren -> schreiben -> lesen -> validieren) und killt
#  gezielt die Grenzverschiebungs-Klasse am Validator: Ueberlebender M3
#  verschiebt die groesse_bytes-Grenze von < 0 auf <= 0, womit 0-Byte-
#  Dateien faelschlich abgelehnt wuerden. Der Vertrag sagt woertlich
#  'numerisch >= 0' — Groesse 0 MUSS validieren.
#
#  Orakel-Politik wie tests/test_manifest.R: externes Literal-Orakel, kein
#  Selbstbezug. Der SHA-256 der leeren Nachricht stammt aus NIST FIPS 180-4
#  (eingefrorener Testvektor) und pinnt hier einen ZWEITEN unabhaengigen
#  Pfad (durch manifest_erstellen statt sha256_datei direkt). Keine Anker-/
#  Tier-1-Literale, kein skip, kein succeed.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))

test_that("manifest: 0-Byte-Datei ist gueltige Eingabe (Randwert der groesse_bytes-Grenze)", {
  # Echte 0-Byte-Datei als einzige Eingabe.
  f0 <- tempfile()
  file.create(f0)

  # Injizierte fixe Umgebung wie in tests/test_manifest.R — der reine Kern
  # bleibt deterministisch testbar.
  umg <- list(r_version        = as.character(getRversion()),
              plattform        = "testplattform",
              git_sha          = NA_character_,
              git_dirty        = NA,
              renv_lock_sha256 = NA_character_)

  m <- manifest_erstellen(eingaben = f0, parameter = list(),
                          seeds = c(sim = 11L), outputs = character(0),
                          zeit = as.POSIXct("2026-07-08 10:34:56", tz = "UTC"),
                          user = "leer", umgebung = umg)

  # NIST-FIPS-180-4-Literal (SHA-256 der leeren Nachricht) — zweiter
  # unabhaengiger Pfad-Pin des Leer-Hashes, hier durch manifest_erstellen.
  expect_identical(
    m$eingaben$sha256,
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  expect_identical(m$eingaben$groesse_bytes, 0)

  # M3-Kill: Groesse 0 MUSS validieren (Vertrag: 'numerisch >= 0');
  # eine Grenze <= 0 lehnt genau diesen Randwert faelschlich ab.
  expect_true(isTRUE(manifest_validieren(m)))

  # Roundtrip: die 0 ueberlebt JSON unverfaelscht.
  p <- tempfile(fileext = ".json")
  manifest_schreiben(m, p)
  g <- manifest_lesen(p)
  expect_true(isTRUE(manifest_validieren(g)))
  expect_identical(g$eingaben$groesse_bytes, 0)

  unlink(c(f0, p))
})
