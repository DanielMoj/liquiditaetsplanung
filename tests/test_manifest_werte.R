# ============================================================================
#  tests/test_manifest_werte.R -- Mutgate-Nachschub Runde 4 fuer
#  phase-12-runmanifest-kern
#
#  Zweck: die NA-Blindheits-Klasse schliessen. Die bisherigen Tests
#  injizieren die umgebung-Felder als NA, und NA ist blind: eine Weitung
#  des git_sha-Hexfensters im Validator (40L -> 64L) faellt nie auf, weil
#  kein Test ein Manifest mit ECHTER 40-Hex-sha validiert (M4); eine
#  Invertierung von git_dirty in manifest_lesen (!as.logical) ist auf
#  NA-Roundtrips unsichtbar, weil !NA = NA (M5). Dieser Nachschub zieht
#  KONKRETE umgebung-Werte durch den ganzen Pfad Validator -> schreiben ->
#  lesen (beide git_dirty-Polaritaeten) und pinnt zusaetzlich die
#  Fenster-ASYMMETRIE (40 Hexzeichen fuer git_sha, 64 fuer
#  renv_lock_sha256) mit Negativ-Pins am Validator gegen jede
#  Vertauschung oder Weitung.
#
#  Orakel-Politik: das 40-Hex-Literal ist HANDGEBAUT (das Muster
#  '0123456789abcdef' zweieinhalbmal wiederholt, 40 Zeichen); das
#  64-Hex-Literal ist der eingefrorene NIST-FIPS-180-4-Testvektor
#  SHA-256('abc') -- beide woertlich, nie per Aufruf der getesteten
#  Funktionen erzeugt. Keine Anker-/Tier-1-Literale, kein skip, kein
#  succeed, nur Tempdateien mit bekanntem Byte-Inhalt.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))

test_that("manifest: konkrete umgebung-Werte ueberleben Validator und JSON-Roundtrip unverfaelscht", {
  # Handgebaute Umgebung mit KONKRETEN Werten statt NA:
  #   git_sha          -> handgebautes 40-Hex-Literal
  #   renv_lock_sha256 -> NIST FIPS 180-4, SHA-256('abc') (64 Hexzeichen)
  mk_umg <- function(dirty) {
    list(r_version        = as.character(getRversion()),
         plattform        = "werte-test",
         git_sha          = "0123456789abcdef0123456789abcdef01234567",
         git_dirty        = dirty,
         renv_lock_sha256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  }

  eingabe <- tempfile()
  writeBin(charToRaw("abc"), eingabe)
  zeit <- as.POSIXct("2026-07-08 10:34:56", tz = "UTC")

  m_true <- manifest_erstellen(eingaben = eingabe, parameter = list(H = 4L),
                               seeds = c(sim = 11L), zeit = zeit,
                               user = "werte", umgebung = mk_umg(TRUE))

  # (1) Validator-Positiv: ein Manifest mit ECHTER 40-Hex-sha MUSS
  #     validieren (eine Weitung des Fensters auf 64 reisst hier).
  expect_true(isTRUE(manifest_validieren(m_true)))

  # (2) Roundtrip-Polaritaet TRUE: konkrete Werte kommen unverfaelscht
  #     durch schreiben -> lesen (eine Invertierung von git_dirty reisst).
  p1 <- tempfile(fileext = ".json")
  manifest_schreiben(m_true, p1)
  g1 <- manifest_lesen(p1)
  expect_identical(g1$umgebung$git_dirty, TRUE)
  expect_identical(g1$umgebung$git_sha,
                   "0123456789abcdef0123456789abcdef01234567")
  expect_identical(g1$umgebung$renv_lock_sha256,
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

  # (3) Roundtrip-Polaritaet FALSE: jede Invertierung reisst mindestens
  #     eine der beiden Polaritaeten.
  m_false <- manifest_erstellen(eingaben = eingabe, parameter = list(H = 4L),
                                seeds = c(sim = 11L), zeit = zeit,
                                user = "werte", umgebung = mk_umg(FALSE))
  p2 <- tempfile(fileext = ".json")
  manifest_schreiben(m_false, p2)
  g2 <- manifest_lesen(p2)
  expect_identical(g2$umgebung$git_dirty, FALSE)

  # (4) Fenster-Negativ-Pins am Validator, je von einer FRISCHEN Kopie des
  #     gueltigen m_true -- pinnt die Asymmetrie 40 (git) / 64 (renv).

  # (4a) git_sha auf das 64-Hex-NIST-Literal gesetzt -> zu lang fuer git.
  v <- m_true
  v$umgebung$git_sha <- "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  expect_error(manifest_validieren(v), regexp = "git_sha")

  # (4b) git_sha auf 39 Zeichen gekuerzt -> zu kurz.
  v <- m_true
  v$umgebung$git_sha <- substr("0123456789abcdef0123456789abcdef01234567", 1, 39)
  expect_error(manifest_validieren(v), regexp = "git_sha")

  # (4c) renv_lock_sha256 auf das 40-Hex-Literal gesetzt -> zu kurz fuer renv.
  v <- m_true
  v$umgebung$renv_lock_sha256 <- "0123456789abcdef0123456789abcdef01234567"
  expect_error(manifest_validieren(v), regexp = "renv_lock_sha256")

  unlink(c(eingabe, p1, p2))
})
