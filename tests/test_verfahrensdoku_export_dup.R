# ============================================================================
#  tests/test_verfahrensdoku_export_dup.R  —  Nachtrag zu
#  phase-15-verfahrensdoku-export: Fehlerfall DOPPELTER erlaubter Sektionsname.
#
#  WARUM DIESE (NEUE) DATEI
#  ------------------------
#  Die setdiff-Validierung in .verfahrensdoku_export_pruefen() prueft nur
#  "fehlend" und "ueberzaehlig". Ein DOPPELTER erlaubter Name (z.B. zwei "meta")
#  laesst beide setdiffs leer und rutschte ohne die Anzahl/Duplikat-Wache in ein
#  doppeltes/sechstes Blatt bzw. einen spaeten openxlsx2-Fehler. Diese Datei
#  pinnt: doppelter erlaubter Sektionsname -> benennbarer stop() VOR jedem
#  Schreiben; die Gegenprobe sichert, dass die Wache ein gueltiges vd NICHT
#  faelschlich rejiziert (Waechter gegen Rueckfall). Die bestehende Spezifikation
#  tests/test_verfahrensdoku_export.R ist bestandsgeschuetzt und bleibt
#  unangetastet.
#
#  Kein skip(), kein succeed(), kein Engine-Aufruf, kein Tier-1-Zahlen-Literal,
#  kleine Laufzeit (die Funktion rechnet nicht).
# ============================================================================

library(testthat)

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "verfahrensdoku.R"))

# INJIZIERTE umgebung als eigene Quelle (deterministisch, nicht-trivial).
.inj <- list(r_version        = "9.9.9",
             plattform        = "vdExportDupAnker",
             git_sha          = "abc123def4567890abcdef1234567890abcdef12",
             git_dirty        = TRUE,
             renv_lock_sha256 = "feedface00112233")
.vd <- verfahrensdoku_erstellen(umgebung = .inj)

test_that("Fehlerfall: doppelter erlaubter Sektionsname (zwei 'meta') wird benennbar abgelehnt, kein Teil-Schreiben", {
  tf <- tempfile(fileext = ".xlsx")

  # sechster Eintrag mit dem erlaubten Namen "meta" -> Name doppelt; die beiden
  # setdiffs sind leer, erst die Anzahl/Duplikat-Wache greift.
  vd_dup <- c(.vd, list(meta = .vd$meta))
  expect_identical(names(vd_dup),
                   c("meta", "allgemeine_beschreibung", "betriebsdoku",
                     "bestand", "review", "meta"))

  # stop() nennt den doppelten Sektionsnamen (benennbarer Befund).
  expect_error(export_verfahrensdoku(vd_dup, tf), regexp = "meta")
  # kein Blatt geschrieben (Pruefung vor der Serialisierung).
  expect_false(file.exists(tf))
})

test_that("Gegenprobe: ein gueltiges vd exportiert weiter (Wache rejiziert nicht zu viel)", {
  tf <- tempfile(fileext = ".xlsx")
  rueck <- export_verfahrensdoku(.vd, tf)
  expect_identical(rueck$blaetter,
                   c("meta", "allgemeine_beschreibung", "betriebsdoku",
                     "bestand", "review"))
  expect_true(file.exists(tf))
})
