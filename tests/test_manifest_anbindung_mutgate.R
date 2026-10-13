# ============================================================================
#  tests/test_manifest_anbindung_mutgate.R — Mutgate-Nachschub (M5):
#  v2-Ruecklese-Normalisierung der kontrollen beobachtbar gepinnt
#
#  Ueberlebender Mutant M5 (Mutgate-Lauf auf cc82423): die Versionskonstante
#  der kontrollen-Normalisierung in manifest_lesen wurde 2L -> 1L gedreht —
#  die Suite blieb gruen, weil jsonlite fuer manifest_schreiben-EIGENE
#  Dateien die Spaltentypen ohnehin korrekt rekonstruiert: die Normalisierung
#  war ueber solche Dateien nie beobachtbar. Dieses Orakel macht sie
#  beobachtbar (Eigenschaft, nicht Mutanten-Diff): die retrograde Pruefung
#  (Rz. 152) haengt an der RUECKLESE, nicht am Schreiber — ein v2-
#  manifest.json, dessen kontrollen-Typen im JSON verwaschen sind (bestanden
#  als String 'true'/'false' statt JSON-Boolean, wie es fremde/aeltere
#  Schreiber liefern), muss beim Lesen auf den Spaltenvertrag normalisiert
#  werden (bestanden wieder logical, Werte-Treue) und den Validator bestehen.
#  Kill-Logik als Rot-zuerst-Ersatz: auf dem erneut angewandten M5 rot
#  (Normalisierung fuer v2 uebersprungen -> Befund kontrollen$bestanden),
#  auf main gruen. NEUE Datei — Test-Bestand bleibt unangetastet.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))

test_that("mutgate M5: manifest_lesen normalisiert v2-kontrollen mit String-Booleans ('true'/'false') auf logical und besteht den Validator", {
  # Gueltiges v2-Manifest schreiben (Basisargumente wie im Kern-Test; ein
  # bestanden = FALSE, damit die Normalisierung BEIDE Wahrheitswerte trifft
  # und nicht als Konstant-TRUE ueberleben kann).
  a <- tempfile(); writeBin(charToRaw("abc"), a)
  m2 <- manifest_erstellen(
    eingaben = a, parameter = list(H = 4L), seeds = c(sim = 11L),
    zeit = as.POSIXct("2026-07-08 10:34:56", tz = "UTC"), user = "testuser",
    umgebung = list(r_version = as.character(getRversion()),
                    plattform = "testplattform", git_sha = NA_character_,
                    git_dirty = NA, renv_lock_sha256 = NA_character_),
    kontrollen = data.frame(
      name      = c("anker_plan_tiefpunkt", "massenerhaltung"),
      bestanden = c(TRUE, FALSE),
      soll      = c("0", "TRUE"),
      ist       = c("0", "FALSE"),
      details   = c("zweiwegiger Anker", "forecast-Diagnostik"),
      stringsAsFactors = FALSE))
  p <- tempfile(fileext = ".json")
  manifest_schreiben(m2, p)

  # Typ-Drift eines fremden Schreibers auf TEXTEBENE simulieren (keine der
  # getesteten Funktionen im Praeparationsweg): JSON-Booleans der bestanden-
  # Spalte werden zu Strings. 'bestanden' existiert im Manifest nur in
  # kontrollen; umgebung$git_dirty ist hier null (kein Kollateraltreffer).
  txt  <- readLines(p, warn = FALSE)
  txt2 <- gsub('"bestanden": true',  '"bestanden": "true"',  txt,  fixed = TRUE)
  txt2 <- gsub('"bestanden": false', '"bestanden": "false"', txt2, fixed = TRUE)
  # Praeparation muss stattgefunden haben, sonst waere das Orakel tot.
  expect_true(any(grepl('"bestanden": "true"',  txt2, fixed = TRUE)))
  expect_true(any(grepl('"bestanden": "false"', txt2, fixed = TRUE)))
  p2 <- tempfile(fileext = ".json")
  writeLines(txt2, p2)

  # Ruecklese normalisiert: bestanden wieder logical MIT beiden Werten,
  # Ergebnis besteht den Validator, uebrige Felder werte-treu.
  g <- manifest_lesen(p2)
  expect_true(isTRUE(manifest_validieren(g)))
  expect_true(is.logical(g$kontrollen$bestanden))
  expect_identical(g$kontrollen$bestanden, c(TRUE, FALSE))
  expect_identical(g$kontrollen$name,    m2$kontrollen$name)
  expect_identical(g$kontrollen$soll,    m2$kontrollen$soll)
  expect_identical(g$kontrollen$ist,     m2$kontrollen$ist)
  expect_identical(g$kontrollen$details, m2$kontrollen$details)
  expect_identical(g$manifest_version, 2L)
  expect_identical(g$run_id, m2$run_id)

  unlink(c(a, p, p2))
})
