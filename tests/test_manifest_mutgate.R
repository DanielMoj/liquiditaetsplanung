# ============================================================================
#  tests/test_manifest_mutgate.R  --  MUTGATE-NACHSCHUB fuer
#  phase-12-runmanifest-kern
#
#  Zweck: gezielter Kill gegen den ueberlebenden Mutanten M5, der 'user' aus
#  der kanonischen Verkettung der run_id in manifest_erstellen entfernt.
#  Getestet wird der VERTRAG, nicht der Mutanten-Diff: das Handoff nennt user
#  als Glied der kanonischen Verkettung (sortierte Eingabe-Hashes, Seed-Namen,
#  Seed-Werte, user) -- also MUSS ein anderer user bei sonst identischen
#  Argumenten eine andere run_id ergeben (Eigenschaftstest), und die
#  SORTIERUNG der Eingabe-Hashes MUSS die run_id gegen die Aufrufreihenfolge
#  der eingaben invariant machen, waehrend die eingaben-data.frames selbst
#  die Aufrufreihenfolge tragen.
#
#  Orakel-Politik wie tests/test_manifest.R: keine Nutzung der getesteten
#  Funktionen zur Erwartungswert-Erzeugung -- der erwartete run_id-Zeitanteil
#  (run-20260708T103456Z-) ist ein handgerechnetes Literal aus der fixen
#  UTC-Zeit, Vergleiche laufen nur ueber Gleichheit/Ungleichheit zwischen
#  unabhaengigen Aufrufen. Keine Anker-/Tier-1-Literale, kein skip(), kein
#  succeed(), keine Fixture-Mappe -- nur Tempdateien mit bekanntem Byte-Inhalt
#  via writeBin(charToRaw(...)).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))

test_that("manifest: run_id ist user-sensitiv und eingaben-reihenfolge-invariant", {
  # Zwei Tempdateien mit bekanntem Byte-Inhalt (verschiedene Bytes ->
  # verschiedene Hashes, damit die Sortierung in der Verkettung greift).
  p1 <- tempfile(); writeBin(charToRaw("mutgate-eingabe-eins"), p1)
  p2 <- tempfile(); writeBin(charToRaw("mutgate-eingabe-zwei"), p2)

  # Helfer: manifest_erstellen mit fixer zeit, fixen seeds und injizierter
  # fixer umgebung -- nur user und eingaben-Vektor variieren.
  mk <- function(user, eingaben) {
    manifest_erstellen(
      eingaben  = eingaben,
      parameter = list(H = 4L),
      seeds     = c(sim = 11L),
      outputs   = character(0),
      zeit      = as.POSIXct("2026-07-08 10:34:56", tz = "UTC"),
      user      = user,
      umgebung  = list(r_version        = as.character(getRversion()),
                       plattform        = "testplattform",
                       git_sha          = NA_character_,
                       git_dirty        = NA,
                       renv_lock_sha256 = NA_character_))
  }

  # (1) user-Sensitivitaet: anderer user, sonst identische Argumente ->
  #     andere run_id. Beide ids tragen denselben handgerechneten
  #     UTC-Zeitanteil -- der Unterschied liegt im 8-Hex-Suffix.
  m_a <- mk("alpha", c(p1, p2))
  m_b <- mk("beta",  c(p1, p2))
  expect_false(identical(m_a$run_id, m_b$run_id))
  runid_fix_re <- "^run-20260708T103456Z-[0-9a-f]{8}$"
  expect_true(grepl(runid_fix_re, m_a$run_id))
  expect_true(grepl(runid_fix_re, m_b$run_id))

  # (2) Positivkontrolle: zweiter Aufruf mit user 'alpha' -> identische
  #     run_id (der Unterschied in (1) kommt vom user, nicht von Rauschen).
  m_a2 <- mk("alpha", c(p1, p2))
  expect_identical(m_a2$run_id, m_a$run_id)

  # (3) Reihenfolge-Invarianz der kanonischen SORTIERUNG: vertauschte
  #     eingaben-Reihenfolge bei gleichem user -> identische run_id, waehrend
  #     die eingaben-data.frames die jeweilige Aufrufreihenfolge tragen.
  m_rw <- mk("alpha", c(p2, p1))
  expect_identical(m_rw$run_id, m_a$run_id)
  expect_identical(m_a$eingaben$name,  basename(c(p1, p2)))
  expect_identical(m_rw$eingaben$name, basename(c(p2, p1)))

  unlink(c(p1, p2))
})
