# ============================================================================
#  tests/test_manifest_umgebung.R  —  MUTGATE-NACHSCHUB (Runde 3) fuer
#  phase-12-runmanifest-kern
#
#  Zweck: WERTSEMANTIK des Umgebungs-Sammlers umgebung_erfassen(). Die
#  Spezifikation in tests/test_manifest.R prueft dort bewusst nur die FORM
#  (logical, exakte Felder, NA-oder-Hex) — dadurch ueberlebt die
#  Inversions-Klasse: Mutant M5 kehrt die git_dirty-Bedingung um
#  (st == 0L statt st != 0L) und liefert formgerecht das FALSCHE Flag.
#  GoBD-Relevanz (Rz. 152): ein verkehrtes dirty-Flag zertifiziert
#  faelschlich saubere Umgebungen — der Lauf waere scheinbar aus einem
#  definierten Stand rekonstruierbar, obwohl er es nicht ist.
#
#  Runde 3 ergaenzt den FEHLERPFAD: auf dem git-ERFOLGSPFAD ist M5
#  aequivalent, weil system2(stdout = TRUE) das status-Attribut nur bei
#  Exit != 0 anhaengt (st ist auf Erfolg NULL, beide Varianten fallen in
#  den porcelain-Zweig). Beobachtbar wird M5 erst, wenn git SCHEITERT:
#  Original liefert NA (unbekannt), der Mutant FALSE (faelschlich sauber).
#
#  Vorgehen: die Werte werden an einem TEMPORAEREN git-Repo gepinnt,
#  unabhaengig vom Zustand des Arbeits-Repos (alle git-Aufrufe nur im
#  Temp-Repo, keinerlei Annahme ueber das umgebende Repo). Vier Zustaende:
#   (0) Verzeichnis OHNE git-Repo (Fehlerpfad, git status Exit 128)
#       -> git_dirty NA, git_sha NA;
#   (1) frisches Repo + untracked Datei -> git_dirty TRUE, git_sha NA
#       (kein Commit), renv_lock_sha256 NA (kein renv.lock);
#   (2) alles committet -> git_dirty exakt FALSE, git_sha 40-Hex;
#   (3) renv.lock mit bekannten Bytes 'abc' -> renv_lock_sha256 gleich dem
#       NIST-FIPS-180-4-Literal, git_dirty wieder TRUE (untracked).
#
#  Windows-Hinweis: git ist Suite-Voraussetzung (Repo-Infrastruktur, siehe
#  Handoff) — daher kein zusaetzlicher Skip. Kein skip(), kein succeed(),
#  keine Anker-/Tier-1-Literale.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))

test_that("manifest: umgebung_erfassen — git_dirty/git_sha/renv_lock_sha256 tragen die richtigen WERTE", {
  alt <- getwd()
  on.exit(setwd(alt), add = TRUE)
  td <- tempfile("umgtest")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  # ZUSTAND 0 (Fehlerpfad): Verzeichnis OHNE git-Repo — das Temp-Verzeichnis
  # liegt ausserhalb jedes Repos, git status scheitert mit Exit 128. Ein
  # unbekannter Zustand darf NIE als sauber/FALSE erscheinen (GoBD-
  # Wahrhaftigkeit — exakt die M5-Klasse: Original NA, Mutant FALSE).
  td0 <- tempfile("umgtest0")
  dir.create(td0)
  on.exit(unlink(td0, recursive = TRUE, force = TRUE), add = TRUE)
  setwd(td0)
  u0 <- umgebung_erfassen()
  expect_true(is.na(u0$git_dirty))
  expect_true(is.na(u0$git_sha))

  setwd(td)

  system2("git", c("init", "-q"), stdout = FALSE, stderr = FALSE)
  system2("git", c("config", "user.email", "test@test"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("config", "user.name", "test"),
          stdout = FALSE, stderr = FALSE)

  # ZUSTAND 1: frisches Repo mit untracked Datei -> porcelain nicht leer,
  # also dirty; kein Commit -> rev-parse scheitert -> git_sha NA; kein
  # renv.lock -> renv_lock_sha256 NA.
  writeLines("x", "a.txt")
  u1 <- umgebung_erfassen()
  expect_true(isTRUE(u1$git_dirty))
  expect_true(is.na(u1$git_sha))
  expect_true(is.na(u1$renv_lock_sha256))

  # ZUSTAND 2: alles committet, Arbeitsbaum sauber -> git_dirty muss exakt
  # FALSE sein (nicht NA — genau hier kippt die Inversions-Klasse M5),
  # git_sha ein 40-stelliger Kleinbuchstaben-Hexstring.
  system2("git", c("add", "a.txt"), stdout = FALSE, stderr = FALSE)
  system2("git", c("commit", "-q", "-m", "init"), stdout = FALSE, stderr = FALSE)
  u2 <- umgebung_erfassen()
  expect_false(isTRUE(u2$git_dirty))
  expect_true(identical(u2$git_dirty, FALSE))
  expect_true(grepl("^[0-9a-f]{40}$", u2$git_sha))

  # ZUSTAND 3: renv.lock mit den Bytes 'abc' -> Hash gleich dem woertlichen
  # NIST-FIPS-180-4-Literal (SHA-256-Testvektor 'abc', uebernommen aus dem
  # Handoff/tests/test_manifest.R — KEIN Selbstbezug); die untracked
  # renv.lock macht das Repo zugleich wieder dirty.
  writeBin(charToRaw("abc"), "renv.lock")
  u3 <- umgebung_erfassen()
  expect_identical(u3$renv_lock_sha256,
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  expect_true(isTRUE(u3$git_dirty))
})
