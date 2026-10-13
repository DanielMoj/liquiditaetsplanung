# ============================================================================
#  tests/test_finalisierung_transaktion.R  —  Zusatz-Spezifikation fuer die
#  TRANSAKTIONALE + ATOMARE Marker-Semantik von phase-13-finalisierung.
#
#  Neudatei (Test-Bestand-Guard): beruehrt keine bestehende tests/-Datei, ruft
#  nur oeffentliche Funktionen. Deckt die Fehlerklassen ab, die der nackte
#  check-then-write-Guard offen liess und die .finalis_marker_transaktion
#  schliesst:
#
#   (A) TRANSAKTION: scheitert protokoll_anhaengen NACH dem Marker-Schreiben,
#       darf KEIN Marker ohne Audit-Ereignis zurueckbleiben -- der Marker wird
#       zurueckgerollt, der Lauf bleibt 'entwurf', und ein Retry gelingt (der
#       write-once-Guard blockiert ihn NICHT). Gilt fuer final.json
#       (lauf_finalisieren) und korrektur.json (korrektur_verketten).
#   (B) ATOMARE SPERRE: eine gehaltene Sperre (paralleler Worker) laesst KEINEN
#       zweiten Marker samt Ereignis durch -- der Zweit-Aufruf scheitert benannt
#       und schreibt nichts.
#
#  Deterministischer Append-Fehler OHNE Mock: ein bereits existierendes, aber
#  LEERES Protokoll (0 Zeilen) laesst protokoll_anhaengen mit 'Protokoll leer'
#  abbrechen (R/protokoll.R Z. 174). Das prueft den Rollback echt, nicht per
#  Stub.
# ============================================================================

.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "manifest.R"))
source(file.path(.root, "R", "protokoll.R"))
source(file.path(.root, "R", "finalisierung.R"))

library(testthat)

.z1 <- as.POSIXct("2026-07-11 08:00:00", tz = "UTC")
.z2 <- as.POSIXct("2026-07-11 09:00:00", tz = "UTC")
.zf <- as.POSIXct("2026-07-11 11:00:00", tz = "UTC")
.zk <- as.POSIXct("2026-07-11 12:00:00", tz = "UTC")

.u <- list(r_version = as.character(getRversion()), plattform = "anker",
           git_sha = NA_character_, git_dirty = NA,
           renv_lock_sha256 = NA_character_)

.frisch_base <- function(tag) {
  b <- file.path(tempdir(), paste0("finalis-tx-", tag))
  unlink(b, recursive = TRUE)
  dir.create(b, recursive = TRUE)
  b
}

.mk <- function(base, tag, zeit, seed) {
  d <- tempfile()
  writeBin(charToRaw(tag), d)
  m <- manifest_erstellen(eingaben = d, parameter = list(H = 4L),
                          seeds = c(sim = seed), outputs = character(0),
                          zeit = zeit, user = "anker", umgebung = .u)
  rdir <- file.path(base, m$run_id)
  dir.create(rdir, recursive = TRUE)
  manifest_schreiben(m, file.path(rdir, "manifest.json"))
  rdir
}

# Ein garantiert LEERES (0-Zeilen) Protokoll: existiert, aber protokoll_anhaengen
# bricht darauf mit 'Protokoll leer' ab -> deterministischer Append-Fehler.
.leeres_prot <- function(tag) {
  p <- file.path(tempdir(), sprintf("finalis-tx-leer-%s.jsonl", tag))
  unlink(p)
  file.create(p)
  stopifnot(file.exists(p), length(readLines(p, warn = FALSE)) == 0L)
  p
}

.frisch_prot <- function(tag) {
  p <- file.path(tempdir(), sprintf("finalis-tx-ok-%s.jsonl", tag))
  unlink(p)
  p
}


# ===========================================================================
#  (A) TRANSAKTION — Marker-Rollback bei Append-Fehler
# ===========================================================================

test_that("lauf_finalisieren: Append-Fehler rollt final.json zurueck; Lauf bleibt entwurf; Retry gelingt", {
  b        <- .frisch_base("fin-rollback")
  rd       <- .mk(b, "f1", .z1, 1L)
  pp_leer  <- .leeres_prot("fin")

  # Append scheitert (leeres Protokoll) -> benannter Fehler ...
  expect_error(lauf_finalisieren(rd, pp_leer, zeit = .zf, user = "anker"))

  # ... und der Marker ist zurueckgerollt: KEIN final.json, Status weiter entwurf,
  # keine liegengebliebene Sperre.
  expect_false(file.exists(file.path(rd, "final.json")))
  expect_identical(lauf_status(rd), "entwurf")
  expect_false(dir.exists(file.path(rd, "final.json.lock")))

  # Der write-once-Guard blockiert den Retry NICHT (kein Marker ohne Ereignis):
  # mit gueltigem Protokoll gelingt die Finalisierung samt genau einem Ereignis.
  pp_ok <- .frisch_prot("fin")
  res <- lauf_finalisieren(rd, pp_ok, zeit = .zf, user = "anker")
  expect_identical(res$status, "final")
  expect_true(file.exists(file.path(rd, "final.json")))
  ev   <- lapply(readLines(pp_ok), function(l) jsonlite::fromJSON(l))
  typs <- vapply(ev, function(x) x$typ, character(1))
  expect_identical(sum(typs == "finalisierung"), 1L)
})

test_that("korrektur_verketten: Append-Fehler rollt korrektur.json zurueck; kein Vorgaenger verzeichnet; Retry gelingt", {
  b        <- .frisch_base("kor-rollback")
  pp_ok    <- .frisch_prot("kor")
  r_vor    <- .mk(b, "v", .z1, 11L)
  r_neu    <- .mk(b, "n", .z2, 12L)

  lauf_finalisieren(r_vor, pp_ok, zeit = .zf, user = "anker")

  # Verkettung mit leerem Protokoll -> Append scheitert.
  pp_leer <- .leeres_prot("kor")
  expect_error(korrektur_verketten(r_vor, r_neu, pp_leer, zeit = .zk, user = "anker"))

  # korrektur.json zurueckgerollt: keine Herkunft verzeichnet, keine Sperre.
  expect_false(file.exists(file.path(r_neu, "korrektur.json")))
  expect_identical(lauf_vorgaenger(r_neu), NA_character_)
  expect_false(dir.exists(file.path(r_neu, "korrektur.json.lock")))

  # Retry mit gueltigem Protokoll gelingt (write-once-Guard blockiert nicht).
  kv <- korrektur_verketten(r_vor, r_neu, pp_ok, zeit = .zk, user = "anker")
  expect_identical(lauf_vorgaenger(r_neu), kv$vorgaenger_run_id)
  expect_true(file.exists(file.path(r_neu, "korrektur.json")))
})


# ===========================================================================
#  (B) ATOMARE SPERRE — gehaltene Sperre laesst keinen zweiten Marker durch
# ===========================================================================

test_that("lauf_finalisieren: gehaltene final.json.lock -> benannter Fehler, kein Marker, kein Ereignis", {
  b  <- .frisch_base("fin-lock")
  rd <- .mk(b, "f2", .z1, 1L)
  pp <- .frisch_prot("fin-lock")

  # Sperre simuliert einen parallel finalisierenden Worker.
  lock <- file.path(rd, "final.json.lock")
  dir.create(lock)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  expect_error(lauf_finalisieren(rd, pp, zeit = .zf, user = "anker"), "Sperre")

  # Kein Marker geschrieben, kein Protokoll angelegt -> nichts durchgerutscht.
  expect_false(file.exists(file.path(rd, "final.json")))
  expect_identical(lauf_status(rd), "entwurf")
  expect_false(file.exists(pp))
})

test_that("korrektur_verketten: gehaltene korrektur.json.lock -> benannter Fehler, kein Marker", {
  b     <- .frisch_base("kor-lock")
  pp    <- .frisch_prot("kor-lock")
  r_vor <- .mk(b, "v", .z1, 11L)
  r_neu <- .mk(b, "n", .z2, 12L)

  lauf_finalisieren(r_vor, pp, zeit = .zf, user = "anker")

  lock <- file.path(r_neu, "korrektur.json.lock")
  dir.create(lock)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  expect_error(korrektur_verketten(r_vor, r_neu, pp, zeit = .zk, user = "anker"),
               "Sperre")
  expect_false(file.exists(file.path(r_neu, "korrektur.json")))
  expect_identical(lauf_vorgaenger(r_neu), NA_character_)
})
