# ============================================================================
#  tests/test_finalisierung.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-13-finalisierung
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: das ungeschuetzte
#  source(R/finalisierung.R) am Kopf bricht ab, solange die Datei fehlt (sie
#  wird erst vom Builder dieser Phase angelegt) -> die GANZE Spezifikation ist
#  Immer-ROT bis zur Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche, keine Tier-1-Literale, kein MC, keine Fixture-Mappe.
#
#  Vertrag (handoff/phase-13-finalisierung.json; vorlagenfrei, schirm-los,
#  IDW PS 880-Anschluss / GoBD Rz. 58 ff.):
#   - lauf_status(run_dir): liest den Lock-Zustand EINES Laufs; einzelner String
#     'entwurf' oder 'final'. Quelle ist ALLEIN der write-once Marker final.json
#     im Run-Verzeichnis (existiert -> 'final', sonst 'entwurf'); NIE das
#     Protokoll (eine Wahrheit pro Fakt). run_dir muss existieren UND ein echter
#     Lauf sein (manifest.json liegt) -> sonst benannter Fehler MIT Pfad. Reines
#     Lesen, kein RNG, kein Sys.time().
#   - lauf_finalisieren(run_dir, protokoll_pfad, zeit, user): entwurf -> final in
#     EINEM kanonischen Effekt-Pfad. run_id via manifest_lesen; Status muss
#     'entwurf' sein (sonst BENANNTER Fehler, KEIN silent succeed / No-Op);
#     Marker final.json write-once schreiben (run_id, zeitstempel UTC-normiert
#     '%Y-%m-%dT%H:%M:%SZ', user); DANN Protokoll-Ereignis 'finalisierung'
#     ueber protokoll_anhaengen (Payload list(run_id, status='final')).
#     Reihenfolge Marker-vor-Protokoll ist Vertrag; der Guard feuert VOR
#     protokoll_anhaengen -> eine Zweit-Finalisierung haengt KEIN zweites
#     Ereignis an. Rueckgabe invisible list(run_id, status='final', marker_pfad,
#     protokoll_pfad). zeit/user INJIZIERT, kein RNG.
#   - lauf_vorgaenger(run_dir): vorgaenger_run_id aus Marker korrektur.json,
#     sonst NA_character_ (kein Korrekturlauf). Symmetrisch zu lauf_status
#     (korrektur.json wird von korrektur_verketten GESCHRIEBEN, hier GELESEN).
#     Reines Lesen, kein RNG.
#   - korrektur_verketten(run_dir_vorgaenger, run_dir_neu, protokoll_pfad, zeit,
#     user): verkettet einen NEUEN Lauf als Korrektur eines finalen Vorgaengers.
#     PRECONDITIONS in Vertragsreihenfolge (je BENANNTER Fehler, isoliert
#     pruefbar): (a) run_id_neu != run_id_vorgaenger ('neuer Lauf'); (b)
#     lauf_status(vorgaenger) == 'final' ('nicht final'); (c) lauf_status(neu)
#     == 'entwurf' ('bereits finalisiert'); (d) Marker korrektur.json write-once
#     ('bereits als Korrektur verzeichnet'). Effekt: Marker korrektur.json
#     (vorgaenger_run_id, run_id, zeitstempel UTC, user) + Protokoll-Ereignis
#     'korrektur' (Payload list(vorgaenger_run_id, run_id)). Rueckgabe invisible
#     list(run_id, vorgaenger_run_id, marker_pfad, protokoll_pfad). Kein RNG.
#
#  Orakel-Politik: die Kennzahlen sind String-/Identitaets-/Existenz-/
#  Fehlerklasse-Pruefungen (Toleranz 0). Die run_id-Referenz wird UNABHAENGIG
#  ueber manifest_lesen() rekonstruiert (nicht aus dem Rueckgabeobjekt der
#  getesteten Funktion). Die Verkettung der angehaengten Ereignisse wird ueber
#  den Primitive protokoll_pruefen aus phase-12-protokoll-pruefen abgesichert
#  (Delegation, keine Duplikation der Hash-Formel).
#
#  Bestandsschutz: reine Neudatei; beruehrt weder kernel/ noch R/manifest.R
#  noch R/protokoll.R noch eine der neun test_manifest*-Dateien. Ruft NUR deren
#  Funktionen auf (manifest_erstellen/manifest_schreiben/manifest_lesen,
#  protokoll_anhaengen/protokoll_pruefen). Keine neuen Pakete — digest und
#  jsonlite sind seit phase-12-runmanifest-kern im need-Vektor von tests/deps.R.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Kontext exakt wie im Anker gefordert: kernel/engine.R, kernel/distributions.R,
# R/scenario.R, R/manifest.R, R/protokoll.R und — ungeschuetzt, rot zuerst —
# R/finalisierung.R. Solange die Datei fehlt, bricht dieses source() ab und die
# ganze Spezifikation faellt hart durch (kein Immer-gruen).
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "manifest.R"))
source(file.path(.root, "R", "protokoll.R"))
source(file.path(.root, "R", "finalisierung.R"))  # existiert noch nicht -> rot zuerst

# Rot-zuerst-Riegel (Verteidigung, falls jemand eine leere Datei anlegt): ohne
# die vier Primitive faellt die GANZE Spezifikation hart durch, statt dass die
# expect_error-Faelle faelschlich am 'could not find function'-Fehler gruen
# werden. Bewusst Immer-ROT bis zur Implementierung.
for (.fn in c("lauf_status", "lauf_finalisieren", "lauf_vorgaenger",
              "korrektur_verketten")) {
  if (!exists(.fn, mode = "function")) {
    stop(.fn, " ist noch nicht definiert (rot zuerst)")
  }
}

library(testthat)

# ---------------------------------------------------------------------------
# Injizierte UTC-Zeiten (das UI liefert im Betrieb die reale Zeit; der Kern
# bleibt durch Injektion deterministisch — wie bei manifest/protokoll).
# ---------------------------------------------------------------------------
.z1 <- as.POSIXct("2026-07-11 08:00:00", tz = "UTC")
.z2 <- as.POSIXct("2026-07-11 09:00:00", tz = "UTC")
.z3 <- as.POSIXct("2026-07-11 10:00:00", tz = "UTC")
.zf <- as.POSIXct("2026-07-11 11:00:00", tz = "UTC")   # Finalisierung
.zk <- as.POSIXct("2026-07-11 12:00:00", tz = "UTC")   # Korrektur

# Umgebungsstempel fuer die Minimal-Manifeste (wie im Anker: v1, ohne
# umgebung_erfassen(), damit der Kern deterministisch bleibt).
.u <- list(r_version = as.character(getRversion()), plattform = "anker",
           git_sha = NA_character_, git_dirty = NA,
           renv_lock_sha256 = NA_character_)

# Frisches, garantiert leeres Basisverzeichnis fuer die Laeufe eines Testfalls.
.frisch_base <- function(tag) {
  b <- file.path(tempdir(), paste0("finalis-", tag))
  unlink(b, recursive = TRUE)
  dir.create(b, recursive = TRUE)
  b
}

# Frischer, garantiert nicht existierender Protokollpfad je Testfall.
.frisch_prot <- function(tag) {
  p <- file.path(tempdir(), sprintf("finalis-protokoll-%s.jsonl", tag))
  unlink(p)
  p
}

# Ein Minimal-Lauf: ein v1-Manifest ueber manifest_erstellen + manifest_schreiben
# in einem eigenen run_dir (benannt nach der run_id). Unterschiedlicher tag
# (andere Eingabe-Bytes) und/oder andere zeit -> stabil unterschiedliche run_id.
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

# Marker (final.json / korrektur.json) als benannte Liste skalarer Felder.
.read_marker <- function(pfad) jsonlite::read_json(pfad, simplifyVector = TRUE)

# Alle Protokoll-Ereignisse als Liste geparster Zeilen (wie im Anker).
.prot_events <- function(pfad) lapply(readLines(pfad), function(l) jsonlite::fromJSON(l))

# run_id eines Laufs UNABHAENGIG rekonstruiert (nicht aus dem Rueckgabeobjekt
# der getesteten Funktion).
.rid <- function(run_dir) manifest_lesen(file.path(run_dir, "manifest.json"))$run_id


# ===========================================================================
#  (1) STATUS-QUERY
# ===========================================================================

test_that("lauf_status: frischer Lauf ist 'entwurf' und liest nur (kein Schreiben, kein RNG)", {
  b  <- .frisch_base("status")
  rd <- .mk(b, "s1", .z1, 1L)

  set.seed(3); rng_vor <- .Random.seed
  dateien_vor <- sort(list.files(rd))

  st <- lauf_status(rd)
  expect_type(st, "character")
  expect_length(st, 1L)
  expect_identical(st, "entwurf")

  expect_identical(sort(list.files(rd)), dateien_vor)  # kein Marker geschrieben
  expect_identical(.Random.seed, rng_vor)              # kein RNG verbraucht
})

test_that("lauf_status: run_dir existiert nicht -> Fehler MIT Pfad im Text", {
  fehlt <- file.path(tempdir(), "finalis-status-gibtsnicht-xyz")
  unlink(fehlt, recursive = TRUE)
  msg <- tryCatch(lauf_status(fehlt), error = function(e) conditionMessage(e))
  expect_type(msg, "character")
  expect_true(grepl(fehlt, msg, fixed = TRUE))
})

test_that("lauf_status: run_dir ohne manifest.json -> benannter Fehler ('kein Lauf'/'manifest.json fehlt')", {
  b    <- .frisch_base("status-kein-manifest")
  leer <- file.path(b, "leerlauf")
  dir.create(leer)
  expect_error(lauf_status(leer), "manifest|Lauf")
})


# ===========================================================================
#  (2) FINALISIEREN
# ===========================================================================

test_that("lauf_finalisieren: entwurf -> final (Rueckgabe, Marker, Status, Protokoll-Ereignis)", {
  b   <- .frisch_base("final")
  pp  <- .frisch_prot("final")
  rd  <- .mk(b, "f1", .z1, 1L)
  rid <- .rid(rd)

  wv <- withVisible(lauf_finalisieren(rd, pp, zeit = .zf, user = "anker"))
  expect_false(wv$visible)                              # invisible
  res <- wv$value

  # Rueckgabe: run_id UNABHAENGIG gegen manifest_lesen, Status, Marker, Pfad.
  expect_identical(res$run_id, rid)
  expect_identical(res$status, "final")
  expect_true(file.exists(res$marker_pfad))
  expect_identical(basename(res$marker_pfad), "final.json")
  expect_identical(res$protokoll_pfad, pp)

  # Marker final.json: run_id, UTC-normierter zeitstempel, injizierter user.
  mk <- .read_marker(res$marker_pfad)
  expect_identical(mk$run_id, rid)
  expect_identical(mk$zeitstempel, format(.zf, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  expect_identical(mk$user, "anker")

  # Status danach 'final' (aus demselben Marker gelesen).
  expect_identical(lauf_status(rd), "final")

  # Protokoll traegt GENAU EIN finalisierung-Ereignis mit run_id/status='final'.
  ev   <- .prot_events(pp)
  typs <- vapply(ev, function(x) x$typ, character(1))
  expect_identical(sum(typs == "finalisierung"), 1L)
  fin  <- ev[[match("finalisierung", typs)]]
  expect_identical(fin$daten$run_id, rid)
  expect_identical(fin$daten$status, "final")
})

test_that("lauf_finalisieren: Zweit-Finalisierung scheitert benannt OHNE zweites Ereignis (Guard vor Append)", {
  b  <- .frisch_base("final2")
  pp <- .frisch_prot("final2")
  rd <- .mk(b, "f2", .z1, 1L)

  lauf_finalisieren(rd, pp, zeit = .zf, user = "anker")

  re <- tryCatch(lauf_finalisieren(rd, pp, zeit = .zf, user = "anker"),
                 error = function(e) e)
  expect_true(inherits(re, "error"))                   # BENANNTER Fehler, kein No-Op

  # Der Guard feuert VOR protokoll_anhaengen: kein zweites finalisierung-Ereignis.
  ev   <- .prot_events(pp)
  typs <- vapply(ev, function(x) x$typ, character(1))
  expect_identical(sum(typs == "finalisierung"), 1L)
})


# ===========================================================================
#  (3) KORREKTUR
# ===========================================================================

test_that("korrektur_verketten: finaler Vorgaenger + entwurf-Neu (DISTINCT run_id) -> verkettet", {
  b     <- .frisch_base("kor")
  pp    <- .frisch_prot("kor")
  r_vor <- .mk(b, "v", .z1, 11L)
  r_neu <- .mk(b, "n", .z2, 12L)
  rid_vor <- .rid(r_vor)
  rid_neu <- .rid(r_neu)
  expect_false(identical(rid_vor, rid_neu))            # DISTINCT run_id

  lauf_finalisieren(r_vor, pp, zeit = .zf, user = "anker")

  wv <- withVisible(korrektur_verketten(r_vor, r_neu, pp, zeit = .zk, user = "anker"))
  expect_false(wv$visible)                             # invisible
  kv <- wv$value

  expect_identical(kv$run_id, rid_neu)
  expect_identical(kv$vorgaenger_run_id, rid_vor)
  expect_true(file.exists(kv$marker_pfad))
  expect_identical(basename(kv$marker_pfad), "korrektur.json")

  # Marker korrektur.json liegt im NEU-Verzeichnis und traegt beide run_ids.
  km <- .read_marker(file.path(r_neu, "korrektur.json"))
  expect_identical(km$vorgaenger_run_id, rid_vor)
  expect_identical(km$run_id, rid_neu)

  # Roundtrip: lauf_vorgaenger liest die Herkunft; der Vorgaenger selbst hat keine.
  expect_identical(lauf_vorgaenger(r_neu), rid_vor)
  expect_identical(lauf_vorgaenger(r_vor), NA_character_)

  # Protokoll traegt ein korrektur-Ereignis mit vorgaenger_run_id/run_id.
  ev   <- .prot_events(pp)
  typs <- vapply(ev, function(x) x$typ, character(1))
  kor  <- ev[[match("korrektur", typs)]]
  expect_identical(kor$daten$vorgaenger_run_id, rid_vor)
  expect_identical(kor$daten$run_id, rid_neu)
})

test_that("korrektur_verketten: Zweit-Verkettung auf demselben neu -> write-once Fehler", {
  b     <- .frisch_base("kor2")
  pp    <- .frisch_prot("kor2")
  r_vor <- .mk(b, "v", .z1, 11L)
  r_neu <- .mk(b, "n", .z2, 12L)

  lauf_finalisieren(r_vor, pp, zeit = .zf, user = "anker")
  korrektur_verketten(r_vor, r_neu, pp, zeit = .zk, user = "anker")

  expect_error(korrektur_verketten(r_vor, r_neu, pp, zeit = .zk, user = "anker"))
})


# ===========================================================================
#  (4) KORREKTUR-INVARIANTEN — je ein BENANNTER Fehler, in Vertragsreihenfolge,
#      isoliert pruefbar.
# ===========================================================================

test_that("korrektur_verketten: (a) neuer Lauf, (b) Vorgaenger final, (c) Neu entwurf", {
  b      <- .frisch_base("inv")
  pp     <- .frisch_prot("inv")
  r_vor  <- .mk(b, "v",  .z1, 11L)   # wird final (dient als Vorgaenger und als 'neu' in (c))
  r_vor2 <- .mk(b, "v2", .z2, 21L)   # zweiter finaler Lauf (Vorgaenger in (c))
  r_neu  <- .mk(b, "n",  .z3, 12L)   # bleibt entwurf
  r_drit <- .mk(b, "d",  .zk, 13L)   # bleibt entwurf

  lauf_finalisieren(r_vor,  pp, zeit = .zf, user = "anker")
  lauf_finalisieren(r_vor2, pp, zeit = .zf, user = "anker")

  # (a) neu == vorgaenger (gleicher run_dir) -> 'neuer Lauf'
  expect_error(korrektur_verketten(r_vor, r_vor, pp, zeit = .zk, user = "anker"),
               "neuer Lauf")
  # (b) Vorgaenger nicht final (entwurf) -> 'nicht final'
  expect_error(korrektur_verketten(r_neu, r_drit, pp, zeit = .zk, user = "anker"),
               "nicht final")
  # (c) Neu bereits final (distinct, Vorgaenger final) -> 'bereits finalisiert'
  expect_error(korrektur_verketten(r_vor2, r_vor, pp, zeit = .zk, user = "anker"),
               "bereits finalisiert")
})


# ===========================================================================
#  (5) PROTOKOLL-INTEGRITAET — die angehaengten Ereignisse bilden eine gueltige
#      Kette (Delegation an protokoll_pruefen aus phase-12-protokoll-pruefen).
# ===========================================================================

test_that("protokoll_pruefen besteht nach finalisieren + korrektur", {
  b     <- .frisch_base("chain")
  pp    <- .frisch_prot("chain")
  r_vor <- .mk(b, "v", .z1, 11L)
  r_neu <- .mk(b, "n", .z2, 12L)

  lauf_finalisieren(r_vor, pp, zeit = .zf, user = "anker")
  korrektur_verketten(r_vor, r_neu, pp, zeit = .zk, user = "anker")

  expect_true(isTRUE(protokoll_pruefen(pp)))
})


# ===========================================================================
#  (6) RNG-WAECHTER — der GoBD-Pfad (finalisieren + verketten) verbraucht keinen
#      RNG.
# ===========================================================================

test_that("GoBD-Pfad ist RNG-frei: .Random.seed vor/nach unveraendert", {
  b     <- .frisch_base("rng")
  pp    <- .frisch_prot("rng")
  r_vor <- .mk(b, "v", .z1, 11L)
  r_neu <- .mk(b, "n", .z2, 12L)

  set.seed(7); s0 <- .Random.seed
  lauf_finalisieren(r_vor, pp, zeit = .zf, user = "anker")
  korrektur_verketten(r_vor, r_neu, pp, zeit = .zk, user = "anker")

  expect_identical(.Random.seed, s0)
})


# ===========================================================================
#  (7) FEHLERFAELLE / SIGNATUR — jeder Fall auf einem FRISCHEN run_dir, damit ein
#      moeglicher Teil-Effekt keinen Folgefall verfaelscht.
# ===========================================================================

test_that("lauf_finalisieren: leerer protokoll_pfad / leerer user / String-zeit -> Fehler", {
  b <- .frisch_base("err-fin")

  # leerer protokoll_pfad
  expect_error(lauf_finalisieren(.mk(b, "e1", .z1, 1L), "", zeit = .zf, user = "anker"))
  # leerer user
  expect_error(lauf_finalisieren(.mk(b, "e2", .z2, 2L), .frisch_prot("err-u"),
                                 zeit = .zf, user = ""))
  # zeit als String (kein POSIXct)
  expect_error(lauf_finalisieren(.mk(b, "e3", .z3, 3L), .frisch_prot("err-z"),
                                 zeit = "2026-07-11T11:00:00Z", user = "anker"))
})

test_that("run_dir existiert nicht -> Fehler MIT Pfad (finalisieren und verketten)", {
  b     <- .frisch_base("err-fehlt")
  fehlt <- file.path(tempdir(), "finalis-fehlt-einzigartig-xyz")
  unlink(fehlt, recursive = TRUE)

  m1 <- tryCatch(lauf_finalisieren(fehlt, .frisch_prot("err-f1"), zeit = .zf, user = "anker"),
                 error = function(e) conditionMessage(e))
  expect_true(grepl(fehlt, m1, fixed = TRUE))

  m2 <- tryCatch(korrektur_verketten(fehlt, .mk(b, "e4", .z1, 4L),
                                     .frisch_prot("err-k"), zeit = .zk, user = "anker"),
                 error = function(e) conditionMessage(e))
  expect_true(grepl(fehlt, m2, fixed = TRUE))
})
