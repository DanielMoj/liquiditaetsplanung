# ============================================================================
#  tests/test_protokoll_anhaengen.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-12-protokoll-anhaengen
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: das ungeschuetzte
#  source(R/protokoll.R) am Kopf GELINGT (die Datei existiert bereits aus
#  phase-12-protokoll-hash und bringt .protokoll_hash mit), aber
#  protokoll_anhaengen ist dort NOCH NICHT definiert -> jeder Aufruf bricht ab
#  -> rot. Der Builder erfuellt sie erst mit der Implementierung des Writers.
#  Kein skip(), kein succeed(), keine Existenz-Weiche.
#
#  Vertrag (handoff/phase-12-protokoll-anhaengen.json; vorlagenfrei,
#  schirm-los, GoBD Rz. 58 ff./107):
#   - protokoll_anhaengen(protokoll_pfad, typ, daten, zeit, user) haengt GENAU
#     EINE Ereigniszeile (JSONL: ein JSON-Objekt je Zeile) append-only an eine
#     Protokoll-Datei und liefert invisible das angehaengte Ereignis = list mit
#     den SIEBEN Feldern in der Reihenfolge
#     seq/typ/zeitstempel/user/daten/vorheriger_hash/hash.
#   - Neue Kette (Datei fehlt): seq = 1L, vorheriger_hash = strrep('0', 64)
#     (Genesis-Sentinel). Bestehende Kette: seq = letzter_seq + 1L,
#     vorheriger_hash = hash der letzten Zeile. Existierende LEERE Datei
#     (0 Zeilen) -> stop('Protokoll leer').
#   - typ: genau einer von import/lauf/kontrolle/export/finalisierung/korrektur;
#     sonst stop() mit Wert UND Sechser-Menge.
#   - daten: benannte Liste (darf leer sein); befuellt -> Namen eindeutig, jeder
#     Wert skalares Atom (logical/numeric/character, Laenge 1, endlich, nicht
#     NA); sonst stop() mit Feldname daten$<name>.
#   - zeitstempel = format(zeit, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'); zeit einzelner
#     POSIXct (sonst stop()); user nicht leerer String (sonst stop());
#     protokoll_pfad nicht leerer String (sonst stop()).
#   - hash <- .protokoll_hash(seq, typ, zeitstempel, user, daten,
#     vorheriger_hash) (Delegation an den oracle-gepinnten Primitive der
#     Vorgaengerphase; die kanon-Formel steht NICHT nochmal hier). Schreiben:
#     jsonlite::toJSON(auto_unbox = TRUE, pretty = FALSE) + Zeilenende,
#     ausschliesslich append (cat(..., append = TRUE)); bestehende Bytes werden
#     nie beruehrt. Kein RNG, kein Sys.time() im Kern.
#
#  Orakel-Politik: die Hash-Korrektheit SELBST ist in phase-12-protokoll-hash
#  gegen NIST FIPS 180-4 und ein unabhaengiges kanon-Orakel gepinnt; HIER
#  werden STRUKTUR und VERKETTUNG geprueft — die Verkettung per direktem
#  String-Vergleich (vorheriger_hash[i] == hash[i-1]), UNABHAENGIG von der
#  Hash-Formel. Keine Anker-/Tier-1-Literale, keine Fixture-Mappe, kein MC,
#  kein Engine-Aufruf — nur Tempdateien mit deterministischen Eingaben ueber
#  injizierte POSIXct-zeit, kleine Laufzeit.
#
#  Bestandsschutz: reine Neudatei; beruehrt weder kernel/ noch R/manifest.R
#  noch R/manifest_anbindung.R noch eine der neun test_manifest*-Dateien.
#  Keine neuen Pakete — digest und jsonlite sind seit
#  phase-12-runmanifest-kern im need-Vektor von tests/deps.R.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "protokoll.R"))  # .protokoll_hash da; protokoll_anhaengen fehlt -> rot

# Rot-zuerst-Riegel: source() GELINGT (R/protokoll.R bringt .protokoll_hash aus
# phase-12-protokoll-hash mit), aber protokoll_anhaengen ist NOCH NICHT
# definiert. Ohne diesen Riegel wuerden die expect_error-Faelle unten schon vom
# 'could not find function'-Fehler faelschlich gruen — deshalb faellt die GANZE
# Spezifikation hart durch, bis der Writer existiert. Kein skip, kein succeed:
# das ist bewusst Immer-ROT bis zur Implementierung, nicht Immer-gruen.
if (!exists("protokoll_anhaengen", mode = "function")) {
  stop("protokoll_anhaengen ist noch nicht definiert (rot zuerst)")
}

# ---------------------------------------------------------------------------
# Feste Ereignis-Koordinaten (handgesetzte Literale, keine Anker/Tier-1).
# ---------------------------------------------------------------------------
.genesis <- strrep("0", 64)
.felder  <- c("seq", "typ", "zeitstempel", "user", "daten",
              "vorheriger_hash", "hash")

# Injizierte UTC-Zeiten (das UI liefert im Betrieb die reale Zeit; der Kern
# bleibt durch Injektion deterministisch — wie beim Manifest).
.z1 <- as.POSIXct("2026-07-09 08:00:00", tz = "UTC")
.z2 <- as.POSIXct("2026-07-09 09:00:00", tz = "UTC")
.z3 <- as.POSIXct("2026-07-09 10:00:00", tz = "UTC")
.z4 <- as.POSIXct("2026-07-09 11:00:00", tz = "UTC")

# Frischer, garantiert nicht existierender Protokollpfad je Testfall.
.frischer_pfad <- function(tag) {
  p <- file.path(tempdir(), sprintf("protokoll-anhaengen-%s.jsonl", tag))
  unlink(p)
  p
}

# Eine JSONL-Zeile als benannte Liste (jsonlite liefert Objekt-Zeilen als list).
.parse_zeile <- function(l) jsonlite::fromJSON(l)


# ===========================================================================
#  (1) GENESIS-Ereignis: frische Datei -> seq 1, Genesis-Sentinel, eine Zeile
#      mit den sieben Feldern in Reihenfolge; Hash deckt sich mit dem
#      delegierten .protokoll_hash der Vorgaengerphase.
# ===========================================================================

test_that("protokoll_anhaengen: Genesis-Ereignis auf frischer Datei", {
  p <- .frischer_pfad("genesis")
  daten <- list(name = "M.xlsx", menge = -0.1)

  # Rueckgabe ist invisible.
  wv <- withVisible(protokoll_anhaengen(p, "import", daten, zeit = .z1, user = "anker"))
  expect_false(wv$visible)
  e1 <- wv$value

  # Ereignis-Struktur: sieben Felder in exakter Reihenfolge.
  expect_type(e1, "list")
  expect_identical(names(e1), .felder)

  expect_identical(e1$seq, 1L)
  expect_identical(e1$typ, "import")
  expect_identical(e1$vorheriger_hash, .genesis)
  expect_identical(e1$zeitstempel, "2026-07-09T08:00:00Z")
  expect_identical(e1$user, "anker")
  expect_true(grepl("^[0-9a-f]{64}$", e1$hash))

  # Die Datei traegt GENAU EINE Zeile.
  lines <- readLines(p)
  expect_length(lines, 1L)

  # JSON-geparst: sieben Felder in Reihenfolge, daten als benannte Liste.
  ev <- .parse_zeile(lines[[1]])
  expect_identical(names(ev), .felder)
  expect_equal(ev$seq, 1)
  expect_identical(ev$typ, "import")
  expect_identical(ev$zeitstempel, "2026-07-09T08:00:00Z")
  expect_identical(ev$user, "anker")
  expect_identical(ev$vorheriger_hash, .genesis)
  expect_identical(ev$hash, e1$hash)
  expect_equal(ev$daten$name, "M.xlsx")
  expect_equal(ev$daten$menge, -0.1)

  # Delegation an den oracle-gepinnten Primitive: der geschriebene Hash ist
  # GENAU .protokoll_hash ueber die injizierten Original-daten. (Die Formel
  # selbst ist in phase-12-protokoll-hash gepinnt, nicht hier.)
  expect_identical(
    e1$hash,
    .protokoll_hash(e1$seq, e1$typ, e1$zeitstempel, e1$user, daten,
                    e1$vorheriger_hash))

  unlink(p)
})


# ===========================================================================
#  (2) KETTENFORTSETZUNG an drei Ereignissen: seq 1/2/3 und Verkettung per
#      direktem String-Vergleich (unabhaengig von der Hash-Formel).
# ===========================================================================

test_that("protokoll_anhaengen: seq fortlaufend und Hash-Verkettung (string-verglichen)", {
  p <- .frischer_pfad("kette")

  e1 <- protokoll_anhaengen(p, "import",
                            list(name = "M.xlsx", menge = -0.1),
                            zeit = .z1, user = "anker")
  e2 <- protokoll_anhaengen(p, "lauf",
                            list(run_id = "run-20260709T080000Z-abcd1234"),
                            zeit = .z2, user = "anker")
  e3 <- protokoll_anhaengen(p, "kontrolle",
                            list(kontrolle = "anker_plan_tiefpunkt", bestanden = TRUE),
                            zeit = .z3, user = "anker")

  # seq der Rueckgaben: typsicher als Integer.
  expect_identical(e1$seq, 1L)
  expect_identical(e2$seq, 2L)
  expect_identical(e3$seq, 3L)

  # Datei: drei Zeilen, jede einzeln geparst mit der Sieben-Feld-Reihenfolge.
  lines <- readLines(p)
  expect_length(lines, 3L)
  ev <- lapply(lines, .parse_zeile)
  for (k in 1:3) expect_identical(names(ev[[k]]), .felder)

  # seq als == verglichen (jsonlite liefert JSON-Zahlen als numeric).
  expect_equal(ev[[1]]$seq, 1)
  expect_equal(ev[[2]]$seq, 2)
  expect_equal(ev[[3]]$seq, 3)

  # VERKETTUNG handgerechnet, UNABHAENGIG von der Hash-Formel: vorheriger_hash
  # jeder Zeile ist identisch zum hash der Vorgaengerzeile; die erste zeigt auf
  # den Genesis-Sentinel.
  expect_identical(ev[[1]]$vorheriger_hash, .genesis)
  expect_identical(ev[[2]]$vorheriger_hash, ev[[1]]$hash)
  expect_identical(ev[[3]]$vorheriger_hash, ev[[2]]$hash)

  # Gegenprobe an den Rueckgabeobjekten (gleiche Kette).
  expect_identical(e2$vorheriger_hash, e1$hash)
  expect_identical(e3$vorheriger_hash, e2$hash)

  unlink(p)
})


# ===========================================================================
#  (3) Schema-Abdeckung: alle SECHS Ereignistypen werden mit realistischen
#      daten-Payloads (character/logical/numeric quer ueber den Lebenszyklus)
#      fehlerfrei angenommen.
# ===========================================================================

test_that("protokoll_anhaengen: alle sechs Ereignistypen annehmbar (voller Lebenszyklus)", {
  p <- .frischer_pfad("schema")
  run  <- "run-20260709T080000Z-abcd1234"
  run2 <- "run-20260709T120000Z-eff05678"

  ereignisse <- list(
    list(typ = "import",        daten = list(name = "M.xlsx", menge = -0.1)),
    list(typ = "lauf",          daten = list(run_id = run)),
    list(typ = "kontrolle",     daten = list(kontrolle = "anker_plan_tiefpunkt",
                                             bestanden = TRUE)),
    list(typ = "export",        daten = list(format = "xlsx",
                                             sha256 = strrep("a", 64))),
    list(typ = "finalisierung", daten = list(run_id = run, status = "final")),
    list(typ = "korrektur",     daten = list(vorgaenger_run_id = run,
                                             run_id = run2))
  )

  zeiten <- list(.z1, .z2, .z3, .z4,
                 as.POSIXct("2026-07-09 12:00:00", tz = "UTC"),
                 as.POSIXct("2026-07-09 13:00:00", tz = "UTC"))

  for (k in seq_along(ereignisse)) {
    # Direktaufruf: ein Fehler bei irgendeinem der sechs Typen laesst den Test
    # von selbst scheitern (Schema-Abdeckung des gesamten Lebenszyklus).
    ev <- protokoll_anhaengen(p, ereignisse[[k]]$typ, ereignisse[[k]]$daten,
                              zeit = zeiten[[k]], user = "anker")
    expect_identical(ev$seq, as.integer(k))
    expect_identical(ev$typ, ereignisse[[k]]$typ)
  }

  # Sechs Zeilen, lueckenlose seq-Folge in der Datei.
  lines <- readLines(p)
  expect_length(lines, 6L)
  seqs <- vapply(lines, function(l) .parse_zeile(l)$seq, numeric(1))
  expect_equal(unname(seqs), as.numeric(1:6))

  unlink(p)
})


# ===========================================================================
#  (4) APPEND-ONLY, DETERMINISMUS und RNG-Waechter.
# ===========================================================================

test_that("protokoll_anhaengen: append-only — bestehende erste Zeile bleibt unberuehrt", {
  p <- .frischer_pfad("append")
  protokoll_anhaengen(p, "import", list(name = "M.xlsx", menge = -0.1),
                      zeit = .z1, user = "anker")
  protokoll_anhaengen(p, "lauf", list(run_id = "run-20260709T080000Z-abcd1234"),
                      zeit = .z2, user = "anker")
  protokoll_anhaengen(p, "kontrolle",
                      list(kontrolle = "anker_plan_tiefpunkt", bestanden = TRUE),
                      zeit = .z3, user = "anker")

  vorher    <- readLines(p)
  zeile1_vor <- vorher[[1]]

  # Ein viertes Ereignis anhaengen — die bestehenden Zeilen bleiben byte-gleich.
  protokoll_anhaengen(p, "export", list(format = "xlsx", sha256 = strrep("a", 64)),
                      zeit = .z4, user = "anker")

  nachher <- readLines(p)
  expect_length(nachher, 4L)
  expect_identical(nachher[[1]], zeile1_vor)          # erste Zeile unveraendert
  expect_identical(nachher[1:3], vorher[1:3])         # alle Bestandszeilen unveraendert

  unlink(p)
})

test_that("protokoll_anhaengen: deterministisch — identische Argumente -> identische Hashes", {
  bau <- function(tag) {
    p <- .frischer_pfad(tag)
    on.exit(unlink(p), add = TRUE)
    h <- character(0)
    h <- c(h, protokoll_anhaengen(p, "import",
                                  list(name = "M.xlsx", menge = -0.1),
                                  zeit = .z1, user = "anker")$hash)
    h <- c(h, protokoll_anhaengen(p, "lauf",
                                  list(run_id = "run-20260709T080000Z-abcd1234"),
                                  zeit = .z2, user = "anker")$hash)
    h <- c(h, protokoll_anhaengen(p, "kontrolle",
                                  list(kontrolle = "anker_plan_tiefpunkt", bestanden = TRUE),
                                  zeit = .z3, user = "anker")$hash)
    h
  }
  expect_identical(bau("det-a"), bau("det-b"))
})

test_that("protokoll_anhaengen: RNG-frei — .Random.seed unveraendert (GoBD-Pfad)", {
  # .Random.seed initialisieren, dann Snapshot.
  set.seed(1)
  invisible(runif(1))
  rng_vor <- .Random.seed

  p <- .frischer_pfad("rng")
  protokoll_anhaengen(p, "import", list(name = "M.xlsx", menge = -0.1),
                      zeit = .z1, user = "anker")
  protokoll_anhaengen(p, "lauf", list(run_id = "run-20260709T080000Z-abcd1234"),
                      zeit = .z2, user = "anker")

  expect_identical(.Random.seed, rng_vor)             # kein RNG-Verbrauch
  unlink(p)
})


# ===========================================================================
#  (5) FEHLERFAELLE: Validierung von typ, daten, user, zeit, Pfad und der
#      existierenden Leerdatei.
# ===========================================================================

test_that("protokoll_anhaengen: unzulaessiger typ -> Fehler nennt Wert UND Sechser-Menge", {
  p <- .frischer_pfad("err-typ")
  msg <- tryCatch(
    protokoll_anhaengen(p, "loeschen", list(), zeit = .z1, user = "anker"),
    error = function(e) conditionMessage(e))
  expect_type(msg, "character")
  expect_true(grepl("loeschen", msg))                 # der abgelehnte Wert
  for (t in c("import", "lauf", "kontrolle", "export",
              "finalisierung", "korrektur")) {
    expect_true(grepl(t, msg, fixed = TRUE))          # die zulaessige Menge
  }
})

test_that("protokoll_anhaengen: daten mit doppeltem Namen -> Fehler", {
  p <- .frischer_pfad("err-dup")
  expect_error(
    protokoll_anhaengen(p, "import", list(a = 1, a = 2), zeit = .z1, user = "anker"))
})

test_that("protokoll_anhaengen: nicht-skalare/NA daten-Werte -> Fehler mit Feldname daten$<name>", {
  p <- .frischer_pfad("err-daten")

  # Vektor-Wert Laenge 2.
  expect_error(
    protokoll_anhaengen(p, "import", list(v = c(1, 2)), zeit = .z1, user = "anker"),
    "daten\\$v")

  # Listen-Wert (kein Atom).
  expect_error(
    protokoll_anhaengen(p, "import", list(w = list(x = 1)), zeit = .z1, user = "anker"),
    "daten\\$w")

  # NA-Wert.
  expect_error(
    protokoll_anhaengen(p, "import", list(n = NA), zeit = .z1, user = "anker"),
    "daten\\$n")
})

test_that("protokoll_anhaengen: leerer user, String-zeit und leerer Pfad -> Fehler", {
  p <- .frischer_pfad("err-scalar")

  expect_error(
    protokoll_anhaengen(p, "import", list(name = "M.xlsx"), zeit = .z1, user = ""))
  expect_error(
    protokoll_anhaengen(p, "import", list(name = "M.xlsx"),
                        zeit = "2026-07-09T08:00:00Z", user = "anker"))
  expect_error(
    protokoll_anhaengen("", "import", list(name = "M.xlsx"), zeit = .z1, user = "anker"))
})

test_that("protokoll_anhaengen: existierende LEERE Datei -> stop('Protokoll leer')", {
  p <- .frischer_pfad("err-leer")
  file.create(p)                                      # existiert, 0 Zeilen
  expect_error(
    protokoll_anhaengen(p, "import", list(name = "M.xlsx"), zeit = .z1, user = "anker"),
    "Protokoll leer")
  unlink(p)
})
