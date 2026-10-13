# ============================================================================
#  tests/test_protokoll_pruefen.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-12-protokoll-pruefen
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: das ungeschuetzte
#  source(R/protokoll.R) am Kopf GELINGT (die Datei existiert aus den
#  Vorgaengerphasen und bringt .protokoll_hash + protokoll_anhaengen mit), aber
#  protokoll_pruefen ist dort NOCH NICHT definiert -> jeder Aufruf bricht ab
#  -> rot. Der Builder erfuellt sie erst mit der Implementierung des Verifiers.
#  Kein skip(), kein succeed(), keine Existenz-Weiche.
#
#  Vertrag (handoff/phase-12-protokoll-pruefen.json; vorlagenfrei, schirm-los,
#  GoBD Rz. 107/152 — lueckenlose, benennbar-nachpruefbare Protokollfolge):
#   - protokoll_pruefen(protokoll_pfad) liest saemtliche JSONL-Zeilen und liefert
#     invisible(TRUE) ODER stop() mit BENANNTEM Befund (Zeilennummer + Feldname,
#     Stil wie manifest_validieren in R/manifest.R).
#   - Pruefungen je Zeile i (1-basiert), Abbruch bei der ERSTEN Verletzung:
#       seq identical i                       (sonst 'Zeile <i>, seq')
#       typ in der Sechser-Menge import/lauf/kontrolle/export/finalisierung/
#         korrektur                           (sonst 'Zeile <i>, typ')
#       zeitstempel ~ ^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$ (sonst '..., zeitstempel')
#       user nicht leerer String              (sonst 'Zeile <i>, user')
#       VERKETTUNG vorheriger_hash identical (i==1 ? strrep('0',64) : hash von
#         Zeile i-1)                          (sonst 'Zeile <i>, verkettung')
#       HASH-REKONSTRUKTION .protokoll_hash(seq, typ, zeitstempel, user, daten,
#         vorheriger_hash) identical mit gespeichertem hash (sonst '..., hash')
#   - Datei fehlt -> stop() mit Pfad im Text; existierende LEERE Datei (0 Zeilen)
#     -> stop('Protokoll leer'). KEIN RNG im GoBD-Pfad.
#
#  Orakel-Politik: die Hash-Formel SELBST ist in phase-12-protokoll-hash gegen
#  NIST FIPS 180-4 und ein unabhaengiges kanon-Orakel gepinnt; HIER wird die
#  VERIFIKATION geprueft — gueltige Kette wird angenommen, jede EINZELNE
#  Feld-Mutilation OHNE Neu-Hash faellt an genau ihrer Zeile benannt auf. Die
#  gueltigen Protokolle werden real via protokoll_anhaengen gebaut (echter
#  verketteter Stand), die Mutilation danach per JSON-Rundreise ohne Neu-Hash
#  zurueckgeschrieben, sodass die 'wahren' Hashes stehen bleiben und nur das
#  eine Feld kaputt ist. Keine Anker-/Tier-1-Literale, keine Fixture-Mappe, kein
#  MC, kein Engine-Aufruf — nur Tempdateien mit injizierten UTC-zeiten.
#
#  Bestandsschutz: reine Neudatei; beruehrt weder kernel/ noch R/manifest.R noch
#  R/manifest_anbindung.R noch eine der neun test_manifest*-Dateien. Keine neuen
#  Pakete — digest und jsonlite sind seit phase-12-runmanifest-kern im
#  need-Vektor von tests/deps.R.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "protokoll.R"))  # .protokoll_hash + protokoll_anhaengen da; protokoll_pruefen fehlt -> rot

# Rot-zuerst-Riegel: source() GELINGT, aber protokoll_pruefen ist NOCH NICHT
# definiert. Ohne diesen Riegel wuerden die expect_error-Faelle unten schon vom
# 'could not find function'-Fehler faelschlich gruen — deshalb faellt die GANZE
# Spezifikation hart durch, bis der Verifier existiert. Kein skip, kein succeed:
# bewusst Immer-ROT bis zur Implementierung, nicht Immer-gruen.
if (!exists("protokoll_pruefen", mode = "function")) {
  stop("protokoll_pruefen ist noch nicht definiert (rot zuerst)")
}

# ---------------------------------------------------------------------------
# Feste Ereignis-Koordinaten (handgesetzte Literale, keine Anker/Tier-1).
# Injizierte UTC-Zeiten halten den Kern deterministisch (wie beim Manifest).
# ---------------------------------------------------------------------------
.z1 <- as.POSIXct("2026-07-09 08:00:00", tz = "UTC")
.z2 <- as.POSIXct("2026-07-09 09:00:00", tz = "UTC")
.z3 <- as.POSIXct("2026-07-09 10:00:00", tz = "UTC")

.frischer_pfad <- function(tag) {
  p <- file.path(tempdir(), sprintf("protokoll-pruefen-%s.jsonl", tag))
  unlink(p)
  p
}

# Baut real ueber protokoll_anhaengen ein gueltiges, verkettetes
# Drei-Ereignis-Protokoll (import/lauf/kontrolle) und liefert den Pfad.
.baue_gueltig3 <- function(tag) {
  p <- .frischer_pfad(tag)
  protokoll_anhaengen(p, "import",
                      list(name = "M.xlsx", menge = -0.1),
                      zeit = .z1, user = "anker")
  protokoll_anhaengen(p, "lauf",
                      list(run_id = "run-20260709T080000Z-abcd1234"),
                      zeit = .z2, user = "anker")
  protokoll_anhaengen(p, "kontrolle",
                      list(kontrolle = "anker_plan_tiefpunkt", bestanden = TRUE),
                      zeit = .z3, user = "anker")
  p
}

# Liest quelle, parst jede JSONL-Zeile, wendet mut(ev) an (EINE Feld-Mutatio
# OHNE Neu-Hash), serialisiert die veraenderten Ereignisse zurueck und schreibt
# sie nach ziel. Die 'wahren' Hashes bleiben stehen — nur das mutierte Feld ist
# kaputt, sodass die Rekonstruktion an genau dieser Zeile scheitern muss.
.tamper <- function(quelle, ziel, mut) {
  lines <- readLines(quelle)
  ev <- lapply(lines, function(l) jsonlite::fromJSON(l))
  ev <- mut(ev)
  tamp <- vapply(ev, function(x)
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE)), character(1))
  writeLines(tamp, ziel)
  ziel
}


# ===========================================================================
#  (1) GUELTIGES PROTOKOLL wird angenommen: invisible(TRUE), kein Fehler.
# ===========================================================================

test_that("protokoll_pruefen: gueltiges Drei-Ereignis-Protokoll -> invisible(TRUE)", {
  p <- .baue_gueltig3("gueltig3")

  # invisible(TRUE): unsichtbar UND echtes TRUE, kein Fehler.
  wv <- withVisible(protokoll_pruefen(p))
  expect_false(wv$visible)
  expect_true(isTRUE(wv$value))

  unlink(p)
})

test_that("protokoll_pruefen: Protokoll mit ALLEN SECHS Ereignistypen -> invisible(TRUE)", {
  # Voller Lebenszyklus; je ein character-, logical- und numerischer daten-Wert
  # ist quer ueber die sechs Ereignisse abgedeckt (Schema-Abdeckung).
  p <- .frischer_pfad("sechs")
  run  <- "run-20260709T080000Z-abcd1234"
  run2 <- "run-20260709T120000Z-eff05678"

  ereignisse <- list(
    list(typ = "import",        daten = list(name = "M.xlsx")),              # character
    list(typ = "lauf",          daten = list(run_id = run)),                 # character
    list(typ = "kontrolle",     daten = list(bestanden = TRUE)),             # logical
    list(typ = "export",        daten = list(groesse = 1024)),               # numeric
    list(typ = "finalisierung", daten = list(final = TRUE)),                 # logical
    list(typ = "korrektur",     daten = list(vorgaenger_run_id = run,
                                             anteil = 0.5))                  # character+numeric
  )
  zeiten <- list(
    .z1, .z2, .z3,
    as.POSIXct("2026-07-09 11:00:00", tz = "UTC"),
    as.POSIXct("2026-07-09 12:00:00", tz = "UTC"),
    as.POSIXct("2026-07-09 13:00:00", tz = "UTC"))

  for (k in seq_along(ereignisse)) {
    protokoll_anhaengen(p, ereignisse[[k]]$typ, ereignisse[[k]]$daten,
                        zeit = zeiten[[k]], user = "anker")
  }

  expect_true(isTRUE(protokoll_pruefen(p)))
  unlink(p)
})


# ===========================================================================
#  (2) TAMPER-EVIDENCE: je EINE Feld-Mutatio ohne Neu-Hash faellt an genau
#      ihrer Zeile BENANNT auf (Lehre: succeed in Fallback ist unsichtbares
#      Immer-Gruen — jede Mutatio MUSS benannt zerreissen, nie still durch).
# ===========================================================================

test_that("protokoll_pruefen: daten-Mutatio in Zeile 2 -> Fehler nennt 'Zeile 2'", {
  # run_id von Zeile 2 veraendert, hash aber unangetastet -> die
  # Hash-Rekonstruktion ueber die (veraenderten) daten passt nicht mehr.
  p  <- .baue_gueltig3("mut-daten")
  p2 <- .tamper(p, .frischer_pfad("mut-daten-tamp"), function(ev) {
    ev[[2]]$daten$run_id <- "run-Y"
    ev
  })
  expect_error(protokoll_pruefen(p2), "Zeile 2")
  unlink(c(p, p2))
})

test_that("protokoll_pruefen: hash-Mutatio in Zeile 2 -> Fehler nennt 'Zeile 2'", {
  # hash[2] auf einen ANDEREN gueltig-geformten 64-Hex gesetzt (keine
  # Neuberechnung) -> Rekonstruktion != gespeicherter hash.
  p  <- .baue_gueltig3("mut-hash")
  p2 <- .tamper(p, .frischer_pfad("mut-hash-tamp"), function(ev) {
    ev[[2]]$hash <- strrep("b", 64)
    ev
  })
  msg <- conditionMessage(tryCatch(protokoll_pruefen(p2), error = function(e) e))
  expect_true(grepl("Zeile 2", msg))
  unlink(c(p, p2))
})

test_that("protokoll_pruefen: vorheriger_hash-Mutatio in Zeile 2 -> Fehler nennt Zeile 2 (verkettung)", {
  # vorheriger_hash[2] passt nach der Mutatio nicht mehr zum hash von Zeile 1
  # -> die Verkettungspruefung schlaegt zuerst an (vor der Hash-Rekonstruktion).
  p  <- .baue_gueltig3("mut-vh")
  p2 <- .tamper(p, .frischer_pfad("mut-vh-tamp"), function(ev) {
    ev[[2]]$vorheriger_hash <- strrep("c", 64)
    ev
  })
  msg <- conditionMessage(tryCatch(protokoll_pruefen(p2), error = function(e) e))
  expect_true(grepl("Zeile 2", msg))
  expect_true(grepl("verkettung", msg))
  unlink(c(p, p2))
})

test_that("protokoll_pruefen: seq-Mutatio in Zeile 3 -> Fehler nennt Zeile 3 (seq)", {
  # seq der Zeile 3 auf 2 gesetzt -> die seq-Pruefung (seq identical i) schlaegt
  # als erste an dieser Zeile an.
  p  <- .baue_gueltig3("mut-seq")
  p2 <- .tamper(p, .frischer_pfad("mut-seq-tamp"), function(ev) {
    ev[[3]]$seq <- 2L
    ev
  })
  msg <- conditionMessage(tryCatch(protokoll_pruefen(p2), error = function(e) e))
  expect_true(grepl("Zeile 3", msg))
  expect_true(grepl("seq", msg))
  unlink(c(p, p2))
})


# ===========================================================================
#  (3) FEHLERFAELLE und RNG-Waechter.
# ===========================================================================

test_that("protokoll_pruefen: nicht existierende Datei -> Fehler mit Pfad im Text", {
  p <- .frischer_pfad("gibtsnicht")   # unlink() im Helper -> existiert garantiert nicht
  expect_false(file.exists(p))
  msg <- conditionMessage(tryCatch(protokoll_pruefen(p), error = function(e) e))
  expect_true(grepl(p, msg, fixed = TRUE))   # Pfad benannt (fixed: Backslashes auf Windows)
})

test_that("protokoll_pruefen: existierende LEERE Datei -> stop('Protokoll leer')", {
  p <- .frischer_pfad("leer")
  file.create(p)                              # existiert, 0 Zeilen
  expect_error(protokoll_pruefen(p), "Protokoll leer")
  unlink(p)
})

test_that("protokoll_pruefen: RNG-frei — .Random.seed unveraendert (GoBD-Pfad)", {
  p <- .baue_gueltig3("rng")

  # .Random.seed initialisieren, dann Snapshot.
  set.seed(1)
  invisible(runif(1))
  rng_vor <- .Random.seed

  invisible(protokoll_pruefen(p))

  expect_identical(.Random.seed, rng_vor)     # kein RNG-Verbrauch
  unlink(p)
})
