# ============================================================================
#  tests/test_protokoll_pruefen_feldnamen.R  —  Nachschaerfung des
#  BENANNTEN-BEFUND-Kontrakts von protokoll_pruefen (Ergaenzung zur
#  Spezifikation in tests/test_protokoll_pruefen.R).
#
#  Warum eine eigene Datei: die committete Spezifikation ist Bestand
#  (Test-Bestand-Guard) und wird nicht angefasst. Sie prueft bei den
#  daten-/hash-Mutationen nur "Zeile 2" — ein Befund, der die richtige Zeile,
#  aber das falsche Feld nennt, bliebe dort gruen. Hier wird zusaetzlich der
#  FELDNAME assertiert (Lehre: succeed/zu schwacher Match in Fallback ist
#  unsichtbares Immer-Gruen). Dazu die beiden neuen Riegel des Verifiers:
#    - unbekanntes/fehlendes Top-Level-Feld  -> 'Zeile <i>, schema'
#    - kaputtes JSON in einer Zeile          -> 'Zeile <i>, json'
#  Beides muss BENANNT (Zeile + Feld) zerreissen, nie still durchlaufen.
#
#  NACHPRUEFBARKEIT (Red-Team-Antwort auf UNVERIFIED): jeder Block ist
#  NICHT-VAKUOES konstruiert und laesst sich nur GRUEN erreichen, wenn
#  protokoll_pruefen sich in BEIDE Richtungen korrekt verhaelt:
#    (a) POSITIV-KONTROLLE  — das frisch gebaute, GUELTIGE Protokoll wird
#        angenommen (expect_true(isTRUE(...))). Waere protokoll_pruefen ein
#        Konstant-Fehler, faellt diese Zeile.
#    (b) MUTATIONS-RIEGEL    — nach GENAU EINER Feld-Mutatio OHNE Neu-Hash MUSS
#        ein Fehler fliegen, dessen Text Zeile UND Feld nennt (expect_error mit
#        'Zeile <i>.*<feld>'). Waere protokoll_pruefen ein No-Op (nimmt alles
#        an), faellt expect_error mangels Fehler.
#  Damit ist keine dieser Zusicherungen erfuellbar, ohne dass der Verifier
#  echt arbeitet — und keine kann still an einem conditionMessage()-auf-TRUE
#  scheitern (expect_error ist gegen den Nicht-Fehler-Fall robust, statt ihn
#  in einen opaken Laufzeitfehler zu drehen).
#
#  Zusaetzlich beweist ein LOSSLESS-ROUNDTRIP-Riegel, dass die Tamper-Mechanik
#  (fromJSON -> toJSON, alle Zeilen neu serialisiert) das gueltige Protokoll
#  NICHT verfaelscht — jeder Fehlschlag in den Mutationsbloecken ist damit
#  allein der Mutatio zuzuschreiben, nicht dem Neu-Serialisieren.
#
#  Orakel-Politik wie in der Spezifikation: gueltige Ketten werden real via
#  protokoll_anhaengen gebaut, danach GENAU EIN Feld ohne Neu-Hash manipuliert
#  (bzw. Zeile durch Garbage ersetzt). Keine Anker/Tier-1-Literale, kein MC,
#  kein Engine-Aufruf — nur Tempdateien mit injizierten UTC-Zeiten.
#
#  Bestandsschutz: reine Neudatei; beruehrt weder kernel/ noch R/manifest*.R
#  noch eine bestehende test_*-Datei. Keine neuen Pakete (digest + jsonlite
#  seit phase-12-runmanifest-kern im need-Vektor von tests/deps.R).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "protokoll.R"))

# Rot-zuerst-Riegel wie in der Spezifikation: ohne definierten Verifier faellt
# die GANZE Datei hart durch, statt sich gruen zu luegen.
if (!exists("protokoll_pruefen", mode = "function")) {
  stop("protokoll_pruefen ist noch nicht definiert (rot zuerst)")
}

.z1 <- as.POSIXct("2026-07-09 08:00:00", tz = "UTC")
.z2 <- as.POSIXct("2026-07-09 09:00:00", tz = "UTC")
.z3 <- as.POSIXct("2026-07-09 10:00:00", tz = "UTC")

.frischer_pfad_fn <- function(tag) {
  p <- file.path(tempdir(), sprintf("protokoll-feldn-%s.jsonl", tag))
  unlink(p)
  p
}

# Reales, verkettetes Drei-Ereignis-Protokoll (import/lauf/kontrolle).
.baue_gueltig3_fn <- function(tag) {
  p <- .frischer_pfad_fn(tag)
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

# Parst jede Zeile, wendet mut(ev) an (EINE Manipulation OHNE Neu-Hash),
# serialisiert zurueck und schreibt nach ziel. mut = identity ergibt einen
# verlustfreien Roundtrip (Kontroll-Positivfall).
.tamper_fn <- function(quelle, ziel, mut) {
  lines <- readLines(quelle)
  ev <- lapply(lines, function(l) jsonlite::fromJSON(l))
  ev <- mut(ev)
  tamp <- vapply(ev, function(x)
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE)), character(1))
  writeLines(tamp, ziel)
  ziel
}


# ---------------------------------------------------------------------------
#  (0) LOSSLESS-ROUNDTRIP-RIEGEL: der Tamper-Apparat selbst verfaelscht nichts.
#      Ohne diese Kontrolle koennte ein Fehlschlag unten faelschlich dem
#      Neu-Serialisieren statt der Mutatio angelastet werden.
# ---------------------------------------------------------------------------

test_that("Roundtrip OHNE Mutatio bleibt gueltig (Tamper-Apparat ist verlustfrei)", {
  p  <- .baue_gueltig3_fn("rt-clean")
  expect_true(isTRUE(protokoll_pruefen(p)))                 # Original gueltig
  p2 <- .tamper_fn(p, .frischer_pfad_fn("rt-clean-tamp"), function(ev) ev)  # identity
  expect_true(isTRUE(protokoll_pruefen(p2)))                # Roundtrip gueltig
  unlink(c(p, p2))
})


# ---------------------------------------------------------------------------
#  (1) FELDNAME-NACHSCHAERFUNG: daten-/hash-Mutatio nennt Zeile UND Feld 'hash'.
# ---------------------------------------------------------------------------

test_that("daten-Mutatio Zeile 2: gueltig akzeptiert, Befund nennt Zeile 2 UND Feld 'hash'", {
  # daten von Zeile 2 veraendert, hash unangetastet -> die Rekonstruktion ueber
  # die (veraenderten) daten passt nicht mehr: Befund am Feld 'hash'.
  p  <- .baue_gueltig3_fn("mut-daten")
  expect_true(isTRUE(protokoll_pruefen(p)))                 # Positiv-Kontrolle
  p2 <- .tamper_fn(p, .frischer_pfad_fn("mut-daten-tamp"), function(ev) {
    ev[[2]]$daten$run_id <- "run-Y"
    ev
  })
  expect_error(protokoll_pruefen(p2), "Zeile 2.*hash")      # Zeile + Feld benannt
  unlink(c(p, p2))
})

test_that("hash-Mutatio Zeile 2: gueltig akzeptiert, Befund nennt Zeile 2 UND Feld 'hash'", {
  # hash[2] auf einen anderen gueltig-geformten 64-Hex gesetzt (kein Neu-Hash)
  # -> Rekonstruktion != gespeicherter hash: Befund am Feld 'hash'.
  p  <- .baue_gueltig3_fn("mut-hash")
  expect_true(isTRUE(protokoll_pruefen(p)))                 # Positiv-Kontrolle
  p2 <- .tamper_fn(p, .frischer_pfad_fn("mut-hash-tamp"), function(ev) {
    ev[[2]]$hash <- strrep("b", 64)
    ev
  })
  expect_error(protokoll_pruefen(p2), "Zeile 2.*hash")      # Zeile + Feld benannt
  unlink(c(p, p2))
})


# ---------------------------------------------------------------------------
#  (2) SCHEMA-RIEGEL: unbekanntes oder fehlendes Top-Level-Feld faellt benannt.
# ---------------------------------------------------------------------------

test_that("Zusatzfeld in Zeile 2: gueltig akzeptiert, Befund nennt Zeile 2 UND Feld 'schema'", {
  # Ein Achtfeld, das der Hash nicht abdeckt, wuerde ohne Schema-Riegel weder
  # geprueft noch gehasht -> muss am Feld 'schema' zerreissen.
  p  <- .baue_gueltig3_fn("schema-extra")
  expect_true(isTRUE(protokoll_pruefen(p)))                 # Positiv-Kontrolle
  p2 <- .tamper_fn(p, .frischer_pfad_fn("schema-extra-tamp"), function(ev) {
    ev[[2]]$heimlich <- "still eingeschmuggelt"
    ev
  })
  expect_error(protokoll_pruefen(p2), "Zeile 2.*schema")    # Zeile + Feld benannt
  unlink(c(p, p2))
})

test_that("fehlendes Feld in Zeile 2: gueltig akzeptiert, Befund nennt Zeile 2 UND Feld 'schema'", {
  # Fehlt eines der sieben Kanon-Felder, ist die Zeile ebenso ungueltig.
  p  <- .baue_gueltig3_fn("schema-fehlt")
  expect_true(isTRUE(protokoll_pruefen(p)))                 # Positiv-Kontrolle
  p2 <- .tamper_fn(p, .frischer_pfad_fn("schema-fehlt-tamp"), function(ev) {
    ev[[2]]$user <- NULL
    ev
  })
  expect_error(protokoll_pruefen(p2), "Zeile 2.*schema")    # Zeile + Feld benannt
  unlink(c(p, p2))
})


# ---------------------------------------------------------------------------
#  (3) JSON-RIEGEL: kaputte JSONL-Zeile faellt benannt (Zeile + Feld 'json').
# ---------------------------------------------------------------------------

test_that("kaputtes JSON in Zeile 2: gueltig akzeptiert, Befund nennt Zeile 2 UND Feld 'json'", {
  # Zeile 1 bleibt gueltig, damit der Verifier Zeile 2 wirklich erreicht; dort
  # steht Garbage statt eines JSON-Objekts -> benannter 'json'-Befund, nicht ein
  # generischer jsonlite-Parsefehler.
  p  <- .baue_gueltig3_fn("json-kaputt")
  expect_true(isTRUE(protokoll_pruefen(p)))                 # Positiv-Kontrolle
  lines <- readLines(p)
  lines[[2]] <- "{ das ist kein gueltiges json"
  p2 <- .frischer_pfad_fn("json-kaputt-tamp")
  writeLines(lines, p2)

  expect_error(protokoll_pruefen(p2), "Zeile 2.*json")      # Zeile + Feld benannt
  unlink(c(p, p2))
})
