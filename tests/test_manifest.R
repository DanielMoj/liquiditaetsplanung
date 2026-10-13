# ============================================================================
#  tests/test_manifest.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-12-runmanifest-kern
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/manifest.R
#  existiert noch nicht -> das ungeschuetzte source() am Kopf bricht ab; jeder
#  Test spezifiziert zusaetzlich das Zielverhalten). Der Builder erfuellt sie
#  erst mit der Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche.
#
#  Vertrag (handoff/phase-12-runmanifest-kern.json; KEIN Legacy-Vorbild —
#  die Einheit ist vorlagenfrei und schirm-los, GoBD Rz. 152/154):
#   - sha256_datei(pfad): SHA-256 ueber die ROHEN BYTES einer Datei,
#     64-stelliger Kleinbuchstaben-Hexstring; fehlende Datei -> Fehler MIT
#     dem Pfad im Fehlertext.
#   - umgebung_erfassen(): duenner Sammler mit EXAKT den Feldern r_version /
#     plattform / git_sha / git_dirty / renv_lock_sha256; wirft nie.
#   - manifest_erstellen(eingaben, parameter, seeds, outputs, zeit, user,
#     umgebung): reiner, DETERMINISTISCHER Kern ohne RNG und ohne Sys.time();
#     eingaben/outputs als data.frames mit exakt name/pfad/sha256/
#     groesse_bytes; zeitstempel UTC-normiert '%Y-%m-%dT%H:%M:%SZ'; run_id
#     'run-<UTC-Zeitanteil>-<8 Hex aus sha256 der kanonischen Verkettung>'.
#   - manifest_validieren(m): deterministischer Schema-Validator, stop() mit
#     dem VERLETZTEN FELDNAMEN im Fehlertext, sonst invisible(TRUE).
#   - manifest_schreiben(m, pfad): validiert ZUERST (invalide -> keine Datei),
#     write-once (Zielpfad existiert -> Fehler mit Pfad, Datei bleibt
#     byte-identisch), jsonlite::write_json; Rueckgabe invisible(pfad).
#   - manifest_lesen(pfad): Ruecklese feldtreu, besteht den Validator; leeres
#     outputs-Array wird als 0-Zeilen-data.frame MIT den vier Spalten
#     rekonstruiert (dokumentierter JSON-Grenzfall).
#
#  Orakel-Politik: EXTERNE Orakel statt Selbstkonsistenz — KEIN Hash prueft
#  sich selbst. Alle Hash-Erwartungswerte sind woertlich uebernommene
#  LITERALE: leere Datei und 'abc' aus NIST FIPS 180-4 (eingefrorene
#  SHA-256-Testvektoren), 'GoBD-Anker-2026' (15 Bytes) unabhaengig
#  AUSSERHALB des Repos via sha256sum berechnet. Erwartete Groessen (3, 15),
#  UTC-Stempel ('2026-07-08T10:34:56Z') und run_id-Praefix
#  (run-20260708T103456Z-) sind handgerechnete Literale, NIE per Aufruf der
#  getesteten Funktionen erzeugt. Keine Anker-/Tier-1-Literale, keine
#  Fixture-Mappe, kein MC, kein Engine-Aufruf — nur Tempdateien mit
#  bekanntem Byte-Inhalt via writeBin(charToRaw(...)) und handgebaute
#  Listen, kleine Laufzeit.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))  # <- existiert noch nicht: rot zuerst

# ---------------------------------------------------------------------------
# Gepinnte Hash-Orakel (LITERALE, Herkunft siehe Kopfkommentar):
#   leer  -> NIST FIPS 180-4 (SHA-256 der leeren Nachricht)
#   'abc' -> NIST FIPS 180-4 (SHA-256-Testvektor 'abc')
#   'GoBD-Anker-2026' (15 Bytes) -> unabhaengig ausserhalb des Repos via
#   sha256sum berechnet
# ---------------------------------------------------------------------------
.hash_leer <- "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
.hash_abc  <- "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
.hash_gobd <- "d8b24ce6fd6db563b2ee90c5f2c59ece7fb98c833515a53941bec5fd3bff2e0d"

# Zeit-Pins OHNE tzdata-Abhaengigkeit (feste POSIX-Offsets). POSIX-
# Vorzeichenumkehr: Etc/GMT-2 IST UTC+2, also 12:34:56 Lokalzeit ->
# 10:34:56 UTC. Erwartete Strings sind handgerechnete Literale.
.zeit_gmt2      <- as.POSIXct("2026-07-08 12:34:56", tz = "Etc/GMT-2")
.zeit_utc       <- as.POSIXct("2026-07-08 10:34:56", tz = "UTC")
.stempel_erw    <- "2026-07-08T10:34:56Z"
.runid_praefix  <- "run-20260708T103456Z-"
.runid_re       <- "^run-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{8}$"

.manifest_felder <- c("manifest_version", "run_id", "zeitstempel", "user",
                      "umgebung", "eingaben", "parameter", "seeds", "outputs")
.df_spalten      <- c("name", "pfad", "sha256", "groesse_bytes")
.umg_felder      <- c("r_version", "plattform", "git_sha", "git_dirty",
                      "renv_lock_sha256")

# Zwei Tempdateien mit bekanntem Byte-Inhalt ('abc' = 3 Bytes,
# 'GoBD-Anker-2026' = 15 Bytes) — Groessen handgerechnet, nicht gemessen.
.mk_eingaben <- function() {
  a <- tempfile(); writeBin(charToRaw("abc"), a)
  g <- tempfile(); writeBin(charToRaw("GoBD-Anker-2026"), g)
  c(a, g)
}

# Handgebaute, feste Umgebung — INJIZIERT, damit der reine Kern
# deterministisch testbar ist (kein umgebung_erfassen()-Aufruf im Pfad).
.mk_umgebung <- function() {
  list(r_version        = as.character(getRversion()),
       plattform        = "testplattform",
       git_sha          = NA_character_,
       git_dirty        = NA,
       renv_lock_sha256 = NA_character_)
}

.params <- list(H = 4L, alpha = 0.25, gewichte = c(0.5, 0.25, 0.25))

.mk_manifest <- function(pfade,
                         seeds   = c(sim = 11L, boot = 23L),
                         zeit    = .zeit_utc,
                         user    = "testuser",
                         outputs = character(0)) {
  manifest_erstellen(eingaben = pfade, parameter = .params, seeds = seeds,
                     outputs = outputs, zeit = zeit, user = user,
                     umgebung = .mk_umgebung())
}

# ===========================================================================
#  (1) sha256_datei: eingefrorene Binding-Orakel
# ===========================================================================

test_that("manifest: sha256_datei gegen NIST-FIPS-180-4- und sha256sum-Orakel — 64 Zeichen Kleinbuchstaben-Hex", {
  leer <- tempfile(); writeBin(raw(0), leer)
  abc  <- tempfile(); writeBin(charToRaw("abc"), abc)
  gobd <- tempfile(); writeBin(charToRaw("GoBD-Anker-2026"), gobd)

  h_leer <- sha256_datei(leer)
  h_abc  <- sha256_datei(abc)
  h_gobd <- sha256_datei(gobd)

  # Binding-Orakel woertlich (Herkunft: NIST FIPS 180-4 fuer leer/'abc';
  # 'GoBD-Anker-2026' unabhaengig ausserhalb des Repos via sha256sum).
  expect_identical(h_leer, .hash_leer)
  expect_identical(h_abc,  .hash_abc)
  expect_identical(h_gobd, .hash_gobd)

  # Formvertrag: 64-stelliger Kleinbuchstaben-Hexstring.
  for (h in list(h_leer, h_abc, h_gobd)) {
    expect_identical(nchar(h), 64L)
    expect_true(grepl("^[0-9a-f]{64}$", h))
  }

  unlink(c(leer, abc, gobd))
})

test_that("manifest: sha256_datei — fehlende Datei wirft Fehler MIT dem Pfad im Fehlertext", {
  fehlt <- tempfile()
  expect_false(file.exists(fehlt))
  expect_error(sha256_datei(fehlt), regexp = fehlt, fixed = TRUE)
})

# ===========================================================================
#  (2) manifest_erstellen: Vertrag, Zeit-Pins, Determinismus, Fehlerfaelle
# ===========================================================================

test_that("manifest: manifest_erstellen — eingaben-data.frame in Aufrufreihenfolge mit gepinnten Hashes und handgerechneten Groessen", {
  pfade <- .mk_eingaben()
  m <- .mk_manifest(pfade)

  expect_true(is.list(m))
  expect_identical(names(m), .manifest_felder)
  expect_identical(m$manifest_version, 1L)
  expect_identical(m$user, "testuser")

  expect_true(is.data.frame(m$eingaben))
  expect_identical(names(m$eingaben), .df_spalten)
  expect_identical(nrow(m$eingaben), 2L)
  # Zeilen in Aufrufreihenfolge: erst 'abc', dann 'GoBD-Anker-2026'.
  expect_identical(m$eingaben$name, basename(pfade))
  expect_identical(m$eingaben$pfad, pfade)
  # Hashes identisch zu den Pins aus (1) — LITERALE, kein Selbstbezug.
  expect_identical(m$eingaben$sha256, c(.hash_abc, .hash_gobd))
  # Groessen handgerechnet: 'abc' = 3 Bytes, 'GoBD-Anker-2026' = 15 Bytes.
  expect_identical(m$eingaben$groesse_bytes, c(3, 15))

  # outputs = character(0) -> 0-Zeilen-data.frame MIT den vier Spalten.
  expect_true(is.data.frame(m$outputs))
  expect_identical(names(m$outputs), .df_spalten)
  expect_identical(nrow(m$outputs), 0L)

  unlink(pfade)
})

test_that("manifest: manifest_erstellen — UTC-Zeitstempel-Pins ohne tzdata (Etc/GMT-2 IST UTC+2) und run_id-Format mit Zeitanteil aus zeit", {
  pfade <- .mk_eingaben()

  # Etc/GMT-2 = UTC+2 (POSIX-Vorzeichenumkehr): 12:34:56 lokal -> 10:34:56Z.
  m1 <- .mk_manifest(pfade, zeit = .zeit_gmt2)
  expect_identical(m1$zeitstempel, .stempel_erw)

  # UTC-Eingabe bleibt unverschoben.
  m2 <- .mk_manifest(pfade, zeit = .zeit_utc)
  expect_identical(m2$zeitstempel, .stempel_erw)

  # run_id-Regex und handgerechnetes Praefix aus dem UTC-Zeitanteil.
  expect_true(grepl(.runid_re, m1$run_id))
  expect_true(grepl(.runid_re, m2$run_id))
  expect_true(startsWith(m1$run_id, .runid_praefix))
  expect_true(startsWith(m2$run_id, .runid_praefix))

  unlink(pfade)
})

test_that("manifest: manifest_erstellen — Determinismus (identische Argumente -> identical) und Seed-Sensitivitaet der run_id", {
  pfade <- .mk_eingaben()

  m_a <- .mk_manifest(pfade)
  m_b <- .mk_manifest(pfade)
  expect_identical(m_b, m_a)                    # kompletter Objektvergleich

  # Geaenderter Seed-WERT -> andere run_id (Negativ-Pin gegen konstantes
  # Suffix).
  m_c <- .mk_manifest(pfade, seeds = c(sim = 12L, boot = 23L))
  expect_false(identical(m_c$run_id, m_a$run_id))

  unlink(pfade)
})

test_that("manifest: manifest_erstellen — Fehlerfaelle: fehlender Eingabepfad (Pfad im Text), eingaben leer, seeds leer/unbenannt, user leer, zeit als String", {
  pfade <- .mk_eingaben()
  fehlt <- tempfile()
  expect_false(file.exists(fehlt))

  # Nicht existierender Eingabepfad -> Fehler MIT dem Pfad im Text.
  expect_error(.mk_manifest(fehlt), regexp = fehlt, fixed = TRUE)

  # eingaben leer (ein Lauf ohne Eingaben ist nicht rekonstruierbar).
  expect_error(.mk_manifest(character(0)))

  # seeds leer bzw. unbenannt.
  expect_error(.mk_manifest(pfade, seeds = numeric(0)))
  expect_error(.mk_manifest(pfade, seeds = c(11L, 23L)))

  # user leer.
  expect_error(.mk_manifest(pfade, user = ""))

  # zeit als String statt POSIXct.
  expect_error(.mk_manifest(pfade, zeit = "2026-07-08 10:34:56"))

  unlink(pfade)
})

# ===========================================================================
#  (3) manifest_validieren: gueltige Basis TRUE, neun Mutilationen mit
#      benanntem Befund (Rz.-152-Pruefbarkeit heisst benennbare Befunde)
# ===========================================================================

test_that("manifest: manifest_validieren — Basis besteht, neun Feld-Mutilationen je von FRISCHER Kopie mit Feldname im Fehlertext", {
  pfade <- .mk_eingaben()
  basis <- .mk_manifest(pfade)

  expect_true(isTRUE(manifest_validieren(basis)))

  # (a) manifest_version 2L statt 1L.
  v <- basis; v$manifest_version <- 2L
  expect_error(manifest_validieren(v), regexp = "manifest_version")

  # (b) run_id ausserhalb des Formats.
  v <- basis; v$run_id <- "run-x"
  expect_error(manifest_validieren(v), regexp = "run_id")

  # (c) zeitstempel ohne Z-Suffix.
  v <- basis; v$zeitstempel <- sub("Z$", "", basis$zeitstempel)
  expect_error(manifest_validieren(v), regexp = "zeitstempel")

  # (d) user leer.
  v <- basis; v$user <- ""
  expect_error(manifest_validieren(v), regexp = "user")

  # (e) umgebung ohne renv_lock_sha256 (Befund nennt das verletzte Feld —
  #     umgebung bzw. renv_lock_sha256 laut Handoff).
  v <- basis; v$umgebung$renv_lock_sha256 <- NULL
  expect_error(manifest_validieren(v), regexp = "umgebung|renv_lock_sha256")

  # (f) eingaben mit 0 Zeilen.
  v <- basis; v$eingaben <- basis$eingaben[0, ]
  expect_error(manifest_validieren(v), regexp = "eingaben")

  # (g) eine eingaben$sha256 auf 63 Zeichen gekuerzt.
  v <- basis; v$eingaben$sha256[1] <- substr(basis$eingaben$sha256[1], 1, 63)
  expect_error(manifest_validieren(v), regexp = "sha256")

  # (h) seeds unbenannt.
  v <- basis; v$seeds <- unname(basis$seeds)
  expect_error(manifest_validieren(v), regexp = "seeds")

  # (i) outputs ohne Spalte sha256.
  v <- basis; v$outputs$sha256 <- NULL
  expect_error(manifest_validieren(v), regexp = "outputs")

  unlink(pfade)
})

# ===========================================================================
#  (4) manifest_schreiben: write-once (GoBD Rz. 58 ff./107-Anschluss)
# ===========================================================================

test_that("manifest: manifest_schreiben — Erstschreiben ok, zweiter Versuch Fehler mit Pfad und Datei byte-identisch, invalides Objekt erzeugt KEINE Datei", {
  pfade <- .mk_eingaben()
  m <- .mk_manifest(pfade)
  p <- tempfile(fileext = ".json")

  # Erstschreiben: Datei entsteht, Rueckgabe unsichtbar der Pfad, gueltiges
  # JSON (read_json wirft nicht).
  sichtbar <- withVisible(manifest_schreiben(m, p))
  expect_false(sichtbar$visible)
  expect_identical(sichtbar$value, p)
  expect_true(file.exists(p))
  expect_error(jsonlite::read_json(p), regexp = NA)

  # ZWEITER Schreibversuch: Fehler MIT dem Pfad im Text, Datei bleibt
  # byte-identisch (readBin-Vollvergleich als Negativ-Pin gegen stilles
  # Ueberschreiben).
  b1 <- readBin(p, "raw", n = file.info(p)$size)
  expect_error(manifest_schreiben(m, p), regexp = p, fixed = TRUE)
  b2 <- readBin(p, "raw", n = file.info(p)$size)
  expect_identical(length(b2), length(b1))
  expect_identical(b2, b1)

  # Invalides Objekt auf FRISCHEN Pfad: Fehler UND der Pfad existiert danach
  # nicht (Validierung VOR Dateierzeugung).
  mi <- m; mi$user <- ""
  p2 <- tempfile(fileext = ".json")
  expect_error(manifest_schreiben(mi, p2))
  expect_false(file.exists(p2))

  unlink(c(p, p2, pfade))
})

# ===========================================================================
#  (5) manifest_lesen: Ruecklese-Treue (retrograde Pruefung)
# ===========================================================================

test_that("manifest: manifest_lesen — gelesenes Objekt besteht den Validator und ist feldtreu, leere outputs ueberleben den Roundtrip", {
  pfade <- .mk_eingaben()
  m <- .mk_manifest(pfade)
  p <- tempfile(fileext = ".json")
  manifest_schreiben(m, p)

  g <- manifest_lesen(p)

  expect_true(isTRUE(manifest_validieren(g)))

  # Feldtreue: run_id/user/zeitstempel/umgebung$git_sha und alle sha256
  # identisch zum Original.
  expect_identical(g$run_id,           m$run_id)
  expect_identical(g$user,             m$user)
  expect_identical(g$zeitstempel,      m$zeitstempel)
  expect_identical(g$umgebung$git_sha, m$umgebung$git_sha)
  expect_identical(g$eingaben$sha256,  m$eingaben$sha256)

  # Typ-Normalisierung nach dem JSON-Roundtrip.
  expect_true(is.integer(g$manifest_version))
  expect_identical(g$manifest_version, 1L)
  expect_true(is.logical(g$umgebung$git_dirty))
  expect_true(is.data.frame(g$eingaben))

  # Numerische parameter bis 1e-12.
  expect_identical(names(g$parameter), names(m$parameter))
  expect_equal(g$parameter$H,        m$parameter$H,        tolerance = 1e-12)
  expect_equal(g$parameter$alpha,    m$parameter$alpha,    tolerance = 1e-12)
  expect_equal(g$parameter$gewichte, m$parameter$gewichte, tolerance = 1e-12)

  # seeds wert- und namensgleich, wieder benannter numerischer Vektor.
  expect_true(is.numeric(g$seeds))
  expect_identical(names(g$seeds), names(m$seeds))
  expect_equal(unname(g$seeds), unname(m$seeds), tolerance = 1e-12)

  # Dokumentierter JSON-Grenzfall: LEERE outputs kommen als
  # 0-Zeilen-data.frame MIT den vier Spalten zurueck.
  expect_true(is.data.frame(g$outputs))
  expect_identical(names(g$outputs), .df_spalten)
  expect_identical(nrow(g$outputs), 0L)

  unlink(c(p, pfade))
})

test_that("manifest: manifest_lesen — fehlende Datei wirft Fehler mit Pfad im Text", {
  fehlt <- tempfile(fileext = ".json")
  expect_false(file.exists(fehlt))
  expect_error(manifest_lesen(fehlt), regexp = fehlt, fixed = TRUE)
})

# ===========================================================================
#  (6) umgebung_erfassen: Formvertrag (GoBD Rz. 152: identifizierbare
#      Umgebung je Lauf) — ohne Annahme ueber renv.lock-Existenz
# ===========================================================================

test_that("manifest: umgebung_erfassen — exakte Felder, r_version-Pin, git_sha/renv_lock_sha256 NA-oder-Hex, git_dirty logical Laenge 1", {
  u <- umgebung_erfassen()

  expect_true(is.list(u))
  expect_identical(names(u), .umg_felder)

  expect_identical(u$r_version, as.character(getRversion()))
  expect_true(is.character(u$plattform) && length(u$plattform) == 1L &&
                nzchar(u$plattform))
  expect_true(is.na(u$git_sha) || grepl("^[0-9a-f]{40}$", u$git_sha))
  expect_true(is.logical(u$git_dirty) && length(u$git_dirty) == 1L)
  expect_true(is.na(u$renv_lock_sha256) ||
                grepl("^[0-9a-f]{64}$", u$renv_lock_sha256))
})

# ===========================================================================
#  (7) RNG-Waechter: kein RNG im GoBD-Pfad
# ===========================================================================

test_that("manifest: RNG-Waechter — kompletter Durchlauf erstellen+schreiben+lesen laesst .Random.seed unveraendert, zweiter Durchlauf identisch", {
  # .Random.seed initialisieren, dann Snapshot.
  set.seed(1)
  invisible(runif(1))
  rng_vor <- .Random.seed

  pfade <- .mk_eingaben()
  p <- tempfile(fileext = ".json")

  m1 <- .mk_manifest(pfade)
  manifest_schreiben(m1, p)
  g <- manifest_lesen(p)
  expect_true(isTRUE(manifest_validieren(g)))

  expect_identical(.Random.seed, rng_vor)       # kein RNG-Verbrauch

  # Zweiter kompletter Durchlauf mit denselben Argumenten -> identisches
  # Manifest (Determinismus des gesamten Pfads).
  m2 <- .mk_manifest(pfade)
  expect_identical(m2, m1)

  unlink(c(p, pfade))
})

# ===========================================================================
#  (8) Review-Fix run_id-Kanon (12.07.2026), Fehlerklasse protokoll v1 —
#      Spec rot-zuerst, Impl folgt in R/manifest.R. Befund: die kanonische
#      Verkettung paste(..., collapse = "|") ist nicht injektiv (Pipe-
#      Zeichen in Tokens verschmelzen Segmentgrenzen), charToRaw haengt
#      ohne enc2utf8 an der impliziten paste-Normalisierung zur NATIVEN
#      Codierung — locale-abhaengig ueber Boxen hinweg, in-box nicht rot
#      beweisbar —, und sort() ohne
#      method = "radix" kollationiert locale-abhaengig.
# ===========================================================================

test_that("run_id-Kanon ist injektiv: verschiedene seeds ergeben verschiedene run_id (Review-Fix, rot-zuerst)", {
  d <- tempfile(); writeBin(charToRaw("abc"), d)
  u <- list(r_version        = as.character(getRversion()),
            plattform        = "test",
            git_sha          = NA_character_,
            git_dirty        = NA,
            renv_lock_sha256 = NA_character_)
  zeit <- as.POSIXct("2026-07-12 10:00:00", tz = "UTC")

  # Gegenbeispiel (handkonstruiert): unter der Pipe-Verkettung erzeugen
  # beide seeds-Vektoren identisches kanon — die Segmentfolge ist jeweils
  # a|1|2|7 (m1: Namen 'a','1' + Werte '2','7'; m2: Name 'a|1|2' + Wert
  # '7'). ROT, bis die Verkettung injektiv ist (Length-Prefix je Token).
  m1 <- manifest_erstellen(eingaben = d, parameter = list(),
                           seeds = setNames(c(2, 7), c("a", "1")),
                           outputs = character(0), zeit = zeit,
                           user = "anker", umgebung = u)
  m2 <- manifest_erstellen(eingaben = d, parameter = list(),
                           seeds = setNames(7, "a|1|2"),
                           outputs = character(0), zeit = zeit,
                           user = "anker", umgebung = u)

  expect_false(identical(m1$run_id, m2$run_id))

  unlink(d)
})

test_that("run_id ist encoding-invariant: latin1- und UTF-8-Repraesentation desselben users ergeben dieselbe run_id (Review-Fix, Regressions-Pin — Mangel cross-platform)", {
  d <- tempfile(); writeBin(charToRaw("abc"), d)
  u <- list(r_version        = as.character(getRversion()),
            plattform        = "test",
            git_sha          = NA_character_,
            git_dirty        = NA,
            renv_lock_sha256 = NA_character_)
  zeit <- as.POSIXct("2026-07-12 10:00:00", tz = "UTC")

  # Derselbe user 'Mueller' mit Umlaut in zwei Repraesentationen: einmal
  # UTF-8-deklariert, einmal via iconv nach latin1 (andere Bytes, gleicher
  # Text). Vorbedingungs-Pin sichert, dass iconv wirklich latin1 deklariert.
  user_utf8 <- "M\u00fcller"
  user_lat1 <- iconv(user_utf8, from = "UTF-8", to = "latin1")
  expect_identical(Encoding(user_lat1), "latin1")

  m_a <- manifest_erstellen(eingaben = d, parameter = list(),
                            seeds = c(sim = 11L),
                            outputs = character(0), zeit = zeit,
                            user = user_utf8, umgebung = u)
  m_b <- manifest_erstellen(eingaben = d, parameter = list(),
                            seeds = c(sim = 11L),
                            outputs = character(0), zeit = zeit,
                            user = user_lat1, umgebung = u)

  # EHRLICH (FAIL-Beweis 12.07.2026): dieser Block ist auf dieser Box
  # bereits GRUEN — paste uebersetzt seine Argumente vor der Verkettung in
  # die native Codierung (hier UTF-8) und heilt den latin1-user implizit.
  # Der Mangel ist die LOCALE-ABHAENGIGKEIT dieser Normalisierung: eine
  # latin1-native Box hasht das Byte 0xFC, eine UTF-8-native Box die Bytes
  # 0xC3 0xBC — gleiche Inputs, verschiedene run_id je Umgebung. In-box ist
  # kein Rot-Fall konstruierbar (Lehre: umgebungsabhaengige Rot-Beweise
  # ehrlich deklarieren). enc2utf8 macht die Normalisierung explizit und
  # locale-fest; der Block bleibt als Regressions-Pin gegen fehlerhafte
  # Normalisierung stehen.
  expect_identical(m_a$run_id, m_b$run_id)

  unlink(d)
})

test_that("run_id ist reihenfolge-invariant in den eingaben (Bestandseigenschaft) — radix-Kollation als deklarierte Hygiene", {
  # EHRLICH: dieser Block ist bereits GRUEN — die Sortierung der Eingabe-
  # Hashes existiert im Bestand. Die Umstellung auf sort(method = "radix")
  # ist Hygiene ohne konstruierbaren Rot-Fall: fuer reine
  # [0-9a-f]-Hexstrings kollationieren reale Locales praktisch identisch.
  # Sie wird hier als Eigenschaft deklariert, nicht rot bewiesen.
  p1 <- tempfile(); writeBin(charToRaw("abc"), p1)
  p2 <- tempfile(); writeBin(charToRaw("GoBD-Anker-2026"), p2)
  u <- list(r_version        = as.character(getRversion()),
            plattform        = "test",
            git_sha          = NA_character_,
            git_dirty        = NA,
            renv_lock_sha256 = NA_character_)
  zeit <- as.POSIXct("2026-07-12 10:00:00", tz = "UTC")

  m_vor  <- manifest_erstellen(eingaben = c(p1, p2), parameter = list(),
                               seeds = c(sim = 11L),
                               outputs = character(0), zeit = zeit,
                               user = "anker", umgebung = u)
  m_rueck <- manifest_erstellen(eingaben = c(p2, p1), parameter = list(),
                                seeds = c(sim = 11L),
                                outputs = character(0), zeit = zeit,
                                user = "anker", umgebung = u)

  expect_identical(m_vor$run_id, m_rueck$run_id)

  unlink(c(p1, p2))
})
