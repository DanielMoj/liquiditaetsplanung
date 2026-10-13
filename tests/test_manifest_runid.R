# ============================================================================
#  tests/test_manifest_runid.R  —  run_id-Herleitungs-Orakel fuer
#  phase-12-runmanifest-kern (Mutgate-Nachschub Runde 2)
#
#  Zweck: retrograde Nachrechenbarkeit der run_id (GoBD Rz. 152: ein Pruefer
#  muss die Lauf-Identitaet aus dokumentierten Regeln UNABHAENGIG nachrechnen
#  koennen; Vorgriff auf phase-13-pruefbarkeit). Gezielter Kill der
#  Verkettungs-Mutantenklasse: M1 (substr-Fenster 2-9 statt 1-8), M5
#  (Seed-Namen und Seed-Werte in der kanonischen Verkettung vertauscht) und
#  die Rueckfall-Klasse "Length-Prefix entfernt / Pipe-Verkettung
#  restauriert" sind nur ueber die exakte Herleitungsformel beobachtbar.
#
#  Formel-Quelle: die KOMPONENTEN und ihre Reihenfolge stammen aus dem
#  Handoff-Wortlaut (handoff/phase-12-runmanifest-kern.json, zweck von
#  manifest_erstellen): run_id = 'run-' + UTC-Zeitanteil + '-' + erste
#  8 Hexzeichen von sha256 ueber die kanonische Verkettung aus sortierten
#  eingaben-Hashes, Seed-Namen, Seed-Werten und user. Die VERKETTUNGSART
#  stammt aus der Kanon-Praezisierung des Review-Fixes 12.07.2026
#  (dokumentiert im Kopf von R/manifest.R): je Token Laengen-Praefix "n:"
#  (Dezimal, Bytelaenge der UTF-8-Repraesentation) vor den enc2utf8-
#  normalisierten Bytes, Eingabe-Hashes radix-sortiert.
#
#  Ehrlicher Vermerk: der fruehere Pin dieser Datei hatte das Pipe-
#  Trennzeichen '|' als angeblichen Handoff-Wortlaut gepinnt — tatsaechlich
#  nennt der Handoff nur die Komponenten, das Pipe-Zeichen war abgelesenes
#  v1-Implementierungsdetail. Die Nachfuehrung folgt dem freigegebenen
#  Kanon-Wechsel (Spec: tests/test_manifest.R Abschnitt (8), Commit
#  bac8079); Suffixe identischer Inputs aendern sich dadurch (Rot-Beweis:
#  run_all-FAIL 12.07.2026, erwartet f028340b, ist d3b60c5f).
#
#  Orakel-Politik: digest ist hier zulaessiges Orakel-Primitiv, weil seine
#  sha256-Korrektheit in tests/test_manifest.R separat gegen NIST-Vektoren
#  gepinnt ist — geprueft wird die KOMPOSITION (Sortierung, Reihenfolge,
#  Length-Prefix, substr-Fenster), unabhaengig ausserhalb der getesteten
#  Funktion rekonstruiert. Die Token-Bytes werden im Test EIGENSTAENDIG
#  gebaut; .manifest_kanon_bytes wird NIE aufgerufen (Selbstkonsistenz
#  haette keine Mutationskraft). Datei-Hashes sind woertliche LITERALE
#  (NIST FIPS 180-4 fuer 'abc'; 'GoBD-Anker-2026' extern via sha256sum
#  gepinnt). Keine Anker-/Tier-1-Literale, kein skip, kein succeed.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "manifest.R"))

test_that("manifest: run_id-Suffix ist aus dem Kanon unabhaengig nachrechenbar", {
  # Gepinnte Hash-Orakel (LITERALE): 'abc' -> NIST FIPS 180-4;
  # 'GoBD-Anker-2026' (15 Bytes) -> unabhaengig ausserhalb des Repos via
  # sha256sum berechnet.
  HASH_ABC  <- "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  HASH_GOBD <- "d8b24ce6fd6db563b2ee90c5f2c59ece7fb98c833515a53941bec5fd3bff2e0d"

  p_abc  <- tempfile(); writeBin(charToRaw("abc"), p_abc)
  p_gobd <- tempfile(); writeBin(charToRaw("GoBD-Anker-2026"), p_gobd)

  # Handgebaute, feste Umgebung — INJIZIERT wie in tests/test_manifest.R,
  # damit der reine Kern deterministisch testbar ist.
  umg <- list(r_version        = as.character(getRversion()),
              plattform        = "testplattform",
              git_sha          = NA_character_,
              git_dirty        = NA,
              renv_lock_sha256 = NA_character_)

  m <- manifest_erstellen(eingaben  = c(p_abc, p_gobd),
                          parameter = list(),
                          seeds     = c(sim = 11L, mc = 7L),
                          outputs   = character(0),
                          zeit      = as.POSIXct("2026-07-08 10:34:56",
                                                 tz = "UTC"),
                          user      = "orakel",
                          umgebung  = umg)

  # Testlokaler Token-Bauer mit BEWUSST ANDERER Bauweise als der Produktiv-
  # Helfer: sprintf/unlist statt paste0/do.call — absichtlich anderer
  # Bauweg, gleiches Schema ("n:" + UTF-8-Bytes je Token).
  tok_bytes <- function(x) {
    b <- charToRaw(enc2utf8(x))
    c(charToRaw(sprintf("%d:", length(b))), b)
  }

  # Token-Reihenfolge als LITERALE ohne sort() im Orakel: HASH_ABC ("ba78…")
  # VOR HASH_GOBD ("d8b2…") — erste Hexziffer 'b' < 'd', Byte-Ordnung
  # haendisch geprueft, kein sort im Orakel. Danach Seed-Namen, Seed-Werte,
  # user — exakt die Aufrufreihenfolge des manifest_erstellen-Aufrufs oben.
  tokens <- c(HASH_ABC, HASH_GOBD,
              c("sim", "mc"),
              sprintf("%.0f", c(11, 7)),
              "orakel")
  kanon_bytes_erwartet <- unlist(lapply(tokens, tok_bytes),
                                 use.names = FALSE)
  suffix_erwartet <- substr(digest::digest(kanon_bytes_erwartet,
                                           algo = "sha256",
                                           serialize = FALSE), 1, 8)

  # Voll-Pin: Zeitanteil handgerechnet (UTC-Eingabe bleibt unverschoben),
  # Suffix unabhaengig nachgerechnet.
  expect_identical(m$run_id,
                   paste0("run-20260708T103456Z-", suffix_erwartet))
  expect_identical(nchar(suffix_erwartet), 8L)

  # Negativ-Pin gegen die substr-Verschiebung (Fenster 2-9 statt 1-8, M1).
  expect_false(identical(suffix_erwartet,
                         substr(digest::digest(kanon_bytes_erwartet,
                                               algo = "sha256",
                                               serialize = FALSE), 2, 9)))

  # Negativ-Pin gegen die Namen/Werte-Vertauschung (M5): Verkettung mit
  # Seed-WERTEN vor den Seed-NAMEN darf NICHT dieselbe run_id ergeben.
  tokens_vertauscht <- c(HASH_ABC, HASH_GOBD,
                         sprintf("%.0f", c(11, 7)),
                         c("sim", "mc"),
                         "orakel")
  bytes_vertauscht <- unlist(lapply(tokens_vertauscht, tok_bytes),
                             use.names = FALSE)
  expect_false(identical(m$run_id,
                         paste0("run-20260708T103456Z-",
                                substr(digest::digest(bytes_vertauscht,
                                                      algo = "sha256",
                                                      serialize = FALSE),
                                       1, 8))))

  # Negativ-Pin gegen den Pipe-Rueckfall: die ALTE v1-Formel woertlich
  # nachgerechnet (Pipe-Verkettung derselben Tokens statt Length-Prefix) —
  # killt die Mutantenklasse "Length-Prefix entfernt / Pipe-Verkettung
  # restauriert". Der v1-Suffix ist der dokumentierte Rot-Beweis-Wert
  # (Herkunft: run_all-Log 12.07.2026) und wird als Zusatz-Pin festgehalten.
  kanon_v1 <- paste(tokens, collapse = "|")
  suffix_v1 <- substr(digest::digest(charToRaw(kanon_v1),
                                     algo = "sha256",
                                     serialize = FALSE), 1, 8)
  expect_identical(suffix_v1, "f028340b")
  expect_false(identical(m$run_id,
                         paste0("run-20260708T103456Z-", suffix_v1)))

  unlink(c(p_abc, p_gobd))
})
