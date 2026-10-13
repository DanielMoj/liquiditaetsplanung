# ============================================================================
#  tests/test_manifest_anbindung.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-12-runmanifest-anbindung
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/manifest_anbindung.R
#  existiert noch nicht -> das ungeschuetzte source() am Kopf bricht ab; jeder
#  Test spezifiziert zusaetzlich das Zielverhalten). Der Builder erfuellt sie
#  erst mit der Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche.
#
#  Vertrag (handoff/phase-12-runmanifest-anbindung.json, GoBD Rz. 100/152):
#   (1) Schema-Erweiterung manifest_erstellen um kontrollen = NULL ANS ENDE
#       der Signatur: NULL liefert das BIT-IDENTISCHE v1-Objekt (Regressionspin
#       gegen einen Referenzaufruf OHNE kontrollen-Argument); ein gueltiges
#       kontrollen-df (Spalten name/bestanden/soll/ist/details, >= 1 Zeile)
#       liefert manifest_version 2L mit ZEHN Feldern (kontrollen nach outputs)
#       und besteht manifest_validieren; Mutilationen wirft der Validator mit
#       dem Feldnamen im Text; Roundtrip schreiben/lesen ist feldtreu
#       INKLUSIVE kontrollen; die run_id ist INVARIANT gegenueber outputs UND
#       kontrollen (Negativ-Pin gegen Drift der kanonischen Verkettung).
#   (2) Whitelist HART: scenario_from_workbook stoppt bei unbekannten
#       assume_overrides (statt der bisherigen warning()); der Fehlertext
#       nennt den unbekannten Namen UND die gueltige Whitelist. Positivpfad
#       als Regime-Guard mit NICHT-trivialem Wert (Lehre: Identitaeten nie
#       nur am trivialen Punkt): menge = -0.30 wird uebernommen und ist
#       ungleich dem Mappen-Default.
#   (3) simulation_mit_manifest(sc, runs_dir, zeit, user, plot): EIN Lauf
#       end-to-end auf der Tier-1-Mappe (S = 40, Seed 2026, plot = FALSE,
#       injizierte zeit/user, frisches runs_dir in tempdir()): runs_dir/
#       <run_id>/ mit byte-identischer Mappen-Kopie (zweikanaliger
#       Hash-Vergleich), endbestand.rds-Kopie im outputs-df, write-once-
#       Manifest, exakt vier IKS-Kontrollen alle bestanden, anker_plan_
#       tiefpunkt String-identisch soll == ist mit Tier-1-Bindung; ein
#       ZWEITER Lauf mit identischer zeit/user/Mappe/Seed scheitert benannt
#       und laesst das Erst-Manifest byte-identisch.
#   (4) Beobachter-Invarianz (Blockprinzip c17d29a): das Hook-Ergebnis ist
#       identical zum reinen phase_simulation-Lauf bis auf GENAU EIN
#       Beobachterfeld gobd; die drei Tier-1-Anker halten bei Toleranz 0.
#
#  Orakel-Politik: Tier-1-Anker AUSSCHLIESSLICH ueber das gesourcte
#  tests/tier1_baseline.R (KEINE neuen Zahlen-Literale, Bindung wie
#  tests/run_all.R: round(...) == TIER1_BASELINE$...). Die Schema-Tests aus
#  Block (1) arbeiten mit Tempdateien OHNE Engine-Lauf (Basisargumente wie
#  tests/test_manifest.R). Laufzeit klein: genau EIN Hook-Lauf, EIN
#  Zweitlauf-Versuch, EIN Referenzlauf phase_simulation, alle mit S = 40
#  (Praezedenz-Idiom tests/test_phase_sim_breach.R: der Konstruktor laesst
#  sim ueberschreiben).
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_forecast.R"))
source(file.path(.root, "R", "phase_simulation.R"))
source(file.path(.root, "R", "manifest.R"))
source(file.path(.root, "tests", "tier1_baseline.R"))
source(file.path(.root, "R", "manifest_anbindung.R"))  # <- existiert noch nicht: rot zuerst

WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Basisargumente fuer die Schema-Tests aus Block (1) — GLEICHE Tempdatei-
# Eingaben/seeds/zeit/user/umgebung wie im Kern-Test (tests/test_manifest.R),
# damit der v1-Regressionspin dieselbe Basis vergleicht. Kein Engine-Lauf.
# ---------------------------------------------------------------------------
.anb_zeit   <- as.POSIXct("2026-07-08 10:34:56", tz = "UTC")
.anb_params <- list(H = 4L, alpha = 0.25, gewichte = c(0.5, 0.25, 0.25))

.anb_felder_v1 <- c("manifest_version", "run_id", "zeitstempel", "user",
                    "umgebung", "eingaben", "parameter", "seeds", "outputs")
.anb_k_spalten <- c("name", "bestanden", "soll", "ist", "details")

.anb_eingaben <- function() {
  a <- tempfile(); writeBin(charToRaw("abc"), a)
  g <- tempfile(); writeBin(charToRaw("GoBD-Anker-2026"), g)
  c(a, g)
}

.anb_umgebung <- function() {
  list(r_version        = as.character(getRversion()),
       plattform        = "testplattform",
       git_sha          = NA_character_,
       git_dirty        = NA,
       renv_lock_sha256 = NA_character_)
}

# Gueltiges kontrollen-df nach dem Vertrag (vier Zeilen; Inhalte fuer die
# Schema-Tests neutral — die ECHTEN Kontrollwerte prueft Block 3 am Hook).
.anb_kontrollen <- function() {
  data.frame(
    name      = c("anker_plan_tiefpunkt", "massenerhaltung",
                  "gv_konsistenz", "treiber_whitelist"),
    bestanden = rep(TRUE, 4L),
    soll      = c("0", "TRUE", "TRUE", "TRUE"),
    ist       = c("0", "TRUE", "TRUE", "TRUE"),
    details   = c("phase_forecast$kpis$trough vs metrics$plan_trough",
                  "phase_forecast$diagnostics$massenerhaltung",
                  "phase_forecast$diagnostics$gv_konsistenz",
                  "whitelist"),
    stringsAsFactors = FALSE)
}

# Aufruf-Helfer: feste Basisargumente, alles Weitere (outputs, kontrollen)
# per ... — der v1-Referenzaufruf laesst kontrollen KOMPLETT weg.
.anb_mk <- function(pfade, ...) {
  manifest_erstellen(eingaben = pfade, parameter = .anb_params,
                     seeds = c(sim = 11L, boot = 23L),
                     zeit = .anb_zeit, user = "testuser",
                     umgebung = .anb_umgebung(), ...)
}

# ===========================================================================
#  (1) Schema-Erweiterung: kontrollen-Argument von manifest_erstellen
# ===========================================================================

test_that("anbindung: manifest_erstellen mit kontrollen = NULL ist identical zum bisherigen v1-Objekt (Regressionspin)", {
  pfade <- .anb_eingaben()

  m_ref  <- .anb_mk(pfade)                     # Referenz OHNE kontrollen-Argument
  m_null <- .anb_mk(pfade, kontrollen = NULL)  # neues Argument, Default-Verhalten

  expect_identical(m_null, m_ref)
  expect_identical(m_null$manifest_version, 1L)
  expect_identical(names(m_null), .anb_felder_v1)

  unlink(pfade)
})

test_that("anbindung: manifest_erstellen mit gueltigem kontrollen-df — manifest_version 2L, zehn Felder mit kontrollen nach outputs, Validator besteht", {
  pfade <- .anb_eingaben()
  k <- .anb_kontrollen()

  m2 <- .anb_mk(pfade, kontrollen = k)

  expect_identical(m2$manifest_version, 2L)
  # ZEHNTES Top-Level-Feld kontrollen ANS ENDE der Namensfolge (nach outputs).
  expect_identical(names(m2), c(.anb_felder_v1, "kontrollen"))
  expect_true(isTRUE(manifest_validieren(m2)))

  # kontrollen-Vertrag: data.frame mit exakt den fuenf Spalten, spaltenweise
  # identisch zur Eingabe (der Kern verbiegt die Kontrollzeilen nicht).
  expect_true(is.data.frame(m2$kontrollen))
  expect_identical(names(m2$kontrollen), .anb_k_spalten)
  expect_identical(nrow(m2$kontrollen), 4L)
  expect_identical(m2$kontrollen$name,      k$name)
  expect_identical(m2$kontrollen$bestanden, k$bestanden)
  expect_identical(m2$kontrollen$soll,      k$soll)
  expect_identical(m2$kontrollen$ist,       k$ist)
  expect_identical(m2$kontrollen$details,   k$details)

  unlink(pfade)
})

test_that("anbindung: manifest_validieren v2 — vier kontrollen-Mutilationen je von FRISCHER Kopie mit Feldname im Fehlertext", {
  pfade <- .anb_eingaben()
  basis <- .anb_mk(pfade, kontrollen = .anb_kontrollen())

  expect_true(isTRUE(manifest_validieren(basis)))

  # (a) bestanden NA (logical ohne NA gefordert).
  v <- basis; v$kontrollen$bestanden[1] <- NA
  expect_error(manifest_validieren(v), regexp = "bestanden")

  # (b) name doppelt (Eindeutigkeit gefordert).
  v <- basis; v$kontrollen$name[2] <- v$kontrollen$name[1]
  expect_error(manifest_validieren(v), regexp = "name")

  # (c) Spalte details fehlt (exakter Spaltenvertrag).
  v <- basis; v$kontrollen$details <- NULL
  expect_error(manifest_validieren(v), regexp = "details")

  # (d) 0 Zeilen (mindestens 1 Zeile gefordert).
  v <- basis; v$kontrollen <- basis$kontrollen[0, ]
  expect_error(manifest_validieren(v), regexp = "kontrollen")

  unlink(pfade)
})

test_that("anbindung: Roundtrip manifest_schreiben/manifest_lesen ist feldtreu INKLUSIVE kontrollen", {
  pfade <- .anb_eingaben()
  k  <- .anb_kontrollen()
  m2 <- .anb_mk(pfade, kontrollen = k)
  p  <- tempfile(fileext = ".json")

  manifest_schreiben(m2, p)
  g <- manifest_lesen(p)

  expect_true(isTRUE(manifest_validieren(g)))

  # manifest_version wieder integer, Feldnamen unveraendert.
  expect_true(is.integer(g$manifest_version))
  expect_identical(g$manifest_version, 2L)
  expect_identical(names(g), c(.anb_felder_v1, "kontrollen"))
  expect_identical(g$run_id, m2$run_id)

  # kontrollen wieder als data.frame mit den fuenf Spalten; bestanden wieder
  # logical, name/soll/ist/details character-identisch zum Original.
  expect_true(is.data.frame(g$kontrollen))
  expect_identical(names(g$kontrollen), .anb_k_spalten)
  expect_true(is.logical(g$kontrollen$bestanden))
  expect_identical(g$kontrollen$bestanden, k$bestanden)
  expect_identical(g$kontrollen$name,      k$name)
  expect_identical(g$kontrollen$soll,      k$soll)
  expect_identical(g$kontrollen$ist,       k$ist)
  expect_identical(g$kontrollen$details,   k$details)

  unlink(c(p, pfade))
})

test_that("anbindung: run_id-Invarianz gegenueber outputs UND kontrollen (Negativ-Pin gegen Drift der kanonischen Verkettung)", {
  pfade <- .anb_eingaben()
  ausg  <- tempfile(); writeBin(charToRaw("output-bytes"), ausg)

  # Identische eingaben/seeds/zeit/user in allen vier Varianten.
  m_o0 <- .anb_mk(pfade, outputs = character(0))
  m_o1 <- .anb_mk(pfade, outputs = ausg)
  m_k0 <- .anb_mk(pfade, kontrollen = NULL)
  m_k1 <- .anb_mk(pfade, kontrollen = .anb_kontrollen())

  expect_identical(m_o1$run_id, m_o0$run_id)
  expect_identical(m_k0$run_id, m_o0$run_id)
  expect_identical(m_k1$run_id, m_o0$run_id)

  unlink(c(pfade, ausg))
})

# ===========================================================================
#  (2) Treiber-Whitelist HART (Rz. 100): stop() statt verpuffender warning()
# ===========================================================================

test_that("anbindung: scenario_from_workbook — unbekannter Override 'mengge' stoppt mit Tippfehler UND gueltigem Treiber im Fehlertext", {
  err <- tryCatch(
    scenario_from_workbook(WB, assume_overrides = list(mengge = -0.10)),
    error = function(e) e)

  expect_true(inherits(err, "error"))
  msg <- conditionMessage(err)
  # Der unbekannte Name steht im Text ...
  expect_match(msg, "mengge", fixed = TRUE)
  # ... und die gueltige Whitelist wird genannt ('menge' ist KEIN Teilstring
  # von 'mengge' — der Treffer kann nur aus der Whitelist kommen).
  expect_match(msg, "menge", fixed = TRUE)
})

test_that("anbindung: scenario_from_workbook — Regime-Guard Positivpfad: menge = -0.30 wird uebernommen und ist ungleich dem Mappen-Default", {
  # NICHT-trivialer Wert (Lehre: Identitaeten nie nur am trivialen Punkt):
  # der Mappen-Default darf nicht zufaellig schon -0.30 sein.
  menge_default <- load_model(WB)$assume$menge
  expect_false(isTRUE(all.equal(menge_default, -0.30)))

  sc_pos <- scenario_from_workbook(WB, name = "WhitelistPositiv",
                                   assume_overrides = list(menge = -0.30))
  expect_identical(sc_pos$assume$menge, -0.30)
  expect_identical(sc_pos$mdl$assume$menge, -0.30)
})

# ===========================================================================
#  Gemeinsamer Hook-Lauf fuer die Bloecke (3) und (4) — genau EIN Lauf
#  (Laufzeit): S = 40, Seed 2026 (Szenario-Schluessel), plot = FALSE,
#  injizierte zeit (POSIXct UTC) und user, frisches runs_dir in tempdir().
# ===========================================================================

.anb_rd <- file.path(tempdir(), "runs-anbindung-spec")
unlink(.anb_rd, recursive = TRUE)
.anb_zeit_hook <- as.POSIXct("2026-07-09 08:00:00", tz = "UTC")

.anb_sc_hook <- scenario_from_workbook(WB, name = "GoBDAnbindung",
                                       sim = list(paths = 40L, seed = 2026))
.anb_out <- simulation_mit_manifest(.anb_sc_hook, runs_dir = .anb_rd,
                                    zeit = .anb_zeit_hook, user = "testuser",
                                    plot = FALSE)

# ===========================================================================
#  (3) Hook end-to-end auf der Tier-1-Mappe
# ===========================================================================

test_that("anbindung: Hook end-to-end — runs/<run_id>/ mit Kopien, valides Manifest, vier IKS-Kontrollen bestanden, Tier-1-Anker-Zeile", {
  # GENAU EIN Beobachterfeld gobd mit run_id/manifest_pfad/kontrollen.
  expect_true(is.list(.anb_out$gobd))
  expect_identical(names(.anb_out$gobd), c("run_id", "manifest_pfad", "kontrollen"))

  run_id <- .anb_out$gobd$run_id
  # run_id-Regex MIT dem injizierten UTC-Zeitanteil (2026-07-09 08:00:00Z).
  expect_match(run_id, "^run-20260709T080000Z-[0-9a-f]{8}$")
  expect_true(dir.exists(file.path(.anb_rd, run_id)))

  # Mappen-Kopie byte-identisch, ZWEIKANALIG: sha256 der Kopie EXTERN
  # nachgerechnet == Manifest-eingaben-Eintrag == sha256_datei(Original).
  kopie_wb <- file.path(.anb_rd, run_id, basename(WB))
  expect_true(file.exists(kopie_wb))
  h_kopie <- sha256_datei(kopie_wb)
  h_orig  <- sha256_datei(WB)
  expect_identical(h_kopie, h_orig)

  mp <- .anb_out$gobd$manifest_pfad
  expect_true(file.exists(mp))
  m_e2e <- manifest_lesen(mp)
  expect_true(isTRUE(manifest_validieren(m_e2e)))
  expect_identical(m_e2e$manifest_version, 2L)
  expect_identical(m_e2e$run_id, run_id)

  ein <- m_e2e$eingaben[m_e2e$eingaben$name == basename(WB), ]
  expect_identical(nrow(ein), 1L)
  expect_identical(ein$sha256, h_kopie)

  # endbestand.rds-Kopie steht im outputs-df mit nachgerechnetem Hash.
  kopie_rds <- file.path(.anb_rd, run_id, "endbestand.rds")
  expect_true(file.exists(kopie_rds))
  orow <- m_e2e$outputs[m_e2e$outputs$name == "endbestand.rds", ]
  expect_identical(nrow(orow), 1L)
  expect_identical(orow$sha256, sha256_datei(kopie_rds))

  # EXAKT die vier gepinnten Kontrollnamen, alle bestanden — im
  # Beobachterfeld UND im persistierten Manifest.
  k_namen <- c("anker_plan_tiefpunkt", "massenerhaltung",
               "gv_konsistenz", "treiber_whitelist")
  k <- .anb_out$gobd$kontrollen
  expect_true(is.data.frame(k))
  expect_identical(nrow(k), 4L)
  expect_true(setequal(k$name, k_namen))
  expect_true(all(k$bestanden))
  expect_identical(nrow(m_e2e$kontrollen), 4L)
  expect_true(setequal(m_e2e$kontrollen$name, k_namen))
  expect_true(all(m_e2e$kontrollen$bestanden))

  # anker_plan_tiefpunkt: ist == soll STRING-identisch (zwei unabhaengige
  # Wege durch denselben deterministischen Kern), und die Tier-1-Bindung
  # wie in tests/run_all.R — KEIN neues Zahlen-Literal, Toleranz 0.
  az <- k[k$name == "anker_plan_tiefpunkt", ]
  expect_identical(nrow(az), 1L)
  expect_identical(az$ist, az$soll)
  expect_true(isTRUE(round(as.numeric(az$ist)) == TIER1_BASELINE$trough))
})

test_that("anbindung: Zweitlauf mit identischer zeit/user/Mappe/Seed scheitert benannt und laesst das Erst-Manifest byte-identisch", {
  mp <- .anb_out$gobd$manifest_pfad
  b1 <- readBin(mp, "raw", n = file.info(mp)$size)

  sc_zweit <- scenario_from_workbook(WB, name = "GoBDAnbindungZweit",
                                     sim = list(paths = 40L, seed = 2026))
  err <- tryCatch({
    simulation_mit_manifest(sc_zweit, runs_dir = .anb_rd,
                            zeit = .anb_zeit_hook, user = "testuser",
                            plot = FALSE)
    NULL
  }, error = function(e) e)

  # Benannter Fehler MIT dem Pfad im Text (die run_id ist der eindeutige
  # Bestandteil des Run-Verzeichnispfads, separatorunabhaengig pruefbar).
  expect_true(inherits(err, "error"))
  expect_match(conditionMessage(err), .anb_out$gobd$run_id, fixed = TRUE)

  # readBin-Vollvergleich vorher/nachher: NICHTS wurde ueberschrieben.
  b2 <- readBin(mp, "raw", n = file.info(mp)$size)
  expect_identical(length(b2), length(b1))
  expect_identical(b2, b1)
})

# ===========================================================================
#  (4) Beobachter-Invarianz (Blockprinzip c17d29a) + Tier-1-Anker
# ===========================================================================

test_that("anbindung: Beobachter-Invarianz — Hook-Ergebnis identical zum reinen phase_simulation-Lauf bis auf gobd; drei Tier-1-Anker halten", {
  # Referenzlauf an einem frischen Szenario GLEICHEN NAMENS derselben Mappe/
  # desselben Seeds (gleicher Name -> scenario/workspace/endbestand_path
  # stringgleich, identical scheitert nicht an Pfadfeldern).
  sc_ref  <- scenario_from_workbook(WB, name = "GoBDAnbindung",
                                    sim = list(paths = 40L, seed = 2026))
  out_ref <- phase_simulation(sc_ref, plot = FALSE)

  # B/Theta/fan/trough/metrics bitgleich — der Hook verbraucht keinen RNG
  # vor der epistemischen Ziehung.
  expect_identical(.anb_out[names(out_ref)], out_ref)
  # GENAU EIN zusaetzliches Beobachterfeld, ans Ende ergaenzt.
  expect_identical(names(.anb_out), c(names(out_ref), "gobd"))

  # Drei Tier-1-Anker bei Toleranz 0, gebunden wie in tests/run_all.R
  # (TIER1_BASELINE aus dem gesourcten tests/tier1_baseline.R, KEINE neuen
  # Zahlen-Literale).
  mdl_t1 <- load_model(WB)
  expect_true(isTRUE(round(validate_history(mdl_t1)$erg_eng) == TIER1_BASELINE$history))
  expect_true(isTRUE(round(.anb_out$metrics$plan_trough)    == TIER1_BASELINE$trough))
  expect_true(isTRUE(round(run_model(mdl_t1)$kpis$result)   == TIER1_BASELINE$result))
})

# ===========================================================================
#  (5) TREIBER-WHITELIST ECHT (Review-Nachholung)
#
#  Befund High (.orch/review-pakete/befund-paket-phase-12-runmanifest-
#  anbindung.md, manifest_anbindung.R:67): die kontrollen-Zeile
#  treiber_whitelist ist hart TRUE mit tautologischen details — sie
#  protokolliert keine echte Pruefung. Zielsemantik (Betreiber-Entscheid):
#    bestanden = (setdiff(names(sc$assume), names(sc$mdl$assume)) ist leer)
#    soll      = paste(names(sc$mdl$assume), collapse = ", ")   (Whitelist)
#    ist       = paste(names(sc$assume),     collapse = ", ")   (angewandt)
#    details   = "angewandt: name=wert, ..." + bei Fremdschluesseln
#                " | fremd: <namen>"
#  Ehrlich FALSE protokollieren, NICHT stoppen: das Manifest wird weiterhin
#  geschrieben und bleibt valide (die harte Durchsetzung sitzt in
#  scenario_from_workbook; die Zeile ist Dokumentationsschicht, konsistent
#  zu den drei anderen Kontrollzeilen).
#
#  ROT ZUERST: gegen den aktuellen Stand (soll/ist hart "TRUE") MUESSEN
#  diese Tests fehlschlagen; der Builder erfuellt sie erst mit der echten
#  Rechnung. R/manifest_anbindung.R bleibt in diesem Schritt unveraendert.
#
#  Rechenneutralitaet des Fremdschluessel-Angriffs (vorab verifiziert):
#  phase_forecast und phase_simulation konsumieren ausschliesslich sc$mdl/
#  sc$sim/sc$workspace; sc$assume ist der Konstruktor-Schnappschuss und
#  fliesst in R/manifest_anbindung.R nur in parameter und die Kontrollzeile.
# ===========================================================================

test_that("anbindung: treiber_whitelist ECHT — Positivpfad am Hook-Lauf traegt Namenslisten in soll/ist, nicht die alte Tautologie", {
  wz <- .anb_out$gobd$kontrollen[.anb_out$gobd$kontrollen$name == "treiber_whitelist", ]
  expect_identical(nrow(wz), 1L)
  expect_true(wz$bestanden)

  # soll = Modell-Whitelist, ist = angewandte Namen — beide als komma-
  # separierte Namenslisten aus dem Hook-Szenario, KEINE neuen Literale.
  expect_identical(wz$soll, paste(names(.anb_sc_hook$mdl$assume), collapse = ", "))
  expect_identical(wz$ist,  paste(names(.anb_sc_hook$assume),     collapse = ", "))
  expect_identical(wz$soll, wz$ist)  # Positivpfad: angewandt == Whitelist

  # Riegel gegen die alte Tautologie: v1 trug hier hart "TRUE"/"TRUE".
  expect_false(identical(wz$soll, "TRUE"))
})

test_that("anbindung: treiber_whitelist ECHT — Fremdschluessel wird ehrlich FALSE protokolliert, ohne zu stoppen; Manifest bleibt valide", {
  # Minimaler Nachbau des Befund-Angriffs: ein manuell konstruiertes
  # liqui_scenario, dessen assume-Schnappschuss NACH dem Konstruktor einen
  # Fremdschluessel erhaelt (umgeht die harte scenario_from_workbook-
  # Pruefung). Numerischer Wert, rechenneutral: der Rechenpfad konsumiert
  # sc$mdl (siehe Sektionskopf).
  rd_f <- file.path(tempdir(), "runs-anbindung-whitelist")
  unlink(rd_f, recursive = TRUE)
  # Eigene injizierte zeit, bewusst VERSCHIEDEN von .anb_zeit_hook —
  # kein write-once-Konflikt mit dem Hook-Lauf.
  zeit_f <- as.POSIXct("2026-07-09 09:00:00", tz = "UTC")

  sc_f <- scenario_from_workbook(WB, name = "WhitelistFremd",
                                 sim = list(paths = 40L, seed = 2026))
  sc_f$assume[["fremdtreiber"]] <- 1

  # Darf NICHT stoppen: ehrliches Protokoll statt harter Abbruch.
  out_f <- simulation_mit_manifest(sc_f, runs_dir = rd_f,
                                   zeit = zeit_f, user = "testuser",
                                   plot = FALSE)

  kf <- out_f$gobd$kontrollen
  wf <- kf[kf$name == "treiber_whitelist", ]
  expect_false(wf$bestanden)                                      # ehrliches FALSE
  expect_true(all(kf$bestanden[kf$name != "treiber_whitelist"]))  # die drei anderen unberuehrt TRUE

  expect_match(wf$ist, "fremdtreiber", fixed = TRUE)              # der Fremde steht im ist
  expect_false(grepl("fremdtreiber", wf$soll, fixed = TRUE))      # aber nicht im soll
  expect_match(wf$details, "fremd", fixed = TRUE)                 # und ist in details benannt

  # Manifest trotz FALSE geschrieben und valide (bestanden muss nur
  # logical ohne NA sein — der Validator erzwingt kein all-TRUE).
  mf <- manifest_lesen(out_f$gobd$manifest_pfad)
  expect_true(isTRUE(manifest_validieren(mf)))
  expect_false(all(mf$kontrollen$bestanden))

  unlink(rd_f, recursive = TRUE)
})
