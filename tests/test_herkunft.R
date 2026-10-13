# ============================================================================
#  tests/test_herkunft.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-13-pruefbarkeit
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: das ungeschuetzte
#  source() von R/herkunft.R am Kopf bricht ab, solange die Datei fehlt; und
#  der herkunft-Zweig von export_xlsx/docx/pdf (das dritte Argument) existiert
#  noch nicht. Der Builder erfuellt sie erst mit der Implementierung. Kein
#  skip(), kein succeed(), keine Existenz-Weiche, keine zweite Monte-Carlo
#  (phase_forecast ist deterministisch: validate_history + run_model, kein MC).
#
#  Vertrag (handoff/phase-13-pruefbarkeit.json; GoBD Rz. 152/154 —
#  Nachvollziehbarkeit/Pruefbarkeit jeder Ergebnisgroesse; vorlagenfrei):
#   - herkunft_bauen(forecast, manifest_pfad): rein, deterministisch, KEIN
#     RNG, kein Sys.time(), keine Engine, kein Grafikgeraet. Liefert
#     list(stempel, herkunft) und liest NUR (a) die drei Tier-1-Anker aus dem
#     UEBERGEBENEN forecast (Phase-1-Vertrag): Historie =
#     forecast$validation$erg_eng, Tiefpunkt = forecast$kpis$trough,
#     Planergebnis = forecast$kpis$result; und (b) den Lauf-Nachweis aus
#     manifest_lesen(manifest_pfad) PLUS den EXTERN nachgerechneten
#     sha256_datei(manifest_pfad).
#       * stempel: Key/Wert-data.frame (Spalten kennzahl/wert, alles
#         Zeichenketten) mit GENAU den sechs Stempeln in Vertragsreihenfolge
#         run_id / manifest_sha256 / git_sha / renv_lock_sha256 / r_version /
#         zeitstempel.
#       * herkunft: data.frame (Spalten kennzahl/wert/run_id/manifest_sha256/
#         blatt/zelle; wert numerisch, uebrige Spalten Zeichenketten) mit
#         GENAU drei Zeilen in Reihenfolge 'Historie (Tier-1)' /
#         'Tiefpunkt M3 (Tier-1)' / 'Planergebnis (Tier-1)'.
#   - KEIN HASH PRUEFT SICH SELBST (Blockprinzip): manifest_sha256 steht NICHT
#     im Manifest (das kann er zirkulaer nicht) — er wird ueber die rohen
#     Bytes der Datei gerechnet und ist unabhaengig nachpruefbar; run_id
#     stammt aus dem Manifest.
#   - export_xlsx/docx/pdf: ERWEITERUNG um herkunft = NULL ANS ENDE der
#     Signatur. herkunft = NULL -> bit-identisch wie heute (der REGRESSIONSPIN
#     der Schwester-Tests test_export_xlsx/docx/pdf.R haelt); herkunft =
#     list(stempel, herkunft) haengt ZWEI Beobachter-Blaetter/-Abschnitte
#     'GoBD_Stempel' + 'Herkunft' ANS ENDE, NACH den Datenblaettern, OHNE die
#     Datenblaetter zu beruehren (Beobachter-Invarianz).
#
#  Orakel-Politik: das Binding-Orakel ist EINGEFROREN und UNABHAENGIG (Lehre:
#  nie beide Seiten aus demselben Code) — die drei herkunft$wert werden gegen
#  die TIER1_BASELINE-Literale (tests/tier1_baseline.R) geprueft, NICHT gegen
#  forecast; ein Vertauschen trough<->result oder ein Binden ans falsche
#  forecast-Feld faellt am Golden Master auf (die drei Anker sind paarweise
#  verschieden). Der manifest_sha256-Stempel wird EXTERN ueber die Datei-Bytes
#  nachgerechnet (kein Selbstbezug). Die Fixture-Naht ist EIN deterministischer
#  Tier-1-Forecast auf der echten Mappe WB plus ein deterministisch gebautes
#  Minimal-Manifest v1 (manifest_erstellen + manifest_schreiben mit injizierter
#  POSIXct-zeit/user/umgebung, keine Engine im Manifest-Teil), lazy memoisiert.
#  BESTANDSSCHUTZ: die Einheit ruft NUR die Funktionen von R/manifest.R und
#  R/export.R auf; sie beruehrt weder kernel/ noch export_bloecke_bauen noch
#  R/manifest.R.
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
source(file.path(.root, "R", "manifest.R"))
source(file.path(.root, "R", "export.R"))
source(file.path(.root, "tests", "tier1_baseline.R"))  # TIER1_BASELINE (Golden Master)
source(file.path(.root, "R", "herkunft.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Vertragskonstanten als EIGENE Literale (unabhaengig vom Rueckgabeobjekt).
.stempel_keys <- c("run_id", "manifest_sha256", "git_sha", "renv_lock_sha256",
                   "r_version", "zeitstempel")
.herkunft_spalten <- c("kennzahl", "wert", "run_id", "manifest_sha256",
                       "blatt", "zelle")
.herkunft_labels <- c("Historie (Tier-1)", "Tiefpunkt M3 (Tier-1)",
                      "Planergebnis (Tier-1)")
# Datenblaetter/-abschnitte von list(forecast = fc): export_bloecke_bauen
# expandiert forecast zu ZWEI Bloecken — unabhaengiges Literal, nicht aus dem
# Rueckgabeobjekt abgeleitet.
.daten_bloecke <- c("forecast", "forecast_kpis")

# Key/Wert-Zugriff, fuer stempel wie herkunft (beide tragen kennzahl/wert).
.wert_von <- function(df, label) df$wert[df$kennzahl == label]

# ---------------------------------------------------------------------------
# Fixture-Naht, LAZY memoisiert (delayedAssign-Muster der Schwester-Specs):
# EIN deterministischer Tier-1-Forecast auf WB + EIN deterministisch gebautes
# Minimal-Manifest v1 (injizierte umgebung/zeit/user, kein umgebung_erfassen()
# im Pfad, kein RNG). Genau EIN teurer Aufruf (phase_forecast), keine MC.
# ---------------------------------------------------------------------------
.cache <- new.env(parent = emptyenv())
delayedAssign("fix", local({
  fc <- phase_forecast(scenario_from_workbook(WB, name = "HerkunftSpec"))
  u  <- list(r_version = as.character(getRversion()), plattform = "spec",
             git_sha = NA_character_, git_dirty = NA, renv_lock_sha256 = NA_character_)
  md <- tempfile(fileext = ".json")
  m  <- manifest_erstellen(eingaben = WB, parameter = list(S = 0L),
                           seeds = c(sim = 2026L), outputs = character(0),
                           zeit = as.POSIXct("2026-07-11 08:00:00", tz = "UTC"),
                           user = "spec", umgebung = u)
  manifest_schreiben(m, md)
  hk <- herkunft_bauen(fc, md)
  list(fc = fc, md = md, m = m, hk = hk)
}), assign.env = .cache)
fix <- function() .cache$fix

# ===========================================================================
#  (1) HERKUNFT_BAUEN-Vertrag: Rueckgabeform, Spalten, Zeilenzahl, Reihenfolge
# ===========================================================================

test_that("herkunft_bauen: list(stempel, herkunft) mit gepinnten Spalten, sechs Stempeln und drei Herkunftszeilen in Vertragsreihenfolge", {
  hk <- fix()$hk
  expect_true(is.list(hk))
  expect_identical(names(hk), c("stempel", "herkunft"))

  st <- hk$stempel
  expect_true(is.data.frame(st))
  expect_identical(names(st), c("kennzahl", "wert"))
  expect_identical(st$kennzahl, .stempel_keys)     # sechs Stempel, Reihenfolge
  expect_identical(nrow(st), 6L)
  expect_true(is.character(st$wert))               # alles Zeichenketten

  hr <- hk$herkunft
  expect_true(is.data.frame(hr))
  expect_identical(names(hr), .herkunft_spalten)   # exakt sechs Spalten
  expect_identical(nrow(hr), 3L)
  expect_identical(hr$kennzahl, .herkunft_labels)  # drei Anker, Reihenfolge
  expect_true(is.numeric(hr$wert))                 # wert numerisch
  for (sp in c("kennzahl", "run_id", "manifest_sha256", "blatt", "zelle"))
    expect_true(is.character(hr[[sp]]), info = sp) # uebrige Spalten Zeichenketten
})

# ===========================================================================
#  (2) BINDING-ORAKEL: drei Tier-1-Anker UNABHAENGIG gebunden (Golden Master,
#      NICHT aus forecast) — ein Vertauschen faellt auf
# ===========================================================================

test_that("herkunft_bauen: die drei wert sind euro-identisch mit den TIER1_BASELINE-Literalen (Toleranz 0 via round()==), paarweise verschieden", {
  hr <- fix()$hk$herkunft
  # Orakel aus dem eingefrorenen Golden Master, NICHT aus forecast gezogen.
  expect_identical(round(.wert_von(hr, "Historie (Tier-1)")),      TIER1_BASELINE$history)  # -5235600
  expect_identical(round(.wert_von(hr, "Tiefpunkt M3 (Tier-1)")),  TIER1_BASELINE$trough)   # -5135990
  expect_identical(round(.wert_von(hr, "Planergebnis (Tier-1)")),  TIER1_BASELINE$result)   # 2672411

  # Paarweise verschieden: ein trough<->result-Tausch traefe ein falsches
  # Literal und faellt oben; hier zusaetzlich als expliziter Negativ-Pin.
  expect_identical(length(unique(hr$wert)), 3L)
})

# ===========================================================================
#  (3) KEIN HASH PRUEFT SICH SELBST: manifest_sha256 EXTERN nachgerechnet,
#      run_id aus dem Manifest, Herkunftszeilen tragen dieselbe eine Wahrheit
# ===========================================================================

test_that("herkunft_bauen: manifest_sha256 == extern gerechneter Datei-Hash (steht NICHT im Manifest), run_id aus manifest_lesen, je Herkunftszeile identisch", {
  f  <- fix()
  st <- f$hk$stempel
  hr <- f$hk$herkunft
  ml <- manifest_lesen(f$md)

  hash_extern <- sha256_datei(f$md)              # unabhaengig ueber die Bytes
  expect_identical(.wert_von(st, "manifest_sha256"), hash_extern)
  expect_identical(.wert_von(st, "run_id"),          ml$run_id)

  # Der Hash kann zirkulaer NICHT im Manifest stehen: weder als Feld noch als
  # Zeichenkette in den rohen Datei-Bytes.
  expect_false("manifest_sha256" %in% names(ml))
  roh <- rawToChar(readBin(f$md, "raw", n = file.info(f$md)$size))
  expect_false(grepl(hash_extern, roh, fixed = TRUE))

  # Uebrige Stempel stammen aus manifest_lesen (Run-ID plus Versions-/Hash-
  # Stempel) — eine Wahrheit pro Fakt.
  expect_identical(.wert_von(st, "git_sha"),          ml$umgebung$git_sha)
  expect_identical(.wert_von(st, "renv_lock_sha256"), ml$umgebung$renv_lock_sha256)
  expect_identical(.wert_von(st, "r_version"),        ml$umgebung$r_version)
  expect_identical(.wert_von(st, "zeitstempel"),      ml$zeitstempel)

  # run_id/manifest_sha256 in JEDER der drei Herkunftszeilen identisch zum
  # Stempel (NICHT je Zeile neu gezogen).
  expect_identical(hr$run_id,          rep(.wert_von(st, "run_id"), 3L))
  expect_identical(hr$manifest_sha256, rep(hash_extern, 3L))
})

# ===========================================================================
#  (4) BLATT/ZELLE: wo der Ankerwert im selben Export Zelle fuer Zelle sitzt
#      (Phase-9-forecast_kpis-Vertrag), Historie allein im Herkunfts-Nachweis
# ===========================================================================

test_that("herkunft_bauen: Tiefpunkt/Planergebnis -> forecast_kpis/trough bzw. /result; Historie -> Herkunft/eigenes Label", {
  hr <- fix()$hk$herkunft
  ze <- function(label, sp) hr[[sp]][hr$kennzahl == label]

  expect_identical(ze("Tiefpunkt M3 (Tier-1)", "blatt"), "forecast_kpis")
  expect_identical(ze("Tiefpunkt M3 (Tier-1)", "zelle"), "trough")
  expect_identical(ze("Planergebnis (Tier-1)", "blatt"), "forecast_kpis")
  expect_identical(ze("Planergebnis (Tier-1)", "zelle"), "result")

  # Historie ist in KEINEM Datenblatt materialisiert, allein im Herkunfts-
  # Nachweis (blatt 'Herkunft', zelle = eigenes Label).
  expect_identical(ze("Historie (Tier-1)", "blatt"), "Herkunft")
  expect_identical(ze("Historie (Tier-1)", "zelle"), "Historie (Tier-1)")
})

# ===========================================================================
#  (5) BEOBACHTER-INVARIANZ xlsx: Datenblatt unberuehrt, zwei Beobachter-
#      Blaetter ANS ENDE, ruecklesbar mit Stempel-/Ankerwerten
# ===========================================================================

test_that("export_xlsx mit herkunft: forecast_kpis rueckgelesen identical mit/ohne herkunft; zwei Blaetter angehaengt; Stempel-/Ankerwerte ruecklesbar", {
  f   <- fix()
  erg <- list(forecast = f$fc)
  tf1 <- tempfile(fileext = ".xlsx")
  tf2 <- tempfile(fileext = ".xlsx")
  r1  <- export_xlsx(erg, tf1)
  r2  <- export_xlsx(erg, tf2, herkunft = f$hk)

  # Datenblatt unberuehrt (Beobachter-Invarianz).
  fk1 <- openxlsx2::wb_to_df(openxlsx2::wb_load(tf1), sheet = "forecast_kpis")
  fk2 <- openxlsx2::wb_to_df(openxlsx2::wb_load(tf2), sheet = "forecast_kpis")
  expect_identical(fk1, fk2)

  # Blattliste: Datenblaetter unveraendert, zwei Beobachter-Blaetter ANS ENDE.
  expect_identical(r1$blaetter, .daten_bloecke)
  expect_identical(r2$blaetter, c(.daten_bloecke, "GoBD_Stempel", "Herkunft"))

  # Beobachter-Blaetter ruecklesbar und tragen die richtigen Werte.
  stg <- openxlsx2::wb_to_df(openxlsx2::wb_load(tf2), sheet = "GoBD_Stempel")
  expect_identical(stg$wert[stg$kennzahl == "manifest_sha256"], sha256_datei(f$md))

  hrg <- openxlsx2::wb_to_df(openxlsx2::wb_load(tf2), sheet = "Herkunft")
  expect_identical(round(hrg$wert[hrg$kennzahl == "Historie (Tier-1)"]),     TIER1_BASELINE$history)
  expect_identical(round(hrg$wert[hrg$kennzahl == "Tiefpunkt M3 (Tier-1)"]), TIER1_BASELINE$trough)
  expect_identical(round(hrg$wert[hrg$kennzahl == "Planergebnis (Tier-1)"]), TIER1_BASELINE$result)

  unlink(c(tf1, tf2))
})

# ===========================================================================
#  (6) SYMMETRIE docx/pdf: dieselbe Abschnittsfolge wie xlsx$blaetter;
#      ohne herkunft bleibt die Datenfolge unveraendert (Regressionspin)
# ===========================================================================

test_that("export_docx/pdf mit herkunft: abschnitte == c(<daten>, 'GoBD_Stempel', 'Herkunft'); ohne herkunft unveraendert", {
  f   <- fix()
  erg <- list(forecast = f$fc)
  erweitert <- c(.daten_bloecke, "GoBD_Stempel", "Herkunft")

  d1 <- export_docx(erg, tempfile(fileext = ".docx"))
  d2 <- export_docx(erg, tempfile(fileext = ".docx"), herkunft = f$hk)
  expect_identical(d1$abschnitte, .daten_bloecke)
  expect_identical(d2$abschnitte, erweitert)

  p1 <- export_pdf(erg, tempfile(fileext = ".pdf"))
  p2 <- export_pdf(erg, tempfile(fileext = ".pdf"), herkunft = f$hk)
  expect_identical(p1$abschnitte, .daten_bloecke)
  expect_identical(p2$abschnitte, erweitert)

  # Symmetrie ueber die Medien: alle drei tragen dieselbe erweiterte Folge.
  expect_identical(d2$abschnitte, p2$abschnitte)
})

# ===========================================================================
#  (7) FEHLERFAELLE: benannte Befunde (Stil manifest_validieren)
# ===========================================================================

test_that("herkunft_bauen: fehlender/kein manifest_pfad -> Fehler MIT Pfad; forecast ohne erg_eng bzw. ohne trough -> Fehler MIT Feldname", {
  f <- fix()

  # Nicht existierender Manifest-Pfad -> Fehler MIT dem Pfad im Text.
  fehlt <- tempfile(fileext = ".json")
  expect_false(file.exists(fehlt))
  expect_error(herkunft_bauen(f$fc, fehlt), regexp = fehlt, fixed = TRUE)

  # manifest_pfad kein String -> Fehler.
  expect_error(herkunft_bauen(f$fc, 123L))

  # forecast ohne validation$erg_eng -> Fehler MIT Feldname.
  fc_ohne_hist <- f$fc; fc_ohne_hist$validation$erg_eng <- NULL
  expect_error(herkunft_bauen(fc_ohne_hist, f$md), regexp = "erg_eng")

  # forecast ohne kpis$trough -> Fehler MIT Feldname.
  fc_ohne_trough <- f$fc; fc_ohne_trough$kpis$trough <- NULL
  expect_error(herkunft_bauen(fc_ohne_trough, f$md), regexp = "trough")
})

# ===========================================================================
#  (8) RNG-WAECHTER: kein RNG im GoBD-Pfad
# ===========================================================================

test_that("herkunft_bauen: .Random.seed vor/nach unveraendert (kein RNG)", {
  f <- fix()
  set.seed(1)
  invisible(runif(1))
  rng_vor <- .Random.seed
  hk2 <- herkunft_bauen(f$fc, f$md)
  expect_identical(.Random.seed, rng_vor)
  # Determinismus: zweiter Bau identisch zum memoisierten.
  expect_identical(hk2, f$hk)
})
