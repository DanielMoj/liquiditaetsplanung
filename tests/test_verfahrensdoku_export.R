# ============================================================================
#  tests/test_verfahrensdoku_export.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-15-verfahrensdoku-export
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: R/verfahrensdoku.R
#  laesst sich sourcen (der Generator verfahrensdoku_erstellen aus phase-14 lebt
#  dort), aber die Serialisierungsfunktion export_verfahrensdoku EXISTIERT NOCH
#  NICHT — jeder Test faellt am fehlenden Symbol. Der Builder erfuellt sie erst
#  mit der Implementierung. Kein skip(), kein succeed(), keine Existenz-Weiche,
#  keine zweite Monte-Carlo, KEIN Engine-Aufruf, KEIN Tier-1-Zahlen-Literal,
#  kleine Laufzeit — die Funktion rechnet nicht.
#
#  Vertrag (handoff/phase-15-verfahrensdoku-export.json):
#   - export_verfahrensdoku(vd, pfad): reine, deterministische Serialisierung
#     des Verfahrensdokumentations-Objekts vd (Rueckgabe von
#     verfahrensdoku_erstellen) in GENAU EINE xlsx-Datei. Rueckgabe
#     list(pfad, blaetter): pfad == uebergebener Pfad; blaetter == character der
#     Blattnamen in Schreibreihenfolge == names(vd).
#   - vd wird als Liste mit EXAKT den fuenf Namen meta, allgemeine_beschreibung,
#     betriebsdoku, bestand, review validiert (sonst stop MIT einem Sektionsnamen
#     im Text, Stil wie der VD-umgebung-Validator); pfad ein einzelner String.
#   - Fuenf Blaetter in Reihenfolge names(vd): Blatt "meta" als Key/Wert-
#     Projektion (Spalten feld/wert, homogen as.character) ueber die sieben
#     Felder titel, geltungsbereich und die fuenf umgebung-Felder r_version,
#     plattform, git_sha, git_dirty (logisch -> Text TRUE/FALSE), renv_lock_sha256
#     — die umgebung-Durchreichung aus phase-14 ueberlebt den Export. Die vier
#     weiteren Blaetter sind die data.frames UNVERAENDERT in native Typen.
#   - Deterministisch (kein Zeitstempel im Zellinhalt; Dateibytes duerfen
#     abweichen, der gelesene Zellinhalt nicht), kein RNG im Serialisierungs-Pfad.
#
#  Orakel-Politik: die INJIZIERTE umgebung ist eine EIGENE Quelle (nie beide
#  Seiten aus demselben Code); gelesen wird ueber openxlsx2::wb_load +
#  wb_to_df. Identitaets-/Strukturpruefungen (Toleranz 0); die review-nummer
#  exakt via as.numeric 1e-12.
#  BESTANDSSCHUTZ: legt export_verfahrensdoku NUR in R/verfahrensdoku.R ab
#  (nicht hier gebaut); beruehrt weder kernel/ NOCH R/export.R NOCH R/manifest.R
#  / R/manifest_anbindung.R / R/protokoll.R / R/finalisierung.R / R/herkunft.R /
#  R/z3.R NOCH eine bestehende test_*-Datei.
# ============================================================================

library(testthat)

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "manifest.R"))
source(file.path(.root, "R", "verfahrensdoku.R"))  # existiert; export_verfahrensdoku fehlt -> rot

# ---------------------------------------------------------------------------
# Vertragskonstanten als EIGENE Literale (unabhaengig vom Rueckgabeobjekt).
# ---------------------------------------------------------------------------
.blaetter_erwartet <- c("meta", "allgemeine_beschreibung", "betriebsdoku",
                        "bestand", "review")
.meta_feld         <- c("titel", "geltungsbereich", "r_version", "plattform",
                        "git_sha", "git_dirty", "renv_lock_sha256")
.sektionen_df      <- c("allgemeine_beschreibung", "betriebsdoku", "bestand",
                        "review")

# INJIZIERTE umgebung als eigene Orakel-Quelle: paarweise unverwechselbare,
# nicht-triviale Werte (git_dirty logisch TRUE -> muss als Text "TRUE" landen).
.inj <- list(r_version        = "9.9.9",
             plattform        = "vdExportAnker",
             git_sha          = "abc123def4567890abcdef1234567890abcdef12",
             git_dirty        = TRUE,
             renv_lock_sha256 = "feedface00112233")

# EIN gebautes VD-Objekt fuer die ganze Datei (deterministisch, rechnet nicht).
.vd <- verfahrensdoku_erstellen(umgebung = .inj)

# ---------------------------------------------------------------------------
# Leseformen (wb_load + wb_to_df, die im Repo etablierte Ruecklese).
# ---------------------------------------------------------------------------
.sheets <- function(pfad) unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(pfad)))
.rdf    <- function(pfad, nm) openxlsx2::wb_to_df(openxlsx2::wb_load(pfad),
                                                  sheet = nm, detectDates = FALSE)

# Zelle-fuer-Zelle-Gleichheit zweier data.frames: gleiche Spaltennamen; je
# Spalte numerisch via all.equal 1e-12, sonst as.character identical.
.ceq <- function(a, b) {
  if (!identical(names(a), names(b))) return(FALSE)
  for (k in names(a)) {
    if (is.numeric(a[[k]]) || is.numeric(b[[k]])) {
      if (!isTRUE(all.equal(as.numeric(a[[k]]), as.numeric(b[[k]]),
                            tolerance = 1e-12))) return(FALSE)
    } else if (!identical(as.character(a[[k]]), as.character(b[[k]]))) {
      return(FALSE)
    }
  }
  TRUE
}

# EIN gemeinsamer Export, LAZY memoisiert.
.cache <- new.env(parent = emptyenv())
delayedAssign("haupt", local({
  tf <- tempfile(fileext = ".xlsx")
  list(rueck = export_verfahrensdoku(.vd, tf), pfad = tf)
}), assign.env = .cache)
haupt <- function() .cache$haupt

# ===========================================================================
#  (1) RUECKGABE-VERTRAG
# ===========================================================================
test_that("Rueckgabe: list(pfad, blaetter); pfad existiert und ist der uebergebene Pfad; blaetter == names(vd) == Datei-Blattliste", {
  h <- haupt()
  expect_type(h$rueck, "list")
  expect_true(all(c("pfad", "blaetter") %in% names(h$rueck)))

  expect_identical(h$rueck$pfad, h$pfad)
  expect_true(file.exists(h$rueck$pfad))

  expect_identical(h$rueck$blaetter, .blaetter_erwartet)
  expect_identical(h$rueck$blaetter, names(.vd))
  expect_identical(.sheets(h$pfad), .blaetter_erwartet)
})

# ===========================================================================
#  (2) RUECKLESE-TREUE — UNABHAENGIGES ORAKEL
# ===========================================================================
# Die vier data.frame-Blaetter sind Zelle fuer Zelle treu (Strings identical,
# review-nummer via as.numeric 1e-12); das meta-Blatt ist eine getreue
# Key/Wert-Projektion der sieben Felder mit den aus der INJIZIERTEN umgebung
# gerechneten wert-Eintraegen (git_dirty als Text "TRUE"). Es gibt EXAKT diese
# fuenf Blaetter — kein Sechstes (kein Phantomsheet, keine Koeder-Felder).
test_that("Ruecklese-Treue: vier data.frame-Blaetter Zelle fuer Zelle, meta als Key/Wert-Projektion, genau fuenf Blaetter", {
  pf <- haupt()$pfad

  # vier data.frame-Blaetter UNVERAENDERT gegen das VD-Objekt
  for (nm in .sektionen_df) {
    expect_true(.ceq(.rdf(pf, nm), .vd[[nm]]),
                info = paste0("Blatt weicht vom VD-data.frame ab: ", nm))
  }
  # review-nummer sind die ganzen Zahlen 1..8 (nicht-triviales Regime)
  rv <- .rdf(pf, "review")
  expect_equal(as.numeric(rv$nummer), 1:8, tolerance = 1e-12)

  # meta-Blatt: Spalten feld/wert, feld exakt, wert aus der injizierten umgebung
  mdf <- .rdf(pf, "meta")
  expect_identical(names(mdf), c("feld", "wert"))
  expect_identical(as.character(mdf$feld), .meta_feld)

  mw <- as.character(c(.vd$meta$titel, .vd$meta$geltungsbereich,
                       .inj$r_version, .inj$plattform, .inj$git_sha,
                       .inj$git_dirty, .inj$renv_lock_sha256))
  expect_identical(as.character(mdf$wert), mw)
  # git_dirty (logisch TRUE) ueberlebt als Text "TRUE"
  expect_identical(as.character(mdf$wert)[.meta_feld == "git_dirty"], "TRUE")

  # genau fuenf Blaetter, kein Sechstes (kein separates erzeugt_umgebung-Sheet o.ae.)
  sh <- .sheets(pf)
  expect_length(sh, 5L)
  expect_true(setequal(sh, .blaetter_erwartet))
})

# ===========================================================================
#  (3) UMGEBUNG-DURCHREICHUNG
# ===========================================================================
# Die meta-wert-Erwartung wird aus der INJIZIERTEN umgebung gerechnet, das
# geschriebene Blatt stammt aus vd$meta$erzeugt_umgebung — beide muessen
# identisch sein. Mutationskraft gegen einen Serialisierer, der die umgebung
# verwirft oder statisch fuellt.
test_that("Umgebung-Durchreichung: die fuenf umgebung-Felder im meta-Blatt sind die injizierten Werte", {
  pf  <- haupt()$pfad
  mdf <- .rdf(pf, "meta")

  # das VD-Objekt traegt die injizierte umgebung unveraendert (Vertrag phase-14)
  expect_identical(.vd$meta$erzeugt_umgebung, .inj)

  umg_soll <- as.character(unlist(.inj[c("r_version", "plattform", "git_sha",
                                         "git_dirty", "renv_lock_sha256")]))
  idx <- match(c("r_version", "plattform", "git_sha", "git_dirty",
                 "renv_lock_sha256"), as.character(mdf$feld))
  expect_identical(as.character(mdf$wert)[idx], umg_soll)
})

# ===========================================================================
#  (4) DETERMINISMUS
# ===========================================================================
# Zwei Exporte desselben vd nach zwei tempfiles liefern identisch
# zurueckgelesene Zellinhalte (Dateibytes duerfen abweichen).
test_that("Determinismus: zwei Exporte -> identische zurueckgelesene Zellinhalte", {
  tfa <- tempfile(fileext = ".xlsx")
  tfb <- tempfile(fileext = ".xlsx")
  ra  <- export_verfahrensdoku(.vd, tfa)
  rb  <- export_verfahrensdoku(.vd, tfb)

  expect_identical(ra$blaetter, rb$blaetter)
  expect_identical(.sheets(tfa), .sheets(tfb))
  for (nm in .blaetter_erwartet) {
    expect_true(.ceq(.rdf(tfa, nm), .rdf(tfb, nm)),
                info = paste0("nicht-deterministischer Zellinhalt in Blatt ", nm))
  }
})

# ===========================================================================
#  (5) RNG-WAECHTER
# ===========================================================================
# .Random.seed bleibt ueber export_verfahrensdoku unveraendert (kein RNG im
# Serialisierungs-Pfad).
test_that(".Random.seed bleibt ueber export_verfahrensdoku unveraendert", {
  set.seed(7)
  s0 <- .Random.seed
  tf <- tempfile(fileext = ".xlsx")
  invisible(export_verfahrensdoku(.vd, tf))
  expect_identical(s0, .Random.seed)
})

# ===========================================================================
#  (6) FEHLERFAELLE — benennbarer Befund
# ===========================================================================
# vd ohne die fuenf Sektionen (oder mit falschem Namen) -> Fehler, dessen Text
# einen Sektionsnamen nennt; pfad kein einzelner String -> Fehler (nennt pfad).
test_that("Fehlerfaelle: falsche vd-Sektionen nennen einen Sektionsnamen; pfad kein einzelner String wirft", {
  tf <- tempfile(fileext = ".xlsx")
  sekt_regex <- "meta|allgemeine_beschreibung|betriebsdoku|bestand|review"

  # fehlende Sektion (review entfernt)
  vd_fehlt <- .vd
  vd_fehlt$review <- NULL
  expect_error(export_verfahrensdoku(vd_fehlt, tf), regexp = sekt_regex)

  # falscher Sektionsname (meta -> xmeta)
  vd_falsch <- .vd
  names(vd_falsch)[names(vd_falsch) == "meta"] <- "xmeta"
  expect_error(export_verfahrensdoku(vd_falsch, tf), regexp = sekt_regex)

  # gar keine Sektionen
  expect_error(export_verfahrensdoku(list(), tf), regexp = sekt_regex)

  # pfad kein einzelner String
  expect_error(export_verfahrensdoku(.vd, c(tf, tf)), regexp = "pfad")
  expect_error(export_verfahrensdoku(.vd, 42), regexp = "pfad")
})
