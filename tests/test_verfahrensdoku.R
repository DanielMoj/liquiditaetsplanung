# ============================================================================
#  tests/test_verfahrensdoku.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-14-verfahrensdoku
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: das ungeschuetzte
#  source() von R/verfahrensdoku.R am Kopf bricht ab, solange die Datei fehlt.
#  Der Builder erfuellt sie erst mit der Implementierung. Kein skip(), kein
#  succeed(), keine Existenz-Weiche, keine zweite Monte-Carlo, kein
#  Engine-Aufruf, KEIN Tier-1-Zahlen-Literal — die Funktion rechnet nicht.
#
#  Vertrag (handoff/phase-14-verfahrensdoku.json; GoBD Rz. 153 —
#  Verfahrensdokumentation; vorlagenfrei, schirm-los):
#   - verfahrensdoku_erstellen(umgebung = umgebung_erfassen()): reine,
#     DETERMINISTISCHE Kernfunktion ohne RNG und ohne Sys.time() im
#     Objektinhalt. Validiert umgebung (Stil manifest_validieren: stop MIT
#     Feldname). Liefert eine BENANNTE Liste mit EXAKT den fuenf Top-Level-
#     Eintraegen meta / allgemeine_beschreibung / betriebsdoku / bestand /
#     review:
#       * meta = list(titel, geltungsbereich, erzeugt_umgebung = umgebung) —
#         die umgebung ist KOMPLETT als Objekt durchgereicht.
#       * allgemeine_beschreibung: data.frame mit EXAKT aspekt/beschreibung/
#         verantwortung (NEU verfasst).
#       * betriebsdoku: data.frame mit EXAKT aspekt/beschreibung/verantwortung
#         (NEU verfasst), inkl. Betreiber-Pflichten (WORM/Aufbewahrungsfristen/
#         Berechtigungswesen) als 'betreiber'-Zeilen.
#       * bestand: data.frame mit EXAKT komponente/datei/funktionen/
#         verfahrensteil/verantwortung — der vorhandene GoBD-Bestand
#         EINSORTIERT, verantwortung 'hersteller', datei-Eintraege real auf
#         Platte.
#       * review: data.frame mit EXAKT nummer/kategorie/punkt — Reviewer-
#         Checkliste, >= 8 Zeilen.
#     verantwortung ist eine geschlossene Menge {hersteller, betreiber,
#     gemeinsam}. Determinismus: zweimal dieselben Argumente -> identical();
#     .Random.seed bleibt unveraendert.
#
#  Orakel-Politik: der GoBD-Bestand wird als EIGENES Literal aufgezaehlt
#  (Lehre: nie beide Seiten aus demselben Code) und gegen die reale Platte
#  geprueft — ein Droppen/Umbenennen eines Bestandseintrags faellt auf. Struktur-
#  , Identitaets- und Existenzpruefungen (Toleranz 0); die reduzierte Gate-Kette
#  dieser Einheit stuetzt sich auf diese Struktur-Checks plus die im Objekt
#  getragene Reviewer-Checkliste.
#  BESTANDSSCHUTZ: die Datei legt nur R/verfahrensdoku.R als Ziel an, beruehrt
#  weder kernel/ noch eine bestehende test_*-Datei noch R/manifest.R /
#  R/manifest_anbindung.R / R/protokoll.R / R/finalisierung.R / R/herkunft.R /
#  R/z3.R und legt KEIN neues Paket an.
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
source(file.path(.root, "R", "verfahrensdoku.R"))  # <- existiert noch nicht: rot zuerst

# ---------------------------------------------------------------------------
# Vertragskonstanten als EIGENE Literale (unabhaengig vom Rueckgabeobjekt).
# ---------------------------------------------------------------------------
.top_namen        <- c("meta", "allgemeine_beschreibung", "betriebsdoku",
                       "bestand", "review")
.ab_spalten       <- c("aspekt", "beschreibung", "verantwortung")
.bd_spalten       <- c("aspekt", "beschreibung", "verantwortung")
.bestand_spalten  <- c("komponente", "datei", "funktionen", "verfahrensteil",
                       "verantwortung")
.review_spalten   <- c("nummer", "kategorie", "punkt")
.verantwortung_menge <- c("hersteller", "betreiber", "gemeinsam")

# Der reale GoBD-Bestand — als EIGENES Literal aufgezaehlt (unabhaengiges
# Orakel gegen ein verkuerztes Inventar), repo-relativ.
.gobd_bestand <- c("R/manifest.R", "R/manifest_anbindung.R", "R/protokoll.R",
                   "R/finalisierung.R", "R/herkunft.R", "R/z3.R")

# Die Reviewer-Kategorien, die abgedeckt sein muessen.
.review_kategorien <- c("anker", "nachweis", "protokoll", "finalisierung",
                        "herkunft", "z3", "split", "grenze")

# Handgebaute, feste Umgebung — INJIZIERT, damit der reine Kern deterministisch
# testbar ist (kein umgebung_erfassen()-Aufruf im Pfad).
.mk_umgebung <- function() {
  list(r_version        = as.character(getRversion()),
       plattform        = "testplattform",
       git_sha          = NA_character_,
       git_dirty        = NA,
       renv_lock_sha256 = NA_character_)
}

.cols_ok <- function(x, cs) is.data.frame(x) && identical(names(x), cs)

# ---------------------------------------------------------------------------
# (1) RUECKGABE-VERTRAG
# ---------------------------------------------------------------------------
test_that("verfahrensdoku_erstellen liefert eine benannte Liste mit exakt fuenf Sektionen und den gepinnten Spaltenvertraegen", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())

  expect_type(vd, "list")
  expect_identical(names(vd), .top_namen)

  # meta = list(titel, geltungsbereich, erzeugt_umgebung)
  expect_true(is.list(vd$meta) && !is.data.frame(vd$meta))
  expect_true(all(c("titel", "geltungsbereich", "erzeugt_umgebung") %in% names(vd$meta)))
  expect_true(is.character(vd$meta$titel) && length(vd$meta$titel) == 1L && nzchar(vd$meta$titel))
  expect_true(is.character(vd$meta$geltungsbereich) && nzchar(vd$meta$geltungsbereich[1]))

  # data.frame-Spaltenvertraege exakt
  expect_true(.cols_ok(vd$allgemeine_beschreibung, .ab_spalten))
  expect_true(.cols_ok(vd$betriebsdoku, .bd_spalten))
  expect_true(.cols_ok(vd$bestand, .bestand_spalten))
  expect_true(.cols_ok(vd$review, .review_spalten))

  # nicht leer wo es zaehlt
  expect_gt(nrow(vd$allgemeine_beschreibung), 0L)
  expect_gt(nrow(vd$betriebsdoku), 0L)
  expect_gt(nrow(vd$bestand), 0L)
})

# ---------------------------------------------------------------------------
# (2) VERANTWORTUNG GESCHLOSSEN
# ---------------------------------------------------------------------------
test_that("jede verantwortung-Zelle der drei tragenden Sektionen ist Element von {hersteller, betreiber, gemeinsam}", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())
  verb <- c(vd$allgemeine_beschreibung$verantwortung,
            vd$betriebsdoku$verantwortung,
            vd$bestand$verantwortung)
  expect_gt(length(verb), 0L)
  expect_true(all(verb %in% .verantwortung_menge))
})

# ---------------------------------------------------------------------------
# (3) BESTAND EINSORTIERT — UNABHAENGIGES ORAKEL
# ---------------------------------------------------------------------------
test_that("der Bestand deckt die sechs realen GoBD-Bestandsdateien ab und jeder datei-Eintrag existiert auf Platte", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())

  # (a) jede erwartete GoBD-Datei ist im Bestand katalogisiert
  for (d in .gobd_bestand) {
    expect_true(d %in% vd$bestand$datei,
                info = paste0("GoBD-Bestandsdatei fehlt im Bestand: ", d))
  }
  # (b) jede erwartete Datei existiert real (repo-relativ von der Wurzel)
  for (d in .gobd_bestand) {
    expect_true(file.exists(file.path(.root, d)),
                info = paste0("erwartete GoBD-Datei nicht auf Platte: ", d))
  }
  # (c) jeder Bestandseintrag zeigt auf eine existierende Datei (kein Phantom)
  expect_true(all(file.exists(file.path(.root, vd$bestand$datei))))

  # Bestand ist nicht verkuerzt
  expect_gte(nrow(vd$bestand), length(.gobd_bestand))

  # der Bestand ist 'hersteller'-Verantwortung
  eintraege <- vd$bestand$verantwortung[vd$bestand$datei %in% .gobd_bestand]
  expect_true(all(eintraege == "hersteller"))
})

# ---------------------------------------------------------------------------
# (4) HERSTELLER-/BETREIBER-SPLIT
# ---------------------------------------------------------------------------
test_that("kombiniert gibt es mindestens eine hersteller- und eine betreiber-Zeile; die Betreiber-Pflichten sind ehrlich als 'betreiber' ausgewiesen", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())
  verb <- c(vd$allgemeine_beschreibung$verantwortung,
            vd$betriebsdoku$verantwortung,
            vd$bestand$verantwortung)
  expect_true(any(verb == "hersteller"))
  expect_true(any(verb == "betreiber"))

  bd_txt <- paste(tolower(vd$betriebsdoku$aspekt),
                  tolower(vd$betriebsdoku$beschreibung), sep = " | ")

  # Aufbewahrungsfristen und Berechtigungswesen erscheinen und sind 'betreiber'
  idx_aufb <- which(grepl("aufbewahrungsfrist", bd_txt))
  idx_ber  <- which(grepl("berechtigung", bd_txt))
  expect_gt(length(idx_aufb), 0L)
  expect_gt(length(idx_ber), 0L)

  # der WORM-/Dateisystem-Schutz ist ebenfalls 'betreiber'
  idx_worm <- which(grepl("worm|dateisystem", bd_txt))
  expect_gt(length(idx_worm), 0L)

  grenz_idx <- unique(c(idx_aufb, idx_ber, idx_worm))
  expect_true(all(vd$betriebsdoku$verantwortung[grenz_idx] == "betreiber"))
})

# ---------------------------------------------------------------------------
# (5) ALLGEMEINE BESCHREIBUNG NEU
# ---------------------------------------------------------------------------
test_that("die allgemeine Beschreibung deckt Systemumgebung, Rechenbasis (Kernel-Guard), Engine-Vertrag und den Schutz der drei Tier-1-Anker (ohne Zahlen-Literal)", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())
  txt <- tolower(paste(vd$allgemeine_beschreibung$aspekt,
                       vd$allgemeine_beschreibung$beschreibung, sep = " | "))

  # Systemumgebung: R-Shiny / Desktop + stochastische Krankenhaus-Liquiditaetsplanung
  expect_true(any(grepl("shiny", txt)))
  expect_true(any(grepl("liquidit", txt)))
  expect_true(any(grepl("stochast", txt)))

  # Rechenbasis: kernel wird VERPACKT, nicht neu geschrieben (Kernel-Guard)
  expect_true(any(grepl("kernel|engine\\.r", txt)))
  expect_true(any(grepl("verpack|nicht neu", txt)))

  # Engine-Vertrag: Blattnamen 01_Positionen ... 10_OPOS
  expect_true(any(grepl("01_positionen", txt)))
  expect_true(any(grepl("10_opos", txt)))

  # Schutz der drei Tier-1-Anker via run_all.R / anker_plan_tiefpunkt —
  # NENNUNG, KEIN Zahlen-Literal.
  expect_true(any(grepl("tier-1|tier 1|tier1", txt)))
  expect_true(any(grepl("run_all|anker_plan_tiefpunkt", txt)))
})

# ---------------------------------------------------------------------------
# (6) BETRIEBSDOKU NEU
# ---------------------------------------------------------------------------
test_that("die Betriebsdoku deckt Einrichtung, DOD, Nebenlaeufigkeit, Determinismus, DE-Format und die write-once-Nachweis-Ablage", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())
  txt <- tolower(paste(vd$betriebsdoku$aspekt,
                       vd$betriebsdoku$beschreibung, sep = " | "))

  expect_true(any(grepl("deps\\.r", txt)))                     # Einrichtung
  expect_true(any(grepl("rscript tests/run_all\\.r|run_all\\.r", txt)))  # DOD
  expect_true(any(grepl("mirai|future|worker", txt)))          # Nebenlaeufigkeit
  expect_true(any(grepl("set\\.seed|seed", txt)))              # Determinismus
  expect_true(any(grepl("fmt", txt)))                          # DE-Format
  expect_true(any(grepl("runs/", txt)))                        # Nachweis-Ablage
  expect_true(any(grepl("write-once|write once", txt)))        # write-once
})

# ---------------------------------------------------------------------------
# (7) REVIEWER-CHECKLISTE
# ---------------------------------------------------------------------------
test_that("die Reviewer-Checkliste hat >= 8 Zeilen mit nicht-leerer Kategorie und deckt die geforderten Kategorien ab", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())
  rv <- vd$review

  expect_gte(nrow(rv), 8L)
  expect_true(all(nzchar(as.character(rv$kategorie))))

  kat <- tolower(as.character(rv$kategorie))
  for (k in .review_kategorien) {
    expect_true(any(grepl(k, kat)),
                info = paste0("Reviewer-Kategorie nicht abgedeckt: ", k))
  }
})

# ---------------------------------------------------------------------------
# (8) UMGEBUNG-DURCHREICHUNG
# ---------------------------------------------------------------------------
test_that("Default-Aufruf ruft umgebung_erfassen(); injizierte umgebung wird identisch durchgereicht", {
  # Default: r_version stammt aus umgebung_erfassen() und ist die aktive R-Version
  vd_default <- verfahrensdoku_erstellen()
  expect_identical(vd_default$meta$erzeugt_umgebung$r_version,
                   as.character(getRversion()))

  # Injektion: die Funktion nutzt die umgebung, ignoriert sie nicht
  inj <- .mk_umgebung()
  vd_inj <- verfahrensdoku_erstellen(umgebung = inj)
  expect_identical(vd_inj$meta$erzeugt_umgebung, inj)
})

# ---------------------------------------------------------------------------
# (9) DETERMINISMUS
# ---------------------------------------------------------------------------
test_that("zwei Aufrufe mit identischer injizierter umgebung liefern identical() dasselbe Objekt", {
  inj <- .mk_umgebung()
  a <- verfahrensdoku_erstellen(umgebung = inj)
  b <- verfahrensdoku_erstellen(umgebung = inj)
  expect_identical(a, b)
})

# ---------------------------------------------------------------------------
# (10) RNG-WAECHTER
# ---------------------------------------------------------------------------
test_that(".Random.seed bleibt ueber verfahrensdoku_erstellen unveraendert (kein RNG im Dokumentations-Pfad)", {
  set.seed(7)
  s0 <- .Random.seed
  invisible(verfahrensdoku_erstellen(umgebung = .mk_umgebung()))
  expect_identical(s0, .Random.seed)
})

# ---------------------------------------------------------------------------
# (11) FEHLERFALL — benennbarer Befund (Stil manifest_validieren)
# ---------------------------------------------------------------------------
test_that("eine invalide umgebung fuehrt zu einem Fehler, dessen Text einen Feldnamen nennt", {
  # leere Liste: mindestens ein Feld fehlt -> Fehler nennt ein umgebung-Feld
  expect_error(
    verfahrensdoku_erstellen(umgebung = list()),
    regexp = "r_version|plattform|git_sha|git_dirty|renv_lock_sha256|umgebung")

  # fehlendes Einzelfeld (renv_lock_sha256 entfernt) -> Fehler nennt den Feldnamen
  unvollstaendig <- .mk_umgebung()
  unvollstaendig$renv_lock_sha256 <- NULL
  expect_error(
    verfahrensdoku_erstellen(umgebung = unvollstaendig),
    regexp = "renv_lock_sha256|umgebung")
})
