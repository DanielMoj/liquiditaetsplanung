# ============================================================================
#  tests/test_verfahrensdoku_blattnamen.R  —  Ergaenzungs-Spezifikation fuer
#  phase-14-verfahrensdoku: der Engine-Vertrag in der allgemeinen Beschreibung
#  muss die ZEHN Blattnamen der Mappe VOLLSTAENDIG nennen, nicht nur das erste
#  und letzte Blatt ("01_Positionen ... 10_OPOS").
#
#  Motivation (Befund High): eine Pruefung nur auf erstes/letztes Blatt laesst
#  eine fehlende oder falsche Dokumentation der acht Zwischenblaetter gruen
#  durchgehen — Schema-Drift der Zwischenblaetter bliebe verborgen. Dieser Test
#  iteriert ueber die vollstaendige, HIER als EIGENES Literal gepinnte Blattliste
#  (Lehre: nie beide Seiten aus demselben Code) und verlangt JEDEN einzelnen
#  Blattnamen im Verfahrensdoku-Text.
#
#  Das Literal ist deckungsgleich mit dem Engine-Vertrag aus IMPLEMENTATION_RULES.md
#  (Blattnamen exakt) und R/new_mandate.R (blaetter-Vektor, ohne 00_Anleitung,
#  das kein Engine-Pflichtblatt der 01..10-Kette ist).
#  BESTANDSSCHUTZ: legt keine Datei an, beruehrt weder kernel/ noch bestehende
#  Dateien; reine Lese-Spezifikation gegen R/verfahrensdoku.R.
#
#  SPEICHER-HYGIENE (Gate-A-Lehre "teure Eigen-Testdatei durch Minimal-Fixtures
#  entlasten"): diese Datei laeuft im ungesicherten Schwanz der DOD-Suite und
#  braucht KEINE der schweren Quellen. verfahrensdoku_erstellen() ist rein und
#  bekommt die umgebung INJIZIERT (.mk_umgebung), ruft also weder
#  umgebung_erfassen() noch die Engine — daher wird AUSSCHLIESSLICH
#  R/verfahrensdoku.R gesourct (die Default-arg umgebung_erfassen() wird bei
#  injizierter umgebung nie ausgewertet). So hebt dieser zusaetzliche
#  Schwanz-Test den RSS-Boden nicht unnoetig. Der committete Spec-Test
#  tests/test_verfahrensdoku.R deckt die vollen Quellen ohnehin ab.
# ============================================================================

library(testthat)

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
# Minimal: nur die zu pruefende Datei. verfahrensdoku_erstellen ist rein und
# bekommt die umgebung injiziert -> keine weitere Quelle noetig.
source(file.path(.root, "R", "verfahrensdoku.R"))

# Die zehn Blattnamen der Mappe — EIGENES Literal (unabhaengiges Orakel gegen
# ein verkuerztes "01_Positionen ... 10_OPOS"), Reihenfolge wie der Engine-Vertrag.
.mappe_blaetter <- c("01_Positionen", "02_Zahlungsprofile", "03_Stammdaten_Eroeffnung",
                     "04_Annahmen", "05_Finanzierungsplan", "06_Verteilungen",
                     "07_Korrelation", "08_Steuern", "09_Darlehen", "10_OPOS")

.mk_umgebung <- function() {
  list(r_version        = as.character(getRversion()),
       plattform        = "testplattform",
       git_sha          = NA_character_,
       git_dirty        = NA,
       renv_lock_sha256 = NA_character_)
}

test_that("der Engine-Vertrag nennt alle zehn Blattnamen der Mappe vollstaendig (keine verborgene Schema-Drift der acht Zwischenblaetter)", {
  vd <- verfahrensdoku_erstellen(umgebung = .mk_umgebung())
  txt <- tolower(paste(vd$allgemeine_beschreibung$aspekt,
                       vd$allgemeine_beschreibung$beschreibung, collapse = " | "))

  for (blatt in .mappe_blaetter) {
    expect_true(any(grepl(tolower(blatt), txt, fixed = TRUE)),
                info = paste0("Blattname im Engine-Vertrag der Verfahrensdoku nicht dokumentiert: ", blatt))
  }
})
