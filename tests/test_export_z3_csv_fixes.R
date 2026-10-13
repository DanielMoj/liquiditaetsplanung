# ============================================================================
#  tests/test_export_z3_csv_fixes.R  —  Regression fuer die Review-Fixes an
#  R/z3.R (phase-13-z3-csv). Ergaenzt tests/test_export_z3_csv.R (unberuehrt);
#  sperrt gezielt zwei Verhaltensaenderungen, die die Spezifikation nicht prueft:
#
#    (A) Idempotenz (High): ein wiederholter Export in ein bestehendes
#        Zielverzeichnis darf keine Block-CSV eines FRUEHEREN Ergebnis-Satzes
#        liegenlassen; index.xml/PDF anderer Schichten muessen ueberleben.
#    (B) Quoting-freier Vertrag (Medium): eine Textzelle mit ';', '"' oder
#        einem Zeilenumbruch bricht das CSV -> benannter Fehler statt Korruption.
#    (C) Fehleratomaritaet (High, Audit-Befund 2): ein scheiternder Export
#        laesst das bestehende Buendel byte-identisch zurueck und hinterlaesst
#        kein Temp-Verzeichnis.
#    (D) Format-Heimat (Critical, Audit-Befund 1): .z3_zahl ist reine
#        Delegation an fmt_maschine (R/format.R) — eine Wahrheit.
#
#  LAUFZEIT-BUDGET (Fix): diese Regressionen sind STRUKTURELL — Idempotenz und
#  Quoting haengen nicht an echten Tier-1-Zahlen. Darum arbeitet die Datei mit
#  HANDGEBAUTEN Minimal-Fixtures (ein winziges forecast-Ergebnis + ein
#  liqui_scenario-Stub) und ruft WEDER scenario_from_workbook NOCH phase_forecast
#  NOCH openxlsx2 auf. Die schwester-Spec tests/test_export_z3_csv.R deckt die
#  echten Vertragszahlen (Tier-1-Roundtrip, Datentreue gegen die xlsx-
#  Ruecklesung) auf der realen Mappe ab; sie ist die EINE teure Naht. Ein
#  zweiter Mappen-Lade- und Modell-Lauf hier waere reine Wiederholung — er
#  brachte die volle DOD-Suite an ihr Zeit-/Speicher-Limit (Mappe via openxlsx2
#  ist die teuerste Zutat), ohne Deckung hinzuzugewinnen. Deterministisch, kein
#  RNG-abhaengiges Orakel, kein skip()/succeed().
#
#  MINIMAL-SOURCE (Suite-Entlastung): der gepruefte Pfad — export_bloecke_bauen
#  fuer den forecast-Block (R/export.R) plus die .z3_*-Serialisierer (R/z3.R) —
#  ist reines Basis-R und ruft KEINE Funktion aus kernel/engine.R oder
#  kernel/distributions.R (verifiziert: der forecast-Zweig in export_bloecke_
#  bauen und .z3_zahl/.z3_zeilen/.z3_schreiben nutzen nur base + formatC). Diese
#  Datei sourct darum NUR R/export.R und R/z3.R — nicht die ~800 Zeilen Kernel,
#  deren Parsen und Funktions-Umgebungen in jeder DOD-Suite unnoetig Speicher/
#  Zeit binden. kernel/engine.R dient hier nur noch als Wurzel-Anker fuer die
#  Repo-Suche (file.exists), wird aber NICHT gesourct.
# ============================================================================

.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
# Bewusst OHNE kernel/engine.R + kernel/distributions.R: der Minimal-Fixture-
# Pfad braucht sie nicht (s. Kopf), und ihr Wegfall entlastet die DOD-Suite.
source(file.path(.root, "R", "export.R"))   # export_bloecke_bauen (EINE Quelle)
source(file.path(.root, "R", "z3.R"))

# ---------------------------------------------------------------------------
# Minimal-Fixtures: das kleinste Ergebnisobjekt, das export_bloecke_bauen als
# forecast-Block akzeptiert (cash-Matrix mit Position-Zeilennamen + kpis mit
# den sechs Vertragsschluesseln), und ein liqui_scenario-Stub mit genau den
# Feldern, die export_z3_csv fuer eingaben.csv liest. Keine Engine, keine Mappe.
# ---------------------------------------------------------------------------
.mini_forecast <- function()
  list(cash = matrix(c(-100, 200, -50, 150), nrow = 2L,
                     dimnames = list(c("Kasse", "Bank"), c("M1", "M2"))),
       kpis = list(trough = -100, trough_month = 1L, maxKK = 0,
                   months_below = 0L, result = 200, end = 200),
       validation = list(erg_eng = -50))

.mini_scenario <- function()
  structure(list(workbook = "Fake_Mappe.xlsx", name = "z3fixIdem",
                 mdl = list(HMAX = 2L, open_cash = 0, kk_zins = 0.065,
                            kk_limit = 1000),
                 sim = list(paths = 100L, seed = 1L),
                 assume = list()),
            class = "liqui_scenario")

# ---------------------------------------------------------------------------
# (A) Idempotenz: alte *.csv weg, Nicht-CSV (index.xml) bleibt, Rueckgabe deckt
#     sich mit dem tatsaechlichen CSV-Bestand des Verzeichnisses.
# ---------------------------------------------------------------------------
test_that("Idempotenz: Wiederholung entfernt stale Block-CSV, verschont index.xml", {
  sc  <- .mini_scenario()
  fc  <- .mini_forecast()
  out <- file.path(tempdir(), "z3-csv-fix-idem")
  unlink(out, recursive = TRUE)

  export_z3_csv(list(forecast = fc), sc, out)

  # Ein Block-CSV, das ein forecast-only-Lauf NICHT erzeugt (Simulations-Block),
  # sowie eine Nicht-CSV-Datei einer anderen Schicht in dasselbe Verzeichnis.
  stale <- file.path(out, "sim_monate.csv")
  keep  <- file.path(out, "index.xml")
  writeLines("kennzahl;wert", stale)
  writeLines("<index/>", keep)

  r2 <- export_z3_csv(list(forecast = fc), sc, out)

  expect_false(file.exists(stale))   # stale Block-CSV wurde entfernt
  expect_true(file.exists(keep))     # Nicht-CSV der Schicht 2/3 unberuehrt
  # Rueckgabe-dateien == tatsaechlicher CSV-Bestand (kein Rest, keine Luecke).
  expect_setequal(basename(r2$dateien),
                  list.files(out, pattern = "\\.csv$"))
  expect_false("sim_monate.csv" %in% basename(r2$dateien))
})

# ---------------------------------------------------------------------------
# (B) Quoting-freier Vertrag: Sonderzeichen in Textzellen -> benannter Fehler.
#     Direkt an .z3_zeilen (die Projektions-Schicht), damit der Waechter ohne
#     Umweg ueber eine Ergebnisliste geprueft wird.
# ---------------------------------------------------------------------------
test_that("Quoting-Vertrag: Semikolon/Quote/Zeilenumbruch in Textzelle -> benannter Fehler", {
  # Semikolon (der Trenner selbst).
  expect_error(.z3_zeilen(data.frame(a = c("ok", "b;ad"), stringsAsFactors = FALSE)),
               "Sonderzeichen")
  # Anfuehrungszeichen.
  expect_error(.z3_zeilen(data.frame(a = c("ok", "b\"ad"), stringsAsFactors = FALSE)),
               "Sonderzeichen")
  # Zeilenumbruch (LF) und Wagenruecklauf (CR).
  expect_error(.z3_zeilen(data.frame(a = "zeile1\nzeile2", stringsAsFactors = FALSE)),
               "Sonderzeichen")
  expect_error(.z3_zeilen(data.frame(a = "zeile1\rzeile2", stringsAsFactors = FALSE)),
               "Sonderzeichen")
})

test_that("Quoting-Vertrag: saubere Zellen laufen unveraendert durch", {
  z <- .z3_zeilen(data.frame(Position = c("Kasse", "Bank"),
                             M1 = c(-5135990, 0.065),
                             stringsAsFactors = FALSE))
  expect_identical(z[[1L]], "Position;M1")
  expect_identical(z[[2L]], "Kasse;-5135990")
  expect_identical(z[[3L]], "Bank;0,065")     # Dezimalkomma, kein Punkt
  expect_false(any(grepl("[0-9][.][0-9]", z)))
})

# ---------------------------------------------------------------------------
# (C) Fehleratomaritaet negativ: ein zweiter Export, dessen ZWEITER Block
#     (vergleich, nach sim_monate) eine Semikolon-Textzelle traegt, bricht mit
#     dem benannten Sonderzeichen-Fehler ab, BEVOR am Zielverzeichnis etwas
#     geloescht oder geschrieben wurde: das Buendel aus Satz A bleibt
#     byte-identisch (Vorher/Nachher-Vergleich der Rohbytes je Datei), und es
#     bleibt kein .z3-tmp-Temp-Verzeichnis zurueck.
# ---------------------------------------------------------------------------
test_that("Atomaritaet: fehlender Export laesst Satz-A-Buendel byte-identisch, kein Temp-Rest", {
  sc  <- .mini_scenario()
  out <- file.path(tempdir(), "z3-csv-fix-atomar")
  unlink(out, recursive = TRUE)

  # Satz A (gueltig): Buendel schreiben, Rohbytes je Datei einfrieren.
  export_z3_csv(list(forecast = .mini_forecast()), sc, out)
  bytes <- function(pf) readBin(pf, "raw", n = file.info(pf)$size)
  dateien_vor <- sort(list.files(out, pattern = "\\.csv$"))
  bytes_vor   <- lapply(file.path(out, dateien_vor), bytes)

  # Satz B: der ZWEITE Block (vergleich) traegt eine Semikolon-Textzelle;
  #         der erste Block (sim_monate) ist sauber — der Fehler feuert also
  #         erst NACH erfolgreicher Projektion des ersten Blocks und beweist,
  #         dass ALLE Zeilen vor jedem Seiteneffekt gebaut werden.
  satz_b <- list(
    sim_monate = list(table   = data.frame(monat = c("M1", "M2"),
                                           wert  = c(1, 2),
                                           stringsAsFactors = FALSE)),
    vergleich  = list(tabelle = data.frame(szenario = c("Basis", "b;oese"),
                                           wert     = c(1, 2),
                                           stringsAsFactors = FALSE)))
  expect_error(export_z3_csv(satz_b, sc, out), "Sonderzeichen")

  # Altbestand byte-identisch: gleiche Dateimenge, gleiche Rohbytes je Datei.
  expect_identical(sort(list.files(out, pattern = "\\.csv$")), dateien_vor)
  bytes_nach <- lapply(file.path(out, dateien_vor), bytes)
  expect_identical(bytes_nach, bytes_vor)
  # Kein Temp-Verzeichnis-Rest (list.dirs listet auch versteckte Verzeichnisse).
  expect_false(any(grepl(".z3-tmp-", list.dirs(out, recursive = TRUE),
                         fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# (D) Format-Heimat: .z3_zahl(x) ist identisch zu fmt_maschine(x) (R/format.R)
#     — reine Delegation, keine lokale Formatierung in der Exportschicht.
#     Wertevektor deckt den Dezimal-Anker (0.065), den Tier-1-Tiefpunkt
#     (-5135990) und 1e-12-nahe Dezimalen ab.
# ---------------------------------------------------------------------------
test_that("fmt_maschine-Delegation: .z3_zahl identisch zur zentralen Format-Heimat", {
  expect_true(exists("fmt_maschine", mode = "function"))
  x <- c(0.065, -5135990, 2672411, 1e-12, -1e-12,
         1 + 1e-12, 0.065000000000001, 0, -0.5)
  expect_identical(.z3_zahl(x), fmt_maschine(x))
  # Stichprobe gegen Literale: Dezimalkomma, keine Gruppierung, kein Exponent.
  expect_identical(fmt_maschine(c(0.065, -5135990)), c("0,065", "-5135990"))
})

# ---------------------------------------------------------------------------
# SUITE-SPEICHERHYGIENE (kein Test, Nebenwirkung fuer die DOD-Suite): diese
# Datei laeuft in der alphabetischen test_dir-Reihenfolge unmittelbar NACH der
# export-schweren Schwester-Spec tests/test_export_z3_csv.R ("csv." < "csv_"),
# die openxlsx2 anzieht (export_xlsx + wb_load in der Datentreue-Pruefung).
# openxlsx2 haelt seinen Arbeitsspeicher hinter externen Zeigern (Rcpp/pugixml),
# deren Umfang R's automatischer gc-Ausloeser UNTERZAEHLT — die C++-Bytes koennen
# bis zum Prozessende liegenbleiben und die Basis-RSS anheben. Ein expliziter
# gc(full = TRUE) an genau dieser Naht — die Umgebung der Spec ist hier bereits
# unreferenziert — stoesst die Finalizer an und gibt den Speicher frei, BEVOR die
# Suite in den schweren E2E-/Monte-Carlo-Schwanz laeuft. Deterministisch, ohne
# Einfluss auf ein Testergebnis.
invisible(gc(full = TRUE))
