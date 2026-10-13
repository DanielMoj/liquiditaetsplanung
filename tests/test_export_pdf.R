# ============================================================================
#  tests/test_export_pdf.R  —  SPEZIFIKATION (rot zuerst) fuer phase-09-pdf
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: R/export.R
#  existiert und laesst sich sourcen, aber export_pdf existiert dort noch
#  nicht — jeder Test faellt am fehlenden Symbol. Kein skip(), kein
#  succeed(), keine Existenz-Weiche.
#
#  Vertrag (handoff/phase-09-pdf.json; KEIN Legacy-Vorbild):
#   - export_pdf(ergebnisse, pfad): benannte Liste -> EIN pdf-Bericht ueber
#     das Quarto-CLI im typst-Format (LaTeX-frei); je Eintrag ein Abschnitt
#     in EINGABEREIHENFOLGE (Ueberschrift Ebene 1 = Vertragsname + GENAU
#     EINE Pipe-Tabelle); Blockformen aus export_bloecke_bauen, Zellen aus
#     der gemeinsamen Zellregel export_zellen_als_text (as.character).
#     Rueckgabe list(pfad, abschnitte); abschnitte identical zu
#     export_xlsx(...)$blaetter derselben Eingabe.
#   - LAYOUT-REGIME (gepinnt, Teil des Vertrags): typst mit margin
#     x=1cm/y=1.2cm und fontsize 9pt — darunter brechen DATENZEILEN nicht
#     um (empirisch verprobt bis 13 Spalten und 17-Zeichen-Zellen); nur
#     die Kopfzeile der 13-Spalten-vergleich-Tabelle darf wickeln und
#     bleibt deshalb ungepinnt.
#   - deterministisch: KEIN Datum/Zeitstempel im Inhalt — pdf_text zweier
#     Renders identisch (Datei-Bytes/PDF-Metadaten duerfen abweichen).
#   - Fehler (aus dem Bauer): unbekannter Name (im Fehlertext), leere
#     Liste, unbenannte Liste.
#
#  Orakel-Politik — ZEILENFORM: pdftools::pdf_text, Zeilen ueber alle
#  Seiten, typst-Minus U+2212 zu ASCII normalisiert, jede Zeile
#  whitespace-gestript. Eine DATENZEILE der Tabelle ist dann die exakte
#  Konkatenation ihrer Zellen — EIN identical-Match pinnt Paarung UND
#  Spaltenreihenfolge. Erwartungszeilen werden per paste0 aus EIGENEN
#  String-Literalen gebaut (Key-Listen + wert-Text-Vektoren unabhaengig
#  vom Stub-Objekt gehalten); die Stubs tragen Koeder (guv, T_*/Theta,
#  phase/scenario), die in KEINER Zeile auftauchen duerfen. Die
#  Fixture-Naht ist EIN memoisierter echter phase_sim_monate-Lauf
#  (paths = 12, Seed 2026): ihre Erwartungszeilen entstehen aus
#  as.character der echten table-Spalten und matchen EXAKT — die
#  Zellregel macht die Ruecklesung stringgenau, keine Toleranz.
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
source(file.path(.root, "R", "phase_simulation.R"))
source(file.path(.root, "R", "phase_sim_monate.R"))
source(file.path(.root, "R", "export.R"))   # export_pdf fehlt dort: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Gemeinsame Leseform: gestripte Zeilen (U+2212 -> '-', Whitespace raus,
# Leerzeilen weg) ueber alle Seiten in Dokumentreihenfolge. REINE Zahl-
# Zeilen sind typst-Seitenfusszeilen (Seitennummern) und fliegen raus —
# jede echte Datenzeile traegt einen Text-Praefix (kennzahl/Position/
# Monat/szenario), eine legitime nackte Zahlzeile gibt es nicht.
.lies_zeilen <- function(pfad) {
  tx <- pdftools::pdf_text(pfad)
  z <- unlist(strsplit(paste(tx, collapse = "\n"), "\n"))
  z <- gsub("−", "-", z)
  z <- gsub("[[:space:]]+", "", z)
  z <- z[nzchar(z)]
  z[!grepl("^[0-9]+$", z)]
}

# ---------------------------------------------------------------------------
# UNABHAENGIG gehaltene Erwartungen: Key-Reihenfolgen + wert-TEXTE als eigene
# String-Literale (paste0 baut daraus die erwarteten Zeilen — Mechanik, keine
# Ableitung aus den Stubs). Alle Zahlen der fuenf Stubs sind PAARWEISE
# UNVERWECHSELBAR; frisch erfunden, keine Tier-1-Werte.
# ---------------------------------------------------------------------------
.deckung_keys <- c("trough_median", "trough_p05", "trough_p95", "P_breach",
                   "P_solvent", "P_breach_pct", "P_solvent_pct",
                   "insO_erfuellt", "kk_limit", "seed", "S")
.deckung_txt  <- c("-633.5", "-1133.25", "393.75", "0.125", "0.875",
                   "12.5", "87.5", "1", "727", "131", "7")

.varianz_keys <- c("var_gesamt", "var_epi", "var_ale", "var_inter",
                   "sd_gesamt", "sd_epi", "sd_ale",
                   "anteil_epi", "anteil_ale", "anteil_inter", "S", "seed")
.varianz_txt  <- c("4489", "2704", "1764", "21", "67", "52", "42",
                   "0.64", "0.32", "0.04", "22", "343")

.kpi_keys <- c("trough", "trough_month", "maxKK", "months_below", "result", "end")
.kpi_txt  <- c("-7300.5", "5", "555.25", "6", "999.75", "1666.5")

.deckung_zeilen <- paste0(.deckung_keys, .deckung_txt)
.varianz_zeilen <- paste0(.varianz_keys, .varianz_txt)
.kpi_zeilen     <- paste0(.kpi_keys, .kpi_txt)
.fc_zeilen  <- c("Endbestand1310.5-2430.253540.75",
                 "KK_Inanspruchnahme640.5-755.25860.75")
.mon_zeilen <- c("M1-1010.5-310.5420.50.45",
                 "M2-940.25-230.25530.250.25",
                 "M3-850.7577.5635.750.15")
.vgl_zeilen <- c("Epsilon-5555.5-840.5-1205.75460.50.40.64060TRUE51515921",
                 "Zeta-6666.25-950.25-1610.5570.750.60.46040FALSE77517923")

# ---------------------------------------------------------------------------
# Synthetische Stubs je Vertragstyp (Muster der Schwester-Specs).
# ---------------------------------------------------------------------------
.fc_cash <- matrix(c(1310.5, -2430.25, 3540.75,
                     640.5,  -755.25,   860.75),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("Endbestand", "KK_Inanspruchnahme"),
                                   c("M1", "M2", "M3")))
.fc <- list(phase = "forecast", scenario = "StubForecastPdf",
            guv  = matrix(7777.5, 1, 1, dimnames = list("NurGuV", "M1")),
            cash = .fc_cash,
            kpis = list(trough = -7300.5, trough_month = 5, maxKK = 555.25,
                        months_below = 6, result = 999.75, end = 1666.5))

.mon_tab <- data.frame(
  Monat   = c("M1", "M2", "M3"),
  P05     = c(-1010.5, -940.25, -850.75),
  P50     = c(-310.5, -230.25,   77.5),
  P95     = c( 420.5,  530.25,  635.75),
  P_unter = c(0.45, 0.25, 0.15),
  stringsAsFactors = FALSE
)
.mon <- list(phase = "sim_monate", scenario = "StubMonatePdf", table = .mon_tab)

.deckung <- list(phase = "sim_deckung", scenario = "StubDeckungPdf",
                 trough_median = -633.5, trough_p05 = -1133.25,
                 trough_p95 = 393.75,
                 P_breach = 0.125, P_solvent = 0.875,
                 P_breach_pct = 12.5, P_solvent_pct = 87.5,
                 insO_erfuellt = TRUE, kk_limit = 727, seed = 131, S = 7)

.varianz <- list(phase = "varianzzerlegung", scenario = "StubVarianzPdf",
                 T_full = c(-7301, -7302), T_epi = c(-7303, -7304),
                 T_ale  = c(-7305, -7306),
                 Theta  = matrix(0.55, 2, 1, dimnames = list(NULL, "menge")),
                 var_gesamt = 4489, var_epi = 2704, var_ale = 1764,
                 var_inter = 21,
                 sd_gesamt = 67, sd_epi = 52, sd_ale = 42,
                 anteil_epi = 0.64, anteil_ale = 0.32, anteil_inter = 0.04,
                 S = 22, seed = 343)

.vgl_tab <- data.frame(
  szenario      = c("Epsilon", "Zeta"),
  plan_trough   = c(-5555.5, -6666.25),
  trough_median = c(-840.5, -950.25),
  trough_p05    = c(-1205.75, -1610.5),
  trough_p95    = c(460.5, 570.75),
  P_breach      = c(0.4, 0.6),
  P_solvent     = c(0.6, 0.4),
  P_breach_pct  = c(40, 60),
  P_solvent_pct = c(60, 40),
  insO_erfuellt = c(TRUE, FALSE),
  kk_limit      = c(515, 775),
  S             = c(15, 17),
  seed          = c(921, 923),
  stringsAsFactors = FALSE
)
.vgl <- list(phase = "szenarienvergleich", n = 2L, tabelle = .vgl_tab)

# Eingabeliste BEWUSST anders gemischt als die xlsx-/docx-Specs: nur echte
# Eingabereihenfolgen-Treue reproduziert diese Abschnittsfolge.
.erg <- list(sim_deckung      = .deckung,
             vergleich        = .vgl,
             varianzzerlegung = .varianz,
             forecast         = .fc,
             sim_monate       = .mon)
.abschnitte_erwartet <- c("sim_deckung", "vergleich", "varianzzerlegung",
                          "forecast", "forecast_kpis", "sim_monate")

# EIN gemeinsamer Export + Ruecklesung, LAZY memoisiert; die Fixture-Naht
# memoisiert ihren einen teuren phase_sim_monate-Lauf genauso.
.cache <- new.env(parent = emptyenv())
delayedAssign("haupt", local({
  tf <- tempfile(fileext = ".pdf")
  r  <- export_pdf(.erg, tf)
  list(rueck = r, pfad = tf, zeilen = .lies_zeilen(tf))
}), assign.env = .cache)
.scfix <- scenario_from_workbook(WB, name = "ExportSpecPdf",
                                 sim = list(paths = 12L, seed = 2026))
delayedAssign("mon_fix", phase_sim_monate(.scfix), assign.env = .cache)
haupt   <- function() .cache$haupt
mon_fix <- function() .cache$mon_fix

# ===========================================================================
#  (1) Rueckgabe-, Magic- und Abschnittsfolgen-Vertrag
# ===========================================================================

test_that("Rueckgabe: pfad existiert (%PDF-Magic), abschnitte == Eingabereihenfolge == Ueberschriftenfolge", {
  h <- haupt()
  expect_identical(h$rueck$pfad, h$pfad)
  expect_true(file.exists(h$rueck$pfad))
  expect_identical(rawToChar(readBin(h$pfad, "raw", 4L)), "%PDF")
  expect_identical(h$rueck$abschnitte, .abschnitte_erwartet)
  idx <- vapply(.abschnitte_erwartet,
                function(n) which(h$zeilen == n)[1], numeric(1))
  expect_false(anyNA(idx))
  expect_true(all(diff(idx) > 0))
})

# ===========================================================================
#  (2) Datenzeilen-Orakel je Abschnitt (Paarung + Spaltenreihenfolge in
#      EINEM Match; Kopf-Pins nur fuer schmale Tabellen)
# ===========================================================================

test_that("sim_deckung: Key/Wert-Zeilen exakt (insO-Zeile 'insO_erfuellt1'), Kopf 'kennzahlwert'", {
  zs <- haupt()$zeilen
  expect_true(any(zs == "kennzahlwert"))
  for (z in .deckung_zeilen) expect_true(any(zs == z), info = z)
  expect_true(any(zs == "insO_erfuellt1"))
  expect_false(any(grepl("StubDeckungPdf", zs, fixed = TRUE)))
})

test_that("varianzzerlegung: Key/Wert-Zeilen exakt; T_*/Theta-Koeder nirgends", {
  zs <- haupt()$zeilen
  for (z in .varianz_zeilen) expect_true(any(zs == z), info = z)
  for (k in c("7301", "7302", "7303", "7304", "7305", "7306"))
    expect_false(any(grepl(k, zs, fixed = TRUE)), info = k)
  # Theta-Koeder SEKTIONSSCHARF statt per Substring (ein Substring wie
  # '0.55' kann zufaellig an einer Zellgrenze einer konkatenierten Zeile
  # entstehen): zwischen der varianz-Ueberschrift und der naechsten
  # Ueberschrift stehen EXAKT der Kopf + die 12 Vertragszeilen — jeder
  # exportierte Koeder (Theta-Matrix, T_*-Vektoren) waere eine
  # zusaetzliche Zeile und faellt am identical.
  i0 <- which(zs == "varianzzerlegung")[1]
  i1 <- which(zs == "forecast")[1]
  expect_identical(zs[(i0 + 1):(i1 - 1)], c("kennzahlwert", .varianz_zeilen))
})

test_that("forecast: cash-Zeilen (Position + M1..M3) und kpis-Zeilen exakt; guv-Koeder nirgends", {
  zs <- haupt()$zeilen
  expect_true(any(zs == "PositionM1M2M3"))
  for (z in .fc_zeilen)  expect_true(any(zs == z), info = z)
  for (z in .kpi_zeilen) expect_true(any(zs == z), info = z)
  expect_false(any(grepl("7777.5", zs, fixed = TRUE)))
  expect_false(any(zs == "guv"))
})

test_that("sim_monate: Kopf + drei Datenzeilen exakt", {
  zs <- haupt()$zeilen
  expect_true(any(zs == "MonatP05P50P95P_unter"))
  for (z in .mon_zeilen) expect_true(any(zs == z), info = z)
})

test_that("vergleich: beide 13-Spalten-Datenzeilen exakt (inkl. TRUE/FALSE-Rundreise)", {
  zs <- haupt()$zeilen
  for (z in .vgl_zeilen) expect_true(any(zs == z), info = z)
})

# ===========================================================================
#  (3) Asymmetrie-Ordnung: seed VOR S (deckung), S VOR seed (varianz)
# ===========================================================================

test_that("Asymmetrie: Zeile seed131 VOR S7 (deckung); Zeile S22 VOR seed343 (varianz)", {
  zs <- haupt()$zeilen
  expect_true(which(zs == "seed131")[1] < which(zs == "S7")[1])
  expect_true(which(zs == "S22")[1]    < which(zs == "seed343")[1])
})

# ===========================================================================
#  (4) Kreuz-Check gegen Abschnitts-/Objektvertauschung
# ===========================================================================

test_that("Kreuz-Check: keine Kennzahl-Zeile traegt den Wert des jeweils anderen Stubs", {
  zs <- haupt()$zeilen
  # Nur die DISJUNKTEN Keys kreuzen: S und seed stehen in beiden Key-Listen —
  # paste0('S', deckung_txt) ergaebe die ECHTE deckung-Zeile 'S7'.
  kreuz <- c(paste0(.deckung_keys[1:9],  .varianz_txt[1:9]),
             paste0(.varianz_keys[1:10], .deckung_txt[1:10]))
  for (z in kreuz) expect_false(any(zs == z), info = z)
})

# ===========================================================================
#  (5) Symmetrie zu export_xlsx: gemeinsamer Block-Bauer sichtbar gemacht
# ===========================================================================

test_that("Symmetrie: export_pdf$abschnitte == export_xlsx$blaetter derselben Eingabe", {
  rx <- export_xlsx(.erg, tempfile(fileext = ".xlsx"))
  expect_identical(haupt()$rueck$abschnitte, rx$blaetter)
})

# ===========================================================================
#  (6) Determinismus: pdf_text haengt nur an den Eingaben
# ===========================================================================

test_that("Determinismus: zwei Renders -> identischer pdf_text (Datei-Bytes duerfen abweichen)", {
  tfa <- tempfile(fileext = ".pdf")
  tfb <- tempfile(fileext = ".pdf")
  ra  <- export_pdf(.erg, tfa)
  rb  <- export_pdf(.erg, tfb)
  expect_identical(ra$abschnitte, rb$abschnitte)
  expect_identical(pdftools::pdf_text(tfa), pdftools::pdf_text(tfb))
})

# ===========================================================================
#  (7) Fixture-Naht: ein kleiner echter phase_sim_monate-Lauf, stringgenau
# ===========================================================================

test_that("Fixture-Naht: Zeilen des echten Laufs erscheinen EXAKT (as.character-Zellregel) und in Tabellenreihenfolge", {
  out <- mon_fix()
  tf  <- tempfile(fileext = ".pdf")
  r   <- export_pdf(list(sim_monate = out), tf)
  expect_identical(r$abschnitte, "sim_monate")
  zs  <- .lies_zeilen(tf)
  tb  <- out$table
  erwartet <- vapply(seq_len(nrow(tb)), function(i)
    paste0(tb$Monat[i], as.character(tb$P05[i]), as.character(tb$P50[i]),
           as.character(tb$P95[i]), as.character(tb$P_unter[i])),
    character(1))
  idx <- vapply(erwartet, function(z) which(zs == z)[1], numeric(1))
  expect_false(anyNA(idx))
  expect_true(all(diff(idx) > 0))
  expect_true(any(zs == "MonatP05P50P95P_unter"))
})

# ===========================================================================
#  (8) Fehlerfaelle
# ===========================================================================

test_that("Fehlerfaelle: unbekannter Name (im Fehlertext), leere Liste, unbenannte Liste", {
  tf <- tempfile(fileext = ".pdf")
  expect_error(export_pdf(list(quark = .mon), tf), "quark")
  expect_error(export_pdf(list(sim_monate = .mon, quark = .deckung), tf), "quark")
  expect_error(export_pdf(list(), tf))
  expect_error(export_pdf(list(.mon), tf))
})
