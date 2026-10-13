# ============================================================================
#  tests/test_export_xlsx.R  —  SPEZIFIKATION (rot zuerst) fuer phase-09-xlsx
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/export.R
#  existiert noch nicht -> source() bricht ab; jeder Test spezifiziert
#  zusaetzlich das Zielverhalten). Der Builder erfuellt sie erst mit der
#  Implementierung. Kein skip(), kein succeed(), keine Existenz-Weiche.
#
#  Vertrag (handoff/phase-09-xlsx.json; KEIN Legacy-Vorbild — Zielort
#  R/export.R laut ARCHITECTURE.md):
#   - export_xlsx(ergebnisse, pfad): ergebnisse = BENANNTE Liste von Phasen-
#     Ergebnisobjekten; schreibt ueber openxlsx2 je Eintrag ein Blatt gleichen
#     Namens in EINGABEREIHENFOLGE — nur Zahlen/Strings, unformatiert (fmt()
#     bleibt der UI); keine Simulation, kein Grafikgeraet, deterministisch
#     (kein Zeitstempel im ZELLinhalt). Rueckgabe: list(pfad = <geschriebener
#     Pfad>, blaetter = <character der Blattnamen in Schreibreihenfolge>).
#   - Blattinhalt je unterstuetztem Namen:
#       * forecast         — BLATTSTRUKTUR-ENTSCHEIDUNG (hier gepinnt, im
#         Handoff dokumentiert): der Eintrag zaehlt als ZWEI Blaetter.
#         Blatt "forecast" = cash-Matrix als data.frame (Zeilennamen als
#         erste Spalte "Position", danach M1..MH); Blatt "forecast_kpis" =
#         kpis als Key/Wert-Paare (kennzahl, wert) in Vertragsreihenfolge
#         trough, trough_month, maxKK, months_below, result, end.
#       * sim_monate       — table (data.frame) unveraendert.
#       * sim_deckung      — zweispaltiges Key/Wert-Blatt (kennzahl, wert)
#         mit den skalaren Kennzahlen in Vertragsreihenfolge des
#         Ergebnisobjekts (deckung_from_out + kk_limit/seed/S):
#         trough_median, trough_p05, trough_p95, P_breach, P_solvent,
#         P_breach_pct, P_solvent_pct, insO_erfuellt, kk_limit, seed, S.
#         insO_erfuellt als 0/1 (as.numeric), damit die wert-Spalte homogen
#         numerisch bleibt (gepinnte Entscheidung).
#       * varianzzerlegung — Key/Wert-Blatt der var_/sd_/anteil_-Felder plus
#         S/seed in Vertragsreihenfolge des Ergebnisobjekts: var_gesamt,
#         var_epi, var_ale, var_inter, sd_gesamt, sd_epi, sd_ale,
#         anteil_epi, anteil_ale, anteil_inter, S, seed (S VOR seed —
#         anders als sim_deckung mit seed VOR S; die Asymmetrie killt
#         zusaetzlich jede Blatt-/Objektvertauschung).
#       * vergleich        — tabelle (data.frame) unveraendert.
#   - Fehler: unbekannter Listenname (Fehlertext enthaelt den Namen), leere
#     Liste, unbenannte Liste -> expect_error.
#
#  Orakel-Politik: RUECKLESE-Orakel an synthetischen, paarweise
#  unverwechselbaren Stubs (KEINE Engine im Stub-Teil, KEINE Tier-1-Werte;
#  den Tier-1-Anker prueft der Golden-Master in run_all.R). Der Test haelt
#  die Erwartungen (Key-Reihenfolgen, wert-Vektoren, Zellwerte) als EIGENE
#  Literale UNABHAENGIG vom Stub-Objekt und liest die Datei mit openxlsx2
#  zurueck (wb_load + wb_to_df — die im Repo etablierte Leseform, s.
#  tests/test_phase_vert_mutgate.R): all.equal 1e-12 fuer Zahlen, identical
#  fuer Strings. Die Stubs tragen bewusst ZUSAETZLICHE Vertragsfelder
#  (guv, T_full/T_epi/T_ale/Theta, phase/scenario), die NICHT im Blatt
#  landen — die exakt gepinnten Spalten/Key-Listen beweisen die selektive
#  Projektion. Kreuz-Check deckung <-> varianz killt Blatt-/Objekt-
#  vertauschung; der Determinismus-Lauf vergleicht die ZELLinhalte zweier
#  Exporte (Datei-Bytes duerfen abweichen, der Zellinhalt nicht). Die
#  Fixture-Naht ist EIN kleiner echter phase_sim_monate-Lauf (paths = 12,
#  Seed 2026, lazy memoisiert wie die Schwester-Specs) mit Toleranz 1e-9
#  wie der monate-Anker.
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
source(file.path(.root, "R", "export.R"))   # <- existiert noch nicht: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Gemeinsame Leseform: alle Blaetter einer Datei als benannte Liste von
# data.frames in Datei-Reihenfolge (wb_load + wb_to_df, Kopfzeile = Spalten).
.lies_blaetter <- function(pfad) {
  wb <- openxlsx2::wb_load(pfad)
  bl <- unname(openxlsx2::wb_get_sheet_names(wb))
  setNames(lapply(bl, function(b) openxlsx2::wb_to_df(wb, sheet = b)), bl)
}

# ---------------------------------------------------------------------------
# UNABHAENGIG gehaltene Erwartungen (eigene Literale, NICHT aus den Stubs
# abgeleitet): Key-Reihenfolgen der Key/Wert-Blaetter und deren wert-Vektoren.
# Alle Zahlen der fuenf Stubs sind PAARWEISE UNVERWECHSELBAR (keine Zahl
# kommt in zwei Vertragsobjekten vor) — nur so hat der Kreuz-Check Kraft.
# ---------------------------------------------------------------------------
.deckung_keys <- c("trough_median", "trough_p05", "trough_p95", "P_breach",
                   "P_solvent", "P_breach_pct", "P_solvent_pct",
                   "insO_erfuellt", "kk_limit", "seed", "S")
.deckung_wert <- c(-411.5, -911.25, 191.75, 0.25, 0.75, 25, 75, 1, 505, 111, 8)

.varianz_keys <- c("var_gesamt", "var_epi", "var_ale", "var_inter",
                   "sd_gesamt", "sd_epi", "sd_ale",
                   "anteil_epi", "anteil_ale", "anteil_inter", "S", "seed")
.varianz_wert <- c(4096, 2500, 1296, 300, 64, 50, 36, 0.625, 0.3125, 0.0625,
                   16, 222)

.kpi_keys <- c("trough", "trough_month", "maxKK", "months_below", "result", "end")
.kpi_wert <- c(-5100.5, 2, 333.25, 3, 777.75, 1444.5)

# ---------------------------------------------------------------------------
# Synthetische Stubs je Vertragstyp (Muster .mk_sim_out aus test_compare.R:
# Feldvertrag nachbauen, keine Engine, kein Seed-Lauf, keine Tier-1-Werte).
# ---------------------------------------------------------------------------

# forecast-Stub (phase_forecast-Vertrag): kleine cash-Matrix mit Zeilennamen
# + kpis-Liste; guv ist ein Koeder, der NICHT exportiert werden darf.
.fc_cash <- matrix(c(1100.5, -2200.25, 3300.75,
                     440.5,  -550.25,   660.75),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("Endbestand", "KK_Inanspruchnahme"),
                                   c("M1", "M2", "M3")))
.fc <- list(phase = "forecast", scenario = "StubForecast",
            guv  = matrix(9999.5, 1, 1, dimnames = list("NurGuV", "M1")),
            cash = .fc_cash,
            kpis = list(trough = -5100.5, trough_month = 2, maxKK = 333.25,
                        months_below = 3, result = 777.75, end = 1444.5))

# sim_monate-Stub (phase_sim_monate-Vertrag): table im Feldvertrag
# Monat/P05/P50/P95/P_unter.
.mon_tab <- data.frame(
  Monat   = c("M1", "M2", "M3"),
  P05     = c(-800.5, -730.25, -640.75),
  P50     = c(-100.5,  -20.25,   55.5),
  P95     = c( 210.5,  320.25,  415.75),
  P_unter = c(0.3, 0.15, 0.05),
  stringsAsFactors = FALSE
)
.mon <- list(phase = "sim_monate", scenario = "StubMonate", table = .mon_tab)

# sim_deckung-Stub (phase_sim_deckung-Vertrag): skalare Kennzahlen in
# Vertragsreihenfolge; phase/scenario duerfen NICHT im Blatt landen.
.deckung <- list(phase = "sim_deckung", scenario = "StubDeckung",
                 trough_median = -411.5, trough_p05 = -911.25,
                 trough_p95 = 191.75,
                 P_breach = 0.25, P_solvent = 0.75,
                 P_breach_pct = 25, P_solvent_pct = 75,
                 insO_erfuellt = TRUE, kk_limit = 505, seed = 111, S = 8)

# varianzzerlegung-Stub (phase_varianzzerlegung-Vertrag): T_*/Theta sind
# Koeder-Vektoren, die NICHT im Blatt landen duerfen.
.varianz <- list(phase = "varianzzerlegung", scenario = "StubVarianz",
                 T_full = c(-9101, -9102), T_epi = c(-9103, -9104),
                 T_ale  = c(-9105, -9106),
                 Theta  = matrix(0.5, 2, 1, dimnames = list(NULL, "menge")),
                 var_gesamt = 4096, var_epi = 2500, var_ale = 1296,
                 var_inter = 300,
                 sd_gesamt = 64, sd_epi = 50, sd_ale = 36,
                 anteil_epi = 0.625, anteil_ale = 0.3125,
                 anteil_inter = 0.0625,
                 S = 16, seed = 222)

# vergleich-Stub (compare_scenarios-Vertrag): tabelle im exakten
# Spaltenvertrag aus test_compare.R, zwei unverwechselbare Zeilen.
.vgl_tab <- data.frame(
  szenario      = c("Alpha", "Beta"),
  plan_trough   = c(-1111.5, -2222.25),
  trough_median = c(-410.5, -620.25),
  trough_p05    = c(-905.75, -1310.5),
  trough_p95    = c(180.5, 240.75),
  P_breach      = c(0.2, 0.6),
  P_solvent     = c(0.8, 0.4),
  P_breach_pct  = c(20, 60),
  P_solvent_pct = c(80, 40),
  insO_erfuellt = c(TRUE, FALSE),
  kk_limit      = c(450, 650),
  S             = c(10, 14),
  seed          = c(901, 902),
  stringsAsFactors = FALSE
)
.vgl <- list(phase = "szenarienvergleich", n = 2L, tabelle = .vgl_tab)

# Eingabeliste BEWUSST weder alphabetisch noch in "natuerlicher" Phasenfolge:
# nur echte Eingabereihenfolgen-Treue reproduziert diese Blattfolge (forecast
# zaehlt als ZWEI Blaetter: forecast + forecast_kpis).
.erg <- list(vergleich        = .vgl,
             sim_deckung      = .deckung,
             forecast         = .fc,
             varianzzerlegung = .varianz,
             sim_monate       = .mon)
.blaetter_erwartet <- c("vergleich", "sim_deckung", "forecast",
                        "forecast_kpis", "varianzzerlegung", "sim_monate")

# EIN gemeinsamer Export + Ruecklesung, LAZY memoisiert (delayedAssign-Muster
# test_compare.R); die Fixture-Naht memoisiert ihren einen teuren
# phase_sim_monate-Lauf (paths = 12, Seed 2026) genauso.
.cache <- new.env(parent = emptyenv())
delayedAssign("haupt", local({
  tf <- tempfile(fileext = ".xlsx")
  list(rueck = export_xlsx(.erg, tf), pfad = tf, blaetter = .lies_blaetter(tf))
}), assign.env = .cache)
.scfix <- scenario_from_workbook(WB, name = "ExportSpec",
                                 sim = list(paths = 12L, seed = 2026))
delayedAssign("mon_fix", phase_sim_monate(.scfix), assign.env = .cache)
haupt   <- function() .cache$haupt
mon_fix <- function() .cache$mon_fix

# ===========================================================================
#  (1) Rueckgabe- und Blattlisten-Vertrag
# ===========================================================================

# ---------------------------------------------------------------------------
# pfad existiert und ist der uebergebene Zielpfad; blaetter ist identical zur
# erwarteten Schreibreihenfolge (Eingabereihenfolge, forecast doppelt) UND
# identical zur openxlsx2-Blattliste der tatsaechlich geschriebenen Datei.
# ---------------------------------------------------------------------------
test_that("Rueckgabe: pfad existiert, blaetter == Eingabereihenfolge == Datei-Blattliste", {
  h <- haupt()
  expect_identical(h$rueck$pfad, h$pfad)
  expect_true(file.exists(h$rueck$pfad))
  expect_identical(h$rueck$blaetter, .blaetter_erwartet)
  wb <- openxlsx2::wb_load(h$pfad)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), .blaetter_erwartet)
  expect_identical(names(h$blaetter), .blaetter_erwartet)
})

# ===========================================================================
#  (2) Ruecklese-Orakel je Blatt: jede Zelle gegen die unabhaengige Erwartung
# ===========================================================================

# ---------------------------------------------------------------------------
# forecast = ZWEI Blaetter (gepinnte Blattstruktur-Entscheidung): Blatt
# "forecast" traegt die cash-Matrix mit den Zeilennamen als erster Spalte
# "Position" und den Spalten M1..MH; Blatt "forecast_kpis" die kpis als
# Key/Wert in Vertragsreihenfolge. Der guv-Koeder darf nirgends auftauchen.
# ---------------------------------------------------------------------------
test_that("forecast: Blatt forecast == cash-Matrix (Position + M1..M3), forecast_kpis == Key/Wert der kpis", {
  fc <- haupt()$blaetter[["forecast"]]
  expect_identical(names(fc), c("Position", "M1", "M2", "M3"))
  expect_identical(fc$Position, c("Endbestand", "KK_Inanspruchnahme"))
  expect_equal(fc$M1, c(1100.5,   440.5),  tolerance = 1e-12)
  expect_equal(fc$M2, c(-2200.25, -550.25), tolerance = 1e-12)
  expect_equal(fc$M3, c(3300.75,  660.75), tolerance = 1e-12)

  kp <- haupt()$blaetter[["forecast_kpis"]]
  expect_identical(names(kp), c("kennzahl", "wert"))
  expect_identical(kp$kennzahl, .kpi_keys)
  expect_true(is.numeric(kp$wert))
  expect_equal(kp$wert, .kpi_wert, tolerance = 1e-12)

  # guv-Koeder: weder als eigenes Blatt noch als Zellwert im forecast-Blatt.
  expect_false("guv" %in% names(haupt()$blaetter))
  expect_false(any(unlist(fc[-1]) == 9999.5))
})

# ---------------------------------------------------------------------------
# sim_monate: table unveraendert — Spaltennamen exakt, Strings identical,
# Zahlen all.equal 1e-12 gegen die unabhaengig gehaltene Stub-Tabelle.
# ---------------------------------------------------------------------------
test_that("sim_monate: Blatt == table des Ergebnisobjekts, Zelle fuer Zelle", {
  tb <- haupt()$blaetter[["sim_monate"]]
  expect_identical(names(tb), c("Monat", "P05", "P50", "P95", "P_unter"))
  expect_identical(tb$Monat, c("M1", "M2", "M3"))
  expect_equal(tb$P05,     c(-800.5, -730.25, -640.75), tolerance = 1e-12)
  expect_equal(tb$P50,     c(-100.5,  -20.25,   55.5),  tolerance = 1e-12)
  expect_equal(tb$P95,     c( 210.5,  320.25,  415.75), tolerance = 1e-12)
  expect_equal(tb$P_unter, c(0.3, 0.15, 0.05),          tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# sim_deckung: Key/Wert-Blatt (kennzahl, wert) — Keys identical in
# Vertragsreihenfolge (seed VOR S), wert homogen numerisch mit
# insO_erfuellt als 1 (TRUE); phase/scenario landen NICHT im Blatt.
# ---------------------------------------------------------------------------
test_that("sim_deckung: Key/Wert-Blatt in Vertragsreihenfolge, insO als 0/1, wert numerisch", {
  dk <- haupt()$blaetter[["sim_deckung"]]
  expect_identical(names(dk), c("kennzahl", "wert"))
  expect_identical(dk$kennzahl, .deckung_keys)
  expect_true(is.numeric(dk$wert))
  expect_equal(dk$wert, .deckung_wert, tolerance = 1e-12)
  expect_false(any(c("phase", "scenario") %in% dk$kennzahl))
})

# ---------------------------------------------------------------------------
# varianzzerlegung: Key/Wert-Blatt — Keys identical in Vertragsreihenfolge
# (S VOR seed), wert numerisch; die T_*/Theta-Koeder landen NICHT im Blatt.
# ---------------------------------------------------------------------------
test_that("varianzzerlegung: Key/Wert-Blatt in Vertragsreihenfolge (var/sd/anteil + S/seed)", {
  vz <- haupt()$blaetter[["varianzzerlegung"]]
  expect_identical(names(vz), c("kennzahl", "wert"))
  expect_identical(vz$kennzahl, .varianz_keys)
  expect_true(is.numeric(vz$wert))
  expect_equal(vz$wert, .varianz_wert, tolerance = 1e-12)
  expect_false(any(vz$wert %in% c(-9101, -9102, -9103, -9104, -9105, -9106)))
})

# ---------------------------------------------------------------------------
# Kreuz-Check gegen Blatt-/Objektvertauschung: das deckung-Blatt traegt
# WEDER die Keys NOCH irgendeinen wert des varianz-Stubs — und umgekehrt
# (alle Stub-Zahlen sind paarweise unverwechselbar, s. o.).
# ---------------------------------------------------------------------------
test_that("Kreuz-Check: deckung-Blatt traegt nicht die varianz-Werte und umgekehrt", {
  dk <- haupt()$blaetter[["sim_deckung"]]
  vz <- haupt()$blaetter[["varianzzerlegung"]]
  expect_false(identical(dk$kennzahl, .varianz_keys))
  expect_false(identical(vz$kennzahl, .deckung_keys))
  expect_false(any(dk$wert %in% .varianz_wert))
  expect_false(any(vz$wert %in% .deckung_wert))
})

# ---------------------------------------------------------------------------
# vergleich: tabelle unveraendert — alle 13 Vertragsspalten, Strings
# identical, Zahlen 1e-12; insO_erfuellt rundreist als Wahrheitswert
# (as.logical toleriert die openxlsx2-Bool-Zelle wie den String "TRUE").
# ---------------------------------------------------------------------------
test_that("vergleich: Blatt == tabelle des Ergebnisobjekts, Zelle fuer Zelle", {
  vg <- haupt()$blaetter[["vergleich"]]
  expect_identical(names(vg), names(.vgl_tab))
  expect_identical(vg$szenario, c("Alpha", "Beta"))
  for (sp in c("plan_trough", "trough_median", "trough_p05", "trough_p95",
               "P_breach", "P_solvent", "P_breach_pct", "P_solvent_pct",
               "kk_limit", "S", "seed"))
    expect_equal(as.numeric(vg[[sp]]), .vgl_tab[[sp]], tolerance = 1e-12,
                 info = sp)
  expect_identical(as.logical(vg$insO_erfuellt), c(TRUE, FALSE))
})

# ===========================================================================
#  (3) Determinismus: Zellinhalt haengt nur an den Eingaben
# ===========================================================================

# ---------------------------------------------------------------------------
# Zwei Exporte derselben Eingabe nach zwei verschiedenen tempfiles: die
# zurueckgelesenen Blattinhalte sind identical (KEIN Zeitstempel, kein
# Zufall im Zellinhalt). Datei-Bytes duerfen abweichen — verglichen wird
# ausschliesslich der Zellinhalt ueber die gemeinsame Leseform.
# ---------------------------------------------------------------------------
test_that("Determinismus: zwei Exporte -> identische zurueckgelesene Zellinhalte", {
  tfa <- tempfile(fileext = ".xlsx")
  tfb <- tempfile(fileext = ".xlsx")
  ra  <- export_xlsx(.erg, tfa)
  rb  <- export_xlsx(.erg, tfb)
  expect_identical(ra$blaetter, rb$blaetter)
  expect_identical(.lies_blaetter(tfa), .lies_blaetter(tfb))
})

# ===========================================================================
#  (4) Fixture-Naht: ein kleiner echter phase_sim_monate-Lauf
# ===========================================================================

# ---------------------------------------------------------------------------
# Der memoisierte Lauf (paths = 12, Seed 2026) wird exportiert und
# zurueckgelesen: die P50-Spalte (und die uebrigen table-Spalten) sind
# identisch zur table des Ergebnisobjekts (Toleranz 1e-9 wie der
# monate-Anker) — die Naht Ergebnisobjekt -> Blatt verbiegt keine Zahl.
# ---------------------------------------------------------------------------
test_that("Fixture-Naht: exportiertes sim_monate-Blatt == table des echten Laufs (1e-9)", {
  out <- mon_fix()
  tf  <- tempfile(fileext = ".xlsx")
  r   <- export_xlsx(list(sim_monate = out), tf)
  expect_identical(r$blaetter, "sim_monate")
  df <- .lies_blaetter(tf)[["sim_monate"]]
  expect_identical(df$Monat, out$table$Monat)
  expect_equal(df$P50,     out$table$P50,     tolerance = 1e-9)
  expect_equal(df$P05,     out$table$P05,     tolerance = 1e-9)
  expect_equal(df$P95,     out$table$P95,     tolerance = 1e-9)
  expect_equal(df$P_unter, out$table$P_unter, tolerance = 1e-9)
})

# ===========================================================================
#  (5) Fehlerfaelle
# ===========================================================================

# ---------------------------------------------------------------------------
# Unbekannter Listenname -> Fehler MIT dem Namen im Text (auch wenn er neben
# gueltigen Namen steht); leere Liste und unbenannte Liste -> Fehler.
# ---------------------------------------------------------------------------
test_that("Fehlerfaelle: unbekannter Name (im Fehlertext), leere Liste, unbenannte Liste", {
  tf <- tempfile(fileext = ".xlsx")
  expect_error(export_xlsx(list(quark = .mon), tf), "quark")
  expect_error(export_xlsx(list(sim_monate = .mon, quark = .deckung), tf), "quark")
  expect_error(export_xlsx(list(), tf))
  expect_error(export_xlsx(list(.mon), tf))
})
