# ============================================================================
#  tests/test_export_docx.R  —  SPEZIFIKATION (rot zuerst) fuer phase-09-docx
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN: R/export.R
#  existiert und laesst sich sourcen, aber export_docx existiert dort noch
#  nicht — jeder Test faellt am fehlenden Symbol. Der Builder erfuellt sie
#  erst mit der Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche.
#
#  Vertrag (handoff/phase-09-docx.json; KEIN Legacy-Vorbild — Zielort
#  R/export.R laut ARCHITECTURE.md):
#   - export_docx(ergebnisse, pfad): ergebnisse = BENANNTE Liste von Phasen-
#     Ergebnisobjekten; schreibt ueber officer je Eintrag einen ABSCHNITT in
#     EINGABEREIHENFOLGE — eine Ueberschrift im Stil 'heading 1', deren Text
#     EXAKT der Vertragsname ist (dieselben Namen wie die xlsx-Blattnamen),
#     gefolgt von GENAU EINER Tabelle. Rueckgabe: list(pfad = <geschriebener
#     Pfad>, abschnitte = <character der Abschnittsnamen in Schreibreihen-
#     folge>); abschnitte ist identical zu export_xlsx(...)$blaetter
#     DERSELBEN Eingabe (gemeinsamer Block-Bauer export_bloecke_bauen).
#   - Tabellenformen je unterstuetztem Namen wie bei xlsx: forecast zaehlt
#     als ZWEI Abschnitte ('forecast' = cash-Matrix als data.frame mit den
#     Zeilennamen als erster Spalte 'Position', danach M1..MH;
#     'forecast_kpis' = kpis als Key/Wert kennzahl/wert in Vertragsreihen-
#     folge trough, trough_month, maxKK, months_below, result, end);
#     sim_monate -> table unveraendert; vergleich -> tabelle unveraendert;
#     sim_deckung -> Key/Wert in Reihenfolge trough_median, trough_p05,
#     trough_p95, P_breach, P_solvent, P_breach_pct, P_solvent_pct,
#     insO_erfuellt, kk_limit, seed, S (seed VOR S); varianzzerlegung ->
#     Key/Wert var_gesamt, var_epi, var_ale, var_inter, sd_gesamt, sd_epi,
#     sd_ale, anteil_epi, anteil_ale, anteil_inter, S, seed (S VOR seed —
#     die Asymmetrie killt jede Abschnitts-/Objektvertauschung).
#   - ZELL-TEXT-REGEL (gepinnte Entscheidung): docx-Zellen sind Text; JEDE
#     Spalte wird von export_docx selbst per as.character konvertiert
#     (Zahlen in der R-eigenen 15-Stellen-Darstellung, logicals TRUE/FALSE,
#     die wert-Spalte der Key/Wert-Tabellen als Text der homogen numerischen
#     Werte, insO als '1'/'0') — officer-eigenes format() beim
#     body_add_table wuerde runden/auffuellen ('1.50') und wird umgangen.
#   - deterministisch: KEIN Zeitstempel im INHALT (docx-Metadaten wie
#     core.xml-Datumsfelder duerfen abweichen; verglichen wird der ueber
#     officer::docx_summary gelesene Inhalt).
#   - Fehler: unbekannter Listenname (Fehlertext enthaelt den Namen), leere
#     Liste, unbenannte Liste -> expect_error.
#
#  Orakel-Politik: RUECKLESE-Orakel an synthetischen, paarweise
#  unverwechselbaren Stubs (KEINE Engine im Stub-Teil, KEINE Tier-1-Werte,
#  keine Zahlen-Wiederverwendung aus anderen Tests). Der Test haelt die
#  Erwartungen (Key-Reihenfolgen, wert-Vektoren, Zellwerte) als EIGENE
#  Literale UNABHAENGIG vom Stub-Objekt und liest die Datei mit
#  officer::docx_summary zurueck: Zellen je table_index nach row_id/cell_id
#  rekonstruiert (row_id laeuft ueber Tabellen HINWEG weiter — deshalb ist
#  table_index die Gruppierung), Kopfzeile = is_header-Zellen; Zahlen via
#  as.numeric der Zell-Texte mit all.equal 1e-12, Strings identical. Die
#  Stubs tragen bewusst ZUSAETZLICHE Vertragsfelder (guv, T_full/T_epi/
#  T_ale/Theta, phase/scenario), die in KEINEM Abschnitt landen duerfen.
#  Kreuz-Check deckung <-> varianz killt Vertauschungen; der Determinismus-
#  Lauf vergleicht die ZURUECKGELESENEN Inhalte zweier Exporte. Die
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
source(file.path(.root, "R", "export.R"))   # export_docx fehlt dort: rot zuerst
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Gemeinsame Leseform: docx_summary nach doc_index; Abschnitte = heading-1-
# Ueberschriften in Dokumentreihenfolge, je Ueberschrift die k-te Tabelle
# (Zellen je table_index nach row_id/cell_id; Kopfzeile = is_header).
.roh_lesen <- function(pfad) {
  s <- officer::docx_summary(officer::read_docx(pfad))
  s[order(s$doc_index), ]
}
.abschnitte_aus <- function(roh) {
  h <- roh[roh$content_type == "paragraph" & roh$style_name == "heading 1",
           c("doc_index", "text")]
  z <- roh[roh$content_type == "table cell", ]
  tidx <- sort(unique(z$table_index))
  stopifnot(length(tidx) == nrow(h))
  tabs <- lapply(tidx, function(t) {
    zk <- z[z$table_index == t, ]
    zk <- zk[order(zk$row_id, zk$cell_id), ]
    kopf    <- zk$text[zk$is_header]
    koerper <- zk[!zk$is_header, ]
    zeilen  <- split(koerper$text, koerper$row_id)
    df <- as.data.frame(do.call(rbind, unname(zeilen)), stringsAsFactors = FALSE)
    names(df) <- kopf
    rownames(df) <- NULL
    df
  })
  setNames(tabs, h$text)
}
.lies_abschnitte <- function(pfad) .abschnitte_aus(.roh_lesen(pfad))

# ---------------------------------------------------------------------------
# UNABHAENGIG gehaltene Erwartungen (eigene Literale, NICHT aus den Stubs
# abgeleitet): Key-Reihenfolgen der Key/Wert-Tabellen und deren wert-Vektoren.
# Alle Zahlen der fuenf Stubs sind PAARWEISE UNVERWECHSELBAR (keine Zahl
# kommt in zwei Vertragsobjekten vor) — nur so hat der Kreuz-Check Kraft.
# ---------------------------------------------------------------------------
.deckung_keys <- c("trough_median", "trough_p05", "trough_p95", "P_breach",
                   "P_solvent", "P_breach_pct", "P_solvent_pct",
                   "insO_erfuellt", "kk_limit", "seed", "S")
.deckung_wert <- c(-522.5, -1022.25, 282.75, 0.375, 0.625, 37.5, 62.5, 1,
                   616, 121, 9)

.varianz_keys <- c("var_gesamt", "var_epi", "var_ale", "var_inter",
                   "sd_gesamt", "sd_epi", "sd_ale",
                   "anteil_epi", "anteil_ale", "anteil_inter", "S", "seed")
.varianz_wert <- c(5041, 3025, 1444, 572, 71, 55, 38, 0.55, 0.4, 0.05,
                   18, 232)

.kpi_keys <- c("trough", "trough_month", "maxKK", "months_below", "result", "end")
.kpi_wert <- c(-6200.5, 3, 444.25, 4, 888.75, 1555.5)

# ---------------------------------------------------------------------------
# Synthetische Stubs je Vertragstyp (Muster der Schwester-Spec
# test_export_xlsx.R: Feldvertrag nachbauen, keine Engine, kein Seed-Lauf,
# keine Tier-1-Werte; alle Zahlen frisch erfunden).
# ---------------------------------------------------------------------------

# forecast-Stub (phase_forecast-Vertrag): kleine cash-Matrix mit Zeilennamen
# + kpis-Liste; guv ist ein Koeder, der NICHT exportiert werden darf.
.fc_cash <- matrix(c(1210.5, -2320.25, 3430.75,
                     530.5,  -640.25,   750.75),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("Endbestand", "KK_Inanspruchnahme"),
                                   c("M1", "M2", "M3")))
.fc <- list(phase = "forecast", scenario = "StubForecastDocx",
            guv  = matrix(8888.5, 1, 1, dimnames = list("NurGuV", "M1")),
            cash = .fc_cash,
            kpis = list(trough = -6200.5, trough_month = 3, maxKK = 444.25,
                        months_below = 4, result = 888.75, end = 1555.5))

# sim_monate-Stub (phase_sim_monate-Vertrag): table im Feldvertrag
# Monat/P05/P50/P95/P_unter.
.mon_tab <- data.frame(
  Monat   = c("M1", "M2", "M3"),
  P05     = c(-910.5, -840.25, -750.75),
  P50     = c(-210.5, -130.25,   66.5),
  P95     = c( 320.5,  430.25,  525.75),
  P_unter = c(0.35, 0.2, 0.1),
  stringsAsFactors = FALSE
)
.mon <- list(phase = "sim_monate", scenario = "StubMonateDocx", table = .mon_tab)

# sim_deckung-Stub (phase_sim_deckung-Vertrag): skalare Kennzahlen in
# Vertragsreihenfolge; phase/scenario duerfen NICHT in der Tabelle landen.
.deckung <- list(phase = "sim_deckung", scenario = "StubDeckungDocx",
                 trough_median = -522.5, trough_p05 = -1022.25,
                 trough_p95 = 282.75,
                 P_breach = 0.375, P_solvent = 0.625,
                 P_breach_pct = 37.5, P_solvent_pct = 62.5,
                 insO_erfuellt = TRUE, kk_limit = 616, seed = 121, S = 9)

# varianzzerlegung-Stub (phase_varianzzerlegung-Vertrag): T_*/Theta sind
# Koeder-Vektoren, die NICHT in der Tabelle landen duerfen.
.varianz <- list(phase = "varianzzerlegung", scenario = "StubVarianzDocx",
                 T_full = c(-8201, -8202), T_epi = c(-8203, -8204),
                 T_ale  = c(-8205, -8206),
                 Theta  = matrix(0.44, 2, 1, dimnames = list(NULL, "menge")),
                 var_gesamt = 5041, var_epi = 3025, var_ale = 1444,
                 var_inter = 572,
                 sd_gesamt = 71, sd_epi = 55, sd_ale = 38,
                 anteil_epi = 0.55, anteil_ale = 0.4, anteil_inter = 0.05,
                 S = 18, seed = 232)

# vergleich-Stub (compare_scenarios-Vertrag): tabelle im exakten
# Spaltenvertrag aus test_compare.R, zwei unverwechselbare Zeilen.
.vgl_tab <- data.frame(
  szenario      = c("Gamma", "Delta"),
  plan_trough   = c(-3333.5, -4444.25),
  trough_median = c(-530.5, -740.25),
  trough_p05    = c(-1105.75, -1510.5),
  trough_p95    = c(290.5, 350.75),
  P_breach      = c(0.3, 0.7),
  P_solvent     = c(0.7, 0.3),
  P_breach_pct  = c(30, 70),
  P_solvent_pct = c(70, 30),
  insO_erfuellt = c(TRUE, FALSE),
  kk_limit      = c(560, 770),
  S             = c(11, 13),
  seed          = c(911, 913),
  stringsAsFactors = FALSE
)
.vgl <- list(phase = "szenarienvergleich", n = 2L, tabelle = .vgl_tab)

# Eingabeliste BEWUSST weder alphabetisch noch in "natuerlicher" Phasenfolge
# (und anders gemischt als die xlsx-Spec): nur echte Eingabereihenfolgen-
# Treue reproduziert diese Abschnittsfolge (forecast zaehlt als ZWEI
# Abschnitte: forecast + forecast_kpis).
.erg <- list(varianzzerlegung = .varianz,
             forecast         = .fc,
             vergleich        = .vgl,
             sim_monate       = .mon,
             sim_deckung      = .deckung)
.abschnitte_erwartet <- c("varianzzerlegung", "forecast", "forecast_kpis",
                          "vergleich", "sim_monate", "sim_deckung")

# EIN gemeinsamer Export + Ruecklesung, LAZY memoisiert (delayedAssign-Muster
# der Schwester-Specs); die Fixture-Naht memoisiert ihren einen teuren
# phase_sim_monate-Lauf (paths = 12, Seed 2026) genauso.
.cache <- new.env(parent = emptyenv())
delayedAssign("haupt", local({
  tf  <- tempfile(fileext = ".docx")
  r   <- export_docx(.erg, tf)
  roh <- .roh_lesen(tf)
  list(rueck = r, pfad = tf, roh = roh, tabellen = .abschnitte_aus(roh))
}), assign.env = .cache)
.scfix <- scenario_from_workbook(WB, name = "ExportSpecDocx",
                                 sim = list(paths = 12L, seed = 2026))
delayedAssign("mon_fix", phase_sim_monate(.scfix), assign.env = .cache)
haupt   <- function() .cache$haupt
mon_fix <- function() .cache$mon_fix

# ===========================================================================
#  (1) Rueckgabe- und Abschnittslisten-Vertrag
# ===========================================================================

# ---------------------------------------------------------------------------
# pfad existiert und ist der uebergebene Zielpfad; abschnitte ist identical
# zur erwarteten Schreibreihenfolge (Eingabereihenfolge, forecast doppelt)
# UND identical zur heading-1-Ueberschriftenfolge des Dokuments.
# ---------------------------------------------------------------------------
test_that("Rueckgabe: pfad existiert, abschnitte == Eingabereihenfolge == Ueberschriftenfolge", {
  h <- haupt()
  expect_identical(h$rueck$pfad, h$pfad)
  expect_true(file.exists(h$rueck$pfad))
  expect_identical(h$rueck$abschnitte, .abschnitte_erwartet)
  ueber <- h$roh[h$roh$content_type == "paragraph" &
                 h$roh$style_name == "heading 1", "text"]
  expect_identical(ueber, .abschnitte_erwartet)
  expect_identical(names(h$tabellen), .abschnitte_erwartet)
})

# ===========================================================================
#  (2) Ruecklese-Orakel je Abschnitt: jede Zelle gegen die unabhaengige
#      Erwartung (Zell-Text-Regel: as.numeric der Texte, Strings identical)
# ===========================================================================

# ---------------------------------------------------------------------------
# forecast = ZWEI Abschnitte: 'forecast' traegt die cash-Matrix mit den
# Zeilennamen als erster Spalte 'Position' und den Spalten M1..MH;
# 'forecast_kpis' die kpis als Key/Wert in Vertragsreihenfolge. Der
# guv-Koeder darf nirgends auftauchen.
# ---------------------------------------------------------------------------
test_that("forecast: Abschnitt forecast == cash-Matrix (Position + M1..M3), forecast_kpis == Key/Wert der kpis", {
  fc <- haupt()$tabellen[["forecast"]]
  expect_identical(names(fc), c("Position", "M1", "M2", "M3"))
  expect_identical(fc$Position, c("Endbestand", "KK_Inanspruchnahme"))
  expect_equal(as.numeric(fc$M1), c(1210.5,   530.5),  tolerance = 1e-12)
  expect_equal(as.numeric(fc$M2), c(-2320.25, -640.25), tolerance = 1e-12)
  expect_equal(as.numeric(fc$M3), c(3430.75,  750.75), tolerance = 1e-12)

  kp <- haupt()$tabellen[["forecast_kpis"]]
  expect_identical(names(kp), c("kennzahl", "wert"))
  expect_identical(kp$kennzahl, .kpi_keys)
  expect_equal(as.numeric(kp$wert), .kpi_wert, tolerance = 1e-12)

  # guv-Koeder: weder als eigener Abschnitt noch als Zellwert im Dokument.
  expect_false("guv" %in% names(haupt()$tabellen))
  expect_false(any(haupt()$roh$text == "8888.5", na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# sim_monate: table unveraendert — Spaltennamen exakt, Strings identical,
# Zahlen via as.numeric mit 1e-12 gegen die unabhaengig gehaltene Tabelle.
# ---------------------------------------------------------------------------
test_that("sim_monate: Abschnitt == table des Ergebnisobjekts, Zelle fuer Zelle", {
  tb <- haupt()$tabellen[["sim_monate"]]
  expect_identical(names(tb), c("Monat", "P05", "P50", "P95", "P_unter"))
  expect_identical(tb$Monat, c("M1", "M2", "M3"))
  expect_equal(as.numeric(tb$P05),     c(-910.5, -840.25, -750.75), tolerance = 1e-12)
  expect_equal(as.numeric(tb$P50),     c(-210.5, -130.25,   66.5),  tolerance = 1e-12)
  expect_equal(as.numeric(tb$P95),     c( 320.5,  430.25,  525.75), tolerance = 1e-12)
  expect_equal(as.numeric(tb$P_unter), c(0.35, 0.2, 0.1),           tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# sim_deckung: Key/Wert-Tabelle (kennzahl, wert) — Keys identical in
# Vertragsreihenfolge (seed VOR S), wert-Texte als Zahlen lesbar, die
# insO-Zelle traegt EXAKT den Text '1' (Zell-Text-Regel: as.character nach
# as.numeric); phase/scenario landen NICHT in der Tabelle.
# ---------------------------------------------------------------------------
test_that("sim_deckung: Key/Wert in Vertragsreihenfolge, insO-Zelle exakt '1', Werte via as.numeric", {
  dk <- haupt()$tabellen[["sim_deckung"]]
  expect_identical(names(dk), c("kennzahl", "wert"))
  expect_identical(dk$kennzahl, .deckung_keys)
  expect_identical(dk$wert[dk$kennzahl == "insO_erfuellt"], "1")
  expect_equal(as.numeric(dk$wert), .deckung_wert, tolerance = 1e-12)
  expect_false(any(c("phase", "scenario") %in% dk$kennzahl))
})

# ---------------------------------------------------------------------------
# varianzzerlegung: Key/Wert-Tabelle — Keys identical in Vertragsreihenfolge
# (S VOR seed), Werte via as.numeric; die T_*/Theta-Koeder landen NICHT in
# der Tabelle.
# ---------------------------------------------------------------------------
test_that("varianzzerlegung: Key/Wert in Vertragsreihenfolge (var/sd/anteil + S/seed)", {
  vz <- haupt()$tabellen[["varianzzerlegung"]]
  expect_identical(names(vz), c("kennzahl", "wert"))
  expect_identical(vz$kennzahl, .varianz_keys)
  expect_equal(as.numeric(vz$wert), .varianz_wert, tolerance = 1e-12)
  expect_false(any(as.numeric(vz$wert) %in% c(-8201, -8202, -8203, -8204,
                                              -8205, -8206, 0.44)))
})

# ---------------------------------------------------------------------------
# Kreuz-Check gegen Abschnitts-/Objektvertauschung: die deckung-Tabelle
# traegt WEDER die Keys NOCH irgendeinen Wert des varianz-Stubs — und
# umgekehrt (alle Stub-Zahlen sind paarweise unverwechselbar, s. o.).
# ---------------------------------------------------------------------------
test_that("Kreuz-Check: deckung-Tabelle traegt nicht die varianz-Werte und umgekehrt", {
  dk <- haupt()$tabellen[["sim_deckung"]]
  vz <- haupt()$tabellen[["varianzzerlegung"]]
  expect_false(identical(dk$kennzahl, .varianz_keys))
  expect_false(identical(vz$kennzahl, .deckung_keys))
  expect_false(any(as.numeric(dk$wert) %in% .varianz_wert))
  expect_false(any(as.numeric(vz$wert) %in% .deckung_wert))
})

# ---------------------------------------------------------------------------
# vergleich: tabelle unveraendert — alle 13 Vertragsspalten, Strings
# identical, Zahlen via as.numeric 1e-12; die insO-Zellen tragen EXAKT die
# Texte TRUE/FALSE (as.character-Regel fuer logicals) und rundreisen via
# as.logical.
# ---------------------------------------------------------------------------
test_that("vergleich: Abschnitt == tabelle des Ergebnisobjekts, Zelle fuer Zelle", {
  vg <- haupt()$tabellen[["vergleich"]]
  expect_identical(names(vg), names(.vgl_tab))
  expect_identical(vg$szenario, c("Gamma", "Delta"))
  for (sp in c("plan_trough", "trough_median", "trough_p05", "trough_p95",
               "P_breach", "P_solvent", "P_breach_pct", "P_solvent_pct",
               "kk_limit", "S", "seed"))
    expect_equal(as.numeric(vg[[sp]]), .vgl_tab[[sp]], tolerance = 1e-12,
                 info = sp)
  expect_identical(vg$insO_erfuellt, c("TRUE", "FALSE"))
  expect_identical(as.logical(vg$insO_erfuellt), c(TRUE, FALSE))
})

# ===========================================================================
#  (3) Ordnung: Ueberschrift steht VOR ihrer Tabelle
# ===========================================================================

# ---------------------------------------------------------------------------
# Ueber doc_index: die k-te Ueberschrift liegt vor der ersten Zelle der
# k-ten Tabelle, und diese vor der (k+1)-ten Ueberschrift — die Paarung
# Ueberschrift -> Tabelle ist Dokumentstruktur, keine Zufallsfolge.
# ---------------------------------------------------------------------------
test_that("Ordnung: jede Ueberschrift unmittelbar vor ihrer Tabelle, Tabellen nicht verschraenkt", {
  roh <- haupt()$roh
  h <- roh[roh$content_type == "paragraph" & roh$style_name == "heading 1",
           c("doc_index", "text")]
  z <- roh[roh$content_type == "table cell", ]
  tidx <- sort(unique(z$table_index))
  expect_identical(length(tidx), nrow(h))
  erste_zelle <- vapply(tidx, function(t)
    as.numeric(min(z$doc_index[z$table_index == t])), numeric(1))
  expect_true(all(h$doc_index < erste_zelle))
  expect_true(all(erste_zelle[-length(erste_zelle)] < h$doc_index[-1]))
})

# ===========================================================================
#  (4) Symmetrie zu export_xlsx: gemeinsamer Block-Bauer sichtbar gemacht
# ===========================================================================

# ---------------------------------------------------------------------------
# abschnitte der docx-Rueckgabe ist identical zu blaetter der xlsx-Rueckgabe
# DERSELBEN Eingabe — beide Medien projizieren dieselbe Block-Liste; eine
# Drift der Vertragskonstanten zwischen den Medien faellt hier rot.
# ---------------------------------------------------------------------------
test_that("Symmetrie: export_docx$abschnitte == export_xlsx$blaetter derselben Eingabe", {
  rx <- export_xlsx(.erg, tempfile(fileext = ".xlsx"))
  expect_identical(haupt()$rueck$abschnitte, rx$blaetter)
})

# ===========================================================================
#  (5) Determinismus: Inhalt haengt nur an den Eingaben
# ===========================================================================

# ---------------------------------------------------------------------------
# Zwei Exporte derselben Eingabe nach zwei verschiedenen tempfiles: die
# zurueckgelesenen Abschnitte (Ueberschriften + alle Zell-Texte) sind
# identical (KEIN Zeitstempel, kein Zufall im Inhalt). Datei-Bytes/
# docx-Metadaten duerfen abweichen — verglichen wird ausschliesslich der
# docx_summary-Inhalt ueber die gemeinsame Leseform.
# ---------------------------------------------------------------------------
test_that("Determinismus: zwei Exporte -> identische zurueckgelesene Inhalte", {
  tfa <- tempfile(fileext = ".docx")
  tfb <- tempfile(fileext = ".docx")
  ra  <- export_docx(.erg, tfa)
  rb  <- export_docx(.erg, tfb)
  expect_identical(ra$abschnitte, rb$abschnitte)
  expect_identical(.lies_abschnitte(tfa), .lies_abschnitte(tfb))
})

# ===========================================================================
#  (6) Fixture-Naht: ein kleiner echter phase_sim_monate-Lauf
# ===========================================================================

# ---------------------------------------------------------------------------
# Der memoisierte Lauf (paths = 12, Seed 2026) wird exportiert und
# zurueckgelesen: Monat identical, P05/P50/P95/P_unter via as.numeric mit
# Toleranz 1e-9 (wie der monate-Anker) — die as.character-Zellregel
# verbiegt keine echte Zahl.
# ---------------------------------------------------------------------------
test_that("Fixture-Naht: exportierter sim_monate-Abschnitt == table des echten Laufs (1e-9)", {
  out <- mon_fix()
  tf  <- tempfile(fileext = ".docx")
  r   <- export_docx(list(sim_monate = out), tf)
  expect_identical(r$abschnitte, "sim_monate")
  df <- .lies_abschnitte(tf)[["sim_monate"]]
  expect_identical(df$Monat, out$table$Monat)
  expect_equal(as.numeric(df$P50),     out$table$P50,     tolerance = 1e-9)
  expect_equal(as.numeric(df$P05),     out$table$P05,     tolerance = 1e-9)
  expect_equal(as.numeric(df$P95),     out$table$P95,     tolerance = 1e-9)
  expect_equal(as.numeric(df$P_unter), out$table$P_unter, tolerance = 1e-9)
})

# ===========================================================================
#  (7) Fehlerfaelle
# ===========================================================================

# ---------------------------------------------------------------------------
# Unbekannter Listenname -> Fehler MIT dem Namen im Text (auch wenn er neben
# gueltigen Namen steht); leere Liste und unbenannte Liste -> Fehler.
# ---------------------------------------------------------------------------
test_that("Fehlerfaelle: unbekannter Name (im Fehlertext), leere Liste, unbenannte Liste", {
  tf <- tempfile(fileext = ".docx")
  expect_error(export_docx(list(quark = .mon), tf), "quark")
  expect_error(export_docx(list(sim_monate = .mon, quark = .deckung), tf), "quark")
  expect_error(export_docx(list(), tf))
  expect_error(export_docx(list(.mon), tf))
})
