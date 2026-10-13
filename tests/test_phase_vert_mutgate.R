# ============================================================================
#  tests/test_phase_vert_mutgate.R
#  Nachgeschobene Tests aus dem Mutations-Score-Gate (mutgate
#  phase-04-verteilungen, Befund 2/5): ueberlebende Mutanten in
#  R/phase_verteilungen.R waren benannte Loecher der Suite
#  (Patches archiviert unter .orch/mut-phase-04-verteilungen/).
#
#  Jede Pruefung bindet die formatierte Treiber-Tabelle bzw. die Diagnose an
#  UNABHAENGIG gerechnete Sollwerte aus out$spec + qpert der eingefrorenen
#  Engine — nie an die Tabelle selbst, keine hartcodierten Erwartungswerte.
#    M5  P5/P95-Spalten vertauscht            -> Tabellen-Vertrag Quantile
#    M3  CV-ifelse-Zweige vertauscht (NA<->0) -> Tabellen-Vertrag CV + Quelle
#    M4  Belastbarkeits-Grenze >=24 zu >24    -> Boundary-Test 24-Monats-Mappe
#
#  Kill-Logik als Rot-zuerst-Ersatz: M3/M5 sind auf dem jeweiligen Mutanten
#  rot (Retest des Gates) und auf dem echten Stand gruen (volle Suite).
#  M4: der Grenzpunkt historie_monate == 24 ist ueber die Mappe strukturell
#  nicht erreichbar — load_model (eingefrorene Engine) rotiert mdl$base immer
#  auf die letzten 12 Kalendermonate, und der Wrapper misst legacy-treu
#  ncol(mdl$base). Der Boundary-Test unten versucht es trotzdem real (24er-
#  Mappe on-the-fly) und endet heute als SICHTBARER Skip mit dieser
#  Begruendung: M4 bleibt benanntes Loch des Gates. Wird historie_monate je
#  auf die rohe Blatt-Historie umgestellt, scharft sich der Test von selbst.
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
source(file.path(.root, "R", "phase_verteilungen.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# Formatierte Prozent-Strings der Tabelle ("+3.3%", "0.0%") numerisch parsen;
# ein Dezimalkomma wuerde ebenfalls verstanden. Nicht-Zahlen (z.B. "NA%")
# werden NA und lassen den Vergleich benannt scheitern statt still passen.
.pnum <- function(x)
  suppressWarnings(as.numeric(sub("%$", "", gsub(",", ".", as.character(x), fixed = TRUE))))
# Sollwert auf die dokumentierte Tabellen-Aufloesung (1 Dezimale) quantisieren —
# mit demselben sprintf-Runden, das der Tabellen-Vertrag ausweist.
.q1 <- function(x) as.numeric(sprintf("%.1f", x))

test_that("Treiber-Tabelle: P5/P50/P95 sind die PERT-Quantile der Spec, streng geordnet", {
  # Loch (Mutant M5): die Quantil-SPALTEN waren nur strukturell geprueft —
  # vertauschte Vertrauensgrenzen (P5 traegt das 95%-Quantil und umgekehrt)
  # blieben unsichtbar. Sollwerte ausschliesslich aus out$spec via
  # qpert(c(.05,.5,.95), min, mode, max), nie aus der Tabelle selbst.
  out <- phase_verteilungen(scenario_from_workbook(WB), plot = FALSE)
  expect_setequal(as.character(out$table$Treiber), names(out$spec))
  for (i in seq_len(nrow(out$table))) {
    d <- as.character(out$table$Treiber[i]); s <- out$spec[[d]]
    q    <- qpert(c(.05, .5, .95), s$min, s$mode, s$max)
    ist  <- setNames(.pnum(c(out$table$P5[i], out$table$P50[i], out$table$P95[i])),
                     c("P5", "P50", "P95"))
    soll <- setNames(.q1(100 * q), c("P5", "P50", "P95"))
    expect_equal(ist, soll, tolerance = 1e-12,
                 label = sprintf("Tabellenzeile '%s' (P5/P50/P95, geparst)", d))
    # Strikte Ordnung, wo die epistemische Bandbreite echt ist (min < max).
    if (s$min < s$max) {
      expect_lt(ist[["P5"]],  ist[["P50"]])
      expect_lt(ist[["P50"]], ist[["P95"]])
    }
  }
})

test_that("Treiber-Tabelle: CV-Spalte traegt 100*cv der Spec, Quelle folgt cv_source", {
  # Loch (Mutant M3): die ifelse-Zweige der CV-Spalte waren vertauschbar —
  # jede geschaetzte CV wuerde als 0.0 ausgewiesen, ohne dass ein Test es sah.
  # Sollwert = 100*cv aus out$spec (NA-cv -> exakt 0.0), quantisiert auf die
  # Tabellen-Aufloesung; Quelle-Spalte identisch zur cv_source der Spec.
  out <- phase_verteilungen(scenario_from_workbook(WB), plot = FALSE)
  for (i in seq_len(nrow(out$table))) {
    d <- as.character(out$table$Treiber[i]); s <- out$spec[[d]]
    soll_cv <- if (is.na(s$cv)) 0 else .q1(100 * s$cv)
    expect_identical(.pnum(out$table$CV[i]), soll_cv,
                     label = sprintf("Tabellenzeile '%s' (CV, geparst)", d))
    expect_identical(out$table$Quelle[i], s$cv_source,
                     label = sprintf("Tabellenzeile '%s' (Quelle)", d))
  }
  # Selbstpruefung der Kill-Kraft: mindestens ein Treiber traegt eine echt
  # positive CV (geschaetzte Basen menge/material) — sonst waere 0.0 == 0.0
  # blind gegen den vertauschten Zweig.
  expect_true(any(vapply(out$spec, function(s) isTRUE(s$cv > 0), logical(1))))
})

test_that("Treiber-Tabelle: NA-cv wird als exakt 0.0 ausgewiesen, Quelle bleibt Vorgabe", {
  # Loch (Mutant M3, NA-Zweig): bei cv = NA weist die Tabelle vertraglich 0.0
  # aus — der vertauschte ifelse wuerde "NA%" rendern. Eine leere cv-Zelle in
  # Blatt 06 ist ein legaler Mappen-Zustand; injiziert wie die Fehlerfall-
  # Tests des Bestands ueber sc$mdl$dists_raw, bei einem Treiber ausserhalb
  # der est-Basen (menge/material), damit keine Schaetzung die NA ueberdeckt.
  sc  <- scenario_from_workbook(WB)
  ref <- phase_verteilungen(sc, plot = FALSE)
  cand <- setdiff(names(ref$spec), c("menge", "material"))
  skip_if(length(cand) == 0,
          "kein Treiber ausserhalb der est-Basen in Blatt 06 — NA-Zweig nicht pruefbar")
  d <- cand[1]
  i <- which(as.character(sc$mdl$dists_raw[[1]]) == d)[1]
  sc$mdl$dists_raw[["cv"]][i] <- NA_real_
  out <- phase_verteilungen(sc, plot = FALSE)
  expect_true(is.na(out$spec[[d]]$cv))                     # Injektion kam in der Spec an
  expect_identical(out$spec[[d]]$cv_source, "Vorgabe")
  row <- out$table[out$table$Treiber == d, , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(.pnum(row$CV), 0)
  expect_identical(row$Quelle, "Vorgabe")
})

test_that("Belastbarkeits-Grenze: eine 24-Monats-Mappe gilt als cv_belastbar (Boundary >= 24)", {
  # Loch (Mutant M4): die >=24-Grenze des Badges war nur am Fixture-Punkt 12
  # geprueft, wo >= und > zusammenfallen. Kill-Versuch am Grenzpunkt selbst:
  # 24-Monats-Variante der Fixture (die 12 Ist-Monatsspalten in 01_Positionen
  # dupliziert angehaengt, Koepfe 2026-01..2026-12 setzen die 2025er fort),
  # als Szenario geladen. Scheitert ein Schritt an einem Engine-Vertragsdetail,
  # wird SICHTBAR uebersprungen (benanntes Loch), nicht still gruen.
  skip_if_not_installed("openxlsx2")
  classcols <- c("pos", "label", "grp", "cashprofile", "driver", "ust")
  wb24 <- tryCatch({
    dst <- tempfile("wb24_", fileext = ".xlsx")
    stopifnot(file.copy(WB, dst, overwrite = TRUE))
    pos_raw <- as.data.frame(readxl::read_excel(dst, "01_Positionen", skip = 3))
    mcols <- setdiff(names(pos_raw), classcols)
    stopifnot(length(mcols) == 12, !anyNA(pos_raw[, mcols]))
    block <- pos_raw[, mcols, drop = FALSE]
    names(block) <- sprintf("2026-%02d", 1:12)
    wb <- openxlsx2::wb_load(dst)
    wb <- openxlsx2::wb_add_data(wb, sheet = "01_Positionen", x = block,
                                 start_row = 4, start_col = ncol(pos_raw) + 1,
                                 col_names = TRUE)
    openxlsx2::wb_save(wb, dst, overwrite = TRUE)
    dst
  }, error = function(e)
    skip(sprintf("Mappen-Erweiterung auf 24 Monate gescheitert (%s) — M4 bleibt benanntes Loch des Gates.",
                 conditionMessage(e))))
  sc24 <- tryCatch(scenario_from_workbook(wb24, name = "Hist24"),
                   error = function(e)
    skip(sprintf("Engine lehnt die 24-Monats-Mappe ab (%s) — M4 bleibt benanntes Loch des Gates.",
                 conditionMessage(e))))
  mon <- setdiff(names(sc24$mdl$pos), classcols)
  if (length(mon) != 24L)
    skip(sprintf("Mappen-Erweiterung unvollstaendig (%d statt 24 Monatsspalten) — M4 bleibt benanntes Loch des Gates.",
                 length(mon)))
  out <- phase_verteilungen(sc24, plot = FALSE)
  if (!identical(out$diagnostics$historie_monate, 24L))
    skip(sprintf(paste0("Engine-Vertragsdetail: load_model rotiert mdl$base immer auf die letzten 12 ",
                        "Kalendermonate (Basisjahr), der Wrapper misst legacy-treu ncol(mdl$base) — ",
                        "historie_monate ist %d statt 24. Die >=24-Grenze (Mutant M4) ist ueber die Mappe ",
                        "nicht erreichbar und bleibt benanntes Loch des Gates (4/5)."),
                 out$diagnostics$historie_monate))
  expect_identical(out$diagnostics$historie_monate, 24L)   # Grenzpunkt exakt getroffen
  expect_true(out$diagnostics$cv_belastbar)                # >= 24 belastbar; M4 (> 24) waere hier FALSE
})
