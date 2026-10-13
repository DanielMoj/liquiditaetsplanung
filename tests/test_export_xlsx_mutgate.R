# ============================================================================
#  tests/test_export_xlsx_mutgate.R  —  Mutgate-Nachschub (rot_zuerst: false)
#  fuer phase-09-xlsx
#
#  Zusatz-Orakel zur Spezifikation tests/test_export_xlsx.R: der Mutgate-Lauf
#  endete 3/5, zwei Ueberlebende, beide im Sammel-Reactive export_ergebnisse
#  von app/app.R — die Sammel-Logik lag im Shiny-Reactive OHNE Testpfad: die
#  Spec testet export_xlsx DAHINTER (bekommt fertige benannte Listen), einen
#  Test DAVOR gab es nicht. Geschlossen nach dem Praezedenzmuster von phase-08
#  (scenarios_from_specs): die Sammel-Logik ist jetzt die benannte, reine
#  Funktion export_ergebnisse_sammeln (R/export.R), die der Reactive aufruft
#  und die hier DIREKT getestet wird. Diese Datei ist gegen den AKTUELLEN
#  (korrekten) Stand gruen und toetet die Ueberlebenden. Keine Anker-Literale,
#  keine Tier-1-Werte, keine Engine, keine Mappe — kleine Laufzeit.
#
#  Block 1 — Filter-Orakel (killt die M5-Klasse):
#    M5 invertierte den NULL-Filter (Filter(is.null, ...) statt
#    Filter(Negate(is.null), ...)) — der Export erhielte GENAU die noch nicht
#    gerechneten Quellen. Das Orakel setzt GENAU ZWEI der vier Quellen
#    (varianz + vergleich) und laesst zwei NULL: der korrekte Filter liefert
#    identical c("varianzzerlegung", "vergleich"); ein invertierter Filter
#    liefert die NULL-Namen (forecast/sim_monate/sim_deckung) und faellt am
#    identical rot. Gegenprobe: alle vier NULL -> Laenge 0 (invertiert: 5).
#    Regime-Waechter als HARTER Fail (kein skip): mindestens eine Quelle NULL
#    UND mindestens eine gesetzt — sonst waere das Orakel filter-blind
#    (alles-gesetzt bzw. alles-NULL unterscheidet die Filterrichtung nicht
#    an den Namen).
#
#  Block 2 — Argument-Orakel (killt die M3-Klasse):
#    M3 vertauschte out und kk_limit beim deckung_from_out-Aufruf
#    (deckung_from_out(rv$kk_limit, o_sim) statt (o_sim, rv$kk_limit)).
#    Das Orakel baut einen sim-Stub im phase_simulation-Ergebnisvertrag
#    (Muster .mk_sim_out aus test_compare.R) plus kk_limit und rechnet
#    P_breach / trough_median EXTERN nach: ein vertauschter Aufruf bricht
#    schon an numerik$trough ab ("$ operator is invalid for atomic vectors")
#    oder liefert falsche Felder — beides faellt rot. Zusaetzlich haengt
#    nrow(sim_monate$table) == H die project_sim_monate-Naht an, und der
#    Fehlerfall (sim ohne kk_limit -> benannter stop) ist gepinnt, damit die
#    Deckungs-Projektion nie stumm mit fehlender Schwelle laeuft.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
# Nur die reinen Funktionsdefinitionen — kein Kernel, kein Szenario-Lader:
# export_ergebnisse_sammeln braucht ausschliesslich project_sim_monate
# (R/phase_sim_monate.R) und deckung_from_out (R/phase_sim_deckung.R).
source(file.path(.root, "R", "phase_sim_monate.R"))
source(file.path(.root, "R", "phase_sim_deckung.R"))
source(file.path(.root, "R", "export.R"))

# ---------------------------------------------------------------------------
# Block 1 — Filter-Orakel: GENAU ZWEI der vier Quellen gesetzt. Die Stubs
# sind fuer den Filter opake Marker (die Funktion reicht varianz/vergleich
# unveraendert durch) — keine Vertragsfelder noetig.
# ---------------------------------------------------------------------------
.vz_stub  <- list(marker = "varianz-quelle")
.vgl_stub <- list(marker = "vergleich-quelle")

test_that("Filter: nur gesetzte Quellen ueberleben (invertierter NULL-Filter faellt rot)", {
  # do.call haelt Guard und Aufruf auf DENSELBEN Eingaben — kein Drift.
  .quellen <- list(forecast = NULL, sim = NULL,
                   varianz = .vz_stub, vergleich = .vgl_stub)
  .ist_null <- vapply(.quellen, is.null, logical(1L))

  # Regime-Waechter als HARTE Fails (kein skip): ohne gemischte Belegung
  # unterscheidet der Namensvergleich die Filterrichtung nicht.
  expect_true(any(.ist_null),
              label = "Regime-Waechter: mindestens eine Quelle ist NULL (sonst filter-blind — bei voller Belegung liefern beide Filterrichtungen dieselben Namen nicht unterscheidbar rot/gruen)")
  expect_true(any(!.ist_null),
              label = "Regime-Waechter: mindestens eine Quelle ist gesetzt (sonst filter-blind fuer die Namensfolge)")

  erg <- do.call(export_ergebnisse_sammeln, .quellen)

  # Korrekter Filter: GENAU die gesetzten Quellen unter ihren Vertragsnamen,
  # in der festen Reihenfolge der Sammel-Liste. Ein invertierter Filter
  # liefert stattdessen die NULL-Namen und faellt am identical rot.
  expect_identical(names(erg), c("varianzzerlegung", "vergleich"))
  expect_identical(erg$varianzzerlegung, .vz_stub)
  expect_identical(erg$vergleich,        .vgl_stub)
})

test_that("Filter-Gegenprobe: alle vier Quellen NULL -> leere Liste (invertiert: Laenge 5)", {
  erg0 <- export_ergebnisse_sammeln()
  expect_true(is.list(erg0))
  expect_length(erg0, 0L)
})

# ---------------------------------------------------------------------------
# Block 2 — Argument-Orakel: synthetischer Stub im phase_simulation-
# Ergebnisvertrag (Muster .mk_sim_out aus test_compare.R, lokal nachgebaut,
# um fan/P_under/H fuer project_sim_monate erweitert) — KEINE Engine, kein
# Seed-Lauf, KEINE Tier-1-Werte. metrics werden konsistent zum trough gebaut.
# ---------------------------------------------------------------------------
.mk_sim_out <- function(trough, fan, P_under, seed) {
  list(
    phase           = "simulation",
    trough          = trough,
    fan             = fan,      # 5 x H, Zeilen "5%","25%","50%","75%","95%"
    P_under         = P_under,  # Anteil je Monat unter Reserve, in [0,1]
    B               = matrix(rep(trough, ncol(fan)), ncol = ncol(fan),
                             dimnames = list(NULL, paste0("M", seq_len(ncol(fan))))),
    plot_path       = NA_character_,
    endbestand_path = NA_character_,
    metrics = list(
      S             = length(trough),
      seed          = seed,
      H             = ncol(fan),
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95))
    )
  )
}

# Sechs Pfade, H = 3: zwei Pfade strikt unter -kk (-950.5, -620.25), vier
# darueber -> P_breach nicht degeneriert (weder 0 noch 1). Alle Zahlen frei
# erfunden und ohne Wiederverwendung aus anderen Tests.
.tr  <- c(-950.5, -620.25, -410.75, -75.5, 260.25, 505.75)
.kk  <- 480.5
.fan <- matrix(c(-881.5, -821.25, -761.75,
                 -441.5, -381.25, -321.75,
                 -121.5,  -61.25,   16.5,
                  141.5,  201.25,  261.75,
                  351.5,  411.25,  471.75),
               nrow = 5, byrow = TRUE,
               dimnames = list(c("5%", "25%", "50%", "75%", "95%"), NULL))
.sim_stub <- .mk_sim_out(.tr, .fan, P_under = c(0.4, 0.2, 0.1), seed = 4242L)

test_that("Argumente: deckung_from_out bekommt (sim, kk_limit) in EXAKT dieser Reihenfolge (vertauschter Aufruf faellt rot)", {
  # Erwartung strikt EXTERN aus den Stub-Literalen gerechnet — NICHT aus der
  # Funktion abgelesen: nur Pfade strikt unter -kk_limit zaehlen.
  P_breach_ext  <- mean(.tr < -.kk)
  trough_med_ext <- unname(median(.tr))

  # Regime-Waechter als HARTER Fail (kein skip): degenerierte P_breach (0
  # oder 1) machte den Anteils-Check trivial und das Orakel schwach.
  expect_true(P_breach_ext > 0 && P_breach_ext < 1,
              label = "Regime-Waechter: P_breach des Stubs weder 0 noch 1 (sonst triviale Anteils-Erwartung)")

  erg <- export_ergebnisse_sammeln(sim = .sim_stub, kk_limit = .kk)

  expect_identical(names(erg), c("sim_monate", "sim_deckung"))
  expect_equal(erg$sim_deckung$P_breach,      P_breach_ext,   tolerance = 1e-12)
  expect_equal(erg$sim_deckung$trough_median, trough_med_ext, tolerance = 1e-12)
  expect_identical(erg$sim_deckung$kk_limit, .kk)

  # project_sim_monate-Naht angebunden: die Monats-Tabelle traegt H = 3
  # Zeilen des Stubs (ein sim_monate ohne echte Projektion faellt hier rot).
  expect_identical(nrow(erg$sim_monate$table), 3L)
})

test_that("Fehlerfall: sim gesetzt, kk_limit NULL -> benannter stop mit kk_limit im Text", {
  expect_error(export_ergebnisse_sammeln(sim = .sim_stub, kk_limit = NULL),
               "kk_limit")
})
