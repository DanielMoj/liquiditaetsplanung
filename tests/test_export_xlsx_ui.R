# ============================================================================
#  tests/test_export_xlsx_ui.R  —  Bindungs-Orakel (rot_zuerst: false) fuer den
#  Sammel-Reactive export_ergebnisse (Mutgate-Nachschub Runde 2, phase-09-xlsx)
#
#  Der Rerun ueberlebte nur noch M4: der Reactive export_ergebnisse in
#  app/app.R bindet sim = vergleich_rv$out statt sim_rv$out. Die reine
#  Sammel-Funktion export_ergebnisse_sammeln ist scharf getestet
#  (tests/test_export_xlsx_mutgate.R, Filter- und Argument-Orakel) — aber die
#  rv-zu-PARAMETER-Bindung im Shiny-Reactive lag ausserhalb jedes Tests: die
#  Funktions-Orakel bekommen ihre Eingaben direkt, einen Test der Naht
#  reactiveValues -> benanntes Argument gab es nicht. Dieselbe Luecken-Klasse
#  schliessen die Schwester-Orakel test_phase_sim_deckung_ui.R (deckung_recon)
#  und test_phase_var_mutgate.R Block 2 (varianz_recon) fuer Render-Bindungen;
#  hier wird KEIN HTML gerendert — der Rueckgabewert des Reactives wird DIREKT
#  geprueft, xml2 wird deshalb nicht gebraucht.
#
#  Kill-Mechanik (M4 und jede Nachbar-Vertauschung im Reactive): die vier
#  Quell-rvs werden mit PAARWEISE UNVERWECHSELBAREN synthetischen Stubs belegt
#  (Werteblock A = forecast, B = sim, C = varianz, D = vergleich; rv$kk_limit
#  mit einem eigenen Wert, der in keinem Stub-Zahlenblock vorkommt). Die
#  sim_deckung-Erwartung wird EXTERN aus den Stub-B-Literalen gerechnet
#  (mean(trough < -kk), median(trough)) — ein an sim gebundenes vergleich_rv
#  (M4) traegt auf oberster Ebene KEIN trough-Feld und faellt in
#  deckung_from_out/project_sim_monate am Fehler bzw. an der Wertabweichung
#  rot; jede andere Vertauschung faellt am identical des falschen Werteblocks
#  (durchgereichte Quellen) oder an der gepinnten Namensfolge.
#
#  ABWEICHUNG von den Schwestern, bewusst: nach der Stub-Belegung KEIN
#  session$flushReact(). Die MockShinySession rechnet beim Flush ALLE Outputs
#  — u. a. statusbar gegen rv$out und vergleich_tbl gegen die Stub-Tabelle —
#  und die Minimal-Stubs bedienen nur die Sammel-Naht, nicht jeden Render.
#  Noetig ist der Flush nicht: reactive() ist pull-basiert, die Zuweisung an
#  eine reactiveValues-Quelle invalidiert den Reactive sofort, der direkte
#  Aufruf export_ergebnisse() rechnet frisch (auch fuer die NULL-Teilprobe).
#
#  Keine Anker-Literale, keine Tier-1-Werte, keine Engine, keine Mappe, kein
#  skip — kleine Laufzeit. Nebenlaeufigkeits-Hygiene wie in den Schwestern:
#  das Sourcen von app/app.R startet bei HAS_ASYNC mirai::daemons(1); der
#  onStop-Handler feuert unter testServer nie. Die Daemons werden am
#  Dateiende deterministisch geschlossen — auch im Fehlerpfad.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# Beim Sourcen gestartete mirai-Daemons schliessen (Suite-Invariante: kein Leak).
.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

# App in-Process laden. chdir=TRUE laesst find_root() der App aus app/ heraus
# die Repo-Wurzel bestimmen — wie bei shiny::runApp("app").
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()  # ein halb gesourctes app.R darf keinen Daemon zuruecklassen
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

# ---------------------------------------------------------------------------
# Vier paarweise unverwechselbare Stubs — Werteblock A (~100er, forecast),
# B (~200er/600er, sim), C (~300er, varianz), D (~400er, vergleich); alle
# Zahlen frei erfunden, ohne Wiederverwendung aus anderen Tests und ohne
# Tier-1-Werte. forecast/varianz/vergleich reicht die Sammel-Funktion
# UNVERAENDERT durch (identical-Orakel); nur der sim-Stub muss den
# phase_simulation-Ergebnisvertrag tragen (Muster .mk_sim_out aus
# test_export_xlsx_mutgate.R), weil project_sim_monate und deckung_from_out
# ihn projizieren.
# ---------------------------------------------------------------------------

# Werteblock A — forecast-Stub: kleine cash-Matrix + kpis im
# phase_forecast-Vertragsausschnitt, den export_xlsx spaeter liest.
.fc_stub <- list(
  phase = "forecast",
  cash  = matrix(c(101.25, 102.5,
                   -108.5, 112.25),
                 nrow = 2L, byrow = TRUE,
                 dimnames = list(c("Zufluss", "Endbestand"), c("M1", "M2"))),
  kpis  = list(trough = -108.5, trough_month = 1L, maxKK = 104.25,
               months_below = 1L, result = 106.75, end = 112.25)
)

# Werteblock B — sim-Stub im phase_simulation-Ergebnisvertrag; metrics
# konsistent zum trough (die Projektion rechnet aus trough; ein konsistenter
# Stub haelt beide moeglichen Quellen gleich).
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

# Sechs Pfade, H = 3: zwei Pfade strikt unter -kk (-905.5, -404.25), vier
# darueber -> P_breach nicht degeneriert (weder 0 noch 1).
.tr      <- c(-905.5, -404.25, -220.75, -55.5, 210.25, 601.75)
.kk      <- 333.5
.P_under <- c(0.34, 0.18, 0.07)
.fan <- matrix(c(-641.5, -602.25, -561.75,
                 -241.5, -202.25, -161.75,
                  -91.5,  -42.25,   18.5,
                  121.5,  162.25,  201.75,
                  281.5,  302.25,  341.75),
               nrow = 5, byrow = TRUE,
               dimnames = list(c("5%", "25%", "50%", "75%", "95%"), NULL))
.sim_stub <- .mk_sim_out(.tr, .fan, P_under = .P_under, seed = 2121L)

# Werteblock C — varianz-Stub: wird unveraendert durchgereicht, opaker Marker
# mit eigenem Zahlenblock genuegt (kein Feld wird von der Naht projiziert).
.vz_stub <- list(phase = "varianzzerlegung",
                 sd_epi = 310.25, sd_ale = 320.75, anteil_epi = 0.66,
                 seed = 3131L, S = 4L)

# Werteblock D — vergleich-Stub: tabelle wie der Vertrag, aber bewusst OHNE
# trough-Feld auf oberster Ebene (siehe Guard unten — genau daran stirbt M4).
.vgl_stub <- list(phase = "vergleich",
                  tabelle = data.frame(Szenario  = c("Basis", "Stress-D"),
                                       tiefpunkt = c(-401.25, -455.5),
                                       ergebnis  = c( 402.75,  391.25),
                                       stringsAsFactors = FALSE),
                  seed = 4141L)

# Alle numerischen Blaetter eines Stubs (rekursiv; data.frame/Matrix inklusive)
# fuer den kk_limit-Kollisions-Guard.
.zahlen_von <- function(x)
  unname(rapply(x, as.numeric,
                classes = c("numeric", "integer", "matrix", "array"),
                how = "unlist"))

test_that("export_ergebnisse bindet jede Quell-rv an GENAU ihren Parameter (sim = vergleich_rv$out, M4, faellt rot); NULL-Quelle faellt aus der Sammlung", {
  # Guards als HARTE Fails (kein skip): ist einer verletzt, ist das Orakel
  # bindungsblind und muss rot sein, nicht schweigen.
  expect_true(is.numeric(.sim_stub$trough) && length(.sim_stub$trough) >= 2L,
              label = "Regime-Waechter: der sim-Stub traegt einen trough-Vektor (ohne ihn saehe deckung_from_out fuer sim- und vergleich-Bindung gleich aus)")
  expect_null(.vgl_stub$trough,
              label = "Regime-Waechter: der vergleich-Stub traegt auf oberster Ebene KEIN trough-Feld (sonst rechnete ein an sim gebundenes vergleich_rv (M4) eine plausible Deckung und das Orakel waere bindungsblind)")
  expect_null(.fc_stub$trough,
              label = "Regime-Waechter: der forecast-Stub traegt auf oberster Ebene KEIN trough-Feld (Nachbar-Vertauschung sim = rv$out muss an der Deckungs-Projektion sterben)")
  expect_null(.vz_stub$trough,
              label = "Regime-Waechter: der varianz-Stub traegt auf oberster Ebene KEIN trough-Feld (Nachbar-Vertauschung sim = var_rv$out muss an der Deckungs-Projektion sterben)")

  P_breach_ext <- mean(.tr < -.kk)
  expect_true(P_breach_ext > 0 && P_breach_ext < 1,
              label = "Regime-Waechter: P_breach des sim-Stubs weder 0 noch 1 (sonst triviale Anteils-Erwartung)")
  expect_false(.kk %in% c(.zahlen_von(.fc_stub), .zahlen_von(.sim_stub),
                          .zahlen_von(.vz_stub), .zahlen_von(.vgl_stub)),
               label = "Regime-Waechter: der kk_limit-Wert kommt in KEINEM Stub-Zahlenblock vor (sonst koennte eine falsche kk_limit-Bindung zufaellig denselben Wert liefern)")

  # Erwartungen strikt EXTERN aus den Stub-B-Literalen gerechnet — NICHT aus
  # deckung_from_out abgelesen (sonst pruefte die Naht sich gegen sich selbst).
  trough_med_ext <- unname(median(.tr))

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> die
                                           # Auto-Trigger (Sim/Varianz) inert.

    # Die vier Quell-rvs + Modell-Schwelle belegen. Danach BEWUSST kein
    # flushReact (Kopfkommentar): der direkte Reactive-Aufruf rechnet frisch,
    # und kein Render muss die Minimal-Stubs verdauen.
    rv$kk_limit      <- .kk
    rv$out           <- .fc_stub
    sim_rv$out       <- .sim_stub
    var_rv$out       <- .vz_stub
    vergleich_rv$out <- .vgl_stub

    erg <- export_ergebnisse()

    # Namensfolge exakt wie export_ergebnisse_sammeln die Liste baut
    # (R/export.R): forecast, sim_monate, sim_deckung, varianzzerlegung,
    # vergleich — eine an sim gebundene NULL-Quelle liesse sim_* hier fehlen.
    expect_identical(names(erg),
                     c("forecast", "sim_monate", "sim_deckung",
                       "varianzzerlegung", "vergleich"))

    # BINDUNGS-Kern: sim_deckung ist aus dem SIM-Stub (Block B) gerechnet —
    # M4 (sim = vergleich_rv$out) findet dort kein trough und stirbt schon in
    # der Projektion bzw. an der Wertabweichung.
    expect_equal(erg$sim_deckung$P_breach,      P_breach_ext,   tolerance = 1e-12)
    expect_equal(erg$sim_deckung$trough_median, trough_med_ext, tolerance = 1e-12)
    expect_identical(erg$sim_deckung$kk_limit, .kk)   # rv$kk_limit, kein Stub-Wert

    # sim_monate-Naht am selben Stub: H = 3 Zeilen, P_unter ist der
    # durchgereichte P_under des SIM-Stubs.
    expect_identical(nrow(erg$sim_monate$table), 3L)
    expect_identical(erg$sim_monate$table$P_unter, unname(.P_under))

    # Durchgereichte Quellen: jede identisch GENAU ihrem Werteblock — jede
    # Nachbar-Vertauschung im Reactive faellt an einem dieser identical rot.
    expect_identical(erg$forecast,         .fc_stub)   # Block A, nicht C/D
    expect_identical(erg$varianzzerlegung, .vz_stub)   # Block C, nicht D
    expect_identical(erg$vergleich,        .vgl_stub)  # Block D, nicht B

    # NULL-Teilprobe im selben testServer: faellt die sim-Quelle weg, fallen
    # GENAU sim_monate/sim_deckung aus der Sammlung; die uebrigen Bindungen
    # bleiben unveraendert (erneuter Reactive-Aufruf, pull-basiert).
    sim_rv$out <- NULL
    erg2 <- export_ergebnisse()
    expect_identical(names(erg2),
                     c("forecast", "varianzzerlegung", "vergleich"))
    expect_identical(erg2$forecast,         .fc_stub)
    expect_identical(erg2$varianzzerlegung, .vz_stub)
    expect_identical(erg2$vergleich,        .vgl_stub)
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
