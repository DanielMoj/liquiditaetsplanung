# ============================================================================
#  tests/test_export_docx_ui.R  —  Verdrahtungs-/Bindungs-Orakel (rot_zuerst:
#  TRUE, Teil der Runde-T-SPEZIFIKATION) fuer phase-09-docx
#
#  Lehre aus phase-09-xlsx: dort lag die Naht reactiveValues -> Export bis
#  zum Mutgate-Rerun (M4) ausserhalb jedes Tests und musste als Nachschub
#  (test_export_xlsx_ui.R) nachgeliefert werden. Fuer docx ist das Orakel
#  von Anfang an Teil der Spezifikation: es prueft die GANZE Kette
#  downloadHandler export_docx_btn -> export_ergebnisse (rv-Bindungen)
#  -> export_docx -> geschriebene Datei, ueber den echten Abruf
#  output$export_docx_btn im testServer (der Abruf liefert den Pfad der vom
#  Handler geschriebenen Datei; Mehrfachabruf rechnet frisch — vorab
#  verprobt). Rot zuerst: der Output export_docx_btn existiert im Server
#  noch nicht, der Abruf faellt als Fehler.
#
#  Kill-Mechanik: die vier Quell-rvs werden mit PAARWEISE UNVERWECHSELBAREN
#  synthetischen Stubs belegt (Werteblock A = forecast, B = sim, C =
#  varianz, D = vergleich; rv$kk_limit mit einem eigenen Wert, der in
#  keinem Stub-Zahlenblock vorkommt). Die sim_deckung-Erwartung wird EXTERN
#  aus den Stub-B-Literalen gerechnet (mean(trough < -kk), median(trough))
#  — ein an die falsche Quelle gebundenes sim traegt kein trough-Feld und
#  stirbt in der Projektion oder an der Wertabweichung; jede andere
#  Vertauschung faellt am blockeigenen Zellwert ihres Abschnitts. Ein
#  Handler, der stattdessen export_xlsx schreibt, stirbt an der Ruecklesung
#  (officer::read_docx liest kein xlsx-Zip) bzw. am gepinnten basename
#  liquiditaetsplanung_export.docx. Der varianz-Stub traegt den VOLLEN
#  12-Key-Vertrag, weil der Export ihn projiziert (anders als im xlsx-
#  Schwester-Orakel, wo der Reactive ihn nur durchreichte).
#
#  ABWEICHUNG von den Schwester-Orakeln, bewusst beibehalten: nach der
#  Stub-Belegung KEIN session$flushReact(). Ein Flush rechnete ALLE Outputs
#  (u. a. statusbar gegen rv$out) — die Minimal-Stubs bedienen nur die
#  Export-Naht. Noetig ist er nicht: der Download-Abruf rechnet pull-basiert
#  frisch (verprobt), genau wie der direkte Reactive-Aufruf im xlsx-Orakel.
#
#  Keine Anker-Literale, keine Tier-1-Werte, keine Engine, keine Mappe,
#  kein skip — kleine Laufzeit. Nebenlaeufigkeits-Hygiene wie in den
#  Schwestern: das Sourcen von app/app.R startet bei HAS_ASYNC
#  mirai::daemons(1); die Daemons werden am Dateiende deterministisch
#  geschlossen — auch im Fehlerpfad.
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

# Kompakte Leseform (Muster tests/test_export_docx.R): heading-1-Folge +
# je table_index eine Tabelle (row_id laeuft ueber Tabellen hinweg weiter).
.lies_abschnitte <- function(pfad) {
  s <- officer::docx_summary(officer::read_docx(pfad))
  s <- s[order(s$doc_index), ]
  h <- s[s$content_type == "paragraph" & s$style_name == "heading 1",
         c("doc_index", "text")]
  z <- s[s$content_type == "table cell", ]
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

# ---------------------------------------------------------------------------
# Vier paarweise unverwechselbare Stubs — Werteblock A (~150er, forecast),
# B (~250er/650er, sim), C (~350er + Quadratzahlen, varianz), D (~450er,
# vergleich); alle Zahlen frei erfunden, ohne Wiederverwendung aus anderen
# Tests und ohne Tier-1-Werte. forecast/varianz/vergleich reicht die
# Sammel-Funktion unveraendert an den Export; nur der sim-Stub muss den
# phase_simulation-Ergebnisvertrag tragen (project_sim_monate und
# deckung_from_out projizieren ihn).
# ---------------------------------------------------------------------------

# Werteblock A — forecast-Stub: kleine cash-Matrix + kpis im Vertragsaus-
# schnitt, den der Export liest (guv-Koeder wie in der Spec nicht noetig —
# die selektive Projektion pinnt tests/test_export_docx.R).
.fc_stub <- list(
  phase = "forecast",
  cash  = matrix(c(151.25, 152.5,
                   -158.5, 162.25),
                 nrow = 2L, byrow = TRUE,
                 dimnames = list(c("Zufluss", "Endbestand"), c("M1", "M2"))),
  kpis  = list(trough = -158.5, trough_month = 1L, maxKK = 154.25,
               months_below = 1L, result = 156.75, end = 162.25)
)

# Werteblock B — sim-Stub im phase_simulation-Ergebnisvertrag; metrics
# konsistent zum trough (Muster der Schwester-Orakel).
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

# Sechs Pfade, H = 3: zwei Pfade strikt unter -kk (-955.5, -454.25), vier
# darueber -> P_breach nicht degeneriert (weder 0 noch 1).
.tr      <- c(-955.5, -454.25, -270.75, -85.5, 240.25, 651.75)
.kk      <- 383.5
.P_under <- c(0.36, 0.16, 0.06)
.fan <- matrix(c(-691.5, -652.25, -611.75,
                 -291.5, -252.25, -211.75,
                 -141.5,  -92.25,   28.5,
                  171.5,  212.25,  251.75,
                  331.5,  352.25,  391.75),
               nrow = 5, byrow = TRUE,
               dimnames = list(c("5%", "25%", "50%", "75%", "95%"), NULL))
.sim_stub <- .mk_sim_out(.tr, .fan, P_under = .P_under, seed = 2525L)

# Werteblock C — varianz-Stub: VOLLER 12-Key-Vertrag (der Export projiziert
# ihn in die Key/Wert-Tabelle), eigener Zahlenblock.
.vz_stub <- list(phase = "varianzzerlegung",
                 var_gesamt = 3481, var_epi = 2116, var_ale = 961,
                 var_inter = 404,
                 sd_gesamt = 59, sd_epi = 46, sd_ale = 31,
                 anteil_epi = 0.61, anteil_ale = 0.28, anteil_inter = 0.11,
                 S = 6L, seed = 3535L)

# Werteblock D — vergleich-Stub: tabelle wie der Vertrag, aber bewusst OHNE
# trough-Feld auf oberster Ebene (siehe Guard unten).
.vgl_stub <- list(phase = "vergleich",
                  tabelle = data.frame(Szenario  = c("Basis", "Stress-E"),
                                       tiefpunkt = c(-451.25, -465.5),
                                       ergebnis  = c( 452.75,  441.25),
                                       stringsAsFactors = FALSE),
                  seed = 4545L)

# Vertragskonstante der varianz-Key/Wert-Tabelle (unabhaengiges Literal).
.varianz_keys <- c("var_gesamt", "var_epi", "var_ale", "var_inter",
                   "sd_gesamt", "sd_epi", "sd_ale",
                   "anteil_epi", "anteil_ale", "anteil_inter", "S", "seed")

# Alle numerischen Blaetter eines Stubs (rekursiv; data.frame/Matrix inklusive)
# fuer den kk_limit-Kollisions-Guard.
.zahlen_von <- function(x)
  unname(rapply(x, as.numeric,
                classes = c("numeric", "integer", "matrix", "array"),
                how = "unlist"))

test_that("export_docx_btn schreibt die Session-Staende ueber GENAU ihre rv-Bindungen in die docx; NULL-Quelle faellt aus; leere Sammlung wirft validate", {
  # Guards als HARTE Fails (kein skip): ist einer verletzt, ist das Orakel
  # bindungsblind und muss rot sein, nicht schweigen.
  expect_true(is.numeric(.sim_stub$trough) && length(.sim_stub$trough) >= 2L,
              label = "Regime-Waechter: der sim-Stub traegt einen trough-Vektor (ohne ihn saehe deckung_from_out fuer alle Bindungen gleich aus)")
  expect_null(.vgl_stub$trough,
              label = "Regime-Waechter: der vergleich-Stub traegt auf oberster Ebene KEIN trough-Feld (sonst rechnete ein an sim gebundenes vergleich_rv eine plausible Deckung und das Orakel waere bindungsblind)")
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
    # flushReact (Kopfkommentar): der Download-Abruf rechnet frisch, und
    # kein Render muss die Minimal-Stubs verdauen.
    rv$kk_limit      <- .kk
    rv$out           <- .fc_stub
    sim_rv$out       <- .sim_stub
    var_rv$out       <- .vz_stub
    vergleich_rv$out <- .vgl_stub

    # ABRUF: der downloadHandler schreibt die Datei und liefert ihren Pfad;
    # der basename ist der gepinnte Download-Dateiname.
    pfad <- output$export_docx_btn
    expect_identical(basename(pfad), "liquiditaetsplanung_export.docx")
    expect_true(file.exists(pfad))

    ab <- .lies_abschnitte(pfad)

    # Abschnittsfolge exakt wie export_ergebnisse_sammeln die Liste baut
    # (forecast expandiert zu forecast + forecast_kpis) — eine an die
    # falsche Quelle gebundene NULL liesse Abschnitte fehlen.
    expect_identical(names(ab),
                     c("forecast", "forecast_kpis", "sim_monate",
                       "sim_deckung", "varianzzerlegung", "vergleich"))

    # BINDUNGS-Kern: sim_deckung ist aus dem SIM-Stub (Block B) gerechnet —
    # eine Vertauschung findet kein trough und stirbt schon in der
    # Projektion bzw. an der Wertabweichung; kk_limit ist die rv-Schwelle.
    dk <- ab[["sim_deckung"]]
    expect_equal(as.numeric(dk$wert[dk$kennzahl == "P_breach"]),
                 P_breach_ext, tolerance = 1e-12)
    expect_equal(as.numeric(dk$wert[dk$kennzahl == "trough_median"]),
                 trough_med_ext, tolerance = 1e-12)
    expect_equal(as.numeric(dk$wert[dk$kennzahl == "kk_limit"]),
                 .kk, tolerance = 1e-12)

    # sim_monate-Naht am selben Stub: H = 3 Zeilen, P_unter ist der
    # durchgereichte P_under des SIM-Stubs.
    expect_identical(nrow(ab[["sim_monate"]]), 3L)
    expect_equal(as.numeric(ab[["sim_monate"]]$P_unter), unname(.P_under),
                 tolerance = 1e-12)

    # Je Quelle ein blockeigener Zellwert: jede Nachbar-Vertauschung im
    # Sammel-Reactive faellt an einem dieser Checks rot.
    fc <- ab[["forecast"]]
    expect_identical(fc$Position, c("Zufluss", "Endbestand"))
    expect_equal(as.numeric(fc$M1), c(151.25, -158.5), tolerance = 1e-12)   # Block A
    kp <- ab[["forecast_kpis"]]
    expect_equal(as.numeric(kp$wert),
                 c(-158.5, 1, 154.25, 1, 156.75, 162.25), tolerance = 1e-12)
    vz <- ab[["varianzzerlegung"]]
    expect_identical(vz$kennzahl, .varianz_keys)
    expect_equal(as.numeric(vz$wert[vz$kennzahl == "var_gesamt"]), 3481,
                 tolerance = 1e-12)                                          # Block C
    vg <- ab[["vergleich"]]
    expect_identical(vg$Szenario, c("Basis", "Stress-E"))                    # Block D
    expect_equal(as.numeric(vg$tiefpunkt), c(-451.25, -465.5), tolerance = 1e-12)

    # NULL-Teilprobe im selben testServer: faellt die sim-Quelle weg, fehlen
    # GENAU die sim_-Abschnitte; die uebrigen Bindungen bleiben unveraendert
    # (erneuter Abruf rechnet frisch — vorab verprobt).
    sim_rv$out <- NULL
    pfad2 <- output$export_docx_btn
    ab2 <- .lies_abschnitte(pfad2)
    expect_identical(names(ab2),
                     c("forecast", "forecast_kpis", "varianzzerlegung",
                       "vergleich"))
    expect_equal(as.numeric(ab2[["forecast"]]$M1), c(151.25, -158.5),
                 tolerance = 1e-12)

    # Leerpfad: ALLE Quellen NULL -> leere Sammlung -> der Handler wirft den
    # validate-Hinweis (shiny.silent.error), statt eine leere Datei zu
    # schreiben.
    rv$out           <- NULL
    var_rv$out       <- NULL
    vergleich_rv$out <- NULL
    expect_error(output$export_docx_btn, class = "shiny.silent.error")
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()
