# ============================================================================
#  tests/test_export_pdf_ui.R  —  Verdrahtungs-/Bindungs-Orakel (rot_zuerst:
#  TRUE, Teil der Runde-T-SPEZIFIKATION) fuer phase-09-pdf
#
#  Praezedenz tests/test_export_docx_ui.R (Lehre aus xlsx-M4): das Orakel
#  prueft die GANZE Kette downloadHandler export_pdf_btn ->
#  export_ergebnisse (rv-Bindungen) -> export_pdf -> geschriebene Datei,
#  ueber den echten Abruf output$export_pdf_btn im testServer. Rot zuerst:
#  der Output export_pdf_btn existiert im Server noch nicht.
#
#  Kill-Mechanik: vier Quell-rvs mit PAARWEISE UNVERWECHSELBAREN Stubs
#  (Werteblock A = forecast, B = sim, C = varianz voll 12-Key, D =
#  vergleich; rv$kk_limit mit eigenem Wert ausserhalb aller Stub-Bloecke).
#  Gelesen wird ueber die gestripte Zeilenform der pdf-Spec (U+2212
#  normalisiert): eine Datenzeile ist die exakte Konkatenation ihrer
#  Zellen — die sim_deckung-Zeilen werden EXTERN aus den Stub-B-Literalen
#  gebaut (P_breach = mean(trough < -kk) als as.character-String), jede
#  Quelle liefert eine blockeigene Zeile als Bindungsbeweis. Ein Handler,
#  der ein anderes Medium schreibt, stirbt an der %PDF-Magic bzw. an
#  pdftools; der gepinnte basename liquiditaetsplanung_export.pdf pinnt
#  die filename-Funktion.
#
#  ABWEICHUNG wie in den Schwestern, bewusst: nach der Stub-Belegung KEIN
#  session$flushReact() — der Download-Abruf rechnet pull-basiert frisch;
#  kein Render muss die Minimal-Stubs verdauen. Keine Anker-Literale,
#  keine Tier-1-Werte, keine Engine, keine Mappe, kein skip. Daemon-
#  Hygiene wie in den Schwestern (mirai::daemons(0) am Dateiende, auch im
#  Fehlerpfad).
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

# App in-Process laden (chdir=TRUE wie shiny::runApp("app")).
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

# Gestripte Zeilenform (Muster tests/test_export_pdf.R); reine Zahl-Zeilen
# sind typst-Seitenfusszeilen und fliegen raus.
.lies_zeilen <- function(pfad) {
  tx <- pdftools::pdf_text(pfad)
  z <- unlist(strsplit(paste(tx, collapse = "\n"), "\n"))
  z <- gsub("−", "-", z)
  z <- gsub("[[:space:]]+", "", z)
  z <- z[nzchar(z)]
  z[!grepl("^[0-9]+$", z)]
}

# ---------------------------------------------------------------------------
# Vier paarweise unverwechselbare Stubs — Werteblock A (~170er, forecast),
# B (~270er/700er, sim), C (Quadratzahlen 63/47/37, varianz voll), D
# (~470er, vergleich); alle Zahlen frisch, ohne Tier-1-Werte.
# ---------------------------------------------------------------------------
.fc_stub <- list(
  phase = "forecast",
  cash  = matrix(c(171.25, 172.5,
                   -178.5, 182.25),
                 nrow = 2L, byrow = TRUE,
                 dimnames = list(c("Zufluss", "Endbestand"), c("M1", "M2"))),
  kpis  = list(trough = -178.5, trough_month = 1L, maxKK = 174.25,
               months_below = 1L, result = 176.75, end = 182.25)
)

.mk_sim_out <- function(trough, fan, P_under, seed) {
  list(
    phase           = "simulation",
    trough          = trough,
    fan             = fan,      # 5 x H, Zeilen "5%","25%","50%","75%","95%"
    P_under         = P_under,
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

# Sechs Pfade, H = 3: zwei strikt unter -kk -> P_breach nicht degeneriert.
.tr      <- c(-1055.5, -504.25, -320.75, -95.5, 290.25, 701.75)
.kk      <- 419.5
.P_under <- c(0.38, 0.18, 0.08)
.fan <- matrix(c(-791.5, -752.25, -711.75,
                 -391.5, -352.25, -311.75,
                 -191.5, -142.25,   38.5,
                  221.5,  262.25,  301.75,
                  381.5,  402.25,  441.75),
               nrow = 5, byrow = TRUE,
               dimnames = list(c("5%", "25%", "50%", "75%", "95%"), NULL))
.sim_stub <- .mk_sim_out(.tr, .fan, P_under = .P_under, seed = 2727L)

.vz_stub <- list(phase = "varianzzerlegung",
                 var_gesamt = 3969, var_epi = 2209, var_ale = 1369,
                 var_inter = 391,
                 sd_gesamt = 63, sd_epi = 47, sd_ale = 37,
                 anteil_epi = 0.56, anteil_ale = 0.34, anteil_inter = 0.1,
                 S = 9L, seed = 4747L)

.vgl_stub <- list(phase = "vergleich",
                  tabelle = data.frame(Szenario  = c("Basis", "Stress-P"),
                                       tiefpunkt = c(-471.25, -485.5),
                                       ergebnis  = c( 472.75,  461.25),
                                       stringsAsFactors = FALSE),
                  seed = 5858L)

# Alle numerischen Blaetter eines Stubs fuer den kk_limit-Kollisions-Guard.
.zahlen_von <- function(x)
  unname(rapply(x, as.numeric,
                classes = c("numeric", "integer", "matrix", "array"),
                how = "unlist"))

test_that("export_pdf_btn schreibt die Session-Staende ueber GENAU ihre rv-Bindungen in die pdf; NULL-Quelle faellt aus; leere Sammlung wirft validate", {
  expect_true(is.numeric(.sim_stub$trough) && length(.sim_stub$trough) >= 2L,
              label = "Regime-Waechter: der sim-Stub traegt einen trough-Vektor")
  expect_null(.vgl_stub$trough,
              label = "Regime-Waechter: vergleich-Stub ohne top-level trough (sonst bindungsblind)")
  expect_null(.fc_stub$trough,
              label = "Regime-Waechter: forecast-Stub ohne top-level trough")
  expect_null(.vz_stub$trough,
              label = "Regime-Waechter: varianz-Stub ohne top-level trough")

  P_breach_ext <- mean(.tr < -.kk)
  expect_true(P_breach_ext > 0 && P_breach_ext < 1,
              label = "Regime-Waechter: P_breach weder 0 noch 1")
  expect_false(.kk %in% c(.zahlen_von(.fc_stub), .zahlen_von(.sim_stub),
                          .zahlen_von(.vz_stub), .zahlen_von(.vgl_stub)),
               label = "Regime-Waechter: kk_limit-Wert in keinem Stub-Zahlenblock")

  # Erwartungszeilen strikt EXTERN aus den Stub-B-Literalen gebaut.
  z_pbreach <- paste0("P_breach", as.character(P_breach_ext))
  z_median  <- paste0("trough_median", as.character(unname(median(.tr))))
  z_kk      <- "kk_limit419.5"
  z_monat1  <- "M1-791.5-191.5381.50.38"   # fan 5%/50%/95% + P_under[1]

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> Auto-Trigger inert.

    rv$kk_limit      <- .kk
    rv$out           <- .fc_stub
    sim_rv$out       <- .sim_stub
    var_rv$out       <- .vz_stub
    vergleich_rv$out <- .vgl_stub

    pfad <- output$export_pdf_btn
    expect_identical(basename(pfad), "liquiditaetsplanung_export.pdf")
    expect_true(file.exists(pfad))
    expect_identical(rawToChar(readBin(pfad, "raw", 4L)), "%PDF")

    zs <- .lies_zeilen(pfad)

    # Abschnittsfolge = Sammel-Reihenfolge (forecast expandiert).
    idx <- vapply(c("forecast", "forecast_kpis", "sim_monate", "sim_deckung",
                    "varianzzerlegung", "vergleich"),
                  function(n) which(zs == n)[1], numeric(1))
    expect_false(anyNA(idx))
    expect_true(all(diff(idx) > 0))

    # BINDUNGS-Kern: sim_deckung aus dem SIM-Stub, kk_limit aus der rv-Schwelle.
    expect_true(any(zs == z_pbreach), info = z_pbreach)
    expect_true(any(zs == z_median),  info = z_median)
    expect_true(any(zs == z_kk))

    # sim_monate-Naht am selben Stub (fan/P_under-Zeile M1).
    expect_true(any(zs == z_monat1), info = z_monat1)

    # Je Quelle eine blockeigene Zeile: Nachbar-Vertauschungen fallen hier rot.
    expect_true(any(zs == "Zufluss171.25172.5"))          # Block A
    expect_true(any(zs == "end182.25"))                   # Block A (kpis)
    expect_true(any(zs == "var_gesamt3969"))              # Block C
    expect_true(any(zs == "Basis-471.25472.75"))          # Block D
    expect_true(any(zs == "Stress-P-485.5461.25"))        # Block D

    # NULL-Teilprobe: sim weg -> GENAU die sim_-Abschnitte und -Zeilen weg.
    sim_rv$out <- NULL
    pfad2 <- output$export_pdf_btn
    zs2 <- .lies_zeilen(pfad2)
    idx2 <- vapply(c("forecast", "forecast_kpis", "varianzzerlegung", "vergleich"),
                   function(n) which(zs2 == n)[1], numeric(1))
    expect_false(anyNA(idx2))
    expect_true(all(diff(idx2) > 0))
    expect_false(any(zs2 == "sim_deckung"))
    expect_false(any(zs2 == "sim_monate"))
    expect_false(any(zs2 == z_monat1))
    expect_true(any(zs2 == "Zufluss171.25172.5"))

    # Leerpfad: ALLE Quellen NULL -> validate-Fehler statt leerer Datei.
    rv$out           <- NULL
    var_rv$out       <- NULL
    vergleich_rv$out <- NULL
    expect_error(output$export_pdf_btn, class = "shiny.silent.error")
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht).
.daemons_zu()
