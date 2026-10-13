# ============================================================================
#  R/manifest_anbindung.R -- Run-Manifest (Anbindung) der GoBD-Schicht
#  (phase-12-runmanifest-anbindung)
#
#  Lauf-Pfad-Hook (GoBD Rz. 100/152): simulation_mit_manifest verpackt EINEN
#  Simulationslauf revisionsfest -- runs_dir/<run_id>/ mit byte-identischer
#  Eingabekopie, endbestand.rds-Kopie und write-once-Manifest (v2 inkl. der
#  vier protokollierten IKS-Kontrollen) -- ohne einen Rechenweg zu
#  veraendern: das Ergebnisobjekt von phase_simulation bleibt identisch bis
#  auf GENAU EIN Beobachterfeld gobd (Beobachter-Invarianz, Blockprinzip
#  c17d29a); vor der epistemischen Ziehung verbraucht der Hook keinen RNG.
#
#  Vertrag: handoff/phase-12-runmanifest-anbindung.json; Spezifikation:
#  tests/test_manifest_anbindung.R. Setzt voraus, dass kernel/engine.R,
#  kernel/distributions.R, R/scenario.R, R/phase_forecast.R,
#  R/phase_simulation.R und R/manifest.R bereits gesourct sind.
# ============================================================================

# soll/ist-Normierung fuer den gemischttypigen kontrollen-data.frame:
# sprintf('%.15g') fuer Zahlen, 'TRUE'/'FALSE' fuer Boolesche -- JSON-robust;
# die maschinenlesbare Wahrheit bleibt die logical-Spalte bestanden.
.gobd_str <- function(x) {
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  sprintf("%.15g", x)
}

simulation_mit_manifest <- function(sc, runs_dir,
                                    zeit = Sys.time(),
                                    user = Sys.info()[["user"]],
                                    plot = TRUE) {
  stopifnot(inherits(sc, "liqui_scenario"))
  if (!is.character(runs_dir) || length(runs_dir) != 1L ||
      is.na(runs_dir) || !nzchar(runs_dir)) {
    stop("simulation_mit_manifest: runs_dir muss ein nicht leerer Pfad sein ",
         "(ABSOLUT uebergeben -- der Worker-cwd ist nicht die Repo-Wurzel).",
         call. = FALSE)
  }
  # user-Fallback 'unbekannt' bei leer/NA (z. B. nackte Worker-Umgebung):
  # manifest_erstellen verlangt einen nicht leeren String.
  if (!is.character(user) || length(user) != 1L || is.na(user) || !nzchar(user)) {
    user <- "unbekannt"
  }

  # (a) IKS-Vorstufe: deterministisch und leichtgewichtig -- nur run_model/
  #     validate_history/implied_dpo_dso, kein RNG, kein MC.
  forecast <- phase_forecast(sc)

  # (b) Der eigentliche Lauf, UNVERAENDERT: phase_simulation setzt
  #     set.seed(seed) selbst direkt vor der epistemischen Ziehung.
  out <- phase_simulation(sc, plot = plot)

  # (c) Vier IKS-Kontrollen (Rz. 100):
  #     [1] anker_plan_tiefpunkt -- ZWEI UNABHAENGIGE WEGE durch denselben
  #         deterministischen Kern (phase_forecast reicht run_model(mdl)$kpis
  #         $trough durch; phase_simulation rechnet plan_trough VOR set.seed
  #         als run_model(mdl)$kpis$trough), Identitaet bei Toleranz 0.
  #     [2]/[3] UEBERNOMMEN aus der bestehenden Forecast-Diagnostik, KEINE
  #         Neurechnung; die Quelle steht woertlich in details.
  #     [4] treiber_whitelist -- ECHTE Pruefung: setdiff der angewandten
  #         assume-Namen gegen die Modell-Whitelist names(sc$mdl$assume);
  #         ehrliches FALSE bei Fremdschluesseln im assume-Schnappschuss
  #         (Befund .orch/review-pakete/befund-paket-phase-12-runmanifest-
  #         anbindung.md, Spec-Commit f16532f). Die harte Durchsetzung am
  #         Konstruktor (scenario_from_workbook stoppt vor jeder Rechnung)
  #         bleibt bestehen und wird hier NICHT dupliziert, sondern im Lauf
  #         gegengeprueft; der Mappen-Hash im Manifest schliesst die Kette
  #         (der Pruefer kann die Whitelist aus der archivierten Mappe
  #         unabhaengig ableiten).
  anker_soll <- forecast$kpis$trough
  anker_ist  <- out$metrics$plan_trough
  masse_ok   <- isTRUE(forecast$diagnostics$massenerhaltung)
  gv_ok      <- isTRUE(forecast$diagnostics$gv_konsistenz)
  wl_soll  <- names(sc$mdl$assume)
  wl_ist   <- names(sc$assume)
  wl_fremd <- setdiff(wl_ist, wl_soll)
  # Wertstring defensiv: .gobd_str nur fuer logical/numeric, sonst
  # as.character -- ein fremder Nicht-Zahl-Wert darf die Protokollierung
  # nicht sprengen (ehrlich protokollieren schlaegt hart scheitern).
  wl_wert <- vapply(sc$assume, function(x) {
    if (is.logical(x) || is.numeric(x)) return(.gobd_str(x))
    paste(as.character(x), collapse = ", ")
  }, character(1))
  wl_details <- paste0("angewandt: ",
                       paste(sprintf("%s=%s", wl_ist, wl_wert),
                             collapse = ", "))
  if (length(wl_fremd) > 0L) {
    wl_details <- paste0(wl_details, " | fremd: ",
                         paste(wl_fremd, collapse = ", "))
  }
  kontrollen <- data.frame(
    name      = c("anker_plan_tiefpunkt", "massenerhaltung",
                  "gv_konsistenz", "treiber_whitelist"),
    bestanden = c(isTRUE(anker_ist == anker_soll), masse_ok, gv_ok,
                  length(wl_fremd) == 0L),
    soll      = c(.gobd_str(anker_soll), "TRUE", "TRUE",
                  paste(wl_soll, collapse = ", ")),
    ist       = c(.gobd_str(anker_ist), .gobd_str(masse_ok),
                  .gobd_str(gv_ok), paste(wl_ist, collapse = ", ")),
    details   = c("phase_forecast$kpis$trough vs metrics$plan_trough",
                  "phase_forecast$diagnostics$massenerhaltung",
                  "phase_forecast$diagnostics$gv_konsistenz",
                  wl_details),
    stringsAsFactors = FALSE)

  # (d) Manifest-Vorstufe OHNE outputs -- NUR zur run_id-Ableitung: die
  #     run_id haengt im Kern nur am UTC-Zeitanteil plus sha256 ueber
  #     sortierte Eingabe-Hashes, Seed-Namen, Seed-Werte und user; outputs
  #     und kontrollen gehen NICHT ein. Die Umgebung wird EINMAL erfasst,
  #     damit Vorstufe und finales Manifest dieselbe tragen.
  umgebung  <- umgebung_erfassen()
  parameter <- c(list(scenario = sc$name, S = sc$sim$paths, H = sc$mdl$HMAX),
                 as.list(sc$assume))
  seeds     <- c(sim = sc$sim$seed)
  m0 <- manifest_erstellen(eingaben = sc$workbook, parameter = parameter,
                           seeds = seeds, zeit = zeit, user = user,
                           umgebung = umgebung, kontrollen = kontrollen)
  run_id  <- m0$run_id
  run_dir <- file.path(runs_dir, run_id)

  # (e) write-once auf VERZEICHNISEBENE: existiert runs_dir/<run_id>/
  #     bereits, benannter Fehler MIT dem Pfad -- NICHTS wird ueberschrieben.
  if (dir.exists(run_dir)) {
    stop("simulation_mit_manifest: write-once verletzt -- Run-Verzeichnis ",
         "existiert bereits: ", run_dir, call. = FALSE)
  }
  if (!dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("simulation_mit_manifest: Run-Verzeichnis nicht anlegbar: ",
         run_dir, call. = FALSE)
  }

  # (f) Eingabe-Mappe byte-identisch hineinkopieren + endbestand.rds aus dem
  #     Szenario-Workspace sichern (out$endbestand_path, phase-06-fan).
  kopie_wb <- file.path(run_dir, basename(sc$workbook))
  if (!file.copy(sc$workbook, kopie_wb, overwrite = FALSE)) {
    stop("simulation_mit_manifest: Mappen-Kopie fehlgeschlagen: ",
         kopie_wb, call. = FALSE)
  }
  kopie_rds <- file.path(run_dir, "endbestand.rds")
  if (!file.copy(out$endbestand_path, kopie_rds, overwrite = FALSE)) {
    stop("simulation_mit_manifest: endbestand.rds-Kopie fehlgeschlagen: ",
         kopie_rds, call. = FALSE)
  }

  # (g) FINALES Manifest MIT outputs = beide Kopien (IDENTISCHE run_id,
  #     siehe d) -- write-once nach <runs_dir>/<run_id>/manifest.json.
  m <- manifest_erstellen(eingaben = sc$workbook, parameter = parameter,
                          seeds = seeds, outputs = c(kopie_wb, kopie_rds),
                          zeit = zeit, user = user,
                          umgebung = umgebung, kontrollen = kontrollen)
  stopifnot(identical(m$run_id, run_id))
  manifest_pfad <- file.path(run_dir, "manifest.json")
  manifest_schreiben(m, manifest_pfad)

  # (h) GENAU EIN Beobachterfeld ans Ende -- alle anderen Felder unberuehrt
  #     (Beobachter-Invarianz).
  out$gobd <- list(run_id = run_id, manifest_pfad = manifest_pfad,
                   kontrollen = kontrollen)
  out
}
