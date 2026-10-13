# ============================================================================
#  R/phase_verteilungen.R  —  Phase 4 (Verteilungen) als Funktion
#
#  Ruft die EINGEFRORENE Engine (kernel/distributions.R) auf und teilt die
#  Unsicherheit je Treiber in zwei Arten auf:
#    epistemisch  — Unwissen ueber die Jahresannahme: PERT ueber min/mode/max
#                   (mode = Punktannahme mdl$assume, min/max aus Blatt 06).
#    aleatorisch  — irreduzible monatliche Streuung um den Pfad (CV), wo sinnvoll
#                   aus Ist-Residuen geschaetzt (estimate_cv_harmonic), sonst Vorgabe.
#
#  Gibt ein Ergebnisobjekt zurueck (spec + cv_est + Tabelle + Plot-Pfad +
#  spec.rds-Pfad + Diagnose), statt in die Konsole zu drucken. Rechenlogik
#  unveraendert aus legacy/03_verteilungen.R; das cat(...) wird zu Listeneintraegen.
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R und R/scenario.R
#  bereits gesourct sind (load_model, build_dists, estimate_cv_harmonic, qpert, rpert).
#
#  Basis-Allokation (F: konsistent mit noise_driver in legacy/05_simulation.R):
#  die aleatorische CV wird ueber den DRIVER zugeordnet — menge ueber die
#  Positionen mit driver in {menge, menge_preis}, material ueber driver == material.
#  Damit landet m_leb (grp=material, driver=menge) in der MENGEN-Basis (es schwankt
#  mit der Menge), nicht in der Material-Basis.
# ============================================================================

# PERT-Histogramme je Treiber (epistemische Bandbreite der Jahresannahme mit
# Mode-Markierung) plus das aleatorische Ist-Residuen-Histogramm der Menge als PNG
# in den Szenario-Workspace. Gibt den Pfad zurueck, oder NA, wenn kein Grafikgeraet
# verfuegbar ist (sauberer Headless-Fallback — nie ein harter Abbruch).
.plot_verteilungen <- function(spec, vol_series, path) {
  tryCatch({
    n_pan <- length(spec) + 1L               # ein Panel je Treiber + Residuen-Panel
    nc    <- max(1L, ceiling(sqrt(n_pan)))
    nr    <- ceiling(n_pan / nc)
    grDevices::png(path, width = 1150, height = 640)
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mfrow = c(nr, nc), mar = c(4, 3, 3, 1)); on.exit(graphics::par(op), add = TRUE)
    for (d in names(spec)) {
      s <- spec[[d]]; x <- rpert(20000, s$min, s$mode, s$max)
      graphics::hist(100 * x, breaks = 40, col = "#1F3864", border = "white", freq = FALSE,
                     main = paste0(d, "  (epistemisch)"), xlab = "Jahresrate [%]")
      graphics::abline(v = 100 * s$mode, col = "#C00000", lwd = 2)
    }
    # Aleatorisches Ist-Residuen-Histogramm der Menge (harmonischer Saisonpfad
    # herausgerechnet). Nur wenn die Reihe streng positiv und lang genug ist.
    vs <- as.numeric(vol_series)
    if (length(vs) >= 8 && all(is.finite(vs)) && all(vs > 0)) {
      t <- seq_along(vs)
      X <- cbind(1, sin(2 * pi * t / 12), cos(2 * pi * t / 12),
                    sin(4 * pi * t / 12), cos(4 * pi * t / 12))
      res <- lm.fit(X, log(vs))$residuals
      graphics::hist(100 * res, breaks = 8, col = "#7F9AC0", border = "white", freq = FALSE,
                     main = "Menge: Ist-Residuen (aleatorisch)", xlab = "Abweichung vom Saisonpfad [%]")
    } else {
      graphics::plot.new()
      graphics::title(main = "Menge: Ist-Residuen (aleatorisch)")
      graphics::text(0.5, 0.5, "Reihe zu kurz / nicht positiv", col = "grey40")
    }
    path
  }, error = function(e) NA_character_)
}

phase_verteilungen <- function(sc, plot = TRUE) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  # Blatt 06_Verteilungen ist Pflicht fuer diese Phase (Quelle der epistemischen
  # Baender). Fehlt es, ist die Aufteilung nicht ableitbar -> benannter Fehler.
  if (is.null(mdl$dists_raw))
    stop("Blatt 06_Verteilungen fehlt in der Mappe — die Verteilungsspezifikation ist nicht ableitbar.")

  # ---- aleatorische CV je Basis aus den Ist-Residuen schaetzen -------------
  # Basis ueber den driver (siehe Kopf): menge = {menge, menge_preis}, material = {material}.
  D          <- mdl$pos$driver
  vol_series <- colSums(mdl$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  mat_series <- colSums(mdl$base[D == "material",                 , drop = FALSE])
  est <- list(menge    = estimate_cv_harmonic(vol_series),
              material = estimate_cv_harmonic(mat_series))

  # ---- Verteilungsspezifikation aus der eingefrorenen Engine ----------------
  spec <- build_dists(mdl, est)
  if (!length(spec))
    stop("Keine Treiber in Blatt 06_Verteilungen — leere Verteilungsspezifikation.")

  # ---- Treiber-Tabelle: epistemische Quantile (PERT) + aleatorische CV ------
  pct <- function(x) sprintf("%+.1f%%", 100 * x)
  rows <- lapply(names(spec), function(d) {
    s <- spec[[d]]; q <- qpert(c(.05, .5, .95), s$min, s$mode, s$max)
    data.frame(Treiber = d, Mode = pct(s$mode), P5 = pct(q[1]), P50 = pct(q[2]), P95 = pct(q[3]),
               CV = sprintf("%.1f%%", 100 * ifelse(is.na(s$cv), 0, s$cv)),
               Quelle = s$cv_source, stringsAsFactors = FALSE)
  })
  table <- do.call(rbind, rows); rownames(table) <- NULL

  # ---- Diagnose (Kennzahlen des Schirms) ------------------------------------
  src              <- vapply(spec, function(s) s$cv_source, character(1))
  # Rohe Monatszahl der Blatt-Historie (kernel: mdl$n_hist) — ncol(mdl$base) sieht
  # nie >= 24, weil load_model das Basisjahr immer auf 12 Monate rotiert.
  # Fallback fuer aeltere Modellobjekte ohne n_hist.
  historie_monate  <- if (!is.null(mdl$n_hist)) mdl$n_hist else ncol(mdl$base)
  basis_text <- paste0("menge = Menge- + Menge_Preis-Basis (m_leb liegt hier), ",
                       "material = Material-Basis; aleatorische Schaetzung ab >=24 Monaten Historie belastbar.")
  diagnostics <- list(
    n_treiber       = length(spec),
    historie_monate = historie_monate,
    cv_belastbar    = historie_monate >= 24,           # Badge: Historie >= 24 Monate
    cv_geschaetzt_n = sum(src == "geschaetzt"),
    cv_vorgabe_n    = sum(src == "Vorgabe"),
    basis_text      = basis_text
  )

  # ---- Pruefplot in den Szenario-Workspace ----------------------------------
  plot_path <- if (isTRUE(plot))
    .plot_verteilungen(spec, vol_series, file.path(sc$workspace, "verteilungen.png")) else NA_character_

  # ---- Spezifikation fuer Phase 5/6 sichern (Artefakt-Vertrag) --------------
  spec_path <- file.path(sc$workspace, "verteilungen_spec.rds")
  saveRDS(spec, spec_path)

  list(
    phase       = "verteilungen",
    scenario    = sc$name,
    spec        = spec,          # Liste je Treiber: mode/min/max/cv/cv_source (bit-identisch zur Engine)
    cv_est      = est,           # geschaetzte aleatorische CVs je Basis (menge/material)
    table       = table,         # Treiber, Mode, P5, P50, P95, CV, Quelle (fuer die UI)
    plot_path   = plot_path,     # PNG im Workspace ODER NA (Headless-Fallback)
    spec_path   = spec_path,     # gesicherte verteilungen_spec.rds (Eingabe Phase 5/6)
    diagnostics = diagnostics
  )
}
