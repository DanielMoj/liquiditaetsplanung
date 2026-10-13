# ============================================================================
#  R/phase_korrelation.R  —  Phase 5 (Korrelation) als Funktion
#
#  Ruft die EINGEFRORENE Engine (kernel/distributions.R) auf: liest die
#  Zielkorrelation der epistemischen Treiber aus Blatt 07, richtet sie auf die
#  Treiberreihenfolge von build_dists aus (die Simulation indiziert spaeter
#  R[drivers, drivers]) und repariert sie mit make_psd zur naechsten positiv-
#  semidefiniten Matrix. Die UI verbiegt Zielmatrix, Ausrichtung und Reparatur
#  NICHT — out$R ist bit-identisch zum direkten Engine-Lauf make_psd(M).
#
#  Gibt ein Ergebnisobjekt zurueck (R, spec, M, method, ev_before, ev_after,
#  max_aenderung, Plot-Pfad, korrelation.rds-Pfad, Diagnose), statt in die
#  Konsole zu drucken. Rechenlogik unveraendert aus legacy/04_abhaengigkeit.R;
#  ABER: die Reparatur laeuft ueber den eingefrorenen Kernel make_psd (Eigenwert-
#  Clipping), NICHT ueber Matrix::nearPD — so bleibt out$R exakt der Engine-Wert
#  (Treue-Vertrag) und Phase 6 erbt eine deterministisch reproduzierbare Matrix.
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R und R/scenario.R
#  bereits gesourct sind (load_model, build_dists, make_psd).
#
#  Artefakt-Vertrag: korrelation.rds im Szenario-Workspace ist die Eingabe fuer
#  Phase 6 (Simulation). dimnames(R) == names(spec) — der Eingabevertrag.
# ============================================================================

# Heatmap der reparierten Korrelationsmatrix (Treiberachsen, Zellwerte,
# Divergenzpalette rot-weiss-blau ueber -1..1) als PNG in den Szenario-Workspace.
# Gibt den Pfad zurueck, oder NA, wenn kein Grafikgeraet verfuegbar / das Ziel
# nicht schreibbar ist (sauberer Headless-Fallback — nie ein harter Abbruch).
.plot_abhaengigkeit <- function(R, path) {
  tryCatch({
    drivers <- rownames(R); if (is.null(drivers)) drivers <- as.character(seq_len(nrow(R)))
    k   <- nrow(R)
    col <- grDevices::colorRampPalette(c("#C0504D", "white", "#1F3864"))(101)
    # png() auf einem nicht schreibbaren Ziel WARNT ("unable to open file") und
    # wirft danach — der Wurf loest den sauberen NA-Fallback aus; die Warnungen
    # sind reines Rauschen und werden stummgeschaltet (kein stiller Skip: der
    # harte Vertrag ist die Existenzpruefung des Aufrufers bzw. das NA hier).
    suppressWarnings(grDevices::png(path, width = 640, height = 600))
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mar = c(7, 7, 4, 2)); on.exit(graphics::par(op), add = TRUE)
    # t(R[k:1, ]) dreht die Zeilen, damit Zeile 1 (erster Treiber) oben liegt.
    graphics::image(1:k, 1:k, t(R[k:1, , drop = FALSE]), col = col, zlim = c(-1, 1),
                    axes = FALSE, xlab = "", ylab = "", main = "Treiber-Korrelation (repariert)")
    graphics::axis(1, 1:k, drivers,      las = 2)
    graphics::axis(2, 1:k, rev(drivers), las = 2)
    for (i in 1:k) for (j in 1:k)
      graphics::text(j, k - i + 1, sprintf("%.2f", R[i, j]), cex = 0.9)
    path
  }, error = function(e) NA_character_)
}

phase_korrelation <- function(sc, plot = TRUE) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  # Blatt 07_Korrelation ist Pflicht — ohne die Zielmatrix ist die Abhaengigkeit
  # der epistemischen Treiber nicht ableitbar. Benannter Fehler statt NULL-Griff.
  if (is.null(mdl$corr_raw))
    stop("Blatt 07_Korrelation fehlt in der Mappe — die Zielkorrelation ist nicht ableitbar.")

  # ---- Treiberreihenfolge = build_dists (Rand-Verteilungen wie Phase 4) -----
  # CV ist fuer die Korrelation irrelevant -> Vorgaben genuegen; es zaehlt nur die
  # Reihenfolge/Menge der Treiber (die Simulation indiziert R[drivers, drivers]).
  spec    <- build_dists(mdl)
  if (!length(spec))
    stop("Keine Treiber in Blatt 06_Verteilungen — leere Verteilungsspezifikation.")
  drivers <- names(spec)

  # ---- Zielmatrix aus Blatt 07, auf die Treiberreihenfolge ausgerichtet ------
  # Exakt die Schritte des Handoff-Ankers: Rohmatrix aus corr_raw, Zeilennamen
  # aus Spalte 1, numerisch, dann auf die Treiber ausgerichtet.
  raw <- mdl$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"

  # Abdeckungspruefung VOR dem Indizieren: jeder Treiber aus Blatt 06 muss in
  # Blatt 07 stehen (Zeile UND Spalte), sonst ein benannter Fehler statt eines
  # "subscript out of bounds". Erzwingt den Eingabevertrag fuer Phase 6.
  miss   <- setdiff(drivers, rownames(M))
  miss_c <- setdiff(drivers, colnames(M))
  if (length(miss) || length(miss_c))
    stop(sprintf("07_Korrelation: Treiber aus 06 fehlen in 07: %s (dort ergaenzen oder Korrelation 0 setzen).",
                 paste(unique(c(miss, miss_c)), collapse = ", ")))
  M <- M[drivers, drivers, drop = FALSE]

  # ---- Reparatur auf die naechste positiv-semidefinite Matrix ----------------
  # Ueber den eingefrorenen Kernel make_psd (Eigenwert-Clipping) — NICHT nearPD.
  # out$R bleibt damit bit-identisch zum direkten Engine-Lauf (Treue-Vertrag).
  ev_before <- min(eigen((M + t(M)) / 2, symmetric = TRUE, only.values = TRUE)$values)
  R         <- make_psd(M)
  method    <- "make_psd (Eigenwert-Clipping)"
  # Eingabevertrag Phase 6: Zeilen-/Spaltennamen sind genau die Treiber aus spec.
  # Fuer eine bereits PSD-Zielmatrix reicht make_psd die benannte Matrix unveraendert
  # durch; nach einer Eigenwert-Reparatur waeren die Namen verloren -> hier robust
  # (re-)setzen, ohne den Treue-Vertrag zu verletzen (Werte bleiben unangetastet).
  dimnames(R) <- list(drivers, drivers)
  ev_after    <- min(eigen((R + t(R)) / 2, symmetric = TRUE, only.values = TRUE)$values)
  max_aenderung <- max(abs(R - M))

  # ---- Diagnose (Kennzahlen des Schirms) ------------------------------------
  diagnostics <- list(
    n_treiber     = length(spec),               # == nrow(R) == length(spec)
    ziel_psd      = ev_before >= -1e-8,          # Badge: war die Zielmatrix schon PSD?
    repariert_psd = ev_after  >= -1e-8           # Badge: reparierte Matrix ist PSD (per Konstruktion TRUE)
  )

  # ---- Heatmap in den Szenario-Workspace ------------------------------------
  plot_path <- if (isTRUE(plot))
    .plot_abhaengigkeit(R, file.path(sc$workspace, "korrelation.png")) else NA_character_

  # ---- korrelation.rds fuer Phase 6 sichern (Artefakt-Vertrag) --------------
  kor_path <- file.path(sc$workspace, "korrelation.rds")
  saveRDS(R, kor_path)

  list(
    phase        = "korrelation",
    scenario     = sc$name,
    R            = R,             # reparierte Korrelationsmatrix (bit-identisch zu make_psd(M)); dimnames == Treiber
    spec         = spec,         # Rand-Verteilungen je Treiber (Reihenfolge = Eingabevertrag Phase 6)
    M            = M,            # Zielmatrix aus Blatt 07, auf die Treiber ausgerichtet (VOR Reparatur)
    method       = method,       # Reparaturmethode (eingefrorener Kernel make_psd)
    ev_before    = ev_before,    # kleinster Eigenwert der Zielmatrix
    ev_after     = ev_after,     # kleinster Eigenwert der reparierten Matrix (>= 0)
    max_aenderung = max_aenderung, # groesste Abweichung Ziel -> repariert (0 = Ziel war schon PSD)
    plot_path    = plot_path,    # PNG im Workspace ODER NA (Headless-Fallback)
    kor_path     = kor_path,     # gesicherte korrelation.rds (Eingabe Phase 6)
    diagnostics  = diagnostics
  )
}
