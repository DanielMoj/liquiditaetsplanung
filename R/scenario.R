# ============================================================================
#  R/scenario.R  —  Szenario-Abstraktion
#
#  Ein Szenario = {Mappe + Annahme-Overrides + Sim-Parameter (Pfade, Seed)}
#  mit eigenem Workspace-Verzeichnis. Die Mappe ist und bleibt der Engine-Vertrag.
#  Vergleich = N Szenarien rechnen und ihre Ergebnisobjekte gegenueberstellen.
#  Setzt voraus, dass kernel/engine.R bereits gesourct ist (load_model).
# ============================================================================

scenario_from_workbook <- function(workbook,
                                    name = "Basis",
                                    assume_overrides = list(),
                                    sim = list(paths = 5000, seed = 2026),
                                    workspace = NULL) {
  if (!file.exists(workbook)) stop(sprintf("Mappe nicht gefunden: %s", workbook))
  mdl <- load_model(workbook)

  # Annahme-Overrides auf das Modell legen (z. B. menge = -0.10 fuer ein Szenario).
  # Treiber-Whitelist HART (GoBD Rz. 100, phase-12-runmanifest-anbindung): ein
  # unbekannter Name stoppt VOR jeder Rechnung -- die fruehere warning()
  # verpuffte im mirai-Worker, und ein Tippfehler ('mengge') rechnete still
  # mit Faktor 1 weiter. Ein protokollierter Lauf mit stillschweigend
  # ignoriertem Treiber darf nicht existieren.
  if (length(assume_overrides)) {
    unknown <- setdiff(names(assume_overrides), names(mdl$assume))
    if (length(unknown))
      stop(sprintf("Unbekannte Annahme(n) in assume_overrides: %s. Gueltige Treiber (Whitelist): %s.",
                   paste(unknown, collapse = ", "),
                   paste(names(mdl$assume), collapse = ", ")), call. = FALSE)
    for (k in intersect(names(assume_overrides), names(mdl$assume)))
      mdl$assume[[k]] <- assume_overrides[[k]]
  }

  if (is.null(workspace))
    workspace <- file.path(tempdir(), paste0("scn_", gsub("[^A-Za-z0-9]+", "_", name)))
  dir.create(workspace, showWarnings = FALSE, recursive = TRUE)

  structure(list(
    name      = name,
    workbook  = workbook,
    mdl       = mdl,
    assume    = mdl$assume,
    sim       = sim,
    workspace = workspace
  ), class = "liqui_scenario")
}

# Cache-Schluessel: Hash aus Annahmen + Sim-Parametern (Basis fuer DAG-Invalidierung).
scenario_key <- function(sc) {
  payload <- list(assume = sc$assume, sim = sc$sim, wb = normalizePath(sc$workbook))
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(payload))
  paste0("k", format(abs(sum(utf8ToInt(paste(capture.output(str(payload)), collapse = "")))), scientific = FALSE))
}

print.liqui_scenario <- function(x, ...) {
  cat(sprintf("<Szenario '%s'>  Mappe: %s | HMAX=%d | Sim: %d Pfade, Seed %d\n",
              x$name, basename(x$workbook), x$mdl$HMAX, x$sim$paths, x$sim$seed))
  invisible(x)
}
