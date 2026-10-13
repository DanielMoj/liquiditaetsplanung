#!/usr/bin/env Rscript
# ============================================================================
#  tests/deps.R  —  Test-Abhaengigkeiten (einmalige Umgebungsvorbereitung)
#
#  Installiert die Pakete, die der DoD-Lauf (Rscript tests/run_all.R) braucht,
#  sofern sie fehlen. shinytest2 + chromote fahren das verbindliche Gate fuer
#  den Sensitivitaets-Schirm headless gegen die Engine-Werte; ohne sie FAELLT
#  das Gate (kein stiller Skip — siehe tests/test_phase_sens.R). mirai + promises
#  treiben den Hintergrund-Worker der Simulation (Phase 6): die genestete Monte-
#  Carlo laeuft nie synchron im UI-Thread (CLAUDE.md §Nebenlaeufigkeit).
#  officer + quarto tragen den Export (Phase 9): officer den docx-Strang,
#  quarto den pdf-Strang (braucht zusaetzlich das Quarto-CLI, siehe unten);
#  pdftools liest die pdf-Exporte fuer die Ruecklese-Orakel zurueck
#  (tests/test_export_pdf.R). digest traegt die SHA-256-Manifeste der
#  GoBD-Schicht und jsonlite deren JSON-Persistierung (phase-12; jsonlite ist
#  via shiny meist praesent, der GoBD-Pfad nennt es explizit). readxl liest
#  die Mappe (kernel/engine.R).
#
#  Aufruf aus der Repo-Wurzel:  Rscript tests/deps.R
#  Browser: chromote nutzt ein vorhandenes Chrome/Chromium (chromote::find_chrome()).
# ============================================================================
need <- c("testthat", "shiny", "bslib", "readxl", "shinytest2", "chromote", "mirai", "promises", "openxlsx2", "officer", "quarto", "pdftools", "digest", "jsonlite")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  cat("Installiere fehlende Test-Abhaengigkeiten:", paste(miss, collapse = " "), "\n")
  install.packages(miss, repos = "https://cloud.r-project.org")
} else cat("Alle Test-Abhaengigkeiten vorhanden.\n")

# Browser-Vorpruefung: macht frueh sichtbar, falls das Gate spaeter mangels
# Chrome fehlschlagen wuerde.
chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
if (is.null(chrome) || !nzchar(chrome)) {
  cat("[Warnung] Kein Chrome/Chromium gefunden — das shinytest2-Gate wird fehlschlagen,\n",
      "          bis ein Browser installiert ist.\n", sep = "")
} else {
  cat("Chrome gefunden:", chrome, "\n")
}

# Quarto-CLI-Vorpruefung: das R-Paket quarto ruft ein System-Binary auf, das
# install.packages NICHT mitbringt. Dieselbe Aufloesungskette wie
# quarto_bin_finden() in R/export.R — inkl. der Windows-Standardpfade, weil
# laufende Prozessbaeume nach einer frischen Quarto-Installation den alten
# PATH erben (beobachtet 2026-07-07: Installation vorhanden, PATH leer).
qcli <- Sys.getenv("QUARTO_PATH")
if (!nzchar(qcli)) qcli <- tryCatch(quarto::quarto_path(), error = function(e) "")
if (is.null(qcli) || !nzchar(qcli)) qcli <- Sys.which("quarto")
if (!nzchar(qcli)) {
  for (k in c("C:/Program Files/Quarto/bin/quarto.exe",
              file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/quarto.exe"))) {
    if (file.exists(k)) { qcli <- k; break }
  }
}
if (is.null(qcli) || !nzchar(qcli)) {
  cat("[Warnung] Kein Quarto-CLI gefunden — der pdf-Export-Strang (Phase 9) wird fehlschlagen,\n",
      "          bis das Quarto-CLI installiert ist (https://quarto.org/docs/get-started/).\n", sep = "")
} else {
  cat("Quarto-CLI gefunden:", qcli, "\n")
}
