#!/usr/bin/env Rscript
# ============================================================================
#  tests/run_all.R  —  Definition of Done (DOD)
#    (1) Tier-1-Golden-Master: Engine-Ausgabe auf der Beispielklinik gegen den
#        EINGEFRORENEN Baseline (tests/tier1_baseline.R, aus calibrate_tier1.R).
#    (2) testthat-Suite.
#  Exit != 0 bei Drift, rotem Test, oder wenn der Baseline noch fehlt.
#  Aufruf aus der Repo-Wurzel:  Rscript tests/run_all.R
# ============================================================================
options(warn = 1)
if (!file.exists("kernel/engine.R")) stop("Aus der Repo-Wurzel ausfuehren: Rscript tests/run_all.R")
source("kernel/engine.R")

WB <- "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
BL <- "tests/tier1_baseline.R"

if (!file.exists(BL))
  stop(paste0("Tier-1-Baseline fehlt (", BL, ").\n",
              "  Einmalig kalibrieren und nach Sichtpruefung committen:\n",
              "    Rscript tests/calibrate_tier1.R"))
if (!file.exists(WB)) stop(sprintf("Tier-1-Mappe fehlt: %s", WB))

source(BL)  # definiert TIER1_BASELINE = list(history, trough, trough_month, result)

mdl <- load_model(WB)
v   <- validate_history(mdl)
res <- run_model(mdl)

chk <- function(label, g, w) {
  ok <- isTRUE(g == w)
  cat(sprintf("  [%-5s] %-22s ist %s  (Baseline %s)\n", ifelse(ok,"OK","DRIFT"), label, fmt(g), fmt(w)))
  ok
}
cat("=== TIER-1-GOLDEN-MASTER (Beispielklinik gegen eingefrorenen Baseline) ===\n")
a1 <- chk("Historie",        round(v$erg_eng),          TIER1_BASELINE$history)
a2 <- chk("Tiefpunkt",       round(res$kpis$trough),    TIER1_BASELINE$trough)
a3 <- chk("Tiefpunkt-Monat", res$kpis$trough_month,     TIER1_BASELINE$trough_month)
a4 <- chk("Planergebnis",    round(res$kpis$result),    TIER1_BASELINE$result)
g1 <- isTRUE(v$ok) && isTRUE(v$mass_ok)
cat(sprintf("  [%-5s] %-22s\n", ifelse(g1,"OK","FEHLER"), "GuV-Konsistenz + Masse"))

if (!all(a1,a2,a3,a4,g1))
  stop("TIER-1 VERLETZT — Drift gegen den eingefrorenen Baseline. Aenderung zuruecknehmen.")
cat("Tier-1 gehalten.\n\n")

if (requireNamespace("testthat", quietly = TRUE)) {
  cat("=== testthat-Suite ===\n")
  # E2E-Controls scharf: shinytest2::AppDriver$new() ruft intern skip_on_cran(),
  # und ohne NOT_CRAN skippen die shinytest2-Tests STILL als "On CRAN" — so
  # rutschte der nicht lauffaehige Verteilungs-Schirm durch ein gruenes Gate.
  Sys.setenv(NOT_CRAN = "true")
  erg <- as.data.frame(testthat::test_dir("tests", stop_on_failure = TRUE, reporter = "summary"))
  # Zaehlung je test_that-Block. Dateien ohne test_that (die Speicher-Naehte
  # tests/test_*_gc_seam.R) listet testthat oben als "empty test" unter Skipped;
  # in der Ergebnisliste fehlen sie und werden darum getrennt ausgewiesen.
  n_dateien <- length(list.files("tests", pattern = "^test.*\\.[rR]$"))
  cat(sprintf("\nZaehlung: %d Testdateien, davon %d ohne test_that; %d Tests: %d bestanden, %d uebersprungen, %d fehlgeschlagen (%d Erwartungen erfuellt, %d Warnungen).\n",
              n_dateien, n_dateien - length(unique(erg$file)), nrow(erg),
              sum(!erg$skipped & erg$failed == 0 & !erg$error), sum(erg$skipped),
              sum(erg$failed > 0 | erg$error), sum(erg$passed), sum(erg$warning)))
  cat("\nAlle Tests gruen.\n")
} else cat("[Hinweis] 'testthat' nicht installiert — Suite uebersprungen.\n")
cat("\nDOD gruen.\n")
