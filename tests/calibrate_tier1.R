#!/usr/bin/env Rscript
# ============================================================================
#  tests/calibrate_tier1.R  —  friert den Tier-1-Golden-Master EINMALIG ein.
#  Rechnet die Beispielklinik mit der eingefrorenen Engine und schreibt die drei
#  Kopfzahlen nach tests/tier1_baseline.R. Danach Sichtpruefung -> committen.
#  Aufruf aus der Repo-Wurzel:  Rscript tests/calibrate_tier1.R
# ============================================================================
options(warn = 1)
if (!file.exists("kernel/engine.R")) stop("Aus der Repo-Wurzel ausfuehren: Rscript tests/calibrate_tier1.R")
source("kernel/engine.R")

WB <- "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
if (!file.exists(WB)) stop(sprintf("Tier-1-Mappe fehlt: %s", WB))

mdl <- load_model(WB)
v   <- validate_history(mdl)
res <- run_model(mdl)

history      <- round(v$erg_eng)
trough       <- round(res$kpis$trough)
trough_month <- as.integer(res$kpis$trough_month)
result       <- round(res$kpis$result)

cat("=== BERECHNET (aus der eingefrorenen Engine) ===\n")
cat(sprintf("  Historie (erg_eng)   : %s EUR\n", fmt(history)))
cat(sprintf("  Tiefpunkt            : %s EUR  (M%d)\n", fmt(trough), trough_month))
cat(sprintf("  Planergebnis         : %s EUR\n", fmt(result)))
cat("\n=== REFERENZ aus deiner Validierung (nur zum Abgleich) ===\n")
cat("  Historie -5.235.600 | Tiefpunkt -5.135.990 (M3) | Planergebnis 2.672.411\n\n")

out <- c(
  "# tests/tier1_baseline.R  —  Tier-1 Golden Master (eingefroren).",
  "# Erzeugt von calibrate_tier1.R nach Sichtpruefung. Ab dann unveraendert committed.",
  "TIER1_BASELINE <- list(",
  sprintf("  history      = %d,", history),
  sprintf("  trough       = %d,", trough),
  sprintf("  trough_month = %dL,", trough_month),
  sprintf("  result       = %d", result),
  ")"
)
writeLines(out, "tests/tier1_baseline.R")
cat("[geschrieben] tests/tier1_baseline.R\n\n")
cat("NAECHSTER SCHRITT: Stimmen die berechneten Zahlen mit der Referenz ueberein?\n")
cat("  JA   -> git add tests/tier1_baseline.R && committen. Ab jetzt ist es der Anker.\n")
cat("  NEIN -> NICHT committen. Erst fixtures/ und kernel/ pruefen.\n")
