# ============================================================================
#  R/phase_forecast.R  —  Phase 1 (Forecast) als Funktion
#
#  Ruft die EINGEFRORENE Engine (kernel/) auf und gibt ein Ergebnisobjekt zurueck
#  (Zahlen + Diagnose), statt in die Konsole zu drucken. Rechenlogik unveraendert
#  aus 01_forecast.R / engine.R — hier wird nur das cat(...) durch Listeneintraege
#  ersetzt. Vorlage: legacy/01_forecast.R.
# ============================================================================

phase_forecast <- function(sc) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  v   <- validate_history(mdl)
  dd  <- implied_dpo_dso(mdl, c("umsatz", "betr_ertrag", "material", "sachkosten"))
  res <- run_model(mdl)

  MN <- paste0("M", seq_len(mdl$HMAX))
  guv  <- res$guv;  colnames(guv)  <- MN
  cash <- res$cash; colnames(cash) <- MN

  solvent <- res$kpis$maxKK <= mdl$kk_limit + 1e-6

  list(
    phase      = "forecast",
    scenario   = sc$name,
    validation = v,            # erg_eng, erg_naiv, ok, mass_ok, mass_res
    dso_dpo    = dd,           # benannter Vektor (Tage)
    guv        = guv,          # Matrix [Zeilen x HMAX]
    cash       = cash,         # Matrix [Zeilen x HMAX]
    kpis       = res$kpis,     # trough, trough_month, maxKK, months_below, result, end
    solvent    = solvent,
    diagnostics = list(
      gv_konsistenz        = isTRUE(v$ok),
      massenerhaltung      = isTRUE(v$mass_ok),
      zahlungsfaehig       = isTRUE(solvent),
      monate_unter_reserve = res$kpis$months_below
    )
  )
}
