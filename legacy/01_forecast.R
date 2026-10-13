# ============================================================================
#  01_forecast.R  —  Phase 1: Validierung der Historie + deterministischer Forecast
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/01_forecast.R [pfad/zur/mappe.xlsx]
# ============================================================================
source("kernel/engine.R")
args <- commandArgs(TRUE)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)

v <- validate_history(mdl)
cat("=== VALIDIERUNG (Ist-Historie, Nullannahmen) ===\n")
cat(sprintf("Jahresergebnis Historie (Engine)     : %s EUR\n", fmt(v$erg_eng)))
cat(sprintf("Gegenprobe (Summe Erloese - Aufwand) : %s EUR\n", fmt(v$erg_naiv)))
cat(sprintf("GuV-Konsistenz                       : %s\n", ifelse(v$ok, "OK", "ABWEICHUNG")))
cat(sprintf("Cash-Massenerhaltung (je Gruppe)     : %s\n", ifelse(v$mass_ok, "OK", "VERLETZT")))
if (!v$mass_ok) cat(sprintf("  Gruppen ueber Toleranz: %s\n", paste(names(v$mass_res)[v$mass_res >= 0.02], collapse=", ")))
cat("Geprueft: GuV-Identitaet + Cash-Massenerhaltung. NICHT geprueft: Zahlungs-TIMING (Lage des\n")
cat("Tiefpunkts) -- ein Lag-Tausch erhaelt die Masse. DSO/DPO unten zum Abgleich gegen die Erfahrung.\n")
dd <- implied_dpo_dso(mdl, c("umsatz","betr_ertrag","material","sachkosten"))
cat(sprintf("Implizit aus cashprofile: DSO Umsatz %.0f T | DSO sonst.Ertrag %.0f T | DPO Material %.0f T | DPO Sachkosten %.0f T\n\n",
            dd["umsatz"], dd["betr_ertrag"], dd["material"], dd["sachkosten"]))

if (!v$ok) stop("GuV-Konsistenz verletzt (erg_eng != erg_naiv) - Forecast abgebrochen. Mapping/Vorzeichen pruefen.")
if (!v$mass_ok) stop("Cash-Massenerhaltung verletzt - Liquiditaetspfad unzuverlaessig. cashprofile/Profilsummen pruefen.")
res <- run_model(mdl)
MN <- paste0("M", 1:mdl$HMAX)
guv <- res$guv; cash <- res$cash; colnames(guv) <- MN; colnames(cash) <- MN
gm <- guv; gm[] <- fmt(guv); cm <- cash; cm[] <- fmt(cash)
cat(sprintf("=== FORECAST (%d Monate)  GuV / BWA  [EUR] ===\n", mdl$HMAX)); print(gm, quote=FALSE, right=TRUE)
cat("\n=== FORECAST  Liquiditaet  [EUR] ===\n"); print(cm, quote=FALSE, right=TRUE)
k <- res$kpis
cat(sprintf("\nErgebnis Planungszeitraum : %s EUR\n", fmt(k$result)))
cat(sprintf("Liquiditaets-Tiefpunkt    : %s EUR  (%s)\n", fmt(k$trough), MN[k$trough_month]))
cat(sprintf("Max. KK-Inanspruchnahme   : %s EUR  (Limit %s)\n", fmt(k$maxKK), fmt(mdl$kk_limit)))
cat(sprintf("KK-Zins im Zeitraum       : %s EUR  (auf Monats-Anfangsbestand; intramonatliche Lage nicht erfasst --\n", fmt(sum(res$cash["KK_Zins", ]))))
cat("                            woechentliche Sicht (01b) faellt wegen Zahlungszeitpunkten deutlich anders aus, F-12)\n")
solvent <- k$maxKK <= mdl$kk_limit + 1e-6
cat(sprintf("Zahlungsfaehig (KK-Linie nie ueberschritten): %s\n", ifelse(solvent, "JA", "NEIN")))
cat(sprintf("Davon getrennt: in %d von %d Monaten unter der Mindestreserve %s -- Liquiditaetsenge trotz Zahlungsfaehigkeit.\n",
            k$months_below, mdl$HMAX, fmt(mdl$mindestreserve)))

if (!is.null(mdl$tax)) {
  et <- res$et
  cat(sprintf("\n=== STEUERN  (Rechtsform %s | GewSt %s | USt %s) ===\n",
              mdl$tax$rechtsform, mdl$tax$gewst_status, mdl$tax$ust_status))
  cat(sprintf("KSt inkl. Soli p.a.        : %s EUR\n", fmt(et$kst_a)))
  cat(sprintf("GewSt p.a.                 : %s EUR\n", fmt(et$gewst_a)))
  if (identical(mdl$tax$ust_status, "pflichtig"))
    cat(sprintf("USt-Abfuehrung im Zeitraum : %s EUR\n", fmt(-sum(res$cash["USt_Zahllast", ]))))
  if (mdl$tax$rechtsform != "kapital" && mdl$tax$entnahme_quote)
    cat(sprintf("Steuerentnahme im Zeitraum : %s EUR\n", fmt(-sum(res$cash["Steuerentnahme", ]))))
}

if (!is.null(res$darlehen)) {
  d <- res$darlehen
  cat(sprintf("\n=== DARLEHEN  (%d Kredit(e) aus 09_Darlehen) ===\n", nrow(mdl$loans)))
  cat(sprintf("Auszahlung im Zeitraum  : %s EUR\n", fmt(sum(d$auszahlung))))
  cat(sprintf("Zinsaufwand im Zeitraum : %s EUR  (in GuV und Liquiditaet)\n", fmt(sum(d$zins))))
  cat(sprintf("Tilgung im Zeitraum     : %s EUR\n", fmt(sum(d$tilgung))))
}

if (!is.null(res$opos)) {
  o <- res$opos
  cat(sprintf("\n=== OFFENE POSTEN  (aus 10_OPOS; ersetzt den Carry-in im Nahbereich) ===\n"))
  cat(sprintf("Forderungen     : %s EUR aus %d Posten, Eingang im Zeitraum %s EUR\n",
              fmt(o$meta$sum_fo), o$meta$n_fo, fmt(sum(o$forderungen))))
  cat(sprintf("Verbindlichkeiten: %s EUR aus %d Posten, Zahlung im Zeitraum %s EUR\n",
              fmt(o$meta$sum_ve), o$meta$n_ve, fmt(sum(o$verbindlichkeiten))))
  if (o$meta$beyond_fo > 0 || o$meta$beyond_ve > 0)                                  # F-17
    cat(sprintf("Faellig NACH dem %d-Monats-Horizont (nicht im Cashflow): Forderungen %s EUR, Verbindlichkeiten %s EUR\n",
                mdl$HMAX, fmt(o$meta$beyond_fo), fmt(o$meta$beyond_ve)))
}
