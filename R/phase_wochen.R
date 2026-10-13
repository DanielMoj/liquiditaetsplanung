# ============================================================================
#  R/phase_wochen.R  —  Phase 2 (Wochenliquiditaet) als Funktion
#
#  Ruft die EINGEFRORENE Engine (kernel/) auf — run_model_weekly leitet die
#  woechentliche direkte Liquiditaet aus dem Monatslauf ab (Σ je Monat erhalten).
#  Gibt ein Ergebnisobjekt zurueck (Zahlen + Tabelle + Plot-Pfad + Diagnose),
#  statt in die Konsole zu drucken. Rechenlogik unveraendert aus legacy/01b_wochen.R;
#  hier wird das cat(...) durch Listeneintraege ersetzt.
#  Setzt voraus, dass kernel/engine.R und R/scenario.R bereits gesourct sind.
# ============================================================================

# Endbestand-Verlauf je Woche als PNG (Vorlage: legacy/01b_wochen.R). Gibt den
# Pfad zurueck, oder NA, wenn kein Grafikgeraet verfuegbar ist (headless ohne png).
.plot_wochen <- function(wk, mdl, path) {
  tryCatch({
    n <- wk$n_weeks; e <- wk$cash["Endbestand", ] / 1e6; k <- wk$kpis
    grDevices::png(path, width = 1150, height = 470)
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mar = c(5, 5, 3, 2)); on.exit(graphics::par(op), add = TRUE)
    x <- seq_len(n)
    plot(x, e, type = "l", lwd = 2, col = "#1F3864", xaxt = "n", xlab = "",
         ylab = "Endbestand [Mio. EUR]", main = "Wochenliquiditaet — Endbestand je Woche",
         ylim = range(e, mdl$mindestreserve / 1e6, -mdl$kk_limit / 1e6))
    graphics::axis(1, x, paste0("W", x), cex.axis = 0.7, las = 2)
    graphics::abline(h = 0, col = "grey60")
    graphics::abline(h = mdl$mindestreserve / 1e6, col = "#C00000", lty = 2)
    graphics::abline(h = -mdl$kk_limit / 1e6, col = "#C00000", lwd = 2)
    graphics::points(k$trough_week, e[k$trough_week], pch = 19, col = "#C00000")
    graphics::legend("topleft", c("Endbestand", "Mindestreserve", "KK-Limit", "Tiefpunkt"),
                     col = c("#1F3864", "#C00000", "#C00000", "#C00000"),
                     lty = c(1, 2, 1, NA), pch = c(NA, NA, NA, 19), lwd = c(2, 1, 2, NA),
                     bty = "n", cex = 0.85)
    path
  }, error = function(e) NA_character_)
}

phase_wochen <- function(sc, plot = TRUE) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  mo <- run_model(mdl)           # Monatssicht (Vergleich + Rueckabstimmungs-Anker)
  wk <- run_model_weekly(mdl)    # Wochensicht (voller Horizont)

  n    <- wk$n_weeks
  cash <- wk$cash                # Zeilen x n_weeks, Spalten W1..Wn
  end  <- cash["Endbestand", ]

  # Rueckabstimmung Σ NettoCashflow  Monat = Woche — der Engine-Vertrag: die
  # Monatsbetraege werden nur kalendarisiert, die Summe bleibt erhalten.
  netcf_mo   <- sum(mo$cash["NettoCashflow", ])
  netcf_wk   <- wk$kpis$netcf_total
  recon_diff <- netcf_mo - netcf_wk
  recon_ok   <- abs(recon_diff) <= 1e-6 * max(1, abs(netcf_mo))

  # KK-Zins haengt am Bezugsraster (F-12): die Wochensicht ist die belastbarere
  # Schaetzung, die Monatssicht UNTERSCHAETZT sie, wenn grosse Aus-/Einzahlungen
  # intramonatlich auseinanderfallen. Diese Grenze offen ausweisen (IMPLEMENTATION_RULES).
  kz_mo <- sum(mo$cash["KK_Zins", ]); kz_wk <- sum(cash["KK_Zins", ])

  # Wochentabelle fuer die UI (Rohwerte; die Oberflaeche formatiert ueber fmt()).
  table <- data.frame(
    Woche         = paste0("W", seq_len(n)),
    ab            = format(wk$wk_start, "%d.%m.%Y"),
    NettoCashflow = unname(cash["NettoCashflow", ]),
    Endbestand    = unname(end),
    KK            = unname(cash["KK_Inanspruchnahme", ]),
    unter_Reserve = unname(end < mdl$mindestreserve),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  plot_path <- if (isTRUE(plot))
    .plot_wochen(wk, mdl, file.path(sc$workspace, "wochenliquiditaet.png")) else NA_character_

  list(
    phase     = "wochen",
    scenario  = sc$name,
    cash      = cash,            # Wochen-Matrix
    kpis      = wk$kpis,         # trough, trough_week, maxKK, weeks_below, end, netcf_total
    n_weeks   = n,
    wk_start  = wk$wk_start,
    wk_end    = wk$wk_end,
    table     = table,
    plot_path = plot_path,
    monatssicht = list(
      trough        = mo$kpis$trough,
      trough_month  = mo$kpis$trough_month,
      netcf_total   = netcf_mo,
      kk_zins_total = kz_mo
    ),
    rueckabstimmung = list(netcf_monat = netcf_mo, netcf_woche = netcf_wk,
                           diff = recon_diff, ok = recon_ok),
    kk_zins = list(monat = kz_mo, woche = kz_wk,
                   faktor = if (kz_mo > 1) kz_wk / kz_mo else NA_real_),
    diagnostics = list(
      rueckabstimmung_ok   = isTRUE(recon_ok),
      tiefpunkt_woche      = wk$kpis$trough_week,
      wochen_unter_reserve = wk$kpis$weeks_below,
      kk_innerhalb_limit   = wk$kpis$maxKK <= mdl$kk_limit + 1e-6
    )
  )
}
