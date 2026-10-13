# ============================================================================
#  01b_wochen.R  —  Wochenliquiditaet (Ableitung aus dem Monatslauf)
#  Woechentliche direkte Liquiditaet ueber den vollen Horizont: Monatsbetraege
#  kalendarisiert (glatt; Personal als Lohn-Lump Monatsende; Steuern auf die
#  gesetzlichen Tage; OPOS exakt datiert), KK-Zins wochengenau. Σ je Monat = Monatslauf.
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/01b_wochen.R [pfad/zur/mappe.xlsx]
# ============================================================================
source("kernel/engine.R")
args <- commandArgs(TRUE); dir.create("out", showWarnings = FALSE)   # Ausgaben nach out/ (gitignored)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)

mo <- run_model(mdl); wk <- run_model_weekly(mdl)
H <- mdl$HMAX; n <- wk$n_weeks

cat(sprintf("=== WOCHENLIQUIDITAET  (%d Wochen, Horizont %d Monate) ===\n", n, H))
cat(sprintf("Rueckabstimmung Σ NettoCashflow  Monat = Woche : %s = %s  (Diff %s)\n\n",
            fmt(sum(mo$cash["NettoCashflow",])), fmt(wk$kpis$netcf_total),
            fmt(sum(mo$cash["NettoCashflow",]) - wk$kpis$netcf_total)))

lab <- format(wk$wk_start, "%d.%m")
df <- data.frame(Woche = paste0("W", 1:n), ab = lab,
                 NettoCashflow = fmt(wk$cash["NettoCashflow",]),
                 Endbestand = fmt(wk$cash["Endbestand",]),
                 KK = fmt(wk$cash["KK_Inanspruchnahme",]),
                 unter_Reserve = ifelse(wk$cash["Endbestand",] < mdl$mindestreserve, "ja", "-"),
                 check.names = FALSE)
print(df, row.names = FALSE, right = TRUE)

k <- wk$kpis
cat(sprintf("\nTiefpunkt woechentlich   : %s EUR  (W%d, ab %s)\n", fmt(k$trough), k$trough_week, format(wk$wk_start[k$trough_week], "%d.%m.%Y")))
cat(sprintf("Vergleich Monatssicht    : %s EUR  (M%d)\n", fmt(mo$kpis$trough), mo$kpis$trough_month))
cat(sprintf("Max. KK-Inanspruchnahme  : %s EUR  (Limit %s)\n", fmt(k$maxKK), fmt(mdl$kk_limit)))
cat(sprintf("Wochen unter Mindestreserve (%s): %d von %d\n", fmt(mdl$mindestreserve), k$weeks_below, n))

# F-12: KK-Zins haengt am Bezugsraster. Monatslauf rechnet auf dem Monats-Anfangsbestand,
# Wochenlauf auf dem Wochenbestand -- die intramonatlichen Zahlungszeitpunkte schlagen durch.
kz_mo <- sum(mo$cash["KK_Zins", ]); kz_wk <- sum(wk$cash["KK_Zins", ])
cat(sprintf("\nKK-Zins Σ Monatssicht    : %s EUR\n", fmt(kz_mo)))
cat(sprintf("KK-Zins Σ Wochensicht    : %s EUR%s\n", fmt(kz_wk),
            if (kz_mo > 1) sprintf("  (Faktor %.1fx zur Monatssicht)", kz_wk/kz_mo) else ""))
cat("Die Wochensicht ist die belastbarere KK-Zins-Schaetzung; die Monatssicht unterschaetzt sie,\n")
cat("wenn grosse Aus- und Einzahlungen innerhalb des Monats auseinanderfallen.\n")

tryCatch({
  png("out/wochenliquiditaet.png", width = 1150, height = 470); par(mar = c(5,5,3,2))
  x <- 1:n; e <- wk$cash["Endbestand",]/1e6
  plot(x, e, type = "l", lwd = 2, col = "#1F3864", xaxt = "n", xlab = "", ylab = "Endbestand [Mio. EUR]",
       main = "Wochenliquiditaet — Endbestand je Woche", ylim = range(e, mdl$mindestreserve/1e6, -mdl$kk_limit/1e6))
  axis(1, x, paste0("W", x), cex.axis = 0.7, las = 2)
  abline(h = 0, col = "grey60"); abline(h = mdl$mindestreserve/1e6, col = "#C00000", lty = 2); abline(h = -mdl$kk_limit/1e6, col = "#C00000", lwd = 2)
  points(k$trough_week, e[k$trough_week], pch = 19, col = "#C00000")
  legend("topleft", c("Endbestand","Mindestreserve","KK-Limit","Tiefpunkt"),
         col = c("#1F3864","#C00000","#C00000","#C00000"), lty = c(1,2,1,NA), pch = c(NA,NA,NA,19), lwd = c(2,1,2,NA), bty = "n", cex = 0.85)
  dev.off(); cat("\n[Plot] out/wochenliquiditaet.png geschrieben\n")
}, error = function(e) cat("\n[Plot] uebersprungen (kein Grafikgeraet)\n"))
