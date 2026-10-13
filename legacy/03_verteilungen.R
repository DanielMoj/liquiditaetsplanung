# ============================================================================
#  03_verteilungen.R  —  Phase 3: Verteilungen der ausgewaehlten Treiber
#
#  Pro Treiber (aus Phase 2): epistemische PERT ueber die Jahresannahme +
#  aleatorische Monatsstreuung (CV), wo sinnvoll aus Ist-Residuen geschaetzt.
#  Ausgabe: Spezifikation (out/verteilungen_spec.rds) + Pruefplot, fuer Phase 5.
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/03_verteilungen.R [pfad/zur/mappe.xlsx]
# ============================================================================
source("kernel/engine.R"); source("kernel/distributions.R")
args <- commandArgs(TRUE); dir.create("out", showWarnings = FALSE)   # Ausgaben nach out/ (gitignored)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)
if (is.null(mdl$dists_raw)) stop("Blatt 06_Verteilungen fehlt in der Mappe.")

# ---- aleatorische CV aus Ist-Residuen schaetzen (nur wo sinnvoll) ----
# Basis ueber den driver waehlen -- denselben Schluessel, ueber den Phase 5 die Schocks routet.
# Damit landet m_leb (grp=material, driver=menge) in der Mengen-Basis (es schwankt mit der Menge),
# nicht in der Material-Basis. Konsistent mit noise_driver in 05_simulation.R.
D <- mdl$pos$driver
vol_series <- colSums(mdl$base[D %in% c("menge","menge_preis"), , drop = FALSE])
mat_series <- colSums(mdl$base[D == "material",                , drop = FALSE])
est <- list(menge    = estimate_cv_harmonic(vol_series),
            material = estimate_cv_harmonic(mat_series))

spec <- build_dists(mdl, est)
if (!length(spec)) stop("Keine Treiber in 06_Verteilungen.")

# ---- Zusammenfassung ----
cat("=== AUFTEILUNG DER UNSICHERHEIT JE TREIBER ===\n")
cat(sprintf("Historie: %d Monate  (>= 24 fuer belastbare aleatorische Schaetzung empfohlen)\n\n", ncol(mdl$base)))
pct <- function(x) sprintf("%+.1f%%", 100 * x)
rows <- lapply(names(spec), function(d) {
  s <- spec[[d]]; q <- qpert(c(.05,.5,.95), s$min, s$mode, s$max)
  data.frame(Treiber=d, Mode=pct(s$mode), P5=pct(q[1]), P50=pct(q[2]), P95=pct(q[3]),
             CV_aleat=sprintf("%.1f%%", 100*ifelse(is.na(s$cv),0,s$cv)), Quelle=s$cv_source)
})
print(do.call(rbind, rows), row.names = FALSE)
cat("\nP5..P95 = epistemische Bandbreite der Jahresannahme (PERT).\n")
cat("CV_aleat = irreduzible monatliche Streuung um den Pfad (multiplikativ).\n")
cat("In Phase 5: pro Lauf 1x epistemisch ziehen, dann monatlich aleatorisch ueberlagern (genestet).\n")

# ---- Pruefplot ----
tryCatch({
  png("out/verteilungen.png", width = 1150, height = 640); par(mfrow = c(2,3), mar = c(4,3,3,1))
  for (d in names(spec)) {
    s <- spec[[d]]; x <- rpert(20000, s$min, s$mode, s$max)
    hist(100*x, breaks=40, col="#1F3864", border="white", freq=FALSE,
         main=paste0(d, "  (epistemisch)"), xlab="Jahresrate [%]")
    abline(v=100*s$mode, col="#C00000", lwd=2)
  }
  t <- seq_along(vol_series); X <- cbind(1, sin(2*pi*t/12), cos(2*pi*t/12), sin(4*pi*t/12), cos(4*pi*t/12))
  res <- lm.fit(X, log(vol_series))$residuals
  hist(100*res, breaks=8, col="#7F9AC0", border="white", freq=FALSE,
       main="Menge: Ist-Residuen (aleatorisch)", xlab="Abweichung vom Saisonpfad [%]")
  dev.off(); cat("\n[Plot] out/verteilungen.png geschrieben\n")
}, error = function(e) cat("\n[Plot] uebersprungen (kein Grafikgeraet)\n"))

# ---- Spezifikation fuer Phase 5 sichern ----
saveRDS(spec, "out/verteilungen_spec.rds")
cat("[Save] out/verteilungen_spec.rds\n")
