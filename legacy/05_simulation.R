# ============================================================================
#  05_simulation.R  —  Phase 5: genestete Monte-Carlo
#
#  Aeussere Schicht (epistemisch): korrelierte Jahreswerte je Treiber (Phase 3+4).
#  Innere Schicht (aleatorisch):   monatliches multiplikatives Rauschen, je Treiber
#                                  ein gemeinsamer Monatsschock (Menge, Material).
#  Jeder Lauf laeuft durch dieselbe Engine. Produkt: Verteilung des Endbestands
#  je Monat und die Schwellenwahrscheinlichkeiten.
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/05_simulation.R [pfad/zur/mappe.xlsx]
#  Voraussetzung: 03 (out/verteilungen_spec.rds) und 04 (out/korrelation.rds) gelaufen.
# ============================================================================
source("kernel/engine.R"); source("kernel/distributions.R")
args <- commandArgs(TRUE); dir.create("out", showWarnings = FALSE)   # Ausgaben nach out/ (gitignored)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)
stopifnot(file.exists("out/verteilungen_spec.rds"), file.exists("out/korrelation.rds"))
spec <- readRDS("out/verteilungen_spec.rds")
R    <- readRDS("out/korrelation.rds")
drivers <- names(spec); H <- mdl$HMAX
R <- R[drivers, drivers, drop = FALSE]

S <- 5000
set.seed(2026)

# ---- aeussere Schicht: korrelierte epistemische Jahreswerte (S x k) ----
Theta <- draw_epistemic_correlated(spec, R, S)

# ---- innere Schicht: monatliches Rauschen, je Treiber ein gemeinsamer Schock ----
noise_driver <- ifelse(mdl$pos$driver %in% c("menge_preis","menge"), "menge",
                ifelse(mdl$pos$driver == "material", "material", NA_character_))
sig <- c(menge    = if (!is.null(spec$menge))    spec$menge$cv    else 0,
         material = if (!is.null(spec$material)) spec$material$cv else 0)
make_noise <- function() {
  N <- matrix(1, nrow(mdl$pos), H)
  sh <- list(menge = rnorm(H, 0, sig["menge"]), material = rnorm(H, 0, sig["material"]))
  for (i in seq_len(nrow(mdl$pos))) { nd <- noise_driver[i]; if (!is.na(nd)) N[i, ] <- exp(sh[[nd]]) }
  N
}

# ---- Simulation ----
B <- matrix(0, S, H, dimnames = list(NULL, paste0("M", 1:H)))
for (s in 1:S) {
  a <- mdl$assume; for (d in drivers) a[[d]] <- Theta[s, d]
  B[s, ] <- run_model(mdl, a, make_noise())$cash["Endbestand", ]
}

# ---- Metriken ----
fmt <- function(x) format(round(x), big.mark=".", decimal.mark=",", scientific=FALSE)
qs  <- c(.05,.25,.5,.75,.95); fan <- apply(B, 2, quantile, probs = qs)
P_under  <- colMeans(B < mdl$mindestreserve)
trough   <- apply(B, 1, min)
P_breach <- mean(trough < -mdl$kk_limit)

cat(sprintf("=== SIMULATION  (%d Laeufe, Horizont %d Monate) ===\n", S, H))
print(data.frame(Monat = paste0("M",1:H),
                 P05 = fmt(fan["5%",]), P50 = fmt(fan["50%",]), P95 = fmt(fan["95%",]),
                 P_unter_Reserve = sprintf("%.0f%%", 100*P_under)), row.names = FALSE)
cat(sprintf("\nTiefpunkt  Median : %s EUR\n", fmt(median(trough))))
cat(sprintf("Tiefpunkt  P05    : %s EUR\n", fmt(quantile(trough, .05))))
cat(sprintf("Tiefpunkt  P95    : %s EUR\n", fmt(quantile(trough, .95))))
cat(sprintf("P(KK-Linie reisst, Limit %s) : %.1f%%\n", fmt(mdl$kk_limit), 100*P_breach))
cat(sprintf("P(zahlungsfaehig ueber %d Monate)   : %.1f%%   (§§ 18/19 InsO: > 50%% gefordert)\n", H, 100*(1-P_breach)))

# ---- Fan-Chart + Unterdeckungswahrscheinlichkeit ----
tryCatch({
  png("out/simulation_fanchart.png", width = 1100, height = 780); par(mfrow = c(2,1), mar = c(4,5,3,2))
  x <- 1:H; ylim <- range(fan, mdl$mindestreserve, -mdl$kk_limit)/1e6
  plot(x, fan["50%",]/1e6, type="n", ylim=ylim, xlab="", ylab="Endbestand [Mio. EUR]",
       main="Liquiditaet — Verteilung ueber die Simulation", xaxt="n"); axis(1, x, paste0("M",x))
  polygon(c(x,rev(x)), c(fan["5%",],rev(fan["95%",]))/1e6, col="#D9E1F2", border=NA)
  polygon(c(x,rev(x)), c(fan["25%",],rev(fan["75%",]))/1e6, col="#7F9AC0", border=NA)
  lines(x, fan["50%",]/1e6, col="#1F3864", lwd=2)
  abline(h=0, col="grey50"); abline(h=mdl$mindestreserve/1e6, col="#C00000", lty=2)
  abline(h=-mdl$kk_limit/1e6, col="#C00000", lwd=2)
  legend("topright", c("P5-P95","P25-P75","Median","Mindestreserve","KK-Limit"),
         fill=c("#D9E1F2","#7F9AC0",NA,NA,NA), border=NA,
         lty=c(NA,NA,1,2,1), lwd=c(NA,NA,2,1,2), col=c(NA,NA,"#1F3864","#C00000","#C00000"), bty="n", cex=0.85)
  barplot(100*P_under, names.arg=paste0("M",x), col="#1F3864", ylab="P(unter Reserve) [%]",
          main="Wahrscheinlichkeit unter Mindestreserve je Monat", ylim=c(0,100))
  dev.off(); cat("\n[Plot] out/simulation_fanchart.png geschrieben\n")
}, error=function(e) cat(sprintf("\n[Plot] uebersprungen (%s)\n", conditionMessage(e))))

saveRDS(B, "out/simulation_endbestand.rds")
cat("[Save] out/simulation_endbestand.rds\n")
