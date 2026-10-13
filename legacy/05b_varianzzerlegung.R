# ============================================================================
#  05b_varianzzerlegung.R  —  Add-on zu Phase 5
#
#  Zerlegt die Streuung des Liquiditaets-Tiefpunkts T = min_m B_m in ihre Quellen.
#  Drei Laeufe gleicher Groesse:
#    gesamt          – Jahreswerte korreliert gezogen, Rauschen an
#    nur epistemisch – Jahreswerte gezogen, Rauschen aus
#    nur aleatorisch – Jahreswerte auf den Modes fixiert, Rauschen an
#  Varianzzerlegungssatz: Var(T) = Var(E[T|theta]) + E[Var(T|theta)].
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/05b_varianzzerlegung.R [pfad/zur/mappe.xlsx]
#  Voraussetzung: 03 + 04 gelaufen (out/verteilungen_spec.rds, out/korrelation.rds).
# ============================================================================
source("kernel/engine.R"); source("kernel/distributions.R")
args <- commandArgs(TRUE); dir.create("out", showWarnings = FALSE)   # Ausgaben nach out/ (gitignored)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)
stopifnot(file.exists("out/verteilungen_spec.rds"), file.exists("out/korrelation.rds"))
spec <- readRDS("out/verteilungen_spec.rds"); R <- readRDS("out/korrelation.rds")
drivers <- names(spec); H <- mdl$HMAX; R <- R[drivers, drivers, drop = FALSE]
S <- 4000

noise_driver <- ifelse(mdl$pos$driver %in% c("menge_preis","menge"), "menge",
                ifelse(mdl$pos$driver == "material", "material", NA_character_))
sig <- c(menge    = if (!is.null(spec$menge))    spec$menge$cv    else 0,
         material = if (!is.null(spec$material)) spec$material$cv else 0)
make_noise <- function(on = TRUE) {
  N <- matrix(1, nrow(mdl$pos), H); if (!on) return(N)
  sh <- list(menge = rnorm(H, 0, sig["menge"]), material = rnorm(H, 0, sig["material"]))
  for (i in seq_len(nrow(mdl$pos))) { nd <- noise_driver[i]; if (!is.na(nd)) N[i, ] <- exp(sh[[nd]]) }
  N
}
trough <- function(a, noise) min(run_model(mdl, a, noise)$cash["Endbestand", ])

set.seed(2026)
Theta <- draw_epistemic_correlated(spec, R, S)
T_full <- T_epi <- numeric(S)
for (s in 1:S) {
  a <- mdl$assume; for (d in drivers) a[[d]] <- Theta[s, d]
  T_full[s] <- trough(a, make_noise(TRUE))   # beide Quellen
  T_epi[s]  <- trough(a, make_noise(FALSE))  # nur epistemisch (Rauschen aus)
}
T_ale <- numeric(S)
for (s in 1:S) T_ale[s] <- trough(mdl$assume, make_noise(TRUE))  # nur aleatorisch (Modes fix)

Vf <- var(T_full); Ve <- var(T_epi); Va <- var(T_ale); inter <- Vf - Ve - Va
fmt <- function(x) format(round(x), big.mark=".", decimal.mark=",", scientific=FALSE)

cat(sprintf("=== VARIANZZERLEGUNG DES TIEFPUNKTS (%d Laeufe) ===\n\n", S))
cat(sprintf("Standardabweichung gesamt        : %s EUR\n", fmt(sqrt(Vf))))
cat(sprintf("  davon epistemisch (Annahmen)   : %s EUR\n", fmt(sqrt(max(Ve,0)))))
cat(sprintf("  davon aleatorisch (Volatilitaet): %s EUR\n\n", fmt(sqrt(max(Va,0)))))
cat("Varianzanteile:\n")
cat(sprintf("  epistemisch : %4.1f%%\n", 100*Ve/Vf))
cat(sprintf("  aleatorisch : %4.1f%%\n", 100*Va/Vf))
cat(sprintf("  Interaktion : %4.1f%%  (Rest; klein bei naeherungsweiser Unabhaengigkeit)\n", 100*inter/Vf))

tryCatch({
  png("out/varianzzerlegung.png", width = 1100, height = 760); par(mfrow = c(2,1), mar = c(4,5,3,2))
  dF <- density(T_full/1e6); dE <- density(T_epi/1e6); dA <- density(T_ale/1e6)
  plot(dF, xlim=range(dF$x,dE$x,dA$x), ylim=c(0, max(dF$y,dE$y,dA$y)),
       main="Tiefpunkt — Verteilung nach Unsicherheitsquelle", xlab="Tiefpunkt [Mio. EUR]", lwd=2, col="#1F3864")
  polygon(dF, col="#D9E1F2", border="#1F3864", lwd=2)
  lines(dE, col="#C00000", lwd=2); lines(dA, col="#548235", lwd=2)
  legend("topleft", c("gesamt","nur epistemisch","nur aleatorisch"),
         col=c("#1F3864","#C00000","#548235"), lwd=2, bty="n")
  sh <- c(epistemisch=Ve/Vf, aleatorisch=Va/Vf, Interaktion=inter/Vf)
  barplot(as.matrix(100*sh), horiz=TRUE, col=c("#C00000","#548235","#BFBFBF"),
          xlab="Varianzanteil [%]", main="Anteile an der Tiefpunkt-Varianz", xlim=c(0,100))
  legend("topright", names(sh), fill=c("#C00000","#548235","#BFBFBF"), bty="n", horiz=TRUE)
  dev.off(); cat("\n[Plot] out/varianzzerlegung.png geschrieben\n")
}, error = function(e) cat(sprintf("\n[Plot] uebersprungen (%s)\n", conditionMessage(e))))
