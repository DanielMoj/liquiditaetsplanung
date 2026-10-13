# ============================================================================
#  04_abhaengigkeit.R  —  Phase 4: Abhaengigkeit der Treiber
#
#  Liest die Zielkorrelation (Blatt 07), repariert sie zur naechsten positiv-
#  semidefiniten Matrix (Matrix::nearPD, sonst Eigenwert-Clipping) und prueft
#  per Gauss-Copula, dass die Ziehungen die Zielkorrelation reproduzieren.
#  Ausgabe: out/korrelation.rds (fuer Phase 5) + Heatmap.
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/04_abhaengigkeit.R [pfad/zur/mappe.xlsx]
# ============================================================================
source("kernel/engine.R"); source("kernel/distributions.R")
args <- commandArgs(TRUE); dir.create("out", showWarnings = FALSE)   # Ausgaben nach out/ (gitignored)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)
if (is.null(mdl$corr_raw)) stop("Blatt 07_Korrelation fehlt in der Mappe.")

# Verteilungen (Raender) wie in Phase 3; CV hier irrelevant -> Vorgaben genuegen
spec <- if (file.exists("out/verteilungen_spec.rds")) readRDS("out/verteilungen_spec.rds") else build_dists(mdl)
drivers <- names(spec)

# ---- Zielmatrix aus Blatt 07, auf Treiberreihenfolge ausgerichtet ----
raw <- mdl$corr_raw
M <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
storage.mode(M) <- "numeric"
miss <- setdiff(drivers, rownames(M)); miss_c <- setdiff(drivers, colnames(M))
if (length(miss) || length(miss_c))
  stop(sprintf("07_Korrelation: Treiber aus 06 fehlen: %s (in 07 ergaenzen oder Korrelation 0 setzen).",
               paste(unique(c(miss, miss_c)), collapse = ", ")))
M <- M[drivers, drivers, drop = FALSE]
cat("=== ZIEL-KORRELATION (Blatt 07) ===\n"); print(round(M, 2))

# ---- Reparatur auf naechste positiv-semidefinite Matrix ----
ev_before <- min(eigen((M + t(M))/2, symmetric = TRUE, only.values = TRUE)$values)
if (requireNamespace("Matrix", quietly = TRUE)) {
  R <- as.matrix(Matrix::nearPD(M, corr = TRUE, keepDiag = TRUE)$mat); method <- "Matrix::nearPD"
} else {
  R <- make_psd(M); method <- "Eigenwert-Clipping"
}
ev_after <- min(eigen(R, symmetric = TRUE, only.values = TRUE)$values)
cat(sprintf("\nReparatur (%s): kleinster Eigenwert vorher %.4f -> nachher %.4f\n", method, ev_before, ev_after))
cat(sprintf("Maximale Aenderung gegenueber Ziel: %.4f  (0 = Ziel war bereits PSD)\n", max(abs(R - M))))

# ---- Validierung per Gauss-Copula ----
set.seed(11)
X <- draw_epistemic_correlated(spec, R, 50000)
emp_p <- cor(X)                      # Pearson der PERT-Raender
emp_s <- cor(X, method = "spearman") # Rang (copula-invariant)
cat(sprintf("\n=== VALIDIERUNG (50.000 Ziehungen) ===\n"))
cat(sprintf("Pearson  – max. Abweichung von R: %.3f  (copulabedingte Daempfung bei schiefen Raendern)\n", max(abs(emp_p[upper.tri(emp_p)] - R[upper.tri(R)]))))
cat(sprintf("Spearman – per Konstruktion erhalten (monotone Transformation):\n"))
print(round(emp_s, 2))

# ---- Heatmap ----
tryCatch({
  png("out/korrelation.png", width = 640, height = 600); par(mar = c(7,7,4,2))
  k <- length(drivers); col <- colorRampPalette(c("#C0504D","white","#1F3864"))(101)
  image(1:k, 1:k, t(R[k:1, ]), col = col, zlim = c(-1,1), axes = FALSE, xlab = "", ylab = "",
        main = "Treiber-Korrelation (repariert)")
  axis(1, 1:k, drivers, las = 2); axis(2, 1:k, rev(drivers), las = 2)
  for (i in 1:k) for (j in 1:k) text(j, k - i + 1, sprintf("%.2f", R[i, j]), cex = 0.9)
  dev.off(); cat("\n[Plot] out/korrelation.png geschrieben\n")
}, error = function(e) cat("\n[Plot] uebersprungen\n"))

dimnames(R) <- list(drivers, drivers)
saveRDS(R, "out/korrelation.rds")
cat("[Save] out/korrelation.rds\n")