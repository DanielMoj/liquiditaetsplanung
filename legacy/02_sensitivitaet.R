# ============================================================================
#  02_sensitivitaet.R  —  Phase 2: welche Treiber bewegen den Liquiditaets-Tiefpunkt?
#  Zielgroesse: min. Endbestand ueber den Planungshorizont.
#  (1) Tornado  – jeden Treiber einzeln +/-1 Prozentpunkt auslenken
#  (2) Sobol    – varianzbasierte globale Sensitivitaet (Saltelli, base R)
#  Aufruf aus der Repo-Wurzel:  Rscript legacy/02_sensitivitaet.R [pfad/zur/mappe.xlsx]
# ============================================================================
source("kernel/engine.R")
args <- commandArgs(TRUE); dir.create("out", showWarnings = FALSE)   # Ausgaben nach out/ (gitignored)
WB <- if (length(args) >= 1) args[1] else "fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx"
mdl <- load_model(WB)

target <- function(a) run_model(mdl, a)$kpis$trough   # Liquiditaets-Tiefpunkt
drivers <- c("menge","preis","tarif","energie","material","pflegebudget","inflation")
base_trough <- target(mdl$assume)
cat(sprintf("Basis-Tiefpunkt: %s EUR  (Monat M%d)\n\n", fmt(base_trough), run_model(mdl)$kpis$trough_month))

# ---------- (1) TORNADO: +/- 1 Prozentpunkt ----------
shock <- 0.01
tor <- data.frame(driver=drivers, low=NA_real_, high=NA_real_)
for (i in seq_along(drivers)) {
  d <- drivers[i]
  aL <- mdl$assume; aL[[d]] <- aL[[d]] - shock
  aH <- mdl$assume; aH[[d]] <- aH[[d]] + shock
  tor$low[i]  <- target(aL); tor$high[i] <- target(aH)
}
tor$swing <- tor$high - tor$low
tor <- tor[order(-abs(tor$swing)), ]
cat("=== (1) TORNADO  —  Wirkung +/-1 Prozentpunkt auf den Tiefpunkt [EUR] ===\n")
print(data.frame(Treiber=tor$driver, `Tief_-1pp`=fmt(tor$low), `Tief_+1pp`=fmt(tor$high),
                 Swing=fmt(tor$swing), check.names=FALSE), row.names=FALSE)

# ---------- (2) SOBOL (Saltelli) ----------
set.seed(1)
# Bandbreiten aus Blatt 06 (einzige Quelle), nicht hartcodiert (F-18). Treiber ohne 06-Zeile
# (z.B. energie, inflation) haben keine dokumentierte epistemische Bandbreite -> nicht im Sobol.
if (is.null(mdl$dists_raw)) stop("02: Blatt 06_Verteilungen fehlt - Sobol-Bandbreiten nicht ableitbar (Tornado oben ist unberuehrt).")
dr_raw   <- as.character(mdl$dists_raw[[1]])
sdrivers <- drivers[drivers %in% dr_raw]
ranges <- setNames(lapply(sdrivers, function(d) {
  i <- match(d, dr_raw)
  c(as.numeric(mdl$dists_raw[["p_min"]][i]), as.numeric(mdl$dists_raw[["p_max"]][i]))
}), sdrivers)
k <- length(sdrivers); N <- 800
sampmat <- function(n){ M<-matrix(0,n,k,dimnames=list(NULL,sdrivers))
  for(j in 1:k){ r<-ranges[[sdrivers[j]]]; M[,j]<-runif(n,r[1],r[2]) }; M }
evalrows <- function(MAT) apply(MAT,1,function(row){ a<-mdl$assume; for(j in 1:k) a[[sdrivers[j]]]<-row[j]; target(a) })
A <- sampmat(N); B <- sampmat(N)
yA <- evalrows(A); yB <- evalrows(B); VarY <- var(c(yA,yB))
S1 <- ST <- numeric(k)
for (j in 1:k) {
  AB <- A; AB[,j] <- B[,j]; yAB <- evalrows(AB)
  S1[j] <- 1 - mean((yB - yAB)^2) / (2*VarY)   # Jansen Erstordnung
  ST[j] <- mean((yA - yAB)^2) / (2*VarY)        # Jansen Totaleffekt
}
so <- data.frame(Treiber=sdrivers, S1=round(pmin(1,pmax(0,S1)),3), ST=round(pmin(1,pmax(0,ST)),3))
so <- so[order(-so$ST), ]
cat(sprintf("\n=== (2) SOBOL  —  Varianzanteil am Tiefpunkt (N=%d, Bandbreiten aus Blatt 06) ===\n", N))
print(so, row.names=FALSE)
cat("Lesart: S1 = Haupteffekt, ST = inkl. Wechselwirkungen.  ST deutlich > S1  =>  Interaktion.\n")

# ---------- optionaler Tornado-Plot ----------
tryCatch({
  o <- order(abs(tor$swing))
  png("out/tornado_tiefpunkt.png", width=950, height=520)
  par(mar=c(5,9,4,2))
  barplot(tor$swing[o]/1e6, names.arg=tor$driver[o], horiz=TRUE, las=1, col="#1F3864",
          xlab="Veraenderung Tiefpunkt [Mio. EUR] bei +/-1 Prozentpunkt",
          main="Tornado — Sensitivitaet des Liquiditaets-Tiefpunkts")
  abline(v=0, col="grey40"); dev.off()
  cat("\n[Plot] out/tornado_tiefpunkt.png geschrieben\n")
}, error=function(e) cat("\n[Plot] uebersprungen (kein Grafikgeraet)\n"))
