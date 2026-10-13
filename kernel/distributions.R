# ============================================================================
#  distributions.R  —  stochastische Bausteine (Phase 3+)
#
#  Trennt zwei Arten von Unsicherheit:
#    epistemisch  – Unwissen ueber die Jahresannahme (PERT ueber min/mode/max)
#    aleatorisch  – irreduzible monatliche Streuung um den Pfad (multiplikativ, CV)
#
#  rpert/qpert            PERT-Verteilung (Beta-basiert, ohne Zusatzpaket)
#  estimate_cv_harmonic   aleatorische CV aus Ist-Residuen (harmonische Saison)
#  build_dists            Verteilungsspezifikation aus Modell + Blatt 06
#  draw_epistemic         eine Ziehung der Jahreswerte je Treiber (= ein Szenario)
#
#  Wird von 03_verteilungen.R und spaeter 03_simulation.R per source() genutzt.
# ============================================================================

rpert <- function(n, a, m, b, lambda = 4) {
  if (b <= a) return(rep(a, n))
  m <- min(max(m, a), b)
  alpha <- 1 + lambda * (m - a) / (b - a)
  beta  <- 1 + lambda * (b - m) / (b - a)
  a + (b - a) * rbeta(n, alpha, beta)
}
qpert <- function(p, a, m, b, lambda = 4) {
  if (b <= a) return(rep(a, length(p)))   # Guard analog rpert
  m <- min(max(m, a), b)
  alpha <- 1 + lambda * (m - a) / (b - a)
  beta  <- 1 + lambda * (b - m) / (b - a)
  a + (b - a) * qbeta(p, alpha, beta)
}

# aleatorische (multiplikative) Variabilitaet aus einer Ist-Reihe:
# harmonische Saisonregression auf log(Reihe); Residual-SD ~ CV.
# Hinweis: bei <24 Monaten grob; ab 24 Monaten belastbar (Jahr-zu-Jahr-Residuen).
estimate_cv_harmonic <- function(series, K = 2) {
  series <- as.numeric(series); n <- length(series)
  if (n < 8 || any(series <= 0, na.rm = TRUE)) return(NA_real_)
  t <- seq_len(n); X <- matrix(1, n, 1)
  for (k in 1:K) X <- cbind(X, sin(2*pi*k*t/12), cos(2*pi*k*t/12))
  sd(lm.fit(X, log(series))$residuals)
}

# Verteilungsspezifikation: mode = Punktannahme (mdl$assume),
# min/max + cv aus Blatt 06; cv kann je Treiber durch eine Schaetzung (est) ueberschrieben werden.
build_dists <- function(mdl, est = list()) {
  raw <- mdl$dists_raw; spec <- list()
  if (is.null(raw) || !nrow(raw)) return(spec)
  for (i in seq_len(nrow(raw))) {
    d <- as.character(raw[[1]][i]); if (is.null(mdl$assume[[d]])) next
    pmin <- as.numeric(raw[["p_min"]][i]); pmax <- as.numeric(raw[["p_max"]][i])
    cv_sheet <- as.numeric(raw[["cv"]][i])
    use_est <- !is.null(est[[d]]) && !is.na(est[[d]])
    spec[[d]] <- list(mode = mdl$assume[[d]], min = pmin, max = pmax,
                      cv = if (use_est) est[[d]] else cv_sheet,
                      cv_source = if (use_est) "geschaetzt" else "Vorgabe")
  }
  spec
}

# eine Ziehung des epistemischen Jahreswerts je Treiber (ein Szenario, unkorreliert)
draw_epistemic <- function(spec) {
  vapply(names(spec), function(d) rpert(1, spec[[d]]$min, spec[[d]]$mode, spec[[d]]$max), numeric(1))
}

# naechste positiv-semidefinite Korrelationsmatrix (Eigenwert-Clipping; Fallback ohne Matrix-Paket)
make_psd <- function(R, eps = 1e-8) {
  R <- (R + t(R)) / 2
  for (it in 1:5) {
    e <- eigen(R, symmetric = TRUE)
    if (min(e$values) >= eps) break                 # bereits PSD -> fertig (valide Matrix unveraendert)
    R <- e$vectors %*% diag(pmax(e$values, eps)) %*% t(e$vectors)
    d <- sqrt(diag(R)); R <- R / (d %o% d)          # Reskalierung auf Einsdiagonale kann neg. EW reintroduzieren
    R <- (R + t(R)) / 2                              # -> naechste Iteration clippt erneut
  }
  R
}

# n korrelierte Ziehungen der epistemischen Jahreswerte: Gauss-Copula + PERT-Raender
#   Z ~ MVN(0,R)  ->  U = Phi(Z)  ->  X_j = F_j^{-1}(U_j)   (F_j = PERT)
draw_epistemic_correlated <- function(spec, R, n) {
  d <- names(spec); k <- length(d)
  Z <- matrix(rnorm(n * k), n, k) %*% chol(R)      # cov(Z) = R
  U <- pnorm(Z)
  X <- matrix(0, n, k, dimnames = list(NULL, d))
  for (j in seq_len(k)) X[, j] <- qpert(U[, j], spec[[d[j]]]$min, spec[[d[j]]]$mode, spec[[d[j]]]$max)
  X
}
