# ============================================================================
#  tests/test_verteilungsschicht.R
#  Deterministische Invarianten der stochastischen Schicht (kernel/distributions.R):
#  PERT (qpert/rpert), CV-Schaetzung, build_dists, make_psd, Gauss-Copula
#  (draw_epistemic_correlated) — plus metamorphe Tiefpunkt-Monotonie unter
#  Zufluss-Skalierung (Engine-Transport der aleatorischen noise-Matrix).
#  Der Kernel wird NUR aufgerufen, nie editiert.
#
#  Seed-/Toleranz-Strategie (Flakiness-Disziplin):
#   - Analytische Invarianten (Quantilfunktion, Quadratur, PSD, Monotonie)
#     sind seed-frei und exakt.
#   - Stochastische Checks laufen unter MEHREREN GEPINNTEN Seeds; die
#     Toleranzbaender sind am MC-Standardfehler der Ziehungszahl kalibriert
#     (Marge >= 4-5 SE; gemessene Abweichung bei korrektem Code <= ~2 SE).
#     Gepinnte Seeds => bitreproduzierbar, die Suite wuerfelt nie.
#   - Keine festen Richtungs-Asserts, wo die Richtung parameterabhaengig
#     kippen kann (F-12): die Monotonie laeuft mit tax_on = FALSE, weil
#     USt-/Steuer-Lags die Richtung monatsweise drehen koennen. Ohne Steuern
#     ist komponentenweise Dominanz ein Theorem: Zufluss-Accruals >= 0 und
#     Profilgewichte >= 0 (als Vorbedingung geprueft) machen jeden Monats-
#     Zufluss nichtfallend in k, und die KK-Zins-Rekursion ist monoton im
#     Vorsaldo (kkz = max(0,-prev)*zins/12).
# ============================================================================
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# PERT-Momente (lambda = 4): mu = (a+4m+b)/6, var = (mu-a)(b-mu)/7.
.pert_mean <- function(a, m, b) (a + 4 * m + b) / 6
.pert_sd   <- function(a, m, b) { mu <- .pert_mean(a, m, b); sqrt((mu - a) * (b - mu) / 7) }

# ---------------------------------------------------------------------------
# qpert: analytische Invarianten (seed-frei, exakt)
# ---------------------------------------------------------------------------
test_that("qpert ist monotone Quantilfunktion mit exakten Raendern und Schranken", {
  a <- -0.02; m <- 0.01; b <- 0.08
  p <- seq(0, 1, by = 0.01)
  q <- qpert(p, a, m, b)
  expect_true(all(is.finite(q)))
  expect_true(all(diff(q) >= 0))                     # monoton nichtfallend
  expect_true(all(q >= a - 1e-12 & q <= b + 1e-12))  # harte Schranken
  expect_equal(q[1], a, tolerance = 1e-12)           # qbeta(0) = 0 exakt
  expect_equal(q[length(q)], b, tolerance = 1e-12)   # qbeta(1) = 1 exakt
})

test_that("qpert: symmetrischer Fall hat Median = Modus = Mitte", {
  expect_equal(qpert(0.5, -0.05, 0, 0.05), 0, tolerance = 1e-12)
})

test_that("qpert-Quadratur reproduziert den PERT-Erwartungswert (a+4m+b)/6", {
  # Integral der Quantilfunktion ueber [0,1] = Erwartungswert; fuer lambda = 4
  # exakt (a+4m+b)/6. Deterministische Quadratur, kein Seed. Gemessenes
  # Residuum bei korrektem Code: < 3e-15.
  for (x in list(c(-0.02, 0.01, 0.08), c(0, 0.02, 0.10))) {
    mu <- integrate(function(p) qpert(p, x[1], x[2], x[3]), 0, 1, rel.tol = 1e-10)$value
    expect_equal(mu, .pert_mean(x[1], x[2], x[3]), tolerance = 1e-8)
  }
})

test_that("qpert/rpert: Degenerations-Guards (b <= a) und Modus-Klammerung", {
  expect_equal(qpert(c(0.2, 0.7), 5, 6, 5), c(5, 5))       # b <= a -> a
  expect_equal(rpert(3, 5, 6, 5), c(5, 5, 5))
  # Modus ausserhalb [a,b] wird auf den Rand geklammert:
  p <- c(0.1, 0.5, 0.9)
  expect_equal(qpert(p, 0, 0.5, 0.2), qpert(p, 0, 0.2, 0.2), tolerance = 1e-12)
  expect_equal(qpert(p, 0, -0.3, 0.2), qpert(p, 0, 0, 0.2), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# rpert: Bereich hart, Mittelwert im MC-kalibrierten Band (3 gepinnte Seeds)
# ---------------------------------------------------------------------------
test_that("rpert zieht im Bereich [a,b] mit PERT-Erwartungswert (mehrere Seeds)", {
  a <- -0.02; m <- 0.01; b <- 0.08; n <- 4000
  band <- 5 * .pert_sd(a, m, b) / sqrt(n)            # 5 SE des Stichprobenmittels
  for (seed in c(4242L, 777L, 2026L)) {
    set.seed(seed)
    x <- rpert(n, a, m, b)
    expect_true(all(x >= a & x <= b))
    expect_lt(abs(mean(x) - .pert_mean(a, m, b)), band)
  }
})

# ---------------------------------------------------------------------------
# estimate_cv_harmonic: exakte Faelle seed-frei, Rausch-Fall kalibriert
# ---------------------------------------------------------------------------
test_that("estimate_cv_harmonic: exakte Harmonik -> 0, skaleninvariant, NA-Guards", {
  t <- 1:48
  pfad <- exp(0.5 + 0.2 * sin(2*pi*t/12) + 0.1 * cos(2*pi*t/12) + 0.05 * sin(4*pi*t/12))
  expect_lt(estimate_cv_harmonic(pfad), 1e-12)                            # exakter Fit
  expect_lt(abs(estimate_cv_harmonic(3 * pfad) - estimate_cv_harmonic(pfad)), 1e-12)  # Skala -> Intercept
  expect_true(is.na(estimate_cv_harmonic(c(pfad[1:20], 0, pfad[22:48]))))  # nichtpositive Werte
  expect_true(is.na(estimate_cv_harmonic(pfad[1:6])))                      # zu kurze Reihe
})

test_that("estimate_cv_harmonic trifft ein bekanntes sigma im kalibrierten Band (mehrere Seeds)", {
  # Wahres multiplikatives sigma = 0.05, n = 48, 5 Regressoren: erwartetes
  # sigma-Dach ~ 0.05*sqrt(43/48) = 0.047, SE(sigma-Dach) ~ 0.0054.
  # Band [0.028, 0.066] = Erwartung +/- ~3.5 SE; gemessen 0.040/0.047/0.049.
  t <- 1:48
  pfad <- exp(0.5 + 0.2 * sin(2*pi*t/12) + 0.1 * cos(2*pi*t/12))
  for (seed in c(1L, 7L, 99L)) {
    set.seed(seed)
    cv <- estimate_cv_harmonic(pfad * exp(rnorm(48, 0, 0.05)))
    expect_gt(cv, 0.028); expect_lt(cv, 0.066)
  }
})

# ---------------------------------------------------------------------------
# build_dists: Spezifikation aus Mappe, CV-Override-Logik
# ---------------------------------------------------------------------------
test_that("build_dists liest Blatt 06 und respektiert die CV-Override-Logik", {
  mdl <- load_model(WB)
  spec <- build_dists(mdl)
  expect_gt(length(spec), 0)
  expect_true(all(names(spec) %in% names(mdl$assume)))     # nur bekannte Treiber
  for (d in names(spec)) {
    expect_equal(spec[[d]]$mode, mdl$assume[[d]])          # Modus = Punktannahme
    expect_true(is.finite(spec[[d]]$min) && is.finite(spec[[d]]$max))
    expect_lt(spec[[d]]$min, spec[[d]]$max)
    expect_identical(spec[[d]]$cv_source, "Vorgabe")
  }
  d1 <- names(spec)[1]
  est <- list(); est[[d1]] <- 0.123
  spec2 <- build_dists(mdl, est)
  expect_equal(spec2[[d1]]$cv, 0.123)                      # Schaetzung gewinnt
  expect_identical(spec2[[d1]]$cv_source, "geschaetzt")
  est[[d1]] <- NA_real_
  spec3 <- build_dists(mdl, est)
  expect_identical(spec3[[d1]]$cv_source, "Vorgabe")       # NA faellt auf Blatt zurueck
})

# ---------------------------------------------------------------------------
# make_psd: valide Matrix unveraendert, kaputte wird echte Korrelationsmatrix
# ---------------------------------------------------------------------------
test_that("make_psd laesst valide Matrizen unveraendert und repariert invalide zu PSD", {
  Rv <- matrix(c(1, 0.5, 0.5, 1), 2)
  expect_lt(max(abs(make_psd(Rv) - Rv)), 1e-12)
  Rb <- matrix(c(1, 0.9, -0.9,  0.9, 1, 0.9,  -0.9, 0.9, 1), 3, 3)   # EW_min = -0.8
  expect_lt(min(eigen(Rb, symmetric = TRUE, only.values = TRUE)$values), 0)
  Rr <- make_psd(Rb)
  expect_gte(min(eigen(Rr, symmetric = TRUE, only.values = TRUE)$values), -1e-10)  # PSD
  expect_lt(max(abs(diag(Rr) - 1)), 1e-8)                                          # Einsdiagonale
  expect_lt(max(abs(Rr - t(Rr))), 1e-12)                                           # symmetrisch
  expect_true(all(abs(Rr[upper.tri(Rr)]) <= 1 + 1e-8))                             # Korrelationen
})

# ---------------------------------------------------------------------------
# Gauss-Copula: Spearman trifft die Zielabhaengigkeit, Pearson ist gedaempft,
# Raender bleiben PERT. Bander am MC-SE kalibriert, 3 gepinnte Seeds.
# ---------------------------------------------------------------------------
.spec3 <- list(
  a = list(min = -0.02, mode = 0.01, max = 0.08),
  b = list(min = -0.05, mode = 0.00, max = 0.03),
  c = list(min =  0.00, mode = 0.02, max = 0.10))
.R3 <- matrix(c(1, 0.6, -0.4,  0.6, 1, 0.2,  -0.4, 0.2, 1), 3, 3)

test_that("draw_epistemic_correlated: Spearman ~ (6/pi)asin(R/2), Pearson gedaempft, Raender hart", {
  # Spearman ist copula-invariant: Soll = (6/pi)*asin(rho/2) (Gauss-Copula),
  # d.h. ~ rho, leicht darunter. Band 0.08 ~ 5 SE bei n = 4000 (gemessen
  # max. Abweichung 0.033 ueber die Seeds). Pearson: nach Gebelein ist
  # |corr(f(Z1), g(Z2))| <= |rho| fuer beliebige monotone Raender ->
  # Obergrenze |rho| + 4-5 SE (0.065); Untergrenze 0.7*|rho| ist der
  # kalibrierte Boden (gemessen >= 0.90*|rho|) und faengt Unabhaengigkeit.
  n <- 4000L
  soll_s <- (6 / pi) * asin(.R3 / 2)
  a_min <- vapply(.spec3, function(s) s$min, numeric(1))
  a_max <- vapply(.spec3, function(s) s$max, numeric(1))
  for (seed in c(11L, 2026L, 4242L)) {
    set.seed(seed)
    X <- draw_epistemic_correlated(.spec3, .R3, n)
    expect_identical(dim(X), c(n, 3L))
    expect_identical(colnames(X), names(.spec3))
    expect_true(all(X >= rep(a_min, each = n) & X <= rep(a_max, each = n)))  # PERT-Schranken
    es <- cor(X, method = "spearman"); ep <- cor(X); ut <- upper.tri(.R3)
    expect_lt(max(abs(es[ut] - soll_s[ut])), 0.08)
    expect_true(all(sign(ep[ut]) == sign(.R3[ut])))
    expect_true(all(abs(ep[ut]) <= abs(.R3[ut]) + 0.065))
    expect_true(all(abs(ep[ut]) >= 0.7 * abs(.R3[ut])))
    # Raender: Spaltenmittel trifft den PERT-Erwartungswert (5 SE; faengt
    # invertierte/vertauschte Raender ueber die Asymmetrie der Spec).
    for (j in seq_along(.spec3)) {
      s <- .spec3[[j]]
      band <- 5 * .pert_sd(s$min, s$mode, s$max) / sqrt(n)
      expect_lt(abs(mean(X[, j]) - .pert_mean(s$min, s$mode, s$max)), band)
    }
  }
})

test_that("draw_epistemic_correlated: R = Einheitsmatrix liefert unabhaengige Ziehungen", {
  set.seed(99L)
  X <- draw_epistemic_correlated(.spec3, diag(3), 4000)
  es <- cor(X, method = "spearman")
  expect_lt(max(abs(es[upper.tri(es)])), 0.08)   # 5 SE um 0
})

test_that("draw_epistemic_correlated: degenerierter Rand (min = max) wird konstant", {
  spec <- .spec3; spec$b <- list(min = 0.01, mode = 0.01, max = 0.01)
  set.seed(7L)
  X <- draw_epistemic_correlated(spec, .R3, 200)
  expect_true(all(X[, "b"] == 0.01))
  expect_true(all(X[, "a"] >= spec$a$min & X[, "a"] <= spec$a$max))
})

test_that("draw_epistemic zieht je Treiber im PERT-Bereich (benannt)", {
  set.seed(123L)
  for (i in 1:20) {
    v <- draw_epistemic(.spec3)
    expect_identical(names(v), names(.spec3))
    for (d in names(.spec3))
      expect_true(v[[d]] >= .spec3[[d]]$min && v[[d]] <= .spec3[[d]]$max)
  }
})

# ---------------------------------------------------------------------------
# Metamorph: Tiefpunkt-Monotonie unter Zufluss-Skalierung (seed-frei).
# Transportiert die aleatorische noise-Matrix (Positionen x Monate) durch die
# Engine — exakt der Mechanismus, den die Simulation (Phase 5) nutzt.
# ---------------------------------------------------------------------------
test_that("Tiefpunkt ist monoton unter Skalierung aller Zufluesse (noise-Matrix, tax_on=FALSE)", {
  mdl <- load_model(WB)
  ri <- which(mdl$pos$grp %in% c("umsatz", "betr_ertrag", "zins_e"))   # Cash-Zuflussgruppen
  # Vorbedingungen des Monotonie-Theorems (Fixture-Eigenschaften, deterministisch):
  expect_gt(length(ri), 0)
  expect_true(all(mdl$base[ri, ] >= 0))                                # Zufluss-Accruals >= 0
  expect_true(all(vapply(mdl$profiles, function(p) all(is.finite(p)) && all(p >= 0), logical(1))))
  expect_gte(mdl$kk_zins, 0)
  mk_noise <- function(k) { nz <- matrix(1, nrow(mdl$pos), mdl$HMAX); nz[ri, ] <- k; nz }

  # k = 1 ist die Identitaet (x * 1 = x in IEEE exakt) -> Basislauf.
  base <- run_model(mdl, tax_on = FALSE)
  expect_equal(run_model(mdl, noise = mk_noise(1), tax_on = FALSE)$kpis$trough,
               base$kpis$trough, tolerance = 1e-12)

  # Monotonie ueber das k-Gitter: mehr Zufluss -> Tiefpunkt nie tiefer, strikt hoeher.
  tr <- vapply(c(0.9, 1, 1.05, 1.1),
               function(k) run_model(mdl, noise = mk_noise(k), tax_on = FALSE)$kpis$trough,
               numeric(1))
  expect_true(all(diff(tr) > 0))

  # Komponentenweise Dominanz: JEDER Monats-Endbestand steigt (Theorem, s. Kopf).
  e1  <- run_model(mdl, noise = mk_noise(1.0), tax_on = FALSE)$cash["Endbestand", ]
  e11 <- run_model(mdl, noise = mk_noise(1.1), tax_on = FALSE)$cash["Endbestand", ]
  expect_true(all(e11 - e1 > -1e-6))
})

test_that("Zufluss-Skalierung verschiebt den Endbestand bei kk_zins = 0 EXAKT um die kumulierten Mehr-Einzahlungen", {
  # Ohne KK-Zins ist die Saldo-Rekursion eine Teleskopsumme:
  # end_k[m] - end_1[m] == cumsum(Einzahlungen_k - Einzahlungen_1)[m], euro-exakt.
  # Gemessenes Residuum bei korrektem Code: 0.0.
  mdl <- load_model(WB); mdl$kk_zins <- 0
  ri <- which(mdl$pos$grp %in% c("umsatz", "betr_ertrag", "zins_e"))
  mk_noise <- function(k) { nz <- matrix(1, nrow(mdl$pos), mdl$HMAX); nz[ri, ] <- k; nz }
  r1  <- run_model(mdl, noise = mk_noise(1.0), tax_on = FALSE)
  r11 <- run_model(mdl, noise = mk_noise(1.1), tax_on = FALSE)
  d_end <- r11$cash["Endbestand", ]   - r1$cash["Endbestand", ]
  d_ein <- r11$cash["Einzahlungen", ] - r1$cash["Einzahlungen", ]
  expect_true(all(d_ein >= 0))                       # Zufluesse nichtfallend in k
  expect_lt(max(abs(d_end - cumsum(d_ein))), 1)      # < 1 EUR auf Millionen-Skala
  expect_gt(min(d_end), 0)                           # und strikt positiv (Fixture traegt Umsatz ab M1)
})
