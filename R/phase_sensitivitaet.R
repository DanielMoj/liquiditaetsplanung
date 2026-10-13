# ============================================================================
#  R/phase_sensitivitaet.R  —  Phase 3 (Sensitivitaet) als Funktion
#
#  Ruft die EINGEFRORENE Engine (kernel/) auf: welche Treiber bewegen den
#  Liquiditaets-Tiefpunkt (min. Endbestand ueber den Horizont)?
#   (1) Tornado  — jeden Treiber einzeln +/-1 Prozentpunkt auslenken (deterministisch)
#   (2) Sobol    — varianzbasierte globale Sensitivitaet (Saltelli/Jansen, base R)
#  Gibt ein Ergebnisobjekt zurueck (Zahlen + Tabellen + Plot-Pfade + Diagnose),
#  statt in die Konsole zu drucken. Rechenlogik unveraendert aus legacy/02_sensitivitaet.R;
#  hier wird das cat(...) durch Listeneintraege ersetzt.
#  Setzt voraus, dass kernel/engine.R und R/scenario.R bereits gesourct sind.
#
#  Sobol-Determinismus: fester Seed 1 (IMPLEMENTATION_RULES) — Teil des Szenario-
#  Schluessels. Die Bandbreiten kommen AUSSCHLIESSLICH aus Blatt 06_Verteilungen
#  (einzige Quelle, F-18); fehlt das Blatt, faellt nur der Sobol sauber zurueck —
#  der Tornado bleibt unberuehrt.
# ============================================================================

# Die sieben dokumentierten Jahrestreiber (Vorlage: legacy/02_sensitivitaet.R).
.SENS_DRIVERS <- c("menge", "preis", "tarif", "energie", "material", "pflegebudget", "inflation")

# Tornado-Plot: Swing je Treiber als horizontale Balken (Vorlage: legacy/02_sensitivitaet.R).
# Gibt den Pfad zurueck, oder NA, wenn kein Grafikgeraet verfuegbar ist (headless ohne png).
.plot_tornado <- function(tor, path) {
  tryCatch({
    o <- order(abs(tor$swing))
    grDevices::png(path, width = 950, height = 520)
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mar = c(5, 9, 4, 2)); on.exit(graphics::par(op), add = TRUE)
    graphics::barplot(tor$swing[o] / 1e6, names.arg = tor$driver[o], horiz = TRUE, las = 1,
                      col = "#1F3864",
                      xlab = "Veraenderung Tiefpunkt [Mio. EUR] bei +/-1 Prozentpunkt",
                      main = "Tornado — Sensitivitaet des Liquiditaets-Tiefpunkts")
    graphics::abline(v = 0, col = "grey40")
    path
  }, error = function(e) NA_character_)
}

# Sobol-Plot: S1 (Haupteffekt) und ST (inkl. Wechselwirkungen) je Treiber als Balken.
.plot_sobol <- function(so, N, path) {
  tryCatch({
    o <- order(so$ST)
    M <- t(as.matrix(so[o, c("S1", "ST")]))
    grDevices::png(path, width = 950, height = 520)
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mar = c(5, 9, 4, 2)); on.exit(graphics::par(op), add = TRUE)
    graphics::barplot(M, names.arg = so$Treiber[o], horiz = TRUE, las = 1, beside = TRUE,
                      col = c("#8FAADC", "#1F3864"),
                      xlab = "Varianzanteil am Tiefpunkt",
                      main = sprintf("Sobol — globale Sensitivitaet (N=%d, Blatt 06)", N))
    graphics::legend("bottomright", c("S1 (Haupteffekt)", "ST (inkl. Wechselwirkung)"),
                     fill = c("#8FAADC", "#1F3864"), bty = "n", cex = 0.85)
    path
  }, error = function(e) NA_character_)
}

phase_sensitivitaet <- function(sc, shock = 0.01, sobol_N = 800, plot = TRUE) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  # Zielgroesse: Liquiditaets-Tiefpunkt (min. Endbestand) bei Annahmevektor a.
  target  <- function(a) run_model(mdl, a)$kpis$trough
  drivers <- intersect(.SENS_DRIVERS, names(mdl$assume))

  base_res    <- run_model(mdl)
  base_trough <- base_res$kpis$trough

  # ---------- (1) TORNADO: +/- 1 Prozentpunkt je Treiber ----------
  tor <- data.frame(driver = drivers, low = NA_real_, high = NA_real_,
                    stringsAsFactors = FALSE)
  for (i in seq_along(drivers)) {
    d <- drivers[i]
    aL <- mdl$assume; aL[[d]] <- aL[[d]] - shock
    aH <- mdl$assume; aH[[d]] <- aH[[d]] + shock
    tor$low[i]  <- target(aL)
    tor$high[i] <- target(aH)
  }
  tor$swing <- tor$high - tor$low
  tor <- tor[order(-abs(tor$swing)), ]
  rownames(tor) <- NULL

  # ---------- (2) SOBOL (Saltelli/Jansen) ----------
  # Bandbreiten aus Blatt 06 (einzige Quelle, F-18), nicht hartcodiert. Treiber ohne
  # 06-Zeile (z.B. energie, inflation) haben keine dokumentierte epistemische
  # Bandbreite -> nicht im Sobol. Fehlt das Blatt ganz, faellt nur der Sobol zurueck.
  so <- NULL; so_raw <- NULL; sdrivers <- character(0); sobol_reason <- NULL
  if (sobol_N < 2) {
    sobol_reason <- "Sobol uebersprungen (sobol_N < 2 — nur Tornado angefordert)."
  } else if (is.null(mdl$dists_raw)) {
    sobol_reason <- "Blatt 06_Verteilungen fehlt — Sobol-Bandbreiten nicht ableitbar (Tornado unberuehrt)."
  } else {
    dr_raw   <- as.character(mdl$dists_raw[[1]])
    sdrivers <- drivers[drivers %in% dr_raw]
    if (!length(sdrivers)) {
      sobol_reason <- "Kein Tornado-Treiber hat eine Zeile in Blatt 06_Verteilungen — keine Bandbreiten."
    } else {
      ranges <- stats::setNames(lapply(sdrivers, function(d) {
        i <- match(d, dr_raw)
        c(as.numeric(mdl$dists_raw[["p_min"]][i]), as.numeric(mdl$dists_raw[["p_max"]][i]))
      }), sdrivers)
      k <- length(sdrivers); N <- sobol_N
      sampmat <- function(n) {
        M <- matrix(0, n, k, dimnames = list(NULL, sdrivers))
        for (j in 1:k) { r <- ranges[[sdrivers[j]]]; M[, j] <- stats::runif(n, r[1], r[2]) }
        M
      }
      evalrows <- function(MAT) apply(MAT, 1, function(row) {
        a <- mdl$assume; for (j in 1:k) a[[sdrivers[j]]] <- row[j]; target(a)
      })
      set.seed(1)                                   # fester Sobol-Seed (IMPLEMENTATION_RULES)
      A <- sampmat(N); B <- sampmat(N)
      yA <- evalrows(A); yB <- evalrows(B); VarY <- stats::var(c(yA, yB))
      S1 <- ST <- numeric(k)
      for (j in 1:k) {
        AB <- A; AB[, j] <- B[, j]; yAB <- evalrows(AB)
        S1[j] <- 1 - mean((yB - yAB)^2) / (2 * VarY)   # Jansen Erstordnung
        ST[j] <- mean((yA - yAB)^2) / (2 * VarY)        # Jansen Totaleffekt
      }
      # Rohe Jansen-Schaetzer (ungeklippt, ungerundet) als Diagnose: das Klippen unten
      # ist reine Anzeige-Kosmetik und darf Schaetzfehler nicht verdecken — die
      # Invarianten-Tests pruefen den ROHEN Index gegen [0,1] (Audit).
      so_raw <- data.frame(Treiber = sdrivers, S1_raw = S1, ST_raw = ST,
                           stringsAsFactors = FALSE)
      so <- data.frame(Treiber = sdrivers,
                       S1 = round(pmin(1, pmax(0, S1)), 3),
                       ST = round(pmin(1, pmax(0, ST)), 3),
                       stringsAsFactors = FALSE)
      so <- so[order(-so$ST), ]
      rownames(so) <- NULL
    }
  }

  # ---------- Plots in den Szenario-Workspace ----------
  tornado_plot <- if (isTRUE(plot))
    .plot_tornado(tor, file.path(sc$workspace, "tornado_tiefpunkt.png")) else NA_character_
  sobol_plot <- if (isTRUE(plot) && !is.null(so))
    .plot_sobol(so, sobol_N, file.path(sc$workspace, "sobol_tiefpunkt.png")) else NA_character_

  # Interaktion: ST deutlich groesser als S1 => Wechselwirkung zwischen den Treibern.
  interaktion <- !is.null(so) && any(so$ST - so$S1 > 0.05)

  list(
    phase        = "sensitivitaet",
    scenario     = sc$name,
    base_trough  = base_trough,
    trough_month = base_res$kpis$trough_month,
    shock        = shock,
    drivers      = tor$driver,       # folgen der Tornado-Sortierung (== tornado$driver)
    tornado      = tor,              # data.frame driver, low, high, swing (nach |swing| sortiert)
    sobol        = so,              # data.frame Treiber, S1, ST (nach ST sortiert) ODER NULL
    sobol_raw    = so_raw,          # rohe Jansen-Schaetzer vor Klippen/Rundung (Diagnose) ODER NULL
    sobol_available = !is.null(so),
    sobol_reason = sobol_reason,    # NULL wenn verfuegbar, sonst Begruendung des Rueckfalls
    sobol_drivers = sdrivers,
    sobol_N      = sobol_N,
    sobol_seed   = 1L,
    tornado_plot = tornado_plot,
    sobol_plot   = sobol_plot,
    diagnostics  = list(
      top_driver      = tor$driver[1],
      top_swing       = tor$swing[1],
      sobol_verfuegbar = !is.null(so),
      interaktion     = isTRUE(interaktion)
    )
  )
}
