# ============================================================================
#  R/phase_simulation.R  —  Phase 6 (Simulation, Kern-Einheit phase-06-fan)
#
#  Rechnet die genestete Monte-Carlo ueber die EINGEFRORENE Engine und gibt ein
#  Ergebnisobjekt zurueck, statt in die Konsole zu drucken. Der Planner hat die
#  urspruengliche Phase 6 wegen Diff-Budget in drei Einheiten geschnitten:
#  phase-06-fan (hier) rechnet den VOLLSTAENDIGEN Ergebnis-Vertrag `out`; die
#  Schwester-Einheiten phase-06-monate (Monats-Tabelle) und phase-06-deckung
#  (Solvenz-Verdikt) projizieren dieses `out` nur unveraendert.
#
#  Genestete Monte-Carlo (wie legacy/05_simulation.R, Rechenlogik unveraendert):
#    * aeussere Schicht (epistemisch): korrelierte Jahreswerte je Treiber ueber
#      draw_epistemic_correlated (Gauss-Copula + PERT-Raender), Seed 2026.
#    * innere Schicht (aleatorisch): monatliches multiplikatives Rauschen je
#      Treiber-Basis mit sigma aus spec$cv, ein gemeinsamer Monatsschock fuer
#      Menge bzw. Material. Jeder Pfad laeuft durch run_model.
#
#  SELBSTENTHALTEN wie phase_kopula: spec (build_dists mit estimate_cv_harmonic-
#  CV auf der dokumentierten Basis-Allokation wie Phase 4) und R (make_psd aus
#  Blatt 07, derselbe Engine-Pfad wie Phase 5) werden SELBST gerechnet — bau- und
#  testbar OHNE Vorphase. out$spec / out$R sind bit-identisch zum direkten
#  Engine-Lauf; die UI verbiegt die Spezifikation nicht.
#
#  Determinismus (IMPLEMENTATION_RULES §Determinismus: Phase Simulation 2026):
#  set.seed(seed) steht DIREKT vor der epistemischen Ziehung — sie ist der erste
#  RNG-Verbraucher, damit der erste simulierte Treibervektor eine frisch gesetzte
#  Ziehung trifft (kein vorzeitiger RNG-Verbrauch). Der Seed ist Teil des
#  Szenario-Schluessels.
#
#  Nebenlaeufigkeit: der Wrapper ist synchron und worker-sicher (kein Grafik-
#  geraet im Rechenpfad, keine Globals). Die schwere MC laeuft im App-Modul im
#  mirai/future-Worker mit Fortschritt + Abbrechen, nie im UI-Thread
#  (CLAUDE.md / IMPLEMENTATION_RULES §Nebenlaeufigkeit).
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R und R/scenario.R
#  bereits gesourct sind (load_model, estimate_cv_harmonic, build_dists,
#  make_psd, draw_epistemic_correlated, run_model).
# ============================================================================

# Zeichnet den Endbestand-Fan (P5-P95- und P25-P75-Baender, Medianlinie,
# Mindestreserve- und KK-Limit-Linien) sowie die Unterdeckungs-Wahrscheinlichkeit
# je Monat als PNG in den Szenario-Workspace. Gibt den Pfad zurueck, oder NA,
# wenn kein Grafikgeraet verfuegbar / das Ziel nicht schreibbar ist (sauberer
# Headless-Fallback — nie ein harter Abbruch).
.plot_simulation <- function(fan, P_under, mindestreserve, kk_limit, path) {
  tryCatch({
    H <- ncol(fan); x <- seq_len(H)
    # png() auf einem nicht schreibbaren Ziel WARNT ("unable to open file") und
    # wirft danach — der Wurf loest den sauberen NA-Fallback aus; die Warnungen
    # sind reines Rauschen und werden stummgeschaltet (kein stiller Skip: der
    # harte Vertrag ist die Existenzpruefung des Aufrufers bzw. das NA hier).
    suppressWarnings(grDevices::png(path, width = 1100, height = 780))
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mfrow = c(2, 1), mar = c(4, 5, 3, 2)); on.exit(graphics::par(op), add = TRUE)

    ylim <- range(fan, mindestreserve, -kk_limit) / 1e6
    graphics::plot(x, fan["50%", ] / 1e6, type = "n", ylim = ylim, xlab = "",
                   ylab = "Endbestand [Mio. EUR]",
                   main = "Liquiditaet — Verteilung ueber die Simulation", xaxt = "n")
    graphics::axis(1, x, paste0("M", x))
    graphics::polygon(c(x, rev(x)), c(fan["5%", ],  rev(fan["95%", ])) / 1e6, col = "#D9E1F2", border = NA)
    graphics::polygon(c(x, rev(x)), c(fan["25%", ], rev(fan["75%", ])) / 1e6, col = "#7F9AC0", border = NA)
    graphics::lines(x, fan["50%", ] / 1e6, col = "#1F3864", lwd = 2)
    graphics::abline(h = 0, col = "grey50")
    graphics::abline(h = mindestreserve / 1e6, col = "#C00000", lty = 2)
    graphics::abline(h = -kk_limit / 1e6,       col = "#C00000", lwd = 2)
    graphics::legend("topright", c("P5-P95", "P25-P75", "Median", "Mindestreserve", "KK-Limit"),
                     fill = c("#D9E1F2", "#7F9AC0", NA, NA, NA), border = NA,
                     lty = c(NA, NA, 1, 2, 1), lwd = c(NA, NA, 2, 1, 2),
                     col = c(NA, NA, "#1F3864", "#C00000", "#C00000"), bty = "n", cex = 0.85)
    graphics::barplot(100 * P_under, names.arg = paste0("M", x), col = "#1F3864",
                      ylab = "P(unter Reserve) [%]",
                      main = "Wahrscheinlichkeit unter Mindestreserve je Monat", ylim = c(0, 100))
    path
  }, error = function(e) NA_character_)
}

phase_simulation <- function(sc, plot = TRUE) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  # Blatt 07_Korrelation ist Pflicht — ohne die Zielmatrix ist die epistemische
  # Ziehung (draw_epistemic_correlated braucht R) nicht ableitbar. Benannter
  # Fehler statt eines NULL-Griffs.
  if (is.null(mdl$corr_raw))
    stop("Blatt 07_Korrelation fehlt in der Mappe — die Zielkorrelation der Simulation ist nicht ableitbar.")

  # ---- SELBSTENTHALTENE spec: build_dists mit geschaetztem CV ----------------
  # Dokumentierte Basis-Allokation wie Phase 4: menge = {menge, menge_preis},
  # material = {material}; estimate_cv_harmonic auf der aggregierten Ist-Basis
  # liefert die aleatorische CV je Treibergruppe (Vorgabe-CV wird ueberschrieben).
  D   <- mdl$pos$driver
  vol <- colSums(mdl$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  mat <- colSums(mdl$base[D == "material",                 , drop = FALSE])
  est  <- list(menge = estimate_cv_harmonic(vol), material = estimate_cv_harmonic(mat))
  spec <- build_dists(mdl, est)
  if (!length(spec))
    stop("Blatt 06_Verteilungen fehlt oder enthaelt keine Treiber — die Rand-Verteilungen der Simulation sind nicht ableitbar.")
  drivers <- names(spec)

  # ---- SELBSTENTHALTENE R: Zielmatrix aus Blatt 07 -> make_psd ---------------
  # Exakt der Engine-Pfad aus phase_korrelation/phase_kopula: Rohmatrix aus
  # corr_raw, auf die build_dists-Treiberreihenfolge ausgerichtet, dann ueber den
  # eingefrorenen Kernel make_psd (Eigenwert-Clipping) auf die naechste positiv-
  # semidefinite Matrix repariert. out$R bleibt bit-identisch zum Engine-Lauf.
  raw <- mdl$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"

  # Abdeckungspruefung VOR dem Indizieren: jeder Treiber aus 06 muss in 07 stehen
  # (Zeile UND Spalte), sonst ein benannter Fehler statt eines "subscript out of
  # bounds". draw_epistemic_correlated braucht R vollstaendig ueber alle Treiber.
  miss   <- setdiff(drivers, rownames(M))
  miss_c <- setdiff(drivers, colnames(M))
  if (length(miss) || length(miss_c))
    stop(sprintf("07_Korrelation: Treiber aus 06 fehlen in 07: %s (dort ergaenzen oder Korrelation 0 setzen).",
                 paste(unique(c(miss, miss_c)), collapse = ", ")))
  M <- M[drivers, drivers, drop = FALSE]
  R <- make_psd(M)
  # Eingabevertrag: Zeilen-/Spaltennamen sind genau die Treiber aus spec —
  # draw_epistemic_correlated indiziert R und spec ueber dieselben Namen.
  dimnames(R) <- list(drivers, drivers)

  H    <- mdl$HMAX
  S    <- sc$sim$paths
  seed <- sc$sim$seed

  # ---- Plan-Tiefpunkt (Tier-1-Wirbelsaeule) VOR dem RNG ----------------------
  # Der ohne Rauschen gerechnete Plan-Tiefpunkt (assume = Mode, noise = NULL) ist
  # der direkte Engine-Lauf run_model(mdl)$kpis$trough — die Simulation verbiegt
  # die Tier-1-Planrechnung nicht. Bewusst VOR set.seed: run_model verbraucht hier
  # keinen RNG, aber so bleibt die epistemische Ziehung der erste Verbraucher.
  plan_trough <- run_model(mdl)$kpis$trough

  # ---- aeussere Schicht: korrelierte epistemische Jahreswerte (S x k) --------
  # set.seed DIREKT vor der Ziehung: draw_epistemic_correlated ist der erste
  # RNG-Verbraucher (Determinismus-Vertrag, Seed = Szenario-Schluessel).
  set.seed(seed)
  Theta <- draw_epistemic_correlated(spec, R, S)

  # ---- innere Schicht: monatliches Rauschen, je Treiber ein Monatsschock -----
  # Jede Position erbt den gemeinsamen Schock ihrer Treibergruppe (Menge bzw.
  # Material); sigma = spec$cv der Gruppe. Multiplikativ ueber exp(Schock).
  noise_driver <- ifelse(mdl$pos$driver %in% c("menge_preis", "menge"), "menge",
                  ifelse(mdl$pos$driver == "material",                  "material", NA_character_))
  sig <- c(menge    = if (!is.null(spec$menge))    spec$menge$cv    else 0,
           material = if (!is.null(spec$material)) spec$material$cv else 0)
  make_noise <- function() {
    N  <- matrix(1, nrow(mdl$pos), H)
    sh <- list(menge    = rnorm(H, 0, sig["menge"]),
               material = rnorm(H, 0, sig["material"]))
    for (i in seq_len(nrow(mdl$pos))) {
      nd <- noise_driver[i]; if (!is.na(nd)) N[i, ] <- exp(sh[[nd]])
    }
    N
  }

  # ---- Simulation: jeder Pfad durch die eingefrorene Engine ------------------
  B <- matrix(0, S, H, dimnames = list(NULL, paste0("M", seq_len(H))))
  for (s in seq_len(S)) {
    a <- mdl$assume
    for (d in drivers) a[[d]] <- Theta[s, d]
    B[s, ] <- run_model(mdl, a, make_noise())$cash["Endbestand", ]
  }

  # ---- Abgeleitete Metriken (unabhaengig aus B + Modell-Schwellen) -----------
  qs       <- c(.05, .25, .5, .75, .95)
  fan      <- apply(B, 2, quantile, probs = qs)           # 5 x H, Zeilen "5%".."95%"
  P_under  <- colMeans(B < mdl$mindestreserve)            # Anteil unter Mindestreserve je Monat
  trough   <- apply(B, 1, min)                            # Tiefpunkt je Pfad
  P_breach <- mean(trough < -mdl$kk_limit)                # Anteil Pfade, deren Tiefpunkt die KK-Linie reisst
  P_solvent <- 1 - P_breach

  metrics <- list(
    S             = S,
    seed          = seed,                                 # Szenario-Schluessel (Determinismus)
    H             = H,
    trough_median = unname(median(trough)),
    trough_p05    = unname(quantile(trough, .05)),
    trough_p95    = unname(quantile(trough, .95)),
    P_breach      = P_breach,
    P_breach_pct  = 100 * P_breach,
    P_solvent     = P_solvent,
    P_solvent_pct = 100 * P_solvent,
    plan_trough   = plan_trough
  )

  # ---- Diagnose-Badges an die REAL gepruefte Eigenschaft binden --------------
  # deterministisch: eine frisch geseedete Ziehung reproduziert Theta[1, ] (kein
  # lazily-immer-TRUE Badge, sondern der tatsaechliche Reproduktionstest). Der
  # Nachzug am Ende verbraucht RNG, hat aber keinen Downstream-Effekt mehr.
  set.seed(seed)
  T_chk           <- draw_epistemic_correlated(spec, R, S)
  deterministisch <- identical(unname(T_chk[1L, ]), unname(Theta[1L, ]))

  # stuetzen_ok: jeder epistemische Zug liegt im deklarierten Traeger [min,max].
  stuetzen_ok <- TRUE
  for (d in drivers) {
    s <- spec[[d]]
    stuetzen_ok <- stuetzen_ok && all(Theta[, d] >= s$min - 1e-9 & Theta[, d] <= s$max + 1e-9)
  }

  diagnostics <- list(
    n_treiber       = length(spec),   # == ncol(Theta) == length(spec)
    deterministisch = deterministisch, # Badge: Seed reproduziert die Ziehung
    stuetzen_ok     = stuetzen_ok      # Badge: PERT-Stuetzen-Vertrag gehalten
  )

  # ---- Artefakt fuer Phase 7 (Varianzzerlegung) + Fan-Chart ------------------
  endbestand_path <- file.path(sc$workspace, "endbestand.rds")
  saveRDS(B, endbestand_path)

  plot_path <- if (isTRUE(plot))
    .plot_simulation(fan, P_under, mdl$mindestreserve, mdl$kk_limit,
                     file.path(sc$workspace, "simulation.png")) else NA_character_

  list(
    phase           = "simulation",
    scenario        = sc$name,
    B               = B,               # S x H Endbestand-Matrix (Artefakt Phase 7)
    Theta           = Theta,           # S x k epistemische Ziehungen
    fan             = fan,             # 5 x H Quantilbaender P5/P25/P50/P75/P95
    P_under         = P_under,         # Anteil unter Mindestreserve je Monat
    trough          = trough,          # Tiefpunkt je Pfad
    metrics         = metrics,         # Kennzahlen des Schirms
    spec            = spec,            # Rand-Verteilungen je Treiber (selbstenthalten)
    R               = R,               # reparierte Korrelationsmatrix (selbstenthalten); dimnames == Treiber
    plot_path       = plot_path,       # PNG im Workspace ODER NA (Headless-Fallback)
    endbestand_path = endbestand_path, # gesicherte endbestand.rds (Eingabe Phase 7)
    diagnostics     = diagnostics      # logische Badges des Schirms
  )
}
