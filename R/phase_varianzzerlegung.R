# ============================================================================
#  R/phase_varianzzerlegung.R  —  Phase 7 (Varianzzerlegung)
#
#  Zerlegt die Streuung des Liquiditaets-Tiefpunkts T = min ueber die
#  Endbestand-Zeile von run_model in ihre Quellen (Vorlage
#  legacy/05b_varianzzerlegung.R). Drei Laeufe gleicher Groesse S ueber die
#  EINGEFRORENE Engine:
#    T_full — epistemische Jahreswerte korreliert gezogen
#             (draw_epistemic_correlated), monatliches Rauschen AN;
#    T_epi  — DIESELBEN Ziehungen Theta, Rauschen AUS (Noise-Matrix aus
#             Einsen, verbraucht KEIN RNG);
#    T_ale  — Treiber fix auf mdl$assume (Modes), Rauschen AN.
#  Varianzzerlegungssatz: Var(T) = Var(E[T|theta]) + E[Var(T|theta)];
#  var_inter ist der Interaktionsrest var_gesamt - var_epi - var_ale (klein
#  bei naeherungsweiser Unabhaengigkeit der Quellen).
#
#  SELBSTENTHALTEN wie phase_simulation, KEIN Aufruf von phase_simulation:
#  spec (build_dists mit estimate_cv_harmonic-CV auf der dokumentierten
#  Basis-Allokation) und R (make_psd aus Blatt 07) werden SELBST gerechnet —
#  exakt derselbe Engine-Pfad wie R/phase_simulation.R, bit-identisch. Auch
#  die Rauschlogik ist bit-treu zur inneren Schicht von phase_simulation
#  (gemeinsamer Monatsschock je Basis menge/material, exp-transformiert,
#  sigma aus spec$cv) — nicht neu erfunden.
#
#  Determinismus (IMPLEMENTATION_RULES §Determinismus, Seed = Szenario-
#  Schluessel): set.seed(seed) steht DIREKT vor der epistemischen Ziehung als
#  erstem RNG-Verbraucher; danach laeuft eine feste deterministische Abfolge
#  (T_full/T_epi verschraenkt je Pfad, dann T_ale) — ein zweiter Aufruf mit
#  identischem sc reproduziert T_full und T_ale exakt.
#
#  Nebenlaeufigkeit: der Wrapper ist synchron und worker-sicher (keine
#  Konsolen-Prints, kein Grafikgeraet im Rechenpfad, keine Globals). Die drei
#  teuren Laeufe laufen im App-Modul im mirai-Worker, nie im UI-Thread.
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R und
#  R/scenario.R bereits gesourct sind (load_model, estimate_cv_harmonic,
#  build_dists, make_psd, draw_epistemic_correlated, run_model).
# ============================================================================

# varianz_from_troughs: die EINE reine Zerlegungsfunktion getrennt vom Wrapper
# (Muster deckung_from_out), loest KEINE Simulation aus — rechnet nur auf den
# drei Tiefpunkt-Vektoren gleicher Laenge. Numerik bleibt numerisch: die EUR-/
# Prozent-Formatierung (fmt()/sprintf) passiert erst im app-Render.
#   * var_gesamt/var_epi/var_ale: var() je Regime-Vektor;
#     var_inter = var_gesamt - var_epi - var_ale (Interaktionsrest).
#   * sd_gesamt = sqrt(var_gesamt); sd_epi/sd_ale mit max(., 0) gegen einen
#     numerisch minimal negativen Rest.
#   * anteil_epi/anteil_ale = var_*/var_gesamt, anteil_inter =
#     var_inter/var_gesamt — die drei Anteile summieren sich exakt zu 1.
#   * S = gemeinsame Laenge der Vektoren.
varianz_from_troughs <- function(T_full, T_epi, T_ale) {
  stopifnot(length(T_full) == length(T_epi), length(T_full) == length(T_ale))

  var_gesamt <- var(T_full)
  var_epi    <- var(T_epi)
  var_ale    <- var(T_ale)
  var_inter  <- var_gesamt - var_epi - var_ale

  list(
    var_gesamt   = var_gesamt,
    var_epi      = var_epi,
    var_ale      = var_ale,
    var_inter    = var_inter,
    sd_gesamt    = sqrt(var_gesamt),
    sd_epi       = sqrt(max(var_epi, 0)),
    sd_ale       = sqrt(max(var_ale, 0)),
    anteil_epi   = var_epi   / var_gesamt,
    anteil_ale   = var_ale   / var_gesamt,
    anteil_inter = var_inter / var_gesamt,
    S            = length(T_full)
  )
}

phase_varianzzerlegung <- function(sc, S = sc$sim$paths) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  # Blatt 07_Korrelation ist Pflicht — ohne die Zielmatrix ist die epistemische
  # Ziehung (draw_epistemic_correlated braucht R) nicht ableitbar. Benannter
  # Fehler statt eines NULL-Griffs.
  if (is.null(mdl$corr_raw))
    stop("Blatt 07_Korrelation fehlt in der Mappe — die Zielkorrelation der Varianzzerlegung ist nicht ableitbar.")

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
    stop("Blatt 06_Verteilungen fehlt oder enthaelt keine Treiber — die Rand-Verteilungen der Varianzzerlegung sind nicht ableitbar.")
  drivers <- names(spec)

  # ---- SELBSTENTHALTENE R: Zielmatrix aus Blatt 07 -> make_psd ---------------
  # Exakt der Engine-Pfad aus phase_simulation: Rohmatrix aus corr_raw, auf die
  # build_dists-Treiberreihenfolge ausgerichtet, dann ueber den eingefrorenen
  # Kernel make_psd (Eigenwert-Clipping) repariert — bit-identisch.
  raw <- mdl$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"

  # Abdeckungspruefung VOR dem Indizieren: jeder Treiber aus 06 muss in 07
  # stehen (Zeile UND Spalte), sonst ein benannter Fehler statt eines
  # "subscript out of bounds".
  miss   <- setdiff(drivers, rownames(M))
  miss_c <- setdiff(drivers, colnames(M))
  if (length(miss) || length(miss_c))
    stop(sprintf("07_Korrelation: Treiber aus 06 fehlen in 07: %s (dort ergaenzen oder Korrelation 0 setzen).",
                 paste(unique(c(miss, miss_c)), collapse = ", ")))
  M <- M[drivers, drivers, drop = FALSE]
  R <- make_psd(M)
  dimnames(R) <- list(drivers, drivers)

  H    <- mdl$HMAX
  seed <- sc$sim$seed

  # ---- aeussere Schicht: korrelierte epistemische Jahreswerte (S x k) --------
  # set.seed DIREKT vor der Ziehung: draw_epistemic_correlated ist der erste
  # RNG-Verbraucher (Determinismus-Vertrag, Seed = Szenario-Schluessel).
  set.seed(seed)
  Theta <- draw_epistemic_correlated(spec, R, S)

  # ---- innere Schicht: Rauschlogik BIT-TREU zu R/phase_simulation.R ----------
  # Jede Position erbt den gemeinsamen Monatsschock ihrer Treibergruppe (Menge
  # bzw. Material); sigma = spec$cv der Gruppe. Multiplikativ ueber exp(Schock).
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
  # Rauschen AUS = Einsen-Matrix, verbraucht KEIN RNG — der T_epi-Lauf ist
  # damit extern rekonstruierbar (assume + Theta[s, ], Noise = Einsen).
  N1 <- matrix(1, nrow(mdl$pos), H)

  trough <- function(a, noise) min(run_model(mdl, a, noise)$cash["Endbestand", ])

  # ---- drei Regime-Laeufe gleicher Groesse S ---------------------------------
  # Feste RNG-Abfolge wie legacy/05b: je Pfad s zuerst T_full (ein
  # make_noise()-Zug), dann T_epi OHNE RNG-Verbrauch; danach S weitere
  # make_noise()-Zuege fuer T_ale. Alles haengt nur am Seed -> Determinismus.
  T_full <- T_epi <- T_ale <- numeric(S)
  for (s in seq_len(S)) {
    a <- mdl$assume
    for (d in drivers) a[[d]] <- Theta[s, d]
    T_full[s] <- trough(a, make_noise())   # beide Quellen
    T_epi[s]  <- trough(a, N1)             # nur epistemisch (Rauschen aus)
  }
  for (s in seq_len(S))
    T_ale[s] <- trough(mdl$assume, make_noise())  # nur aleatorisch (Modes fix)

  # varianz_from_troughs liefert auch S (== Laenge der Laeufe) mit.
  c(
    list(
      phase    = "varianzzerlegung",
      scenario = sc$name,
      T_full   = T_full,
      T_epi    = T_epi,
      T_ale    = T_ale,
      Theta    = Theta               # S x k, dimnames = Treiber
    ),
    varianz_from_troughs(T_full, T_epi, T_ale),
    list(seed = seed)
  )
}
