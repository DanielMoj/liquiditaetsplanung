# ============================================================================
#  R/phase_kopula.R  —  Phase 5 (Kopula) als Funktion
#
#  Ruft die EINGEFRORENE Engine (kernel/distributions.R) auf und validiert die
#  Gauss-Copula, mit der die Simulation (Phase 6) korrelierte epistemische
#  Jahreswerte zieht: Z ~ MVN(0, R) -> U = Phi(Z) -> X_j = F_j^{-1}(U_j) (PERT).
#
#  SELBSTENTHALTEN: der Wrapper rechnet die Treiberspezifikation (build_dists)
#  und die reparierte Korrelationsmatrix R (make_psd) selbst — exakt derselbe
#  Engine-Pfad wie R/phase_korrelation.R, ohne Artefakt aus einer Vorphase. So
#  ist die Phase bau- und testbar ohne die Korrelations-Phase davor.
#
#  VALIDIERUNG (Seed 11, Phase Abhaengigkeit; IMPLEMENTATION_RULES §Determinismus):
#    * Rangtreue (Gauss-Copula-Vertrag): der empirische Spearman der Ziehungen
#      trifft die VEROEFFENTLICHTE Formel (6/pi)*asin(R/2). Der Rang ist copula-
#      invariant -> die Formel ist der unabhaengige Orakel-Wert (nicht die Engine
#      gegen sich selbst). spearman_max_dev ist die groesste absolute Abweichung.
#    * Copulabedingte Daempfung: pearson_max_dev ist die groesste absolute
#      Abweichung des empirischen Pearson von R (bei schiefen PERT-Raendern
#      daempft die Copula die lineare Korrelation — keine Richtungsbehauptung,
#      nur die reine Vertragsformel max|.|).
#    * PERT-Stuetzen-Vertrag: jeder korrelierte Zug liegt im deklarierten Traeger
#      [min,max] je Treiber — die Copula respektiert die Raender (qpert bildet
#      [0,1] per Konstruktion in [min,max] ab; hier aus der Ziehung nachgeprueft).
#
#  Die gemeldeten Kennzahlen sind GENAU die aus Seed und n reproduzierten Werte
#  (kein geschoenter Wert): set.seed(seed) direkt vor der EINEN Ziehung, aus der
#  Spearman- und Pearson-Kennzahl gemeinsam abgeleitet werden. Gibt ein
#  Ergebnisobjekt zurueck statt in die Konsole zu drucken.
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R und R/scenario.R
#  bereits gesourct sind (load_model, build_dists, make_psd,
#  draw_epistemic_correlated).
# ============================================================================

phase_kopula <- function(sc, seed = 11, n = 100000) {
  stopifnot(inherits(sc, "liqui_scenario"))
  mdl <- sc$mdl

  # Blatt 07_Korrelation ist Pflicht — ohne die Zielmatrix ist die Copula-
  # Struktur nicht ableitbar. Benannter Fehler statt eines NULL-Griffs.
  if (is.null(mdl$corr_raw))
    stop("Blatt 07_Korrelation fehlt in der Mappe — die Zielkorrelation der Copula ist nicht ableitbar.")

  # ---- Treiberreihenfolge = build_dists (Rand-Verteilungen wie Phase 4) -----
  # CV ist fuer die Copula irrelevant -> Vorgaben genuegen; es zaehlt die
  # Reihenfolge/Menge der Treiber (draw_epistemic_correlated indiziert R und
  # spec ueber dieselben Namen).
  spec <- build_dists(mdl)
  if (!length(spec))
    stop("Keine Treiber in Blatt 06_Verteilungen — leere Verteilungsspezifikation.")
  drivers <- names(spec)

  # ---- Zielmatrix aus Blatt 07, auf die Treiberreihenfolge ausgerichtet ------
  # Exakt die Schritte des Handoff-Ankers (identisch zu phase_korrelation).
  raw <- mdl$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"

  # Abdeckungspruefung VOR dem Indizieren: jeder Treiber aus Blatt 06 muss in
  # Blatt 07 stehen (Zeile UND Spalte), sonst ein benannter Fehler (nennt den
  # fehlenden Treiber) statt eines "subscript out of bounds".
  # draw_epistemic_correlated braucht R vollstaendig ueber alle Treiber.
  miss   <- setdiff(drivers, rownames(M))
  miss_c <- setdiff(drivers, colnames(M))
  if (length(miss) || length(miss_c))
    stop(sprintf("07_Korrelation: Treiber aus 06 fehlen in 07: %s (dort ergaenzen oder Korrelation 0 setzen).",
                 paste(unique(c(miss, miss_c)), collapse = ", ")))
  M <- M[drivers, drivers, drop = FALSE]

  # ---- Reparatur auf die naechste positiv-semidefinite Matrix (Kernel) -------
  # Ueber den eingefrorenen make_psd (Eigenwert-Clipping) — bit-identisch zum
  # direkten Engine-Lauf (Treue-Vertrag), derselbe R-Wert wie phase_korrelation.
  R <- make_psd(M)
  # Eingabevertrag: Zeilen-/Spaltennamen sind genau die Treiber aus spec —
  # draw_epistemic_correlated indiziert R und spec ueber dieselben Namen.
  dimnames(R) <- list(drivers, drivers)

  # ---- Validierung der Gauss-Copula (Seed 11, EINE Ziehung) ------------------
  # set.seed direkt vor der Ziehung: draw_epistemic_correlated ist der erste
  # RNG-Verbraucher, damit die gemeldeten Kennzahlen aus (seed, n) exakt
  # reproduzierbar sind. Spearman- UND Pearson-Kennzahl aus DERSELBEN Ziehung.
  set.seed(seed)
  X  <- draw_epistemic_correlated(spec, R, n)

  # Rangtreue gegen die VEROEFFENTLICHTE Formel (unabhaengiges Orakel).
  es <- cor(X, method = "spearman")
  ex <- (6 / pi) * asin(R / 2)
  spearman_max_dev <- max(abs(es - ex))

  # Copulabedingte Daempfung: Pearson gegen R (off-diagonale Paare).
  ep <- cor(X)
  pearson_max_dev <- max(abs(ep[upper.tri(ep)] - R[upper.tri(R)]))

  # PERT-Stuetzen-Vertrag: jeder Zug in [min,max] je Treiber (aus X geprueft).
  stuetzen_ok <- TRUE
  for (j in drivers) {
    s <- spec[[j]]
    stuetzen_ok <- stuetzen_ok && all(X[, j] >= s$min - 1e-9 & X[, j] <= s$max + 1e-9)
  }

  # Reproduktions-Toleranz aus dem Handoff-Anker (Spearman vs. Formel).
  repro_tol <- 0.03

  validierung <- list(
    seed             = seed,             # Szenario-Schluessel Phase Abhaengigkeit
    n                = n,                # Stichprobengroesse der Validierungs-Ziehung
    spearman_max_dev = spearman_max_dev, # max|empirischer Spearman - (6/pi)asin(R/2)|
    pearson_max_dev  = pearson_max_dev,  # max|empirischer Pearson - R| (off-diagonal)
    repro_tol        = repro_tol
  )

  diagnostics <- list(
    n_treiber   = length(spec),                 # == nrow(R) == length(spec)
    reproduziert = spearman_max_dev <= repro_tol, # Badge: Copula reproduziert die Korrelation
    stuetzen_ok  = stuetzen_ok                   # Badge: PERT-Stuetzen-Vertrag gehalten
  )

  list(
    phase       = "kopula",
    scenario    = sc$name,
    R           = R,           # reparierte Korrelationsmatrix (bit-identisch make_psd(M)); dimnames == Treiber
    spec        = spec,        # Rand-Verteilungen je Treiber (Reihenfolge = Eingabevertrag)
    validierung = validierung, # ehrlich aus (seed, n) reproduzierte Kennzahlen
    diagnostics = diagnostics  # logische Badges des Schirms
  )
}
