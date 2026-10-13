# ============================================================================
#  tests/test_phase_sim_noise.R  --  Mutations-Nachschub fuer phase-06-fan (M2)
#
#  Gate-Ueberlebender M2 (Treibergruppen-Tausch in noise_driver): die innere
#  (aleatorische) Schicht von phase_simulation ordnet jeder Position den
#  gemeinsamen Monatsschock ihrer EIGENEN Treibergruppe zu
#  (menge/menge_preis -> Menge-Schock, material -> Material-Schock;
#  sigma = spec$cv der jeweiligen Gruppe). Die bestehende Suite prueft B nur
#  auf Selbstkonsistenz (fan/trough/P_* aus B nachgerechnet) -- ein Tausch der
#  Zuordnung blieb dort unsichtbar.
#
#  Dieses externe Orakel macht die Zuordnung testbar: Pfad 1 wird vollstaendig
#  AUSSERHALB des Wrappers nachgerechnet, in exakt dessen RNG-Reihenfolge
#  (set.seed(seed) direkt vor draw_epistemic_correlated; der erste
#  make_noise()-Aufruf zieht danach ERST den Menge-, DANN den Material-Schock)
#  und mit der DOKUMENTIERTEN Zuordnung. Bit-Vergleich gegen out$B[1, ]
#  (Toleranz 0): erbt eine Position den Schock der FALSCHEN Gruppe (M2),
#  faellt der Vergleich rot. Dass sigma dabei aus spec$cv der jeweils
#  RICHTIGEN Gruppe stammt, steckt implizit im Orakel (die Rekonstruktion
#  nutzt spec_ext$menge$cv fuer den Menge-Schock usw.).
#
#  Kill-Logik ersetzt Rot-zuerst: rot auf dem Mutanten M2, gruen auf main.
#  Keine Anker-Literale -- das Orakel rechnet allein aus Mappe + Engine.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "phase_simulation.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

test_that("Pfad 1 bit-identisch zum externen Orakel: jede Position erbt den Monatsschock ihrer EIGENEN Treibergruppe", {
  m <- load_model(WB)

  # spec/R unabhaengig rekonstruieren -- exakt die selbstenthaltenen Schritte
  # (Basis-Allokation wie Phase 4, make_psd-Pfad wie Phase 5), ohne den
  # Wrapper zu befragen.
  D   <- m$pos$driver
  vol <- colSums(m$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  mat <- colSums(m$base[D == "material",                  , drop = FALSE])
  spec_ext <- build_dists(m, list(menge    = estimate_cv_harmonic(vol),
                                  material = estimate_cv_harmonic(mat)))
  drivers <- names(spec_ext)

  raw <- m$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"
  R_ext <- make_psd(M[drivers, drivers, drop = FALSE])
  dimnames(R_ext) <- list(drivers, drivers)

  # EIN Wrapper-Lauf; kleines S fuer die Laufzeit (der Konstruktor laesst sim
  # ueberschreiben), Seed bleibt der Szenario-Schluessel 2026.
  S   <- 25L
  sc  <- scenario_from_workbook(WB, name = "NoiseOrakel",
                                sim = list(paths = S, seed = 2026))
  out <- phase_simulation(sc)

  # Pfad 1 extern in derselben RNG-Reihenfolge wie der Wrapper nachziehen.
  H <- m$HMAX
  set.seed(sc$sim$seed)
  Theta_ext <- draw_epistemic_correlated(spec_ext, R_ext, S)
  sig_menge    <- if (!is.null(spec_ext$menge))    spec_ext$menge$cv    else 0
  sig_material <- if (!is.null(spec_ext$material)) spec_ext$material$cv else 0
  n_menge    <- exp(rnorm(H, 0, sig_menge))     # Reihenfolge wie make_noise():
  n_material <- exp(rnorm(H, 0, sig_material))  # erst Menge, dann Material.

  # KORREKTE Zuordnung (der Kern des Orakels): menge/menge_preis erben den
  # Menge-Schock, material den Material-Schock, alle uebrigen bleiben 1.
  N <- matrix(1, nrow(m$pos), H)
  for (i in seq_len(nrow(m$pos))) {
    if      (D[i] %in% c("menge", "menge_preis")) N[i, ] <- n_menge
    else if (D[i] %in% "material")                N[i, ] <- n_material
  }

  a <- m$assume
  for (d in drivers) a[[d]] <- Theta_ext[1L, d]
  B1_ext <- run_model(m, a, N)$cash["Endbestand", ]

  expect_identical(unname(out$B[1L, ]), unname(B1_ext))
})
