# ============================================================================
#  tests/test_phase_var_mutgate.R  —  Mutgate-Nachschub (rot_zuerst: false)
#  fuer phase-07-varianzzerlegung
#
#  Zusatz-Orakel zur Spezifikation tests/test_phase_var.R: der Mutgate-Lauf
#  endete 2/5, drei Ueberlebende, zwei Loecher — diese Datei schliesst beide.
#  Sie ist gegen den AKTUELLEN (korrekten) Stand gruen und toetet die
#  Ueberlebenden. Keine Anker-Literale, keine Tier-1-Werte.
#
#  Block 1 — RNG-Naht-Orakel (killt M2 und M4 mit EINEM Orakel):
#    M2 vertauscht die sigma-Zuweisung im Monatsschock (sh$menge zieht mit
#    sig["material"] und umgekehrt); M4 laesst menge_preis aus der vol-Basis
#    der CV-Schaetzung fallen (D == "menge" statt D %in% c("menge",
#    "menge_preis")). Beide wirken NUR ueber das Rausch-sigma. Die Spec ist
#    dagegen blind, weil sie var/sd/Anteile aus den ZURUECKGEGEBENEN
#    Roh-Vektoren nachrechnet (konsistent falsch bleibt konsistent) und ihr
#    einziges externes Regime-Orakel T_epi OHNE Rauschen laeuft. Dieses
#    Orakel rekonstruiert T_full[1] KOMPLETT ausserhalb des Wrappers aus
#    DERSELBEN RNG-Sequenz — set.seed(seed) -> draw_epistemic_correlated ->
#    die beiden Monatsschock-Zuege in Implementations-Reihenfolge (erst
#    Menge, dann Material) — und verlangt exakte Gleichheit: ein falsches
#    sigma (getauscht oder von der schmalen Basis) verschiebt die
#    rnorm-Zuege und faellt zwingend rot. Guards als HARTE Fails (kein
#    skip) sichern, dass das Orakel fuer beide Mutanten sehend ist.
#
#  Block 2 — Render-Bindungs-Orakel (killt M5):
#    output$varianz_recon in app/app.R gibt sd_epi und sd_ale unter den
#    vertauschten Kennzahl-IDs aus; der DOM-Verifier prueft die IDs nur auf
#    Format/Befuelltheit, nicht auf die BINDUNG DOM -> Quellfeld — dieselbe
#    Luecke, die die Schwester-Orakel der 06er-Schirme schliessen
#    (test_phase_sim_ui.R fuer sim_diag, test_phase_sim_deckung_ui.R fuer
#    deckung_recon). testServer-Basis (kein Browser, keine Monte-Carlo):
#    app/app.R in-Process gesourct, var_rv$out mit einem paarweise
#    unverwechselbaren Stub im phase_varianzzerlegung-Ergebnisvertrag
#    belegt, das gerenderte HTML per xml2 je Kennzahl-ID gegen die
#    UNABHAENGIG aus den Stub-Rohwerten gerechnete Erwartung verglichen;
#    zweiter Flush mit veraendertem Stub beweist, dass der Render dem
#    Objekt folgt (kein eingefrorenes HTML).
#
#  Nebenlaeufigkeits-Hygiene wie in den Schwester-Orakeln: das Sourcen von
#  app/app.R startet bei HAS_ASYNC mirai::daemons(1); der onStop-Handler
#  feuert unter testServer nie. Die Daemons werden am Dateiende
#  deterministisch geschlossen — auch im Skip- und Fehlerpfad.
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
source(file.path(.root, "R", "phase_varianzzerlegung.R"))
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# ---------------------------------------------------------------------------
# Block 1 — RNG-Naht-Orakel: T_full[1] wird KOMPLETT ausserhalb des Wrappers
# aus derselben RNG-Sequenz rekonstruiert (Naht set.seed -> Ziehung ->
# Monatsschock) und muss bit-identisch getroffen werden. Kleines S haelt die
# Suite schnell (ein Pfad genuegt dem Orakel, der Wrapper rechnet 3*S Laeufe).
# ---------------------------------------------------------------------------
test_that("RNG-Naht: T_full[1] == extern aus derselben RNG-Sequenz rekonstruierter Tiefpunkt (sigma-Tausch / schmale CV-Basis fallen rot)", {
  m <- load_model(WB)

  # (a) Externe CV-Schaetzung auf der DOKUMENTIERTEN Basis-Allokation
  # (menge = {menge, menge_preis}, material = {material}), spec ueber
  # build_dists, R exakt ueber den Engine-Pfad (Muster test_phase_sim.R) —
  # ohne den Wrapper zu befragen.
  D   <- m$pos$driver
  vol <- colSums(m$base[D %in% c("menge", "menge_preis"), , drop = FALSE])
  mat <- colSums(m$base[D == "material",                  , drop = FALSE])
  est      <- list(menge = estimate_cv_harmonic(vol), material = estimate_cv_harmonic(mat))
  spec_ext <- build_dists(m, est)
  drivers  <- names(spec_ext)

  raw <- m$corr_raw
  M   <- as.matrix(raw[, -1]); rownames(M) <- as.character(raw[[1]])
  storage.mode(M) <- "numeric"
  R_ext <- make_psd(M[drivers, drivers, drop = FALSE])
  dimnames(R_ext) <- list(drivers, drivers)

  # Die Monatsschock-sigmas, die die Implementation aus spec$cv zieht.
  cv_menge_ext    <- spec_ext$menge$cv
  cv_material_ext <- spec_ext$material$cv

  # Guards als HARTE Fails (kein skip): ist einer verletzt, ist das Orakel
  # blind und der Test muss rot sein, nicht schweigen.
  expect_gt(cv_menge_ext, 0,
            label = "cv_menge_ext (sigma Menge; bei 0 verbrauchte der Monatsschock kein wirksames Rauschen und das Orakel waere rauschblind)")
  expect_gt(cv_material_ext, 0,
            label = "cv_material_ext (sigma Material; bei 0 verbrauchte der Monatsschock kein wirksames Rauschen und das Orakel waere rauschblind)")
  expect_true(cv_menge_ext != cv_material_ext,
              label = "cv_menge_ext != cv_material_ext (bei Gleichheit waere der sigma-Tausch M2 wirkungslos und unsichtbar)")
  cv_menge_schmal <- estimate_cv_harmonic(colSums(m$base[D == "menge", , drop = FALSE]))
  expect_false(isTRUE(all.equal(cv_menge_schmal, cv_menge_ext)),
               label = "estimate_cv_harmonic auf der SCHMALEN Basis (nur D == 'menge') muss einen ANDEREN Wert liefern als cv_menge_ext (sonst waere die fehlende menge_preis-Basis M4 unsichtbar)")

  # (b) DIESELBE RNG-Sequenz wie der Wrapper: set.seed(seed) direkt vor der
  # epistemischen Ziehung, danach in der IMPLEMENTATIONS-Reihenfolge die
  # beiden Monatsschock-Zuege des ersten make_noise() (erst Menge, dann
  # Material — die list()-Konstruktionsreihenfolge der inneren Schicht).
  S <- 6L
  H <- m$HMAX
  set.seed(2026)
  Theta_ext   <- draw_epistemic_correlated(spec_ext, R_ext, S)
  sh_menge    <- rnorm(H, 0, cv_menge_ext)
  sh_material <- rnorm(H, 0, cv_material_ext)

  # Noise-Matrix ueber die noise_driver-Zuordnung (menge/menge_preis ->
  # Menge-Schock, material -> Material-Schock, exp-transformiert);
  # Positionen ohne Zuordnung bleiben 1.
  noise_driver <- ifelse(m$pos$driver %in% c("menge_preis", "menge"), "menge",
                  ifelse(m$pos$driver == "material",                  "material", NA_character_))
  N_ext <- matrix(1, nrow(m$pos), H)
  for (i in seq_len(nrow(m$pos))) {
    if (is.na(noise_driver[i])) next
    N_ext[i, ] <- if (noise_driver[i] == "menge") exp(sh_menge) else exp(sh_material)
  }

  # (c) Referenz-Tiefpunkt des ersten Pfads ausserhalb des Wrappers ...
  a <- m$assume
  for (d in drivers) a[[d]] <- Theta_ext[1, d]
  t_ref <- min(run_model(m, a, N_ext)$cash["Endbestand", ])

  # ... gegen den Wrapper-Lauf mit identischem Seed/S: exakte Gleichheit an
  # der RNG-Naht (Theta) und im vollen Regime (T_full[1], Rauschen AN).
  sc  <- scenario_from_workbook(WB, name = "VarMut", sim = list(paths = S, seed = 2026))
  out <- phase_varianzzerlegung(sc)
  expect_identical(out$Theta, Theta_ext)
  expect_identical(out$T_full[[1]], t_ref)
})

# ---------------------------------------------------------------------------
# Block 2 — Render-Bindungs-Orakel fuer output$varianz_recon (testServer).
# ---------------------------------------------------------------------------

# xml2 (DOM-Parse des varianz_recon-HTML) ist reines Test-Tooling, keine
# App-Abhaengigkeit: fehlt es, wird der Test SICHTBAR uebersprungen (skip_if
# mit Begruendung, im test_that unten) — kein harter stop(), der den DoD
# bricht, und kein stiller Skip.

# Beim Sourcen gestartete mirai-Daemons schliessen (Suite-Invariante: kein Leak).
.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

# App in-Process laden. chdir=TRUE laesst find_root() der App aus app/ heraus
# die Repo-Wurzel bestimmen — wie bei shiny::runApp("app").
.app_env <- new.env(parent = globalenv())
.src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .app_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.src_err)) {
  .daemons_zu()  # ein halb gesourctes app.R darf keinen Daemon zuruecklassen
  stop("app/app.R laesst sich nicht in-Process sourcen: ", conditionMessage(.src_err))
}

.kz_node <- function(doc, id) xml2::xml_find_first(doc, sprintf("//*[@id='%s']", id))
.kz_text <- function(doc, id) {
  n <- .kz_node(doc, id)
  if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
}

# Stub im phase_varianzzerlegung-Ergebnisvertrag (alle Felder), SYNTHETISCH —
# keine Engine, kein Seed-Lauf, KEINE Tier-1-Werte. Die var_*-Felder sind zu
# den sd_*-Rohwerten konsistent (sd^2 bzw. Rest), damit der Stub beide
# denkbaren Quellen gleich haelt; der Render liest sd_*/anteil_*/S/seed.
.mk_var_out <- function(sd_gesamt, sd_epi, sd_ale, anteil_epi, anteil_ale, seed, S) {
  var_gesamt <- sd_gesamt^2; var_epi <- sd_epi^2; var_ale <- sd_ale^2
  list(
    phase    = "varianzzerlegung",
    scenario = "MutgateStub",
    T_full   = c(-410000, -180000, 90000),
    T_epi    = c(-350000, -160000, 40000),
    T_ale    = c( -50000,  -20000, 15000),
    Theta    = matrix(c(1.01, 0.99, 1.02, 0.98, 1.00, 1.03), nrow = 3L,
                      dimnames = list(NULL, c("menge", "material"))),
    var_gesamt   = var_gesamt,
    var_epi      = var_epi,
    var_ale      = var_ale,
    var_inter    = var_gesamt - var_epi - var_ale,
    sd_gesamt    = sd_gesamt,
    sd_epi       = sd_epi,
    sd_ale       = sd_ale,
    anteil_epi   = anteil_epi,
    anteil_ale   = anteil_ale,
    anteil_inter = 1 - anteil_epi - anteil_ale,
    S            = S,
    seed         = seed
  )
}

test_that("varianz_recon bindet jede Kennzahl-ID an GENAU ihr Quellfeld (sd_epi/sd_ale-Tausch faellt rot)", {
  # xml2 ist reines Test-Tooling (DOM-Parse des gerenderten HTML) — fehlt es,
  # sichtbar ueberspringen statt den Build zu brechen.
  skip_if(!requireNamespace("xml2", quietly = TRUE),
          "Paket 'xml2' fehlt (DOM-Parse des varianz_recon-HTML): install.packages(\"xml2\")")

  # Stub A/B: sd_gesamt/sd_epi/sd_ale PAARWEISE UNVERWECHSELBAR (drei klar
  # verschiedene fmt-Darstellungen) und anteil_epi/ale/inter paarweise
  # unverwechselbar mit Summe exakt 1 (61.5/26.5/12.0 bzw. 20.5/70.5/9.0 %).
  stub_a <- .mk_var_out(sd_gesamt = 987654, sd_epi = 123450, sd_ale = 67890,
                        anteil_epi = 0.615, anteil_ale = 0.265, seed = 4242L, S = 3L)
  # Stub B kehrt die Groessenordnung von sd_epi/sd_ale um (epi klein, ale
  # gross) — ein Render, der zufaellig zur A-Belegung passt, faellt hier.
  stub_b <- .mk_var_out(sd_gesamt = 456789, sd_epi = 34567, sd_ale = 210987,
                        anteil_epi = 0.205, anteil_ale = 0.705, seed = 999L, S = 3L)

  # fmt aus der Aufloesungskette der gesourcten App (app-Env -> Kernel), nicht
  # nachgebaut — die Erwartung ist exakt die Darstellung, die die App rendert.
  fmt_app <- get("fmt", envir = .app_env)

  # Erwartungen UNABHAENGIG aus den Stub-Rohwerten (fmt bzw. sprintf mit
  # einer Nachkommastelle mal 100) — nicht aus dem Render abgelesen.
  .check_bindung <- function(doc, o) {
    expect_identical(.kz_text(doc, "varianz-sd-gesamt"), fmt_app(o$sd_gesamt))  # sd_gesamt, nicht epi/ale
    expect_identical(.kz_text(doc, "varianz-sd-epi"),    fmt_app(o$sd_epi))     # sd_epi, nicht sd_ale
    expect_identical(.kz_text(doc, "varianz-sd-ale"),    fmt_app(o$sd_ale))     # sd_ale, nicht sd_epi
    expect_identical(.kz_text(doc, "varianz-anteil-epi"),
                     sprintf("%.1f", 100 * o$anteil_epi))                       # anteil_epi, nicht ale/inter
    expect_identical(.kz_text(doc, "varianz-anteil-ale"),
                     sprintf("%.1f", 100 * o$anteil_ale))                       # anteil_ale, nicht epi/inter
    expect_identical(.kz_text(doc, "varianz-anteil-inter"),
                     sprintf("%.1f", 100 * o$anteil_inter))                     # anteil_inter, nicht epi/ale
  }

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> Auto-
                                           # Trigger (Sim UND Varianz) inert.
    var_rv$running <- FALSE
    var_rv$out     <- stub_a
    session$flushReact()

    ui   <- output$varianz_recon           # renderUI: list(html, deps)
    html <- if (is.list(ui)) ui$html else ui
    .check_bindung(xml2::read_html(as.character(html)), stub_a)

    # Re-Render-Fall (wie im deckung-Orakel): veraenderter Stub -> der Render
    # folgt dem Objekt, kein eingefrorenes HTML.
    var_rv$out <- stub_b
    session$flushReact()

    ui   <- output$varianz_recon
    html <- if (is.list(ui)) ui$html else ui
    .check_bindung(xml2::read_html(as.character(html)), stub_b)
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem/uebersprungenem test_that
# erreicht): die beim Sourcen der App gestarteten mirai-Daemons schliessen —
# kein Prozess-Leak.
.daemons_zu()
