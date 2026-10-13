# ============================================================================
#  tests/test_massnahmen_ui.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-11-massnahmen-ui
#
#  Vertrag: handoff/phase-11-massnahmen-ui.json (tests[0].prueft; Vertrags-
#  namen woertlich aus funktionen[*].zweck und schirm.*). Diese Datei ist der
#  EINZIGE neue Test der Runde T — app/app.R und R/massnahmen.R bleiben
#  unangetastet, die Implementierung (szenario_mit_paket, Massnahmen-Schirm,
#  start_paket_vergleich) folgt in Runde I.
#
#  ROT-ZUERST-Inventar gegen den aktuellen Stand (kalibriert per Quelltext-
#  Suche — KEINES der Vertragsliterale kommt heute in app/app.R vor, auch
#  nicht als Substring; szenario_mit_paket existiert in R/massnahmen.R nicht):
#   - Block 1 (Quelltext-Bindungsorakel): jede Probe faellt rot — weder der
#     PHASEN-Eintrag 'massnahmen' noch Schirm-ids, Span-ids, Erzeugungs-
#     Literale, paket_rv oder die Baustein-Aufrufe stehen im Quelltext.
#   - Block 2 (szenario_mit_paket): die Funktion fehlt — jeder Aufruf steht
#     IM test_that und endet als FAIL (testthat faengt den Error); die
#     expect_error-Proben tragen regexp-Anker (id bzw. 'start_m'), die die
#     heutige could-not-find-Meldung NICHT erfuellt — auch sie sind rot.
#   - Block 3 (Delta-Vorschau-Projektion): BEWUSST GRUENE Basis-Checks,
#     als solche gekennzeichnet — sie nutzen NUR Bestand (massnahmen_overlay)
#     und pinnen die EXTERNE Rechenvorschrift der sechs Vorschau-Spans
#     (median/P_breach_pct/plan_trough, vorher/nachher). Die BINDUNG dieser
#     Vorschrift an vorschau_ui prueft Block 1 (rot, Span-ids fehlen), das
#     DOM-Auslesen Block 4 (skip).
#   - Block 4: sichtbarer skip fuer die E2E-Anteile (etablierte Regel).
#
#  FIXTURE-ENTSCHEID (dokumentierte Abweichung von der Rundenorder, die
#  fixtures/Mandant_NEU.xlsx nannte): Block 2 baut Szenarien ueber
#  scenario_from_workbook und braucht eine ENGINE-LADBARE Mappe.
#  Mandant_NEU.xlsx ist die LEERE Onboarding-Vorlage (R/new_mandate.R:
#  "alles Mandantenspezifische ist leer") — load_model weist sie hart zurueck
#  (keine Positionszeilen nach Filterung, Annahmen nicht numerisch); ein
#  Szenariobau darauf bliebe auch NACH Runde I rot und braeche die
#  rot->gruen-Semantik der Spezifikation. Verwendet wird darum
#  fixtures/Mandant_Liquiditaetsmodell_Vorlage.xlsx — DIESELBE Mappe, auf der
#  die phasen_anker-Pruefung (scripts/check_anchors.R: WB) und
#  tests/test_compare_mutgate.R den Szenariobau pruefen. Nur load_model,
#  KEIN Monte-Carlo, kein RNG.
#
#  Orakel-Politik: EXTERNE Orakel statt Selbstkonsistenz — szenario_mit_paket
#  wird gegen GETRENNT gerufene Referenzen gehalten (quantifiziere_paket(
#  paket, H)$treiber_overrides, szenario_name_mit_paket(basis_name, paket),
#  unveraenderte Basis via scenario_from_workbook(WB)); die Vorschau-
#  Projektion gegen selbst per cumsum/Schleife gebaute Erwartungen,
#  unabhaengig von massnahmen_overlay. Kein succeed(), keine Anker-/Tier-1-
#  Literale, kein Browser/shinytest2, keine Formatierungs-Pins (fmt/%);
#  Formatierung ist laut Vertrag Sache des Renders.
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
source(file.path(.root, "R", "massnahmen.R"))

# Engine-ladbare Fixture-Mappe (Begruendung im Kopf: FIXTURE-ENTSCHEID).
WB <- file.path(.root, "fixtures", "Mandant_Liquiditaetsmodell_Vorlage.xlsx")

# app/app.R NUR als QUELLTEXT (readLines) — die Bindungsorakel in Block 1
# pruefen Literale im Quelltext; die App wird hier weder gesourct noch
# gestartet (die deterministische Render-Pruefung des Schirms uebernimmt der
# Phasen-Anker, die E2E-Anteile Block 4). Darum auch KEINE Daemon-Hygiene
# noetig: ohne source(app/app.R) startet nichts.
.app_src <- paste(readLines(file.path(.root, "app", "app.R"),
                            warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")

# ===========================================================================
#  (1) Quelltext-Bindungsorakel gegen app/app.R — ALLE Proben rot zuerst.
#  Grosse Treffer-Strings nie via expect_match auf .app_src (der Fehlerausweis
#  druckte sonst die ganze App) — stattdessen grepl + expect_true mit Label.
# ===========================================================================

# PHASEN-Vektorsegment: von 'PHASEN <- c(' bis zur schliessenden Klammer —
# die Paar-Probe muss IM Menue-Vektor treffen, nicht irgendwo in der Datei
# (Segment-Eingrenzung nach dem Muster von tests/test_new_mandate_ui.R).
.phasen_segment <- function(src) {
  p <- regexpr("PHASEN\\s*<-\\s*c\\(", src)
  if (p < 0) return(NA_character_)
  rest <- substr(src, p, nchar(src))
  substr(rest, 1L, regexpr(")", rest, fixed = TRUE))
}

test_that("PHASEN traegt das Menue-Paar 'Massnahmen' -> 'massnahmen' (schirm.menue_wert)", {
  seg <- .phasen_segment(.app_src)
  expect_false(is.na(seg),
               label = "app/app.R traegt einen PHASEN <- c(...)-Vektor (Segment-Anker der Paar-Probe)")
  expect_match(seg, '"Massnahmen"\\s*=\\s*"massnahmen"',
               info = "das Paar \"Massnahmen\" = \"massnahmen\" steht IM PHASEN-Vektor")
})

test_that("alle fuenf Schirm-output-ids stehen woertlich im Quelltext und sind serverseitig gebunden (output$<id>)", {
  ids <- c("massnahmen_info", "massnahmen_katalog_tbl", "massnahmen_paket_ui",
           "vorschau_ui", "paket_vergleich_ui")

  da <- vapply(ids, function(id) grepl(id, .app_src, fixed = TRUE), logical(1L))
  expect_true(all(da),
              label = paste0("output-ids woertlich in app/app.R (fehlend: ",
                             paste(ids[!da], collapse = ", "), ")"))

  gebunden <- vapply(ids, function(id)
    grepl(paste0("output$", id), .app_src, fixed = TRUE), logical(1L))
  expect_true(all(gebunden),
              label = paste0("serverseitige Bindung output$<id> (fehlend: ",
                             paste(ids[!gebunden], collapse = ", "), ")"))
})

test_that("Kennzahl-dom-id massnahmen-katalog-n und die sechs Vorschau-Span-ids stehen woertlich im Quelltext", {
  span_ids <- c("massnahmen-katalog-n",
                "vorschau-trough-median-vorher", "vorschau-trough-median-nachher",
                "vorschau-p-breach-vorher",      "vorschau-p-breach-nachher",
                "vorschau-plan-trough-vorher",   "vorschau-plan-trough-nachher")
  da <- vapply(span_ids, function(id) grepl(id, .app_src, fixed = TRUE), logical(1L))
  expect_true(all(da),
              label = paste0("Kennzahl-/Vorschau-Span-ids woertlich in app/app.R (fehlend: ",
                             paste(span_ids[!da], collapse = ", "), ")"))
})

test_that("Bedien-ids und Erzeugungs-Literale des Input-Vertrags stehen woertlich im Quelltext", {
  # Die zwei actionButton-ids des Paket-Vergleichs.
  knoepfe <- c("paket_vergleich_start", "paket_vergleich_abbruch")
  da_k <- vapply(knoepfe, function(id) grepl(id, .app_src, fixed = TRUE), logical(1L))
  expect_true(all(da_k),
              label = paste0("actionButton-ids (fehlend: ",
                             paste(knoepfe[!da_k], collapse = ", "), ")"))

  # Praefix/Suffixe der dynamischen Input-ids plus die verlauf-Choices: als
  # STRING-Literale erzeugt (paste0/sprintf) — die Substring-Probe ist
  # erzeugungsform-agnostisch. Kalibriert: KEINER dieser Substrings kommt
  # heute in app/app.R vor (auch nicht '_start' in anderen ids) — rot zuerst.
  lits <- c("mn_", "_delta", "_betrag", "_start", "_dauer", "_verlauf",
            "einmalig", "linear")
  da_l <- vapply(lits, function(l) grepl(l, .app_src, fixed = TRUE), logical(1L))
  expect_true(all(da_l),
              label = paste0("Erzeugungs-Literale des Input-Vertrags (fehlend: ",
                             paste(lits[!da_l], collapse = ", "), ")"))
})

test_that("Stub-Vertrag paket_rv und die vier Baustein-AUFRUFE stehen im Quelltext", {
  expect_true(grepl("paket_rv", .app_src, fixed = TRUE),
              label = "reactiveValues-Name paket_rv (Stub-Vertrag fuer kuenftige Bindungsorakel, Muster vergleich_rv)")

  # Als AUFRUF gebunden (name gefolgt von oeffnender Klammer), nicht bloss
  # als Wortnennung in einem Kommentar.
  bausteine <- c("szenario_mit_paket", "quantifiziere_paket",
                 "massnahmen_overlay", "vergleich_from_outs")
  da <- vapply(bausteine, function(f)
    grepl(paste0("\\b", f, "\\s*\\("), .app_src), logical(1L))
  expect_true(all(da),
              label = paste0("Baustein-Aufrufe f(...) in app/app.R (fehlend: ",
                             paste(bausteine[!da], collapse = ", "), ")"))
})

# ===========================================================================
#  (2) Vertragstests szenario_mit_paket — EXTERNE Orakel auf der Fixture
#  (nur load_model, kein MC). Rot zuerst: die Funktion existiert noch nicht;
#  jeder Aufruf steht IM test_that, testthat faengt den Error als FAIL.
# ===========================================================================

# Synthetisches Paket: ZWEI treiber-Eintraege auf VERSCHIEDENEN Treibern plus
# EIN delta-Eintrag (keine Tier-1-Werte, keine Anker-Literale). Der energie-
# Wert ist BEWUSST krumm (-0.0173): der Mappen-Default assume$energie der
# Fixture ist -0.05 — ein identischer Override machte den Waechter unten und
# damit den Exakt-Check blind.
.paket <- list(
  list(id = "preiserhoehung",    params = list(delta = 0.02)),
  list(id = "energieeinsparung", params = list(delta = -0.0173)),
  list(id = "kurzarbeit",
       params = list(betrag = 90000, start_m = 2L, dauer = 3L, verlauf = "linear"))
)

test_that("szenario_mit_paket: liqui_scenario, Name extern via szenario_name_mit_paket, assume-Treue an BEIDEN Treibern, Rest identical zur Basis, delta NICHT in den assumes", {
  basis <- scenario_from_workbook(WB)          # unveraenderte Basis als Referenz
  H     <- basis$mdl$HMAX
  q     <- quantifiziere_paket(.paket, H)      # GETRENNT gerufene Referenz

  # Regime-Waechter als HARTE Fails (Muster test_compare_mutgate): entspraeche
  # der Basis-Default schon dem Override, waere der Exakt-Check unten blind;
  # ohne delta-Anteil im Paket waere die Nicht-in-assumes-Probe leer.
  expect_identical(length(q$treiber_overrides), 2L)
  for (k in names(q$treiber_overrides))
    expect_false(isTRUE(all.equal(basis$assume[[k]], q$treiber_overrides[[k]],
                                  tolerance = 1e-12)),
                 label = sprintf("Waechter: Basis-Default assume$%s unterscheidet sich vom Override (sonst blindes Orakel)", k))
  expect_true(any(q$delta_monat != 0),
              label = "Waechter: das Paket traegt einen nicht-trivialen delta-Anteil (sonst ist die Nicht-in-assumes-Probe leer)")

  sc <- szenario_mit_paket(WB, .paket, "Basis")   # <- existiert noch nicht: rot

  expect_true(inherits(sc, "liqui_scenario"),
              label = "Rueckgabe ist ein liqui_scenario (Muster test_compare_mutgate)")
  expect_identical(sc$name, szenario_name_mit_paket("Basis", .paket))

  # Override greift EXAKT an BEIDEN belegten Treibern — in BEIDEN Quellen
  # (assume ist der Cache-Schluessel, mdl$assume rechnet; Muster
  # test_compare_mutgate Block 2).
  for (k in names(q$treiber_overrides)) {
    expect_identical(sc$assume[[k]],     q$treiber_overrides[[k]], info = k)
    expect_identical(sc$mdl$assume[[k]], q$treiber_overrides[[k]], info = k)
  }

  # Der delta-Anteil laeuft NICHT in die assumes: exakt die Basis-Namen,
  # kein Eintrag kommt hinzu, keiner faellt weg.
  expect_identical(names(sc$assume),     names(basis$assume))
  expect_identical(names(sc$mdl$assume), names(basis$mdl$assume))

  # Alle UEBRIGEN Annahmen identical zur Basis — der Helfer trifft nur die
  # belegten Treiber, nichts sonst.
  rest <- setdiff(names(basis$assume), names(q$treiber_overrides))
  expect_identical(sc$assume[rest],     basis$assume[rest])
  expect_identical(sc$mdl$assume[rest], basis$mdl$assume[rest])
})

test_that("szenario_mit_paket: Namens-Zweitfall mit anderem basis_name; Default basis_name = 'Basis'", {
  sc2 <- szenario_mit_paket(WB, .paket, "Stress 2026")   # <- fehlt noch: rot
  expect_identical(sc2$name, szenario_name_mit_paket("Stress 2026", .paket))

  # Signatur-Pin: ohne drittes Argument gilt basis_name = "Basis".
  sc0 <- szenario_mit_paket(WB, .paket)
  expect_identical(sc0$name, szenario_name_mit_paket("Basis", .paket))
})

test_that("szenario_mit_paket: Fehler aus quantifiziere_paket propagieren unveraendert (unbekannte id MIT id im Text; start_m > H der Mappe)", {
  # Die regexp-Anker sind der Rot-Garant: die heutige could-not-find-Meldung
  # ('szenario_mit_paket') traegt weder die Paket-id noch 'start_m' — beide
  # Proben fallen gegen den aktuellen Stand rot.
  expect_error(
    szenario_mit_paket(WB, list(list(id = "diese_id_gibt_es_nicht",
                                     params = list(delta = 0.01))), "Basis"),
    regexp = "diese_id_gibt_es_nicht")

  # H-Validierung an der MAPPE: start_m = HMAX + 1 ist ausserhalb 1..H —
  # beweist, dass der Helfer das H der Mappe (mdl$HMAX) an
  # quantifiziere_paket durchreicht.
  basis <- scenario_from_workbook(WB)
  expect_error(
    szenario_mit_paket(WB, list(list(
      id = "kurzarbeit",
      params = list(betrag = 100, start_m = basis$mdl$HMAX + 1L, dauer = 1L,
                    verlauf = "einmalig"))), "Basis"),
    regexp = "start_m")
})

# ===========================================================================
#  (3) Delta-Vorschau-Projektion am synthetischen out-Stub — BEWUSST GRUENE
#  Basis-Checks (Kennzeichnung laut Rundenorder): sie nutzen NUR Bestand
#  (massnahmen_overlay) und pinnen die EXTERNE Rechenvorschrift der sechs
#  Vorschau-Spans, die Runde I in vorschau_ui bindet; die Bindung selbst ist
#  in Block 1 rot (Span-ids fehlen), das DOM liest Block 4 (skip).
#  Muster .mk_overlay_out aus tests/test_massnahmen.R NACHGEBAUT — die
#  committete Spezifikationsdatei bleibt unveraendert; EIGENE Zahlen, keine
#  Tier-1-Werte, synthetisches kk_limit.
# ===========================================================================

.mk_vorschau_out <- function() {
  B <- rbind(c( 100, -250,  -80, 220),
             c( 450,  120,  380, 260),
             c( -40,  600,  550, -30))
  dimnames(B) <- list(NULL, paste0("M", seq_len(ncol(B))))
  trough <- apply(B, 1, min)
  list(
    phase           = "simulation",
    scenario        = "VorschauStub",
    B               = B,
    Theta           = matrix(c(0.02, -0.01, 0.015, -0.005, 0.03, 0.01),
                             nrow = 3, dimnames = list(NULL, c("menge", "preis"))),
    fan             = apply(B, 2, quantile, probs = c(.05, .25, .5, .75, .95)),
    P_under         = colMeans(B < 0),
    trough          = trough,
    metrics         = list(
      S             = 3L,
      seed          = 4242L,
      H             = 4L,
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95)),
      P_breach      = 1 / 3,
      P_breach_pct  = 100 / 3,
      P_solvent     = 2 / 3,
      P_solvent_pct = 200 / 3,
      plan_trough   = -500
    ),
    spec            = list(menge = list(cv = 0.1)),
    R               = diag(2),
    plot_path       = NA_character_,
    endbestand_path = NA_character_,
    diagnostics     = list(n_treiber = 2L, deterministisch = TRUE, stuetzen_ok = TRUE)
  )
}

# Die drei Vorschau-Kennzahlen EXAKT nach schirm-Vertrag, als WERTE (die
# Formatierung fmt/%.1f ist Sache des Renders): trough_median =
# median(trough); P_breach_pct = 100 * mean(trough < -kk_limit) — der
# kk_limit-Kennzahlenpfad des Deckungs-Schirms; plan_trough aus metrics.
.vorschau_kennzahlen <- function(trough, plan_trough, kk_limit) {
  list(trough_median = median(trough),
       p_breach_pct  = 100 * mean(trough < -kk_limit),
       plan_trough   = plan_trough)
}

.kk_limit_syn   <- 150                       # synthetisch, KEIN Modellwert
.delta_vorschau <- c(-120, 260, -420, 60)    # Vorzeichenwechsel; kum != delta

test_that("Delta-Vorschau: drei Kennzahlen vorher/nachher — cumsum-Orakel unabhaengig von massnahmen_overlay == Projektion auf massnahmen_overlay (1e-12) [BEWUSST GRUEN: nur Bestand]", {
  out <- .mk_vorschau_out()
  kum <- cumsum(.delta_vorschau)             # externes Orakel

  # Design-Waechter als harte Fails (Muster test_massnahmen.R): Vorzeichen-
  # wechsel im delta, kum != delta (kum-vs-punktuell unterscheidbar), Zeilen
  # und troughs paarweise unverwechselbar.
  expect_true(min(.delta_vorschau) < 0 && max(.delta_vorschau) > 0)
  expect_false(identical(kum, .delta_vorschau))
  expect_identical(nrow(unique(out$B)), 3L)
  expect_identical(anyDuplicated(out$trough), 0L)

  # nachher UNABHAENGIG von massnahmen_overlay: cumsum-Orakel per Schleife.
  Berw <- out$B
  for (m in seq_along(kum)) Berw[, m] <- out$B[, m] + kum[m]
  terw <- apply(Berw, 1, min)

  vorher   <- .vorschau_kennzahlen(out$trough, out$metrics$plan_trough, .kk_limit_syn)
  nach_ext <- .vorschau_kennzahlen(terw, min(out$metrics$plan_trough + kum), .kk_limit_syn)

  # Dieselbe Projektion auf massnahmen_overlay(stub, delta) — der Pfad, den
  # vorschau_ui laut Vertrag nimmt (res$trough, res$metrics$plan_trough).
  res      <- massnahmen_overlay(out, .delta_vorschau)
  nach_ovl <- .vorschau_kennzahlen(res$trough, res$metrics$plan_trough, .kk_limit_syn)

  expect_equal(nach_ovl$trough_median, nach_ext$trough_median, tolerance = 1e-12)
  expect_equal(nach_ovl$p_breach_pct,  nach_ext$p_breach_pct,  tolerance = 1e-12)
  expect_equal(nach_ovl$plan_trough,   nach_ext$plan_trough,   tolerance = 1e-12)

  # Nicht-Trivialitaets-Waechter als HARTE Fails: alle drei Kennzahlen
  # aendern sich — ein Stub/delta-Paar, das eine Kennzahl unbewegt laesst,
  # machte die vorher/nachher-Anzeige fuer diese Kennzahl unpruefbar.
  expect_false(isTRUE(all.equal(nach_ext$trough_median, vorher$trough_median, tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(nach_ext$p_breach_pct,  vorher$p_breach_pct,  tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(nach_ext$plan_trough,   vorher$plan_trough,   tolerance = 1e-12)))
})

test_that("Delta-Vorschau: Neutralitaets-Gegenprobe — Nullvektor-delta laesst alle drei Kennzahlen identisch zum Stub [BEWUSST GRUEN: nur Bestand]", {
  out  <- .mk_vorschau_out()
  res0 <- massnahmen_overlay(out, numeric(ncol(out$B)))
  n0   <- .vorschau_kennzahlen(res0$trough, res0$metrics$plan_trough, .kk_limit_syn)
  v    <- .vorschau_kennzahlen(out$trough,  out$metrics$plan_trough,  .kk_limit_syn)
  expect_identical(n0, v)
})

# ===========================================================================
#  (3b) Mutgate-Nachschub phase-11-massnahmen-ui — ZWEI Survivor in app/app.R:
#   - M4 (Paket-Reactive, app/app.R ~1129): start_m und dauer in der params-
#     Zuweisung vertauschbar — ueberlebte, weil kein Test das
#     massnahmen_paket-Reactive mit gesetzten Inputs AUSFUEHRTE.
#   - M5 (vorschau_ui-Render, app/app.R ~1167): P-breach-Vergleich
#     trough > -kk statt trough < -kk (komplementaere Bruchwahrscheinlichkeit)
#     — ueberlebte, weil Block 3 die Rechenvorschrift nur EXTERN auf dem Stub
#     prueft, nie den App-Renderausdruck.
#  Darum hier testServer-Orakel nach dem Muster tests/test_phase_sim_ui.R:
#  app/app.R wird in-Process gesourct (das shinyApp() am Dateiende gibt nur
#  ein Objekt zurueck, gestartet wird nichts), Inputs via session$setInputs,
#  Reactive/Render DIREKT in der server-Umgebung ausgelesen. Orakel bleiben
#  EXTERN (eigene cumsum/mean-Rechnung, massnahmen_overlay ist NICHT das
#  Orakel der nachher-Werte); keine Tier-1-Literale, kein Monte-Carlo.
#  Nebenlaeufigkeits-Hygiene wie im Muster: das Sourcen startet bei HAS_ASYNC
#  mirai::daemons(1) — deterministische Schliessung am Sektionsende (laeuft
#  auch nach rotem test_that, Top-Level-Code).
# ===========================================================================

# Harte Voraussetzung VOR dem Sourcen der App (das Sourcen startet ggf. schon
# Daemons): fehlt xml2, bricht der Test benannt ab — kein stiller Skip.
if (!requireNamespace("xml2", quietly = TRUE))
  stop("Paket 'xml2' fehlt (DOM-Parse des vorschau_ui-HTML): install.packages(\"xml2\")")

# Beim Sourcen gestartete mirai-Daemons schliessen (Suite-Invariante: kein Leak).
.daemons_zu <- function() {
  if ("mirai" %in% loadedNamespaces()) try(mirai::daemons(0), silent = TRUE)
  invisible()
}

# App in-Process laden (Muster test_phase_sim_ui.R); chdir=TRUE laesst
# find_root() der App aus app/ heraus die Repo-Wurzel bestimmen.
.ui_env <- new.env(parent = globalenv())
.ui_src_err <- tryCatch({
  source(file.path(.root, "app", "app.R"), local = .ui_env, chdir = TRUE)
  NULL
}, error = identity)
if (!is.null(.ui_src_err)) {
  .daemons_zu()  # ein halb gesourctes app.R darf keinen Daemon zuruecklassen
  stop("app/app.R laesst sich nicht in-Process sourcen: ",
       conditionMessage(.ui_src_err))
}

.span_text <- function(doc, id) {
  n <- xml2::xml_find_first(doc, sprintf("//*[@id='%s']", id))
  if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
}

test_that("massnahmen_paket-Reactive: params-Felder landen an den RICHTIGEN Schluesseln (Mutgate-Nachschub M4)", {
  shiny::testServer(.ui_env$server, {
    session$setInputs(phase = "massnahmen")  # rv$wbpath bleibt NULL -> die
                                             # Auto-Sim-Trigger bleiben inert.
    # ASYMMETRISCHE Belegung start=2 != dauer=5: ein start_m/dauer-Tausch im
    # Reactive (M4) kann sich nicht hinter gleichen Werten verstecken — der
    # sonst uebliche Design-Waechter (expect_true(2L != 5L)) ist per
    # Konstruktion der Literale erfuellt und entfaellt.
    session$setInputs(
      mn_kurzarbeit = TRUE, mn_kurzarbeit_betrag = 900,
      mn_kurzarbeit_start = 2, mn_kurzarbeit_dauer = 5,
      mn_kurzarbeit_verlauf = "linear",
      mn_preiserhoehung = TRUE, mn_preiserhoehung_delta = 0.02)

    pk  <- massnahmen_paket()                # Reactive DIREKT ausgefuehrt
    ids <- vapply(pk, function(e) e$id, character(1L))

    expect_identical(length(pk), 2L)
    expect_setequal(ids, c("kurzarbeit", "preiserhoehung"))

    # Reihenfolge-tolerant ueber die id adressiert; exakte Schluessel-Struktur
    # im quantifiziere_paket-Vertrag (betrag/start_m/dauer/verlauf bzw. delta).
    kur <- pk[[which(ids == "kurzarbeit")]]$params
    expect_setequal(names(kur), c("betrag", "start_m", "dauer", "verlauf"))
    expect_identical(kur$betrag,  900)
    expect_identical(kur$start_m, 2L)  # aus mn_kurzarbeit_start — M4 lieferte hier 5L
    expect_identical(kur$dauer,   5L)  # aus mn_kurzarbeit_dauer — M4 lieferte hier 2L
    expect_identical(kur$verlauf, "linear")

    pre <- pk[[which(ids == "preiserhoehung")]]$params
    expect_identical(names(pre), "delta")
    expect_identical(pre$delta, 0.02)
  })
})

# Eigener Stub NUR fuer den M5-Block (Muster .mk_vorschau_out, EIGENE Zahlen):
# 5 Pfade x 3 Monate — die Fuenftel-Aufloesung erlaubt Bruchanteile, bei denen
# vorher (20 %), nachher (60 %) und BEIDE Komplemente (80 % / 40 %) paarweise
# verschieden sind; mit 4 Pfaden kollidierte jede Wahl mit 0/25/50/75/100.
# Synthetisches kk_limit, keine Tier-1-Werte; plot_path/endbestand_path = NA
# bedienen die uebrigen Outputs der MockShinySession (rechnet ALLE Outputs).
.mk_render_out <- function() {
  B <- rbind(c(-350,  100, 900),
             c(-150,  -90, 700),
             c(  60, -180, 820),
             c( 500,  450, 620),
             c( 300,  480, 940))
  dimnames(B) <- list(NULL, paste0("M", seq_len(ncol(B))))
  trough <- apply(B, 1, min)
  list(
    phase           = "simulation",
    scenario        = "RenderStub",
    B               = B,
    Theta           = matrix(seq(-0.02, 0.025, by = 0.005),
                             nrow = 5, dimnames = list(NULL, c("menge", "preis"))),
    fan             = apply(B, 2, quantile, probs = c(.05, .25, .5, .75, .95)),
    P_under         = colMeans(B < 0),
    trough          = trough,
    metrics         = list(
      S             = 5L,
      seed          = 707L,
      H             = 3L,
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95)),
      P_breach      = 1 / 5,
      P_breach_pct  = 100 / 5,
      P_solvent     = 4 / 5,
      P_solvent_pct = 400 / 5,
      plan_trough   = -450
    ),
    spec            = list(menge = list(cv = 0.1)),
    R               = diag(2),
    plot_path       = NA_character_,
    endbestand_path = NA_character_,
    diagnostics     = list(n_treiber = 2L, deterministisch = TRUE, stuetzen_ok = TRUE)
  )
}

test_that("vorschau_ui: P-breach-Prozente im gerenderten HTML entsprechen der externen Rechnung 100*mean(trough < -kk) (Mutgate-Nachschub M5)", {
  stub   <- .mk_render_out()
  kk_syn <- 200  # synthetisch; belegt unten GENAU die Render-Quelle rv$kk_limit
                 # (vorschau_ui liest kk <- rv$kk_limit, die beim Mappe-Laden
                 # gesicherte Modell-Schwelle des Deckungs-Schirms)

  # delta-Anteil KOMPLETT von Hand (Massnahme unten: kurzarbeit, einmalig
  # -600 in M2) — weder quantifiziere_paket noch massnahmen_overlay dienen
  # als Orakel; kum/troughs/plan_trough per eigener cumsum/Schleife.
  delta_hand <- c(0, -600, 0)
  kum_hand   <- cumsum(delta_hand)
  Berw <- stub$B
  for (m in seq_along(kum_hand)) Berw[, m] <- stub$B[, m] + kum_hand[m]
  terw <- apply(Berw, 1, min)

  p_vor  <- 100 * mean(stub$trough < -kk_syn)
  p_nach <- 100 * mean(terw        < -kk_syn)

  # Design-Waechter als HARTE Fails: vorher weder 0 noch 50 noch 100 (bei 50
  # zeigte die komplementaere Mutante denselben String, bei 0/100 degeneriert
  # die Verteilung); nachher vom vorher verschieden UND von BEIDEN
  # Komplementen verschieden (auch eine nur-nachher- oder beidseitige
  # Mutation faellt rot); keine Grenzfall-Gleichheit trough == -kk (die
  # Vergleichsrichtung entscheidet wirklich); Median und Plan-Tiefpunkt
  # bewegen sich (vorher/nachher-Spans unterscheidbar).
  expect_false(p_vor  %in% c(0, 50, 100))
  expect_false(p_nach %in% c(0, 50, 100))
  expect_false(isTRUE(all.equal(p_nach, p_vor)))
  expect_false(isTRUE(all.equal(p_nach, 100 - p_vor)))
  expect_true(all(stub$trough != -kk_syn) && all(terw != -kk_syn))
  expect_false(isTRUE(all.equal(median(terw), median(stub$trough))))
  expect_false(isTRUE(all.equal(min(stub$metrics$plan_trough + kum_hand),
                                stub$metrics$plan_trough)))

  # fmt aus der Aufloesungskette der gesourcten App (Muster
  # test_phase_sim_ui.R) — die Erwartung ist exakt die App-Darstellung.
  fmt_app <- get("fmt", envir = .ui_env)

  shiny::testServer(.ui_env$server, {
    session$setInputs(phase = "massnahmen")
    session$setInputs(
      mn_kurzarbeit = TRUE, mn_kurzarbeit_betrag = -600,
      mn_kurzarbeit_start = 2, mn_kurzarbeit_dauer = 1,
      mn_kurzarbeit_verlauf = "einmalig")
    sim_rv$out  <- stub     # der VORHANDENE Simulationsstand der Vorschau
    rv$kk_limit <- kk_syn   # GENAU die kk-Quelle des vorschau-Renders
    session$flushReact()

    ui   <- output$vorschau_ui             # renderUI: list(html, deps)
    html <- if (is.list(ui)) ui$html else ui
    doc  <- xml2::read_html(as.character(html))

    # DER M5-Kill: der gerenderte vorher-Prozentwert ist sprintf('%.1f',
    # 100*mean(trough < -kk)) — die Mutante (trough > -kk) zeigte das
    # Komplement (80.0 statt 20.0 bzw. 40.0 statt 60.0 im nachher).
    expect_identical(.span_text(doc, "vorschau-p-breach-vorher"),
                     sprintf("%.1f", p_vor))
    expect_identical(.span_text(doc, "vorschau-p-breach-nachher"),
                     sprintf("%.1f", p_nach))

    # Flankierend (das Muster gibt es her): Median- und Plan-Tiefpunkt-Spans
    # gegen fmt()-Erwartungen aus der Hand-Rechnung — bindet auch die
    # uebrigen vier Span-ids semantisch an ihre Quellfelder.
    expect_identical(.span_text(doc, "vorschau-trough-median-vorher"),
                     fmt_app(median(stub$trough)))
    expect_identical(.span_text(doc, "vorschau-trough-median-nachher"),
                     fmt_app(median(terw)))
    expect_identical(.span_text(doc, "vorschau-plan-trough-vorher"),
                     fmt_app(stub$metrics$plan_trough))
    expect_identical(.span_text(doc, "vorschau-plan-trough-nachher"),
                     fmt_app(min(stub$metrics$plan_trough + kum_hand)))
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem test_that erreicht): die beim
# Sourcen der App gestarteten mirai-Daemons schliessen — kein Prozess-Leak.
.daemons_zu()

# ===========================================================================
#  (4) E2E-Anteile — SICHTBARER skip (etablierte Regel: kein Browser-Zwang;
#  die deterministische Render-Pruefung des Schirms uebernimmt der
#  Phasen-Anker massnahmen_info/massnahmen_katalog_tbl).
# ===========================================================================

test_that("E2E Massnahmen-Schirm: Reiterklick, DOM-Auslesen der Vorschau-Spans, Worker-Lauf (sichtbarer skip)", {
  skip(paste("shinytest2-Infrastruktur fehlt:",
             "echter Reiterklick auf den Massnahmen-Schirm, DOM-Auslesen der",
             "sechs Vorschau-Spans (vorschau-*-vorher/-nachher) und der",
             "mirai-Worker-Lauf ueber paket_vergleich_start /",
             "paket_vergleich_abbruch brauchen einen Browser-Treiber;",
             "die deterministische Render-Pruefung (Katalog-Kennzahl",
             "massnahmen-katalog-n) uebernimmt der Phasen-Anker."))
})
