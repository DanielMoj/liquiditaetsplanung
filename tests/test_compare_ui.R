# ============================================================================
#  tests/test_compare_ui.R  —  Render-Bindungs-Orakel (ROT ZUERST) fuer den
#  Szenario-Vergleichs-Schirm (phase-08-szenarienvergleich)
#
#  ROT-ZUERST gegen den KOMMENDEN Schirm: app/app.R kennt weder die server-
#  internen reactiveValues vergleich_rv (Feld out — Vertragsname aus
#  handoff/phase-08-szenarienvergleich.json, damit dieser Test stubben kann)
#  noch die Outputs vergleich_info/vergleich_tbl — die Zuweisung
#  vergleich_rv$out bzw. der Output-Zugriff im testServer bricht ab. Der
#  Builder erfuellt den Test erst mit dem Schirm. Kein succeed(), keine
#  Existenz-Weiche; der einzige skip ist das sichtbar begruendete
#  xml2-Tooling.
#
#  Geprueft wird die SEMANTISCHE Bindung der Anzeige (Muster
#  test_phase_sim_deckung_ui.R bzw. Block 2 von test_phase_var_mutgate.R —
#  der Verifier prueft IDs nur auf Format/Befuelltheit, nicht auf die
#  Bindung DOM -> Quellfeld):
#   (a) der Kennzahl-Span vergleich-n (im uiOutput vergleich_info) zeigt
#       exakt n (Anzahl Szenarien, Integer als Text, sprintf %d);
#   (b) das gerenderte vergleich_tbl-HTML traegt Zeile 1 mit den fmt-/
#       Prozent-formatierten Werten von tabelle-Zeile 1 und Zeile 2 mit
#       denen von Zeile 2 — Erwartungen UNABHAENGIG aus dem Stub gerechnet
#       (fmt aus der App-Umgebung, sprintf %.1f%% fuer die Prozente),
#       expliziter Kreuz-Check gegen Zeilentausch;
#   (c) die insO-Spalte zeigt "ja" fuer TRUE und den Nicht-ja-Marker
#       (Em-Dash —) fuer FALSE — auch am Rand P_solvent == 0.5 (FALSE).
#  Ein zweiter Flush mit veraendertem Stub (drei Zeilen, andere Werte,
#  gekippte insO-Belegung) beweist, dass Re-Render UND Kennzahl dem Objekt
#  folgen (kein eingefrorenes HTML, kein eingefrorenes n).
#
#  Zeilenvertrag der gerenderten Tabelle (je Szenario, in dieser Reihenfolge):
#    Szenario | plan_trough | trough_median | trough_p05 | trough_p95
#    (EUR ueber fmt()) | P_breach_pct | P_solvent_pct (eine Nachkommastelle,
#    sprintf %.1f%%) | insO_erfuellt als ja/—. Numerische Quelle ist
#    vergleich_rv$out$tabelle — die Formatierung passiert erst im Render.
#
#  testServer-Basis (kein Browser, keine Monte-Carlo): app/app.R wird
#  in-Process gesourct, vergleich_rv$out mit einem synthetischen
#  compare_scenarios-Stub belegt (paarweise unverwechselbare Werte, KEINE
#  Tier-1-Werte). Nebenlaeufigkeits-Hygiene wie in den Schwester-Orakeln:
#  das Sourcen von app/app.R startet bei HAS_ASYNC mirai::daemons(1); der
#  onStop-Handler feuert unter testServer nie. Die Daemons werden am
#  Dateiende deterministisch geschlossen — auch im Skip- und Fehlerpfad.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}

# xml2 (DOM-Parse des gerenderten HTML) ist reines Test-Tooling, keine App-
# Abhaengigkeit: fehlt es, wird der Test SICHTBAR uebersprungen (skip_if mit
# Begruendung, im test_that unten) — kein harter stop(), der den DoD bricht,
# und kein stiller Skip.

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

# Die tbody-Zeilen der gerenderten Tabelle als Liste von Zell-Textvektoren.
.tbody_rows <- function(html) {
  doc  <- xml2::read_html(as.character(html))
  rows <- xml2::xml_find_all(doc, "//tbody/tr")
  lapply(rows, function(r) trimws(xml2::xml_text(xml2::xml_find_all(r, ".//td"))))
}

# Synthetische Vergleichstabelle im vergleich_from_outs-Spaltenvertrag:
# P_solvent/*_pct/insO_erfuellt werden KONSISTENT aus P_breach abgeleitet
# (strikt > 0.5), damit der Stub beide denkbaren Render-Quellen gleich haelt.
# KEINE Engine, kein Seed-Lauf, KEINE Tier-1-Werte.
.mk_tabelle <- function(szenario, plan_trough, trough_median, trough_p05,
                        trough_p95, P_breach, kk_limit, S, seed) {
  data.frame(
    szenario      = szenario,
    plan_trough   = plan_trough,
    trough_median = trough_median,
    trough_p05    = trough_p05,
    trough_p95    = trough_p95,
    P_breach      = P_breach,
    P_solvent     = 1 - P_breach,
    P_breach_pct  = 100 * P_breach,
    P_solvent_pct = 100 * (1 - P_breach),
    insO_erfuellt = (1 - P_breach) > 0.5,
    kk_limit      = kk_limit,
    S             = S,
    seed          = seed,
    stringsAsFactors = FALSE
  )
}
.mk_cmp_out <- function(tabelle) {
  list(phase = "szenarienvergleich", n = nrow(tabelle), tabelle = tabelle)
}

test_that("vergleich_tbl/vergleich-n binden Zeile fuer Zeile an GENAU ihre tabelle-Zeile (Zeilentausch faellt rot)", {
  # xml2 ist reines Test-Tooling (DOM-Parse des gerenderten HTML) — fehlt es,
  # sichtbar ueberspringen statt den Build zu brechen.
  skip_if(!requireNamespace("xml2", quietly = TRUE),
          "Paket 'xml2' fehlt (DOM-Parse des vergleich_tbl-HTML): install.packages(\"xml2\")")

  # Stub A: zwei Zeilen, alle Werte paarweise unverwechselbar (EUR-Betraege,
  # Prozente 12.5/87.5 vs 67.5/32.5, Namen, kk/S/seed) — insO TRUE in Zeile 1
  # (P_solvent 0.875), FALSE in Zeile 2 (0.325).
  tb_a <- .mk_tabelle(
    szenario      = c("StubAlpha", "StubBeta"),
    plan_trough   = c(-1234500, -3456700),
    trough_median = c( -234500,  -543200),
    trough_p05    = c( -876500,  -987600),
    trough_p95    = c(  345600,   123400),
    P_breach      = c(0.125, 0.675),
    kk_limit      = c(250000, 300000),
    S             = c(24L, 48L),
    seed          = c(4711L, 815L)
  )
  # Stub B (zweiter Flush): DREI Zeilen mit anderen Werten und gekippter
  # insO-Belegung (FALSE/TRUE/FALSE) — Zeile 3 pinnt den Rand P_solvent
  # exakt 0.5 -> Nicht-ja-Marker. Beweist Re-Render UND n-Bindung.
  tb_b <- .mk_tabelle(
    szenario      = c("StubGamma", "StubDelta", "StubEpsilon"),
    plan_trough   = c(-111100, -555500, -905000),
    trough_median = c( -22200,  -66600, -101000),
    trough_p05    = c(-333300, -777700, -650500),
    trough_p95    = c(  44400,   88800,   70700),
    P_breach      = c(0.925, 0.045, 0.5),
    kk_limit      = c(150000, 175000, 200000),
    S             = c(12L, 36L, 60L),
    seed          = c(99L, 4242L, 512L)
  )
  stub_a <- .mk_cmp_out(tb_a)
  stub_b <- .mk_cmp_out(tb_b)

  # fmt aus der Aufloesungskette der gesourcten App (app-Env -> Kernel), nicht
  # nachgebaut — die Erwartung ist exakt die Darstellung, die die App rendert.
  fmt_app <- get("fmt", envir = .app_env)

  # Erwartete Zell-Textzeile i, UNABHAENGIG aus dem Stub gerechnet (fmt fuer
  # die EUR-Spalten, sprintf %.1f%% fuer die Prozente, ja/— fuer insO).
  .erw_zeile <- function(tb, i) c(
    tb$szenario[i],
    fmt_app(tb$plan_trough[i]),
    fmt_app(tb$trough_median[i]),
    fmt_app(tb$trough_p05[i]),
    fmt_app(tb$trough_p95[i]),
    sprintf("%.1f%%", tb$P_breach_pct[i]),
    sprintf("%.1f%%", tb$P_solvent_pct[i]),
    if (isTRUE(tb$insO_erfuellt[i])) "ja" else "—"
  )

  # Nicht-Trivialitaets-Guard: die beiden erwarteten Zeilen von Stub A sind
  # verschieden — sonst waere der Kreuz-Check gegen Zeilentausch blind.
  expect_false(identical(.erw_zeile(tb_a, 1L), .erw_zeile(tb_a, 2L)),
               label = "Stub-A-Zeilen unverwechselbar (Guard fuer den Kreuz-Check)")

  shiny::testServer(.app_env$server, {
    session$setInputs(phase = "forecast")  # rv$wbpath bleibt NULL -> alle
                                           # Auto-Trigger bleiben inert.
    vergleich_rv$running <- FALSE          # defensiv (Muster var_rv), falls
                                           # der Render einen Lauf-Guard hat
    vergleich_rv$out     <- stub_a         # <- existiert noch nicht: rot zuerst
    session$flushReact()

    # (a) Kennzahl-Span vergleich-n im uiOutput vergleich_info: exakt n.
    ui   <- output$vergleich_info          # renderUI: list(html, deps)
    html <- if (is.list(ui)) ui$html else ui
    doc  <- xml2::read_html(as.character(html))
    expect_identical(.kz_text(doc, "vergleich-n"), sprintf("%d", stub_a$n))

    # (b) Tabelle: Zeile 1 == formatierte tabelle-Zeile 1, Zeile 2 == Zeile 2.
    tbl  <- output$vergleich_tbl           # renderTable: HTML-String
    rows <- .tbody_rows(if (is.list(tbl)) tbl$html else tbl)

    expect_equal(length(rows), 2L)
    expect_identical(rows[[1]], .erw_zeile(tb_a, 1L),
                     label = "Zeile 1 der Vergleichstabelle (Stub A)")
    expect_identical(rows[[2]], .erw_zeile(tb_a, 2L),
                     label = "Zeile 2 der Vergleichstabelle (Stub A)")
    # Expliziter Kreuz-Check gegen Zeilentausch: Zeile 1 traegt NICHT die
    # formatierten Werte von tabelle-Zeile 2.
    expect_false(identical(rows[[1]], .erw_zeile(tb_a, 2L)),
                 label = "Kreuz-Check: Zeile 1 != formatierte tabelle-Zeile 2 (Stub A)")

    # (c) insO-Spalte: "ja" fuer TRUE (Zeile 1), Nicht-ja-Marker fuer FALSE.
    expect_identical(rows[[1]][8L], "ja")
    expect_identical(rows[[2]][8L], "—")

    # Zweiter Flush mit veraendertem Stub: Re-Render folgt dem Objekt —
    # drei Zeilen, neue Werte, gekippte insO-Belegung, neues n.
    vergleich_rv$out <- stub_b
    session$flushReact()

    ui   <- output$vergleich_info
    html <- if (is.list(ui)) ui$html else ui
    doc  <- xml2::read_html(as.character(html))
    expect_identical(.kz_text(doc, "vergleich-n"), sprintf("%d", stub_b$n))

    tbl  <- output$vergleich_tbl
    rows <- .tbody_rows(if (is.list(tbl)) tbl$html else tbl)

    expect_equal(length(rows), 3L)
    expect_identical(rows[[1]], .erw_zeile(tb_b, 1L),
                     label = "Zeile 1 der Vergleichstabelle (Stub B)")
    expect_identical(rows[[2]], .erw_zeile(tb_b, 2L),
                     label = "Zeile 2 der Vergleichstabelle (Stub B)")
    expect_identical(rows[[3]], .erw_zeile(tb_b, 3L),
                     label = "Zeile 3 der Vergleichstabelle (Stub B)")

    # insO-Belegung gekippt: Zeile 2 ist jetzt "ja", Zeile 1 der Marker —
    # und am exakten Rand P_solvent == 0.5 (Zeile 3) gilt der Marker (strikt >).
    expect_identical(rows[[2]][8L], "ja")
    expect_identical(rows[[1]][8L], "—")
    expect_identical(rows[[3]][8L], "—")
  })
})

# Daemon-Hygiene (Top-Level, wird auch nach rotem/uebersprungenem test_that
# erreicht): die beim Sourcen der App gestarteten mirai-Daemons schliessen —
# kein Prozess-Leak.
.daemons_zu()
