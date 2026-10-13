# ============================================================================
#  R/compare_scenarios.R  —  Phase 8 (Szenario-Vergleich)
#
#  Stellt N Szenarien ueber ihre Simulationsergebnisse gegenueber und gibt
#  ein Ergebnisobjekt zurueck, statt zu drucken. KEIN Legacy-Vorbild —
#  fachliche Grundlage ist die Szenario-Abstraktion in R/scenario.R
#  (scenario_from_workbook, assume_overrides, sim$paths/seed).
#
#  Drei Funktionen (Muster deckung_from_out / phase_sim_deckung):
#    * vergleich_from_outs(outs, kk_limits, namen) — REINE Funktion ohne
#      Simulation und ohne Formatierung: eine Zeile je Szenario in
#      EINGABEREIHENFOLGE mit Tiefpunkt-Quantilen (median / quantile .05/.95,
#      unname), Breach-/Solvenz-Wahrscheinlichkeiten (P_breach =
#      mean(trough < -kk_limit), strikt <, wie legacy/05_simulation.R) und
#      InsO-Verdikt (P_solvent > 0.5, STRIKT >, exakt 0.5 -> FALSE;
#      §§ 18/19 InsO). plan_trough/S/seed werden aus out$metrics
#      DURCHGEREICHT — die Tier-1-Planzahl wird nicht neu gerechnet.
#    * compare_scenarios(scs) — Wrapper: alle Eingabevertraege werden VOR der
#      ersten Simulation geprueft; je Szenario GENAU EIN Aufruf
#      phase_simulation(sc) — keine zweite Monte-Carlo, keine Kernel-
#      Aenderung, kein eigenes Rauschen; kk_limit je Szenario aus
#      sc$mdl$kk_limit (Modell-Schwelle der jeweiligen Mappe).
#    * scenarios_from_specs(wbpath, specs, basis_name) — Szenariobau OHNE
#      Simulation (nur load_model via scenario_from_workbook): aus dem
#      Mappenpfad und leichten Spezifikationen die Szenarioliste fuer
#      compare_scenarios. Herausgezogen aus dem mirai-Ausdruck von
#      start_vergleich (app/app.R), damit der Worker-Szenariobau einen
#      direkten Testpfad hat (tests/test_compare_mutgate.R).
#
#  Numerik bleibt numerisch: die EUR-/Prozent-Formatierung (fmt()/sprintf)
#  passiert erst im app-Render (output$vergleich_tbl / vergleich_info).
#
#  Nebenlaeufigkeit: der Wrapper ist synchron und worker-sicher (keine
#  Konsolen-Prints, kein Grafikgeraet — die Laeufe rechnen mit plot = FALSE,
#  das Fan-Chart gehoert dem Simulations-Schirm —, keine Globals). Die N
#  teuren Laeufe laufen im App-Modul im mirai-Worker, nie im UI-Thread; das
#  Ergebnisobjekt liegt dort in den server-internen reactiveValues
#  vergleich_rv (Feld out — Vertragsname aus
#  handoff/phase-08-szenarienvergleich.json fuer das Bindungs-Orakel).
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R, R/scenario.R
#  und R/phase_simulation.R bereits gesourct sind.
# ============================================================================

# vergleich_from_outs: die EINE reine Gegenueberstellung aus VORHANDENEN
# Simulationsergebnissen (phase_simulation-Vertrag). Loest KEINE Simulation
# aus — rechnet nur median/quantile/Anteil auf out$trough und reicht
# plan_trough/S/seed aus out$metrics durch. Eingabevertrag: gleiche Laengen
# >= 1 und eindeutige namen (stopifnot).
vergleich_from_outs <- function(outs, kk_limits, namen) {
  stopifnot(
    is.list(outs),
    length(outs) >= 1L,
    length(kk_limits) == length(outs),
    length(namen)     == length(outs),
    anyDuplicated(namen) == 0L
  )

  # Anteil Pfade, deren Tiefpunkt die KK-Linie des JEWEILIGEN Szenarios
  # reisst (strikt <); alle weiteren Wahrscheinlichkeiten sind Ableitungen.
  P_breach  <- vapply(seq_along(outs), function(i)
    mean(outs[[i]]$trough < -kk_limits[[i]]), numeric(1L))
  P_solvent <- 1 - P_breach

  data.frame(
    szenario      = as.character(namen),
    plan_trough   = vapply(outs, function(o) o$metrics$plan_trough,          numeric(1L)),
    trough_median = vapply(outs, function(o) unname(median(o$trough)),       numeric(1L)),
    trough_p05    = vapply(outs, function(o) unname(quantile(o$trough, .05)), numeric(1L)),
    trough_p95    = vapply(outs, function(o) unname(quantile(o$trough, .95)), numeric(1L)),
    P_breach      = P_breach,
    P_solvent     = P_solvent,
    P_breach_pct  = 100 * P_breach,
    P_solvent_pct = 100 * P_solvent,
    insO_erfuellt = P_solvent > 0.5,   # §§ 18/19 InsO: STRIKT >, 0.5 -> FALSE
    kk_limit      = as.numeric(kk_limits),
    S             = vapply(outs, function(o) as.numeric(o$metrics$S),    numeric(1L)),
    seed          = vapply(outs, function(o) as.numeric(o$metrics$seed), numeric(1L)),
    stringsAsFactors = FALSE
  )
}

compare_scenarios <- function(scs) {
  # Alle Eingabevertraege VOR der ersten (teuren) Simulation: Liste mit
  # mindestens einem liqui_scenario, Szenario-Namen eindeutig.
  stopifnot(
    is.list(scs),
    length(scs) >= 1L,
    all(vapply(scs, inherits, logical(1L), what = "liqui_scenario"))
  )
  namen <- vapply(scs, function(sc) sc$name, character(1L))
  stopifnot(anyDuplicated(namen) == 0L)

  # Je Szenario GENAU EIN phase_simulation-Aufruf (Determinismus haengt am
  # Seed des Szenarios); plot = FALSE haelt den Worker frei von jedem
  # Grafikgeraet — verglichen werden Zahlen, nicht Charts.
  outs      <- lapply(scs, function(sc) phase_simulation(sc, plot = FALSE))
  kk_limits <- vapply(scs, function(sc) sc$mdl$kk_limit, numeric(1L))

  list(
    phase   = "szenarienvergleich",
    n       = length(scs),
    tabelle = vergleich_from_outs(outs, kk_limits, namen)
  )
}

# scenarios_from_specs: Szenariobau OHNE Simulation — baut aus dem Mappenpfad
# und einer Liste leichter Spezifikationen (je Element list(name, treiber,
# wert)) die Szenarioliste fuer compare_scenarios. Element 1 ist die Basis
# (scenario_from_workbook ohne Override), danach je spec ein Szenario mit
# benanntem Ein-Element-Override sp$treiber -> sp$wert; sim bleibt jeweils
# Default — der Seed ist der Szenario-Schluessel, alle Laeufe rechnen mit
# derselben Pfadanzahl. Keine Formatierung, keine Monte-Carlo (nur load_model
# via scenario_from_workbook). Eingabevertrag (stopifnot VOR dem ersten
# load_model): specs ist eine Liste, jedes Element traegt ein character-name,
# ein character-treiber und ein numerisch-endliches wert.
scenarios_from_specs <- function(wbpath, specs, basis_name = "Basis") {
  stopifnot(
    is.list(specs),
    all(vapply(specs, function(sp)
      is.list(sp) &&
        is.character(sp$name)    && length(sp$name)    == 1L &&
        is.character(sp$treiber) && length(sp$treiber) == 1L &&
        is.numeric(sp$wert)      && length(sp$wert)    == 1L && is.finite(sp$wert),
      logical(1L)))
  )

  c(list(scenario_from_workbook(wbpath, name = basis_name)),
    lapply(specs, function(sp) {
      ov <- list(sp$wert); names(ov) <- sp$treiber
      scenario_from_workbook(wbpath, name = sp$name, assume_overrides = ov)
    }))
}
