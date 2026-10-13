# ============================================================================
#  R/phase_sim_deckung.R  —  Phase 6 (Simulation, Einheit phase-06-deckung)
#
#  Projiziert das Simulationsergebnis phase_simulation(sc) auf den Deckungs-/
#  Solvenz-Verdict und gibt ein Ergebnisobjekt zurueck, statt in die Konsole zu
#  drucken. Der Planner hat die urspruengliche Phase 6 wegen Diff-Budget in drei
#  Einheiten geschnitten: phase-06-fan rechnet den VOLLSTAENDIGEN Ergebnis-
#  Vertrag `out` (genestete Monte-Carlo ueber die eingefrorene Engine); diese
#  Schwester-Einheit projiziert dieses `out` NUR auf das Deckungs-/Solvenz-
#  Verdikt — KEINE zweite Monte-Carlo, KEINE Kernel-Aenderung.
#
#  Reine Projektion: phase_sim_deckung ruft den Wrapper phase_simulation(sc)
#  GENAU EINMAL auf (im App-Modul laeuft derselbe Lauf im Hintergrund-Worker)
#  und projiziert dessen trough/metrics:
#    * trough_median / trough_p05 / trough_p95 — Tiefpunkt-Verteilung aus
#      out$trough (median / quantile .05 / .95, unname).
#    * P_breach = Anteil Pfade mit trough < -kk_limit, Limit aus dem MODELL
#      (sc$mdl$kk_limit); P_solvent = 1 - P_breach; *_pct = 100 * Anteil.
#    * insO_erfuellt — §§ 18/19 InsO-Verdict: P_solvent > 0.5 (STRIKT >, reine
#      Schwellenprojektion, kein hartcodiertes Urteil; exakt 0.5 -> FALSE).
#    * kk_limit / seed / S — Modell-Schwelle + Szenario-Schluessel + Pfadanzahl
#      (seed/S aus out$metrics durchgereicht, kein zweites Sample).
#
#  Numerik bleibt numerisch: das Ergebnisobjekt traegt EUR-Quantile und die
#  Wahrscheinlichkeiten als reine Zahlen (fuer Tests/Anker). Die EUR-
#  Formatierung ueber fmt() und die Prozent-Anzeige via sprintf passieren erst
#  im app-Render.
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R, R/scenario.R und
#  R/phase_simulation.R bereits gesourct sind.
# ============================================================================

# deckung_from_out: die EINE reine Projektion aus einem VORHANDENEN Simulations-
# ergebnis `out` (phase_simulation-Vertrag) auf die Deckungs-Kennzahlen. Loest
# KEINE Simulation aus — rechnet nur median/quantile/Anteil auf out$trough und
# reicht seed/S aus out$metrics durch. Sowohl phase_sim_deckung() als auch der
# App-Render (output$deckung_recon in app/app.R) setzen auf diese Funktion auf,
# damit Vertrag und Anzeige nicht auseinanderdriften (eine Quelle der Projektion).
#
#   * trough_median/p05/p95: median / quantile(.05) / quantile(.95) von
#     out$trough, EUR, numerisch. unname(), weil quantile() benannt liefert.
#   * P_breach = mean(trough < -kk_limit) (strikt <, wie legacy/05_simulation.R);
#     P_solvent = 1 - P_breach; *_pct = 100 * Anteil.
#   * insO_erfuellt = P_solvent > 0.5 — §§ 18/19 InsO, STRIKT > (0.5 -> FALSE).
# Die EUR-/Prozent-Formatierung (fmt()/sprintf) passiert erst im app-Render.
deckung_from_out <- function(out, kk_limit) {
  trough    <- out$trough
  P_breach  <- mean(trough < -kk_limit)
  P_solvent <- 1 - P_breach

  list(
    trough_median = unname(median(trough)),
    trough_p05    = unname(quantile(trough, .05)),
    trough_p95    = unname(quantile(trough, .95)),
    P_breach      = P_breach,
    P_solvent     = P_solvent,
    P_breach_pct  = 100 * P_breach,
    P_solvent_pct = 100 * P_solvent,
    insO_erfuellt = P_solvent > 0.5,
    kk_limit      = kk_limit,
    seed          = out$metrics$seed,
    S             = out$metrics$S
  )
}

phase_sim_deckung <- function(sc) {
  stopifnot(inherits(sc, "liqui_scenario"))

  # Reine Projektion: der volle Ergebnis-Vertrag stammt aus GENAU EINEM
  # phase_simulation(sc)-Aufruf — keine zweite Ziehung, keine eigene Rechnung
  # neben der gemeinsamen Projektion.
  out <- phase_simulation(sc)

  c(
    list(phase = "sim_deckung", scenario = sc$name),
    deckung_from_out(out, sc$mdl$kk_limit)
  )
}
