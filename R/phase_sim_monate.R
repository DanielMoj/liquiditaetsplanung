# ============================================================================
#  R/phase_sim_monate.R  —  Phase 6 (Simulation, Einheit phase-06-monate)
#
#  Projiziert das Simulationsergebnis phase_simulation(sc) auf die Endbestand-
#  Tabelle je Monat und gibt ein Ergebnisobjekt zurueck, statt in die Konsole zu
#  drucken. Der Planner hat die urspruengliche Phase 6 wegen Diff-Budget in drei
#  Einheiten geschnitten: phase-06-fan rechnet den VOLLSTAENDIGEN Ergebnis-Vertrag
#  `out` (genestete Monte-Carlo ueber die eingefrorene Engine); diese Schwester-
#  Einheit projiziert dieses `out` NUR auf die Monats-Tabelle — KEINE zweite
#  Monte-Carlo, KEINE Kernel-Aenderung.
#
#  Reine Projektion: phase_sim_monate ruft den Wrapper phase_simulation(sc) auf
#  (im App-Modul DAG-gecacht ueber scenario_key) und uebernimmt dessen fan /
#  P_under unveraendert. Die Quantilspalten der Tabelle sind exakt die fan-Zeilen
#  5%/50%/95% des Simulationslaufs (kein Etikettentausch), P_unter ist der
#  durchgereichte P_under (Anteil je Monat unter der Mindestreserve, in [0,1]).
#  Weil phase_simulation deterministisch ueber den Szenario-Seed rechnet, ist die
#  Projektion bit-identisch zu einem unabhaengigen phase_simulation(sc)-Lauf.
#
#  Numerik bleibt numerisch: das Ergebnisobjekt traegt EUR-Quantile und den
#  Anteil P_unter als reine Zahlen (fuer Tests/Anker). Die EUR-Formatierung ueber
#  fmt() und die Prozent-Anzeige via sprintf passieren erst im app-Render.
#
#  Setzt voraus, dass kernel/engine.R, kernel/distributions.R, R/scenario.R und
#  R/phase_simulation.R bereits gesourct sind.
# ============================================================================

# project_sim_monate: die EINE reine Projektion aus einem VORHANDENEN
# Simulationsergebnis `out` (phase_simulation-Vertrag) auf die numerische Monats-
# Tabelle. Loest KEINE Simulation aus — reines Umsortieren von out$fan/out$P_under
# in Spalten. Sowohl phase_sim_monate() als auch der App-Render (output$
# sim_monate_tbl in app/app.R) setzen auf diese Funktion auf, damit Wrapper-
# Vertrag und Anzeige nicht auseinanderdriften (eine Quelle fuer die Projektion).
#
#   * P05/P50/P95: exakt die fan-Zeilen 5%/50%/95% (kein Etikettentausch), EUR,
#     numerisch. unname(), weil fan-Zeilen und P_under benannt sind — die
#     Tabellenspalten sollen reine Zahlvektoren sein.
#   * P_unter: der durchgereichte P_under (Anteil je Monat unter Reserve, [0,1]).
# Die EUR-/Prozent-Formatierung (fmt()/sprintf) passiert erst im app-Render.
project_sim_monate <- function(out) {
  H       <- out$metrics$H
  fan     <- out$fan       # 5 x H, Zeilen "5%","25%","50%","75%","95%"
  P_under <- out$P_under   # Anteil je Monat unter Mindestreserve, in [0,1]

  data.frame(
    Monat   = paste0("M", seq_len(H)),
    P05     = unname(fan["5%",  ]),
    P50     = unname(fan["50%", ]),
    P95     = unname(fan["95%", ]),
    P_unter = unname(P_under),
    stringsAsFactors = FALSE
  )
}

phase_sim_monate <- function(sc) {
  stopifnot(inherits(sc, "liqui_scenario"))

  # Reine Projektion: der volle Ergebnis-Vertrag stammt aus phase_simulation —
  # keine zweite Ziehung, keine eigene Quantil-Rechnung. fan / P_under werden
  # unveraendert durchgereicht; die Tabelle baut die gemeinsame Projektion.
  out <- phase_simulation(sc)

  list(
    phase    = "sim_monate",
    scenario = sc$name,
    table    = project_sim_monate(out),  # H Zeilen: Monat/P05/P50/P95/P_unter
    fan      = out$fan,        # 5 x H, aus phase_simulation durchgereicht
    P_under  = out$P_under,    # Vektor je Monat, aus phase_simulation durchgereicht
    H        = out$metrics$H   # metrics$H
  )
}
