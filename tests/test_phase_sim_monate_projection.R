# ============================================================================
#  tests/test_phase_sim_monate_projection.R  —  "keine zweite MC" mit Mutations-
#  kraft (Einheit phase-06-monate)
#
#  Das Schwester-Orakel test_phase_sim_monate.R prueft die "reine Projektion" nur
#  ueber bit-identische DETERMINISTISCHE Outputs (out$fan == ref$fan, Seed 2026).
#  Diese Gleichheit ueberlebt aber einen zweiten, synchronen phase_simulation()-
#  Re-Run: derselbe Seed liefert erneut dieselben Zahlen. Die Regel "Schwere
#  Monte-Carlo laeuft NICHT synchron im UI-Thread / phase_sim_monate loest nur EINE
#  Ziehung aus" (CLAUDE.md) braucht daher ein Orakel mit Aufruf-Zaehlung.
#
#  Dieses Orakel stubt phase_simulation() durch einen zaehlenden Fake (KEINE
#  Engine, kein Seed) und prueft:
#    (1) phase_sim_monate(sc) ruft phase_simulation GENAU EINMAL — ein zweiter
#        synchroner Re-Run faellt hier zwingend rot (Zaehler == 2).
#    (2) project_sim_monate(out) loest KEINE Simulation aus (reine Projektion aus
#        vorhandenem out) und liefert die Tabelle exakt aus out$fan/out$P_under.
#  Der Stub-out traegt bewusst KEINE Tier-1-Ankerwerte — nur paarweise
#  unverwechselbare Zahlen, damit ein Spalten-/Etikettentausch auffaellt.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "phase_sim_monate.R"))  # phase_sim_monate + project_sim_monate

# Deterministischer Stub-Ergebnisvertrag (phase_simulation-Form): metrics$H, fan
# (5xH, benannte Zeilen), P_under. Paarweise verschiedene, geordnete Zahlen und
# je Monat unterschiedliche Werte -> ein Etikettentausch erzeugt eine falsche
# Zelle. KEINE Engine, kein Seed — rein synthetisch.
.stub_out <- function() {
  H   <- 3L
  fan <- rbind(
    "5%"  = c(-500000, -400000, -300000),
    "25%" = c(-200000, -100000,       0),
    "50%" = c( 100000,  200000,  300000),
    "75%" = c( 500000,  600000,  700000),
    "95%" = c( 900000,  850000,  800000)
  )
  colnames(fan) <- paste0("M", seq_len(H))
  list(metrics = list(H = H), fan = fan, P_under = c(0.12, 0.34, 0.00))
}

# ---------------------------------------------------------------------------
# (1) phase_sim_monate loest GENAU eine Monte-Carlo aus: phase_simulation wird
#     durch einen zaehlenden Fake ersetzt (in globalenv, wo die gesourcte
#     Funktion aufloest) und nach dem Test wieder hergestellt. Ein zusaetzlicher
#     synchroner Re-Run von phase_simulation() liesse den Zaehler auf 2 laufen.
# ---------------------------------------------------------------------------
test_that("phase_sim_monate ruft phase_simulation GENAU einmal (kein synchroner Re-Run)", {
  calls <- 0L
  fake  <- function(sc) { calls <<- calls + 1L; .stub_out() }

  had_orig <- exists("phase_simulation", envir = globalenv(), inherits = FALSE)
  orig     <- if (had_orig) get("phase_simulation", envir = globalenv()) else NULL
  assign("phase_simulation", fake, envir = globalenv())
  on.exit({
    if (had_orig) assign("phase_simulation", orig, envir = globalenv())
    else if (exists("phase_simulation", envir = globalenv(), inherits = FALSE))
      rm("phase_simulation", envir = globalenv())
  }, add = TRUE)

  sc  <- structure(list(name = "MockSc"), class = "liqui_scenario")
  res <- phase_sim_monate(sc)

  expect_equal(calls, 1L)                       # nicht 0 (Projektion braucht out), nicht >=2 (keine zweite MC)
  expect_equal(nrow(res$table), 3L)             # Projektion aus dem Fake-out gebaut
  expect_equal(unname(res$table$P05), c(-500000, -400000, -300000))
})

# ---------------------------------------------------------------------------
# (2) project_sim_monate ist eine reine Projektion: sie darf KEINE Simulation
#     ausloesen. phase_simulation wird durch einen Fake ersetzt, der beim Aufruf
#     hart faellt — bleibt er unberuehrt (kein Fehler), ist bewiesen, dass die
#     Projektion nur aus dem uebergebenen out rechnet. Zusaetzlich: die Tabelle
#     entspricht exakt out$fan-Zeilen 5%/50%/95% und out$P_under.
# ---------------------------------------------------------------------------
test_that("project_sim_monate loest KEINE Simulation aus und projiziert nur das gegebene out", {
  had_orig <- exists("phase_simulation", envir = globalenv(), inherits = FALSE)
  orig     <- if (had_orig) get("phase_simulation", envir = globalenv()) else NULL
  assign("phase_simulation",
         function(...) stop("project_sim_monate darf keine Monte-Carlo ausloesen"),
         envir = globalenv())
  on.exit({
    if (had_orig) assign("phase_simulation", orig, envir = globalenv())
    else if (exists("phase_simulation", envir = globalenv(), inherits = FALSE))
      rm("phase_simulation", envir = globalenv())
  }, add = TRUE)

  out <- .stub_out()
  expect_error(project_sim_monate(out), NA)  # keine Simulation ausgeloest (Fake wuerde stop()en)
  tbl <- project_sim_monate(out)

  expect_equal(as.character(tbl$Monat), paste0("M", seq_len(3)))
  expect_equal(unname(tbl$P05),     unname(out$fan["5%",  ]))
  expect_equal(unname(tbl$P50),     unname(out$fan["50%", ]))
  expect_equal(unname(tbl$P95),     unname(out$fan["95%", ]))
  expect_equal(unname(tbl$P_unter), unname(out$P_under))
})
