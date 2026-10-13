# ============================================================================
#  tests/test_massnahmen.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-11-massnahmen-kern
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/massnahmen.R
#  existiert noch nicht -> das ungeschuetzte source() am Kopf bricht ab; jeder
#  Test spezifiziert zusaetzlich das Zielverhalten). Der Builder erfuellt sie
#  erst mit der Implementierung. Kein skip(), kein succeed(), keine
#  Existenz-Weiche.
#
#  Vertrag (handoff/phase-11-massnahmen-kern.json; KEIN Legacy-Vorbild —
#  fachliche Grundlage ist die Fimovi-Paritaetsluecke; die Einheit ist
#  schirm-los, der Massnahmen-Schirm folgt in phase-11-massnahmen-ui):
#   - massnahmen_katalog(): Startkatalog als DATEN (data.frame) mit >= 12
#     Standard-Massnahmen der Sanierungspraxis in drei Kategorien
#     (leistungswirtschaftlich / finanzwirtschaftlich / liquiditaetswirksam);
#     je Zeile id (snake_case, eindeutig), name, kategorie, typ ("treiber"
#     oder "delta"), treiber (bei typ treiber einer aus menge/preis/tarif/
#     energie/material/pflegebudget/inflation, sonst NA), hinweis; mindestens
#     4 treiber- und mindestens 6 delta-Massnahmen.
#   - quantifiziere_paket(paket, H): paket = Liste von list(id, params);
#     treiber-params list(delta), delta-params list(betrag, start_m (1..H),
#     dauer (>= 1), verlauf "einmalig" | "linear"). Rueckgabe list(
#     treiber_overrides = benannte Liste SUMMIERTER Treiber-Deltas (mehrere
#     Massnahmen auf denselben Treiber addieren sich, auch bei wiederholter
#     id), delta_monat = UNBENANNTER numerischer Vektor Laenge H (einmalig:
#     voller betrag in start_m; linear: betrag/dauer in start_m..min(start_m
#     + dauer - 1, H) — ueber H hinauslaufende Raten werden ABGESCHNITTEN und
#     NICHT umverteilt), doku = data.frame je Massnahme (id, typ,
#     wirkung_text) in Paketreihenfolge). Unbekannte id -> Fehler MIT der id
#     im Text; start_m ausserhalb 1..H -> Fehler; betrag 0 erlaubt; leeres
#     paket -> neutrale Rueckgabe.
#   - massnahmen_overlay(out, delta_monat): REINE Projektion auf ein fertiges
#     phase_simulation-Ergebnis, KEINE Simulation, kein RNG: kum =
#     cumsum(delta_monat) wird auf JEDE Zeile der B-Matrix addiert, trough je
#     Pfad neu als Zeilen-Minimum, metrics$plan_trough nach derselben
#     Minimum-Logik fortgeschrieben als min(plan_trough + kum) — der
#     out-Vertrag traegt keinen monatlichen Plan-Pfad, der Plan-Tiefpunkt-
#     Pegel wird konstant gehalten (dokumentierte Naeherung erster Ordnung,
#     ohne KK-Zins-Rueckkopplung der Deltas). ALLE uebrigen Felder
#     unveraendert durchgereicht; Nullvektor -> identical(out).
#   - szenario_name_mit_paket(basisname, paket): "<basisname> + <n>
#     Massnahmen" (n = Anzahl Paket-Eintraege) — benannte, testbare
#     Namenskonvention fuer die spaetere UI-/compare-Anbindung.
#
#  Orakel-Politik: EXTERNE Orakel statt Selbstkonsistenz — der Test baut
#  Erwartungsvektoren/-matrizen SELBST (numeric()/cumsum/Schleife) und
#  vergleicht identical bzw. all.equal (Toleranz 1e-12). Keine Anker-
#  Literale, keine Tier-1-Werte, keine Fixture-Mappe, kein MC — nur Stubs,
#  kleine Laufzeit.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "massnahmen.R"))  # <- existiert noch nicht: rot zuerst

# Vertragskonstanten des Katalogs (gueltige Treibernamen, Kategorien).
.treiber_gueltig <- c("menge", "preis", "tarif", "energie", "material",
                      "pflegebudget", "inflation")
.kategorien      <- c("leistungswirtschaftlich", "finanzwirtschaftlich",
                      "liquiditaetswirksam")

# Gepinnte Beispiel-Massnahmen aus dem Fachvertrag (id -> Treiber bzw. delta).
.bsp_treiber <- c(preiserhoehung  = "preis", mengenprogramm    = "menge",
                  tarifmoderation = "tarif", energieeinsparung = "energie")
.bsp_delta   <- c("gesellschafterdarlehen", "investitionsstopp",
                  "sale_and_lease_back", "stundung_verbindlichkeiten",
                  "forderungsverkauf", "kurzarbeit")

# ===========================================================================
#  (1) Katalog: Daten-Vertrag mit harten Waechtern
# ===========================================================================

test_that("Katalog: data.frame-Vertrag — >= 12 Zeilen, exakte Spalten, ids eindeutig+snake_case, Typen/Kategorien/Treiber gueltig, Mindestzahlen je Typ", {
  kat <- massnahmen_katalog()

  expect_true(is.data.frame(kat))
  expect_identical(names(kat), c("id", "name", "kategorie", "typ", "treiber", "hinweis"))
  expect_gte(nrow(kat), 12L)

  expect_true(is.character(kat$id))
  expect_identical(anyDuplicated(kat$id), 0L)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", kat$id)),
              info = "ids muessen snake_case sein (^[a-z][a-z0-9_]*$).")

  expect_true(all(kat$typ %in% c("treiber", "delta")))
  # Harte Mindestzahlen-Waechter je Typ (Vertrag: >= 4 treiber, >= 6 delta).
  expect_gte(sum(kat$typ == "treiber"), 4L)
  expect_gte(sum(kat$typ == "delta"),   6L)

  expect_true(all(kat$kategorie %in% .kategorien))
  expect_true(all(.kategorien %in% kat$kategorie),
              info = "alle drei Kategorien muessen im Startkatalog vorkommen.")

  # Zweipfadiges Wirkmodell: treiber-Zeilen tragen einen gueltigen
  # Treibernamen, delta-Zeilen NA (nicht "", nicht "delta").
  expect_true(all(kat$treiber[kat$typ == "treiber"] %in% .treiber_gueltig))
  expect_true(all(is.na(kat$treiber[kat$typ == "delta"])))

  expect_true(is.character(kat$name)    && all(nzchar(kat$name)))
  expect_true(is.character(kat$hinweis) && all(nzchar(kat$hinweis)))
})

test_that("Katalog: gepinnte Beispiel-Massnahmen des Fachvertrags vorhanden mit korrektem Typ und Treiber", {
  kat <- massnahmen_katalog()

  for (id in names(.bsp_treiber)) {
    z <- kat[kat$id == id, ]
    expect_identical(nrow(z), 1L, info = paste0("Beispiel-Massnahme fehlt: ", id))
    expect_identical(z$typ, "treiber", info = id)
    expect_identical(z$treiber, unname(.bsp_treiber[id]), info = id)
  }
  for (id in .bsp_delta) {
    z <- kat[kat$id == id, ]
    expect_identical(nrow(z), 1L, info = paste0("Beispiel-Massnahme fehlt: ", id))
    expect_identical(z$typ, "delta", info = id)
    expect_true(is.na(z$treiber), info = id)
  }
})

# ===========================================================================
#  (2) quantifiziere_paket: synthetische Pakete, EXTERNE Orakel
# ===========================================================================

test_that("quantifiziere_paket (a): zwei Massnahmen auf DENSELBEN Treiber addieren sich; getrennte Treiber bleiben getrennt", {
  .d1 <- 0.02; .d2 <- 0.035
  q <- quantifiziere_paket(list(
    list(id = "preiserhoehung", params = list(delta = .d1)),
    list(id = "preiserhoehung", params = list(delta = .d2))
  ), H = 6L)

  expect_identical(names(q$treiber_overrides), "preis")
  expect_equal(q$treiber_overrides$preis, .d1 + .d2, tolerance = 1e-12)  # externe Summe
  expect_identical(q$delta_monat, numeric(6))   # reines Treiber-Paket: Nullvektor

  # Gegenprobe: zwei VERSCHIEDENE Treiber landen in zwei getrennten
  # Eintraegen mit je EIGENEM Delta (killt Alles-in-einen-Topf-Summierer).
  q2 <- quantifiziere_paket(list(
    list(id = "preiserhoehung",    params = list(delta = .d1)),
    list(id = "energieeinsparung", params = list(delta = -0.01))
  ), H = 6L)
  expect_identical(sort(names(q2$treiber_overrides)), c("energie", "preis"))
  expect_equal(q2$treiber_overrides$preis,    .d1,  tolerance = 1e-12)
  expect_equal(q2$treiber_overrides$energie, -0.01, tolerance = 1e-12)
})

test_that("quantifiziere_paket (b): einmalig — voller Betrag exakt in start_m, 0 sonst (identical gegen extern gebauten Vektor)", {
  .betrag <- 1500
  q <- quantifiziere_paket(list(
    list(id = "gesellschafterdarlehen",
         params = list(betrag = .betrag, start_m = 3L, dauer = 1L, verlauf = "einmalig"))
  ), H = 6L)

  erw <- numeric(6); erw[3] <- .betrag          # externes Orakel
  expect_identical(q$delta_monat, erw)
  expect_identical(length(q$treiber_overrides), 0L)
})

test_that("quantifiziere_paket (c): linear mit Abschnitt am Horizont — H=6, start_m=5, dauer=4 -> genau 2 Raten a betrag/4, Rest ABGESCHNITTEN, nicht umverteilt", {
  .betrag <- 2000
  q <- quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = .betrag, start_m = 5L, dauer = 4L, verlauf = "linear"))
  ), H = 6L)

  erw <- numeric(6); erw[5:6] <- .betrag / 4    # externes Orakel: 2 von 4 Raten
  expect_identical(q$delta_monat, erw)          # der Grenzfall-Pin des Abschneide-Vertrags
  # Abschneide-Vertrag laut: die Summe ist NICHT der volle Betrag — eine
  # umverteilende Implementierung (betrag/2 je Restmonat) faellt hier rot.
  expect_equal(sum(q$delta_monat), .betrag / 2, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(sum(q$delta_monat), .betrag, tolerance = 1e-12)))
})

test_that("quantifiziere_paket (d): Mischpaket — beide Kanaele gleichzeitig korrekt, Rueckgabe- und doku-Vertrag", {
  paket <- list(
    list(id = "mengenprogramm",    params = list(delta = 0.04)),
    list(id = "energieeinsparung", params = list(delta = -0.015)),
    list(id = "kurzarbeit",
         params = list(betrag = 900, start_m = 2L, dauer = 3L, verlauf = "linear")),
    list(id = "forderungsverkauf",
         params = list(betrag = 250, start_m = 1L, dauer = 1L, verlauf = "einmalig"))
  )
  q <- quantifiziere_paket(paket, H = 4L)

  expect_identical(names(q), c("treiber_overrides", "delta_monat", "doku"))

  expect_identical(sort(names(q$treiber_overrides)), c("energie", "menge"))
  expect_equal(q$treiber_overrides$menge,    0.04,  tolerance = 1e-12)
  expect_equal(q$treiber_overrides$energie, -0.015, tolerance = 1e-12)

  erw <- numeric(4)                             # externes Orakel, beide Kanaele
  erw[2:4] <- erw[2:4] + 900 / 3                # linear voll im Horizont: M2..M4
  erw[1]   <- erw[1] + 250                      # einmalig in M1
  expect_equal(q$delta_monat, erw, tolerance = 1e-12)

  expect_true(is.data.frame(q$doku))
  expect_identical(nrow(q$doku), 4L)
  expect_identical(q$doku$id,
                   c("mengenprogramm", "energieeinsparung", "kurzarbeit", "forderungsverkauf"))
  expect_identical(q$doku$typ, c("treiber", "treiber", "delta", "delta"))
  expect_true(is.character(q$doku$wirkung_text) && all(nzchar(q$doku$wirkung_text)))
})

test_that("quantifiziere_paket (e): leeres Paket -> neutrale Rueckgabe; betrag 0 ist erlaubt", {
  q <- quantifiziere_paket(list(), H = 5L)
  expect_identical(q$delta_monat, numeric(5))   # Nullvektor Laenge H
  expect_true(is.list(q$treiber_overrides))
  expect_identical(length(q$treiber_overrides), 0L)
  expect_true(is.data.frame(q$doku))
  expect_identical(nrow(q$doku), 0L)

  q0 <- quantifiziere_paket(list(
    list(id = "investitionsstopp",
         params = list(betrag = 0, start_m = 1L, dauer = 1L, verlauf = "einmalig"))
  ), H = 3L)
  expect_identical(q0$delta_monat, numeric(3))
  expect_identical(nrow(q0$doku), 1L)
})

test_that("quantifiziere_paket (f): unbekannte id (id im Fehlertext), start_m = 0 und start_m = H+1 werden zurueckgewiesen", {
  expect_error(
    quantifiziere_paket(list(
      list(id = "diese_id_gibt_es_nicht", params = list(delta = 0.01))
    ), H = 6L),
    regexp = "diese_id_gibt_es_nicht")
  expect_error(quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = 100, start_m = 0L, dauer = 1L, verlauf = "einmalig"))
  ), H = 6L))
  expect_error(quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = 100, start_m = 7L, dauer = 1L, verlauf = "einmalig"))
  ), H = 6L))
})

# ===========================================================================
#  (2b) Mutgate-Nachschub M1: raten_ende — der ungekappte Normalfall
#  Beide Bestandsfaelle (c) und (d) laufen in die H-Kappung; dort maskiert
#  min(..., H) den Off-by-one. Hier wird der Normalfall gegen LITERALE
#  gepinnt (keine Selbstkonsistenz gegen die Formel).
# ===========================================================================

test_that("raten_ende: Normalfall ungekappt, buendiger Rand, Kappung, dauer=1 — gegen Literale", {
  expect_identical(raten_ende(3L, 4L, 12L), 6L)    # Normalfall: M3..M6, NICHT M7
  expect_identical(raten_ende(1L, 12L, 12L), 12L)  # buendig: start+dauer-1 == H
  expect_identical(raten_ende(10L, 5L, 12L), 12L)  # Kappung am Horizont
  expect_identical(raten_ende(5L, 1L, 12L), 5L)    # dauer=1: nur der Startmonat
})

test_that("quantifiziere_paket (g): linear MITTEN im Horizont — letzte Rate exakt in start_m+dauer-1, Folgemonat exakt 0, Wirkungstext traegt das korrekte Ende", {
  .betrag <- 1200
  q <- quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = .betrag, start_m = 3L, dauer = 4L, verlauf = "linear"))
  ), H = 12L)

  erw <- numeric(12); erw[3:6] <- .betrag / 4   # externes Orakel: 4 volle Raten
  expect_identical(q$delta_monat, erw)
  expect_identical(q$delta_monat[7], 0)          # der Off-by-one-Killer: M7 leer
  expect_equal(sum(q$delta_monat), .betrag, tolerance = 1e-12)  # ungekappt: voller Betrag
  expect_true(grepl("M3..M6", q$doku$wirkung_text[1], fixed = TRUE))
})

test_that("quantifiziere_paket (h): Gueltigkeitsrand start_m == H — beide Verlaeufe gueltig; Minihorizont H=1 (Mutgate-Nachschub M4: >= statt > in der Bereichspruefung)", {
  # einmalig exakt am Horizont: voller Betrag in M_H, kein Fehler
  q1 <- quantifiziere_paket(list(
    list(id = "gesellschafterdarlehen",
         params = list(betrag = 800, start_m = 6L, dauer = 1L, verlauf = "einmalig"))
  ), H = 6L)
  erw1 <- numeric(6); erw1[6] <- 800
  expect_identical(q1$delta_monat, erw1)

  # linear exakt am Horizont: genau EINE Rate (betrag/dauer) in M_H,
  # Rest jenseits H abgeschnitten
  q2 <- quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = 900, start_m = 6L, dauer = 3L, verlauf = "linear"))
  ), H = 6L)
  erw2 <- numeric(6); erw2[6] <- 900 / 3
  expect_identical(q2$delta_monat, erw2)
  expect_true(grepl("M6..M6", q2$doku$wirkung_text[1], fixed = TRUE))

  # Minihorizont H = 1: start_m = 1 == H ist der einzige gueltige Monat
  q3 <- quantifiziere_paket(list(
    list(id = "forderungsverkauf",
         params = list(betrag = 120, start_m = 1L, dauer = 1L, verlauf = "einmalig"))
  ), H = 1L)
  expect_identical(q3$delta_monat, 120)
})

test_that("quantifiziere_paket (i): Stapelung im selben Monat — Deltas ADDIEREN sich, nichts wird ueberschrieben (Mutgate-Nachschub M5); beide Zweige einmalig und linear", {
  # zwei EINMALIGE Massnahmen im selben Monat: Summe, nicht letzter Wert
  qa <- quantifiziere_paket(list(
    list(id = "gesellschafterdarlehen",
         params = list(betrag = 700, start_m = 3L, dauer = 1L, verlauf = "einmalig")),
    list(id = "sale_and_lease_back",
         params = list(betrag = 250, start_m = 3L, dauer = 1L, verlauf = "einmalig"))
  ), H = 6L)
  erwa <- numeric(6); erwa[3] <- 700 + 250      # externes Orakel: Summe
  expect_identical(qa$delta_monat, erwa)
  # Ueberschreiben-Detektor explizit: weder der erste noch der zweite Betrag allein
  expect_false(isTRUE(all.equal(qa$delta_monat[3], 700, tolerance = 1e-12)))
  expect_false(isTRUE(all.equal(qa$delta_monat[3], 250, tolerance = 1e-12)))

  # EINMALIG mitten auf laufende LINEAR-Raten (einmalig zuerst im Paket,
  # linear danach): der Linear-Zweig muss auf den Einmalbetrag ADDIEREN —
  # killt Ueberschreiben in BEIDEN Zweigen, egal welcher zuletzt schreibt
  qb <- quantifiziere_paket(list(
    list(id = "investitionsstopp",
         params = list(betrag = 400, start_m = 3L, dauer = 1L, verlauf = "einmalig")),
    list(id = "kurzarbeit",
         params = list(betrag = 900, start_m = 2L, dauer = 3L, verlauf = "linear"))
  ), H = 6L)
  erwb <- numeric(6)
  erwb[2:4] <- 900 / 3                          # linear M2..M4
  erwb[3]   <- erwb[3] + 400                    # plus Einmalbetrag in M3
  expect_equal(qb$delta_monat, erwb, tolerance = 1e-12)
  expect_equal(qb$delta_monat[3], 900 / 3 + 400, tolerance = 1e-12)
  expect_equal(sum(qb$delta_monat), 900 + 400, tolerance = 1e-12)
})

test_that("quantifiziere_paket (j): ungueltiger verlauf wird zurueckgewiesen (praeventiver Pin des Validierungspfads)", {
  expect_error(quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = 100, start_m = 1L, dauer = 2L, verlauf = "quadratisch"))
  ), H = 6L))
  expect_error(quantifiziere_paket(list(
    list(id = "kurzarbeit",
         params = list(betrag = 100, start_m = 1L, dauer = 2L))
  ), H = 6L))
})

# ===========================================================================
#  (3) massnahmen_overlay: synthetischer out-Stub, alles EXTERN gerechnet
# ===========================================================================

# ---------------------------------------------------------------------------
# Synthetischer out-Stub im phase_simulation-Ergebnisvertrag (Muster
# .mk_sim_out aus tests/test_compare.R): kleine B-Matrix 3 x 4 mit PAARWEISE
# UNVERWECHSELBAREN Zeilen, trough konsistent als Zeilen-Minimum, metrics mit
# plan_trough (BEWUSST unabhaengig von min(trough) gesetzt). KEINE Engine,
# kein Seed-Lauf, KEINE Tier-1-Werte. Theta/fan/P_under/spec/R/diagnostics
# sind reine Durchreich-Marker.
# ---------------------------------------------------------------------------
.mk_overlay_out <- function() {
  B <- rbind(c(   0, -300, -100, 200),
             c( 500,  100,  400, 300),
             c( -50,  700,  650, -20))
  dimnames(B) <- list(NULL, paste0("M", seq_len(ncol(B))))
  trough <- apply(B, 1, min)
  list(
    phase           = "simulation",
    scenario        = "OverlayStub",
    B               = B,
    Theta           = matrix(c(0.01, -0.02, 0.03, 0.005, -0.015, 0.025),
                             nrow = 3, dimnames = list(NULL, c("menge", "preis"))),
    fan             = apply(B, 2, quantile, probs = c(.05, .25, .5, .75, .95)),
    P_under         = colMeans(B < 0),
    trough          = trough,
    metrics         = list(
      S             = 3L,
      seed          = 99L,
      H             = 4L,
      trough_median = unname(median(trough)),
      trough_p05    = unname(quantile(trough, .05)),
      trough_p95    = unname(quantile(trough, .95)),
      P_breach      = 1 / 3,
      P_breach_pct  = 100 / 3,
      P_solvent     = 2 / 3,
      P_solvent_pct = 200 / 3,
      plan_trough   = -480
    ),
    spec            = list(menge = list(cv = 0.1)),
    R               = diag(2),
    plot_path       = NA_character_,
    endbestand_path = NA_character_,
    diagnostics     = list(n_treiber = 2L, deterministisch = TRUE, stuetzen_ok = TRUE)
  )
}

# delta_monat mit VORZEICHENWECHSEL: kum = c(-100, 150, -250, -200) ist
# ungleich delta (kum-vs-punktuell unterscheidbar), Minimum von kum in M3.
.delta_vz <- c(-100, 250, -400, 50)

test_that("massnahmen_overlay: kum-Projektion — B', trough' und plan_trough' EXTERN gerechnet, jede Zelle 1e-12; Argmin-Verschiebung als Nicht-Trivialitaets-Waechter", {
  out <- .mk_overlay_out()
  kum <- cumsum(.delta_vz)                       # externes Orakel

  # Design-Waechter (harte Fails, KEIN skip): Vorzeichenwechsel im delta,
  # kum != delta (sonst blind fuer kum-vs-punktuell-Verwechslung), Zeilen
  # und troughs paarweise unverwechselbar (sonst blind fuer Zeilentausch).
  expect_true(min(.delta_vz) < 0 && max(.delta_vz) > 0)
  expect_false(identical(kum, .delta_vz))
  expect_identical(nrow(unique(out$B)), 3L)
  expect_identical(anyDuplicated(out$trough), 0L)

  Berw <- out$B
  for (m in seq_along(kum)) Berw[, m] <- out$B[, m] + kum[m]   # externes Orakel
  terw <- apply(Berw, 1, min)

  # Nicht-Trivialitaets-Waechter: mindestens ein Pfad-trough verschiebt den
  # Monatsindex (Zeile 1: M2 -> M3) — eine Implementierung, die nur
  # trough + kum[alter Argmin] rechnet statt neu zu minimieren, faellt rot.
  expect_true(any(apply(out$B, 1, which.min) != apply(Berw, 1, which.min)),
              info = "Waechter verletzt: kein Pfad verschiebt den trough-Monat — Stub/delta anpassen.")

  res <- massnahmen_overlay(out, .delta_vz)

  expect_equal(res$B,      Berw, tolerance = 1e-12)  # jede Zelle inkl. dimnames
  expect_equal(res$trough, terw, tolerance = 1e-12)
  # plan_trough nach derselben Minimum-Logik auf dem konstant gehaltenen
  # Plan-Tiefpunkt-Pegel: min(plan_trough + kum) = plan_trough + min(kum)
  # (der out-Vertrag traegt keinen monatlichen Plan-Pfad — dokumentierte
  # Naeherung; hier -480 - 250 = -730, NICHT plan + sum(delta) = -680).
  expect_equal(res$metrics$plan_trough,
               min(out$metrics$plan_trough + kum), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(res$metrics$plan_trough,
                                out$metrics$plan_trough + sum(.delta_vz),
                                tolerance = 1e-12)))
})

test_that("massnahmen_overlay: Durchreich-Waechter (Theta/seed/S und alle uebrigen Felder identisch), kein RNG-Verbrauch, deterministisch", {
  out <- .mk_overlay_out()
  set.seed(424242)                        # .Random.seed existiert -> RNG-Waechter
  rng_vor <- .Random.seed
  res  <- massnahmen_overlay(out, .delta_vz)
  res2 <- massnahmen_overlay(out, .delta_vz)

  expect_identical(.Random.seed, rng_vor)   # KEINE Simulation, kein RNG
  expect_identical(res2, res)               # deterministisch

  expect_identical(names(res), names(out))  # kein Feld faellt weg oder kommt hinzu
  expect_identical(res$Theta,        out$Theta)
  expect_identical(res$metrics$seed, out$metrics$seed)
  expect_identical(res$metrics$S,    out$metrics$S)
  # metrics: AUSSER plan_trough alles unveraendert durchgereicht (Vertrag:
  # nur B, trough und metrics$plan_trough aendern sich; nachgelagerte
  # Projektionen wie deckung_from_out rechnen aus out$trough ohnehin frisch).
  uebrig <- setdiff(names(out$metrics), "plan_trough")
  expect_identical(res$metrics[uebrig], out$metrics[uebrig])
  for (f in c("phase", "scenario", "fan", "P_under", "spec", "R",
              "plot_path", "endbestand_path", "diagnostics"))
    expect_identical(res[[f]], out[[f]])
})

test_that("massnahmen_overlay: Neutralitaet — Nullvektor liefert identical(out)", {
  out <- .mk_overlay_out()
  expect_identical(massnahmen_overlay(out, numeric(ncol(out$B))), out)
})

test_that("massnahmen_overlay: TEILNULL-Vektor (einzelne Nullmonate, nicht alle) projiziert — kein Fruehabbruch (Mutgate-Nachschub M2: any statt all)", {
  out <- .mk_overlay_out()
  d   <- c(0, 200, 0, -150)                     # Nullen an 1 und 3, aber nicht alle
  # Design-Waechter: mindestens eine 0, mindestens ein Nicht-Null-Eintrag
  expect_true(any(d == 0) && any(d != 0))

  kum  <- cumsum(d)                             # externes Orakel: c(0, 200, 200, 50)
  Berw <- out$B
  for (m in seq_along(kum)) Berw[, m] <- out$B[, m] + kum[m]

  res <- massnahmen_overlay(out, d)
  expect_false(identical(res, out))             # der M2-Kill: KEIN Durchreichen
  expect_equal(res$B,      Berw,                tolerance = 1e-12)
  expect_equal(res$trough, apply(Berw, 1, min), tolerance = 1e-12)
  expect_equal(res$metrics$plan_trough,
               min(out$metrics$plan_trough + kum), tolerance = 1e-12)
})

# ===========================================================================
#  (4) szenario_name_mit_paket: Namenskonvention
# ===========================================================================

test_that("szenario_name_mit_paket: '<basisname> + <n> Massnahmen' mit n = Anzahl Paket-Eintraege", {
  p2 <- list(
    list(id = "preiserhoehung", params = list(delta = 0.02)),
    list(id = "kurzarbeit",
         params = list(betrag = 100, start_m = 1L, dauer = 2L, verlauf = "linear"))
  )
  p4 <- c(p2, list(
    list(id = "mengenprogramm", params = list(delta = 0.01)),
    list(id = "forderungsverkauf",
         params = list(betrag = 50, start_m = 2L, dauer = 1L, verlauf = "einmalig"))
  ))
  expect_identical(szenario_name_mit_paket("Basis", p2),       "Basis + 2 Massnahmen")
  expect_identical(szenario_name_mit_paket("Stress 2026", p4), "Stress 2026 + 4 Massnahmen")
})
