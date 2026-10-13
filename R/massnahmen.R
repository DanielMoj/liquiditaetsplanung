# ============================================================================
#  R/massnahmen.R  —  phase-11-massnahmen-kern
#
#  Zweck: schliesst die Fimovi-Paritaetsluecke — ein Massnahmenkatalog der
#  Sanierungspraxis plus Quantifizierung und Overlay, damit Massnahmenpakete
#  ("was bringt Kurzarbeit ab M2?") auf ein fertiges Simulationsergebnis
#  gerechnet werden koennen, ohne die Engine anzufassen.
#
#  Zweipfadiges Wirkmodell:
#   - typ "treiber": wirkt als Delta auf einen Engine-Treiber (menge/preis/
#     tarif/energie/material/pflegebudget/inflation) und wird als
#     treiber_overrides an den naechsten Simulationslauf uebergeben.
#   - typ "delta": wirkt als direkter Liquiditaets-Zufluss/-Abfluss je Monat
#     (delta_monat, Laenge H) und wird OHNE neuen Lauf per Overlay auf ein
#     bestehendes Ergebnis projiziert.
#
#  massnahmen_overlay ist eine REINE, MC-freie Projektion: kum = cumsum(
#  delta_monat) wird auf jede Pfadzeile der B-Matrix addiert, trough je Pfad
#  neu minimiert, metrics$plan_trough als min(plan_trough + kum) fort-
#  geschrieben. Der Ergebnisvertrag traegt keinen monatlichen Plan-Pfad,
#  darum wird der Plan-Tiefpunkt-Pegel konstant gehalten — dokumentierte
#  Naeherung erster Ordnung ohne KK-Zins-Rueckkopplung der Deltas. Alle
#  uebrigen Felder werden unveraendert durchgereicht; kein RNG, keine Prints.
#
#  Worker-sicher und deterministisch; keine Abhaengigkeit ausser base.
# ============================================================================

# ----------------------------------------------------------------------------
# massnahmen_katalog(): Startkatalog als DATEN — >= 12 Standard-Massnahmen in
# drei Kategorien; treiber-Zeilen tragen den Treibernamen, delta-Zeilen NA.
# ----------------------------------------------------------------------------
massnahmen_katalog <- function() {
  data.frame(
    id = c(
      "preiserhoehung", "mengenprogramm", "tarifmoderation",
      "energieeinsparung", "materialkostenprogramm", "pflegebudget_ausschoepfung",
      "gesellschafterdarlehen", "investitionsstopp", "sale_and_lease_back",
      "stundung_verbindlichkeiten", "forderungsverkauf", "kurzarbeit"
    ),
    name = c(
      "Preiserhoehung Wahlleistungen und Ambulanz",
      "Mengen-/Fallzahlprogramm",
      "Tarifmoderation Personal",
      "Energieeinsparprogramm",
      "Einkaufs- und Materialkostenprogramm",
      "Pflegebudget-Ausschoepfung",
      "Gesellschafterdarlehen",
      "Investitionsstopp",
      "Sale-and-Lease-Back",
      "Stundung von Verbindlichkeiten",
      "Forderungsverkauf (Factoring)",
      "Kurzarbeit"
    ),
    kategorie = c(
      "leistungswirtschaftlich", "leistungswirtschaftlich",
      "leistungswirtschaftlich", "leistungswirtschaftlich",
      "leistungswirtschaftlich", "leistungswirtschaftlich",
      "finanzwirtschaftlich", "finanzwirtschaftlich", "finanzwirtschaftlich",
      "liquiditaetswirksam", "liquiditaetswirksam", "liquiditaetswirksam"
    ),
    typ = c(
      "treiber", "treiber", "treiber", "treiber", "treiber", "treiber",
      "delta", "delta", "delta", "delta", "delta", "delta"
    ),
    treiber = c(
      "preis", "menge", "tarif", "energie", "material", "pflegebudget",
      NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_
    ),
    hinweis = c(
      "Hebt den Preis-Treiber dauerhaft an; Wirkung ueber Erloesbasis, nicht als Einmalzahlung.",
      "Fallzahl-/Auslastungsprogramm auf den Mengen-Treiber; realistisch erst mit Anlaufzeit.",
      "Dampft die Tarifsteigerung (Tarif-Treiber); setzt Einigung mit Tarifpartnern voraus.",
      "Senkt den Energie-Treiber (negatives Delta = Einsparung); technische Massnahmen noetig.",
      "Senkt den Material-Treiber ueber Einkaufskonditionen und Standardisierung.",
      "Schoepft refinanzierbare Pflegepersonalkosten ueber den Pflegebudget-Treiber aus.",
      "Einmaliger Liquiditaetszufluss vom Traeger; Rueckzahlung ausserhalb des Horizonts.",
      "Verschiebt geplante Auszahlungen; wirkt als vermiedener Abfluss im Startmonat.",
      "Einmalzufluss aus Anlagenverkauf; spaetere Leasingraten hier nicht modelliert.",
      "Schiebt Auszahlungen ueber die Laufzeit; typischerweise linear ueber die Stundungsdauer.",
      "Vorgezogener Zufluss aus offenen Forderungen, abzueglich Factoring-Abschlag.",
      "Reduziert Personalauszahlungen ueber die Bezugsdauer; typischerweise linear."
    ),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# raten_ende(start_m, dauer, H): letzter Ratenmonat einer linearen Massnahme —
# start_m + dauer - 1, am Horizont H gekappt (Abschneiden, keine Umverteilung).
# Als benannte Funktion herausgezogen (Mutgate-Nachschub M1): der ungekappte
# Normalfall ist direkt testbar, der Off-by-one (+1 am Ende) nicht mehr
# vertragsfreier Raum.
# ----------------------------------------------------------------------------
raten_ende <- function(start_m, dauer, H) {
  min(start_m + as.integer(dauer) - 1L, as.integer(H))
}

# ----------------------------------------------------------------------------
# quantifiziere_paket(paket, H): paket = Liste von list(id, params).
#  - treiber-params: list(delta) -> summiert je Treiber in treiber_overrides
#    (mehrere Massnahmen auf denselben Treiber addieren sich, auch bei
#    wiederholter id).
#  - delta-params: list(betrag, start_m in 1..H, dauer >= 1, verlauf
#    "einmalig" | "linear") -> delta_monat (unbenannt, Laenge H); einmalig:
#    voller Betrag in start_m; linear: betrag/dauer je Monat ueber
#    start_m..min(start_m + dauer - 1, H) — ueber H hinauslaufende Raten
#    werden abgeschnitten, NICHT umverteilt.
# Validierung zuerst (unbekannte id MIT id im Fehlertext), dann Rechnung.
# ----------------------------------------------------------------------------
quantifiziere_paket <- function(paket, H) {
  stopifnot(is.list(paket), is.numeric(H), length(H) == 1L, H >= 1)
  H   <- as.integer(H)
  kat <- massnahmen_katalog()

  # --- Validierungspass ueber das ganze Paket, bevor irgendetwas rechnet ---
  for (eintrag in paket) {
    id <- eintrag$id
    if (is.null(id) || length(id) != 1L || !id %in% kat$id)
      stop(sprintf("Unbekannte Massnahmen-id: '%s' (nicht im Katalog).",
                   as.character(id)))
    typ <- kat$typ[kat$id == id]
    p   <- eintrag$params
    if (identical(typ, "treiber")) {
      if (is.null(p$delta) || !is.numeric(p$delta) || length(p$delta) != 1L)
        stop(sprintf("Massnahme '%s': params$delta (eine Zahl) erforderlich.", id))
    } else {
      if (is.null(p$betrag) || !is.numeric(p$betrag) || length(p$betrag) != 1L)
        stop(sprintf("Massnahme '%s': params$betrag (eine Zahl) erforderlich.", id))
      if (is.null(p$start_m) || !is.numeric(p$start_m) || length(p$start_m) != 1L ||
          p$start_m < 1 || p$start_m > H)
        stop(sprintf("Massnahme '%s': start_m muss in 1..%d liegen.", id, H))
      if (is.null(p$dauer) || !is.numeric(p$dauer) || length(p$dauer) != 1L ||
          p$dauer < 1)
        stop(sprintf("Massnahme '%s': dauer muss >= 1 sein.", id))
      if (is.null(p$verlauf) || !p$verlauf %in% c("einmalig", "linear"))
        stop(sprintf("Massnahme '%s': verlauf muss 'einmalig' oder 'linear' sein.", id))
    }
  }

  # --- Rechenpass: Treiber summieren, Monatsdeltas stapeln, doku sammeln ---
  overrides    <- list()
  delta_monat  <- numeric(H)
  doku_id      <- character(0)
  doku_typ     <- character(0)
  doku_wirkung <- character(0)

  for (eintrag in paket) {
    id  <- eintrag$id
    typ <- kat$typ[kat$id == id]
    p   <- eintrag$params

    if (identical(typ, "treiber")) {
      tr        <- kat$treiber[kat$id == id]
      bisher    <- if (is.null(overrides[[tr]])) 0 else overrides[[tr]]
      overrides[[tr]] <- bisher + p$delta
      wirkung <- sprintf("Treiber '%s': Delta %+g als Override auf den naechsten Lauf.",
                         tr, p$delta)
    } else {
      start_m <- as.integer(p$start_m)
      dauer   <- as.integer(p$dauer)
      if (identical(p$verlauf, "einmalig")) {
        delta_monat[start_m] <- delta_monat[start_m] + p$betrag
        wirkung <- sprintf("Einmalig %+g in Monat %d.", p$betrag, start_m)
      } else {
        ende <- raten_ende(start_m, dauer, H)
        idx  <- start_m:ende
        delta_monat[idx] <- delta_monat[idx] + p$betrag / dauer
        wirkung <- sprintf(
          "Linear %+g je Monat (M%d..M%d, %d von %d Raten; Rest jenseits H abgeschnitten).",
          p$betrag / dauer, start_m, ende, length(idx), dauer)
      }
    }

    doku_id      <- c(doku_id, id)
    doku_typ     <- c(doku_typ, typ)
    doku_wirkung <- c(doku_wirkung, wirkung)
  }

  list(
    treiber_overrides = overrides,
    delta_monat       = delta_monat,
    doku              = data.frame(id = doku_id, typ = doku_typ,
                                   wirkung_text = doku_wirkung,
                                   stringsAsFactors = FALSE)
  )
}

# ----------------------------------------------------------------------------
# massnahmen_overlay(out, delta_monat): reine Projektion auf ein fertiges
# phase_simulation-Ergebnis — keine Simulation, kein RNG, keine Prints.
# Nullvektor -> das Original unveraendert (identical) zurueck.
# ----------------------------------------------------------------------------
massnahmen_overlay <- function(out, delta_monat) {
  stopifnot(is.numeric(delta_monat), length(delta_monat) == ncol(out$B))
  if (all(delta_monat == 0)) return(out)

  kum <- cumsum(delta_monat)
  out$B      <- sweep(out$B, 2, kum, "+")
  out$trough <- apply(out$B, 1, min)
  # Plan-Tiefpunkt-Pegel konstant gehalten (kein monatlicher Plan-Pfad im
  # Vertrag): dieselbe Minimum-Logik als Naeherung erster Ordnung.
  out$metrics$plan_trough <- min(out$metrics$plan_trough + kum)
  out
}

# ----------------------------------------------------------------------------
# szenario_name_mit_paket(basisname, paket): Namenskonvention fuer die
# UI-/compare-Anbindung — "<basisname> + <n> Massnahmen".
# ----------------------------------------------------------------------------
szenario_name_mit_paket <- function(basisname, paket) {
  sprintf("%s + %d Massnahmen", basisname, length(paket))
}

# ----------------------------------------------------------------------------
# szenario_mit_paket(wbpath, paket, basis_name = "Basis"): Szenariobau fuer
# den Paket-Vergleich (phase-11-massnahmen-ui) — legt den treiber-Anteil
# eines Massnahmenpakets als assume_overrides auf die Mappe
# (scenario_from_workbook SETZT die Annahme: mdl$assume[[treiber]] <- wert),
# Name nach szenario_name_mit_paket(basis_name, paket). Setzt gesourctes
# R/scenario.R voraus (einziger Baustein neben dieser Datei).
#
# Warum ein eigener Helfer statt scenarios_from_specs: scenarios_from_specs
# (R/compare_scenarios.R) traegt je spec nur EINEN benannten Ein-Treiber-
# Override — Massnahmenpakete koennen MEHRERE Treiber gleichzeitig belegen
# (quantifiziere_paket summiert je Treiber ueber alle Paket-Eintraege).
# Dieser Helfer schliesst genau diese Luecke, ohne scenarios_from_specs oder
# compare_scenarios anzufassen.
#
# Vertragsgrenzen: H = mdl$HMAX der Mappe dient allein der Validierung der
# delta-Eintraege im Paket (die treiber_overrides sind H-unabhaengig) —
# dafuer wird die Mappe zweistufig ueber scenario_from_workbook geladen,
# MC-frei (nur load_model). delta-Anteile laufen NICHT in die assumes; sie
# wirken als massnahmen_overlay auf das Simulationsergebnis (siehe
# start_paket_vergleich in app/app.R). Kein Monte-Carlo, kein RNG, keine
# Formatierung; Fehler aus quantifiziere_paket (unbekannte id, start_m
# ausserhalb 1..H) propagieren unveraendert.
# ----------------------------------------------------------------------------
szenario_mit_paket <- function(wbpath, paket, basis_name = "Basis") {
  basis <- scenario_from_workbook(wbpath, name = basis_name)
  q     <- quantifiziere_paket(paket, basis$mdl$HMAX)
  nm    <- szenario_name_mit_paket(basis_name, paket)
  # Reines delta-Paket (leere treiber_overrides): assume_overrides weglassen,
  # das Szenario ist die unveraenderte Basis unter neuem Namen.
  if (length(q$treiber_overrides))
    scenario_from_workbook(wbpath, name = nm,
                           assume_overrides = q$treiber_overrides)
  else
    scenario_from_workbook(wbpath, name = nm)
}
