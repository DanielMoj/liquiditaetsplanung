# ============================================================================
#  R/herkunft.R  —  GoBD-Herkunft eines Exports (phase-13-pruefbarkeit)
#
#  Zweck (GoBD Rz. 152/154: Nachvollziehbarkeit/Pruefbarkeit jeder
#  Ergebnisgroesse): fuer einen Export wird ein Run-ID- plus Versions-/Hash-
#  Stempel gebildet UND eine Herkunftstabelle, die jeden der drei Tier-1-Anker
#  (Historie / Tiefpunkt M3 / Planergebnis) an das Manifest und an Blatt/Zelle
#  im selben Export bindet — Kennzahl -> Manifest -> Blatt/Zelle.
#
#  herkunft_bauen(forecast, manifest_pfad) ist REIN und DETERMINISTISCH: KEIN
#  RNG, kein Sys.time(), keine Engine, kein Grafikgeraet. Es liest NUR
#    (a) die drei Tier-1-Anker aus dem UEBERGEBENEN forecast (Phase-1-Vertrag
#        R/phase_forecast.R): Historie = forecast$validation$erg_eng,
#        Tiefpunkt = forecast$kpis$trough, Planergebnis = forecast$kpis$result;
#    (b) den Lauf-Nachweis aus manifest_lesen(manifest_pfad) PLUS den EXTERN
#        nachgerechneten Datei-Hash sha256_datei(manifest_pfad).
#
#  KEIN HASH PRUEFT SICH SELBST (Blockprinzip c17d29a): der manifest_sha256
#  steht NICHT im Manifest (das kann er zirkulaer nicht) — er wird ueber die
#  rohen Bytes der Manifest-Datei gerechnet und ist damit unabhaengig vom
#  Manifest-Inhalt nachpruefbar. run_id/manifest_sha256 in JEDER Herkunftszeile
#  identisch zum Stempel (eine Wahrheit pro Fakt, NICHT je Zeile neu gezogen).
#
#  BESTANDSSCHUTZ: die Funktion ruft NUR die Bestands-Funktionen von
#  R/manifest.R auf (manifest_lesen, sha256_datei) — sie beruehrt weder
#  kernel/ noch R/manifest.R noch R/export.R selbst.
# ============================================================================

# Vertragskonstanten (EINE Quelle): die sechs Stempel in Vertragsreihenfolge
# und die sechs Herkunftsspalten.
.herkunft_stempel_keys <- c("run_id", "manifest_sha256", "git_sha",
                            "renv_lock_sha256", "r_version", "zeitstempel")
.herkunft_spalten      <- c("kennzahl", "wert", "run_id", "manifest_sha256",
                            "blatt", "zelle")

herkunft_bauen <- function(forecast, manifest_pfad) {
  # --- Fehlerfaelle als benannte Befunde (Stil manifest_validieren) ---------
  if (!is.character(manifest_pfad) || length(manifest_pfad) != 1L ||
      is.na(manifest_pfad) || !nzchar(manifest_pfad)) {
    stop("herkunft_bauen: manifest_pfad muss ein einzelner nicht leerer String sein.",
         call. = FALSE)
  }
  if (!file.exists(manifest_pfad)) {
    stop("herkunft_bauen: Manifest-Datei nicht gefunden: ", manifest_pfad, call. = FALSE)
  }
  # Existenz EXAKT ueber names() pruefen, nicht ueber `$`: `$`/`[[` matchen auf
  # Listen partiell (forecast_kpis-Vertrag c(trough, trough_month, ...) => `$trough`
  # wuerde nach dem Entfernen von trough still auf trough_month zeigen).
  if (!is.list(forecast) || !is.list(forecast$validation) ||
      !("erg_eng" %in% names(forecast$validation)) ||
      is.null(forecast$validation[["erg_eng", exact = TRUE]])) {
    stop("herkunft_bauen: forecast$validation$erg_eng fehlt (Historie-Anker).",
         call. = FALSE)
  }
  if (!is.list(forecast$kpis) || !("trough" %in% names(forecast$kpis)) ||
      is.null(forecast$kpis[["trough", exact = TRUE]])) {
    stop("herkunft_bauen: forecast$kpis$trough fehlt (Tiefpunkt-Anker).",
         call. = FALSE)
  }
  if (!("result" %in% names(forecast$kpis)) ||
      is.null(forecast$kpis[["result", exact = TRUE]])) {
    stop("herkunft_bauen: forecast$kpis$result fehlt (Planergebnis-Anker).",
         call. = FALSE)
  }

  # --- Lauf-Nachweis: Manifest zurueckgelesen + EXTERN gerechneter Hash ------
  ml   <- manifest_lesen(manifest_pfad)          # run_id + Versions-Stempel
  hash <- sha256_datei(manifest_pfad)            # ueber die rohen Bytes, nicht
  run_id <- ml$run_id                            # aus dem Manifest gelesen

  # STEMPEL-Block: Key/Wert, alles Zeichenketten, Vertragsreihenfolge. Die
  # uebrigen Stempel stammen aus manifest_lesen (eine Wahrheit pro Fakt);
  # git_sha/renv_lock_sha256 bleiben NA, wenn das Manifest sie so dokumentiert.
  stempel <- data.frame(
    kennzahl = .herkunft_stempel_keys,
    wert     = c(run_id, hash, ml$umgebung$git_sha, ml$umgebung$renv_lock_sha256,
                 ml$umgebung$r_version, ml$zeitstempel),
    stringsAsFactors = FALSE)

  # HERKUNFT-Block: drei Zeilen in Vertragsreihenfolge; wert numerisch (Namen
  # gestrippt, damit die Anker-Vergleiche namensblind sind), uebrige Spalten
  # Zeichenketten. Blatt/Zelle zeigen, wo der Ankerwert im selben Export Zelle
  # fuer Zelle sitzt (forecast_kpis-Vertrag, Phase 9); die Historie ist in
  # KEINEM Datenblatt materialisiert, allein im Herkunfts-Nachweis.
  herkunft <- data.frame(
    kennzahl        = c("Historie (Tier-1)", "Tiefpunkt M3 (Tier-1)",
                        "Planergebnis (Tier-1)"),
    wert            = c(as.numeric(forecast$validation$erg_eng),
                        as.numeric(forecast$kpis$trough),
                        as.numeric(forecast$kpis$result)),
    run_id          = rep(run_id, 3L),
    manifest_sha256 = rep(hash, 3L),
    blatt           = c("Herkunft", "forecast_kpis", "forecast_kpis"),
    zelle           = c("Historie (Tier-1)", "trough", "result"),
    stringsAsFactors = FALSE)

  list(stempel = stempel, herkunft = herkunft)
}
