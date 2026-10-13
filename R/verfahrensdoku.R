# ============================================================================
#  R/verfahrensdoku.R -- Verfahrensdokumentation (GoBD Rz. 153) der GoBD-Schicht
#  (phase-14-verfahrensdoku)
#
#  Zweck (GoBD Rz. 153): die geforderte Verfahrensdokumentation als reines,
#  DETERMINISTISCHES Objekt bauen -- allgemeine Beschreibung + Betriebsdoku NEU
#  verfasst, der vorhandene GoBD-Bestand EINSORTIERT (katalogisiert, nicht
#  gerechnet) und der Hersteller-/Betreiber-Split ehrlich ausgewiesen. Dazu die
#  Reviewer-Checkliste, die die verkuerzte Gate-Kette dieser Einheit traegt (ein
#  Pruefer arbeitet sie ab). Vorlagenfrei; schirm-los (kein UI-Element). Vertrag:
#  handoff/phase-14-verfahrensdoku.json; Spezifikation: tests/test_verfahrensdoku.R.
#
#  Reinheit: kein RNG, kein Sys.time() im Objektinhalt; die umgebung wird wie bei
#  manifest_erstellen INJIZIERT (Default umgebung_erfassen()). Die Funktion ruft
#  KEINE Kernel-Funktion und KEINE der GoBD-Bestandsfunktionen als Rechenglied --
#  sie KATALOGISIERT den Bestand nur als Text. Zweimal dieselben Argumente
#  liefern identical() dasselbe Objekt.
# ============================================================================

# Die fuenf umgebung-Felder (Vertrag identisch zu R/manifest.R -- eine Wahrheit
# pro Fakt: dieselbe Feldmenge wie umgebung_erfassen()/manifest_validieren()).
.verfahrensdoku_umg_felder <- c("r_version", "plattform", "git_sha",
                                "git_dirty", "renv_lock_sha256")

# Deterministischer umgebung-Validator im Stil manifest_validieren: stop() nennt
# den VERLETZTEN FELDNAMEN (Rz.-153-Pruefbarkeit heisst benennbare Befunde).
.verfahrensdoku_umgebung_pruefen <- function(umgebung) {
  if (!is.list(umgebung) || is.data.frame(umgebung)) {
    stop("verfahrensdoku_erstellen: umgebung muss eine Liste sein.", call. = FALSE)
  }
  fehlend <- setdiff(.verfahrensdoku_umg_felder, names(umgebung))
  if (length(fehlend)) {
    stop("verfahrensdoku_erstellen: umgebung fehlt das Feld ",
         paste(fehlend, collapse = ", "), " (Soll: ",
         paste(.verfahrensdoku_umg_felder, collapse = "/"), ").", call. = FALSE)
  }
  ueberzaehlig <- setdiff(names(umgebung), .verfahrensdoku_umg_felder)
  if (length(ueberzaehlig)) {
    stop("verfahrensdoku_erstellen: umgebung traegt unerwartete Felder ",
         paste(ueberzaehlig, collapse = ", "), " (Soll exakt: ",
         paste(.verfahrensdoku_umg_felder, collapse = "/"), ").", call. = FALSE)
  }
  invisible(TRUE)
}

# Reine Kernfunktion: baut das Verfahrensdokumentations-Objekt. Signatur wie
# manifest_erstellen -- umgebung injiziert, Default die Live-Erfassung.
verfahrensdoku_erstellen <- function(umgebung = umgebung_erfassen()) {
  .verfahrensdoku_umgebung_pruefen(umgebung)

  df <- function(...) data.frame(..., stringsAsFactors = FALSE)

  # (1) meta -- die umgebung KOMPLETT als Objekt getragen (maschinell pruefbare
  # Durchreichung), nicht nur als Prosa.
  meta <- list(
    titel = "Verfahrensdokumentation (GoBD Rz. 153) -- Liqui-App",
    geltungsbereich = paste(
      "Stochastische Krankenhaus-Liquiditaetsplanung (R-Shiny Desktop-Software):",
      "Erfassung, Verarbeitung und Nachweis der Rechenlaeufe rund um die",
      "validierte kernel-Engine. Deckt die Herstellersicht der App; die",
      "Betreiber-Pflichten (WORM/Aufbewahrungsfristen/Berechtigungswesen) sind",
      "als Grenze ausgewiesen."),
    erzeugt_umgebung = umgebung)

  # (2) allgemeine_beschreibung -- NEU verfasst; nennt Anker als geschuetzten
  # Vertrag OHNE Zahlen-Literal (driftsicher).
  allgemeine_beschreibung <- df(
    aspekt = c(
      "Systemumgebung",
      "Rechenbasis (Engine)",
      "Engine-Vertrag (Mappe)",
      "Schutz der drei Tier-1-Anker"),
    beschreibung = c(
      paste("R-Shiny Desktop-Software fuer die stochastische",
            "Krankenhaus-Liquiditaetsplanung; die identifizierte Systemumgebung",
            "wird je Lauf ueber r_version/plattform/git_sha/git_dirty/",
            "renv_lock_sha256 erfasst und im Run-Manifest getragen."),
      paste("kernel/engine.R und kernel/distributions.R sind die validierte",
            "Engine; sie werden VERPACKT, nicht neu geschrieben (Kernel-Guard --",
            "jede Verhaltensaenderung gehoert in einen Wrapper in R/)."),
      paste("Der Engine-Vertrag ist die Mappe: Blattnamen exakt",
            "01_Positionen, 02_Zahlungsprofile, 03_Stammdaten_Eroeffnung,",
            "04_Annahmen, 05_Finanzierungsplan, 06_Verteilungen, 07_Korrelation,",
            "08_Steuern, 09_Darlehen, 10_OPOS, Kopfzeile in Zeile 4 (skip = 3),",
            "Historie >= 12 Monate."),
      paste("Die drei Tier-1-Anker (Historie / Tiefpunkt M3 / Planergebnis)",
            "sind unantastbar und werden bei jedem Lauf ueber tests/run_all.R",
            "sowie die IKS-Kontrolle anker_plan_tiefpunkt gesichert -- Nennung",
            "als geschuetzter Vertrag, kein hartcodierter Zahlenwert.")),
    verantwortung = c("hersteller", "hersteller", "gemeinsam", "hersteller"))

  # (3) betriebsdoku -- NEU verfasst; Betrieb der App PLUS die ehrlich als
  # 'betreiber' ausgewiesenen Pflichten und Grenzen.
  betriebsdoku <- df(
    aspekt = c(
      "Einrichtung",
      "Kanonischer Lauf (DOD)",
      "Nebenlaeufigkeit",
      "Determinismus",
      "Zahlenformatierung",
      "Nachweis-Ablage",
      "WORM-/Dateisystem-Schutz",
      "Aufbewahrungsfristen",
      "Berechtigungswesen"),
    beschreibung = c(
      paste("R und die benoetigten Pakete werden ueber tests/deps.R installiert;",
            "dokumentierte renv.lock-Luecke -- der Paketstand ist bis zur",
            "renv-Einfuehrung ueber tests/deps.R im git_sha mitversioniert."),
      paste("Ein kanonischer Lauf ist die Definition of Done:",
            "Rscript tests/run_all.R (Anker + Golden-Master + testthat-Suite)."),
      paste("Schwere Monte-Carlo (Simulation/Varianz) laeuft im",
            "mirai/future-Hintergrund-Worker, nie synchron im UI-Thread."),
      paste("Reproduzierbarkeit ueber set.seed; die Seeds sind Teil des",
            "Szenario-Schluessels (gleicher Schluessel -> byte-identisches Ergebnis)."),
      paste("Deutsche Zahlenformatierung ausschliesslich ueber fmt() aus dem",
            "kernel (eine Wahrheit fuer das Anzeigeformat)."),
      paste("Jeder Lauf wird unter runs/<run_id>/ abgelegt; die Ablage ist",
            "write-once auf Anwendungsebene (Manifest/Protokoll/Nachweise werden",
            "nicht ueberschrieben) -- byte-identisch nachvollziehbar."),
      paste("WORM-/Dateisystem-Schutz der Nachweis-Ablage ist Betreiberpflicht:",
            "das write-once auf Anwendungsebene ersetzt ihn NICHT (siehe die",
            "Kommentare in R/manifest.R und R/protokoll.R)."),
      paste("Aufbewahrungsfristen der Nachweise sind ausdruecklich NICHT",
            "Baustelle der App, sondern Betreiberpflicht -- dokumentierte,",
            "ehrlich ausgewiesene Grenze."),
      paste("Berechtigungswesen / Zugriffssteuerung ist ausdruecklich NICHT",
            "Baustelle der App, sondern Betreiberpflicht -- dokumentierte,",
            "ehrlich ausgewiesene Grenze.")),
    verantwortung = c("gemeinsam", "hersteller", "hersteller", "hersteller",
                      "hersteller", "hersteller", "betreiber", "betreiber",
                      "betreiber"))

  # (4) bestand -- der vorhandene GoBD-Bestand EINSORTIERT (katalogisiert),
  # verantwortung 'hersteller', datei-Eintraege repo-relativ und real auf Platte.
  bestand <- df(
    komponente = c(
      "Run-Manifest (Kern)",
      "Run-Manifest (Anbindung)",
      "Verfahrensprotokoll",
      "Finalisierung",
      "Pruefbarkeit/Herkunft",
      "Z3-Datentraegerueberlassung"),
    datei = c(
      "R/manifest.R",
      "R/manifest_anbindung.R",
      "R/protokoll.R",
      "R/finalisierung.R",
      "R/herkunft.R",
      "R/z3.R"),
    funktionen = c(
      paste("sha256_datei, umgebung_erfassen, manifest_erstellen,",
            "manifest_validieren, manifest_schreiben, manifest_lesen"),
      "simulation_mit_manifest",
      ".protokoll_hash, protokoll_anhaengen, protokoll_pruefen",
      "lauf_status, lauf_finalisieren, lauf_vorgaenger, korrektur_verketten",
      "herkunft_bauen",
      "export_z3_csv, export_z3_index, export_z3"),
    verfahrensteil = c(
      "Nachvollziehbarkeit/Nachpruefbarkeit (Rz. 152/154)",
      "Lauf-Hook + IKS-Kontrollen (Rz. 100/152)",
      "Tamper-evidente Hash-Kette (Rz. 58 ff./107)",
      "Entwurf/Final + Korrektur als neuer Lauf (IDW PS 880-Anschluss)",
      "Provenanz Kennzahl -> Manifest -> Blatt/Zelle",
      "CSV + index.xml nach Beschreibungsstandard (Z3)"),
    verantwortung = rep("hersteller", 6L))

  # (5) review -- Reviewer-Checkliste; traegt die reduzierte Gate-Kette dieser
  # Einheit. >= 8 Punkte ueber die Kategorien anker/nachweis/protokoll/
  # finalisierung/herkunft/z3/split/grenze (Spaltenvertrag exakt
  # nummer/kategorie/punkt).
  review <- df(
    nummer = 1:8,
    kategorie = c("anker", "nachweis", "protokoll", "finalisierung",
                  "herkunft", "z3", "split", "grenze"),
    punkt = c(
      paste("Die drei Tier-1-Anker sind bei jedem Lauf gesichert",
            "(tests/run_all.R + IKS-Kontrolle anker_plan_tiefpunkt); keine",
            "Drift der Kopfzahlen."),
      paste("Jeder Lauf ist ueber runs/<run_id>/ mit Run-Manifest byte-identisch",
            "nachvollziehbar (Eingaben SHA-256-gehasht, Umgebung erfasst)."),
      paste("Das Verfahrensprotokoll ist tamper-evident (protokoll_pruefen",
            "verifiziert die Hash-Kette); kein Hash prueft sich selbst."),
      paste("Finalisierte Laeufe sind app-seitig gesperrt; eine Korrektur ist",
            "nur ein neuer Lauf mit vorgaenger_run_id (korrektur_verketten)."),
      paste("Die Herkunft der drei Anker ist manifestgestuetzt bis auf",
            "Blatt/Zelle rueckverfolgbar (herkunft_bauen)."),
      paste("Das Z3-Buendel liefert CSV + index.xml nach Beschreibungsstandard",
            "(export_z3)."),
      paste("Hersteller-/Betreiber-Split ist ehrlich ausgewiesen; die",
            "verantwortung ist durchgehend {hersteller, betreiber, gemeinsam}."),
      paste("Die Betreiber-Pflichten WORM/Aufbewahrungsfristen/Berechtigungswesen",
            "sind NICHT Teil der App -- dokumentierte, nicht verschwiegene Grenze.")))

  list(meta = meta,
       allgemeine_beschreibung = allgemeine_beschreibung,
       betriebsdoku = betriebsdoku,
       bestand = bestand,
       review = review)
}

# ============================================================================
#  export_verfahrensdoku (phase-15-verfahrensdoku-export)
#
#  Reine, DETERMINISTISCHE Serialisierung des Verfahrensdokumentations-Objekts
#  vd (Rueckgabe von verfahrensdoku_erstellen) in GENAU EINE xlsx-Datei. Kein
#  Rechenglied: vd kommt fertig herein, es wird KEINE Kernel- oder GoBD-
#  Bestandsfunktion aufgerufen, kein RNG, kein Zeitstempel im Zellinhalt (die
#  xlsx-Dateibytes duerfen abweichen, der GELESENE Zellinhalt nicht). Nur
#  unformatierte Strings/Zahlen -- fmt() bleibt der UI.
#
#  Die VD ist strukturell verschieden von den Ergebnis-Exporten (R/export.R,
#  export_bloecke_bauen kennt nur die fuenf Ergebnis-Namen): eine meta-Liste
#  plus vier data.frames. Sie bekommt darum ihren EIGENEN, duennen
#  Serialisierer hier im VD-Modul -- NICHT in R/export.R.
#
#  Fuenf Blaetter in der Reihenfolge names(vd):
#    * meta  -- Key/Wert-Projektion (Spalten feld/wert, homogen as.character)
#      ueber die sieben Felder titel, geltungsbereich und die fuenf umgebung-
#      Felder r_version/plattform/git_sha/git_dirty/renv_lock_sha256 aus
#      vd$meta$erzeugt_umgebung. Die umgebung-Durchreichung aus phase-14
#      ueberlebt den Export (maschinell pruefbar); logisches TRUE/FALSE wird
#      via as.character zum Text "TRUE"/"FALSE".
#    * allgemeine_beschreibung, betriebsdoku, bestand, review -- die jeweiligen
#      data.frames UNVERAENDERT in native Typen (review$nummer bleibt Zahl).
#
#  Rueckgabe list(pfad, blaetter): pfad identical dem uebergebenen Pfad,
#  blaetter ein character der Blattnamen in Schreibreihenfolge (== names(vd)).
# ============================================================================

# Die fuenf Sektionen des VD-Objekts (Vertrag der Rueckgabe von
# verfahrensdoku_erstellen -- eine Wahrheit pro Fakt).
.verfahrensdoku_export_sektionen <- c("meta", "allgemeine_beschreibung",
                                      "betriebsdoku", "bestand", "review")

# Die sieben Felder des meta-Blatts (titel/geltungsbereich + die fuenf
# umgebung-Felder). Ordnung ist Vertrag (Ruecklese-Treue-Orakel).
.verfahrensdoku_export_meta_felder <- c("titel", "geltungsbereich",
                                        .verfahrensdoku_umg_felder)

# Deterministischer Validator im Stil .verfahrensdoku_umgebung_pruefen: stop()
# nennt die VERLETZTE Sektion bzw. das Argument (benennbare Befunde).
.verfahrensdoku_export_pruefen <- function(vd, pfad) {
  sekt <- .verfahrensdoku_export_sektionen
  if (!is.list(vd) || is.data.frame(vd)) {
    stop("export_verfahrensdoku: vd muss eine Liste mit den Sektionen ",
         paste(sekt, collapse = ", "), " sein.", call. = FALSE)
  }
  fehlend <- setdiff(sekt, names(vd))
  if (length(fehlend)) {
    stop("export_verfahrensdoku: vd fehlt die Sektion ",
         paste(fehlend, collapse = ", "), " (Soll: ",
         paste(sekt, collapse = "/"), ").", call. = FALSE)
  }
  ueberzaehlig <- setdiff(names(vd), sekt)
  if (length(ueberzaehlig)) {
    stop("export_verfahrensdoku: vd traegt unerwartete Sektionen ",
         paste(ueberzaehlig, collapse = ", "), " (Soll exakt: ",
         paste(sekt, collapse = "/"), ").", call. = FALSE)
  }
  # EXAKT fuenf Sektionen, je genau einmal: das setdiff-Paar oben laesst einen
  # DOPPELTEN erlaubten Namen (z.B. zwei "meta") durch -- beide setdiffs sind
  # dann leer. Nach den setdiffs gilt: Laenge != 5 genau dann, wenn ein
  # Duplikat vorliegt -- die frueher zusaetzlich gepruefte Laengenbedingung
  # war tot und machte den ||-Operator unbeobachtbar (aequivalenter Mutant,
  # Mutgate-Befund M2 12.07.2026); Redundanz entfernt statt eine untestbare
  # Bedingung zu behalten. Ein Duplikat gaebe ein doppeltes Blatt bzw. einen
  # spaeten openxlsx2-Fehler; hier benennbar (mit dem doppelten Namen)
  # abfangen.
  doppelt <- unique(names(vd)[duplicated(names(vd))])
  if (length(doppelt)) {
    stop("export_verfahrensdoku: vd traegt Sektionen doppelt/ueberzaehlig",
         if (length(doppelt))
           paste0(" (doppelt: ", paste(doppelt, collapse = ", "), ")") else "",
         " (Soll exakt je einmal: ", paste(sekt, collapse = "/"), ").",
         call. = FALSE)
  }
  if (!is.character(pfad) || length(pfad) != 1L || is.na(pfad) ||
      !nzchar(pfad)) {
    stop("export_verfahrensdoku: pfad muss ein einzelner String sein.",
         call. = FALSE)
  }
  invisible(TRUE)
}

export_verfahrensdoku <- function(vd, pfad) {
  .verfahrensdoku_export_pruefen(vd, pfad)

  # meta -- Key/Wert-Projektion; die umgebung-Felder aus der DURCHGEREICHTEN
  # erzeugt_umgebung (phase-14). as.character homogenisiert die wert-Spalte
  # (logisches git_dirty -> Text "TRUE"/"FALSE").
  umg <- vd$meta$erzeugt_umgebung
  meta_blatt <- data.frame(
    feld = .verfahrensdoku_export_meta_felder,
    wert = as.character(c(vd$meta$titel, vd$meta$geltungsbereich,
                          umg$r_version, umg$plattform, umg$git_sha,
                          umg$git_dirty, umg$renv_lock_sha256)),
    stringsAsFactors = FALSE)

  # Genau fuenf Blaetter in der Reihenfolge names(vd) (nach der Pruefung eine
  # Permutation von .verfahrensdoku_export_sektionen; die kanonische Ordnung
  # kommt aus verfahrensdoku_erstellen). meta als Projektion, die vier
  # data.frames unveraendert in nativen Typen.
  wb <- openxlsx2::wb_workbook()
  for (nm in names(vd)) {
    blatt <- if (identical(nm, "meta")) meta_blatt else vd[[nm]]
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = nm)
    wb <- openxlsx2::wb_add_data(wb, sheet = nm, x = blatt)
  }
  openxlsx2::wb_save(wb, file = pfad, overwrite = TRUE)

  list(pfad = pfad, blaetter = names(vd))
}
