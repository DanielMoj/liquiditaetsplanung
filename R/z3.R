# ============================================================================
#  R/z3.R  —  GoBD-Z3-Export (Datentraegerueberlassung, Rz. 158 ff.)
#
#  Schicht 1/3: export_z3_csv schreibt die EINGABEN, ZWISCHENGROESSEN und
#  ERGEBNISSE eines Laufs als CSV-Dateien in ein Verzeichnis. Schicht 2/3
#  (export_z3_index) baut die index.xml-Beschreibung, Schicht 3/3
#  (export_z3) komponiert beides und haengt die App an.
#
#  CSV-Format (deutsche Datentraegerueberlassung): UTF-8, Semikolon-Trenner,
#  DezimalKOMMA ohne Tausenderpunkt, CRLF-Zeilenende, KEIN Quoting. Kopfzeile
#  = Spaltennamen. Zellwerte sind numerisch (fixed-notation, Dezimalkomma,
#  keine wissenschaftliche Notation) oder einfache Bezeichner ohne
#  Semikolon/Anfuehrungszeichen/Zeilenumbruch.
#
#  Reine Funktion, deterministisch: kein Sys.time() im Inhalt, kein RNG, kein
#  Grafikgeraet. Sie ruft KEINE Kernel-Funktion — die Ergebnis-Bloecke kommen
#  aus der EINEN Quelle export_bloecke_bauen (R/export.R, nur aufgerufen, nicht
#  veraendert), damit CSV und xlsx dieselben Vertragszahlen projizieren.
#  Bestandsschutz: beruehrt weder kernel/ noch R/export.R noch R/manifest.R.
# ============================================================================

# ---------------------------------------------------------------------------
# Format-Heimat nachladen: fmt_maschine (R/format.R) ist die EINE Wahrheit der
# maschinenlesbaren Zahl-Serialisierung. Die geschuetzte Spec sourct R/z3.R
# direkt (ohne R/format.R) — darum laedt z3.R seine Format-Heimat selbst,
# falls sie im Aufruf-Kontext noch fehlt. Wurzel-Suche wie in den Tests:
# aufwaerts von getwd(), bis kernel/engine.R den Repo-Anker markiert.
# ---------------------------------------------------------------------------
if (!exists("fmt_maschine", mode = "function")) local({
  wurzel <- getwd()
  for (i in 1:6) {
    if (file.exists(file.path(wurzel, "kernel", "engine.R"))) break
    wurzel <- dirname(wurzel)
  }
  source(file.path(wurzel, "R", "format.R"))
})

# ---------------------------------------------------------------------------
# .z3_zahl: duenne Delegation an die zentrale Format-Heimat fmt_maschine
# (R/format.R). Dort liegen Verhalten (formatC "fg", 15 signifikante Stellen,
# keine Gruppierung, Dezimalkomma, drop0trailing) und das Verhaeltnis zur
# Anzeige-Regel (Anzeige via fmt, Maschine via fmt_maschine) — keine lokale
# Zahlenformatierung in dieser Exportschicht.
# ---------------------------------------------------------------------------
.z3_zahl <- function(x) fmt_maschine(x)

# ---------------------------------------------------------------------------
# .z3_zeilen: eine data.frame-Projektion auf CSV-Zeilen (Kopfzeile = Spalten-
# namen, Semikolon-getrennt). Numerische Spalten laufen ueber .z3_zahl
# (Dezimalkomma), Text-Spalten unveraendert (Position-Namen, Mappe/Szenario).
#
# Der GoBD-CSV-Vertrag ist quoting-frei: eine Textzelle mit ';' (Trenner),
# '"' oder einem Zeilenumbruch (CR/LF) wuerde den Aufbau still zerreissen.
# Solche Zellen werden vor der Projektion mit einem benannten Fehler
# (Stil manifest_validieren) abgewiesen, statt korruptes CSV zu schreiben.
# .z3_zahl-Ausgaben sind per Konstruktion sonderzeichenfrei (nur Ziffern,
# '-' und ','); geprueft werden daher nur die nicht-numerischen Spalten.
# ---------------------------------------------------------------------------
.z3_zeilen <- function(df) {
  cols <- lapply(df, function(col) {
    if (is.numeric(col)) return(.z3_zahl(col))
    txt <- as.character(col)
    verboten <- grepl("[;\r\n\"]", txt)
    if (any(verboten))
      stop("export_z3_csv: Textzelle enthaelt ein den quoting-freien GoBD-CSV-",
           "Vertrag verletzendes Sonderzeichen (';', '\"', CR oder LF): '",
           txt[which(verboten)[1L]], "'", call. = FALSE)
    txt
  })
  header <- paste(names(df), collapse = ";")
  if (nrow(df) == 0L) return(header)
  daten <- vapply(seq_len(nrow(df)),
                  function(i) paste(vapply(cols, `[[`, character(1L), i),
                                    collapse = ";"),
                  character(1L))
  c(header, daten)
}

# ---------------------------------------------------------------------------
# .z3_schreiben: CSV-Zeilen als UTF-8-Bytes mit CRLF-Zeilenende schreiben.
# writeBin umgeht jede plattformseitige Zeilenumbruch-Uebersetzung — jedes
# 0x0A wird von 0x0D eingeleitet, auch unter Windows.
# ---------------------------------------------------------------------------
.z3_schreiben <- function(zeilen, pfad) {
  inhalt <- paste0(paste(zeilen, collapse = "\r\n"), "\r\n")
  writeBin(charToRaw(enc2utf8(inhalt)), pfad)
  invisible(pfad)
}

# ---------------------------------------------------------------------------
# .Z3_CSV_DATEIEN: die VOLLSTAENDIGE, geschlossene Menge der CSV-Dateinamen, die
# diese Schicht je erzeugt — eingaben.csv plus jeder moegliche Block aus
# export_bloecke_bauen (forecast expandiert zu forecast + forecast_kpis; die
# uebrigen Bloecke tragen ihren Ergebnisnamen). Die Idempotenz-Loeschung raeumt
# AUSSCHLIESSLICH diese bekannten Namen ab — fremde .csv im Zielverzeichnis
# (Nutzerdaten, Fremd-Exporte) bleiben unangetastet.
# ---------------------------------------------------------------------------
.Z3_CSV_DATEIEN <- c("eingaben.csv", "forecast.csv", "forecast_kpis.csv",
                     "sim_monate.csv", "sim_deckung.csv",
                     "varianzzerlegung.csv", "vergleich.csv")

# ---------------------------------------------------------------------------
# export_z3_csv(ergebnisse, sc, pfad): Schicht 1/3 des GoBD-Z3-Exports.
#
#   ergebnisse — BENANNTE Ergebnisobjekt-Liste (Vertrag wie export_xlsx:
#     forecast, sim_monate, sim_deckung, varianzzerlegung, vergleich; nur die
#     vorhandenen Eintraege). Die Validierung ist an export_bloecke_bauen
#     delegiert (leere/unbenannte/unbekannte Liste -> benannter stop dort).
#   sc — liqui_scenario (liefert die Eingaben).
#   pfad — Zielverzeichnis (rekursiv angelegt; ein wiederholter Lauf ist
#     idempotent UND fehleratomar: ALLE CSV-Inhalte werden VOR jedem
#     Seiteneffekt am Zielverzeichnis vollstaendig gebaut und validiert
#     (Sonderzeichen-Waechter feuert, bevor irgendetwas geloescht oder
#     geschrieben ist), dann in ein frisches Temp-Unterverzeichnis innerhalb
#     von pfad geschrieben; erst DANACH werden die bekannten Z3-Block-CSVs
#     (.Z3_CSV_DATEIEN) eines frueheren Exports entfernt (mit Pruefung —
#     scheitert eine Entfernung, bricht der Export VOR dem Ersetzen ab und
#     der Altbestand bleibt konsistent) und die neuen Dateien per file.rename
#     an ihren Zielort gebracht. Fremde .csv (Nutzerdaten), index.xml/PDF
#     anderer Schichten bleiben unberuehrt. write-once liegt auf Run-Ebene
#     im Manifest, nicht hier).
#
# Schreibt eingaben.csv (Key/Wert der Eingaben in Vertragsreihenfolge) sowie
# je Ergebnis-Block aus export_bloecke_bauen EINE CSV gleichen Namens
# (forecast -> forecast.csv + forecast_kpis.csv, ...). KEIN index.xml
# (Schicht 2/3), KEIN PDF/A. Rueckgabe list(pfad = <Eingabeverzeichnis,
# unveraendert>, dateien = <geschriebene CSV-Dateinamen in Schreibreihenfolge>).
# ---------------------------------------------------------------------------
export_z3_csv <- function(ergebnisse, sc, pfad) {
  # (1) Ergebnis-Bloecke aus der EINEN Quelle; validiert ergebnisse (delegiert).
  bloecke <- export_bloecke_bauen(ergebnisse)

  if (!inherits(sc, "liqui_scenario"))
    stop("export_z3_csv: sc muss der Klasse liqui_scenario angehoeren.",
         call. = FALSE)
  if (!is.character(pfad) || length(pfad) != 1L || is.na(pfad) || !nzchar(pfad))
    stop("export_z3_csv: pfad muss ein nicht-leerer String (Zielverzeichnis) sein.",
         call. = FALSE)

  # (2) eingaben.csv-Inhalt — Eingaben in Vertragsreihenfolge (kennzahl/wert).
  #     Text-Werte (Mappe, Szenario) unveraendert, Zahlen ueber .z3_zahl;
  #     Historie_GuV nur, wenn forecast vorliegt (Kalibrierungs-Eingabeanker);
  #     danach je ein Treiber aus sc$assume in sc$assume-Reihenfolge.
  kennzahl <- c("Mappe", "Szenario", "HMAX", "Sim_Pfade", "Sim_Seed",
                "Open_Cash", "KK_Zins", "KK_Limit")
  wert <- c(basename(sc$workbook),
            sc$name,
            .z3_zahl(sc$mdl$HMAX),
            .z3_zahl(sc$sim$paths),
            .z3_zahl(sc$sim$seed),
            .z3_zahl(sc$mdl$open_cash),
            .z3_zahl(sc$mdl$kk_zins),
            .z3_zahl(sc$mdl$kk_limit))
  if ("forecast" %in% names(ergebnisse)) {
    kennzahl <- c(kennzahl, "Historie_GuV")
    wert     <- c(wert, .z3_zahl(ergebnisse$forecast$validation$erg_eng))
  }
  for (nm in names(sc$assume)) {
    kennzahl <- c(kennzahl, nm)
    wert     <- c(wert, .z3_zahl(sc$assume[[nm]]))
  }
  eingaben_df <- data.frame(kennzahl = kennzahl, wert = wert,
                            stringsAsFactors = FALSE)

  # (3) ALLE CSV-Zeilen VORAB erzeugen, in Schreibreihenfolge: erst eingaben,
  #     dann die Bloecke in export_bloecke_bauen-Reihenfolge. Hier feuert der
  #     Sonderzeichen-Waechter (.z3_zeilen), BEVOR am Zielverzeichnis irgend-
  #     etwas geloescht oder geschrieben ist — Fehleratomaritaet.
  quellen <- c(list(eingaben = eingaben_df), bloecke)
  zeilen  <- lapply(quellen, .z3_zeilen)
  dateien <- paste0(names(quellen), ".csv")

  # Zielverzeichnis anlegen (rekursiv); scheitert es -> benannter Fehler MIT Pfad.
  if (!dir.exists(pfad)) {
    ok <- suppressWarnings(dir.create(pfad, recursive = TRUE, showWarnings = FALSE))
    if (!isTRUE(ok) || !dir.exists(pfad))
      stop("export_z3_csv: Zielverzeichnis nicht anlegbar: ", pfad, call. = FALSE)
  }

  # (4) In ein frisches Temp-Unterverzeichnis INNERHALB von pfad schreiben
  #     (gleicher Datentraeger -> file.rename ist spaeter ein reiner Move).
  #     on.exit raeumt das Temp-Verzeichnis auf JEDEM Ausgang restlos ab.
  tmp <- file.path(pfad, paste0(".z3-tmp-", Sys.getpid()))
  unlink(tmp, recursive = TRUE)
  ok <- suppressWarnings(dir.create(tmp, showWarnings = FALSE))
  if (!isTRUE(ok) || !dir.exists(tmp))
    stop("export_z3_csv: Temp-Verzeichnis nicht anlegbar: ", tmp, call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  for (i in seq_along(dateien))
    .z3_schreiben(zeilen[[i]], file.path(tmp, dateien[[i]]))

  # (5) Idempotenz: alte Z3-Block-CSVs eines frueheren Laufs entfernen — sonst
  #     blieben Block-CSVs eines anderen Ergebnis-Satzes (z.B. sim_monate.csv
  #     aus einem Lauf MIT Simulation) im Buendel liegen. Geloescht werden
  #     AUSSCHLIESSLICH die bekannten Namen (.Z3_CSV_DATEIEN) der obersten
  #     Ebene — fremde .csv (Nutzerdaten), index.xml (Schicht 2/3), PDF/A und
  #     Unterverzeichnisse bleiben unberuehrt. Scheitert eine Entfernung:
  #     benannter Fehler MIT Dateipfad und Abbruch VOR dem Ersetzen — der
  #     Altbestand bleibt konsistent (on.exit raeumt das Temp-Verzeichnis).
  alte <- file.path(pfad, .Z3_CSV_DATEIEN)
  alte <- alte[file.exists(alte) & !dir.exists(alte)]
  if (length(alte)) {
    entfernt <- suppressWarnings(file.remove(alte))
    if (!all(entfernt))
      stop("export_z3_csv: alte Z3-CSV nicht entfernbar: ",
           alte[which(!entfernt)[1L]], call. = FALSE)
  }

  # (6) Neue Dateien an ihren Zielort. file.rename ist auf demselben Daten-
  #     traeger atomar; liefert es FALSE (z.B. exotisches Dateisystem), greift
  #     file.copy MIT Pruefung als Fallback.
  for (d in dateien) {
    von  <- file.path(tmp,  d)
    nach <- file.path(pfad, d)
    if (!isTRUE(suppressWarnings(file.rename(von, nach)))) {
      kopiert <- suppressWarnings(file.copy(von, nach, overwrite = TRUE))
      if (!isTRUE(kopiert) || !file.exists(nach))
        stop("export_z3_csv: Datei nicht an den Zielort uebertragbar: ",
             nach, call. = FALSE)
    }
  }

  list(pfad = pfad, dateien = dateien)
}

# ===========================================================================
#  Schicht 2/3 — export_z3_index: index.xml des GoBD-Z3-Beschreibungsstandards
#  (Anlage GoBD 2024) fuer ein Verzeichnis mit CSV-Dateien.
#
#  VON HAND gebaut, KEIN xml2 (kein neues Paket, deps.R bleibt unveraendert):
#  XML-Deklaration, Wurzel <DataSet version='2.0'> ... </DataSet> mit
#  <DataName>/<DataDescription>; je uebergebener CSV EIN <Table>-Element
#  (Content-Modell, keine Attribute) mit <URL>, <Name>, <Description>,
#  <Hash>sha256:<64-Hex></Hash>, <Validity>/<Range>/<From>/<To> und je Spalte
#  <Variable>/<Name>/<Type>. Reine Funktion, deterministisch (kein Sys.time()
#  im Inhalt, kein RNG, kein Grafikgeraet); ruft KEINE Kernel-Funktion, nutzt
#  fuer die Datei-Hashes ausschliesslich sha256_datei (R/manifest.R,
#  unantastbarer Bestand — nur aufgerufen).
#
#  KEIN HASH PRUEFT SICH SELBST: jeder <Hash> ist die SHA-256 ueber die rohen
#  CSV-Bytes (extern nachpruefbar); index.xml traegt KEINEN Hash seiner selbst.
#  Der Bestand der indexierten Dateien steht fest — dateien bestimmt ihn
#  DETERMINISTISCH, es gibt KEINEN Verzeichnis-Scan.
# ===========================================================================

# ---------------------------------------------------------------------------
# .z3_xml_escape: die drei textrelevanten XML-Metazeichen im Elementinhalt
# neutralisieren (& zuerst, damit die eingefuegten Entity-& nicht erneut
# maskiert werden). URL/Name/Description/Spaltennamen laufen darueber; die
# Hash-Hexstrings und die numerischen Range-Grenzen sind per Konstruktion
# metazeichenfrei.
# ---------------------------------------------------------------------------
.z3_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ---------------------------------------------------------------------------
# .z3_spalten_typ: GoBD-Spaltentyp aus den Zellwerten. Eine Spalte ist
# "numeric", wenn jede NICHT-leere Zelle dem quoting-freien GoBD-Zahlformat
# (optionales Vorzeichen, Ziffern, optional Dezimalkomma) entspricht, sonst
# "alphanumeric". Eine spaltenlose oder rein leere Spalte gilt als
# alphanumeric (kein numerischer Nachweis).
# ---------------------------------------------------------------------------
.z3_spalten_typ <- function(zellen) {
  zellen <- zellen[nzchar(zellen)]
  if (!length(zellen)) return("alphanumeric")
  if (all(grepl("^-?[0-9]+(,[0-9]+)?$", zellen))) "numeric" else "alphanumeric"
}

# ---------------------------------------------------------------------------
# .z3_table_xml: baut das <Table>-Fragment fuer EINE CSV-Datei. Spaltennamen
# und Spaltenanzahl stammen aus der CSV-Kopfzeile (Semikolon-getrennt), der
# Zeilenbereich aus den Datenzeilen, der Hash EXTERN aus den Datei-Bytes
# (sha256_datei). Kein Attribut am <Table> (Content-Modell).
# ---------------------------------------------------------------------------
.z3_table_xml <- function(pfad, dateiname) {
  cp <- file.path(pfad, dateiname)
  # Zeilen wie das unabhaengige Test-Orakel lesen (Basis-readLines, ohne
  # zusaetzliche Encoding-Umdeutung), damit die Spaltennamen bit-treu aus der
  # Kopfzeile stammen.
  ll <- readLines(cp, warn = FALSE)
  header  <- if (length(ll) >= 1L) strsplit(ll[[1L]], ";", fixed = TRUE)[[1L]] else character(0)
  daten   <- if (length(ll) >= 2L) ll[-1L] else character(0)
  daten   <- daten[nzchar(daten)]
  zellen  <- lapply(daten, function(z) strsplit(z, ";", fixed = TRUE)[[1L]])

  hash <- sha256_datei(cp)   # EXTERNES, unabhaengiges Orakel — kein Selbst-Hash

  variablen <- vapply(seq_along(header), function(j) {
    spalte <- vapply(zellen, function(z) if (j <= length(z)) z[[j]] else "",
                     character(1L))
    paste0(
      "    <Variable>\n",
      "      <Name>", .z3_xml_escape(header[[j]]), "</Name>\n",
      "      <Type>", .z3_spalten_typ(spalte), "</Type>\n",
      "    </Variable>")
  }, character(1L))

  paste0(
    "  <Table>\n",
    "    <URL>", .z3_xml_escape(dateiname), "</URL>\n",
    "    <Name>", .z3_xml_escape(dateiname), "</Name>\n",
    "    <Description>GoBD-Z3-Tabelle ", .z3_xml_escape(dateiname),
    "</Description>\n",
    "    <Hash>sha256:", hash, "</Hash>\n",
    "    <Validity>\n",
    "      <Range>\n",
    "        <From>1</From>\n",
    "        <To>", length(daten), "</To>\n",
    "      </Range>\n",
    "    </Validity>\n",
    if (length(variablen)) paste0(paste(variablen, collapse = "\n"), "\n") else "",
    "  </Table>")
}

# ---------------------------------------------------------------------------
# export_z3_index(pfad, dateien): schreibt index.xml nach pfad und liefert
# 'index.xml'. Fehlerfaelle benannt (Stil manifest_validieren).
# ---------------------------------------------------------------------------
export_z3_index <- function(pfad, dateien) {
  # (1) pfad: nicht-leerer Ein-Element-String, existierendes Verzeichnis.
  if (!is.character(pfad) || length(pfad) != 1L || is.na(pfad) || !nzchar(pfad))
    stop("export_z3_index: pfad muss ein nicht-leerer String (Verzeichnis der CSVs) sein.",
         call. = FALSE)
  if (!dir.exists(pfad))
    stop("export_z3_index: Verzeichnis nicht gefunden: ", pfad, call. = FALSE)

  # (2) dateien: character-Vektor nicht-leerer Strings, Laenge >= 1.
  if (!is.character(dateien) || length(dateien) < 1L || anyNA(dateien) ||
      any(!nzchar(dateien)))
    stop("export_z3_index: dateien muss ein character-Vektor nicht-leerer ",
         "CSV-Basisnamen mit Laenge >= 1 sein.", call. = FALSE)

  # (3) jede genannte CSV muss in pfad liegen (kein Verzeichnis-Scan — die
  #     uebergebene Liste bestimmt den Bestand); fehlende Namen benennen.
  fehlt <- dateien[!file.exists(file.path(pfad, dateien))]
  if (length(fehlt))
    stop("export_z3_index: CSV-Datei nicht in ", pfad, " gefunden: ",
         paste(fehlt, collapse = ", "), call. = FALSE)

  # (4) je CSV genau ein <Table>-Fragment, in uebergebener Reihenfolge.
  tabellen <- vapply(dateien, function(dn) .z3_table_xml(pfad, dn),
                     character(1L), USE.NAMES = FALSE)

  xml <- paste0(
    "<?xml version='1.0' encoding='UTF-8'?>\n",
    "<DataSet version='2.0'>\n",
    "  <DataName>GoBD-Z3-Datentraegerueberlassung</DataName>\n",
    "  <DataDescription>Beschreibungsstandard (Anlage GoBD 2024) der ",
    "Z3-CSV-Ausleitung; je Tabelle SHA-256 ueber die rohen CSV-Bytes.",
    "</DataDescription>\n",
    paste(tabellen, collapse = "\n"), "\n",
    "</DataSet>\n")

  # index.xml als UTF-8-Bytes schreiben (writeBin umgeht plattformseitige
  # Zeilenumbruch-Uebersetzung); Rueckgabe ist der Basisname.
  idx_path <- file.path(pfad, "index.xml")
  writeBin(charToRaw(enc2utf8(xml)), idx_path)
  "index.xml"
}

# ===========================================================================
#  Schicht 3/3 — export_z3: KOMPOSITION des GoBD-Z3-Buendels aus Schicht 1/3
#  (export_z3_csv schreibt die CSVs) und Schicht 2/3 (export_z3_index baut die
#  index.xml). Reine Kern-Funktion, deterministisch (kein Sys.time() im Inhalt,
#  kein RNG, kein Grafikgeraet); ruft KEINE Kernel-Funktion — sie ruft
#  ausschliesslich export_z3_csv + export_z3_index (unantastbarer Bestand der
#  Vorgaenger-Schichten, hier nur aufgerufen) auf.
#
#  SIGNATUR export_z3(ergebnisse, sc, pfad) — deckungsgleich mit export_z3_csv;
#  pfad = Zielverzeichnis. Ablauf:
#    (1) csv <- export_z3_csv(ergebnisse, sc, pfad) schreibt eingaben.csv +
#        je Ergebnis-Block EINE CSV und liefert list(pfad, dateien).
#    (2) export_z3_index(csv$pfad, csv$dateien) haengt die index.xml an
#        (SHA-256 je CSV ueber die rohen Bytes, kein Selbst-Hash).
#    (3) Rueckgabe list(pfad = <das uebergebene Verzeichnis, unveraendert>,
#        dateien = c(csv$dateien, 'index.xml') in Schreibreihenfolge —
#        index.xml als LETZTES, die Naht der Komposition —, index_xml =
#        'index.xml').
#
#  PDF/A-ENTSCHEID (Vertragsbestandteil): export_z3 erzeugt KEIN PDF/A. Das
#  Buendel ist CSV + index.xml per Beschreibungsstandard; die Signatur hat
#  KEINEN PDF/A-Parameter, das Buendel enthaelt keine .pdf/.pdfa-Datei und die
#  index.xml referenziert ausschliesslich .csv-Dateien.
#
#  Die Fehlerfaelle (ergebnisse unbenannt/leer, sc ohne Klasse liqui_scenario,
#  pfad nicht anlegbar) sind an export_z3_csv delegiert und wirken benannt und
#  ungeaendert durch die Komposition.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/export.R noch R/manifest.R noch
#  den export_z3_csv/export_z3_index-Code der Vorgaenger-Schichten — ruft sie
#  nur auf und legt kein neues Paket an.
# ---------------------------------------------------------------------------
export_z3 <- function(ergebnisse, sc, pfad) {
  # (1) Schicht 1/3 — CSVs schreiben; validiert ergebnisse/sc/pfad (delegiert,
  #     benannte Fehler wirken ungeaendert durch).
  csv <- export_z3_csv(ergebnisse, sc, pfad)

  # (2) Schicht 2/3 — index.xml ueber die eben geschriebenen CSVs bauen.
  idx <- export_z3_index(csv$pfad, csv$dateien)

  # (3) Rueckgabe-Vertrag: das uebergebene Verzeichnis unveraendert, die
  #     Dateien in Schreibreihenfolge (CSVs, dann index.xml am Ende).
  list(pfad     = csv$pfad,
       dateien  = c(csv$dateien, idx),
       index_xml = idx)
}

# ===========================================================================
#  .z3_zip_wall — die Zip-Slip-Wall als EINE benannte Funktion.
#
#  Extraktion aus .z3_zip_store (Review-Nachholung): eine Wahrheit fuer beide
#  Zweige — .z3_zip (Primaerpfad utils::zip) und .z3_zip_store (Fallback)
#  rufen dieselbe Wall am Anfang. Nur flache Basisnamen INNERHALB von dir
#  duerfen gepackt werden; ein Eintrag mit Pfadseparator ('/' oder '\'),
#  Laufwerks-/Wurzelanteil oder '..' wuerde sonst ausserhalb gelesen UND
#  traversierend als ZIP-Eintrag geschrieben. Benannter Abbruch, bevor
#  irgendein Byte gelesen/geschrieben wird.
# ---------------------------------------------------------------------------
.z3_zip_wall <- function(dateien) {
  for (dn in dateien) {
    if (!is.character(dn) || length(dn) != 1L || is.na(dn) || !nzchar(dn) ||
        !identical(dn, basename(dn)) || grepl("[/\\\\]", dn) || dn %in% c(".", ".."))
      stop("z3-zip: unsicherer ZIP-Eintrag (kein flacher Basisname): '",
           dn, "'", call. = FALSE)
  }
  invisible(TRUE)
}

# ===========================================================================
#  .z3_zip_store — Basis-R-ZIP-Schreiber (Methode "store", keine Kompression)
#  fuer den GoBD-Z3-Download (phase-13-z3-ui).
#
#  HEIMAT R/z3.R (vorher inline in app/app.R): eine reine Basis-R-Funktion
#  (nur digest fuer die CRC-32, seit phase-12 Bestand) gehoert laut CLAUDE.md
#  in einen R/-Wrapper, nicht in die App-Huelle. Der GEWINN der Verschiebung
#  ist zugleich eine echte Speicherentlastung der DOD-Suite: der Roundtrip-Test
#  test_export_z3_zip.R kann den Packer jetzt ueber source(R/z3.R) erreichen,
#  statt das GANZE app/app.R (Shiny-UI/-Server + 15 R-Quellen + openxlsx2 +
#  mirai) nur fuer diese eine Funktion zu sourcen — eine schwere Allokation
#  weniger im EINEN run_all.R-Prozess (Gate-A-Kontext). app/app.R ruft die
#  Funktion unveraendert auf; source(R/z3.R) am App-Kopf stellt sie bereit.
#
#  utils::zip shellt zum externen zip-Programm (Rtools/R_ZIPCMD); fehlt es
#  (Rtools-lose Windows-Box, CI), warnt es nur und schreibt NICHTS — der
#  downloadHandler liefe stumm leer. Deshalb der store-ZIP von Hand, den
#  utils::unzip (internes miniz) verlustfrei liest. dir = das Buendel-
#  Verzeichnis, dateien = die zu packenden Basisnamen (flach, ohne Unterordner).
#  PK-Magic per Konstruktion (Local-Header-Signatur).
# ---------------------------------------------------------------------------
.z3_zip_store <- function(zipfile, dir, dateien) {
  # Zip-Slip-Wall: nach .z3_zip_wall extrahiert (Review-Nachholung) — eine
  # Wahrheit fuer beide Zweige, benannter Abbruch bevor irgendein Byte
  # gelesen wird.
  .z3_zip_wall(dateien)

  # 32-Bit-Ganzzahl -> n Bytes little-endian (ZIP speichert LE).
  le <- function(x, n) as.raw(bitwAnd(bitwShiftR(as.integer(x), 8L * (seq_len(n) - 1L)), 0xFFL))
  # CRC-32 der rohen Bytes als 4 Bytes little-endian (digest liefert Big-Endian-Hex).
  crc_le <- function(bytes) {
    hex <- digest::digest(bytes, algo = "crc32", serialize = FALSE)
    rev(as.raw(strtoi(substring(hex, c(1L, 3L, 5L, 7L), c(2L, 4L, 6L, 8L)), 16L)))
  }

  lokale  <- list()   # Local-File-Header + Daten je Datei
  zentral <- list()   # Central-Directory-Eintrag je Datei
  offset  <- 0L       # Offset des naechsten Local-Headers (fuer das Central Dir)

  for (dn in dateien) {
    bytes <- readBin(file.path(dir, dn), "raw", n = file.info(file.path(dir, dn))$size)
    name  <- charToRaw(enc2utf8(dn))
    crc   <- crc_le(bytes)
    sz    <- le(length(bytes), 4L)
    nlen  <- le(length(name), 2L)

    lokal <- c(
      as.raw(c(0x50, 0x4b, 0x03, 0x04)),  # Signatur PK\003\004
      le(20L, 2L),                        # Version needed to extract
      le(0L, 2L),                         # General purpose bit flag
      le(0L, 2L),                         # Compression method: 0 = store
      le(0L, 2L),                         # Mod time (fix 0)
      le(0x21L, 2L),                      # Mod date (fix 1980-01-01, gueltiges DOS-Datum)
      crc, sz, sz,                        # CRC-32, comp size, uncomp size
      nlen, le(0L, 2L),                   # Dateiname-Laenge, Extra-Laenge 0
      name, bytes)

    ceintrag <- c(
      as.raw(c(0x50, 0x4b, 0x01, 0x02)),  # Signatur PK\001\002
      le(20L, 2L), le(20L, 2L),           # Version made by / needed
      le(0L, 2L), le(0L, 2L),             # Bit flag / Compression store
      le(0L, 2L), le(0x21L, 2L),          # Mod time / date
      crc, sz, sz,                        # CRC-32, comp size, uncomp size
      nlen, le(0L, 2L), le(0L, 2L),       # Name-Laenge, Extra 0, Comment 0
      le(0L, 2L), le(0L, 2L), le(0L, 4L), # Disk 0, int attrs 0, ext attrs 0
      le(offset, 4L),                     # Offset des Local-Headers
      name)

    lokale[[length(lokale) + 1L]]   <- lokal
    zentral[[length(zentral) + 1L]] <- ceintrag
    offset <- offset + length(lokal)
  }

  lokal_raw   <- do.call(c, lokale)
  zentral_raw <- do.call(c, zentral)
  eocd <- c(
    as.raw(c(0x50, 0x4b, 0x05, 0x06)),    # Signatur PK\005\006
    le(0L, 2L), le(0L, 2L),               # Disk-Nummer / Disk mit Central Dir
    le(length(dateien), 2L),              # Eintraege auf dieser Disk
    le(length(dateien), 2L),              # Eintraege gesamt
    le(length(zentral_raw), 4L),          # Groesse des Central Directory
    le(length(lokal_raw), 4L),            # Offset des Central Directory
    le(0L, 2L))                           # Kommentar-Laenge 0

  writeBin(c(lokal_raw, zentral_raw, eocd), zipfile)
  invisible(zipfile)
}

# ===========================================================================
#  .z3_zip_gueltig — echte Gueltigkeitspruefung des utils::zip-Resultats
#  statt PK-Prefix (Befund Medium z3.R:532).
#
#  utils::unzip(list = TRUE) liest per internem miniz, rein lesend, kein
#  externes Binary. Ein PK-Torso oder eine falsche Eintragsmenge ist KEIN
#  gueltiges Archiv; eine Warnung wird konservativ als ungueltig gewertet —
#  der Fallback baut dann byte-kompatibel neu, kein Schaden.
# ---------------------------------------------------------------------------
.z3_zip_gueltig <- function(zipfile, dateien) {
  if (!file.exists(zipfile) || isTRUE(file.info(zipfile)$size < 4)) return(FALSE)
  le <- tryCatch(utils::unzip(zipfile, list = TRUE),
                 error = function(e) NULL, warning = function(w) NULL)
  if (is.null(le)) return(FALSE)
  setequal(le$Name, dateien)
}

# ===========================================================================
#  .z3_zip — Packer-Naht des GoBD-Z3-Downloads (phase-13-z3-ui).
#
#  HANDOFF-VORGABE: das Buendel wird "via utils::zip (Basis-R, kein neues Paket)"
#  gezippt. utils::zip ist darum der PRIMAER-Weg und wird hier echt aufgerufen.
#  utils::zip shellt aber zum externen zip-Programm (Rtools/R_ZIPCMD); fehlt es
#  (Rtools-lose Windows-Box, CI), warnt es nur und schreibt NICHTS — der
#  downloadHandler liefe stumm leer. Deshalb: nach dem utils::zip-Aufruf pruefen,
#  ob ein gueltiges Archiv entstand, und NUR dann auf den byte-kompatiblen
#  Basis-R-Store-ZIP (.z3_zip_store) zurueckfallen.
#
#  HAERTUNG (Review-Nachholung, .orch/review-pakete/
#  befund-paket-phase-13-z3-ui-hybridfix.md, Spec-Commit 30d53cc):
#  die Zip-Slip-Wall (.z3_zip_wall) steht an der Naht fuer BEIDE Zweige —
#  vorher packte der Primaerpfad utils::zip traversierende Eintraege
#  widerspruchslos (Befund High z3.R:526); die Gueltigkeit wird per
#  unzip-list + Eintragsabgleich geprueft statt per PK-Magic
#  (Befund Medium z3.R:532, .z3_zip_gueltig).
#
#  Beide Ergebnisse liest utils::unzip (internes miniz) verlustfrei; das
#  Resultat ist in JEDEM Fall ein gueltiges ZIP mit PK-Signatur. Kein neues
#  Paket (utils ist Basis-R, digest ist seit phase-12 Bestand).
#  dir = Buendel-Verzeichnis, dateien = flache Basisnamen.
# ---------------------------------------------------------------------------
.z3_zip <- function(zipfile, dir, dateien) {
  # Wall an der Naht, VOR normalizePath/setwd/unlink/utils::zip — kein Byte
  # wird vorher angefasst (Befund High z3.R:526).
  .z3_zip_wall(dateien)
  zipfile <- normalizePath(zipfile, winslash = "/", mustWork = FALSE)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)                 # Arbeitsverz. auf jedem Ausgang zurueck
  tryCatch({
    setwd(dir)                                   # utils::zip packt relativ zum cwd
    unlink(zipfile)                              # ehrliche PK-Pruefung: kein Altbestand
    suppressWarnings(utils::zip(zipfile = zipfile, files = dateien,
                                flags = "-q -r9X"))
  }, error = function(e) NULL)
  setwd(wd)

  # Hat utils::zip ein gueltiges Archiv geschrieben? Echte Pruefung per
  # unzip-list + Eintragsabgleich (.z3_zip_gueltig) statt PK-Prefix.
  gueltig <- .z3_zip_gueltig(zipfile, dateien)

  # Fallback nur, wenn kein externes zip-Binary ein gueltiges Archiv lieferte.
  if (!gueltig) {
    unlink(zipfile)
    .z3_zip_store(zipfile, dir, dateien)
  }
  invisible(zipfile)
}
